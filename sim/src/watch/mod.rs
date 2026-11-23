//! `--watch [--port N] [--tick-ms N]`: the world served each tick to a Screeps
//! client (lazycreep) as a private server's API and socket, the run paced to it.
//! krebs's backend answers what it can off the published view; what it holds
//! that lazycreep cannot pass (token expiry, same-origin sockets, the unsharded
//! tile paths) is answered here instead.
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream, UdpSocket};
use std::sync::atomic::{AtomicUsize, Ordering::SeqCst};
use std::sync::mpsc::{self, SyncSender};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

use krebs::atom;
use krebs::common::calc_world_size;
use krebs::doc::DocRef;
use krebs::room_name::RoomXY;
use krebs::world::{World as Krebs, WorldView};
use krebs_backend::http::{Request, Response, obj, url_decode};
use krebs_backend::{ApiCtx, Published, api, field, png, server, socket, str_field, ws};
use krebs_util::json::Json;
use serde_json::{Map, Value, json};

use crate::driver::Driver;
use crate::view::text;
use crate::world::{US, USERNAME};

type Fault = (&'static str, String);

/// The socket binds every interface: what a LAN client can make it hold is bounded.
const SERVING: usize = 16;
const MAX_LINE: u64 = 8 << 10;
const MAX_BODY: usize = 64 << 10;
/// Frames a client may fall behind by before it is dropped; lazycreep reconnects.
const BEHIND: usize = 1024;

pub struct Watch {
    shared: Arc<Shared>,
    every: Duration,
    next: Instant,
    prev: WorldView,
    decided: f64,
    told: BTreeSet<Fault>,
}

struct Shared {
    published: Arc<Published>,
    room: String,
    conns: Mutex<Vec<Arc<Mutex<Conn>>>>,
    serving: AtomicUsize,
}

/// A room keeps the objects and users its frames showed. Frames go out on a
/// writer thread, so a slow client never holds up the tick.
struct Conn {
    out: SyncSender<(u8, Vec<u8>)>,
    socket: TcpStream,
    gzip: bool,
    rooms: BTreeMap<String, (Arc<[DocRef]>, HashSet<String>)>,
    channels: BTreeSet<String>,
    dead: bool,
}

impl Watch {
    pub fn open(k: &Krebs, room: &str, port: u16, every: u64) -> Watch {
        let listener = TcpListener::bind(("0.0.0.0", port))
            .unwrap_or_else(|e| panic!("--watch cannot listen on port {port}: {e}"));
        let view = k.view();
        let shared = Arc::new(Shared {
            published: Arc::new(Published::new(view.clone())),
            room: room.to_string(),
            conns: Mutex::new(Vec::new()),
            serving: AtomicUsize::new(0),
        });
        let accepting = shared.clone();
        thread::spawn(move || {
            for stream in listener.incoming().flatten() {
                if accepting.serving.fetch_add(1, SeqCst) >= SERVING {
                    accepting.serving.fetch_sub(1, SeqCst);
                    continue;
                }
                let sh = accepting.clone();
                thread::spawn(move || {
                    serve(stream, &sh);
                    sh.serving.fetch_sub(1, SeqCst);
                });
            }
        });
        banner(port, every, room);
        Watch {
            shared,
            every: Duration::from_millis(every),
            next: Instant::now(),
            prev: view,
            decided: 0.0,
            told: BTreeSet::new(),
        }
    }

    /// Publishes the tick `k` ended on to every subscriber, then waits out the rest of `--tick-ms`.
    pub fn tick<W: Write>(&mut self, k: &Krebs, driver: &Driver<W>) {
        let view = k.view();
        self.shared.published.set(view.clone());
        let cpu = json!({ "cpu": driver.decide_ms - self.decided, "memory": driver.memory.len() });
        self.decided = driver.decide_ms;
        let fresh: Vec<String> = (driver.faults.iter())
            .filter(|f| self.told.insert((*f).clone()))
            .map(|(at, why)| format!("fault {at}: {why}"))
            .collect();
        let console =
            (!fresh.is_empty()).then(|| json!({ "messages": { "log": fresh, "results": [] } }));
        let conns = {
            let mut all = lock(&self.shared.conns);
            all.retain(|c| !lock(c).dead);
            all.clone()
        };
        for c in conns {
            lock(&c).tick(&view, &self.prev, &cpu.to_string(), console.as_ref());
        }
        self.prev = view;
        self.next += self.every;
        match self.next.checked_duration_since(Instant::now()) {
            Some(wait) => thread::sleep(wait),
            None => self.next = Instant::now(),
        }
    }

    /// The last tick stays served until the process is killed.
    pub fn park(self) -> ! {
        eprintln!("watch: the last tick is stepped; the world stays up until Ctrl-C");
        loop {
            thread::sleep(Duration::from_secs(3600));
        }
    }
}

fn lock<T>(m: &Mutex<T>) -> std::sync::MutexGuard<'_, T> {
    m.lock().unwrap_or_else(std::sync::PoisonError::into_inner)
}

/// What `soak.mjs --watch` prints: how to point lazycreep at this run.
fn banner(port: u16, every: u64, room: &str) {
    // A connected UDP socket names the address this host reaches the LAN by; nothing is sent.
    let lan = UdpSocket::bind("0.0.0.0:0")
        .and_then(|s| s.connect("192.0.2.1:9").and(s.local_addr()))
        .map_or("127.0.0.1".to_string(), |a| a.ip().to_string());
    let api = format!("http://127.0.0.1:{port}");
    eprintln!(
        "\nwatch: {api}, a tick every {every} ms; user {USERNAME}, any token\n  client, in another shell (MSC_PORT=… if 8080 is taken):\n    MSC_ADDRESS=0.0.0.0 MSC_API_UPSTREAM={api} nix run github:Xerxes-2/lazycreep#gateway\n  then http://{lan}:8080 (or localhost) → 设置 → 添加自定义 Server:\n    名称 fabot-watch · API 根路径 /api · WebSocket 地址 ws://{lan}:{port}/socket/websocket · 分 Shard 不勾\n  then 设置 → Auth token: guest\n  room: {room}\n"
    );
}

fn room_idx(view: &WorldView, name: &str) -> Option<usize> {
    (view.meta.rooms.iter()).position(|m| &*m.name == name && !m.removed)
}

fn us(view: &WorldView) -> Option<u32> {
    (view.users.iter().position(|u| u.id.as_str() == US)).map(|u| u as u32)
}

fn map_view<'a>(view: &'a WorldView, room: &str) -> &'a str {
    match room_idx(view, room).map(|r| &view.rooms[r].map_view) {
        Some(m) if !m.is_empty() => m,
        _ => "{}",
    }
}

impl Conn {
    fn send(&mut self, channel: &str, data: &str) {
        let mut text = format!("[{},{data}]", Value::from(channel));
        if self.gzip {
            let gz =
                krebs_util::base64::encode(&krebs_util::deflate::zlib_compress(text.as_bytes()));
            if gz.len() + 3 < text.len() {
                text = format!("gz:{gz}");
            }
        }
        self.put(ws::OP_TEXT, text.into_bytes());
    }

    fn put(&mut self, op: u8, frame: Vec<u8>) {
        if !self.dead && self.out.try_send((op, frame)).is_err() {
            self.dead = true;
            let _ = self.socket.shutdown(Shutdown::Both);
        }
    }

    fn command(&mut self, line: &str, view: &WorldView) {
        let (verb, arg) = line.split_once(' ').unwrap_or((line, ""));
        match verb {
            "gzip" => self.gzip = arg == "on",
            // Any token, never expiring: the client keeps the one it was given.
            "auth" => self.put(ws::OP_TEXT, format!("auth ok {arg}").into_bytes()),
            "subscribe" => self.subscribe(arg, view),
            "unsubscribe" => {
                self.rooms.remove(arg.strip_prefix("room:").unwrap_or(arg));
                self.channels.remove(arg);
            }
            _ => {}
        }
    }

    fn subscribe(&mut self, channel: &str, view: &WorldView) {
        if let Some(room) = channel.strip_prefix("room:") {
            let Some(r) = room_idx(view, room) else {
                return;
            };
            if let Some(first) = socket::rooms::room(us(view), channel, view) {
                // krebs writes the NPCs' entries without the badge their docs hold.
                let mut first: Value = serde_json::from_str(&first.stringify()).unwrap_or_default();
                let mut users = HashSet::new();
                if let Some(Value::Object(all)) = first.get_mut("users") {
                    for (id, user) in all.iter_mut() {
                        *user = brief(view, id);
                        users.insert(id.clone());
                    }
                }
                self.send(channel, &first.to_string());
                let shown = (view.rooms[r].objects.clone(), users);
                self.rooms.insert(room.to_string(), shown);
            }
            return;
        }
        if let Some(room) = channel.strip_prefix("roomMap2:") {
            self.send(channel, map_view(view, room));
        }
        self.channels.insert(channel.to_string());
    }

    fn tick(&mut self, view: &WorldView, prev: &WorldView, cpu: &str, console: Option<&Value>) {
        let mut frames = Vec::new();
        for (room, (shown, known)) in &mut self.rooms {
            let Some(r) = room_idx(view, room) else {
                continue;
            };
            let now = &view.rooms[r].objects;
            let mut frame = json!({ "gameTime": view.time, "objects": diff_docs(shown, now) });
            let users: Map<String, Value> = (now.iter())
                .filter_map(|d| str_field(&d.obj, "user"))
                .filter(|id| known.insert(id.to_string()))
                .map(|id| (id.to_string(), brief(view, id)))
                .collect();
            if !users.is_empty() {
                frame["users"] = Value::Object(users);
            }
            frames.push((format!("room:{room}"), frame.to_string()));
            *shown = now.clone();
        }
        for channel in &self.channels {
            let data = if let Some(room) = channel.strip_prefix("roomMap2:") {
                let Some(r) = room_idx(view, room) else {
                    continue;
                };
                let same = prev
                    .rooms
                    .get(r)
                    .is_some_and(|p| Arc::ptr_eq(p, &view.rooms[r]));
                (!same).then(|| map_view(view, room).to_string())
            } else if channel.ends_with("/cpu") {
                Some(cpu.to_string())
            } else if channel.ends_with("/console") {
                console.map(Value::to_string)
            } else {
                None
            };
            frames.extend(data.map(|d| (channel.clone(), d)));
        }
        for (channel, data) in frames {
            self.send(&channel, &data);
        }
    }
}

fn brief(view: &WorldView, id: &str) -> Value {
    let user = view.users.iter().find(|u| u.id.as_str() == id);
    let mut brief =
        json!({ "_id": id, "username": user.and_then(|u| str_field(&u.doc, "username")) });
    if let Some(badge) = user.and_then(|u| field(&u.doc, "badge")) {
        brief["badge"] = serde_json::from_str(&badge.to_json().stringify()).unwrap_or_default();
    }
    brief
}

fn id_of(d: &DocRef) -> &str {
    text(&d.obj, atom!("_id"))
}

fn parse(d: &DocRef) -> Value {
    serde_json::from_str(d.json()).unwrap_or_default()
}

/// The room channel's tick: each changed object's diff, `null` for one gone, whole for one new.
fn diff_docs(old: &[DocRef], new: &[DocRef]) -> Value {
    let by_id: HashMap<&str, &DocRef> = new.iter().map(|d| (id_of(d), d)).collect();
    let mut out = Map::new();
    for o in old {
        match by_id.get(id_of(o)) {
            None => drop(out.insert(id_of(o).to_string(), Value::Null)),
            Some(n) if Arc::ptr_eq(o, n) || o.json() == n.json() => {}
            Some(n) => {
                if let Some(d) = diff(&parse(o), &parse(n)) {
                    out.insert(id_of(o).to_string(), d);
                }
            }
        }
    }
    let had: HashSet<&str> = old.iter().map(id_of).collect();
    for n in new.iter().filter(|n| !had.contains(id_of(n))) {
        out.insert(id_of(n).to_string(), parse(n));
    }
    Value::Object(out)
}

/// lazycreep's merge inverted: objects key by key (`null` deletes), anything else replaced whole.
fn diff(old: &Value, new: &Value) -> Option<Value> {
    let (Value::Object(o), Value::Object(n)) = (old, new) else {
        return (old != new).then(|| new.clone());
    };
    let mut d: Map<String, Value> = (o.keys())
        .filter(|k| !n.contains_key(*k))
        .map(|k| (k.clone(), Value::Null))
        .collect();
    for (k, v) in n {
        let changed = o.get(k).map_or(Some(v.clone()), |was| diff(was, v));
        d.extend(changed.map(|c| (k.clone(), c)));
    }
    (!d.is_empty()).then_some(Value::Object(d))
}

fn serve(stream: TcpStream, sh: &Shared) {
    // An idle connection gives its slot back; a socket waits on its client for good.
    let _ = stream.set_read_timeout(Some(Duration::from_secs(30)));
    let Ok(read) = stream.try_clone() else { return };
    let (mut r, mut w) = (BufReader::new(read), stream);
    loop {
        let res = match read_request(&mut r) {
            Ok(req) if req.path == "/socket/websocket" => return websocket(&req, r, w, sh),
            Ok(req) => route(&req, sh),
            Err(0) => return,
            Err(status) => Response::text(status, "text/plain", ""),
        };
        if write_response(&mut w, &res).is_err() || [400, 413, 431].contains(&res.status) {
            return;
        }
    }
}

/// One line of the head; `Err` holds the status to answer, 0 for a connection gone.
fn head_line(r: &mut impl BufRead) -> Result<String, u16> {
    let mut line = String::new();
    match r.take(MAX_LINE).read_line(&mut line) {
        Ok(0) | Err(_) => Err(0),
        Ok(_) if !line.ends_with('\n') => Err(431),
        Ok(_) => Ok(line),
    }
}

fn read_request(r: &mut impl BufRead) -> Result<Request, u16> {
    let line = head_line(r)?;
    let mut parts = line.split_whitespace();
    let (method, target) = (
        parts.next().ok_or(400u16)?.to_string(),
        parts.next().ok_or(400u16)?,
    );
    let (path, qs) = target.split_once('?').unwrap_or((target, ""));
    let pair = |p: &str| {
        let (k, v) = p.split_once('=').unwrap_or((p, ""));
        (url_decode(k), url_decode(v))
    };
    let query = qs.split('&').filter(|p| !p.is_empty()).map(pair).collect();
    let (path, mut headers) = (path.to_string(), Vec::new());
    while headers.len() < 100 {
        let line = head_line(r)?;
        let Some((k, v)) = line.trim_end().split_once(':') else {
            break;
        };
        headers.push((k.trim().to_ascii_lowercase(), v.trim().to_string()));
    }
    let length = headers.iter().find(|(k, _)| k == "content-length");
    let length = length.map_or(Ok(0), |(_, v)| v.parse().map_err(|_| 400u16))?;
    if length > MAX_BODY {
        return Err(413);
    }
    let mut body = vec![0; length];
    r.read_exact(&mut body).map_err(|_| 0u16)?;
    Ok(Request {
        method,
        path,
        query,
        headers,
        body,
    })
}

/// Every response says any origin may read it: the client's page is served from the gateway's port.
fn write_response(w: &mut impl Write, res: &Response) -> io::Result<()> {
    let reason = match res.status {
        200 => "OK",
        204 => "No Content",
        400 => "Bad Request",
        401 => "Unauthorized",
        404 => "Not Found",
        413 => "Payload Too Large",
        431 => "Request Header Fields Too Large",
        _ => "Error",
    };
    let mut head = format!("HTTP/1.1 {} {reason}\r\n", res.status);
    for (k, v) in &res.headers {
        head += &format!("{k}: {v}\r\n");
    }
    head += "Access-Control-Allow-Origin: *\r\nAccess-Control-Allow-Headers: X-Token, X-Username, Content-Type\r\n";
    head += &format!("Content-Length: {}\r\n\r\n", res.body.len());
    w.write_all(head.as_bytes())?;
    w.write_all(&res.body)
}

fn route(req: &Request, sh: &Shared) -> Response {
    let view = sh.published.get();
    let ctx = ApiCtx {
        published: sh.published.clone(),
        commands: mpsc::channel().0,
        user: us(&view),
    };
    let ok = |pairs| Response::ok(obj(pairs));
    match (req.method.as_str(), req.path.as_str()) {
        ("OPTIONS", _) => Response::text(204, "text/plain", ""),
        ("GET", "/api/version") => server::version(req, &ctx),
        ("GET", "/api/auth/me") => api::auth::me(req, &ctx),
        ("GET", "/api/user/find") => api::user::find(req, &ctx),
        ("GET", "/api/game/world-size") => {
            let size = Json::Num(calc_world_size(view.meta.rooms.iter().map(|m| &*m.name)));
            ok(vec![("width", size.clone()), ("height", size)])
        }
        ("GET", "/api/game/room-terrain") => api::game::room_terrain(req, &ctx),
        ("GET", "/api/game/time") => api::game::time(req, &ctx),
        ("POST", "/api/game/map-stats") => api::game::map_stats(req, &ctx),
        ("GET", "/api/user/world-start-room") => {
            ok(vec![("room", Json::Arr(vec![Json::Str(sh.room.clone())]))])
        }
        ("GET", "/api/user/rooms") => {
            ok(vec![("shards", obj(vec![("", Json::Arr(owned(&view)))]))])
        }
        ("GET", "/api/game/room-objects") => room_objects(req.param("room").unwrap_or(""), &view),
        ("GET", "/api/game/room-decorations") => ok(vec![("decorations", Json::Arr(vec![]))]),
        ("POST", "/api/user/console") => ok(vec![]),
        ("GET", path) if path.starts_with("/assets/map/") => tile(path, &ctx),
        (method, path) => Response::not_found(method, path),
    }
}

fn owned(view: &WorldView) -> Vec<Json> {
    let controllers = view
        .rooms
        .iter()
        .flat_map(|r| r.objects.iter())
        .filter(|d| d.kind() == "controller");
    (controllers.filter(|d| str_field(&d.obj, "user") == Some(US)))
        .map(|d| Json::Str(str_field(&d.obj, "room").unwrap_or("").to_string()))
        .collect()
}

fn room_objects(room: &str, view: &WorldView) -> Response {
    let objects = room_idx(view, room).map_or(Vec::new(), |r| {
        view.rooms[r].objects.iter().map(|d| d.json()).collect()
    });
    let body = format!(
        r#"{{"ok":1,"objects":[{}],"users":{{}}}}"#,
        objects.join(",")
    );
    Response {
        status: 200,
        headers: vec![("Content-Type".into(), "application/json".into())],
        body: body.into_bytes(),
    }
}

/// `/assets/map/<shard>/…` with the shard empty (`//`) or `shard0`; krebs draws all but zoom1.
fn tile(path: &str, ctx: &ApiCtx) -> Response {
    let rest = path
        .trim_start_matches("/assets/map/")
        .trim_start_matches('/');
    let rest = rest.strip_prefix("shard0/").unwrap_or(rest);
    if let Some(corner) = rest
        .strip_prefix("zoom1/")
        .and_then(|f| f.strip_suffix(".png"))
    {
        return zoom1(corner, &ctx.published.get())
            .unwrap_or_else(|| Response::not_found("GET", path));
    }
    let req = Request {
        method: "GET".into(),
        path: format!("/assets/map/{rest}"),
        query: vec![],
        headers: vec![],
        body: vec![],
    };
    png::map_png(&req, ctx)
}

/// `zoom1/<corner>.png`: the 10×10 rooms of a sector, 20 pixels a room, missing rooms transparent.
fn zoom1(corner: &str, view: &WorldView) -> Option<Response> {
    let at: RoomXY = corner.parse().ok()?;
    let mut px = vec![0u8; 200 * 200 * 4];
    let mut any = false;
    for (r, m) in view
        .meta
        .rooms
        .iter()
        .enumerate()
        .filter(|(_, m)| !m.removed)
    {
        let (dx, dy) = (m.xy.x - at.x, m.xy.y - at.y);
        if !(0..10).contains(&dx) || !(0..10).contains(&dy) {
            continue;
        }
        any = true;
        let terrain = view.terrain.get(r as u32);
        for (x, y) in (0..20).flat_map(|y| (0..20).map(move |x| (x, y))) {
            let color = png::tile_color(terrain, (x * 5 / 2) as u8, (y * 5 / 2) as u8);
            let at = ((dy * 20 + y) * 200 + dx * 20 + x) as usize * 4;
            px[at..at + 4].copy_from_slice(&color);
        }
    }
    let body = any.then(|| png::encode_png(200, 200, &px))?;
    Some(Response {
        status: 200,
        headers: vec![("Content-Type".into(), "image/png".into())],
        body,
    })
}

fn websocket(req: &Request, r: BufReader<TcpStream>, mut w: TcpStream, sh: &Shared) {
    let head = match ws::handshake(req, &["13"]) {
        Ok(head) => head,
        Err(res) => return drop(write_response(&mut w, &res)),
    };
    let _ = w.set_read_timeout(None);
    let (Ok(()), Ok(socket)) = (w.write_all(head.as_bytes()), w.try_clone()) else {
        return;
    };
    let (out, frames) = mpsc::sync_channel::<(u8, Vec<u8>)>(BEHIND);
    thread::spawn(move || {
        for (op, frame) in frames {
            if ws::write_frame(&mut w, op, &frame).is_err() {
                break;
            }
        }
        let _ = w.shutdown(Shutdown::Both);
    });
    let conn = Conn {
        out,
        socket,
        gzip: false,
        rooms: BTreeMap::new(),
        channels: BTreeSet::new(),
        dead: false,
    };
    let conn = Arc::new(Mutex::new(conn));
    lock(&sh.conns).push(conn.clone());
    let mut reader = ws::Reader::new(r);
    while let Ok(message) = reader.next() {
        let mut c = lock(&conn);
        match message {
            ws::Message::Text(line) => c.command(&line, &sh.published.get()),
            ws::Message::Ping(p) => c.put(ws::OP_PONG, p),
            ws::Message::Close(_) => break,
            _ => {}
        }
    }
    lock(&conn).dead = true;
}
