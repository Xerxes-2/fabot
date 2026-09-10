# 多跳 outpost — ADR 0058 落地后 W12S28 / W13S28 还能 declare 哪些房

Date: 2026-09-10（tick 302,850–303,110，`shardSeason`）。姊妹篇 `docs/research/remote-candidates.md`（tick 202,311）当日的核心限制 —— *"`Atlas.seams` 只认正交相邻，对角房和两房外的房静默变成空房"* —— **已经被 ADR 0058 解除**：一次 walk 现在跨一条 Seam 链，`Tuning.MaxHops = 3`（`src/Core/Types/Rules.fs:276`），`dist/main.js` 里 `routeBy` / `transitBetween` 都在跑。本文把那份普查在新的几何下重做一遍：**1–3 跳之内，两个 home 各自还能/该 declare 哪些房、按什么顺序、收益和风险各是多少。**

本文所有房间事实都是**本日用只读 API 在赛季服现测的**（只用 `GET /api/game/room-objects`、`GET /api/game/room-terrain?encoded=1`、`POST /api/game/map-stats`、`GET /api/game/time`，以及 `scripts/observe.mjs` 的只读 Memory 通道；没有调用任何写入或 console 端点）。所有步数是**本文自己的多房 Dijkstra 算的**，并且**按 `Atlas.chainedInto` 的链式定价复刻**，不是自由 flood。所有能量数字用仓库自己的常量与体型规则重算（`src/Core/Types/Rules.fs`、`src/Core/Decide/Bodies.fs`、`src/Core/Decide/Quota.fs`）。

已定的约束，本文不重新论证但全程当作前提：**第三个 colony 已决定 claim W15S28**，由 W13S28 当 mother 走 ADR 0047 的 candidate-colony 路径（`src/Core/Types/Colonies.fs:280` 的 `Outpost.w15s28`，以及 `Colony.declared` 里那第三条 `Home = "W15S28"; Mother = Some "W13S28"` 的记录，本日已在 `dist/main.js` 里）。W14S28 是它的 transit room。

**Verified against**（本日一手调用，脚本全部在 `/tmp/claude-1000/-home-xerxes2-Dev-fabot/38e968fc-…/scratchpad/`）：

| 脚本 | 干了什么 |
|---|---|
| `survey.mjs` | 两个 home 的 Manhattan ≤ 3 并集共 **32 房**，逐房 `GET /api/game/room-objects` + 一次 `POST /api/game/map-stats`（`statName: owner0`）。写出 `survey.json`（含每个 source / controller 的引擎真实 id 与坐标）。 |
| `detail.mjs` | 把 `survey.json` 摊成 §1 的表。 |
| `terrain.mjs` | 同 32 房 `api.gameRoomTerrain(room, shard, true)`，写出 `terrain.json`。 |
| `paths.mjs` | 基础设施：`Seam.pairsAcross` / `Seam.bandBy` / `Seam.joinedBy` / `RoomName.adjacent` / `hopsBetween` / `transitBetween` / `routeBy` 的逐行 JS 复刻，加一个多房 Dijkstra。 |
| `chain.mjs` | `Atlas.carriedAcross` / `foldChain` / `chainedInto` 的复刻 —— **链式定价**，每段一次单房 flood，不允许回退到已离开的房。 |
| `bands.mjs` | 箱内每一对相邻房的接缝宽度（52 条边界）。 |
| `routes.mjs` | 每个候选房的 hops、`transitBetween`、投影内 route、任意路 route，判定是否需要矩形外 detour。 |
| `tiebreak.mjs` | 每个 ≥2 跳候选房的**所有**等长 chain 逐条定价，与 `routeBy` 实际选的那条对比。 |
| `trunk.mjs` | 沿代码自己选的 chain 回溯路径，逐房拆分格数、接缝宽度、已铺/待铺格数。 |
| `econ.mjs` | ADR 0042 / 0049 / 0056 口径的全账（含 `haulerDemandOf` 的"最贵 sink"与 ADR 0049 的一次取整）。 |
| `ladder.mjs` | 按建议次序算**边际** hauler（ADR 0049 是全房一次取整，所以每个房的 hauler 成本是顺序相关的）。 |
| `threats.mjs` / `users.mjs` | sector（W10–W19 × S20–S29）逐房查 `invaderCore` 与 effect；W6–W24 × S18–S34 共 323 房的 owner / reservation 扫描并解析用户名。 |
| `scripts/observe.mjs quotas / raids / layout --colony …` | 活体读数（tick 303,107–303,109）：两个 colony 的 hauler demand 逐 container 明细、20 条 raid log、layout record。 |

代码侧核对：`src/Core/Types/Geometry.fs`（`adjacent:212`、`neighbouring:247`、`hopsBetween:257`、`transitBetween:274`、`routeBy:300`、`Seam.pairsAcross:394`、`bandBy:409`、`joinedBy:426`）、`src/Core/Types/Colonies.fs`（`Outpost.Controller:28`、`withinHopBudget:59`、`routable:75`、`refused:99`、`roomsProjected:115`）、`src/Core/Types/Sightings.fs`（`World.linked:405`、`scanOf:432`）、`src/Core/Types/Views.fs:385`、`src/Core/Atlas.fs`（`seams:1269`、`route:1288`、`carriedAcross:1455`、`foldChain:1506`、`chainedInto:1540`、`joinedAlong:1735`、`haulRoundTripTicks:2248`、`castAlong:2298`、`castWalkTicks:2327`）、`src/Core/Types/Rules.fs`、`src/Core/Decide/Bodies.fs`、`src/Core/Decide/Quota.fs`，以及 ADR 0042 / 0047 / 0049 / 0056 / 0057 / 0058。

## Summary

- **建议次序：`W11S28`（→W12S28）→ `W14S28`（→W13S28）→ `W12S29`（→W12S28）→ `W13S27`（→W13S28，等 W15S28 独立后）→ `W11S29`（→W12S28）→ `W14S29`（→W13S28，可选）**。前四个把两个 home 的入侵门全部关上，且全部是 **1 跳**；后两个是本轮唯一两个值得的 2 跳房。`W15S27` / `W15S29` 明确**不给 W13S28**，将来给 W15S28（各 1 跳），理由见下一条。
- **最反直觉的一条：多跳的价格不是最短的价格，而是 tie-break 的价格。** `RoomName.routeBy` 是宽度优先，平局由 `RoomName.adjacent` 的固定 N→E→S→W 顺序破（`Geometry.fs:212`、ADR 0058 决策 1：*"the chain is a function of the world and not of the search"*）—— 它保证**确定**，不保证**最短**。对每个 L 形（对角）目标，本文把所有等长 chain 都定了价（`tiebreak.mjs`）：`W13S28 → W15S27` 代码走 `W13S27>W14S27` 得 **146** tick，而 `W14S28>W15S28` 那条只要 **107**（贵 36%）；`W13S28 → W15S29` 是 **141** 对 **101**；`W13S28 → W14S29` 是 **104/89** 对 **86/84**；`W12S28 → W13S29` 是 **103/95** 对 **54/73**（几乎两倍）。**这条不是能量账里的一项，它就是能量账**：`haulRoundTripTicks`（`Atlas.fs:2248`）吃的是 `route` 给的那条链，`haulerDemandOf` 按它买 hauler。所以对角房的定价要按代码选的链算，本文全表都是这么算的。
- **两房外的房现在能定价了，但没有一个比 1 跳的房好。** 单源房的净收益基本只由距离决定：1 跳的 6.3 → 2 跳的 4.7 → 3 跳的 3.1–4.8 e/tick。双源房里唯一进入建议的是 `W14S29`（2 跳，净 12.30）。这一条**没有推翻** ADR 0058 Consequences 里那句 *"`remote-candidates.md` measured every two-hop candidate and found none with better net income than a one-hop room"* —— 它成立，只是原因换了：不再是"定不了价"，而是**距离 + tie-break**。
- **`remote-candidates.md` 的 §5 全节已过期，§2.2 的步数也已过期。** §5"代码今天做不到的事"的两条结论（*"两房外的 outpost 不支持"*、*"对角邻房同样不支持"*）**被 ADR 0058 推翻**；§2.2 那张"走得到、但 Seam 模型定不了价"的表里的步数是**自由多房 Dijkstra** 的，与代码今天真正会付的价差得很远（见上一条）。§3 的能量表也过期：两个 home 都已 **RCL6**（bank 2300），旧文的 1800/1300 与 1.20/0.80 hauler 摊销都是旧数。§1、§2.1、§4 的房间事实和 1 跳步数**本日全部复现**，仍然可用。
- **hauler 的取整已经不是"每个房 ceil 一次"，而是三条规则叠在一起，而且顺序相关。** ADR 0049 是**全 colony 一次取整**（`haulerDemandOf` 在 `Quota.fs:178`，那一次 `ceilDiv demand capacity` 在 `Quota.fs:314`）；`haulerDemandOf` 按**最贵的 sink**（cluster / buffer / storage 三者取最大往返）定价，不是按 storage；#279 又加了一条 **remote 地板**：`if remote * 2 >= capacity then max hired 2`（`Quota.fs:316`）。三条合起来的后果是可算的：**W12S28 的第一个新 outpost 是零 hauler 成本的** —— 它今天 demand 1,170 / 载重 1,500 = 0.78 只，但地板已经替它买了第 2 只（77% 空闲），所以 W11S28 的 1,460 demand 塞进去仍然是 2 只。第二个（W12S29）才 +1。
- **风险的第一位不是 invader core，是 remote 房里死掉的 creep，而且活体日志已经量出来了。** `observe.mjs raids` 的 20 条记录：**W12S28** 在 52,947 tick 里被 raid 20 次（14 次 Invader），**损失 0 只 creep**；**W13S28** 在 42,901 tick 里被 raid 20 次（17 次 Invader），**损失 27 只**（13 anchor / 10 reserver / 3 hauler / 1 worker，≈22,750 e），全部死在 W13S29。折算 **−0.53 e/tick 的尸体 + −0.95 e/tick 的停产 = W13S29 的 raid 税约 1.48 e/tick，占它建模净收益（13.82）的 11%**。差别在哪：**W12S27 的 47 格里有 40 格在自己房里，W13S29 的 46/65 格里有 28–37 格在房外**。**一个 2–3 跳的房，整条 trunk 都在房外** —— 所以本文的建议把 1 跳房排在前面，不是因为它们的建模净收益高一点，而是因为这一项。
- **本 sector 的入侵开关是开的，接班的 stronghold 换了地方。** `remote-candidates.md` 记的 W16S25 level-2 stronghold **已经塌了**（本日 W16S25 无 core）；现在站着的是 **W14S24 的 level-1 core**（1 tower、4 rampart、0 creep），加它已经扩出的 **W13S24 level-0 core**，`EFFECT_COLLAPSE_TIMER`（effect 1002）`endTime = 330,267` —— 距今 **27,417 tick**。`EFFECT_INVULNERABILITY`（1001）在 254,349 已过期。**W13S24 离 W13S27 只有 3 房、离 W13S26 只有 2 房**，这是把 W13S26 / W12S26 / W12S25 / W14S27 一线排除的一半理由。ADR 0043 里那句 "Tick 170,283 is a date worth watching" 与 `observe.mjs raids` 打印的 "sector clock: W15S24's collapse timer ended t170,283 … this sector's invasion switch is off unless another stronghold has spawned since" **都已经过期了**：另一座确实生成了，新日期是 **330,267**。
- **人类邻居比旧文近了一点，但仍然不构成排除项。** 323 房扫描：最近的是 **Odiodin**（自有 W14S22 已 **RCL5**，reserve 了 W13S23 / W13S22 / W14S23），最近一格 W13S23 **距两个 home 5 房**、距 W13S27 4 房；新出现的 **nightred**（自有 W9S24 RCL4，reserve W8S24 / W8S25 / W9S25）距 6–7 房；**Shibdib** 在 W18S23 RCL4，10 房。**32 房候选箱内没有任何一个房有别人的 owner、reservation 或建筑。**
- **五个房在 hop 预算内却根本走不到，declare 会被吵闹地拒绝。** 从 W12S28：**W11S26**（3 跳）、**W9S28**（3 跳）；从 W13S28：**W13S25**（3 跳）、**W13S26**（2 跳）、**W16S28**（3 跳）。这正是 ADR 0058 / #259 的场合：`Outpost.withinHopBudget`（名字层，`Colonies.fs:59`）说是，`Outpost.routable`（地形层，`Colonies.fs:75`）说不是，`ColonyView.Refused`（`Views.fs:385`）会点名。**其中 W13S26 最值得记一笔：它对 W13S28 是 2 跳且不可达，对 W12S28 却是 3 跳且可达（绕 `W12S27>W12S26`）—— 同一个房，近的那个 home 走不到，远的那个走得到。**
- **没有一个候选房需要 ADR 0058 决策 2 说的那种"矩形外 detour"。** 逐房实测（`routes.mjs`）：把 route 搜索限制在 `{home} ∪ {outpost} ∪ transitBetween` 内，与放开到全部有地形的房去搜，**两者答案逐房一致**。上面那五个不可达的房也不是被矩形挡的 —— 它们在**任何** ≤3 跳的链下都不可达（`hopsBetween` 的奇偶性把 detour 的长度顶到 4 跳以上）。所以 ADR 0058 那条明说的代价，在我们这一带**今天一次也没触发**。这是本文最想让人放心的一节，也是最容易想错的一节。
- **3 跳的房 reserver 仍然划得来，算过了。** 摊销分母是 `600 − 到 controller 的步数`（`Engine.claimLifetime = 600`），本箱内最远的 controller 步数是 W12S25 的 **159**，即 650/(600−159) = **1.47 e/tick**，而 reserve 带来的是每 source **+5 e/tick**（`heldOutputPerTick 10` 对 `neutralOutputPerTick 5`）。**没有一个候选房的 reserver 是亏的**，连最差的 W12S25（单源、净 3.06）也不是。反过来说：reserver 从来不是排序的依据，**hauler 和死人才是**。
- **SK 房与 sector centre 的排除今天仍然成立，而且这次是在 hop 预算之内被排除的。** `Outpost.Controller` 是必填字段（`Colonies.fs:28`）。**W14S26 距 W13S28 恰好 3 跳**（在预算内！），本日实测 3 source / 4 keeperLair / 1 extractor / **无 controller** —— 类型系统替我们拒了。W15S26（SK）与 W15S25（sector centre，Season #11 的 Reactor）都在 4 跳以上，连预算都进不来。

## 0. 方法与假设

### 0.1 步数（沿用 `remote-candidates.md` §2 的同一套假设，逐条写死在 `paths.mjs` 头部）

1. **路由自己铺**，每格走 1 tick：2C:1M 的 hauler 在路面上每格 2 fatigue、1 个 MOVE 每 tick 消 2；**沼泽按平地计价**。天然 wall 不可走，现有建筑一律忽略（只看地形）。
2. **八向移动、代价一致。**
3. **只有 1..48 的内部格可站**（ADR 0036 的 trim，ADR 0041 *"a Seam is never a tile to stand on"*）。
4. **过境按引擎自己的两 tick 走法**：从紧挨出口格 `e` 的内部格 `a` 迈上 `e`（1 tick），引擎在 tick 末把 creep 免费放到邻房的 `l` 上，再从 `l` 迈进内部格 `b`（1 tick）。边权 **2**，`e` 与 `l` 都必须非墙，**四个角格排除**（`Seam.pairsAcross`，`Geometry.fs:394`）。
5. **source 格与 controller 格在服务端地形里读作 wall**（32 房逐个验证，无一例外），所以量的是 **seat** —— 周围最便宜的可站内部格，正是 ADR 0042 放 container 的地方。
6. 从 **storage 的可站邻格以代价 0 起播**（与旧文口径一致，用于"单程"列）；hauler 的**往返**另按 §0.2 算，reserver / anchor 的 cast walk 从 **spawn 的可站邻格**起播（`castWalkTicks` 的真实起点）。
7. 边权放大 100 倍、每次过境额外加 1 —— 同 tick 数的路线里选过境最少的。

**并且，本文比旧文多一条 —— 也是它与代码对齐的关键：**

8. **多跳的价格按链算，不按自由 flood 算。** `Atlas.chainedInto`（`Atlas.fs:1540`）先在链的最后一房 flood 进 Task 的地面，再由 `foldChain`（`1506`）逐 hop 用 `carriedAcross`（`1455`）把场**播**到下一房重新 flood。所以：**走过的房不能回头**，链外的房不能借道，链本身由 `Atlas.route`（`1288`）→ `RoomName.routeBy`（`Geometry.fs:300`）决定。`chain.mjs` 复刻的是这一套。旧文 §2.2 的步数是自由 Dijkstra，**不是代码会付的价**。

### 0.2 能量（仓库自己的常量与体型规则，不用社区数字）

- `Engine`（`Rules.fs`）：`heldOutputPerTick = 10`、`neutralOutputPerTick = 5`、`creepLifetime = 1500`、`claimLifetime = 600`、`reservationCap = 5000`、`harvestPerWork = 2`、`carryPartCapacity = 50`、`maxBodyParts = 50`、`guardCap = 2`。
- **Anchor**（`Bodies.fs:259`）：`workCapOf 10 = 10/2+1 = 6`（`Bodies.fs:179`），bank 2300 下 `(2300−100)/100 = 22` 被 cap 到 6 ⇒ **6W/1C/1M = 700 e**，摊 1500 tick = **0.467 e/tick/source**。未 reserve 时 `workCapOf 5 = 3` ⇒ 3W/1C/1M = 400。
- **Hauler**（`Bodies.fs:286`）：整块 `[Carry;Carry;Move]`，块价 150。**两个 home 都是 RCL6，bank 2300 ⇒ `2300/150 = 15` 块（未触 `50/3 = 16` 的部件上限）= 30C/15M，造价 2,250，载重 1,500**，摊 **1.50 e/tick/只**。旧文的 12 块/1,200 载/1.20 摊销与 8 块/800 载/0.80 摊销都已过期。
- **Hauler 配额**（`haulerDemandOf`：`Quota.fs:178`，ADR 0049 的那一次取整在 `Quota.fs:314`）：每个 source container 的 demand = `output × max(往返到 cluster, 往返到 buffer, 往返到 storage)`，**取最贵的 sink**；全 colony 求和后 `ceilDiv demand capacity` **一次**取整；再叠 #279 的地板 `if remote*2 >= capacity then max hired 2`。因为是全房一次取整，**每个房的 hauler 成本是边际的、顺序相关的** —— 见 §4 的阶梯。
- **Reserver**（`Bodies.fs:332`、`Quota.fs:457`）：整块 `[Claim;Move] = 650`，claims = `ceil((5000 − ticksToEnd) / 600) |> max 1`。**摊销分母是 `600 − 到 controller 的步数`**，不是 600。这条形式上很像手算，实际上是自洽的：一个 k CLAIM 的身体每 tick 加 k tick reservation，活 600 tick 里有 `walk` tick 在走路，所以一个周期净得 `k(600−walk) − 600`；deficit-based 的 sizing 会把 k 稳定在 `600/(600−walk)`，于是每 tick 成本 = `k × 650 / 600 = 650/(600−walk)`。**旧文 §3 的那条公式是对的，本文重新推了一遍。**
- **Guard**（`Bodies.fs:309`、`Quota.fs:432`，ADR 0056）：块 `[Tough;Move×5;Attack×3;Heal] = 750`，bank 2300 ⇒ 3 块 = **2,250 e**，一个 raid 买 1–2 只。**只在 threat 站在房里的那些 tick 雇**，所以它不是稳态成本，而是 §5 那笔 raid 税的一部分（本文把它算进 raid 税，不单列一列）。
- **container 衰减 0.5 e/tick/个**（无主房；`remote-mining.md` §1.3，**本文未核源，标 unverified**）。
- **路面维护**：`remote-candidates.md` §3 用的是 `0.001×D + 0.001×P×haulers`。本文按引擎常量重推得到的是 `0.001×D`（`ROAD_DECAY_AMOUNT 100 / ROAD_DECAY_TIME 1000` ÷ `REPAIR_POWER 100`）加上 **`0.01×P` 每只持续行走的 hauler**（`ROAD_WEAROUT = 1` hit/部件/步 ÷ 100）—— 也就是**磨损那一项比旧文大 10 倍**：45 部件的 hauler 是 **0.45 e/tick/只**，不是 0.045。这两组常量**都不在仓库里**（`Rules.fs` 只有 `RepairTrigger` 之类的 tunable，没有 road 衰减常量），所以**两条都标 unverified**，本文不把它算进下表的"净"，而在 §4 末尾单列敏感度：按本文的推法，一个吃 2 只 hauler 的房要再扣约 **0.9 + 0.07 = 1.0 e/tick**，把 2 跳单源房从 4.7 压到 3.7。**这一项值得单开一张 ticket 去核。**

## 1. 32 房普查（tick 302,850，`survey.mjs` / `detail.mjs`）

两个 home 的 Manhattan（名字网格）≤ 3 的并集 = 32 房。**箱内除我们自己以外，没有任何 owner、reservation、hostile structure 或他人 creep。**

| 房 | 类型 | source（引擎 id 尾 6 位 @ 坐标） | controller | 常规矿 | Thorium | 相对两个 home |
|---|---|---|---|---|---|---|
| W10S27 / W10S28 / W10S29 / W11S30 / W12S30 / W13S30 / W14S30 | highway | 无 | **无** | — | — | 只能当 transit |
| W12S31 / W13S31 | out of borders | — | — | — | — | 排除 |
| W11S26 | normal | `…95e9`@46,10 `…95eb`@44,38 | `…95ea`@33,23 | O d2 35k | `11,6` d3 22k | W12S28 3 跳，**不可达** |
| W11S27 | normal | `…95ed`@13,28 `…95ef`@46,33 | `…95ee`@14,29 | Z d2 35k | `47,12` d3 22k | W12S28 2 跳 |
| W11S28 | normal | `…95f1`@33,15 | `…95f2`@8,16 | X d2 35k | `40,17` d2 10k | **W12S28 1 跳（东，接缝 2 格）** |
| W11S29 | normal | `…95f5`@7,33 | `…95f4`@30,29 | H d1 15k | `39,4` **d4 45k** | W12S28 2 跳 |
| W12S25 | normal | `…949f`@3,7 | `…94a0`@17,26 | L d3 70k | `46,23` d3 22k | W12S28 3 跳 |
| W12S26 | normal | `…94a3`@41,40 | `…94a2`@8,32 | U d2 35k | `33,39` d3 22k | W12S28 2 跳 |
| **W12S27** | normal | `…94a6`@16,45 | `…94a5`@37,43 **我们 reserve 到 307,332** | U d2 35k | `24,16` d3 22k | W12S28 1 跳，**已 declare** |
| **W12S28** | **home** | `…94a8`@17,40 `…94aa`@9,44 | `…94a9`@5,41 **RCL6** | O d3 70k | `26,5` d3 22k | — |
| W12S29 | normal | `…94ad`@40,43 | `…94ac`@15,36 | X d3 70k | `12,27` d2 10k | **W12S28 1 跳（南，接缝 5 格）** |
| W13S25 | normal | `…9357`@20,15 `…9358`@32,28 | `…9359`@19,37 | H d2 35k | `24,30` d3 22k | W13S28 3 跳，**不可达** |
| W13S26 | normal | `…935c`@35,23 | `…935b`@33,23 | O d2 35k | `45,15` d3 22k | **W13S28 2 跳不可达 / W12S28 3 跳可达** |
| W13S27 | normal | `…935f`@25,20 | `…935e`@26,15 | L d3 70k | `38,43` d2 10k | **W13S28 1 跳（北，接缝 34 格）** |
| **W13S28** | **home** | `…9361`@18,4 `…9362`@16,7 | `…9363`@24,17 **RCL6** | O d3 70k | `42,30` d3 22k | — |
| **W13S29** | normal | `…9365`@29,6 `…9366`@14,29 | `…9367`@15,41 **我们 reserve 到 307,257** | O d1 15k | `38,23` d2 10k | W13S28 1 跳，**已 declare** |
| W14S26 | **SK** | `…91ed`@42,6 `…91ef`@3,14 `…91f4`@32,39 | **无** | K d2 35k（带 extractor） | 无 | W13S28 **3 跳（预算内）**，4 lair，`Outpost.Controller` 拒 |
| W14S27 | normal | `…91f7`@15,37 | `…91f6`@43,27 | H d3 70k | `41,7` d2 10k | W13S28 2 跳 |
| W14S28 | normal | `…91f9`@6,8 | `…91fa`@22,15 | U d3 70k | `2,29` d3 22k | **W13S28 1 跳（西，接缝 21 格）；W15S28 的 transit room** |
| W14S29 | normal | `…91fd`@12,33 `…91fe`@31,35 | `…91fc`@29,5 | Z d2 35k | `31,30` d2 10k | W13S28 2 跳 |
| W15S27 | normal | `…9011`@14,28 | `…9010`@6,9 | U d3 70k | `8,24` d2 10k | W13S28 3 跳 / **W15S28 1 跳** |
| **W15S28** | normal | `…9014`@6,30 `…9013`@10,19 | `…9015`@25,31 | O d3 70k | `29,12` **d3 22k** | W13S28 2 跳，**已 declare，将成第三 colony** |
| W15S29 | normal | `…9017`@18,20 | `…9018`@12,34 | H d2 35k | `25,2` d1 3k | W13S28 3 跳 / **W15S28 1 跳** |
| W16S28 | normal | `…8e48`@30,29 | `…8e49`@44,44 | Z d2 35k | `15,31` d2 10k | W13S28 3 跳，**不可达** |
| W9S28 | normal | `…9798`@29,6 `…9799`@28,8 | `…979a`@6,35 | Z d1 15k | `47,35` **d4 45k** | W12S28 3 跳，**不可达** |

（完整 24 位 id 在 `survey.json`；§7 的 `Outpost` 记录里给的是完整 id。）

**Thorium 仍然不是选房依据**（旧文 §14 那条结论不变）：箱内每个 normal 房都有一处 Thorium，但 `extractor` 需要 RCL6 的**自有** controller，reserve 挖不出来（引擎侧本文未核源，标 **unverified**）。Thorium 是"claim 成 colony"的依据 —— W15S28 的 `29,12` d3 22,000 正是第三个 colony 选它的一半理由 —— 不是"declare 成 outpost"的依据。

## 2. 多跳在代码里的真实边界（读代码 + 本日实测）

### 2.1 一次 declaration 要过的三道门

| 门 | 在哪 | 问什么 | 拒了会怎样 |
|---|---|---|---|
| `Outpost.withinHopBudget` | `Colonies.fs:59` | `RoomName.hopsBetween home room`（名字网格 Manhattan）在 `1..MaxHops` 内。**只看名字**，所以能在读任何地形之前问。 | — |
| `Outpost.routable` | `Colonies.fs:75` | 上一条**且** `RoomName.routeBy linked MaxHops home room |> Option.isSome`。`linked` 由 `World.linked`（`Sightings.fs:405`）提供，读的是 world 自己的 border map，走 `Seam.joinedBy`。 | `World.scanOf`（`Sightings.fs:432`）把它从 outpost 列表里滤掉：不投影、不铺 furniture、不 pool、不雇 reserver / guard。 |
| `Outpost.refused` | `Colonies.fs:99` | 上一条的补集，按房名列出。 | `ColonyView.Refused`（`Views.fs:385`）→ layout record → `observe.mjs layout`。**吵闹的那一半就是这条。** |

**注意 `observe.mjs` 的措辞已经落后于 ADR 0058**：`scripts/observe.mjs:799-802` 打印的还是 *"N declared outposts that do not border {home}"* / *"not a neighbour of {home}, so it is worked by nobody"*，而今天的拒绝理由可能是"在预算内但没有链"。本日两个 colony 的 layout record 都是空拒绝（`observe.mjs layout` 打 "every declared outpost borders this home"），所以这句话现在还没被读错，但它会。**这是一张小 ticket。**

### 2.2 chain 怎么定出来

`Atlas.route`（`Atlas.fs:1288`）= `memoised atlas.Routes (fromRoom, toRoom)` 包着 `RoomName.routeBy`，`linked` 是 `Seam.joinedBy (ringWalkable here) (ringWalkable far)`。**记忆化按有序对，`None` 也记**（`Atlas.fs:1289`）—— 所以 `home→outpost` 和 `outpost→home` 是两个条目，可以是两条不同的链（见下）。

`routeBy`（`Geometry.fs:300`）的三个可观察性质，本文都复刻并实测过：

1. **hop 预算的算法是链长而不是 hop 数**：`chainLength = List.length (snd (List.head frontier))`，`if chainLength > maxHops then None`，且这个判断在**展开 frontier 之前**。一条 n 房的链跨 n−1 条边界，所以 `MaxHops = 3` 允许**最多 4 房 / 3 次过境**。
2. **先试目标，再展开**：`if List.contains toRoom (adjacent room) && linked room toRoom` 在前（`Geometry.fs:335`）—— 一跳的 route 只要一次 `linked`，这是 ADR 0058 那 +0.10 ms 的来源之一。
3. **平局由 `adjacent` 的固定顺序破**：`[0,-1; 1,0; 0,1; -1,0]` = 北、东、南、西（`Geometry.fs:212`）。**这是本文最重要的一条发现的机制**，见 §3.2。

### 2.3 哪些房被投影 —— 以及"矩形外 detour"实测没有触发

`Outpost.roomsProjected`（`Colonies.fs:115`）= `home :: (每个 outpost 的 RoomName :: RoomName.transitBetween home RoomName)`。`transitBetween`（`Geometry.fs:274`）是**两个名字张成的名字网格矩形的内部**（去掉两端）。ADR 0058 决策 2 明说的代价是：**绕过一堵墙的 detour 落在这个矩形之外，不会被投影，因此 `route` 找不到，该 declaration 会被吵闹地拒绝。**

本文对**每个**候选房都实测了这件事（`routes.mjs`）：一次把 `linked` 限制在 `{home} ∪ {room} ∪ transitBetween` 内，一次放开到全部 32 房的地形。**两者答案逐房完全一致**，包括那五个不可达的房。原因是奇偶性：`hopsBetween` 是 Manhattan 距离，任何链的长度与它同奇偶，所以一个 2 跳房的 detour 至少 4 跳、一个 1 跳房的 detour 至少 3 跳 —— 前者已越预算，后者本箱内没有出现（所有 1 跳房的直接边界都有带）。

**结论：ADR 0058 决策 2 的代价在我们这一带今天一次也没触发。** 它仍然是真的，只是要等到出现"直接边界被整列封死、而绕一房可达"的 1 跳房才会咬人 —— 本箱内没有这样的房（`bands.mjs` 实测的 52 条边界里，被封死的 12 条全部导致**无论怎么绕都超预算**）。

### 2.4 本箱内被墙封死的边界（`bands.mjs`，52 条边界全查）

| 封死的边界 | 后果 |
|---|---|
| `W11S26 | W11S27`、`W12S26 | W11S26` | **W11S26 从我们这边不可达**（旧文 §17 的结论本日复现，机制更精确：它的南、西两面封死，只剩北面 W11S25 与东面 W10S26 出框） |
| `W11S28 | W10S28`、`W10S28 | W9S28` | **W9S28 不可达**（W10S28 这条 highway 与 W11S28 之间整列是墙），可惜 —— 它是 2 source + T d4 45k |
| `W13S26 | W13S27` | **W13S26 对 W13S28 不可达（2 跳）**，但对 W12S28 可达（3 跳，绕 `W12S27>W12S26`） |
| `W13S25 | W13S26`、`W13S25 | W12S25` | **W13S25 不可达** |
| `W16S28 | W15S28` | **W16S28 不可达**；顺带：**W15S28 的东门天然是封的**，它将来只有 3 个出口邻房 |
| `W14S27 | W14S28` | W14S27 只能从 W13S27 进（接缝**只有 2 格**，26..27），是本箱最窄的通道之一 |
| `W12S27 | W11S27`、`W11S28 | W11S29`、`W13S29 | W13S30`、`W12S30 | W12S31` | 分别把 W11S27 逼上 `W11S28` 那条 2 格接缝、把 W11S29 逼上 `W12S29`、封住 W13S29 的南门 |

窄接缝一览（会成为 raid 时的瓶颈，也会成为 `Seam` 带上唯一的过境格）：**`W12S28|W11S28` 只有 2 格（y 31–32）**、`W13S27|W14S27` 2 格、`W14S29|W14S30` 2 格、`W13S29|W14S29` 9 格、`W11S27|W11S28` 5 格、`W12S28|W12S29` 5 格。宽的：`W13S29|W12S29` 41 格、`W14S28|W14S29` 44 格、`W12S28|W12S27` **36 格**、`W13S28|W13S27` **34 格**、`W13S28|W12S28` **19 格**。**后三个与 ADR 0041 写下的 "36 tiles north (W12S27) and 19 tiles west (W13S28)" 逐字相符**，这是本文测量方法与仓库 Atlas 一致的第一处交叉验证。

### 2.5 多跳对定价与体型的影响

- `pricedAcrossInto`（`Atlas.fs:1752`）→ `joinedAlong`（`1735`）：`match route atlas fromRoom toRoom with | Some(_ :: (next :: _ as onward)) -> match seams atlas fromRoom next with | [] -> None | band -> joinedAcross … (far onward)`。**近腿仍然是 creep 自己那一房的 flood，远腿变成一条链**（`chainedInto`），join 本身**一个字没改** —— 一跳的价格与 ADR 0058 之前**逐位相同**（ADR 0058 Consequences 第一条）。本文的 1 跳步数全部复现旧文，就是这条的实证。
- `haulRoundTripTicks`（`Atlas.fs:2248`）：**两次过链**，装载腿和空载腿各一次（ADR 0029：两段是两次旅程）。铺满路之后两个 factor 每格都是 1 tick，所以往返 = 2 × 单程 —— 本文实测的 `observe.mjs` 活体读数与这条相符（见 §3.3）。
- `castWalkTicks`（`Atlas.fs:2327`）→ `castAlong`（`2298`）：**同一个 `foldChain`，从 spawn 那头起播**。跳数越多，anchor / reserver / guard 的到岗时间越长；reserver 的命是白烧的那一段。
- **reserver 的摊销分母 = `600 − 到 controller 的步数`。** 本箱内 controller 步数（从 spawn 起播，`econ.mjs`）：1 跳房 44–77，2 跳房 68–129，3 跳房 87–159。**即使 159 步，也只是 1.47 e/tick，对一个 reserve 带来 +5 或 +10 e/tick 的房完全划得来。** 三跳的房不划算的原因**不是 reserver**，是 hauler 和 raid 税。
- **anchor / guard 的体型与跳数无关**（都是 bank 的函数），所以多跳不改变它们的单价，只改变"死一只要多久才能补上"。

## 3. 步数（`paths.mjs` + `chain.mjs`）

### 3.1 三处交叉验证（外加一处活体验证）

1. **`W12S28 → W12S27 16,45` 算出 47 tick，seat `15,44`** —— 与 ADR 0042 的 *"W12S27 walks 40 of 47 tiles inside our own room"* 和旧文 §2.1 完全一致，seat 也正是今天 container 真站的那格。
2. **`W12S28 → W13S28` 两个 source 算出 47 / 56 单程** —— 与旧文 §2 的交叉验证、`remote-mining.md` 的 "92 与 112 tick 往返" 相符。
3. **旧文 §2.1 那五个 1 跳房的步数本文逐个复现**：W13S29 `46/65`、W13S27 `60`、W14S28 `65`、W11S28 `66`、W12S29 `67`。**一格不差。**
4. **活体验证（新增，比前三条更强）**：`observe.mjs quotas` 打印的是**运行中的 bot 自己算的**每个 container 的三条 sink 腿。对比：

| container | 活体（cluster / buffer / storage） | 本文脚本（最贵 sink 往返） | 差 |
|---|---|---|---|
| W12S28 `16,39` | 6 / **16** / 4 → demand 160 | 16 → 160 | **0** |
| W12S28 `10,43` | 4 / **6** / 6 → demand 60 | 6 → 60 | **0** |
| W12S27 `15,44` | 91 / 89 / **95** → demand 950 | 94 → 940 | 1 tick |
| W13S28 `18,3` | 30 / **40** / 28 → demand 400 | 40 → 400 | **0** |
| W13S28 `15,8` | 6 / **12** / 4 → demand 120 | 12 → 120 | **0** |
| W13S29 `15,28` | 130 / **132** / 132 → demand 1320 | 130 → 1300 | 2 tick |
| W13S29 `28,6` | 93 / **95** / 95 → demand 950 | 92 → 920 | 3 tick |

**全 colony：活体 1,170 对本文 1,160（W12S28）、活体 2,790 对本文 2,740（W13S28），误差 ≤ 2%。** 下表的往返列可以当作代码会算出来的数用，剩下的 1–3 tick 差来自本文按纯地形定价而活体的 Atlas 读的是带建筑与 traffic 的格。

### 3.2 tie-break 的代价（`tiebreak.mjs`）—— 本文最反直觉的一节

对每个 ≥2 跳的候选房，把矩形内**所有**等长（最短跳数）的链逐条定价，与 `routeBy` 实际选的那条对比：

| home → 房 | 代码选的链 | 代码的价 | 矩形内最便宜的链 | 最便宜的价 | 代码贵了 |
|---|---|---|---|---|---|
| W13S28 → **W15S27** | `W13S27>W14S27` | **146** | `W14S28>W15S28` | 107 | **+39（36%）** |
| W13S28 → **W15S29** | `W13S29>W14S29` | **141** | `W14S28>W15S28` | 101 | **+40（40%）** |
| W12S28 → **W13S29** | `W12S29` | **103/95** | `W13S28` | 54/73 | **+49/+22** |
| W12S28 → **W14S29** | `W12S29>W13S29` | **152/137** | `W13S28>W13S29` | 112/97 | **+40/+40** |
| W13S28 → **W14S29** | `W13S29` | **104/89** | `W14S28` | 86/84 | **+18/+5** |
| W13S28 → **W12S29** | `W12S28` | **121** | `W13S29` | 113 | +8 |
| W13S28 → **W11S29** | `W12S28>W12S29` | **109** | （代码选的就是最便宜的） | 109 | 0 |

机制：`adjacent` 的顺序是**北、东、南、西**，BFS 先试目标再展开，所以第一条到达目标的链就赢。从 W13S28 出发，北（W13S27）永远排在西（W14S28）前面 —— 于是每个"西南/西北"方向的目标都被优先塞进那条**从北绕**的链，哪怕西边那条短 40 tick。ADR 0058 决策 1 只承诺 *"a function of the world and not of the search"*（**确定**），从没承诺**最短**；这一节是那个承诺的价签。

**三条后果，都要写进 declare 的判断里：**

1. **对角（L 形）多跳房的报价要用代码选的那条链**，不能用"看地图上最近的走法"。本文 §4 全表都是代码的价。
2. **W15S27 / W15S29 因此明确不给 W13S28**：3 跳、代码走最贵那条链（146 / 141）。它们对 **W15S28 是 1 跳**，等第三个 colony 站起来再收 —— `Colony.declared` 里那条注释（`Colonies.fs` 第三个 colony 的 comment）已经这么写了，本文给出它的数字依据。
3. **如果哪天想把某个 L 形房的链掰过来**，能动的旋钮只有 `adjacent` 的顺序（会全局改变所有平局，不可接受）或给 `route` 加一层"按 walk 定价选链"（那是一次 flood 换一次 route，正是 ADR 0058 拒绝的成本）。**所以这是一条设计上的既定事实，不是 bug** —— 但它属于"代码今天做不到的事"，见 §6。

### 3.3 逐房路径与要铺的路（`trunk.mjs`）

沿代码自己选的链回溯，逐房拆分。"待铺"是**上界**：回溯在等价格里任取一条，实际 Layout 会尽量贴现有路网（`already paved` 那一列因此偏小）。

| home → 房 | 链（接缝宽度） | source | seat | seats | 单程 | 逐房格数 | 独立 trunk 格 | 待铺 ≈ e |
|---|---|---|---|---|---|---|---|---|
| W12S28 → **W11S28** | `W11S28`(2) | 33,15 | **32,14** | **1** | **66** | 自家 34 / W11S28 32 | 66 | 62 ≈ 18,600 |
| W12S28 → **W12S29** | `W12S29`(5) | 40,43 | **41,44** | **1** | **67** | 自家 23 / W12S29 44 | 67 | 63 ≈ 18,900 |
| W12S28 → **W11S29** | `W12S29`(5)`>W11S29`(17) | 7,33 | **6,32** | 3 | **55** | 自家 23 / W12S29 13 / W11S29 18 | 54 | 50 ≈ 15,000 |
| W12S28 → W11S27 | `W11S28`(2)`>W11S27`(5) | 13,28 / 46,33 | 13,27 / 45,34 | 5 / 1 | 97 / 90 | 自家 34 / W11S28 31 / W11S27 24–31 | 134 | 130 ≈ 39,000 |
| W12S28 → W13S27 | `W13S28`(19)`>W13S27`(34) | 25,20 | 24,19 | 2 | 82 | 自家 17 / W13S28 23 / W13S27 41 | 81 | 79 ≈ 23,700 |
| W12S28 → W12S26 | `W12S27`(36)`>W12S26`(35) | 41,40 | 42,39 | 1 | 100 | 自家 41 / W12S27 48 / W12S26 10 | 99 | 96 ≈ 28,800 |
| W12S28 → W13S26 | `W12S27`(36)`>W12S26`(35)`>W13S26`(32) | 35,23 | 36,22 | 1 | 142 | 41 / 48 / 38 / 13 | 140 | 137 ≈ 41,100 |
| W12S28 → W14S27 | `W13S28`(19)`>W13S27`(34)`>W14S27`(**2**) | 15,37 | 16,38 | 1 | 142 | 17 / 23 / 41 / 59 | 140 | 138 ≈ 41,400 |
| W13S28 → **W13S27** | `W13S27`(34) | 25,20 | **24,19** | 2 | **60** | 自家 10 / W13S27 50 | 60 | 56 ≈ 16,800 |
| W13S28 → **W14S28** | `W14S28`(21) | 6,8 | **6,9** | 3 | **65** | 自家 14 / W14S28 51 | 65 | 65 ≈ 19,500 |
| W13S28 → W14S27 | `W13S27`(34)`>W14S27`(**2**) | 15,37 | 16,38 | 1 | 120 | 10 / 50 / 59 | 119 | 115 ≈ 34,500 |
| *W13S28 → W15S28（已 declare）* | `W14S28`(21)`>W15S28`(29) | 10,19 / 6,30 | 11,18 / 6,29 | 2 / 4 | 101 / 106 | 14 / 48 / 38–43 | 136 | 136 ≈ 40,800 |
| W13S28 → **W14S29** | `W13S29`(12)`>W14S29`(9) | 12,33 / 31,35 | 13,33 / 30,36 | 4 / 2 | **104 / 89** | 37 / 24 / 27–42 | 146 | 141 ≈ 42,300 |
| W13S28 → W15S27 | `W13S27`(34)`>W14S27`(**2**)`>W15S27`(20) | 14,28 | 13,29 | 1 | 146 | 10 / 50 / 48 / 36 | 144 | 140 ≈ 42,000 |
| W13S28 → W15S29 | `W13S29`(12)`>W14S29`(9)`>W15S29`(18) | 18,20 | 19,21 | 5 | 141 | 37 / 24 / 48 / 30 | 139 | 134 ≈ 40,200 |

（W12S28 → W12S25、W13S28 → W11S28 / W11S29 / W12S26 / W12S27 / W12S29 / W11S27 见 `econ.json`，全部落在"劝退"档。）

**`W11S28` 与 `W12S29` 的 source 各只有 1 个 seat**：6W 的 Anchor 正好占满，**没有换班余地** —— 一只 anchor 死了，接班的那只要等这只的尸体清掉才能站上去。这条在 §5 的 raid 账里有分量。

## 4. 经济账（ADR 0042 / 0049 / 0056 口径，`econ.mjs` / `ladder.mjs`）

### 4.1 全表（两个 home 都 RCL6 / bank 2300 / hauler 30C15M 载 1,500）

`hauler(n)` 是**独立**边际（把该房单独加到今天的 colony 上）；真正的顺序相关成本见 §4.2。`净` = 毛 − anchor − reserver − hauler − container 衰减，**不含**路面维护（§0.2 的 unverified 项）与 raid 税（§5）。

| 房 | home | 跳 | 源 | 单程（storage） | 往返（最贵 sink） | ctrl 步 | 毛 | anchor | reserver | hauler(n) | decay | **净 e/tick** | **净/hauler** |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| *W12S27（在跑）* | W12S28 | 1 | 1 | 47 | 94 | 44 | 10 | 0.47 | 1.17 | 1.50(1) | 0.5 | **6.36** | 6.36 |
| *W13S29（在跑）* | W13S28 | 1 | 2 | 46/65 | 92/130 | 77 | 20 | 0.93 | 1.24 | 3.00(2) | 1.0 | **13.82** | 6.91 |
| **W11S28** | W12S28 | **1** | 1 | 66 | 146 | 51 | 10 | 0.47 | 1.18 | 1.50(1) | 0.5 | **6.35** | **6.35** |
| **W13S27** | W13S28 | **1** | 1 | 60 | 132 | 64 | 10 | 0.47 | 1.21 | 1.50(1) | 0.5 | **6.32** | **6.32** |
| **W14S28** | W13S28 | **1** | 1 | 65 | 144 | 68 | 10 | 0.47 | 1.22 | 1.50(1) | 0.5 | **6.31** | **6.31** |
| **W12S29** | W12S28 | **1** | 1 | 67 | 148 | 68 | 10 | 0.47 | 1.22 | 1.50(1) | 0.5 | **6.31** | **6.31** |
| **W11S29** | W12S28 | 2 | 1 | **55** | 124 | 68 | 10 | 0.47 | 1.22 | 1.50(1) | 0.5 | **6.31** | **6.31** |
| **W14S29** | W13S28 | 2 | **2** | 104/89 | 180/176 | 85 | 20 | 0.93 | 1.26 | 4.50(3) | 1.0 | **12.30** | 4.10 |
| *W15S28（已 declare）* | W13S28 | 2 | **2** | 101/106 | 216/226 | 90 | 20 | 0.93 | 1.27 | 4.50(3) | 1.0 | **12.29** | 4.10 |
| W11S27 | W12S28 | 2 | **2** | 97/90 | 208/194 | 100 | 20 | 0.93 | 1.30 | 4.50(3) | 1.0 | **12.27** | 4.09 |
| W13S29（对 W12S28） | W12S28 | 2 | 2 | 103/95 | 108/146 | 97 | 20 | 0.93 | 1.29 | 3.00(2) | 1.0 | 13.77 | 6.89 |
| W14S27 | W13S28 | 2 | 1 | 120 | 252 | 68 | 10 | 0.47 | 1.22 | 3.00(2) | 0.5 | **4.81** | 2.41 |
| W11S28（对 W13S28） | W13S28 | 2 | 1 | 120 | 240 | 102 | 10 | 0.47 | 1.31 | 3.00(2) | 0.5 | 4.73 | 2.36 |
| W12S29（对 W13S28） | W13S28 | 2 | 1 | 121 | 242 | 119 | 10 | 0.47 | 1.35 | 3.00(2) | 0.5 | 4.68 | 2.34 |
| W12S26 | W12S28 | 2 | 1 | 100 | 200 | 129 | 10 | 0.47 | 1.38 | 3.00(2) | 0.5 | **4.65** | 2.33 |
| W14S27（对 W12S28） | W12S28 | 3 | 1 | 142 | 284 | 87 | 10 | 0.47 | 1.27 | 3.00(2) | 0.5 | 4.77 | 2.38 |
| W11S29（对 W13S28） | W13S28 | 3 | 1 | 109 | 218 | 119 | 10 | 0.47 | 1.35 | 3.00(2) | 0.5 | 4.68 | 2.34 |
| W13S26 | W12S28 | **3** | 1 | 142 | 284 | 144 | 10 | 0.47 | 1.43 | 3.00(2) | 0.5 | **4.61** | 2.30 |
| W15S29 | W13S28 | 3 | 1 | 141 | 216 | 146 | 10 | 0.47 | 1.43 | 3.00(2) | 0.5 | 4.60 | 2.30 |
| W12S26（对 W13S28） | W13S28 | 3 | 1 | 122 | 244 | 152 | 10 | 0.47 | 1.45 | 3.00(2) | 0.5 | 4.58 | 2.29 |
| W15S27 | W13S28 | 3 | 1 | 146 | 304 | 154 | 10 | 0.47 | 1.46 | 3.00(2) | 0.5 | **4.58** | 2.29 |
| W11S27（对 W13S28） | W13S28 | 3 | 2 | 151/144 | 302/288 | 151 | 20 | 0.93 | 1.45 | 6.00(4) | 1.0 | 10.62 | 2.65 |
| W14S29（对 W12S28） | W12S28 | 3 | 2 | 152/137 | 248/244 | 136 | 20 | 0.93 | 1.40 | 6.00(4) | 1.0 | 10.67 | 2.67 |
| W15S28（对 W12S28） | W12S28 | 3 | 2 | 149/154 | 298/308 | 135 | 20 | 0.93 | 1.40 | 6.00(4) | 1.0 | 10.67 | 2.67 |
| W12S25 | W12S28 | 3 | 1 | 183 | 366 | 159 | 10 | 0.47 | 1.47 | 4.50(3) | 0.5 | **3.06** | 1.02 |
| W11S26 / W9S28 | W12S28 | 3 | — | — | — | — | — | — | — | — | — | **无路，会被拒** | — |
| W13S25 / W13S26 / W16S28 | W13S28 | 2–3 | — | — | — | — | — | — | — | — | — | **无路，会被拒** | — |

三条要读出来的：

1. **单源房的净收益几乎只是距离的函数，而且非常平**：1 跳 6.31–6.35（五个房差 0.04！）、2 跳 4.65–4.81、3 跳 3.06–4.77。**在 1 跳这一档里，排序完全不该看能量** —— 该看关门、看 seat 数、看 raid 暴露、看 spawn 带宽。
2. **双源房的绝对净收益是单源的两倍，净/hauler 却更低**（4.10 对 6.31）。旧文 §3 的第三条结论在新几何下**仍然成立且更强**：*"净/hauler 真正惩罚的是距离，不是源数"*。CPU / spawn 带宽是瓶颈时，1 跳单源房赢；纯能量是瓶颈时，双源房赢。
3. **`W11S29` 是本轮最漂亮的一个数字**：2 跳，但单程 **55 tick** —— 比五个 1 跳房里的四个都近（只有 W13S29 的 46 和 W12S27 的 47 更近），因为 `W12S28>W12S29>W11S29` 那两条接缝（5 格 / 17 格）刚好排在一条近对角线上。**"跳数"和"步数"在这里公开分家了**，这是旧文那条"两房外的房 100–164 步、不值得"最直接的反例。

### 4.2 边际 hauler 阶梯（`ladder.mjs`，ADR 0049 一次取整 + #279 地板）

活体基线（`observe.mjs quotas`，tick 303,107）：**W12S28 demand 1,170 → 2 只**（`ceil(1170/1500) = 1`，被 #279 的地板顶到 2）、**W13S28 demand 2,790 → 2 只**。

```
W12S28（今天 1,170 / 2 只）
  + W11S28   +1,460 →  2,630 = 1.75 载 → 2 只  (+0)   ← 白送
  + W12S29   +1,480 →  4,110 = 2.74 载 → 3 只  (+1)
  + W11S29   +1,240 →  5,350 = 3.57 载 → 4 只  (+1)
  (+ W11S27  +4,020 →  9,370 = 6.25 载 → 7 只  (+3))

W13S28（今天 2,790 / 2 只）
  + W15S28   +4,420 →  7,210 = 4.81 载 → 5 只  (+3)   ← 已 declare，账已经开始付
  + W14S28   +1,440 →  8,650 = 5.77 载 → 6 只  (+1)
  + W13S27   +1,320 →  9,970 = 6.65 载 → 7 只  (+1)
  − W15S28（独立后离开 mother 列表）−4,420 → 5,550 = 3.70 载 → 4 只  (−3)
  + W14S29   +3,560 →  9,110 = 6.07 载 → 7 只  (+3)
```

**要读出来的三条：**

1. **W12S28 的第一个新 outpost 是免费的。** #279 的 remote 地板（`Quota.fs:294-316`：*"a haul that crosses a Seam is never one body"*）已经替它买了第 2 只 hauler，而那只今天只干 78% 的活。**W11S28 的 1,460 demand 正好塞进那 22% 的空闲里**（2,630/3,000 = 88%）。这是旧文 §3 第一条"hauler 取整是最大的一项非线性"在 RCL6 + #279 下的新形态 —— 旧文预言 *"W12S28 升到 RCL6 会把配额压回 1 只"*，方向对了，但它没有 #279，所以低估了这次白送。
2. **W13S28 已经在为 W15S28 付 3 只 hauler（4.5 e/tick）**，而它今天 `target 11 / living 7 / casting 1 / deficit 3` —— **已经欠 3 只身体**，reserver 缺 1、anchor 缺 2。再往里塞房只会拉大这个缺口。所以建议里 **W13S27 排在 W15S28 独立之后**：那一刻 hauler 从 7 掉回 4，spawn 队列腾出来。
3. **7 只 30C/15M 的 hauler 是 315 个部件、每只 135 tick 的 casting 时间、15,750 e 的重置成本。** 单 spawn 的 colony 到这个数量级就该问"是不是该先起 link，而不是再 declare 一个房"了 —— 那是另一张 ticket，但这张表是它的输入。

### 4.3 路面维护的敏感度（unverified）

按 §0.2 本文重推的系数，一个 trunk 100 格、吃 2 只 45 部件 hauler 的房要再扣 `0.001×100 + 0.01×45×2 = 0.1 + 0.9 = 1.0 e/tick`。对上表的影响：1 跳单源房 6.3 → **5.3**（吃 1 只时 −0.55）、2 跳单源房 4.7 → **3.7**、3 跳单源房 4.6 → **3.5**。**排序不变，但 3 跳单源房从"边缘可做"掉到"基本不值得"**。这两组常量既不在 `Rules.fs` 也没在本文核源，**整段标 unverified**。

## 5. 风险

### 5.1 活体 raid 日志 —— 这一节是本文最有说服力的风险数据

`observe.mjs raids --colony …`（各 20 条，ring 的全长）：

| colony | 记录跨度 | raid 次数 | 其中 Invader | **损失 creep** | 折算 |
|---|---|---|---|---|---|
| **W12S28**（outpost W12S27，47 步里 40 步在自家房） | 248,579 → 301,526（52,947 tick） | 20 | 14 | **0** | — |
| **W13S28**（outpost W13S29，46/65 步里 28–37 步在房外） | 257,875 → 300,831（42,901 tick） | 20 | 17 | **27**（13 anchor / 10 reserver / 3 hauler / 1 worker，≈22,750 e） | 尸体 **−0.53 e/tick** + 停产 **−0.95 e/tick**（raid 在 W13S29 站了 2,032 tick = 跨度的 4.7%）= **raid 税 ≈ 1.48 e/tick**，占 W13S29 建模净收益的 **11%** |

两条具体记录值得抄进来：

- `t291382–291986`（**604 tick**）：**一次 raid 吃掉 12 只 creep**。
- `t300191–300503`（**312 tick**）：一只 **1 work / 5 move / 1 attack / 1 ranged_attack / 2 tough** 的 small invader —— 一只 —— 连杀 **7 只**（anchor ×3、reserver ×3、hauler ×1，≈6,300 e）。日志里 `damage: 0 hits off the Keep`：它根本没碰家门，就在 outpost 里把我们的补给线一只一只吃掉。ADR 0056 的 guard 行在这两次里都没有把房守下来（`Colony.declared` 里 W13S29 的注释记着 2026-09-08 那次是**手动**撤的，理由是 ADR 0043 的 stand-down 只认 invader **core**、不认 creep —— #257）。

**这就是 raid 频率的实测口径**：W12S28 每 ~3,780 tick 一次 Invader raid，W13S28 每 ~2,520 tick 一次。**远比 `INVADERS_ENERGY_GOAL = 100,000 / 10 e/tick ≈ 10,000 tick` 的模型频繁**（`remote-mining.md` §1.4 的那条推算，本文**实测推翻**它的频率量级；能量阈值机制本身本文未核源，标 unverified）。

**外推到多跳：raid 税几乎正比于"trunk 有多少格在房外"。** W12S27 是 7/47 在房外 → 0 死；W13S29 是 28–37/46–65 → 27 死。**一个 2 跳房的 trunk 是 100% 在房外，而且它的 creep 要穿两条接缝**（其中 `W12S28|W11S28` 只有 2 格宽 —— 一个堵在那两格上的 invader 能把整条线掐断）。所以：

> **建议次序把 1 跳房排在 2 跳房前面，主要理由是这一节，不是 §4 那 1.6 e/tick 的差。**

### 5.2 invader core / stronghold（本日实测，`threats.mjs`）

sector 门槛按 `remote-mining.md` §1.4 的读法由房名生成（`"W12S27" → ^W1\dS2\d$`，即 W10–W19 × S20–S29；`genInvaders` 的实现本文未核源，标 unverified）。**该 sector 内本日实测有两座 core：**

| 房 | level | tower | rampart | invader creep | effect |
|---|---|---|---|---|---|
| **W14S24** | **1** | 1 | 4 | 0 | 1001 (INVULNERABILITY) end **254,349**（已过期）、1002 (COLLAPSE_TIMER) end **330,267** |
| **W13S24** | 0 | 0 | 0 | 0 | 同上两个 endTime |

- **所以本 sector 的入侵开关是开的，还会开 27,417 tick**（到 330,267）。
- **`remote-candidates.md` §15 记的 W16S25 level-2 stronghold 已经塌了**（本日 W16S25 无 core），它扩出的那七个 level-0 core（含 W16S27）也一并没了。旧文那条"W16S27 离 W14S28 只有两房，`INVADER_CORE_EXPAND_TIME[2] = 3500` ⇒ ≈10,500 tick 后 W14S28 可能落 core"**已过期**。
- **新的前沿是北面**：`W13S24 → W13S27` 3 步、`→ W13S26` 2 步、`→ W12S26` 3 步、`→ W12S25` 2 步、`→ W14S27` 4 步。`INVADER_CORE_EXPAND_TIME[1]`（社区值 4,000 tick，**本文未核源，unverified**）意味着塌之前还能扩 ~6 次。**按扩张步数排的 core 风险：W12S25 / W13S26（2 步）> W13S27 / W12S26（3 步）> W14S27（4 步）> W14S28（5 步）> W11S28 / W12S29 / W14S29 / W15S28（6 步）> W11S29（7 步）。**
- **reserve 挡不住 core 落地**（`expandStronghold` 只看 `!controller.user`；`remote-mining.md` §1.5，unverified）。挡它的是 ADR 0043 的 stand-down，一个 outpost 一个门。
- **ADR 0043 的观察日期该换了，`observe.mjs` 打印的那句也该换了**：`observe.mjs raids` 现在会打 *"sector clock: W15S24's collapse timer ended t170,283, 132,824 ticks ago — this sector's invasion switch is off unless another stronghold has spawned since"*。**另一座确实生成了**（W14S24），所以那句"unless"现在指向错误的结论。新日期 **330,267**。

### 5.3 人类邻居（`users.mjs`，W6–W24 × S18–S34 共 323 房）

按到最近 home 的名字距离排：

| 距离 | 房 | 玩家 | 状态 |
|---|---|---|---|
| 4 | W13S24 | Invader | core L0 |
| 5 | W13S23 | **Odiodin** | reserve |
| 5 | W14S24 | Invader | core L1（stronghold） |
| 6 | W13S22 / W14S23 | **Odiodin** | reserve |
| 6 | W9S25 | **nightred** | reserve |
| 7 | W14S22 | **Odiodin** | 自有 **RCL5** |
| 7 | W9S24 | **nightred** | 自有 **RCL4** |
| 7 | W8S25 | nightred | reserve |
| 8 | W8S24 | nightred | reserve |
| 10 | W18S23 | **Shibdib** | 自有 RCL4 |
| 10 | W14S19 | Kamots | 自有 RCL5 |

- **Odiodin 从 RCL4 长到了 RCL5**（旧文 §16 记的是 RCL4），reserve 圈从 W13S22/W13S23/W14S23 没变 —— **扩张方向仍然是北，不是南**。他离 **W13S27 只有 4 房**，是唯一可能与我们抢北面的人。
- **nightred 是新面孔**（旧文未记）：W9S24 RCL4 + 三个 reserve，离 W12S28 **6–7 房**。他往东扩会先碰到 W11S26 / W11S27 一线 —— 而 W11S26 我们走不到，W11S27 是我们 2 跳的候选。**旧文 §15 记的 "W5S24 level-5 stronghold，前沿 W8S24" 已经换成人了**：W8S24 现在是 nightred 的 reservation。
- Kazkel 的独 MOVE 侦察 creep 本月三次进过 W12S27 / W13S29（raid log 里三条 1–48 tick 的记录，`lost 0 damage 0`）—— 侦察，不是攻击。
- **32 房候选箱内一个玩家都没有**。六个 bot 一致的"敌对邻居是布尔排除项"（`remote-mining.md` §2.1）在这里仍然不触发。

### 5.4 出口门（唯一一项不在能量账里的收益）

`genInvaders` 的 `checkExit` 拒绝邻房 `controller.user || controller.reservation` 的出口（`remote-mining.md` §1.4 / `remote-candidates.md` §4，**本文未核源，unverified**）。本日的门：

| home | N | E | S | W | 全 declare 后 |
|---|---|---|---|---|---|
| W12S28 | W12S27 ✅ reserve | **W11S28 ⬜** | **W12S29 ⬜** | W13S28 ✅ 自有 | **四门全闭** |
| W13S28 | **W13S27 ⬜** | W12S28 ✅ 自有 | W13S29 ✅ reserve | **W14S28 ⬜** | **四门全闭** |
| *（将来）W15S28* | W15S27 ⬜ | **W16S28 天然封死**（接缝 0 格） | W15S29 ⬜ | W14S28 ⬜ | 它只有 3 个门，其中一个（W14S28）本建议里就会是我们的 |

**剩下四扇门恰好是四个 1 跳房：W11S28、W12S29（W12S28 的）与 W13S27、W14S28（W13S28 的）** —— 和旧文 §13 一样，一个不多一个不少，而且**全部在本文的建议前四位**。W12S28 在 52,947 tick 里挨了 14 次 Invader raid、W13S28 在 42,901 tick 里挨了 17 次；关门是唯一能把这个数字变成零的动作。

**注意这条只保护 home 房**：outpost 自己仍会被入侵 —— W11S28 的西邻 W10S28 是 highway、无 controller ⇒ 永远是合法出口；W14S28 的南北邻 W14S27 / W14S29 也是。

### 5.5 spawn 带宽（本日活体）

| colony | target | living | casting | deficit | 缺哪一行 |
|---|---|---|---|---|---|
| W12S28 | 9 | 8 | 1 | **0** | 满编。guard 0 / reserver 1 / anchor 3 / hauler 2 / upgrader 1 / worker 2 |
| W13S28 | 11 | 7 | 1 | **3** | **reserver 缺 1、anchor 缺 2** —— W15S28 的行已经在雇了 |

**这是排序的第二理由**（第一是 §5.1）：W12S28 有余量，W13S28 没有。**新房先给 W12S28。**

## 6. 代码今天做不到的事

**ADR 0058 把"多跳不支持"换成了"多跳按 tie-break 的链定价"，这不是同一句话。**

1. **`route` 给的是确定的链，不是最便宜的链**（§3.2）。`RoomName.routeBy` 宽度优先 + `adjacent` 的 N→E→S→W 固定顺序（`Geometry.fs:212`、`:300`），所以每个 L 形目标都优先走"从北绕"那条。实测代价：W15S27 +36%、W15S29 +40%、W12S28→W13S29 +91%。**能改的只有两条路，都贵**：动 `adjacent` 的顺序（全局改变所有平局）或让 `route` 按 walk 定价选链（一次 route 变成 k 次 flood —— 正是 ADR 0058 拒绝"Recursive Seam join"时的成本论证）。**今天正确的应对是：把 L 形多跳房交给离它更近的那个 colony，而不是掰链。** 这直接支持了 W15S27 / W15S29 归 W15S28 的决定。
2. **`transitBetween` 的矩形 detour 代价是真的，但本箱内一次也没咬人**（§2.3）。奇偶性替我们挡了：一个 n 跳房的 detour 至少 n+2 跳。**要它咬人，需要一个"直接边界被整列封死、但绕一房可达"的 1 跳房** —— 本箱内没有。所以这条不该出现在任何 declare 的前置条件里，只该留在 `ColonyView.Refused` 的解释里。
3. **`observe.mjs` 的拒绝措辞落后于 ADR 0058**（`scripts/observe.mjs:799-802` 仍写 "do not border {home}" / "not a neighbour of {home}"）；`observe.mjs raids` 的 sector clock 仍指着 W15S24 与 tick 170,283，而实际接班的是 W14S24 / 330,267。**两张小 ticket。**（本文不改任何代码。）
4. **`Outpost.Controller` 必填仍然把 SK 房与 sector centre 挡在门外，而且这次是在预算内挡的**：**W14S26 距 W13S28 恰好 3 跳**，3 source + extractor，`hopsBetween` 说可以、类型说不行。这是对的（reserve 不了的房，source 只有半产），但也意味着 SK 与 Reactor 房**永远**不能借 outpost 这条路进经济层 —— 将来要挖 SK 得另开一套 mission（社区六家的一致做法）。Season #11 的 Reactor（W15S25）同理，它是 ADR 0057 / `third-colony.md` 的题目。
5. **ADR 0043 的 stand-down 只认 invader core，不认 invader creep**（#257，`Colonies.fs` 里 W13S29 的注释记着 2026-09-08 那次手动撤退）。§5.1 那 27 只死 creep 全部死在这个洞里。**多跳房会把这个洞放大**，因为它们的 trunk 100% 在房外。**在 declare 任何 2 跳房之前，#257 应该先关掉。**

## 7. 建议

### 7.1 declare 次序与前置条件

| # | 房 | home | 跳 | 净 e/tick | 边际 hauler | 前置条件 | 为什么在这个位置 |
|---|---|---|---|---|---|---|---|
| 1 | **W11S28** | W12S28 | 1 | 6.35 | **+0** | 无 | hauler 白送（#279 的地板已买单）；关 W12S28 东门；core 风险最低（≥6 步）；controller 只 51 步（reserver 最便宜的一个）。**唯一顾虑：source 只有 1 个 seat（`32,14`），换班没余地。** |
| 2 | **W14S28** | W13S28 | 1 | 6.31 | +1 | 无（它**已经**因 W15S28 而被投影成 transit room，`transitBetween W13S28 W15S28 = [W14S28]`） | **成本最低的一个**：地形、border ring、trunk 的前 14+48 格都已经因 W15S28 而在投影里，roads 与 W15S28 的 bootstrap 车流共用。关 W13S28 西门。source seat 3 个。 |
| 3 | **W12S29** | W12S28 | 1 | 6.31 | +1 | 无 | 关 W12S28 最后一扇门 ⇒ **W12S28 不再是入侵产房**（本日的 14 次/52,947 tick 应归零）。顺带它是 W11S29 的 transit room，路网共用。同样只有 1 个 seat（`41,44`）。 |
| 4 | **W13S27** | W13S28 | 1 | 6.32 | +1 | **等 W15S28 独立**（它离开 mother 列表时 W13S28 的 hauler 从 7 掉回 4）；理想上也等 #257 关掉 | 关 W13S28 最后一扇门 ⇒ 两个 home 的门全闭。接缝 34 格（本箱第二宽，几乎堵不住）。**但它离 W13S24 那个 core 只有 3 步、离 Odiodin 的 W13S23 只有 4 房** —— 两个方向的压力都在这里。 |
| 5 | **W11S29** | W12S28 | 2 | 6.31 | +1 | 先上 #3（共用 W12S29 的 trunk）；#257 关掉 | **第一个值得的多跳房**：单程 55 tick，比四个 1 跳房都近；trunk 只 54 格、待铺 ~50 格（≈15,000 e，本箱最便宜）；`T d4 45,000` 是全箱最富的 Thorium ⇒ **第四个 colony 的候选**。 |
| 6 | **W14S29** | W13S28 | 2 | 12.30 | +3 | 上面全部；W15S28 已独立；#257 已关；先观察一轮 2 跳 trunk 的 raid 损耗 | **绝对净收益最高的未取房**（2 source）。代价：+3 只 hauler、141 格待铺（≈42,300 e）、代码的链比最优链贵 18/5 tick、`W13S29|W14S29` 接缝只 9 格。**这是"可选"，不是"应该"。** |

**明确劝退（附理由）：**

- **W11S27**（W12S28 2 跳，2 source，净 12.27）：+3 hauler、130 格待铺（≈39,000 e）、而且它和 W11S28 **共用 `W12S28|W11S28` 那条只有 2 格宽的接缝** —— 一个 invader 站在 `48,31`/`48,32` 就能同时掐断两个房。留档，不做。
- **W14S27**（净 4.81）：接缝**只有 2 格**（`W13S27|W14S27`，26..27）、trunk 115 格待铺、离 core 4 步。
- **W12S26 / W13S26 / W12S25**（净 4.65 / 4.61 / 3.06）：全部朝着 W13S24 那个 core 的方向，2–3 步扩张距离；W12S25 是全箱最差的一个。
- **W15S27 / W15S29**：**给 W15S28（各 1 跳），不给 W13S28（3 跳且代码走最贵的链，146 / 141）。** `Colony.declared` 第三条记录的注释已经这么写了。
- **W11S26 / W9S28 / W13S25 / W13S26(←W13S28) / W16S28**：**declare 会被 `Outpost.routable` 吵闹地拒绝**（`ColonyView.Refused`）。W13S26 若真想要，**只能挂在 W12S28 名下**（3 跳，净 4.61）—— 不值得。
- **W14S26 / W15S26（SK）、W15S25（sector centre / Reactor）**：`Outpost.Controller` 必填，类型系统已经拒了。

### 7.2 建议的 `Outpost` 记录（引擎真实 id 与坐标，本日 `survey.json` 实测）

> 写进 `src/Core/Types/Colonies.fs` 的 `Colony.declared`。**id 必须是引擎自己的 24 位 id** —— 写短名会静默匹配不上（`Colonies.fs:16-17`）。source 的顺序照抄 `room-objects` 的返回顺序，与 `tests/Core.Tests/rooms/*.room` 的 capture 一致。

```fsharp
/// W12S28's east outpost (2026-09-10, `docs/research/multihop-outposts.md`):
/// one source across a two-tile Seam, and the marginal hauler #279's remote
/// floor already bought. Closes W12S28's east gate.
let w11s28: Outpost =
    {
        RoomName = "W11S28"
        Sources = [ "6a8caac6dd4872bccd3195f1", { Room = "W11S28"; X = 33; Y = 15 } ]
        Controller = "6a8caac6dd4872bccd3195f2", { Room = "W11S28"; X = 8; Y = 16 }
    }

/// W13S28's west outpost, and W15S28's transit room (ADR 0058): the room is
/// already projected for the chain to the candidate colony, so what this
/// declaration adds is its furniture and its rows. Closes W13S28's west gate.
let w14s28: Outpost =
    {
        RoomName = "W14S28"
        Sources = [ "6a8caaa1dd4872bccd3191f9", { Room = "W14S28"; X = 6; Y = 8 } ]
        Controller = "6a8caaa1dd4872bccd3191fa", { Room = "W14S28"; X = 22; Y = 15 }
    }

/// W12S28's south outpost: one source across a five-tile Seam, and the transit
/// room a later W11S29 would walk through. Closes W12S28's last gate.
let w12s29: Outpost =
    {
        RoomName = "W12S29"
        Sources = [ "6a8caabadd4872bccd3194ad", { Room = "W12S29"; X = 40; Y = 43 } ]
        Controller = "6a8caabadd4872bccd3194ac", { Room = "W12S29"; X = 15; Y = 36 }
    }

/// W13S28's north outpost: the widest Seam in the neighbourhood, thirty-four
/// tiles. Closes W13S28's last gate — and stands three expansions from the
/// W13S24 invader core, which is why it waits for W15S28's independence.
let w13s27: Outpost =
    {
        RoomName = "W13S27"
        Sources = [ "6a8caaaddd4872bccd31935f", { Room = "W13S27"; X = 25; Y = 20 } ]
        Controller = "6a8caaaddd4872bccd31935e", { Room = "W13S27"; X = 26; Y = 15 }
    }

/// The first two-hop outpost: W12S28 > W12S29 > W11S29, fifty-five tiles - a
/// shorter walk than four of the five one-hop rooms (ADR 0058).
let w11s29: Outpost =
    {
        RoomName = "W11S29"
        Sources = [ "6a8caac6dd4872bccd3195f5", { Room = "W11S29"; X = 7; Y = 33 } ]
        Controller = "6a8caac6dd4872bccd3195f4", { Room = "W11S29"; X = 30; Y = 29 }
    }

/// Optional, and last: two sources two hops out, priced along the chain the
/// tie-break picks (W13S29) and not the cheaper one (W14S28) - eighteen ticks
/// dearer, and three more haulers.
let w14s29: Outpost =
    {
        RoomName = "W14S29"
        Sources =
            [
                "6a8caaa1dd4872bccd3191fd", { Room = "W14S29"; X = 12; Y = 33 }
                "6a8caaa1dd4872bccd3191fe", { Room = "W14S29"; X = 31; Y = 35 }
            ]
        Controller = "6a8caaa1dd4872bccd3191fc", { Room = "W14S29"; X = 29; Y = 5 }
    }
```

seat（container 该落的格）与 anchor 该站的格，供 Layout 对照：W11S28 `32,14`（**1 seat**）、W14S28 `6,9`（3 seat）、W12S29 `41,44`（**1 seat**）、W13S27 `24,19`（2 seat）、W11S29 `6,32`（3 seat）、W14S29 `13,33`（4 seat）与 `30,36`（2 seat）。

### 7.3 每一步的前置检查（declare 之前跑一遍）

1. `observe.mjs layout --colony <home>` —— 确认 `refused` 是空的（新 declaration 若拼错了 id 或选错了房，这里点名）。
2. `observe.mjs quotas --colony <home>` —— 对照 §4.2 的阶梯，确认 hauler 数与 demand 落在预期上；确认 `deficit` 没有变大。
3. `observe.mjs raids --colony <home>` —— 确认没有 raid 正站在新房里（#257 之前 stand-down 不会替你判断）。
4. 若目标是 2 跳房：**先确认 #257 已经关掉**（stand-down 认 invader creep），否则 §5.1 那 27 只死 creep 的账会在一条 100% 暴露的 trunk 上重演。

### 7.4 与 `remote-candidates.md` 的关系

**那份文档不要删、不要改。** 本文推翻/更新它的地方，逐条列在这里：

| 旧文位置 | 旧文说 | 本文（tick 302,850） |
|---|---|---|
| §Summary 第 1 条、**§5 全节** | "能 declare 的房只有五个"、"两房外的 outpost 不支持"、"对角邻房同样不支持"、"静默失败" | **被 ADR 0058 推翻。** 1–3 跳内都能定价；不可达的房被 `Outpost.routable` **吵闹地**拒绝（`ColonyView.Refused`）。 |
| §2.2 的步数表 | W11S27 90/97、W11S29 55、W13S29(←W12S28) 54/73、W14S29 84/86、W15S28 101/106 | **口径过期**（自由 Dijkstra，不是链式定价）。代码的价：97/90、55、**103/95**、**104/89**、101/106。差最大的是 W13S29 与 W14S29（§3.2）。 |
| §3 全表 | bank 1800/1300、hauler 1,200/800 载、摊销 1.20/0.80 | **过期**：两个 home 都 RCL6 / bank 2300 / 30C15M / 1,500 载 / 1.50 摊销。且 `haulerDemandOf` 现在按**最贵 sink**定价并带 #279 的 remote 地板。 |
| §3 第 1 条 | "W12S28 升到 RCL6 会把 W11S28 与 W12S29 的配额压回 1 只" | **方向对，程度低估**：#279 的地板让 **W11S28 的边际 hauler 是 0**。 |
| §4 "Invader core / stronghold" | W16S25 level-2 stronghold，collapse 249,241，七个 level-0 core 含 W16S27；W5S24 level-5，前沿 W8S24 | **全部过期**：W16S25 已塌；现在是 **W14S24 level-1 + W13S24 level-0，collapse 330,267**，前沿在**北**；W8S24 现在是 nightred 的 reservation。 |
| §4 "入侵时刻表" | 单源房 ≈10,000 tick 触发第一次 | **实测频率高一个量级**：W12S28 每 ~3,780 tick、W13S28 每 ~2,520 tick 一次 Invader raid（各 20 条日志，4.3–5.3 万 tick 跨度）。 |
| §4 "别的玩家" | Odiodin RCL4，最近 5 房；次近 Kamots 9 房 / FR4C74LH3X 9 房 | **更新**：Odiodin **RCL5**；新增 **nightred**（W9S24 RCL4，6–7 房）；Shibdib W18S23 RCL4，10 房。**箱内仍然一个玩家都没有。** |
| §17 W11S26 | "S/E/W 三面 48 格全是 wall，从我们这边根本走不到" | **复现**（机制更精确：`W11S26|W11S27` 与 `W12S26|W11S26` 两条边界封死），并**新增三个同类**：W9S28、W13S25、W16S28。 |
| §18 SK 房 | 两个 SK 房不是候选，`Outpost.Controller` 必填 | **复现**，且**这次 W14S26 是在 hop 预算之内（3 跳）被类型系统拒的**。 |
| §1、§2.1、§4 出口门表 | 30 房事实、五个 1 跳房的步数、两个 home 各四扇门 | **本日全部复现，一格不差**（步数 46/65、60、65、66、67；接缝 36 / 34 / 19 / 12 / 21 / 5 / 2）。 |

**Thorium 那条结论不变**：reserve 挖不出 Thorium，它是"claim 成 colony"的依据。第三个 colony 选 W15S28（`29,12` T d3 22,000、到 Reactor 3 跳）正是这条的应用；**第四个的候选是 W11S29（`39,4` T d4 45,000，全箱最富）** —— 那是 ADR 0047 的题目，不是本文的。
