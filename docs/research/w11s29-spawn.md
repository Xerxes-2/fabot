# W11S29 的第一个 spawn 摆哪一格 — 全房 1,772 格实跑，而房间自己的地形把这个决定压成了一条守恒的对角线

Date: 2026-09-16（与 `fourth-colony.md` 同一批工作，**没有任何新的 API 调用**：本文只读仓库里已经 commit 的 capture `tests/Core.Tests/rooms/W11S29.room`（tick 491,477）与 `W12S29.room`）。姊妹篇：`fourth-colony.md`（选房的那一篇，本文细化它 §5 的 spawn sweep，并**确认**它 §7 的落地步骤不变）、`third-colony.md` §3（sweep 的方法来源）。动机：ADR 0047 —— nursery 的第一个 spawn 是**人类的动作**，代码不会替他选格子，而 `fourth-colony.md` §5 给出的是 stride 6 的 49 格粗筛、只按 road 数排序。

**结论先说**：**摆 `11,30`**；次席是 `fourth-colony.md` 推荐的 `12,30`，它在本文的口径下只差 **2 tick**（一条守恒和上的 110 对 108），两者的 hauler 人头数完全一样（都是 1 只）。**粗筛没有选错，它只是没有看到该看的东西**：真正被地形决定的不是 road 数，而是 (a) 一条 **`min2store + door2spawn = 108` 的守恒对角线**，(b) 一格会丢 trunk 的反例 `1,32`，(c) fixture 的 admissibility 规则**允许 23 格站在 working ground 上**。

**Verified against**（全部是本地命令，没有 API、没有 `jj` 变更、`src/` 一行未动）：

| 干了什么 | 怎么跑的 |
|---|---|
| 全房 sweep（stride 1，1,772 格），每格 3 次 `Decide.decide` | 临时文件 `tests/Core.Tests/SpawnScoutTests.fs`（跑完已删、fsproj 已恢复），`SCOUT_STRIDE=1 dotnet test --filter "FullyQualifiedName~w11s29" --logger "console;verbosity=detailed"`，原始 stdout 在 `/tmp/scout/spawn-sweep.log` |
| 复现 `fourth-colony.md` §5 的粗筛 | 同一个文件 `SCOUT_STRIDE=6`，输出在 `/tmp/scout/spawn-sweep-stride6.log`：**49 格、49 格干净、最省 road 20**，与那篇逐字一致 |
| 闸门 | `npm run format:check` 干净；`dotnet test` **1377 通过 / 1 跳过**（跳过的是本文那一格环境变量没设的 scout），删文件后 **1377/1377** |

代码侧核对（每个数字都来自这些函数，没有手算的路程）：`Decide.Layout.planLayout`（`home`/`anchor`/`controller` 三件套的 orient 门、`workingGroundIn` 的禁区、`allowanceCeiling` 的 reservation）、`Decide.Entry.decide`、`Decide.Quota.haulerDemandOf`（**本文的目标函数就是它**）、`Decide.Bodies.bodyFor haulerPattern`、`Atlas.castWalkTicks` / `Atlas.haulRoundTripTicks` / `Atlas.workArea` / `Atlas.seams` / `Atlas.workingGroundIn` / `Atlas.walkableTilesIn`、`Tests.RoomFixtures.load` + `project`、`Tests.RoomInvariantFixtures.spawnTiles` / `stride` / `colonyOf` / `placementsOf` / `tilesOfKind` / `clusteredTiles` / `withRoadsStanding` / `acrossFrom`；ADR 0011 / 0022 / 0026 / 0036 / 0040 / 0041 / 0042 / 0047 / 0049 / 0055 / 0057 / 0062 / 0063 / 0064 / 0068。

## Summary

- **摆 `11,30`**：road 20（全房最省，51 格并列）、RCL6 与 RCL7 都 `UnservedFootings = 0` / `UnroutedTrunks = 0`、ext 40→50、hauler demand 840（1 只）、门口 round trip **27**、钍 round trip **81**、spawn 自己的补给腿（source container → storage）**8**、birth tile 5、cluster 最西只到 x=6 且门的落地格上一格建筑都没有。
- **次席 `12,30`**（`fourth-colony.md` 的推荐）：**除了守恒和 110 对 108，每一项都一样** —— 同样 road 20、同样 0 损失、同样 1 只 hauler、门 30、钍 80。**所以那篇的结论是对的，误差 2 tick**；本文换格子换来的是 3 tick 的门腿，代价是 1 tick 的钍腿。
- **road 数排不出胜负，因为它 51 格并列在 20**（全房范围 20–51）。而且 **RCL6 与 RCL7 的 road 计划逐格相同**（1,772 格无一例外），这正是 ADR 0064「reservation 按 allowance 上限定尺、不读 level」透出来的样子；level 只改 extension：40 → 50，处处摆满。
- **真正的目标函数是 `Quota.haulerDemandOf`，而它在这个房里对 spawn 位几乎免疫。** 它给一个 source container 的定价是 `output × 三个 sink 里最贵的那一个`（cluster / controller buffer / storage）。W11S29 的 buffer 腿（source container `8,32` → buffer `28,27`）是 **57**，而 spawn 与 storage 无论摆哪儿都比它近 —— 所以 **547 格的 `srcDearest` 全等于 57**，source 的 haul 项恒为 `10 × 57 = 570`。**1,748 格里 1,743 格雇 1 只 hauler**，只有东南角 5 格（`48,41`–`48,44` 一带）踩到 1,500 的门槛雇第 2 只。
- **于是决定权落到两条**：钍的那一腿（mine container → storage，ADR 0057 决策 3 的 `minerRate × trip / 6`）和门的那一腿（W12S29 落地格 → spawn）。**这两条在 road=20 的 16 格上严格 1 换 1**：`min2store + door2spawn ≡ 108`，从 `10,29`（门 24 / 钍 84）一路到 `17,23`（门 45 / 钍 63）。**这是本文唯一的真发现**：房间的几何把「靠门」和「靠钍」做成了一条守恒的对角线，road 数完全看不见它。
- **本文选门那一端，理由是时钟而不是路程**：钍那一腿只在开采期付（miner 在 2,300 bank 上是 `[20 Work; 4 Move]`，20/6 ≈ 3.33 T/tick，45,000 T 约 **13,500 tick**），而门那一腿是 bootstrap 期每一趟 ferry、每一只 pioneer 都付的，而 bootstrap 决定 extractor 什么时候开工（`fourth-colony.md` 估 t750,000，赛季到 t≈1,966,000）。对角线两端的 18 tick 差，在钍那边值约 600 hauler-tick，在门那边值 ferry 往返的 5%–6%（mother 往返 324 + 门腿）。
- **road 数看不见的四件事，本文都量了**：① `1,32` 在 RCL6 与 RCL7 都**丢一条 trunk**，而且**连钍的 container 都摆不出来**（containers 3 → 2），它正是「footing cluster 被挤在开着门的那面墙上」的形状（cluster 最西 x=1、门的五格内 28 个 cluster 格、落地格上压了 3 个、birth tile 只剩 1）；② fixture 的「离 furniture ≥3」**允许 23 格站在 working ground 上** —— Upgrade 的 range 是 3（`Atlas.actionOn`），所以 controller `30,29` 那一圈**恰好 3 格远**的环（`27,26`–`27,32`、`33,26` 与 `33,29`–`33,32`、`28,26`/`29,26`、`28,32`–`32,32`，共 19 格；环上其余位置是墙或 swamp）与钍旁的 `38,4 / 38,5 / 39,5 / 40,5` 全是 upgrader 或 miner 的站位，被本文剔除；③ 192 格的 cluster 会压在门的落地格上，346 格的 cluster 伸进门的五格内；④ **这个房没有 neck，但 source 有个口袋**：`7,33` 的 Seat 只有 `6,32 / 7,32 / 8,32` 三格，每一条 trunk 都得从这个口袋嘴出来，所以「trunk 沿途最紧的 5×5 可走格数」最好也只有 **16/25**，这是 source 的属性而不是 spawn 的。
- **如果以后 declare W12S29（它唯一可能 declare 的房），结论不变。** 那条 outpost 腿的 dearest sink **也是 buffer**，`1,003` 格里 648 格恒为 **105**，`11,30` / `12,30` / `17,23` 全部落在 105；declare 之后**每一格都雇 2 只 hauler**。也就是说 outpost 这件事**不构成换格子的理由**。
- **全房 48×48 跑得动，不需要 stride。** 1,772 格 × (RCL6 一次 `decide` + RCL7 一次 + 铺好 road 再一次取 container 位) + Atlas 的 flood = **45.6 s**；再给 1,003 格算 outpost 腿 **5.8 s**；`dotnet test` 全程 **52 s**。粗筛的 stride 6 只是为了让 `RoomInvariantFixtures` 当闸门用，选格子这件事没有必要省。

## 1. 候选集合：admissibility 规则是从哪儿来的

`Layout.planLayout` 自己只要三样东西：`Atlas.homeRoom` 有名字、**spawn 的 target 落在这个房里**（`positionOf ... |> Option.filter inHome`）、`view.Controller` 在。除此之外它不挑格子：parity、cluster、reservation、trunk 全是从 spawn 那一格**推**出来的。所以「Layout 会接受的格子」= 投影能把 spawn 放上去的格子 = 引擎意义上的非 wall 非 exit 格。

真正窄一圈的是 fixture 的规则（`RoomInvariantFixtures.spawnTiles`）：**terrain = Plain**、坐标能被 stride 整除、**离 source / controller ≥ 3**。本文把 stride 抽掉，其余照抄：

| | 格数 |
|---|---|
| 投影窗口 1..48 | 2,304 |
| Plain | 1,794 |
| Swamp（fixture 不扫，**本文也没扫**，见 §7） | 217 |
| Wall | 293 |
| **Plain 且离 furniture ≥3 = 本文的候选集** | **1,772** |
| 其中 RCL6 或 RCL7 有损失 | **1**（`1,32`） |
| 其中站在 working ground 上（`Atlas.workingGroundIn` 41 格） | **23** |
| 剩下的「活」格 | **1,748**（其中 birth tile ≥3 的 1,669） |

两条要读出来的：

1. **`≥3` 这个数不是安全距离，是 harvest 的 range。** Harvest 的 range 是 1、Upgrade 是 3（`Atlas.actionOn`），所以「离 controller 恰好 3」的格子**就在 Upgrade Work Area 里面**。fixture 这么写没有错（它扫的是 Layout 的不变量，spawn 吃掉一格 upgrader 站位不违反任何不变量），但**选 spawn 的时候这 23 格必须剔掉**：ADR 0022 花了整节把 working ground 从 cluster 的排序里拿掉，人类再把 spawn 放上去是白费。
2. **working ground 要用「别处站着 spawn」的投影去读。** 一格自己的投影会把那格变成 obstacle，于是它从任何 Work Area 里消失 —— 拿自己的 atlas 问「我是不是站在 Work Area 上」永远答 false。本文用 `24,24` 的投影当参照（它离 controller 6、离 source 17，不碰任何 Work Area）。

## 2. Layout：RCL6 与 RCL7，1,772 格

方法同 `third-colony.md` §3 与 `fourth-colony.md` §5：`load` capture → `project capture spawn None` → `colonyOf loaded 6`（与 `... 7`）→ `Decide.decide`，读 `PlaceConstructionSite` 里的 Extension / Road 计数与 `Memo.UnservedFootings` / `Memo.UnroutedTrunks`；container 位另跑一次（`withRoadsStanding` 把这一格自己的 road 计划铺上去，因为 container 会让给同格的 road site，ADR 0040）。

| 量 | RCL6 | RCL7 |
|---|---|---|
| Extension | **40**，1,772 格全部摆满 | **50**，全部摆满 |
| Road | 20–51，**逐格与 RCL7 相同** | 同 RCL6 |
| `UnservedFootings` | 0（1,772 格全空） | 0 |
| `UnroutedTrunks` | 0，**除 `1,32` 的 1 条** | 同 RCL6 |
| container 位 | 3（source `6,32|7,32|8,32`、钍 `38,4|38,5|39,5|40,5`、buffer `28,27|29,29|29,30`） | — |

- **road 的 floor 是 20，并列 51 格**；`fourth-colony.md` 说的「最省 20 在 `12,30`」在 stride 6 的格子上其实是**三格并列**（`12,30` / `18,24` / `24,30`），那篇取的是坐标序的第一个。本文复现的 stride 6 输出逐字相同。
- **road6 ≡ road7** 是 ADR 0064 的直接观测：reservation 按 `allowanceOf` 的上限定尺、不读 level，所以 trunk 绕的东西不随 level 动。ADR 0063 那一轮「一次 level-up 扔掉 589 格路面」的账在这个房是 0。
- **`1,32` 是本轮的反例**（ADR 0036 的「真实地形是反例发生器」又兑现一次）：spawn 贴在开着门的那面墙上（x=1），cluster 最西 x=1、门的五格内 28 格 cluster、**3 格压在门的落地格上**、birth tile 只剩 1、source 的 trunk 丢掉、**钍的 container 也摆不出来**（3 → 2）。它值一条 ticket，归档进 `RoomInvariantFixtures` 的 `SealedDoorsteps` 一类（与 `fourth-colony.md` §10.4 的 W17S28/W17S29 五格同一档）。

## 3. 每 tick 要付的那几条腿 —— 用 `Quota.haulerDemandOf` 的口径，不是手算

`haulerDemandOf` 给一个 source container 的定价是

```
Demand = output × max(trip→cluster, trip→buffer, trip→storage)
```

**最贵的 sink，而不是平均**（那段注释自己写了为什么）；矿的那一项是 `minerRate × trip→storage / mineralHarvestCycle`，**只对 storage**。所以本文量的就是这几条 `Atlas.haulRoundTripTicks`，body 是 `Bodies.bodyFor Bodies.haulerPattern 2300`（RCL6 的 bank：300 + 40×50；30 Carry / 15 Move，载重 1,500）。

| 腿 | 谁付 | 全房范围 | 随 spawn 变？ |
|---|---|---|---|
| source container → **buffer** | 升级的每一点能量 | **57**（少数 plan 是 60/63） | **几乎不变**（buffer 是 `28,27`，与 spawn 无关） |
| source container → spawn / storage | 铸造与存货 | 6–117 | 变 |
| **`srcDearest` = 三者最贵** | 就是 quota 乘的那个数 | **57–117，其中 547 格恰好 57** | 只在 spawn 离 source 超过 57 时才变 |
| 钍 container → storage | 45,000 T 的开采期 | 0–138 | **变** |
| 门的落地格 → spawn | mother 派来的每一具身体、每一趟 ferry | 0–143 | **变** |

于是 hauler 的需求数：

| 格 | `srcDearest` | 钍腿 | demand = 10×57 + 20×钍腿/6 | 雇几只 |
|---|---|---|---|---|
| `28,14`（demand 的 floor） | 57 | 32 | **676** | 1 |
| **`11,30`（本文的决定）** | 57 | 81 | **840** | 1 |
| `12,30`（次席） | 57 | 80 | 836 | 1 |
| `17,23`（对角线另一端） | 57 | 63 | 780 | 1 |
| `48,44`（最差） | 117 | 111 | 1,540 | **2** |

**1,748 格里 1,743 格雇 1 只**。也就是说：**在这个房，spawn 位不改 hauler 的人头数**，它只改一个 1,500 分母下的小数。这一条是本文最想被读出来的话 —— 谁想用「摆得好省几只 hauler」来论证一格，这个房给不出那个论证。

### 守恒的那条对角线

把 road = 20 且 `srcDearest` = 57 的格子按门腿排开（16 格，全部 `plugged = 0`、`nearDoor = 0`、pinch 16、birth 5）：

| spawn | 门 → spawn | 钍 → storage | **和** | source → storage | spawn→源 cast | spawn→controller cast | storage |
|---|---|---|---|---|---|---|---|
| `10,29` | **24** | 84 | 108 | 9 | 2 | 16 | `9,28`（但 cluster 伸进门的五格内 4 格） |
| `11,28` | 27 | 81 | 108 | 12 | 3 | 15 | `10,27` |
| `11,29` | 27 | 81 | 108 | 11 | 2 | 15 | `10,28` |
| **`11,30`** | **27** | **81** | **108** | **8** | **2** | **15** | **`10,29`** |
| `12,27` | 30 | 78 | 108 | 15 | 4 | 14 | `11,26` |
| `12,28` | 30 | 78 | 108 | 14 | 3 | 14 | `11,27` |
| `12,29` | 30 | 78 | 108 | 11 | 3 | 14 | `11,28` |
| `13,26` / `13,27` / `13,28` | 33 | 75 | 108 | 18 / 17 / 14 | 5 / 4 / 4 | 13 | — |
| `14,26` / `14,27` | 36 | 72 | 108 | 20 / 17 | 5 | 12 | — |
| `15,25` / `15,26` | 39 | 69 | 108 | 23 / 20 | 6 | 11 | — |
| `16,24` | 42 | 66 | 108 | 26 | 7 | 10 | `15,23` |
| `17,23` | 45 | **63** | 108 | 29 | 8 | 9 | `16,22` |
| 对照 `12,30`（粗筛的推荐） | 30 | 80 | **110** | 8 | 3 | 14 | `11,29` |
| 对照 `24,30`（stride 6 并列 road 20 的第三格） | 66 | 69 | 135 | 42 | 15 | 2 | `23,29` |
| 对照 `26,28`（贴着 controller） | 72 | 63 | 135 | 48 | 17 | 0 | `25,27` |

**门每近 3 tick，钍就远 3 tick**，一路到底。房间的几何是这么长的：source 在西南 `7,33` 的墙口袋里、门在正西 x=0、controller 在东偏南 `30,29`、钍在东北 `39,4` —— 门和钍分居两头，而 source→buffer 那条恒定的 57 把 controller 从竞争里拿掉了。往东走到 `24,30` / `26,28` 之后连守恒都破了（和涨到 135），因为那时 spawn 自己的补给腿（source → storage 42–48）开始变贵。

### 两端各值多少

- **钍腿**：miner 在 2,300 bank 上是 `[20 Work; 4 Move]`（`Bodies.minerBodyFor` 的注释就写着这两个数），`minerRate = 20`，`mineralHarvestCycle = 6` ⇒ **3.33 T/tick**，45,000 T ≈ **13,500 tick 的开采期**。对角线两端差 18 tick ⇒ demand 差 60 ⇒ `60/1500 × 13,500 ≈ 540 hauler-tick`，一次性。
- **门腿**：`fourth-colony.md` 量的 mother → W11S29 haul 往返是 **324**（W13S28 三跳）。门腿 27 对 45 把 ferry 的往返从 351 抬到 369，**每趟少运 5%**；bootstrap 期（那篇估 25–30 万 tick 到 RCL6）每一趟 pioneer、每一趟 ferry、将来每一只 guard 与 courier 都走这道门，而且 **W11S29 只有这一道非 highway 门**（`W11S28 ↔ W11S29` = 0 格）。
- 结论：**两端都不贵，而门那一端买到的是时钟**。extractor 在 RCL6 才解锁（`Tuning.ExtractorLevel`），钍那一腿要等它；bootstrap 快一点，整条钍的账都往前挪。

## 4. 如果以后 declare W12S29

这是它唯一能 declare 的房（`fourth-colony.md` §3：另外两个出口通 highway，北面 0 格）。把 W12S29 的地、ring 与它的 source `40,43` 铺进投影（`Outpost` 的形状，ADR 0042），再用同一套 sink 问一次：

| 格 | outpost 腿的 dearest sink | 到 storage | 加上 outpost 之后雇几只 |
|---|---|---|---|
| `11,30` | **105** | 81 | 2 |
| `12,30` | **105** | 81 | 2 |
| `17,23` | **105** | 102 | 2 |
| `28,14`（solo 的 demand floor） | 129 | 129 | 2 |

**1,003 个被问过的格子里 648 格的 outpost dearest 恒为 105 —— 又是 buffer。** 那条腿是「W12S29 的 rock → W11S29 的 controller buffer」，跨一道 Seam、与 spawn 摆哪儿无关。所以：

- **declare W12S29 不改变选格子的结论**（`11,30` 与 `12,30` 同为 105，与最优并列）；
- 它确实把 hauler 从 1 只推到 2 只，**每一格都是**，这是 declaration 的代价而不是格子的代价 —— 和 `fourth-colony.md` §7.2 记的 W13S28/W14S28 那笔账同一个形状；
- 唯一在这件事上真吃亏的是**贴着钍的东北角**（`28,14` 之类，129），而它们本来就因为 road 30+、门腿 78 落选。

## 5. road 数看不见的东西

| 问题 | 怎么量的 | 答案 |
|---|---|---|
| trunk 要不要穿房间的窄颈 | 「trunk 沿途最紧的 5×5 可走格数」的最小值（`walkableTilesIn` 数的） | **这个房没有颈**（内部每一列最少 31/48 可走），**但 source 有个口袋**：`7,33` 的 Seat 只有 `6,32 / 7,32 / 8,32`，三面是墙，每条 trunk 都要从口袋嘴出来 ⇒ 全房最好的 pinch 也只有 **16/25**，最差 8。这是 source 的属性，不是 spawn 的：本文对角线上 16 格 pinch 全是 16。 |
| footing / cluster 被挤在开着门的那面墙上 | cluster 的最小 x、门五格内的 cluster 格数、压在落地格上的 cluster 格数 | **192 格会把 cluster 压到门的落地格上**（最多 6 格），**346 格的 cluster 伸进门的五格内**。`1,32` 是极端：28 格 + 压 3 格 + 丢 trunk + 丢钍的 container。对角线上的 16 格全部 `plugged = 0`；只有最西的 `10,29` 有 4 格 cluster 进了门的五格内 —— **这是它落选的唯一原因**（门腿 24，本来是最好的）。 |
| Work Area 与门的 band 重叠 | source Seat ∪ Upgrade area ∩ 门的 20 个落地格 | **1,772 格全部 false**：source 在西南口袋、controller 在东南，两个 Work Area 都碰不到 x=1 那一列。这条风险在这个房不存在。 |
| spawn 自己吃掉 working ground | `workingGroundIn`（参照投影） | **23 格**，见 §1。 |
| 出生格（ADR 0026） | cluster 站起来之后 spawn 还剩几个可走邻格 | 全房 1–6；对角线上全是 5；`1,32` 只有 1。 |

## 6. 决定

**摆 `11,30`。** 次席 **`12,30`**（`fourth-colony.md` 的推荐）。

| | `11,30`（决定） | `12,30`（次席） | `17,23`（对角线另一端） |
|---|---|---|---|
| Road（RCL6 = RCL7） | **20** | **20** | **20** |
| Extension RCL6 / RCL7 | 40 / 50 | 40 / 50 | 40 / 50 |
| `UnservedFootings` / `UnroutedTrunks`（RCL6 与 RCL7） | 0 / 0 | 0 / 0 | 0 / 0 |
| `srcDearest`（quota 乘的那个数） | **57**（floor） | **57** | **57** |
| hauler demand / 人头 | 840 / **1** | 836 / **1** | 780 / **1** |
| 钍 → storage | 81 | 80 | **63** |
| 门 → spawn | **27** | 30 | 45 |
| 守恒和 | **108** | 110 | **108** |
| source → storage（铸造腿） | **8** | 8 | 29 |
| spawn → source / controller（单程 cast） | 2 / 15 | 3 / 14 | 8 / 9 |
| storage / buffer | `10,29` / `28,27` | `11,29` / `28,27` | `16,22` / `28,27` |
| cluster 最西 x / 门五格内 / 压落地格 | 6 / 0 / 0 | 7 / 0 / 0 | 12 / 0 / 0 |
| birth tile / trunk pinch | 5 / 16 | 5 / 16 | 5 / 16 |
| declare W12S29 之后 | 105，2 只 | 105，2 只 | 105，2 只 |

选 `11,30` 的全部理由，按它们的分量排：

1. **它在守恒对角线上，而 `12,30` 差 2 tick。** 对角线是 road = 20 且 `srcDearest` = 57 的那 16 格，`12,30` 是它旁边一格（110）。
2. **在对角线上取门那一端**，因为门腿买的是 bootstrap 的时钟（§3 末），而钍腿买的是 13,500 tick 里的 540 hauler-tick。
3. **门那一端里取 `11,30` 而不是 `10,29`**：`10,29` 门腿 24（更好）但把 4 格 cluster 推进门的五格内；`11,30` 的 cluster 最西只到 x=6，门的落地格上干净。
4. **同门腿的三格（`11,28` / `11,29` / `11,30`）里取 `11,30`**：它的 source → storage 是 **8**，另两格 11 与 12 —— 这条腿在 bootstrap 期每铸一具身体都在付。

**要说清楚的是这个决定有多小**：`11,30` 与 `12,30` 的差别是 2 tick 的守恒和、0 只 hauler、0 格 road、0 条损失。**如果人类已经把 `12,30` 记在手上了，照那个摆没有任何实质损失**；本文换格子只是把「门近 3 tick、钍远 1 tick」这笔已经量过的账收下来。真正不该摆的是别的东西：`1,32`（丢 trunk）、23 格 working ground、192 格会堵门、东南角那 5 格（雇第二只 hauler），以及任何 road ≥ 30 的格子（一次性多付 3,000 能量以上，而这个房的收入是 10 e/tick）。

落地步骤不变，照 `fourth-colony.md` §7 的两步走（先写 `Outpost.w11s29` 进 W13S28 的 `Outposts` 加 `Colony.declared` 的第四条 entry，claim 落地那天把它从 mother 的 `Outposts` 里去掉），本文只补一句：**spawn 站起来的那一天，W11S29 就进了 `RoomInvariantFixtures.rooms` 的候选**，那时 `1,32` 应当以 `SealedDoorsteps` 的形状进档，否则这条反例就只活在本文里。

## 7. 替代（本文数字里的堆叠误差）

- **capture 里没有 storage、没有 container、没有任何建筑**（`load` 的文档写着「furniture only — no structures, by design」）。所以：
  - **storage 用 Layout 自己这一格摆出来的 Storage site 代替**（每格都有，见 §6 表），`min2store` 与 `src2store` 的 sink 就是它；
  - **source / 钍 / buffer 的 container 用「铺好这一格自己的 road 计划之后 `decide` 摆出的 container site」代替**（`withRoadsStanding`，ADR 0040），三个位置见 §2；
  - **W12S29 的 rock 没有 container**（它的 container 要 `planOutpostContainers` 在有视野的 tick 才摆），所以 **outpost 那一腿是从它的 Seat 出发的**，`fourth-colony.md` §5 的 haul 价也是这么替的。
- **hauler body 是 `bodyFor haulerPattern 2300`**，而 sweep 的 `colonyOf` 里 `Bank = 300/300`。2,300 是 RCL6 有 40 个 extension 的房的真 bank，`haulerDemandOf` 用的是 `view.Bank.Capacity` —— 本文把 body 按 2,300 手动铸出来，是为了让 demand 的数字是**房子建好之后**的那个数，而不是空房的。低 bank 下载重更小、分母更小，人头会更早跳；顺序不变。
- **miner 的 20 Work 是读注释来的**，不是算出来的：`Bodies.minerBodyFor` 是 Core 的 `internal`，测试装配拿不到（没有 `InternalsVisibleTo`），所以 §3 的 `minerRate = 20`、`mineralHarvestCycle = 6` 引的是 `minerBodyFor` 的文档与 `Engine` 的常量。它对每一格都是同一个乘数，所以**按钍腿排名 = 按 quota 的矿项排名**，与这个常数无关。
- **门的落地格是本文自己按 ADR 0062 数的**：`acrossFrom (load "W12S29") (load "W11S29")` 之后 `seams "W12S29" "W11S29"`，取每个 landing 背后的 `adjacentWalkableIn` —— **20 格**。`fourth-colony.md` §3 那条边记的是 17（crossing 数），两个数不是一回事（一个落地格可以被两个 crossing 共用）。
- **`castWalkTicks` 的方向**：它是 spawn 往外的 lead 价（ADR 0026），本文用它当「单程走这段路要多少 tick」的读数（`srcW` / `ctrlW` / `minW` / `doorW` 四列），空载 factor。真进 quota 的四条腿全部用 `haulRoundTripTicks`。
- **RCL7 的 colony 仍然是 1 个 spawn、2 个 tower 都没建的空房**（`colonyOf loaded 7`），所以 RCL7 那一列问的是「同一间空房在 7 级的窗口里会要什么」，不是「建到 6 级之后再升一级会要什么」—— 后者是 `RoomInvariantFixtures.sweep` 的 `LevelUpAsks` 在做的事，本文没有重做。

## 8. 没做的事 / unverified

- **没有扫 swamp 上的 spawn 位**（217 格）。引擎允许在 swamp 上盖 spawn；fixture 的规则只收 Plain，本文照抄了。一格 swamp 上的 spawn 会改 cluster 的 parity 起点与 trunk 的权重（`Tuning.TrunkSwampWeight`），**可能**有比 20 更省的 road 计划。标 unverified。
- **没有算 tower 的覆盖**。「cluster 离门太近好不好」本文只量到「有没有堵门」与「有几格进了门的五格内」，**没有**量 tower 打不打得到落地格（ADR 0014 / 0034 的 keep 与 rampart 一个字都没进这一轮）。防守面上这是最该补的一格。
- **没有跑 `Decide` 的 quota 本体**。§3 的人头数是照 `haulerDemandOf` 的算式**手复现**的（sink 的 trip 是代码算的，乘加与 `ceilDiv` 是本文算的）：sweep 的 colony 没有 container 站着，`haulerDemandOf` 对它答 0，所以没法直接读 `decision.Quotas`。
- **bootstrap 的 ferry 趟数没有量**，所以 §3 末「门腿值 5%」只是比例，不是 tick。`Tuning.FerryLoads` 的上限与 ADR 0052 决策 7 的口径本文没有重读。
- **没有量 RCL1–5 的 Layout**。本文只跑了 6 与 7（ADR 0055/0063 的 horizon 与 `Tuning.ExtractorLevel`），而这个房会在 1–5 待 25–30 万 tick。低 level 的 cluster 更小、road 更短，`fourth-colony.md` §5 的第 2 条与 ADR 0064 都指向「road 计划不读 level」，但**本文没有对 1–5 验证这一点**。
- **`12,29` 与 `11,29` 与决定只差 1–3 tick**，本文按 §6 的第 3、4 条理由排了序，这两条理由（cluster 离门的距离、铸造腿）**没有被任何测试或 ADR 背书**，它们是本文自己的权重。换一组权重会在这三格之间换答案；不会换到对角线之外。
- **钍的开采速率仍未实测**（`fourth-colony.md` §9 同一条）：3.33 T/tick 是从 `minerBodyFor` 的注释与 `mineralHarvestCycle` 推的，没有在服务器上量过两次读数之差。
- **本文没有碰 W11S28**。它与 W11S29 是 0 格边（`fourth-colony.md` §3），所以那面墙上的格子在本文里只是普通的墙边格；`transitBetween` 会把 W11S28 拉进投影，但没有任何一条腿走它。
