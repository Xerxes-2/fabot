# 下一批 outpost 候选房 — 30 房普查、铺路后的真实步数、ADR 0042 口径的净能量

> **本文三条结论已过期（2026-09-10）**：ADR 0058 落地后 hop 预算是 3，"只有正交相邻的五个房能 declare" 与 §5 整节（"代码今天做不到的事"）已不成立 —— §2.2 那张"走得到但定不了价"的表现在全部可定价，其中 W15S28 已被 claim 成第三个 colony（`docs/research/third-colony.md`）；W16S25 那个 stronghold 已按 249,241 塌掉；两个 home 已到 RCL6，§3 的 bank 1800 / 1300 与由它算出的 hauler 取整都该重算。新的普查见 `docs/research/multihop-outposts.md`。本文其余部分（30 房普查、步数方法、经济口径）仍然有效，按写下的日期读。


Date: 2026-09-07（tick 202,311，`shardSeason`）。本文所有房间事实都是**当日用只读 API 在赛季服实测的**，不是从 ADR 或既有研究里抄的；所有步数是**本文自己写的多房 Dijkstra 算出来的**，代码在 scratchpad 里逐条列出并注明假设；所有能量数字都用 `src/Core/Types.fs` 的 `Engine` 常量与 `src/Core/Decide.fs` 的体型规则重算，不用姊妹篇的旧数。姊妹篇：`remote-mining.md`（引擎算术与社区做法，本文不重复推导，只引用）。动机：W12S28 已经在 W12S27 上跑通一个 outpost（当下 reservation `endTime = 207,222`，容器立在 `15,44`，13 格路已铺），W13S28 自 2026-09-06 立起自己的 spawn 后一个 outpost 都没有 —— 下一个该declare 哪里。

**Verified against**（本日 tick 202,311 的一手 API 调用，脚本全部在 `/tmp/claude-1000/-home-xerxes2-Dev-fabot/94cfc683-.../scratchpad/`）：
`survey.mjs`（30 个候选房逐房 `GET /api/game/room-objects` + `POST /api/game/map-stats`）、`wide.mjs` / `wide2.mjs`（W1–W30 × S14–S40 共 810 房的 owner 扫描）、`cores.mjs`（invader core 的 level 与 `EFFECT_COLLAPSE_TIMER`）、`terrain.mjs`（30 房 `GET /api/game/room-terrain?encoded=1`）、`paths.mjs`（多房 Dijkstra）、`trunk.mjs`（路线的逐房拆分与过境格）、`econ.mjs`（ADR 0042 口径的全账）。代码侧核对的是 `src/Core/Types.fs`、`src/Core/Decide.fs`、`src/Core/Atlas.fs` 与 `docs/adr/0041-0043`。

## Summary

- **能 declare 的房只有五个，不是本文枚举的三十个。** `Atlas.seams` 用 `borderPairs (thereX - hereX, thereY - hereY)` 求接缝，而 `borderPairs` 只匹配 `(0,±1)` 与 `(±1,0)` 四种偏移 —— **对角邻房和两房外的房一律返回空 band**，`pricedAcross` 与 `haulRoundTripTicks` 随即返回 `None`。所以 ADR 0041 的 Seam 模型**只支持正交相邻**。W12S28 可用的是 **W11S28、W12S29**（W12S27 已declare，W13S28 已独立成 colony）；W13S28 可用的是 **W13S27、W13S29、W14S28**。详见 §5。
- **最好的一个是 W13S29，给 W13S28。** 两个 source（`29,6` 与 `14,29`），铺路后单程 46 / 65 tick，净 **+14.2 e/tick**，是这批里唯一的双源 1-hop 房，也是 net/hauler 最高的（5.13）。
- **其余四个都是单源房，净 +5.3 到 +6.1 e/tick，彼此差不到 0.9。** 排序（净/hauler）：W11S28 4.83 > W12S29 4.72 > W13S27 4.08 > W14S28 3.75。作为标尺：已在跑的 W12S27 是 8.40 —— **新的没有一个有 W12S27 好，因为 W12S27 的 47 格里有 40 格在自己房里**。
- **把这五个全declare，两个 home 房的入侵门就全关了。** `genInvaders` 的 `checkExit` 拒绝邻房 `controller.user || controller.reservation` 的出口（`remote-mining.md` §1.4）。W12S28 的四个出口是 W12S27（已 reserve）、W13S28（自有）、W11S28、W12S29；W13S28 的四个是 W12S28（自有）、W13S27、W13S29、W14S28。**这五个房恰好就是两个 home 房剩下的全部出口邻房**，一个不多一个不少。这是一项不在能量账里的收益。
- **Thorium 不是选房依据。** 本 sector 每个 normal 房都有一处 Thorium（30 房实测无一例外），差别只在密度：W11S29 `39,4` d4 **45,000** 最富，其次是一批 d3 22,000，最穷的 W15S29 `25,2` d1 只有 3,000。但 **extractor 需要 RCL6 的自有 controller**，reserve 不算 —— outpost 里的 Thorium 一克也挖不出来（引擎侧未在本文核源，标 **unverified**，但 `CONTROLLER_STRUCTURES.extractor` 的 controller 依赖是公认规则）。**Thorium 是"要不要 claim 成第三个 colony"的依据，不是"要不要 declare 成 outpost"的依据。**
- **ADR 0043 的观察日期到了，而且换人了。** 该 ADR 写下的 W15S24 level-4 stronghold **已经塌了**（`EFFECT_COLLAPSE_TIMER` 原定 170,283，今日 W15S24 已空）。接替它的是 **W16S25 的 level-2 stronghold**，2 座 tower、9 ramparts，collapse 定在 **tick 249,241**，已经扩出七个 level-0 core：W15S23、W16S23、**W16S27**、W17S24、W17S25、W17S26、W18S25。**W16S27 离 W14S28 只有两房**。另有一个更凶的 level-5 stronghold 在 W5S24（6 座 tower、9 只 invader creep，collapse 244,405），它的前沿 W8S24 离 W12S28 四房。
- **人类邻居仍然很远。** 810 房扫描里，离我们最近的人类是 **Odiodin**：自有 W14S22（RCL4），reserve 了 W13S22 / W13S23 / W14S23 —— 最近的一格是 W13S23 / W14S23，**距两个 home 房都是 5 房**。候选框内 30 个房**没有任何一个有 owner、reservation、hostile structure 或别人的 creep**（我们自己的除外）。
- **W11S26 从我们这边根本走不到。** 它的南、东、西三面边界**一格出口都没有**（`terrain.mjs` 实测：`W11S26` S/E/W 三边 48 格全是 wall），只有北面通向框外的 W11S25。两源加 d3 Thorium 的房，但对我们不存在。
- **两个 SK 房（W14S26 / W15S26，各 3 source + 4 keeperLair + 一座 extractor）不是候选**：它们**没有 controller**，而 `Outpost.Controller` 是必填字段（`Types.fs`：*"a room with no controller to reserve — a sector centre or a Source Keeper room — is not a candidate outpost at all"*）。类型系统已经替我们拒了。

## 1. 三十房普查（tick 202,311，`survey.mjs`）

Chebyshev ≤ 2 of W12S28 ∪ W13S28 = W10–W15 × S26–S30，共 30 房。**全部无主、无 reservation、无 invader core、无敌对建筑、无他人 creep**，故下表省略这五列，只在异常处标注。

| 房 | 类型 | source（位置） | controller | 常规矿 | Thorium | 到两个 home 的出口 |
|---|---|---|---|---|---|---|
| W10S26–30 | highway | — | — | — | — | — |
| W11S26 | normal | 2：`46,10` `44,38` | `33,23` | O d2 35k | `11,6` d3 22k | **无**（S/E/W 三面全墙） |
| W11S27 | normal | 2：`13,28` `46,33` | `14,29` | Z d2 35k | `47,12` d3 22k | 对角，无 Seam |
| W11S28 | normal | 1：`33,15` | `8,16` | X d2 35k | `40,17` d2 10k | **W12S28 东**（x=49，y 31–32，2 格） |
| W11S29 | normal | 1：`7,33` | `30,29` | H d1 15k | `39,4` **d4 45k** | 对角，无 Seam |
| W11S30, W12S30, W13S30, W14S30, W15S30 | highway | — | — | — | — | — |
| W12S26 | normal | 1：`41,40` | `8,32` | U d2 35k | `33,39` d3 22k | 两房外 |
| **W12S27** | normal | 1：`16,45` | `37,43` | U d2 35k | `24,16` d3 22k | **W12S28 北**（36 格）—— 已declare |
| W12S28 | **home** | 2：`17,40` `9,44` | `5,41` RCL5 | O d3 70k | `26,5` d3 22k | — |
| W12S29 | normal | 1：`40,43` | `15,36` | X d3 70k | `12,27` d2 10k | **W12S28 南**（y=49，x 38–42，5 格） |
| W13S26 | normal | 1：`35,23` | `33,23` | O d2 35k | `45,15` d3 22k | 两房外 |
| W13S27 | normal | 1：`25,20` | `26,15` | L d3 70k | `38,43` d2 10k | **W13S28 北**（34 格，x 8–41） |
| W13S28 | **home** | 2：`18,4` `16,7` | `24,17` RCL4 | O d3 70k | `42,30` d3 22k | — |
| W13S29 | normal | **2**：`29,6` `14,29` | `15,41` | O d1 15k | `38,23` d2 10k | **W13S28 南**（12 格：x 15–19、39–45） |
| W14S26 | **SK** | 3：`42,6` `3,14` `32,39` | **无** | K d2 35k（带 extractor） | 无 | 4 lair，排除 |
| W14S27 | normal | 1：`15,37` | `43,27` | H d3 70k | `41,7` d2 10k | 两房外 |
| W14S28 | normal | 1：`6,8` | `22,15` | U d3 70k | `2,29` d3 22k | **W13S28 西**（21 格：y 7–22、36–40） |
| W14S29 | normal | 2：`12,33` `31,35` | `29,5` | Z d2 35k | `31,30` d2 10k | 两房外 |
| W15S26 | **SK** | 3：`11,16` `4,33` `39,34` | **无** | X d3 70k（带 extractor） | 无 | 4 lair，排除 |
| W15S27 | normal | 1：`14,28` | `6,9` | U d3 70k | `8,24` d2 10k | 两房外，且**紧邻 W16S27 的 core** |
| W15S28 | normal | 2：`10,19` `6,30` | `25,31` | O d3 70k | `29,12` d3 22k | 两房外 |
| W15S29 | normal | 1：`18,20` | `12,34` | H d2 35k | `25,2` d1 3k | 两房外 |

两个 home 房的接缝宽度实测为 W12S28 北 36 格、W12S28 西 19 格 —— **与 ADR 0041 写下的 "36 tiles north (W12S27) and 19 tiles west (W13S28)" 逐字相符**，这是本文测量方法与仓库 Atlas 一致的第一处交叉验证。

## 2. 铺路后的步数（`paths.mjs`）

**方法与假设**（脚本头部逐条写死，此处摘要）：

1. **路由用户自己铺**，所以每一格走一 tick：2C:1M 的 hauler 在路面上每格 2 fatigue、1 个 MOVE 每 tick 消 2，**沼泽按平地计价**。天然 wall 不可走，现有建筑一律忽略（只看地形）。
2. 八向移动、代价一致。
3. **只有 1..48 的内部格可站**，与 ADR 0036 的 trim 和 ADR 0041 "a Seam is never a tile to stand on" 一致。
4. **过境按引擎自己的两 tick 走法**，也就是 ADR 0041 `joinedAcross` 的算术正着读：从紧挨出口格 `e` 的内部格 `a` 迈上 `e`（1 tick），引擎在 tick 末把 creep**免费**放到邻房的 `l` 上，再从 `l` 迈进内部格 `b`（1 tick）。故 `a→b` 的边权是 **2**，且 `e` 与 `l` 都必须非墙，角格排除。**这就是"跨房 walk 可以比两房的 Chebyshev 距离还短一格"的来源**（ADR 0041 原话）。
5. **source 格与 controller 格在服务端地形里读作 wall**（30 房逐个验证，无一例外），所以量的是 **seat** —— source 周围最便宜的可站内部格，正是 ADR 0042 放 container 的地方。
6. hauler 在**紧挨 storage 的格**上 transfer，所以搜索从 storage 的可站邻格以代价 0 起播。单程 = 该代价；往返 = 2×。
7. 边权放大 100 倍、每次过境额外加 1 —— **同 tick 数的路线里选过境最少的**，否则贴边走的路线会在两房之间来回横跳（同代价、但读起来是垃圾）。

两处交叉验证：**W12S28 → W12S27 `16,45` 算出 47 tick**，与 ADR 0042 的 *"W12S27 walks 40 of 47 tiles inside our own room"* 完全一致；**W12S28 → W13S28 的两个 source 算出 47 / 56 单程（往返 94 / 112）**，与 `remote-mining.md` §Summary 的 *"92 与 112 tick"* 相符（差 2 tick 来自量的起点不同）。

### 2.1 正交相邻（可 declare）的五个房

| 目标房 | 归属 home | source | seat | 单程 tick | 跨房数 | seats | 过境格 | 路线逐房格数 |
|---|---|---|---|---|---|---|---|---|
| **W13S29** | W13S28 | `29,6` | `28,6` | **46** | 2 | 3 | `W13S28(18,48)→W13S29(20,1)` | W13S28 37 / W13S29 9 |
| **W13S29** | W13S28 | `14,29` | `15,28` | **65** | 2 | 5 | `W13S28(16,48)→W13S29(18,1)` | W13S28 37 / W13S29 28 |
| **W13S27** | W13S28 | `25,20` | `24,19` | **60** | 2 | 2 | `W13S28(22,1)→W13S27(24,48)` | W13S28 19 / W13S27 41 |
| **W14S28** | W13S28 | `6,8` | `6,9` | **65** | 2 | 3 | `W13S28(1,6)→W14S28(48,6)` | W13S28 14 / W14S28 51 |
| **W11S28** | W12S28 | `33,15` | `32,14` | **66** | 2 | 1 | `W12S28(48,30)→W11S28(1,31)` | W12S28 34 / W11S28 32 |
| **W12S29** | W12S28 | `40,43` | `41,44` | **67** | 2 | 1 | `W12S28(37,48)→W12S29(37,1)` | W12S28 23 / W12S29 44 |
| 参照：W12S27 | W12S28 | `16,45` | `15,44` | 47 | 2 | 3 | 已铺 13 格 | W12S28 40 / W12S27 7 |

### 2.2 走得到、但 Seam 模型定不了价的（存档用）

| 目标房 | 从 | 关系 | source 单程 | 净 e/tick | 备注 |
|---|---|---|---|---|---|
| W11S27（2 源） | W12S28 | **对角** | 90 / 97 | 11.64 | 需经 W11S28 |
| W11S29（1 源，T d4 45k） | W12S28 | **对角** | 55 | 6.52 | 净/hauler 7.11，仅次于 W12S27 |
| W13S29（2 源） | W12S28 | **对角** | 54 / 73 | 12.97 | 但对 W13S28 是正交，见上表 |
| W14S29（2 源） | W13S28 | 两房外 | 84 / 86 | 12.59 | |
| W15S28（2 源） | W13S28 | 两房外 | 101 / 106 | 11.64 | 紧邻 invader 前沿 |
| W12S26 / W13S26 / W14S27 / W15S27 / W15S29 | 两者 | 两房外 | 100–164 | 3.3–5.2 | 全部单源，不值得 |

## 3. 经济账（ADR 0042 口径，`econ.mjs`）

用的是仓库自己的常量与体型规则，不是姊妹篇的社区体型：

- `Engine.heldOutputPerTick = 10`、`neutralOutputPerTick = 5`、`creepLifetime = 1500`、`claimLifetime = 600`、`reservationCap = 5000`、`harvestPerWork = 2`。
- **Anchor**：`Decide.anchorBodyFor`，`workCapOf 10 = 10/2+1 = 6` ⇒ 两个 colony 都买 **6W/1C/1M = 700 e**，摊 1500 tick = **0.47 e/tick/source**（题面给的 800 是旧数）。未 reserve 时 `workCapOf 5 = 3` ⇒ 3W/1C/1M = 400。
- **hauler**：`Decide.haulerBodyFor` 取整块 `[Carry;Carry;Move]`。W12S28 bank 1800 ⇒ 12 块 = **24C/12M，载 1,200，造价 1,800**（摊 1.20 e/tick）；W13S28 bank 1300 ⇒ 8 块 = **16C/8M，载 800，造价 1,200**（摊 0.80 e/tick）。配额是 `ceilDiv (Σ output × 往返tick) capacity`（`haulerDemandOf`，ADR 0049 全房只 round 一次）。
- **reserver**：`reserverBodyWithin` 按 `claims = ceil((5000 − ticksToEnd)/600)` 定尺寸，稳态下就是 **`[1Claim;1Move]` = 650 e**（W12S27 此刻 `ticksToEnd = 4,911`，deficit 89 ⇒ claims = 1，实测印证）。**摊销的分母不是 600 而是 `600 − 到 controller 的步数`** —— 走路那段命是白烧的，远房因此更贵。下表的 `net` 用这条；`netADR` 用 ADR 0042 保守的 `[2Claim;2Move]`。
- **container 衰减 0.5 e/tick/个**（无主房，`remote-mining.md` §1.3），**路面维护** `0.001×D + 0.001×P×haulers`。

| 房 | home | 源 | 单程 | ctrl 步 | 毛 | anchor | reserver | hauler | **净 e/tick** | netADR | 不 reserve | hauler 分数 | 整只 | **净/hauler** |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| *W12S27（在跑）* | W12S28 | 1 | 47 | 44 | 10 | 0.47 | 1.17 | 1.20 | **6.58** | 5.41 | 2.95 | 0.78 | 1 | **8.40** |
| **W13S29** | W13S28 | **2** | 46/65 | 77 | 20 | 0.93 | 1.24 | 2.40 | **14.24** | 13.00 | 6.71 | 2.77 | 3 | **5.13** |
| **W11S28** | W12S28 | 1 | 66 | 51 | 10 | 0.47 | 1.18 | 2.40 | **5.31** | 4.13 | 2.93 | 1.10 | 2 | **4.83** |
| **W12S29** | W12S28 | 1 | 67 | 68 | 10 | 0.47 | 1.22 | 2.40 | **5.27** | 4.05 | 2.93 | 1.12 | 2 | **4.72** |
| **W13S27** | W13S28 | 1 | 60 | 64 | 10 | 0.47 | 1.21 | 1.60 | **6.11** | 4.90 | 3.35 | 1.50 | 2 | **4.08** |
| **W14S28** | W13S28 | 1 | 65 | 68 | 10 | 0.47 | 1.22 | 1.60 | **6.10** | 4.88 | 3.34 | 1.63 | 2 | **3.75** |

三条要读出来的：

1. **hauler 配额的取整是这张表里最大的一项非线性。** W11S28 的分数配额是 1.10，向上取整成 2 只 —— 第二只 hauler 只干了 10% 的活，却吃掉 1.20 e/tick，把净收益从 6.5 压到 5.3。**W13S28 的 800 载重比 W12S28 的 1,200 更容易踩到这条**（W13S29 的 2.77 → 3，浪费 23%）。反过来说，**W12S28 升到 RCL6（bank 2300 ⇒ 15 块 = 30C，载 1,500）会把 W11S28 与 W12S29 的配额压回 1 只，两个房各自净收益跳到 ≈ +6.5**。这是"先升级还是先扩 outpost"的一个可算的答案。
2. **reserve 与不 reserve 的差是 +2.4（单源）到 +7.5（双源）e/tick**，全部远超 reserver 的 1.2 —— ADR 0042 的 *"reserved from the first version"* 在这五个房上没有一个例外。
3. **净/hauler 这个比值真正惩罚的是距离，不是源数。** W13S29 的绝对净收益是 W11S28 的 2.7 倍，但每只 hauler 的产出只高 6%。**若 CPU 或 spawn 带宽是瓶颈，五个房排序几乎不分先后；若能量是瓶颈，W13S29 一骑绝尘。**

## 4. 风险

**别的玩家**：候选框内一个都没有。最近的人类 **Odiodin**（自有 W14S22 RCL4，reserve W13S22 / W13S23 / W14S23），最近的一格 W13S23 / W14S23 **距两个 home 房都是 5 房**；他的扩张方向是北，不是南。次近的 Kamots（W14S19）9 房、FR4C74LH3X（W3S23/W3S24）9 房。**六个 bot 一致的"敌对邻居是布尔排除项"（`remote-mining.md` §2.1）在这里不触发。**

**Invader core / stronghold**：本 sector（`^W1\dS2\d$`，即 W10–W19 × S20–S29，五个候选房全在内）的入侵开关**是开的**，因为 **W16S25 站着一个 level-2 stronghold**（2 tower、9 rampart、1 只 invader creep，`EFFECT_COLLAPSE_TIMER` 到 **249,241**，还有 46,930 tick）。它已扩出七个 level-0 core，最近的是 **W16S27**（`26,32`，reservation 到 207,345）：

- W16S27 → W15S27 / W16S26 / W16S28 是一步，→ W15S28 是两步，→ **W14S28 是三步**。`INVADER_CORE_EXPAND_TIME[2] = 3500`，即 **≈10,500 tick 后 W14S28 可能落一个 core**。五个推荐房里 **W14S28 的 core 风险最高**，W13S27 / W13S29 次之（各 3 步），W11S28 / W12S29 最低（≥5 步）。
- **reserve 挡不住 core 落地**（`expandStronghold` 只看 `!controller.user`），ADR 0043 的 stand-down 门就是为这条写的 —— 每个 outpost 一个独立的门，五个房五个门。
- **ADR 0043 里那句 "Tick 170,283 is a date worth watching" 已经过期**：W15S24 的 level-4 stronghold 已塌，今日该房空无一物；接班的是 W16S25，新日期是 **249,241**。这行数字该在 observe channel 里换掉。
- 另有一个 **level-5 stronghold 在 W5S24**（6 tower、9 只 invader creep，collapse 244,405），前沿已到 **W8S24（距 W12S28 四房）**。它不在我们 sector 的正则里（W5 ⇒ `^W\dS2\d$`），对入侵开关无影响，但如果它继续东扩会先碰到 W11S28 / W11S29 一线。

**入侵时刻表**（`INVADERS_ENERGY_GOAL = 100,000`，reserve 后单源 10 e/tick）：单源房 **≈10,000 tick** 触发第一次，W13S29 双源 **≈5,000 tick**，之后每 70k–130k 一次，且**无主房的 raid 永远是 small 体**（`remote-mining.md` §1.4）。

**出口门（唯一一项防御收益）**：`checkExit` 拒绝邻房已 owned 或 reserved 的出口。两个 home 房各有四个出口，**这五个候选房恰好是全部剩余的四个 + 三个**：

| home | N | S | E | W | declare 全部后 |
|---|---|---|---|---|---|
| W12S28 | W12S27 ✅已 reserve | W12S29 ⬜ | W11S28 ⬜ | W13S28 ✅自有 | **四门全闭**，W12S28 不再是入侵产房 |
| W13S28 | W13S27 ⬜ | W13S29 ⬜ | W12S28 ✅自有 | W14S28 ⬜ | **四门全闭**，W13S28 不再是入侵产房 |

**注意这条只保护 home 房**：outpost 自己仍会被入侵（W11S28 的西邻 W10S28 是 highway，无 controller ⇒ 永远是合法出口）。

## 5. 代码今天做不到的事

**ADR 0041 的 Seam 模型只认正交相邻，两房外的 outpost 完全不支持，而且是静默失败的。**

`Atlas.seams`（`src/Core/Atlas.fs:2523`）的实现是：

```fsharp
borderPairs (thereX - hereX, thereY - hereY)
|> List.filter (fun (here, there) -> walkableAt near here && walkableAt far there)
```

而 `borderPairs` 只对 `(0,-1) | (0,1) | (-1,0) | (1,0)` 四种偏移返回非空列表，其余一律 `[]`。下游：

- `pricedAcross`（`Atlas.fs:2999`）`match seams … with | [] -> None` ⇒ **Matcher 对该房的任何 Task 都得不到价格**；
- `haulRoundTripTicks`（`Atlas.fs:3469`）同样先读 band ⇒ **`haulerDemandOf` 里那个 container 的 `Trip` 是 `None`，`Demand = 0`，一只 hauler 都不会雇**；
- 而 `Outpost.roomsProjected` 照样把这个房并进投影、`Outpost.place` 照样把 furniture 铺进去 —— 按 ADR 0004 的全域性，**"定不了价的几何不计入任何 Task 也不阻塞任何动作"**，所以整件事**不报错、不告警，只是那个 outpost 永远没有 creep 去**。

结论有两条，都请写进 declare 的注释里：

1. **两房外的 outpost 不支持。** §2.2 那六个房（W11S27、W14S29、W15S28、W12S26、W13S26、W14S27、W15S27、W15S29）今天写进 `Colony.declared` 会得到一个安静的空房。
2. **对角邻房同样不支持**，而这一条更容易踩，因为它 Chebyshev 距离是 1、看地图像是"邻房"。**W11S27、W11S29、W13S27、W13S29 对 W12S28 都是对角的**；其中 W11S29（净/hauler 7.11，仅次于 W12S27）和 W13S29（净 12.97）是本轮**因为这条限制而落选的两个最好的房**。想要它们，需要一张新 ticket：把 `pricedAcross` 从"一条 Seam"推广到"一条房序列"（TooAngel 的 `findRoute` 骨架 + 逐房 leg 缝合，`remote-mining.md` §1.7 与 ADR 0041 的 Considered Options 里都已备案），代价是 ADR 0041 明确拒绝过的多房 flood 或一层 route 缓存。

第三条较轻但真实：**`Outpost.Controller` 必填**把两个 SK 房自动挡在门外，这是对的；但也意味着 sector centre 与 SK 房**永远**不能借这条路径进入经济层，将来要挖 SK 得另开一套 mission（社区六家的一致做法）。

## 6. 建议

### W12S28（RCL5，bank 1800）

1. **W11S28**（东出口，2 格宽的接缝 y 31–32）。1 source `33,15`，seat `32,14`（**只有一个 seat**，6W 的 Anchor 正好占满，不留换班余地）。trunk 约 **66 格**，其中 34 格在自己房里（可与现有 100 格路网接驳）、32 格在 W11S28 —— 新铺约 **32–50 格，≈9,600–15,000 e**。净 **+5.3 e/tick**。选它排第一是因为 **core 风险最低**（离最近的 invader core 五步以上）且 controller 只有 51 步（reserver 摊销最便宜的一个）。
2. **W12S29**（南出口，5 格宽 x 38–42）。1 source `40,43`，seat `41,44`，同样单 seat。trunk 约 **67 格**，44 格在 W12S29。净 **+5.27 e/tick**。两个一起 declare 才关得上 W12S28 的门 —— 而这两个的能量差只有 0.04 e/tick，**关门这项收益比它们之间的排序重要得多**。
3. **等 RCL6 再回头看一次**：bank 2300 把 hauler 载重从 1,200 抬到 1,500，这两个房的配额都会从 2 只掉回 1 只，各多约 1.2 e/tick。

**避免**：W11S27（对角，不支持；且需借道 W11S28）、W12S26（两房外）、W11S26（物理不可达）。

### W13S28（RCL4，bank 1300，单 spawn，已有 7 只 creep）

1. **W13S29** —— 无争议的第一。两个 source `29,6`（seat `28,6`，3 seat，单程 **46 tick**，比 W12S28 到 W12S27 还近）与 `14,29`（seat `15,28`，5 seat，65 tick）。南出口两条带（x 15–19 与 39–45），trunk 共约 **74 格独立格**（W13S28 内 37 格共用），**≈22,200 e**。净 **+14.2 e/tick**，几乎是 W13S28 当前自有两源产出的一半。**唯一的顾虑是 spawn 带宽**：这一个房要加 1 reserver + 2 Anchor + 3 hauler = 6 只，单 spawn 的 casting 队列会明显变长，且 ADR 0042 的 reserver 排在最前 —— 先观察一轮 `planSpawns` 的 gap 再上第二个。
2. **W13S27**（北出口，34 格宽，最宽的一条）。1 source `25,20`，seat `24,19`（2 seat），单程 **60 tick**，trunk 60 格里 41 格在 W13S27，净 **+6.11 e/tick**。排第二而不是 W14S28，因为它离 invader 前沿远一步、接缝宽 34 格（几乎不可能被堵）、且 controller 只 64 步。
3. **W14S28** —— 排最后，且**建议先不上**。净收益（6.10）与 W13S27 只差 0.01，但它是**离 W16S27 那个 core 最近的推荐房（三次扩张，≈10,500 tick）**，并且 51 格的 trunk 全在房外、暴露最长。等 W16S25 在 249,241 塌掉之后再 declare，届时全 sector 的入侵开关会一起关掉。

**避免**：W14S29 / W15S28（两房外，虽然各有两源）、W15S27（两房外 **且**紧贴 W16S27 的 core）、W12S29（对 W13S28 是两房外，它属于 W12S28）。

### 通用

- **五个全上才关得上两扇门**，但那是 6 + 12 = 18 只新 creep 分摊在两个 colony 的两个 spawn 上。合理的次序是：**W13S29（W13S28）→ W11S28（W12S28）→ W12S29（W12S28）→ W13S27（W13S28）→ W14S28（等 stronghold 塌）**。
- **每个新 outpost 都要在 `Colony.declared` 里写死 source id 与 controller id**（`Types.fs` 的 `Outpost` 用的是引擎自己的 id，写短名会静默匹配不上）。本文 `survey.json` 里已存了 30 个房全部 source / controller 的真实 id。
- **Thorium 的账要单独开一张 ticket**：五个候选房各有一处 22k 或 10k 的 Thorium，但 reserve 挖不出来。若要吃 Thorium，正确的动作是**把 W13S29 或 W11S27 claim 成第三个 colony**（GCL 允许时），而不是 declare 成 outpost —— 那是 ADR 0047 的题目，不是 ADR 0042 的。
