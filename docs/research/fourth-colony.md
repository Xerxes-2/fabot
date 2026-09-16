# 第四个 colony 选哪个房 — 地形先删掉一半候选，钍的交付路径决定剩下的一半

Date: 2026-09-16（tick 491,332–491,747，`shardSeason`）。本文所有房间事实都是**当日用只读 API 在赛季服实测的**；接缝是本文自己按 ADR 0062 的口径（两侧 border ring + **落地格背后的地** + keeper margin 6）重数的，并用第三个房那三道已知接缝（20/20/20 去程、13 回程）与 #336 的 `W14S26 → W15S26` 做了交叉验证。姊妹篇：`third-colony.md`（2026-09-10，本文接着它写，**采纳**它 §9 把 W11S29 留给第四个房的建议，并改正它 §9 的一条事实——W11S29 的非 highway 邻居不是「W11S28 与 W12S29」两个，`W11S28 ↔ W11S29` 是 0 格，只有 W12S29）、`thorium-season-plan.md`（2026-09-13 的赛季账，它的规则仍然是权威，关于我们、记分板与 sector 的活体数字本文全部重测，两处旧文各自已加了指向本文的日期横幅）、`multihop-outposts.md`（outpost 普查）。动机：GCL 刚过第四个房的门槛（14,104,557 ≥ 11,190,000）。

**Verified against**（当日一手调用，脚本全部在 `/tmp/scout/`，只用只读端点：`GET /api/auth/me`、`/api/game/time`、`/api/user/rooms`、`/api/game/room-objects`、`/api/game/room-terrain?encoded=1`、`POST /api/game/map-stats`、`GET /api/scoreboard/list`，以及 `scripts/observe.mjs` 的只读 Memory 通道）：

| 脚本 | 干了什么 |
|---|---|
| `who.mjs` | 账号、GCL、tick、自有与 reserve 的房。 |
| `survey.mjs` | `map-stats`（`owner0`）扫 W6–W22 × S20–S34 共 255 房，按 `x%10 / y%10` 分 highway / SK / centre / claimable，算到三个 home 与到 Reactor 的 hop。 |
| `objs.mjs` | 逐房 `room-objects`：source 与 controller 的坐标与**引擎真实 id**、矿种、密度与**当前**储量、invader core 的 level 与 `effects`、房里站着谁的 creep。 |
| `home.mjs` | 三个自有房的 RCL、progress、建筑普查、每个 store 的内容（含钍）。 |
| `reactor.mjs` | W15S25 的 reactor 对象（owner / store / `launchTime`）、房里的 creep、`scoreboard/list` 全表。 |
| `threats.mjs` | W4–W26 × S16–S38 共 529 房的 owner/reservation 扫描并解析用户名；6 跳内 99 房逐房查 `invaderCore` 与 effect。 |
| `borders.mjs` / `seams.mjs` | 候选房每一条边界**双向**按 ADR 0062 数：`ring`（两侧 ring 都非 wall）与 `open`（落地格旁还有对面房自己的地），SK 房带 margin 6 的 mask。 |
| `scripts/observe.mjs reactor / cpu / quotas / raids / layout` | 活体读数（t491,449–491,747）。 |

代码与 ADR 侧核对：`src/Core/Types/Colonies.fs`、`src/Core/Types/Geometry.fs`（`Seam.bandBy`、`landsOnGround`、`RoomName.routesBy`、`transitBetween`）、`src/Core/Types/Keepers.fs`（`centres` 只声明了 W15S26 一个房）、`src/Core/Types/Rules.fs`（`MaxHops = 3`、`ExtractorLevel = 6`、`keeperMargin = 6`）、ADR 0041 / 0042 / 0047 / 0057 / 0058 / 0060 / 0062 / 0067；赛季规则侧重读 `screeps/mod-season5` 的 `src/terminal-restriction.js`（本文唯一一条新的规则结论，见 §4）。

## Summary

- **第四个 colony 选 W11S29，mother 是 W13S28，走 ADR 0047 的 candidate-colony 路径。** 1 source（`7,33`）、controller `30,29`、Thorium **d4 45,000** @`39,4`（本 sector 我们走得到的**最富的一处钍**，是任何其他候选房的两倍）、常规矿 H d1 15,000。
- **地形，而不是能量、也不是威胁，删掉了一半候选。** 用 ADR 0062 的口径（两侧 ring + 落地格背后的地 + keeper margin 6）双向数完每一条边界，再用仓库自己的 `Declaration.routable` 在真实 capture 上复核：**W9S28 的 d4 45,000 到不了**（`W10S28↔W9S28` 与 `W11S28↔W10S28` 都是 0 格，整个 W10 列封死）、**W13S25 唯一的门开在 sector 中心那侧**（北南东全 0，`third-colony.md` 的第二名就此出局，而且不是因为 invader）、**W11S26 只有北面通**（把旧文那条 unverified 的"三面全墙"实测为真）、**W12S31 与 W16S28 都是 0 格边**。名字层接受、地形层拒绝的 outpost 声明本轮有 **六个**（从 W12S28 数 W9S28 与 W11S26，从 W13S28 数 W13S25 与 W13S26，从 W15S28 数 **一跳**的 W16S28 与两跳的 W17S28），另有一个 errand 类的拒绝（W12S25 到 Reactor 的链），全部是 #259 的形状。准确的说法是 ADR 0058 决策 2 的说法：**`transitBetween` 的矩形里没有链**，绕矩形之外的路不算。
- **交付路径是第二把刀，而它把"自带交付路"的候选压到只剩三个单 source 小矿房。** 一个房的钍要变成分数，只能靠它自己声明 `Errand`、courier 走 ≤3 跳的链到 W15S25。实测只有 **W13S26（22,000，从 W12S28 三跳可达，claim walk 247，到 Reactor 140 tick）**、W14S27（10,000）、W16S27（10,000）满足，而且每一个都要先给 `Keepers.centres` 手抄一到两个新的 SK 房（W14S25 / W14S26 / W16S26，本文把 lair 坐标读出来了）。
- **所以选房的真问题不是"哪个房离 Reactor 近"，而是"terminal 这张票做不做"。** `mod-season5/src/terminal-restriction.js` 本文重读全文：**只有目标 terminal 属于别人的 `send` 会被置空，自己发给自己不受限，钍也不例外**，代价是 `ceil(amount·(1−e^(−range/30)))` 能量（W11S29→W15S28 range 4 ⇒ 每 1,000 T 约 125 能量）。而这张票**跟第四个房无关也必须做**：两个 home 此刻存着 **36,484 T**（storage 里 35,564，矿物 container 里 920），**按 5 分/T 值 182,420 分**，是任何一个第四个房的矿的 1.6–8 倍，而 `Layout` 里今天连 terminal 这个词都没有。既然它必做，第四个房就该按"地里的矿"选，而不是按"能不能走过去送"选 —— 这就是 W11S29 胜过 W13S26 的全部理由。
- **W11S29 同时是所有候选里 bootstrap 最便宜的一个。** 仓库自己的定价：从 W12S28 两跳（`castWalkTicks [Claim;Move]` = **70**、`haulRoundTripTicks` = **171**），从 W13S28 三跳（**121 / 324**，两条等长链）；对比 W11S27 的 100/297·282、W17S29 的 106/353·252、W13S26 的 247/624。它的 trunk 20 格，也只有 W15S28 的 57 的三分之一。
- **它还是防守面最小的房：唯一的非 highway 出口是 W12S29**（`W11S28↔W11S29` = 0 格），另外两个出口通向 highway 的 W10S29 / W11S30，第四个是墙。
- **代价说清楚：1 个 source。** 它的 RCL 爬升只有 10/tick 的自有收入（再加将来 declare W12S29 的 10），按 W15S28 的实测（t305,2xx claim → 今天 RCL6，183,000 tick，两个 source）估约 **25–30 万 tick 到 RCL6**，也就是 extractor 大约在 t750,000 开工，而赛季还有 1,475,000 tick。**钟不是约束，矿是。**
- **本轮唯一"已经在门上"的代价是 CPU，而且这一次是量得出来的。** `observe cpu` 100 tick：**mean 40.34 ms、max 79.79 ms**，而 ADR 0041 的 revisit 门是 mean > 50 或单 tick > 80 —— **差 0.21 ms**（第三个房落地时是 mean 21.53 / max 40.95）。`profile` 的 `pair` 场景在这条 declaration 前后各跑三轮：**前 5.24–5.72，后 6.47–7.27 ms/tick，两个区间不重叠**，即三个新地形层约 **+20%** —— 上一次同一个度量里读不出差别。所以第四个房应当**先买回 CPU 再派 claimer**：#332（keeper mask 每 tick 重算，占投影一个 masked 房的 tick 的 15–17%，而 W15S28 每 tick 都在投影 W15S26）与 #278 正对着这件事。
- **记分板：`giaco` 511,561，我们 rank 37、0 分。** reactor W15S25 是我们的，`store.T = 0`、`launchTime: null`、`observe reactor` 读作 *last delivery **never***，第一趟投递此刻在路上（`hauler-491206-Spawn3` 的 Task 是 `refill:<reactor>:Thorium`）。Odiodin 与 Shibdib 都在往那个房派 1-move 的 scout，而 `claimReactor` 无 cooldown、无前置。
- **风险面比 2026-09-10 干净又更近：6 跳内此刻一个 invader core 都没有**（W16S24 的 level-1 stronghold 与 W15S23 的 level-0 core 在本文写作期间按期塌在 t491,491），但 **Odiodin 多了一个自有 W12S23 (RCL6)**、新玩家 **Trepidimous** 的 W18S26 (RCL5) 离候选房 W17S29 只有两房。W11S29 方向上最近的是 Ague 的 W7S28，四房。
## 1. 前提：今天的三个殖民地，和一个 0 分的记分板

| 房 | RCL | storage energy | storage T | 容器里的 T | 地里剩的 T | extractor | ext | tower |
|---|---|---|---|---|---|---|---|---|
| W12S28 | **7** | **3,746**（30 分钟前读到 0） | 19,848 | 140 | **矿已挖光、对象已删** | 1 | 50 | 3 |
| W13S28 | **7** | 625,402 | 15,716 | 780 | **矿已挖光、对象已删** | 1 | 50 | 3 |
| W15S28 | 6 | 355,827 | 332 | 720 | **19,760**（d3，起始 22,000） | 1 | 40 | 2 |

reserve 中：W12S27、W13S29。三个房都已建 extractor（ADR 0057 的 #251 已落地），W12S28 与 W13S28 的钍矿**已经挖光并被引擎删除**——`mod-season5` 的 `postProcessObject` 在 `mineralAmount` 归零时 `bulk.remove`，钍**永不再生**（`thorium-reactor.md` §1，本文未重读源码，沿用）。

**账上有 36,484 T（storage 35,564 + 矿物 container 920），记分板上是 0 分。**

- Reactor W15S25 (44,6) **是我们的**（`user` 是我们，`store.T = 0`，`launchTime: null`），`observe reactor` 读作 *banked 332 T · last delivery **never** · dry 15,069 ticks*。一支 `hauler-491206-Spawn3` 此刻的 Task 是 `refill:6a901a3bb8684d0008337ed2:Thorium`，也就是第一趟投递正在路上。
- 记分板：`giaco` 511,561、`Kalgen` 335,294、`CrAzYDubC` 293,347、`MeowKittyWow` 221,069、`Odiodin` 192,305……`Xerxes_2` **rank 37，无 score 字段**。
- 钍的分数口径（`thorium-season-plan.md` §2，规则侧已两次复核）：连续燃烧 9,999 tick 之后每 T 值 **5 分**，所以 **22,000 的矿 = 110,000 分、45,000 的矿 = 225,000 分**，而**已经存在两个 home 里的 36,484 T 值 182,420 分**。
- 赛季钟不是约束：`/api/game/shards/info` 的滚动均值 2,738 ms/tick，赛季 2026-11-01 结束，约还有 **1,475,000 tick**（season-end tick ≈ 1,966,000）。全部可达钍加起来约 10 万 tick 的反应堆时间，也就是 3 天多。
- GCL 14,104,557，`(n−1)^2.2 × 10^6` ⇒ 第四个房已开、**第五个要 21,110,000**；按 t302,458→t491,564 实测的 43.2 GCL/tick，第五个房大约落在 **t654,000**。所以本文选的是"第四个"，不是"最后一个"。

## 2. 候选集合怎么筛出来的

255 房扫描后按房名去掉 highway（`x%10 = 0 || y%10 = 0`）与 SK / centre 房（`x%10 ∈ 4..6 && y%10 ∈ 4..6`）——后两类没有 controller，而 `Outpost.Controller` 是必填字段，类型系统替我们拒了。再去掉有主的与别人 reserve 的（§6 的邻居表）。剩下**三个 home 的 3 跳并集内 26 个空的可 claim 房**，逐房读了 objects：

| 房 | src | 钍 | 常规矿 | 名义 hop（W12S28/W13S28/W15S28） | 到 Reactor 名义 hop |
|---|---|---|---|---|---|
| **W11S29** | 1 | **d4 45,000** @39,4 | H d1 15k | 2/3/5 | 8 |
| W9S28 | 2 | **d4 45,000** @47,35 | Z d1 15k | 3/4/6 | 9 |
| **W11S27** | 2 | d3 22,000 @47,12 | Z d2 35k | 2/3/5 | 6 |
| W11S26 | 2 | d3 22,000 @11,6 | O d2 35k | 3/4/6 | 5 |
| W13S25 | 2 | d3 22,000 @24,30 | H d2 35k | 4/3/5 | 2 |
| **W17S29** | 2 | d3 22,000 @16,44 | H d3 70k | 6/5/3 | 6 |
| **W13S26** | 1 | d3 22,000 @45,15 | O d2 35k | 3/2/4 | 3 |
| W12S25 | 1 | d3 22,000 @46,23 | L d3 70k | 3/4/6 | 3 |
| W12S26 | 1 | d3 22,000 @33,39 | U d2 35k | 2/3/5 | 4 |
| W18S28 | 1 | d3 22,000 @35,20 | O d2 35k | 6/5/3 | 6 |
| W14S28 | 1 | d3 22,000 @2,29 | U d3 70k | 2/1/1 | 4 |
| **W14S29** | 2 | d2 10,000 @31,30 | Z d2 35k | 3/2/2 | 5 |
| W14S27 | 1 | d2 10,000 @41,7 | H d3 70k | 3/2/2 | 3 |
| W16S27 | 1 | d2 10,000 @44,31 | Z d3 70k | 5/4/2 | 3 |
| W16S28 | 1 | d2 10,000 @15,31 | Z d2 35k | 4/3/1 | 4 |
| W17S28 | 1 | d2 10,000 @27,14 | L d1 15k | 5/4/2 | 5 |
| W11S28 | 1 | d2 10,000 @40,17 | X d2 35k | 1/2/4 | 7 |
| W12S29 | 1 | d2 10,000 @12,27 | X d3 70k | 1/2/4 | 7 |
| W13S27 | 1 | d2 10,000 @38,43 | L d3 70k | 2/1/3 | 4 |
| W13S31 | 1 | d2 10,000 @17,22 | K d3 70k | 4/3/5 | 8 |
| W16S29 | 2 | d1 3,000 @46,5 | H d2 35k | 5/4/2 | 5 |
| W12S31 | 2 | d1 3,000 @27,33 | O d2 35k | 3/4/6 | 9 |
| W15S31 | 2 | d1 3,000 @13,22 | K d2 35k | 6/5/3 | 6 |
| W15S27 | 1 | d2 10,000 @8,24 | U d3 70k | 4/3/1 | 2 |
| W15S29 | 1 | d1 3,000 @25,2 | H d2 35k | 4/3/1 | 4 |
| W9S27 | 2 | d2 10,000 @44,22 | O d3 70k | 4/5/7 | — |

外加两个**已经是我们 reservation** 的房，它们也是合法的第四个房候选：**W12S27**（1 src、**d3 22,000** @24,16、距 W12S28 一跳、已有 container/road/guard）与 **W13S29**（2 src、d2 10,000、距 W13S28 一跳）。

## 3. 地形先删掉一半候选 —— 本文最贵的一节

`Declaration.withinHopBudget` 只问房名，`Declaration.routable` 才问地（ADR 0058、#259），而 ADR 0062 之后它**两个方向都问**。本文把候选房的每一条边界双向数了一遍（`open` = ring 两侧非 wall **且**落地格旁有对面房自己的地）：

| 房 | 北 | 南 | 东（W 更大=西，故"东"是 x−1） | 西 |
|---|---|---|---|---|
| W13S25 | W13S24 **0** | W13S26 **0** | W12S25 **0** | W14S25 11 |
| W13S26 | W13S25 **0** | W13S27 **0** | W12S26 32 | W14S26 14 |
| W13S27 | W13S26 **0** | W13S28 34 | W12S27 **0** | W14S27 2 |
| W12S25 | W12S24 **0** | W12S26 16 | W11S25 4 | W13S25 **0** |
| W11S26 | W11S25 13 | W11S27 **0** | W10S26 **0** | W12S26 **0** |
| W11S27 | W11S26 **0** | W11S28 5 | W10S27 18 | W12S27 **0** |
| W11S28 | W11S27 5 | W11S29 **0** | W10S28 **0** | W12S28 **2** |
| W11S29 | W11S28 **0** | W11S30 10 | W10S29 17 | W12S29 17 |
| W9S28 | W9S27 23 | W9S29 25 | W8S28 **0** | W10S28 **0** |
| W10S28（highway） | W10S27 41 | W10S29 41 | W9S28 **0** | W11S28 **0** |
| W14S27 | W14S26 19 | W14S28 **0** | W13S27 2 | W15S27 20 |
| W16S27 | W16S26 15 | W16S28 **0** | W15S27 24 | W17S27 **0** |
| W16S28 | W16S27 **0** | W16S29 6 | W15S28 **0** | W17S28 19 |
| W16S29 | W16S28 6 | W16S30 34 | W15S29 41 | W17S29 35 |
| W17S29 | W17S28 5 | W17S30 23 | W16S29 35 | W18S29 24 |

（全表在 `/tmp/scout/borders.json`；双向数值在本文测的每一条边上都相等，唯一的例外是已知的 `W15S26 ↔ W15S25`（去 20 / 回 13）与 `W14S26 → W15S26`（去 **0** / 回 7）、`W16S25 → W15S25`（去 7 / 回 **0**）——三条都是 ADR 0062 的孤儿。）

**被地形删掉的候选，以及删掉它们的那一条边：**

- **W13S25 死了。** 它只有一条开着的边（W14S25，SK 房，11 格）。`third-colony.md` §5 说它"每一项都比 W15S28 好，落选只因为隔壁 W13S24 的 invader core"——今天 W13S24 的 core 早没了，而房间本身**从我们这侧走不进去**：北南东三面全墙，唯一的入口在 sector 中心那一侧。从 W13S28 走要 W13S27→W14S27→W14S26→W14S25→W13S25 五跳，`transitBetween` 的矩形之外，`routesBy` 在 `MaxHops = 3` 内找不到。这是 #259 的形状：名字层会接受它（3 跳），地形层拒绝。
- **W9S28 的 d4 45,000 到不了。** `third-colony.md` §9 把 45,000 的希望寄在 W11S29 上，本文顺手发现 W9S28 同样是 d4 45,000 **而且有两个 source**——但 `W10S28 ↔ W9S28` 与 `W11S28 ↔ W10S28` 都是 **0 格**：整个 W10 列在这一带被墙封死。从 W12S28 过去最短是绕 W9S27/W9S29，≥4 跳。**死。**
- **W11S26 的"三面全墙"复测成立**，而且现在知道开着的是哪一面：只有北面 W11S25（SK 房，13 格）。从我们这侧不可达。`remote-candidates.md` 标为 unverified 的那条结论**本文实测确认**。
- **W13S26 活着，但入口在另一边。** 它的南（W13S27）与北（W13S25）都是 0，开着的是 W12S26（32）与 W14S26（14）。所以它不是"W13S28 的 2 跳房"，而是 **W12S28 的 3 跳房**：W12S28→W12S27(36)→W12S26(35)→W13S26(32)，三道都宽、双向都通。
- **W12S31 死了**（`W12S30 ↔ W12S31` = 0 格），S30 整行是 highway，没有 controller。
- **W16S28 不是 W15S28 的 1 跳房**（`W15S28 ↔ W16S28` = 0 格，`third-colony.md` §6 已经写过这条），它是 3 跳房：W15S29→W16S29→W16S28。
- **W11S29 的入口只有一个**：`W11S28 ↔ W11S29` = 0，所以它只能从 W12S29 进。W12S28→W12S29(5)→W11S29(17) 两跳，或 W13S28→W13S29(12)→W12S29(41)→W11S29(17) 三跳，都是双向无孤儿。

## 4. 交付路径：为什么 "离 Reactor 三跳" 几乎买不到，以及 terminal 是唯一的通路

一个房的钍要变成分数，得有一条**代码今天能定价**的路把它送到 W15S25。今天只有一种路：这个房自己声明 `Errand`（ADR 0060），courier 走一条 ≤ `MaxHops = 3` 的 Seam 链（ADR 0067）。于是"这个房到 W15S25 是不是 3 跳**且地形上真的通**"就是全部问题。实测的答案：

| 房 | 到 Reactor 的链 | 双向通？ | 代价 |
|---|---|---|---|
| W15S28（已在跑） | W15S27(20) → W15S26(20) → W15S25(20 去 / 13 回) | 是 | keeper 房 W15S26 已声明 |
| **W13S26** | W14S26(14) → W14S25(3) → W15S25(6) | **是** | 要新声明 **两个** keeper 房（W14S26、W14S25） |
| W14S27 | W14S26(19) → W14S25(3) → W15S25(6) | 是 | 同上两个 |
| W16S27 | W16S26(15) → W15S26(8) → W15S25(20 去 / 13 回) | 是 | 要新声明 W16S26 |
| W12S25 | 名义 3 跳，实链要走 W13S25（0 格）或 W11S25 绕远 | **否** | — |
| W13S25 | 2 跳，但房间本身不可达（§3） | — | — |
| **W11S29 / W11S27 / W17S29 / W12S27 / W13S29 / W14S29** | 6–8 跳 | **超预算** | 只能靠 terminal（下） |

也就是说：**地形把"自带交付路"的候选压到只剩三个，而且都是单 source 的小矿房**（W13S26 22,000、W14S27 10,000、W16S27 10,000），并且每一个都要先给 `Keepers.centres` 加一到两个 SK 房——那是一份手抄的 lair 坐标（本文顺手读了：W14S25 `lair@16,14 43,16 48,43 13,45`，`src@13,12 44,40 10,43`，`minH@41,17`；W14S26 `lair@42,9 1,17 12,37 31,37`，`src@42,6 3,14 32,39`，`minK@10,38`；W16S26 `lair@4,9 41,12 39,37 15,41`，`src@41,8 37,39 13,44`，`minO@5,5`），而 `Keepers.fs` 的文档说得很清楚：这份数据的前提（lair 离它喂的矿在 margin 内）**没有任何测试能替它把关**。

**另一条路今天不存在，但赛季规则允许，而且它本来就是必须做的：terminal。** `mod-season5/src/terminal-restriction.js` 本文重读了全文，它只做一件事——把 `send` 的目标 terminal **属于别人**的那些请求置空：

```js
const target = _.find(terminals, {room: terminal.send.targetRoomName});
if(target && (terminal.user != target.user)) { terminal.send = null; }
```

自己发给自己**不受限**，钍也不例外。代价是 `ceil(amount · (1 − e^(−range/30)))` 能量（`utils.calcTerminalEnergyCost`，range 是房间的 Chebyshev 距离）：W12S28 → W15S28 是 range 3 ⇒ **每 1,000 T 约 95 能量**；W11S29 → W15S28 是 range 4 ⇒ 每 1,000 T 约 125 能量。**把两个 home 里存着的 36,484 T 全部搬到能交付的房，总代价不到 4,000 能量**，而 W13S28 的 storage 里有 625,402。

真正的代价是建筑本身：terminal 造价 100,000 能量、RCL6 解锁一座，而**这个 bot 的 Layout 里没有 terminal 这个词**（`grep -n Terminal src/Core/Decide/Layout.fs src/Core/Types/Vocabulary.fs src/Core/Types/Rules.fs` 零命中）。所以 terminal 是一张真票：Layout 要摆它、要有一条"把钍发给交付房"的决策、`send` 要进 intent。

**这张票的价值和第四个房无关，它已经值 182,420 分**（账上 36,484 T × 5），是任何一个第四个房的矿的 1.6–8 倍。本文因此把它当成**既定要做的事**，并据此选房——这是本文最重要的一步推理，也是最容易被质疑的一步，所以在 §7 给了"如果这张票不做"的分支。

## 5. Layout 实跑与仓库自己的定价

方法同 `third-colony.md` §3，用一个临时 Expecto 文件（`tests/Core.Tests/CandidateScoutTests.fs`，跑完已删、fsproj 已恢复、`dotnet test` 1377/1377 继续绿）：每个候选房按 `RoomInvariantFixtures.spawnTiles`（stride 6、平地、离 furniture ≥3）取 spawn 位，逐位 `project` + `colonyOf loaded 6` + 一次 `Decide.decide`，读 Extension / Road 计数与 `Memo.UnservedFootings` / `Memo.UnroutedTrunks`。共 543 个 (房, spawn) 组合。

| 房 | src | sweep 位 | 干净的位 | 最省 trunk | 在 | unserved / unrouted |
|---|---|---|---|---|---|---|
| **W11S29** | 1 | 49 | 49 | **20** | `12,30` | 0 / 0 |
| W11S27 | 2 | 37 | 37 | 43 | `36,30` | 0 / 0 |
| W11S26 | 2 | 36 | 36 | 43 | `36,18` | 0 / 0（但不可达） |
| W9S28 | 2 | 45 | 45 | 40 | `24,6` | 0 / 0（但不可达） |
| W13S25 | 2 | **16** | 16 | 34 | `24,36` | 0 / 0（但不可达） |
| W13S26 | 1 | 42 | 42 | **5** | `36,18` | 0 / 0 |
| W13S27 | 1 | 47 | 47 | 6 | `18,18` | 0 / 0 |
| W12S25 | 1 | 48 | 48 | 21 | `18,18` | 0 / 0 |
| W12S29 | 1 | 42 | 42 | 24 | `18,36` | 0 / 0 |
| W14S29 | 2 | 31 | 31 | 46 | `24,24` | 0 / 0 |
| W16S28 | 1 | 29 | 29 | 29 | `24,30` | 0 / 0 |
| W16S29 | 2 | 45 | 45 | 53 | `18,36` | 0 / 0 |
| W17S28 | 1 | 26 | **23** | 18 | `36,30` | 0 / **3 个位丢 trunk** |
| W17S29 | 2 | 34 | **32** | **70** | `24,42` | 0 / **2 个位丢 trunk** |
| 标尺：W12S28 | 2 | 42 | 42 | 29 | `18,30` | 0 / 0 |
| 标尺：W13S28 | 2 | 34 | 34 | 29 | `6,6` | 0 / 0 |
| 标尺：W15S28 | 2 | 34 | **33** | 57 | `24,24` | **1**（#331 已知）/ 0 |

三条要读出来的：

1. **本轮终于出了反例。** `third-colony.md` 那一轮九个房、三百多个 (房, spawn) 组合没有一个丢 trunk；本轮 **W17S28 26 个位里丢 3、W17S29 34 个里丢 2**（那五个位分别是 `6,30 / 12,12 / 18,12` 与 `48,18 / 48,42`）。ADR 0036 那句“真实地形是反例发生器”在这两个房上兑现了 —— 两个房都不是本文选的，但它们是第五、第六个房的候选，这五个位值得进 `RoomInvariantFixtures` 的 `SealedDoorsteps` 一类的档案（§10）。
2. **“基地摆不下”仍然不是任何候选房的否决项，Extension 处处摆满，unserved 全空。** 而且把 W11S29 / W9S28 / W11S27 重跑一遍 RCL7：**同一个最省 spawn 位、同一个 trunk 格数、同样空的损失集**，只有 extension 从 40 变 50 —— 这正是 ADR 0064 “reservation 不读 level” 透出来的样子。
3. **最省 trunk 的两个房（W13S26 5 格、W13S27 6 格）都是单 source 房**，而 W11S29 的 20 格是所有“带大矿且可达”的房里最短的。W15S28 当年付的是 57 格，本文选的房在这一项上比它便宜三分之二。

路径与价格（全部用 `Atlas.routes` / `castWalkTicks` / `haulRoundTripTicks` 在真实 capture 上算的；claim 价是 `[Claim;Move]` 走到 controller **旁边的可站格**，haul 价是 `bodyFor haulerPattern 2800` 从源的 Seat 到 mother 的 spawn 格的往返 —— capture 里没有 container 也没有 storage，这两个替代是本节数字唯一的堆叠误差）：

| mother → 候选 | 名义 hop | routable（去/回） | 等长链数 | claim walk | haul 往返 |
|---|---|---|---|---|---|
| **W12S28 → W11S29** | 2 | true / true | 1（W12S29） | **70** | **171** |
| **W13S28 → W11S29** | 3 | true / true | **2**（W12S28·W12S29 与 W13S29·W12S29） | 121 | 324 |
| W12S28 → W11S27 | 2 | true / true | 1（W11S28） | 100 | 297 / 282 |
| W13S28 → W11S27 | 3 | true / true | 1 | 151 | 450 / 435 |
| W12S28 → W13S26 | 3 | true / true | 1（W12S27·W12S26） | 247 | 624 |
| **W13S28 → W13S26** | 2 | **false / false** | 0 | — | — |
| **W13S28 → W13S25** | 3 | **false / false** | 0 | — | — |
| W13S28 → W14S29 | 2 | true / true | 2 | 54 | 261 / 251 |
| W15S28 → W16S29 | 2 | true / true | 1 | 55 | 183 / 253 |
| W15S28 → W17S29 | 3 | true / true | 1 | 106 | 353 / 252 |
| **W15S28 → W16S28** | **1** | **false / false** | 0 | — | — |
| W15S28 → W17S28 | 2 | **false / false** | 0 | — | — |
| W12S28 → W9S28 / W11S26 | 3 | **false / false** | 0 | — | — |

还有一张表是本文 §4 的根据：候选房自己能不能声明 `Errand.w15s25`。

| 候选 | 名义 hop | routable（去/回） | 链 | `[Claim;Move]` 走到 (44,6) |
|---|---|---|---|---|
| W13S25 | 2 | true / true | W14S25 | 182（但房间本身不可达） |
| **W13S26** | 3 | true / true | W14S26 · W14S25 | **140** |
| W12S25 | 3 | false / false | — | —（`W12S25↔W13S25` = 0） |
| 其他全部 | 4–9 | false | — | — |
| 标尺：W15S28 | 3 | true / true | W15S27 · W15S26 | 198 |

**这两个 140 / 182 必须带着警告读**：`Keepers.centres` 今天只声明了 W15S26，所以 W14S25 与 W14S26 是**没有任何 keeper mask** 被 flood 与数接缝的。两个数字与两个 `true` 是这个 bot **今天会算出的结果**，不是一只 courier 走得过去的结果 —— W15S26 自己的 mask 就把它 20 格的 band 切到了 13 个落地格。

## 6. 风险

**invader core / stronghold：6 跳内一个都没有了，而且是刚刚没有的。** `threats.mjs` 在 t491,507 逐房查了 6 跳内 99 个房，`invaderCore` 命中 **0**。t491,332（本文开工那一刻）W16S24 还站着一个 **level-1 stronghold**（1 tower、4 rampart、4 只 50 部件的 tough/attack/ranged 守卫，`EFFECT_COLLAPSE_TIMER` endTime **491,491**），以及它扩出的 W15S23 level-0 core——两个都在本文写作期间**按期塌掉**。sector 里最近的 core 现在是 W22S25（endTime 498,470，10 跳外）。`third-colony.md` 记的 W14S24 那个 level-1 core（endTime 330,267）也早已过期。**结论要小心地读**：这不是"本 sector 的入侵开关关了"，而是"这一刻没有 core 站着"；`genStrongholds` 会再生成，`ADR 0043` 的 stand-down 仍然是唯一的刹车。

**人类邻居（529 房扫描，t491,507）：**

| 玩家 | 自有 | reserve | 离我们最近 |
|---|---|---|---|
| **Odiodin**（rank 5, 192,305） | W5S19 (7)、**W12S23 (6)**、W14S22 (6) | W4S19 W5S17 W5S18 W6S18 W6S19 W11S23 W13S21 W13S22 **W13S23** W14S23 | **5 房** |
| **Trepidimous** | W18S26 (5) | W17S25、W18S27 | **4 房** |
| Ague | W6S29 (6)、W7S28 (4) | W6S28 | 5 房 |
| nightred | W9S17 (7)、W9S24 (6) | W8S17 W8S18 W8S24 W8S25 W9S16 W9S18 W9S25 | 6 房 |
| Shibdib | W18S23 (6)、W23S22 (5) | W17S23 W19S23 W21S22 W22S22 | 7 房 |
| Kamots / Kazkel / Mirroar / giaco | — | — | ≥10 房 |

比 2026-09-10 近的两条：**Odiodin 多了一个自有 W12S23（RCL6）**，他的 reservation 前沿 W13S23 离候选房 W13S26 只有 **3 房**；**Trepidimous 是新出现的**，W18S26 (RCL5) + reserve W18S27 离候选房 W17S29 只有 **2 房**，离 W18S28 一房。另外 Odiodin 与 Shibdib 都在往 W15S25 派 1-move 的 scout（raid log t491,389 起那一条记的就是他们，同一条 episode 里还有四只 Source Keeper 走到距我们 re-claimer 4 格）——**reactor 是可以被一只 `[CLAIM, MOVE]` 当场抢走的**（`claimReactor` 无 cooldown、无前置），而抢走不会打断 streak（`thorium-reactor.md` §4）。两个人在看它。

**出口数**（防守面，§3 的表里读出来的）：W11S29 只有 **1 个非 highway 出口**（W12S29），W13S26 有 **2 个**（W12S26 与 SK 房 W14S26），W11S27 有 2 个（W11S28 与 highway W10S27），W17S29 有 **4 个全开**（5/23/35/24）。ADR 0056 的 guard 是按 declared outpost 雇的，新房自己的四门在 RCL1–5 那几千 tick 里是敞开的，这是第四个 colony 的固有成本（`third-colony.md` §5 末尾那条，本文不重复论证）。

**CPU 是本文唯一一项"已经在门上"的风险。** `observe cpu` 在 t491,351–491,454 的 100 tick：

```
mean 40.34 ms   max 79.79 ms (t491,438)     limit 100
ADR 0041 revisit trigger (mean > 50 ms, or any single tick > 80 ms): not triggered
```

mean 分解：snapshot 10.44 / decide 23.39 / save 2.18 / execute 4.27。**单 tick 峰值 79.79 对门槛 80 —— 差 0.21 ms。**

而这条 declaration 本身的代价本文**量到了，而且不在噪声里**。`profile.mjs` 的 `pair` 场景（100 tick）在 declaration 前后各跑三轮：

| | 第一轮 | 第二轮 | 第三轮 |
|---|---|---|---|
| declaration 前 | 5.43 | 5.72 | 5.24 |
| declaration 后 | **7.27** | **6.76** | **6.47** |

两个区间**不重叠**：多出来的约 **+1.0–1.5 ms/tick（约 +20%）**，是三个新的地形层（W11S29 与 transit 的 W12S29、W11S28）加它们的 route。对比 `third-colony.md` 那一次的 5.38–5.89 → 5.52–5.94（区间重叠、读不出差别）—— **这一次是读得出来的**，而活体的 max 只差 0.21 ms 就到门。 第三个 colony 落地时（`third-colony.md` Summary）均值是 21.53、峰值 40.95；三个跑起来的 colony 之后翻了一倍。第四个 colony 会加一份 snapshot（它自己的房 + transit 房的地形层）、一次 `decide`、一份 Layout 与 plan memo。**所以这次和第三次不一样：CPU 是要先买回来的东西，不是事后量一量的东西。** 现成的两张票正对着这件事：**#332**（keeper mask 每 tick 重算，占"投影一个 masked 房的 tick"的 15–17% —— 而 W15S28 每 tick 都在投影 W15S26）与 **#278**（`RoomLayer.Terrain` 是每 tick 走一遍的树而不是平坦网格）。

## 7. 决定

**选 W11S29，mother 是 W13S28。**

把三个候选的账摆在一起（分数按 `thorium-season-plan.md` §2 的口径：9,999 tick 之后每 T 值 5 分，过 99,999 tick 值 6 分，总量 N 的不断烧为 `38,889 + 5(N−9,999)`）：

| 方案 | 可交付的 T | 不断烧的分数 | 前提 |
|---|---|---|---|
| 什么都不做 | 20,812（W15S28 地里的 19,760 + 库里的 1,052） | 92,954 | 无 |
| 只做 terminal | 57,296 | 275,374 | terminal 票 |
| **terminal + W11S29** | **102,296** | **502,671** | terminal 票 + 本文的 declaration |
| terminal + W13S26 | 79,296 | 385,374 | terminal 票 + 两个 SK 房的 `Keepers.centres` |
| 不做 terminal，改选 W13S26 | 42,812 | 202,954 | 两个 SK 房的 `Keepers.centres` |

两条理由而已：

1. **45,000 是 22,000 的两倍，而 W11S29 是所有候选里最便宜的那个**（claim 70–121 tick、haul 171–324 tick、trunk 20 格、一个非 highway 出口）。它唯一的实质缺点是 **1 个 source**，而那只拖慢 RCL6、不改分数上限 —— 赛季剩 1,475,000 tick，而 RCL6 预估用 25–30 万。
2. **W13S26 那个“自带交付路”的优势不值 23,000 T 的差价**，因为账上那 36,484 T 无论选哪个房都需要 terminal，而 terminal 一旦存在，“离 Reactor 几跳”这个坐标就从选房决策里消失。反过来说：**如果人类决定不做 terminal，那么本文的结论应当改成 W13S26**，这是本文唯一一条可以被一个决定推翻的结论，写在这里而不是藏在注释里。

**mother 选 W13S28 而不是更近的 W12S28**，理由是米而不是路：W12S28 的 storage 本文量到的是 **0 → 3,746**（三百 tick 里回了 3.7k，而 controller progress 同期 +5,260，约 17.5 e/tick 全进了升级）—— 它是个手停口停的 RCL7，**借不出 ferry 的货**；而 W13S28 的 storage 里有 **625,402**。ADR 0047 的 pioneer 与 ferry 花的是 mother 的存货，三跳的 324 tick 往返比“没钱”便宜。

**落地的两步，第二步不能忘**：

```
source     6a8caac6dd4872bccd3195f5  {W11S29;  7,33}
controller 6a8caac6dd4872bccd3195f4  {W11S29; 30,29}
Thorium    6a901a44b8684d00083389d6  {W11S29; 39,4}  d4 45,000
```

1. 新增 `Outpost.w11s29`（引擎真实 id），把它加进 **W13S28 的 `Outposts`**，并在 `Colony.declared` 末尾加一条 `{ Home = "W11S29"; Outposts = []; Errands = []; Mother = Some "W13S28" }`。两处一起写才是 candidate colony：mother 的列表让房进投影，第二条 entry 把那个 controller 从 Reserve 变成 Claim。**W13S29、W12S28、W12S29、W11S28** 作为 transit room 进投影——`transitBetween "W13S28" "W11S29"` 的矩形内部**整个**，W12S28 自己的 home 也在里面（`Views.fs` 的 `transiting` 把它 narrow 成只有地形，所以不进 pool、不进配额、不进 census）——只带地形与 border ring。三个新 capture（W11S29、W12S29、W11S28）因此都要 commit。
2. **claim 落地的那一天，把 W11S29 从 W13S28 的 `Outposts` 里去掉** —— `Colonies.fs` 里 W15S28 那段注释已经把原因写完了（`childrenWhere` 会把两个列表里都有的房归给 outpost，于是一个已拥有、无 spawn 的房被当成“我们挖的房”）。这同时也是把 haul 账限制在一个窗口里的办法：按 `observe quotas`，W13S28 今天是 demand 2,780 / 2 hauler，三跳外多一个 source 会把它推到 3 只——W14S28 当年（2 跳）是 2,790 → 4,810、2 → 4 只并因此被手动收回的，本文不重蹈那一步。
3. **CPU 先于 claim**：本文建议在派 claimer 之前先做 #332（或 #278）。declaration 本身只多两个地形层，而多一次 `decide` 是从自己的 spawn 站起那天开始的 —— 也就是说，这张票的截止时间是花园期而不是今天。

## 8. 顺序：第五、第六个房

GCL 每 tick 约 43.2（t302,458 的 5,939,008 → t491,564 的 14,104,557），第五个房要 21,110,000 ⇒ **约 t654,000**，第六个（5^2.2 = 34,490,000）约 **t964,000**。赛季到 t≈1,966,000，所以本季还能 claim **两到三个**房。本文的推荐排序（每一条都已在 §5 里定价）：

| 次序 | 房 | src | 钍 | mother / hop | 为何在这个位置 |
|---|---|---|---|---|---|
| 4（本文） | **W11S29** | 1 | **45,000** | W13S28 / 3 | 全场最富且可达的矿，最便宜的 bootstrap |
| 5 | **W11S27** | 2 | 22,000 | W12S28 / 2 | 唯一“双 source + d3 矿 + 可达”的房；入口是 2 格的窄缝，防守便宜、haul 贵 |
| 6 | **W13S26** 或 **W17S29** | 1 / 2 | 22,000 | W12S28 / 3、W15S28 / 3 | W13S26 自带交付路（但要两个 SK 房的 mask）；W17S29 双 source 但贴着 Trepidimous，而且 34 个 spawn 位里有 2 个丢 trunk |
| 不推荐 | W14S29 / W16S29 / W12S29 / W13S29 / W12S27 | | 10,000 / 3,000 / 10,000 / 10,000 / 22,000 | | 矿太小；W12S27 的 22,000 是例外（已是我们的 reservation、已有 container/road/guard、一跳），但它一旦独立就把 W12S28 的三分之一收入带走，而 W12S28 此刻的 storage 是 3,746 |

## 9. 本文没做的事 / unverified

- **terminal 路径没有在引擎上跑过。** `terminal-restriction.js` 本文重读了全文，`calcTerminalEnergyCost` 的公式与 `TERMINAL_*` 常量沿用 `thorium-reactor.md` §4 的读法，**本文未重读 `engine` 的 `terminal/tick.js`**，也未在服务器上实际 send 过一笔钍。标 **unverified**。
- **RCL6 的到达时间是一个粗估**（按 W15S28 的 183,000 tick / 两个 source 等比缩放到一个 source），没有算 pioneer / ferry 的贡献，也没有算 W12S29 何时归 W11S29。
- **钍的开采速率没有测。** W15S28 的矿从 22,000 降到 19,760，但本文没有把它换算成 T/tick（需要两次隔开的读数），也没有检查 reactor 的 `storeCapacityResource.T = 1000` 会不会把一趟 courier 的载重卡住。
- **W14S27 没有 capture**，所以 §4 里它“到 Reactor 三跳”是本文自己的接缝数字而不是仓库代码算的（W13S26 / W13S25 两条是代码算的）。
- **W14S25 / W14S26 / W16S26 的 keeper mask 没有声明**，所以涉及这三个房的每一个接缝与价格都是“无 mask”下的数。本文把 lair / rock 坐标读出来了（§4），但没有按 #327 的口径重测“加上 mask 还过得去吗”。
- **`genStrongholds` 的生成节律未核**：§6 只能说“此刻 6 跳内无 core”，不能说“本 sector 安全了”。
- **Odiodin 往 W15S25 派 scout 的意图未知**。他 rank 5、192,305 分，而我们占着他那一带的 reactor。本文不抄 `seasonal-threats-safemode.md` 的功课。
- **W9S27（2 src、d2 10,000）未入候选**：它在三个 home 的 4 跳外，只是在 §2 表里作为“W9S28 那一片长什么样”的参照。

## 10. 本文顺手暂露的缺口（都值得单开 ticket）

1. **Layout 里没有 terminal**，而它是本季单票价值最高的一张（182,420 分的已有库存）。
2. **`Keepers.centres` 里只有 W15S26**；第五、第六个房的交付路都要 W14S25 / W14S26 / W16S26。
3. **CPU 已到 ADR 0041 的门上**（max 79.79 / 80），#332 与 #278 应当从 needs-triage 抬上去。
4. **W17S28 / W17S29 的五个丢 trunk 的 spawn 位**（`W17S28 6,30 / 12,12 / 18,12`、`W17S29 48,18 / 48,42`）是两个房未来入选时要先归档的反例。
5. **`profile.mjs` 的第一个 self-check 把墙格数当成了地形的指纹**，于是一条真的 declaration 把闸门打红了：**W13S29 与 W12S29 恰好都是 710 个墙格**，三个站着多于一个房的场景全部失败并指控 stub “忽略了房名参数”（`stub` 只有一个房，闸门本身跳过）。本次一并修了（改成逐格比整张 grid，并把碰撞的那一对房名打出来）。**要读出来的一般结论**：这是 `third-colony.md` §8 那个 `DECLARED_UNFURNISHED` 缺口的同一个形状 —— #287 把“哪些房”从 harness 里抽走了，却留下了一个对“这些房长什么样”做哈希的校验。
6. **§3 的六个“名字层接受、地形层拒绝”的房**说明 #259 的口子在真实地图上是常态而不是特例；`ColonyView.Refused` 这个渠道值得一个能从终端读的命令。
7. **W15S25 本身没有 keeper lair，却有 Source Keeper 走到距 reactor 6 格**（raid log t491,698 那一条，它记的是 (38,5)）。最近的矿离 reactor 12 格，所以稳态下我们的 re-claimer 安全；但“为何会走到 6 格”本文没查，标 **unverified**，它是 #327 的形状。
