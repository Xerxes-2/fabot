# 第三个 colony 选哪个房 — 一手实测、Layout 实跑、按 hop 预算与 Reactor 距离定的选择

Date: 2026-09-10（tick 302,458–302,650，`shardSeason`）。本文所有房间事实都是**当日用只读 API 在赛季服实测的**；所有 Layout 结论都是**用仓库自己的 `Decide.decide` 对候选房的真实地形跑出来的**，不是看图估的；接缝宽度是本文自己按 `Atlas.seams` 的口径数的。姊妹篇：`remote-candidates.md`（2026-09-07 的 30 房 outpost 普查，本文推翻它三条结论，见 §7）、`thorium-reactor.md`（钍与 Reactor 的引擎侧读法）。动机：GCL 刚过第三个房的门槛，而两个 home 都到了 RCL6 —— 第三个房该 claim 哪里。

**Verified against**（当日一手 API 调用，脚本全部在 `/tmp/claude-1000/-home-xerxes2-Dev-fabot/38e968fc-.../scratchpad/`）：

- `who.mjs` — `GET /api/auth/me`、`/api/game/time`、`/api/user/rooms`：账号、GCL、tick、自有与 reserve 的房。
- `survey.mjs` — `POST /api/game/map-stats`（`statName: owner0`）扫 W10–W19 × S20–S29 共 100 房，按 `x%10 / y%10` 分出 highway / SK / center / claimable，并算到两个 home 与到 Reactor 的距离。
- `objs.mjs` / `dens.mjs` — 逐房 `GET /api/game/room-objects`：source 与 controller 的坐标与**引擎真实 id**、矿种、**密度与当前储量**、invader core 的 level 与 `effects`。
- `seams.mjs` — 逐对房 `GET /api/game/room-terrain?encoded=1`，按 `Atlas.seams` 的口径数接缝：只数内部格 1..48，两侧都非 wall 才算一格。
- `home.mjs` — 两个 home 的 RCL、storage、creep 数、建筑数。
- `ids.mjs` — 要写进 `Colony.declared` 的引擎 id。
- 一个**临时** Expecto 报告（`tests/Core.Tests/CandidateScoutTests.fs`，跑完已删）：对每个候选房的committed capture 做 spawn sweep（`RoomInvariantFixtures.spawnTiles`，stride 6、平地、离 furniture ≥3 格），每个 spawn 位跑一次 `Decide.decide`（RCL6，`colonyOf loaded 6`），记录 extension 数、road 数、`Memo.UnservedFootings`、`Memo.UnroutedTrunks`。
- 代码与 ADR 侧核对：`src/Core/Types/Colonies.fs`（`Outpost`、`Outpost.withinHopBudget`、`Outpost.roomsProjected`、`Colony.declared`）、`src/Core/Types/Geometry.fs`（`RoomName.transitBetween`、`routeBy`）、`src/Core/Types/Rules.fs`（`Tuning.MaxHops = 3`、`Tuning.ExtractorLevel`）、ADR 0041 / 0042 / 0047 / 0055 / 0057 / 0058。

## Summary

- **第三个 colony 选 W15S28，mother 是 W13S28，走 ADR 0047 的 candidate-colony 路径。** 2 source（`10,19`、`6,30`）、controller `25,31`、Thorium **d3 22,000** @`29,12`、常规矿 O d3 70,000。已写进 `Colony.declared`（W13S28 的 outpost 列表 + 自己的一条 entry）。
- **选它的决定性理由是 hop 预算，不是能量。** 从 W15S28 到 sector Reactor W15S25 是 **3 hop**（W15S27 → W15S26(SK) → W15S25，三道接缝实测 20 / 20 / 20 格全开），正好等于 `Tuning.MaxHops = 3`；从 W12S28 是 6 hop、从 W13S28 是 5 hop，**两个 home 都在 ADR 0058 的 chain 模型之外**。ADR 0057 决策 4 当初要写死 `Tuning.ReactorRoute` 就是因为这个距离。
- **两个 home 都已 RCL6，但一个 extractor 都没建 —— 钍至今一克没挖。** W12S28 storage 487（在猛升级）、W13S28 storage 143,316；各 40 extension、2 tower、1 spawn。ADR 0057 的 44,000 可达储量在**#251 落地之前是 0**。
- **GCL 5,939,008 ⇒ GCL3，第三个房是最后一格。** 第四个房要 11,190,000（`(n−1)^2.2 × 10^6`），现在只到 53%。所以这一枪没有第二次机会。
- **CPU 有余量，而且这次的代价量过了**：`observe cpu` 100 tick 均值 21.53 ms、峰值 40.95 ms（t302,515），ADR 0041 的 revisit 门（均值 >50 或单 tick >80）没触发，limit 100。profile harness 的 `pair` 场景在这条 declaration 前后各跑三轮 100 tick（各自的干净 workspace）：**前 5.38–5.89 ms，后 5.52–5.94 ms** —— 两个区间重叠，所以这条 declaration 多出来的那点（两个房的地形层：W15S28 与 transit 的 W14S28，加它们的 route）**在这个 harness 的 100 tick 噪声里分辨不出来**，不要把它读成 +6%。第三个 colony 多一次 `decide` 与一份 Layout 要等它自己的 spawn 立起来才算，账上放得下。
- **Layout 不是任何候选房的否决项。** 九个候选房逐房 sweep：**每一个 spawn 位都是 ext=40 / unserved=0 / unrouted=0**，没有一个房需要为"基地摆不下"落选。差别只在 trunk 长度（最省的 spawn 位）：W13S27 6 格 < W14S28 13 < W15S27 22 < W11S28 22 < W12S29 24 < W12S28 29 = W13S28 29 < W13S29 35 < W13S25 38 < W12S27 41 < W11S27 43 = W11S26 43 < W14S29 46 < W16S29 53 < **W15S28 57**。**W15S28 是候选里 trunk 最长的房，这是它唯一的实质代价。**
- **钍的密度差三倍，而且不在我们选的房里。** 全 sector 每个 normal 房都有一处 Thorium，储量按密度：d4 45,000 / d3 22,000 / d2 10,000 / d1 3,000。**最富的是 W11S29 的 d4 45,000**（`39,4`），是任何其他候选房的两倍 —— 但它 1 source、角落房、到 Reactor 8 hop，见 §6。
- **风险面比三天前干净了。** `remote-candidates.md` 写的 W16S25 那个 level-2 stronghold **已按期塌掉**（今日该房无 core）。当下 sector 内只有 **W14S24 的 level-1 core**（1 tower、4 rampart、4 只 creep，`EFFECT_COLLAPSE_TIMER` endTime **330,267**，约 27,600 tick）和它扩出的 **W13S24 level-0 core**。W15S28 距 W14S24 五房；**W13S25 距 W13S24 一房，这是它落选的唯一原因**。
- **人类邻居仍然不接壤，但比三天前近了一级。** Odiodin 自有 W14S22 已从 RCL4 升到 **RCL5**，reserve W13S22 / W13S23（W14S23 今日读不到 reservation，标 unverified）；东边 W18S23 有一家 RCL4（id 尾 `f20f`）。W15S28 四周三房内无 owner、无 reservation、无敌对建筑。

## 1. 前提：今天的殖民地

| 房 | RCL | controller progress | storage | creep | ext | tower | spawn | extractor |
|---|---|---|---|---|---|---|---|---|
| W12S28 | 6 | 1,777,688 | **487** | 7 | 40 | 2 | 1 | **0** |
| W13S28 | 6 | 547,661 | 143,316 | 9 | 40 | 2 | 1 | **0** |

reserve 中：W12S27（W12S28 的 outpost）、W13S29（W13S28 的）。两条都在 `map-stats` 里读作 `own.level = 0`，与 `Colony.declared` 一致。

**这张表推翻了 ADR 0057 前提段的两个数**：那段写的是"W12S28 RCL5、258k storage、GCL 1,823,489"，今日是 RCL6、487、5,939,008。ADR 0057 自己那句 *"every live number here should be re-read before the first ticket lands"* 就是为这种情况写的。

## 2. 候选集合怎么筛出来的

100 房扫描后先按房名去掉不可 claim 的：`x%10 = 0 || y%10 = 0` 是 highway，`x%10 ∈ 4..6 && y%10 ∈ 4..6` 是 SK 房（其中 W15S25 是 sector centre，即 Reactor 房）。SK 房与 centre **没有 controller**，而 `Outpost.Controller` 是必填字段（`src/Core/Types/Colonies.fs:22-29`），claim 更谈不上 —— 类型系统替我们拒了，今日复核仍成立。

再去掉有主的：Odiodin 的 W14S22（RCL5）与其 reserve、`f20f` 的 W18S23（RCL4）、invader 的 W13S24 / W14S24。剩下的 claimable 空房里，**只有 2 source 的房值得当 colony**（1 source 的房 RCL 爬升太慢，而 extractor 要 RCL6），于是候选是：

| 房 | source | controller | 常规矿 | Thorium | 到 Reactor（hop） | 到两个 home（hop） |
|---|---|---|---|---|---|---|
| **W15S28** | 2：`10,19` `6,30` | `25,31` | O d3 70k | `29,12` **d3 22,000** | **3** | 2（W13S28） |
| W14S29 | 2：`12,33` `31,35` | `29,5` | Z d2 35k | `31,30` d2 10,000 | 5 | 2 |
| W13S25 | 2：`20,15` `32,28` | `19,37` | H d2 35k | `24,30` d3 22,000 | **2** | 3 |
| W11S27 | 2：`13,28` `46,33` | `14,29` | Z d2 35k | `47,12` d3 22,000 | 6 | 2 |
| W11S26 | 2：`46,10` `44,38` | `33,23` | O d2 35k | `11,6` d3 22,000 | 5 | 3 |
| W16S29 | 2：`23,22` `17,43` | `23,32` | H d2 35k | `46,5` d1 3,000 | 5 | 3 |
| 参照：W13S29（已 reserve） | 2：`29,6` `14,29` | `15,41` | O d1 15k | `38,23` d2 10,000 | 6 | 1 |

hop 数是房名网格上的正交步数（`RoomName.hopsBetween` 的口径）。**W11S26 从我们这侧走不到**：`remote-candidates.md` §1 实测它南、东、西三边 48 格全是 wall，只有北面通向框外 —— 本文未复测该房地形，沿用该结论并标 **unverified**。

## 3. Layout 实跑（临时 sweep，跑完已删）

方法：每个房读committed capture（`tests/Core.Tests/rooms/*.room`，缺的当日 `npm run capture-room` 抓、跑完删），按 `RoomInvariantFixtures.spawnTiles` 取 spawn 位（stride 6、平地、离 source/controller ≥3 格），每个位置 `project` 出一个 `SpatialInfo`、`colonyOf loaded 6` 造一个 RCL6 的 `ColonyView`、跑一次 `Decide.decide`，然后读三个数：`PlaceConstructionSite` 里的 Extension 与 Road 计数、`Memo.UnservedFootings`、`Memo.UnroutedTrunks`。RCL6 是 ADR 0055 的 horizon，也是 `Tuning.ExtractorLevel`。

| 房 | source | sweep 的 spawn 位 | 干净的位 | 最省 spawn | ext | road | unserved | unrouted |
|---|---|---|---|---|---|---|---|---|
| W12S28（在跑，标尺） | 2 | 42 | 42 | `18,30` | 40 | 29 | 0 | 0 |
| W13S28（在跑，标尺） | 2 | 34 | 34 | `6,6` | 40 | 29 | 0 | 0 |
| **W15S28** | 2 | 34 | 34 | `18,30` | 40 | **57** | 0 | 0 |
| W14S29 | 2 | 31 | 31 | `24,24` | 40 | 46 | 0 | 0 |
| W13S25 | 2 | **16** | 16 | `18,18` | 40 | 38 | 0 | 0 |
| W11S27 | 2 | 37 | 37 | `36,30` | 40 | 43 | 0 | 0 |
| W11S26 | 2 | 36 | 36 | `36,18` | 40 | 43 | 0 | 0 |
| W16S29 | 2 | 45 | 45 | `18,36` | 40 | 53 | 0 | 0 |
| W13S29 | 2 | 49 | 48 | `24,12` | 40 | 35 | 0 | 0 |
| W12S27 | 1 | 17 | 16 | `12,48` | 40 | 41 | 0 | 0 |
| W14S28 | 1 | 38 | 36 | `18,12` | 40 | **13** | 0 | 0 |
| W15S27 | 1 | 52 | 52 | `12,18` | 40 | 22 | 0 | 0 |

三条要读出来的：

1. **"基地摆不下"不是任何候选房的问题。** 九个候选房、三百多个 (房, spawn) 组合，`UnservedFootings` 与 `UnroutedTrunks` 全空，Extension 全部摆满 40。ADR 0036 那句"真实地形是反例发生器"在这一轮里没有生出反例 —— 唯一的例外是 W13S29 的 49 个位里有 1 个、W12S27 的 17 里有 1 个、W14S28 的 38 里有 2 个不干净，而它们都不是我们要选的 spawn 位（`RoomInvariantFixtures` 里 W12S27 的 `SealedDoorsteps` 记的就是这类位，#105）。
2. **W15S28 的 trunk 是候选里最长的（57 格）。** 两个 source 在西侧（`10,19`、`6,30`）而 controller 在东南（`25,31`），最省的 spawn 位 `18,30` 也要把两条 trunk 拉过半个房。按 ADR 0010 的口径这是一次性的建造能量（≈17,100 e）加上 `0.001×D` 的维护，不是每 tick 的税；对比 W13S27 的 6 格，这是本文唯一为"位置"付的价。
3. **W13S25 只有 16 个 sweep 位**，是候选里最挤的房 —— 墙多。它的 trunk（38）反而短，因为 source 与 controller 挨得近。地形上它是个好房；落选是 §5 的原因。

## 4. Reactor 距离与 hop 预算 —— 本文的决定性一节

`Tuning.MaxHops = 3`（`src/Core/Types/Rules.fs:276`）。ADR 0058 把一次 walk 的定价推广成"一条最多 3 道 Seam 的 chain，每一段仍是单房 flood"，`Outpost.withinHopBudget` 与 `Outpost.routable` 是这条预算的两个门（名字层 / 地形层）。所以**一个 colony 能不能把 Reactor 房当成可定价的目标，取决于它到 W15S25 是几 hop**：

| 起点 | 到 W15S25 的 chain | hop | 在预算内？ |
|---|---|---|---|
| **W15S28** | W15S27 → W15S26(SK) → W15S25 | **3** | **是（正好用满）** |
| W13S25 | W14S25(SK) → W15S25 | 2 | 是 |
| W13S28 | W14S28 → W15S28 → W15S27 → W15S26 → W15S25 | 5 | 否 |
| W12S28 | 同上再加一跳 | 6 | 否 |
| W13S29 / W11S27 | — | 6 | 否 |

沿途接缝实测（`seams.mjs`，只数两侧都非 wall 的内部格）：

| 边 | 格数 | 用途 |
|---|---|---|
| W13S28 ↔ W14S28 | **21** | 到 W15S28 的 chain 第一跳；与 `remote-candidates.md` 的 21 格逐字相符（本文方法的交叉验证） |
| W14S28 ↔ W15S28 | 29 | 第二跳 |
| W15S28 ↔ W15S27 | 20 | 到 Reactor 第一跳 |
| W15S27 ↔ W15S26 | 20 | 第二跳（进 SK 房） |
| W15S26 ↔ W15S25 | 20 | 第三跳（进 Reactor 房） |
| W15S28 ↔ W15S29 | 26 | 将来的 outpost |
| W15S28 ↔ W16S28 | **0** | **整边全墙 —— W16S28 永远不能当 W15S28 的 outpost** |
| W14S28 ↔ W14S29 | 44 | W14S29 归谁的问题（见 §6） |
| W13S28 ↔ W13S29 | 12 | 现有 outpost |
| W12S28 ↔ W11S28 | 2 | 参照：全 sector 最窄的一条 |

**必须诚实说清楚这一节买到的是什么，不是什么。**

- **买到的**：W15S28 的 storage 到 Reactor 房里的任何目标，`Atlas.route` 能找出 chain（3 hop，且 `transitBetween` 的矩形就是这条直线，沿途三道接缝各 20 格全开，不需要绕墙），`pricedAcrossInto` / `haulRoundTripTicks` / `castWalkTicks` 能给出价（ADR 0058 决策 3）。**从两个 home 出发这件事今天做不到**，除非把 MaxHops 从 3 抬到 5–6，而那正是 ADR 0041 用"三十几次加法而不是三十几次 flood"论证过要控制的东西。
- **没买到的**：**投递本身还没实现**。ADR 0057 的 #251 一颗砖都没落地（两个房 extractor = 0），而且 **Reactor 房 W15S25 没有 controller，永远不能写成一个 `Outpost`** —— `Outpost.Controller` 必填。所以"3 hop"意味着的是"现有的 Seam-chain 定价模型能覆盖这段路"，不是"declare 一下就能送钍"。#251 / #263 仍然需要自己的词汇（一个不叫 outpost 的远程目标），本文不替它设计。

## 5. 风险

**invader core / stronghold**（`dens.mjs`，逐房读 `effects`）：

| 房 | 类型 | core | 建筑 | effects |
|---|---|---|---|---|
| W14S24 | SK | **level 1** | 1 tower、4 rampart、4 creep | `1001` endTime 254,349（已过期）、`1002` endTime **330,267** duration 75,919 |
| W13S24 | claimable，被 invader reserve | level 0 | — | 同上两条 |
| W16S25 | claimable | **无** | — | — |

`1002` 按引擎常量是 `EFFECT_COLLAPSE_TIMER`（口径同 `remote-candidates.md` §4，本文未再核引擎源码，标 **unverified**）：**W14S24 那个 stronghold 在 tick 330,267 塌**，约 27,600 tick 后。到那之前 sector 的入侵开关是开的。

- **W13S25 距 W13S24 一房**，而 W13S24 已经是 invader 的 reservation + level-0 core。一个要在 RCL1–5 待几千 tick、没有 tower 的新房放在那里，会被反复清 —— 这是 W13S25 唯一但充分的落选理由，它其他每一项（2 hop 到 Reactor、d3 22k 钍、trunk 38）都比 W15S28 好。
- **W15S28 距 W14S24 五房、距 W13S24 六房**。`INVADER_CORE_EXPAND_TIME` 的扩张是一房一房来的（口径同姊妹篇），五房的缓冲加上 27,600 tick 的塌台钟，够我们把它升到有塔。
- **旧研究里那个"W16S27 的 core 离 W14S28 只有两房"已经不成立**：W16S25 的 stronghold 塌了，它扩出的七个 level-0 core（含 W16S27）今日读不到 core。这条是 §7 的第一项过期结论。

**人类邻居**：Odiodin（W14S22 **RCL5**，reserve W13S22 / W13S23）在北，最近一格距 W13S28 五房、距 W15S28 六房；东边 `f20f` 的 W18S23（RCL4）距 W15S28 **三房** —— 这是本文唯一一项"比三天前更近"的风险，但仍不接壤，且他的方向是北/东。W15S28 三房内无 owner、无 reservation、无敌对建筑、无他人 creep。

**出口门**（`genInvaders` 的 `checkExit` 拒绝邻房已 owned/reserved 的出口，口径见 `remote-mining.md` §1.4）：W15S28 claim 之后，它自己的四个出口是 W14S28（W13S28 的 transit，未 reserve）、W15S27、W15S29、W16S28（0 格接缝，引擎侧是否仍算合法出口未核，标 **unverified**）。**新房一开始是四门全开的入侵产房**，这是第三个 colony 的固有成本，不是选址问题。

## 6. 决定与落地

**已改 `src/Core/Types/Colonies.fs`**：新增 `Outpost.w15s28`（引擎真实 id，source 顺序取committed capture 的顺序），W13S28 的 `Outposts` 变成 `[ w13s29; w15s28 ]`，并在 `Colony.declared` 末尾加一条 `{ Home = "W15S28"; Outposts = []; Mother = Some "W13S28" }`。

```
source     6a8caa95dd4872bccd319014  {W15S28;  6,30}
source     6a8caa95dd4872bccd319013  {W15S28; 10,19}
controller 6a8caa95dd4872bccd319015  {W15S28; 25,31}
```

这两处一起写才是 ADR 0047 的 candidate-colony 安排：mother 的 outpost 列表让房子进投影，第二条 entry 让那个 controller 从 Reserve 变成 Claim（`RoomOutpostTests` 的 *"the candidate colony's controller is the pool's one Claim, on the real rooms"* 就是这条的针）。`Outpost.withinHopBudget` 的活体测试（`ViewTests` 的 *"every outpost a human has declared is inside the hop budget"*）覆盖这条新 declaration：2 hop ≤ 3，绿。

**W14S28 会作为 transit room 进投影**（`RoomName.transitBetween "W13S28" "W15S28" = [W14S28]`），只带地形与 border ring，不铺 furniture、不进 pool、不雇人（ADR 0058 决策 2）。

**要盯的三件事**（都不是本文能算的，属于 `multihop-outposts.md` 与部署后的观察）：

1. **两跳 outpost 的 hauler 账**。W15S28 在 bootstrap 窗口里是 W13S28 的 outpost，而它的 source 离 W13S28 的 storage 是两跳、一百多格 —— ADR 0049 的 `haulerDemandOf` 会给出一个大配额，而 ADR 0042 的 reserver 这次不需要（declare 成 candidate colony 之后是 Claim 不是 Reserve，一次性）。**这是第三个 colony 最贵的一段，也是它最短的一段**：claim 落地、自己的 spawn 立起来之后，这段 haul 就没了。
2. **W15S27 / W15S29 归谁**。两个房对 W15S28 是 1 hop（接缝 20 / 26），对 W13S28 是 3 hop。等 W15S28 独立之后再 declare 给它，比现在declare 给 W13S28 省一半路。**W14S29 是两边都能要的房**（对 W13S28 与对 W15S28 都是 2 hop，2 源）：`multihop-outposts.md` 把它排给 W13S28 的第 6 位（可选、要等 #257），本文不与它争 —— 归谁按那份文档的账定，不按本文。
3. **W16S28 不要 declare**：`W15S28 ↔ W16S28` 接缝 0 格。ADR 0058 决策 2 的代价条款在这里最刺眼 —— 名字层的 `withinHopBudget` 会**接受**它（1 hop），地形层的 `routable` 才会拒（#259 的口子），而绕路要经过 W16S27 / W15S27，落在 `transitBetween` 的矩形之外，**找不到**。

## 7. 哪几条 `remote-candidates.md` 的结论已经过期

那份文档不改不删；已过期的是这三条：

1. **"能 declare 的房只有五个（正交相邻）"** —— ADR 0058 之后，1–3 hop 都能 declare，对角房也能。那份文档 §5 整节（"代码今天做不到的事"）已经被 ADR 0058 关掉，它 §2.2 那张"走得到但定不了价"的表现在**全部可定价**。本文选的 W15S28 正是那张表里的一行（它当时的批注是"两房外，紧邻 invader 前沿"，两条都不再成立）。新的普查见 `multihop-outposts.md`。
2. **"W16S25 的 level-2 stronghold，collapse 定在 249,241，已扩出七个 level-0 core（含 W16S27）"** —— 已按期塌掉，今日 W16S25 与 W16S27 都读不到 core。接班的是 W14S24 的 level-1 core，新日期 **330,267**。
3. **"W12S28 bank 1800 / W13S28 bank 1300"** —— 两个房今日都 **RCL6**，bank 2300。那份文档 §3 注 1 自己预言过这件事（"W12S28 升到 RCL6 会把 W11S28 与 W12S29 的配额压回 1 只"），现在该按新 bank 重算，这也是 `multihop-outposts.md` 的活。

另外**它 §6 里"若要吃 Thorium，正确的动作是把 W13S29 或 W11S27 claim 成第三个 colony"这条建议本文不采纳**，理由是当时还没有 hop chain、也没算 Reactor 距离：W13S29 的钍只有 d2 10,000 且到 Reactor 6 hop，W11S27 是 d3 22,000 但同样 6 hop。**钍的储量决定分数上限，Reactor 的距离决定这段路今天能不能被定价** —— 两项一起看，W15S28（d3 22,000 + 3 hop）胜过两者。

## 8. 这条 declaration 顺手暴露的一个闸门缺口

`scripts/profile.mjs` 的 `DECLARED_UNFURNISHED` 是硬编码的房名表（当时只有 `["W13S29"]`），而 `outpost` / `young` / `pair` 三个场景刻意不传 `unmodelled` —— 所以 W13S28 一旦多declare 一个房，三个场景全部在 `getRoomTerrain` 上抛异常（`the stub world holds no terrain for W15S28`），只有 `stub` 活着。ADR 0058 早写下过这件事的一半（*"A profile scenario that stands a multi-hop outpost properly is its own ticket."*），但它写的是"站好一个多跳 outpost"，没写"忘了它会让闸门整个熄掉"。本次一并修了（补上 W15S28 与 transit 的 W14S28，两个 capture 都早已 committed），四个场景现在都跑得通，Summary 里那两个区间就是这么量出来的。**要读出来的一般结论**：`DECLARED_UNFURNISHED` 是 `Colony.declared` 的影子，改 declaration 必须一起改它，而它今天没有任何测试保护 —— 这值得单开一张 ticket（让 harness 自己从 `Colony.declared` 推这张表，而不是抄一份）。

## 9. 本文没做的事

- **W11S29 的 d4 45,000 没有被认真估价。** 它是全 sector 最富的钍矿（是任何候选房的两倍，按 ADR 0057 的"1 T = 1 reactor tick、稳态每 T ≈5 分"折算约 22.5 万分的上限），但 1 source、角落房（非 highway 邻居只有 W11S28 与 W12S29，两个都是 W12S28 的 outpost 预定）、到 Reactor 8 hop。**它该是第四个房（GCL4 要 11,190,000，今日 5,939,008）或者一支专门的采钍任务的目标**，值得单开一张 ticket 算，不该塞进第三个 colony 的选择里。
- **钍是否会再生、按什么速率**，本文未核（`thorium-reactor.md` 是引擎侧的读法，但再生这一条本文没有回读）。标 **unverified**。如果不再生，那么"总储量 = 总分数上限"，选房就该更偏向密度而不是距离，W11S29 的权重会明显上升。
- **W11S26 的地形**未复测（沿用旧文的"三面全墙"）。
- **W14S23 是否仍在 Odiodin 的 reservation 下**未确认（今日 `map-stats` 读不到该房的 `own`/`rsv`）。
- **`checkExit` 对 0 格接缝的邻房算不算合法出口**未核（§5 末）。
