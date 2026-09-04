# 跨房 remote mining — 引擎算术、成熟 bot 的做法，与本服实测

Date: 2026-09-05. 所有论断均对照一手来源核验（`screeps/engine`、`screeps/common`、`screeps/backend-local`、`screeps/docs` 的默认分支源码，本日 shallow-clone 本地阅读；七个 bot / 库仓库的默认分支源码同样本地阅读；官方 Steam 公告），未核实处标注 **unverified**。另有一批数字不是读来的而是**本日在官方赛季服 `shardSeason` 上用只读 API 实测的**，正文里一律写明测量 tick，并与"在源码里读到的"严格区分。动机：issue #83（跨房 remote mining 的可行性与目标房选择，W12S27 先行、W13S28 次之）。#83 已经做完本地地形底稿（出口贴片、多房 BFS 距离、路线暴露度），本文不重做，只回答两件 #83 缺的事：**社区成熟 bot 怎么做**，以及**这台服务器上的邻居正在怎么做**。姊妹篇：`body-design-task-matching.md`（本房体型，本文只补它缺的 remote 行）、`seasonal-threats-safemode.md`（赛季规则与 invader 机制）。

阅读的 commit（本日 shallow clone，默认分支）：`screeps/engine` 8097782、`screeps/common` 2fb779b、`screeps/backend-local` 9d07928、`screeps/docs` a3cfc15；Overmind 5eca49a（2019-06-14）、bonzAI 4a0006d（2017-09-24）、The International 7e5106e（2024-08-27）、Quorum f6868c5（2020-03-16）、TooAngel 87645a0（2026-08-17）、screeps-cartographer a801645（v1.8.16, 2026-08-18）、Jon Winsley `glitchassassin/screeps` a835bbc（2023-07-28，最后一个公开快照，README 已挂弃用通知）。Overmind 的顶层 `Overmind` 对象只以混淆 JS 形式发布（`src/Overmind_obfuscated.js`），outpost→colony 的注册逻辑不可读，涉及处已标注。

## Summary

- **reserve 的引擎收益是精确的 +5 e/tick/source**：`sources/tick.js` 按 `roomController.user || roomController.reservation` 把 `energyCapacity` 在 `SOURCE_ENERGY_NEUTRAL_CAPACITY = 1500` 与 `SOURCE_ENERGY_CAPACITY = 3000` 之间切换，regen 周期恒为 300 tick。**成本的关键不是 CLAIM 的数量而是 `CREEP_CLAIM_LIFE_TIME = 600`**：带 CLAIM 的 creep 只活 600 tick，`[2Claim;2Move]` 摊销 1300/600 = **2.17 e/tick**，`[1Claim;1Move]` 只要 **1.08 e/tick**。`CONTROLLER_RESERVE = 1`，所以 n 个 CLAIM 每 tick 给 `endTime` 加 n，而 reservation 自身每 tick 掉 1 —— **1 个 CLAIM 只能把 reservation 钉在"还剩 2 tick"上，攒不出任何缓冲；2 个 CLAIM 才每 tick 净攒 1**。缓冲上限 `CONTROLLER_RESERVE_MAX = 5000`。
- **单源房 reserve 划算但很薄，双源房明显划算。** 按引擎常量与社区体型算全账（矿工 + hauler + container 衰减 + 路面维护，§3.1）：单源房 reserve 比不 reserve 净多约 **+1.8 e/tick**，双源房约 **+5.9 e/tick**。**本服实测印证：163 个人类 reservation 里 53 个（33%）是单源房**（tick 106,014）。
- **写下来的 break-even 公式全社区只有一条，而且是无量纲的**：TooAngel 的 `threshold = sources / distance * spawns >= 1`（带一段逐例推导的注释），线性距离与路线长度各算一遍。它同时就是 TooAngel 的距离上限 —— **单源房只能相邻，双源房能到两房外**。其余都是代理量：Overmind 的 `energyPerSource / (pathLength + 25)`（只用于扩张评分）、bonzAI 的 `if (cartsNeeded > sourceCount) 取消资格`、Winsley 的 `franchiseDistanceLimit = (Game.cpu.limit / offices) * 10`（**按 CPU 而不是按能量的贪心背包**）。**The International 把这条公式的意图写成了注释然后留空**：`// reserver cost is 650 / claimer lifetime - real path distance / source count` —— 正是本文 §3.1 在算的东西。判据真正一致的只有两条：**目标房必须与已有房相邻（或近乎相邻）**，且 **SK 房要么排除、要么另开一套 mission**。
- **remote 的三行体型，社区收敛得比本房还紧**：矿工 **5–7 WORK + 1 CARRY**，每 source 一只、以可站位数封顶 —— 唯一的分歧是 MOVE 比例（Overmind/bonzAI/TooAngel/Winsley 保持 1:2，**TI 与 Quorum 明确为 remote 加 MOVE**，TI 在最高档到 1:1）；hauler `[Carry;Carry;Move]` 重复，数量由**带宽账本**倒推 —— bonzAI `bandwidthNeeded = distance × load × 2.1`、Winsley `franchiseCapacity = 往返 tick × 采出速率`、Overmind `Σ energyPerTick × 2 × distance / CARRY_CAPACITY`、TI `findCarryPartsRequired(distance, income) = ceil(distance × 2 × income / 50)`，**四家独立实现同一个公式，也正是 fabot ADR 0012 的 hauler 配额公式**；reserver `[Claim;Move]` 重复，上限 4–5 段。
- **TI 是唯一把 remote 与本房的同一条公式并排写出来的**，因此也是唯一能读出"remote 该加多少余量"的样本：距离项 **remote `pathLen × 2` vs 本房 `pathLen + 3`**，收入项 remote 用**实测**的 `remoteSourceCreditChange`、本房用**估算**的 `estimatedSourceIncome × 1.1`。而且它**把 container 的衰减直接从 `maxSourceIncome` 里扣掉**（`CONTAINER_DECAY / (CONTAINER_DECAY_TIME * REPAIR_POWER)` = 0.5 e/tick，与本文 §1.3 的推导逐位相同）—— 社区里唯一一处把 remote 运营损耗写进配额的实现。
- **本服实测的体型分布与源码逐字吻合**（tick 106,014，163 个人类 reserve 房内全部 creep）：最常见的单一体型是 **110 只 `6work+1carry+3move`**；reserver 是 **55 只 `2claim+2move`** + 25 只 `1claim+1move` + 11 只 `3claim+3move`；hauler 全部 2:1，最常见的两个尺寸 **`24carry+12move`（23 只）与 `16carry+8move`（12 只）** —— 这**恰好就是 RCL5 的 1800 与 RCL4 的 1300 能量上限花在 `[Carry;Carry;Move]` 上的结果**；再大的 `32carry+16move` 则是撞 `MAX_CREEP_SIZE = 50` 的部件上限体。
- **无主房的 road 衰减与自有房**完全一样**（`roads/tick.js` 里根本没有 `roomController` 分支）；有 5 倍差距的是 container**：`CONTAINER_DECAY_TIME = 100` vs `CONTAINER_DECAY_TIME_OWNED = 500`，每次掉 `CONTAINER_DECAY = 5000` 点 —— 即无主房的 container 每 tick 掉 50 点，**光维持不塌就要 0.5 e/tick**（`REPAIR_COST = 0.01`）。路是**资本开销不是运营开销**：一条 47 格的路造价 14,100 e，维护只有约 0.1 e/tick。TI 把这两个数字原样写进了常量文件（`roadUpkeepCost = ROAD_DECAY_AMOUNT / REPAIR_POWER / ROAD_DECAY_TIME`、`remoteContainerUpkeepCost = CONTAINER_DECAY / REPAIR_POWER / CONTAINER_DECAY_TIME`），**独立复核了本文的推导**。
- **铺路与修路有三种活着的做法，最便宜的一种 fabot 已经具备条件**：Overmind/Winsley/bonzAI 把 remote 路并进本房路网、RCL3–4 起建；**TI 与 Quorum 干脆不给 remote 铺路**（TI 还因此把带路偏好的 hauler 排除在 remote 之外）；**TooAngel 让每只带 WORK 的 hauler 一边走一边铺、并且每一步都修脚下那格**（`repairRoadOnSpot`，无阈值）—— 由 §1.3 的算术，一条常走的路线因此永远不会积累衰减，代价只是体型表里的一个 WORK 前缀。**没有任何一家为 remote 设常驻 repairer。**
- **remote 房的入侵门槛是 100,000 而不是 1,000,000。** `backend-local/lib/game/api/game.js` 的 1,000,000 宽限期只在**放置 spawn** 时写给自己的房；别的房走 `INVADERS_ENERGY_GOAL = 100000`。一个 reserve 后的双源 remote 每 tick 20 e，**约 5,000 tick 就会触发第一次入侵**，之后每 70k–130k 一次。实测佐证：Kalgen 的 E18S24 两个 source 的 `invaderHarvested` 合计 **82,428 / ~100,000**（tick 105,995），而他在隔壁房备着 `eguardian`（`10move+5ranged_attack+5heal`）。房主不是自己的房，`createRaid` 一律派 **small** 体。
- **reserve 挡得住 stronghold 新生，挡不住它扩张。** `strongholds.js` 的 `spawnStronghold`/`selectRoom` 都跳过 `controller.reservation` 非空的房；但 `expandStronghold` 的落点判据只有 `if (controller && !controller.user && !hasCore)` —— **不看 reservation**。core 落进你 reserve 的房之后，它的 `handleController` 走 `attackController` 分支，每 tick 把你的 `reservation.endTime` 砍掉 `INVADER_CORE_CONTROLLER_POWER × CONTROLLER_RESERVE = 2`，即**你的 reservation 变成每 tick 净掉 3**，2 个 CLAIM 的 reserver 顶不住。
- **架构上社区的共识是"路径缓存，不是矩阵缓存"，而地形是免费的。** `Game.map.getRoomTerrain` / `new Room.Terrain(name)` 官方文档明写 *"works for any room in the world even if you have no access to it"* —— **多房投影的地形层不需要视野，也不需要失效策略**（fabot ADR 0031 的按房名 memo 直接就能用）。screeps-cartographer **完全没有 CostMatrix 缓存**，它缓存的是打包成 UTF-16 的成品路径（`reusePath: 100000`）；Winsley 在它上面自己加了一层 `memoizeByTick` 的矩阵缓存，并把"记住的远程路"从缓存路径注回矩阵（`territoryPlannedRoadsCost`）。Overmind/bonzAI 则相反：矩阵按房缓存（超时 10–10000 tick），路径不缓存。
- **成熟 bot 全部保留"单房语义"作为局部，只在两处升到多房**：①跨房的路径查询；②一份**按房名 keyed 的持久 intel**。**交通仲裁一律留在单房**：cartographer 的 `reconcileTraffic` 严格按 `creep.pos.roomName` 分房独立求解，边界格不做跨房争用检查。
- **而第①条有一条更便宜的实现，fabot 应当认真考虑**：主流是"先 `findRoute` 收窄房集合 → 跑**一次**跨多房 `PathFinder.search`"（Overmind、bonzAI、TI `maxOps: min(100000, (1+maxRooms)*2000)`、cartographer）；**但 TooAngel 每次搜索都 `maxRooms: 1`，把逐房路段缝在 `findRoute` 骨架上**，接缝取出口贴片的中位数格 —— 它至今仍在维护。这意味着 fabot 的 Atlas flood **可以保持单房**，跨房由"到出口 + 出口到目标"两段相加承担，几何语义一行不改。
- **"这个 remote 一直赔钱/一直死人所以撤"这条规则，社区没有一个活着的实现。** Overmind 采集 `CASUALTIES`/`safety1k` 却从不读；Winsley 的 `HarvestLedger` 完整记账但唯一的消费者是报表和一段死代码；TooAngel 只写日志与外交声誉；Quorum 连防守 creep 都不派。**唯一普遍存在的撤离触发是"房被别人 own 或 reserve 走了"**，其次是 TI 的"敌人还能活多久就放弃多久"（`abandonRemote = 最短敌人 TTL + 0~100`，并沿 `pathsThrough` 传染给借道的兄弟 remote）与 TooAngel 的"自家 storage 掉到 100,000 以下就收手"。
- **本服（Season #11，`shardSeason`，102×102，扫到 3,844 个 `normal` 房，61 名注册用户）实测**：56 名玩家占着房，其中 **44 名（79%）至少 reserve 了一个房；人类 reservation 共 163–164 个，而人类自有房只有 87 个 —— 平均每个自有房配约 1.9 个 remote**。**157/163 个 reservation 是自有房的直接邻房**，只有 6 个隔一格。RCL6+ 的 18 名玩家**无一例外**都 reserve；RCL5 是 20/27；RCL4 是 4/10。**fabot 是 RCL4 里 6 个不 reserve 的之一**（tick 105,945）。
- **另有 46 个无主、未 reserve 的邻房带着 container**（其中 32 个当下有 creep 站着），说明"**先不 reserve 直接采，之后再补 reserver**"是一个真实存在、普遍使用的中间态 —— 与 Overmind 的 `canSpawnReserversAtRCL = 3` 之前先派 scout、Winsley 的"reserver 跟着 harvester 走，从不领跑"完全一致。
- **fabot 的候选房当下完全无人问津。** tick 105,995–106,014 实测：W12S27、W13S28、W11S28、W12S29 四个邻房全部**无 owner、无 reservation、无任何建筑、无 invader core**，`invaderHarvested` 全部为 0（从来没人在这里挖过）。**离 W12S28 最近的人类是 6 房之外的 Odiodin（W14S22, RCL2）**；最近的 invader stronghold 是 4 房外的 W15S24（level 4，已经 reserve 了 W14S23/W15S23/W16S23 三个房）。
- **一个 #83 没写进去的赛季变量：W12S27 有一处 Thorium 矿（`mineralType: "T"`, density 3, 22,000）**，在 (24,16)。W12S28 自己也有一处（(26,5)，同为 density 3 / 22,000）。Thorium 是本赛季的计分资源，这让 W12S27 的价值不止一个 source。
- **对 ADR 0038 的直接影响，算得出来**：W13S28 两个 source 的往返（有路，空去满回）是 92 与 112 tick。**在 RCL4 的 800 容量下每个 container 要 2 只 hauler —— hauler 行离开地板，ADR 0038 的翻案条件成立**；但**在 RCL5 的 1200 容量下又回到 1 只**。即：**修好路的 RCL5 remote 并不翻案，没修路的中间态才翻**。而 link 不跨房（`links/transfer.js` 的 target 取自 `roomObjects`，同房限定），所以 remote 对 link 的影响只经由本房 hauler 行的地板这一条路径。

## 1. 引擎算术（全部对照源码核验）

### 1.1 reserve 把一个 source 从 5 e/tick 变成 10 e/tick

`engine/src/processor/intents/sources/tick.js`，原文：

```js
if (!roomController.user && !roomController.reservation && object.energyCapacity != C.SOURCE_ENERGY_NEUTRAL_CAPACITY) {
    bulk.update(object, {energyCapacity: C.SOURCE_ENERGY_NEUTRAL_CAPACITY,
                         energy: Math.min(object.energy, C.SOURCE_ENERGY_NEUTRAL_CAPACITY)});
}
if ((roomController.user || roomController.reservation) && object.energyCapacity != C.SOURCE_ENERGY_CAPACITY) {
    bulk.update(object, {energyCapacity: C.SOURCE_ENERGY_CAPACITY});
}
```

常量（`common/lib/constants.js`）：`SOURCE_ENERGY_NEUTRAL_CAPACITY: 1500`、`SOURCE_ENERGY_CAPACITY: 3000`、`SOURCE_ENERGY_KEEPER_CAPACITY: 4000`、`ENERGY_REGEN_TIME: 300`。regen 是**满额重置而非线性回血**：`if(gameTime >= object.nextRegenerationTime-1) bulk.update(object, {nextRegenerationTime: null, energy: object.energyCapacity})`，而 `nextRegenerationTime` 只在 `energy < energyCapacity` 时才起算 —— 即**计时从第一次被挖走一点开始**，不是从上次 regen 开始。所以"5 e/tick / 10 e/tick"是一个平均值，前提是矿工把它挖干净；一个只有 3 个 WORK 的矿工在 1500 上限下 250 tick 挖完 1500 然后空转 50 tick，仍是 5 e/tick，但在 3000 上限下就挖不完，实际拿不到 10。

**注意 `energy: Math.min(...)` 那一行**：reservation 一旦失效，source 里超过 1500 的存量被就地砍掉。lapse 不只是"以后少挣"，还立刻损失存量。

### 1.2 reservation 的收支：CLAIM 数决定的不是速度，而是能不能攒

`engine/src/processor/intents/creeps/reserveController.js`：

```js
var effect = _.filter(object.body, (i) => i.hits > 0 && i.type == C.CLAIM).length * C.CONTROLLER_RESERVE;
if(!target.reservation) { target.reservation = { user: object.user, endTime: gameTime+1 }; }
if(target.reservation.endTime + effect > gameTime + C.CONTROLLER_RESERVE_MAX) { return; }
target.reservation.endTime += effect;
```

`engine/src/processor/intents/controllers/tick.js` 第一段：

```js
if(object.reservation && (gameTime >= object.reservation.endTime-1 || object.user)) {
    bulk.update(object, {reservation: null});
}
```

常量：`CONTROLLER_RESERVE: 1`、`CONTROLLER_RESERVE_MAX: 5000`、`CREEP_CLAIM_LIFE_TIME: 600`、`BODYPART_COST[CLAIM] = 600`。

由此推出四条（推导，自洽于上述代码）：

1. **`endTime` 是绝对 tick，每过一 tick 自动少 1 tick 余量**。n 个 CLAIM 每 tick 加 n，所以**余量的净增速是 n − 1**。
2. **1 个 CLAIM 净增 0**：reservation 稳定在 `endTime − gameTime = 2`，只要 creep 有 2 tick 不 reserve（换班、被挤开、死亡）就整个作废。
3. **2 个 CLAIM 净增 1/tick**，600 tick 寿命减去路上时间 T，一生能攒到 `600 − T` 的余量 —— 足够覆盖换班空窗，也远不到 5000 的天花板。
4. **摊销成本**：`[Claim;Move]` 650/600 = **1.083 e/tick**；`[2Claim;2Move]` 1300/600 = **2.167 e/tick**；`[3Claim;3Move]` 1950/600 = 3.25；`[4Claim;4Move]` 2600/600 = 4.333。**寿命 600 而不是 1500 是这笔账里最贵的一项**（`body-design-task-matching.md` 的本房体型全部按 1500 摊销，remote 的 reserver 是唯一的例外）。

`CONTROLLER_RESERVE_MAX = 5000` 的意义是：一个 4 CLAIM 的 reserver 可以**巡回**几个房各攒一段余量再走，而不必常驻。本服实测的 Kalgen 正是这么做的（§8.3）。

### 1.3 无主房里 container 衰减 5 倍，road 一模一样

**road**（`engine/src/processor/intents/roads/tick.js`）—— 整个函数里**没有任何 `roomController` 分支**：每 `ROAD_DECAY_TIME = 1000` tick 掉 `ROAD_DECAY_AMOUNT = 100` 点（沼泽 ×5，墙 ×150，与建造成本的比例一致）。另有磨损，在 `movement.js`：

```js
road.nextDecayTime -= C.ROAD_WEAROUT * object.body.length;   // ROAD_WEAROUT: 1
```

即**每个 body part 每踩一步，把那格路的衰减时钟提前 1 tick**。`ROAD_HITS = 5000`，`CONSTRUCTION_COST.road = 300`，`REPAIR_POWER = 100`（每 WORK 每 tick）、`REPAIR_COST = 0.01`（每点 1/100 能量）。

推导出的运营成本（自洽于上述常量）：一条 D 格的路，被动衰减 `0.1 × D` 点/tick = `0.001 × D` e/tick；一只 P 部件的 hauler 跑一趟往返额外磨掉 `0.2 × D × P` 点 = `0.002 × D × P` 能量。**D = 50、P = 36 的 hauler 每 100 tick 跑一趟：被动 0.05 + 磨损 0.036 ≈ 0.09 e/tick。**路是资本开销（50 格 = 15,000 e），维护费可以忽略。

**container**（`engine/src/processor/intents/containers/tick.js`）则真的分房：

```js
object.nextDecayTime = gameTime + (roomController && roomController.level > 0 ? C.CONTAINER_DECAY_TIME_OWNED : C.CONTAINER_DECAY_TIME);
```

`CONTAINER_DECAY: 5000`、`CONTAINER_DECAY_TIME: 100`、`CONTAINER_DECAY_TIME_OWNED: 500`、`CONTAINER_HITS: 250000`。注意判据是 `roomController.level > 0` —— **reserve 不算，只有自己拥有才算**。所以：

- 自有房：每 500 tick 掉 5000 点 = 10 点/tick = **0.1 e/tick** 维护费；
- 无主/仅 reserve 的房：每 100 tick 掉 5000 点 = **50 点/tick = 0.5 e/tick**，无人修则 5,000 tick 塌掉。

对照"不放 container 直接 drop"：`engine/src/processor/intents/energy/tick.js` 每 tick 扣 `Math.ceil(amount / C.ENERGY_DECAY)`，`ENERGY_DECAY = 1000`。一堆在 0…1000 之间的能量每 tick 掉 1 点；hauler 每 94 tick 来一次、堆积速度 10 e/tick 的话，一个周期损失约 94/940 = **10%**，即 1 e/tick。**所以 remote 的 container 是划算的（0.5 < 1.0），但只勉强划算，而且 5,000 e 的造价要 10,000 tick 才靠省下的衰减回本** —— container 的真正价值是解耦矿工与 hauler 的时序，不是省这半点能量。

### 1.4 remote 房的入侵时刻表：goal 是 100,000，不是 1,000,000

`seasonal-threats-safemode.md` §1 已核验过 `genInvaders` 的整套门槛（能量阈值 → 同 sector 有 `invaderCore` → 出口邻房既非 owned 也非 reserved）。**本文补一条它没有区分、而 remote 必须区分的事**：那份研究引用的 1,000,000 宽限来自 `backend-local/lib/game/api/game.js` 的**放置 spawn** 处理，只写给自己新落生的房。任何别的房用默认值：

```js
const goal = room.invaderGoal || C.INVADERS_ENERGY_GOAL;   // INVADERS_ENERGY_GOAL: 100000
```

而 `invaderHarvested` 由 `creeps/harvest.js` 无条件累加（谁挖都算）。所以：

- 一个 reserve 后的**双源** remote 产 20 e/tick → **约 5,000 tick 触发第一次入侵**；
- 单源 reserve 后 10 e/tick → 约 10,000 tick；未 reserve 5 e/tick → 约 20,000 tick；
- 之后每次重置为 70k–130k（5% 概率 ×2，5% 概率 ×0），即**周期性、可预测**。

sector 门槛在 fabot 这里**是开着的**：`sectorRegex` 由房名生成（`"W12S27" → ^W1\dS2\d$`，即 W10–W19 × S20–S29），而该 sector 里有一个 level 4 的 invaderCore 站在 W15S24（§8.4 实测）。**W12S27 与 W13S28 的入侵不是"要不要考虑"，是"什么时候到"。**

`createRaid(controller && controller.user && controller.level, ...)` 的第一个参数在无主房上是 falsy，`createCreep` 里 `controllerLevel >= 4 ? 'big' : 'small'` 因此**永远取 small** —— remote 房的入侵永远是小体（1 只，10% 概率 2–5 只），与 `seasonal-threats-safemode.md` §1 描述的 RCL<4 情形同级。这是好消息：一支 `ranged_attack + heal` 的小队就能常驻处理。

Overmind 与 bonzAI 都独立实现了同一个**预测器**，用于提前造防守 creep 而不是撤退（Overmind `RoomIntel.isInvasionLikely`、bonzAI `InvaderGuru.trackEnergyTillInvader`），阈值几乎逐字相同：3 源 65,000 / 2 源 75,000 / 1 源 90,000（并要求 `lastSeen`/`tickLastSeen` 在 20,000 tick 以内）。**两家都把阈值取在 100,000 之下留出余量**，这是社区对同一条后端规则的两次独立复刻。

**一条反向的机会**：`checkExit` 拒绝那些邻房 `controller.user || controller.reservation` 的出口。fabot 若 reserve 了 W12S27，W12S28 的北向出口就不再是入侵入口。这是 reserve 的一项**防御**收益，不在能量账里。

### 1.5 invader core：reserve 挡得住新生，挡不住扩张

`backend-local/lib/strongholds.js`：

- `spawnStronghold`：`if(controller && (controller.user || (controller.reservation && controller.reservation.user != user))) { ... }` —— **已被 reserve 的房不会新生 stronghold**。
- `selectRoom`：`if(!!controller.user || !!controller.reservation) { continue; }` —— 同上。
- `expandStronghold`：落点判据是 **`if(controller && !controller.user && !hasCore) { found = {room: nextRoom, controller}; break; }`** —— **只看 `user`，不看 `reservation`**。扩张出的 core 以 `level: 0` 插入，跳过 highway 房与 novice/respawn 区。`INVADER_CORE_EXPAND_TIME = {1:4000, 2:3500, 3:3000, 4:2500, 5:2000}`，`expandStrongholds` cron 每 15 分钟跑一次。

core 落地之后（`engine/src/processor/intents/invader-core/stronghold/stronghold.js` 的 `handleController`）：

```js
} else if(!roomController.reservation || roomController.reservation.user === core.user) {
    intents.set(core._id, 'reserveController', {id: roomController._id});
} else {
    intents.set(core._id, 'attackController', {id: roomController._id});
}
```

`attackController.js` 对被 reserve 的 controller 执行 `endTime -= INVADER_CORE_CONTROLLER_POWER × CONTROLLER_RESERVE = 2`。**加上自然流逝的 1，你的 reservation 每 tick 净掉 3**；`[2Claim;2Move]` 每 tick 只加 2，扛不住。`level: 0` 的 core 不产兵（`create-creep.js` 开头 `if(!object.level || !C.INVADER_CORE_CREEP_SPAWN_TIME[object.level]) return;`），`INVADER_CORE_HITS = 100000`，所以处理方式是**去拆掉它**，而不是撤退。Winsley 为此专设了一个 mission（`KillCoreMission`，一只 GUARD，`MissionStatus.DONE` 于 guard 阵亡）。**Overmind 这个 2019 快照里 `STRUCTURE_INVADER_CORE` 一次也没出现** —— 该机制晚于该快照。

`genInvaders` 的房间选择集是 `db.rooms.find({status: 'normal', _id: {$nin: activeRooms}})`；`ACTIVE_ROOMS` 是 redis 集合，在公开的 backend 代码里**只有写入没有清除**，清除逻辑在闭源的主循环里，因此这一条筛选在正式服上的实际语义 **unverified**，本文不据此推论。

### 1.6 link 不跨房

`engine/src/processor/intents/links/transfer.js` 的目标来自 `roomObjects[intent.id]`（该房本 tick 的对象表），且 `game/structures.js` 的 `transferEnergy` 要求 `this.room.controller` 存在并通过 `checkStructureAgainstController`（link 需要 RCL5 的自有 controller）。**link 只能在同一个自有房内传输**，remote 房里既不能建 link，也不能把远处的 link 连过来。remote 对 ADR 0038 的影响只有一条路径：**它是否把本房 hauler 行抬离地板**（§9.3）。

### 1.7 没有视野也能拿到的东西

官方 API 文档（`screeps/docs`，`api/source/Map.md` 与 `api/source/Room.Terrain.md`）原文：

> `Game.map.getRoomTerrain(roomName)` — "Get a `Room.Terrain` object which provides fast access to static terrain data. **This method works for any room in the world even if you have no access to it.**"
> `Room.Terrain` constructor — "`Terrain` objects can be constructed for any room in the world even if you have no access to it."

`PathFinder.search` 的默认值（`api/source/PathFinder.md`）：`maxOps: 2000`（"1 op ~ 0.001 CPU"）、`maxRooms: 16`（最大 64）、`heuristicWeight: 1.2`，且 **"[`roomCallback`] will only be called once per room per search"**。`Game.map.findRoute(from, to, {routeCallback})` 是房级别的路由，返回 `{exit, room}` 列表。

**这两条合起来是 fabot 多房投影的关键前提**：地形层跨房是免费且永不失效的（ADR 0031 的按房名 memo 原样适用），需要视野的只有结构、creep 与 store。

## 2. 目标房选择

### 2.1 六个 bot 的判据

| bot | 何时新增一个 remote | 距离上限 | 排除规则 | 择优规则 |
|---|---|---|---|---|
| **Overmind** `Overseer.handleNewOutposts` / `computePossibleOutposts` | source 预算 `remoteSourcesByLevel = {1:1,…,7:7,8:9}` 未满（每 250 tick 复查） | `Colony.settings.maxSourceDistance = 100` 路径步 | 非 `ROOMTYPE_CONTROLLER`（SK/core/alley）排除；`RoomIntel.roomOwnedBy` / `roomReservedBy` 排除；**必须与本 colony 已有房相邻**（`isReachableFromColony`） | `minBy` 各 source 路径长度的**平均值** |
| **bonzAI** `SurveyAnalyzer` | 本房有 storage 之后周期性 survey；要求 `spawnGroup.averageAvailability` 有余、`empire.underCPULimit()` | `MAX_HARVEST_DISTANCE = 2` 房（线性），`MAX_HARVEST_PATH = 165` 步（到 storage） | alley 排除；SK 房与"路上要穿过他人占领房"标 `danger`（RCL<8 不碰）；**任一邻房有 owner 就整轮放弃** | `score = sourceCount * 1000 - averageDistance`（source 数绝对压倒距离） |
| **The International** `Room.prototype.scoutMyRemote`（`src/room/roomFunctions.ts`） | 侦察到一个有 controller 的无主房时当场判定 | `maxRemoteRoomDistance = 5` 房、`maxRemotePathDistance = 250` 步（注释原文 `// Past this it's probably not efficient`）；两条都对 source 路径与 controller 路径分别检查 | `remoteTypeWeights` 把 `sourceKeeper / enemy / enemyRemote / ally / allyRemote` 全部记 `Infinity`，即路线不许穿过；`manageSourceUse` 每 150–200 tick 复查，路线上任一房有 owner 即 `disable` | **抢占式**：`if (newCost >= currentCost) return`，只有当"两组 source 路径 + controller 路径"的总长度变短时，这个 remote 才换主人。**没有 source 数过滤** |
| **Quorum** `Room.prototype.selectNextMine` / `getMineScore`（`src/extends/room/territory.js`） | 全 empire 每 2000 tick 最多加一个（`getTimeSinceEvent('addmine') >= 2000`），且 CPU 宽裕（`cpuUsage.long <= 1.25`）；数量上限 `REMOTE_MINES` 按**实用房等级**给（PRL3:1、PRL4:2、PRL8:3，PRL1–2 无此键 ⇒ 完全不做 remote） | `MINE_MAX_DISTANCE = 2`；**第一个 mine 只能是相邻房**（`getRoomsInRange(this.name, existing.length <= 0 ? 1 : 2)`） | SK 房硬拒（注释 `// Mining program currently doesn't support SK rooms`）；`intel[INTEL_OWNER]`（同时覆盖 owner 与他人 reservation）拒；已属别人的 mine 拒；`findRoute(..., {avoidHostileRooms: true})` 无路即拒 | `score = (sources/3)×5 + walkability×0 + swampiness×(−1) + (distance/2)×(−3)`。注意 `MINE_WEIGHTS_WALKABILITY = 0` —— 这一项算了也白算 |
| **TooAngel** `spawnCreepsForReservation` / `getReserveRoomDistanceThreshold`（`src/prototype_room_external.js`） | 每次看见一个无主、无 reservation 的外房时；`data.lastChecked` 500 tick 节流 | **没有常数上限**，由公式隐含（见 §2.2） | `isRouteValidForReservedRoom`：**路线上每一个中间房都必须此刻可见，且要么是自己的、要么已是自己 `Reserved`**；`isReservedRoomValid`：**任何一个现有 remote 不健康就不许再开新的**；本房 `memory.spawnIdle` 必须 ≥ `reserveSpawnIdleThreshold = 0.2` | 先来先得（按 `findMyRoomsSortByDistance`） |
| **Winsley** `recalculateTerritories` | 每 50 tick 全局重算 | `TERRITORY_RADIUS = 3`（shard2 为 1），按 `getRoomPathDistance` | SK 房、他人 office、`threatLevel === OWNED`、`Memory.rooms[t].owner` 排除；威胁分超 `THREAT_TOLERANCE.remote[rcl]`（`{0:0,1:10,…,8:120}`）排除；`getClosestOfficeFromMemory(t) === office` 保证两个 office 不抢同一个房 | 按平均距离从近到远塞进 **CPU 预算**：`franchiseDistanceLimit = (Game.cpu.limit / offices) * 10`，`totalDistance = averageDistance × sourceCount` |
| **cartographer** | 不适用（它是移动库，不做经济决策） | — | — | — |

三条跨 bot 一致的事实：

1. **必须相邻，或近乎相邻。** Overmind 用 `isReachableFromColony` 把 `depth = 3` 的候选集实质压回一圈；bonzAI 只看线性距离 1；Quorum 的第一个 mine 只能相邻；TooAngel 的公式让单源房只在距离 1 时通过；只有 TI 名义上放到 5 房、250 步。**本服实测：163 个人类 reservation 里 157 个是自有房的直接邻房，只有 6 个隔一格**（tick 106,014）。
2. **SK 房单独处理或直接排除。** Overmind 的 `DirectiveSKOutpost` 要求 `colony.level >= 7` 且只能手动放旗；bonzAI 走另一个 operation（`keeper_`，`levelRequirement = 8`）；TI 记 `Infinity`；Quorum 硬拒并写下"还不支持"；TooAngel 的 SK 采矿默认关闭（`config.keepers: {enabled: false, minControllerLevel: 8}`）；Winsley `isSourceKeeperRoom` 排除并把 `sourceKeeperRoomCost = Infinity`。**#83 把 W14S26（4 个 lair）排除掉是与社区一致的判断。**
3. **敌对邻居是布尔排除项，不是扣分项。** 六家没有一家把"离敌人多近"折成能量；最细的是 Winsley 的威胁分阈值，而它也是一个阈值而不是一项成本。

### 2.2 break-even 公式：只有一条，而且是无量纲的

对七个仓库 `grep -i "breakeven\|break-even\|net energy\|profit\|payback\|roi"` 只在 Winsley 的 ledger 展示代码里有命中。**唯一一条写下来的、带推导注释的准入公式在 TooAngel**（`src/prototype_room_external.js` `getReserveRoomDistanceThreshold`，原文注释）：

```js
/**
 * Base reservability on number of spawns, distance and number of sources
 *
 * sources: 1, distance: 1, spawns: 1 = 1 fine
 * sources: 1, distance: 2, spawns: 1 = 0.5 not fine
 * sources: 2, distance: 2, spawns: 1 = 1 fine
 * sources: 1, distance: 2, spawns: 2 = 1 fine
 */
const spawns = myRoom.find(FIND_MY_SPAWNS).length;
const threshold = toReserve.data.sources / distance * spawns;
```

同一个式子还用**路线长度**再算一遍（`getReserveRouteDistanceThreshold`），两者都必须 ≥ 1。它不是能量方程 —— 分母是房间距离而不是 tick，分子里的 `spawns` 代表的是**孵化带宽**而不是收益 —— 但它抓住了 fabot 需要的那三个自变量（source 数、距离、自家产能），而且它就是 TooAngel 的距离上限本身：**单源房只能在相邻房，双源房能到两房外，多一个 spawn 就把射程再翻一倍**。

其余的都是**代理量**：

- **Overmind**（`strategy/ExpansionEvaluator.ts` `computeExpansionData`，且只用于**扩张评分**，不驱动 outpost 决定）：
  ```ts
  const offset = 25; // prevents over-sensitivity to very close sources
  roomScore += energyPerSource / (ret.path.length + offset);
  ```
  SK 房的 `energyPerSource` 打 0.6 折，注释是 `// don't favor SK rooms too heavily -- more CPU`。
- **bonzAI**（`SurveyAnalyzer.analyzeRoom`）—— "成本超过收益就取消资格"：
  ```ts
  let cartsNeeded = Mission.analyzeTransport(distance, Mission.loadFromSource(source), 12900).cartsNeeded;
  if (cartsNeeded > data.sourceCount){
      notifier.log(`SURVEY: disqualified ${room.name} due to distance to source: ${cartsNeeded}`);
  ```
  即"服务一个 source 所需的满级 hauler 数不得超过全房的 source 数"。
- **The International** —— **想写但没写完**。`src/room/room.ts` 的 `get remoteSourceIndexesByEfficacy` 里留着一段空的意图注释：
  ```ts
  if (this.room.energyCapacityAvailable >= BODYPART_COST[CLAIM] + BODYPART_COST[MOVE]) {
    let score = 0
    // associate score for source with accompaning costs
    // prefer based on rough energy / tick
    // reserver cost is 650 / claimer lifetime - real path distance / source count
    //
  }
  ```
  排序最终落回裸路径长度。**注意那行注释写的正是本文 §3.1 在算的东西**（`650 / CREEP_CLAIM_LIFE_TIME` 摊到 source 数上），而且它被留成了空实现 —— 这条公式在社区里是"人人都知道要算、没人写完"的状态。
- **Winsley** —— **按 CPU 而不是按能量做预算**（`getTerritoriesByOffice.ts`）：
  ```ts
  // limit distance by CPU
  let franchiseDistanceLimit = (Game.cpu.limit / Object.keys(Memory.offices).length) * 10; // magic number in lieu of detailed calculation
  ```
  注释自认是 magic number。Winsley 确实**算了**每个 franchise 的真实盈亏（`HarvestLedger`，累计 `spawn_harvest`、`spawn_logistics`、`deposit`、`repair`、container `decay`，每 1500 tick 结一次 `perTick` 并保留 10 期），但在这个最后的公开快照里，**读这份账的只有报表和一段死代码**（`Selectors/Franchises/franchiseDisabled.ts` 已无任何引用）—— 真正下线一个 remote 的是 `HarvestMission.disabled()` 和领地重算。
- **The International 的收益账**（`RemotesManager.initRun`/`run`）是唯一一个**逐 tick 记账**的：`remoteSourceCredit` 累计 container 与地上能量的实测存量，`remoteSourceCreditChange` 是它的变化率，并且**把 container 的衰减直接从 `maxSourceIncome` 里扣掉**（§5.1）。矿工与 hauler 的配额都读这个扣完的数。这是六家里最接近"净能量"的东西，但它是一本**测出来的**账，不是一条**算出来的**公式。

- **Overmind**（`strategy/ExpansionEvaluator.ts` `computeExpansionData`，且只用于**扩张评分**，不驱动 outpost 决定）：
  ```ts
  const offset = 25; // prevents over-sensitivity to very close sources
  roomScore += energyPerSource / (ret.path.length + offset);
  ```
  SK 房的 `energyPerSource` 打 0.6 折，注释是 `// don't favor SK rooms too heavily -- more CPU`。
- **bonzAI**（`SurveyAnalyzer.analyzeRoom`）—— 唯一一条形如"成本超过收益就取消"的判据：
  ```ts
  let cartsNeeded = Mission.analyzeTransport(distance, Mission.loadFromSource(source), 12900).cartsNeeded;
  if (cartsNeeded > data.sourceCount){
      notifier.log(`SURVEY: disqualified ${room.name} due to distance to source: ${cartsNeeded}`);
  ```
  即"服务一个 source 所需的满级 hauler 数不得超过全房的 source 数"。
- **Winsley** —— **按 CPU 而不是按能量做预算**（`getTerritoriesByOffice.ts`）：
  ```ts
  // limit distance by CPU
  let franchiseDistanceLimit = (Game.cpu.limit / Object.keys(Memory.offices).length) * 10; // magic number in lieu of detailed calculation
  ```
  注释自认是 magic number。Winsley 确实**算了**每个 franchise 的真实盈亏（`HarvestLedger`，累计 `spawn_harvest`、`spawn_logistics`、`deposit`、`repair`、container `decay`，每 1500 tick 结一次 `perTick` 并保留 10 期），但在这个最后的公开快照里，**读这份账的只有报表和一段死代码**（`Selectors/Franchises/franchiseDisabled.ts` 已无任何引用）—— 真正下线一个 remote 的是 `HarvestMission.disabled()` 和领地重算。

**结论：想要一个"能量/tick 净收益"公式，只能自己推。**§3.1 给出 fabot 口径的那一份。

### 2.3 一个 Winsley 独有、且很适合 fabot 的判据

`HarvestMission.disabled()` 里有一条与地形、距离、敌人都无关的：

```ts
if (storageEnergyAvailable(this.missionData.office) > STORAGE_CAPACITY * 0.75) return true; // plenty of energy in storage
```

**本房富得流油时主动停掉 remote。**remote 在他的模型里是一笔 CPU/能量的交易，而不是一份永久承诺。#83 最初的"花得掉"前置说的正是这件事，只不过 Winsley 把它做成了每 tick 重判的运行期条件，而不是上线前的一次性判断。

### 2.4 距离怎么量

除 TooAngel 外都不用 `getRoomLinearDistance` 做经济决策，而用**真实路径长度**：TI 直接量打包路径的长度（`packedPath.length / packedPosLength`，两套路径按有无 storage 切换：`remoteSourceHubPaths` / `remoteSourceFastFillerPaths`）、Quorum 缓存 `room.findPath(storage.pos, source.pos).length`（有截断缺陷，见 §4.2）、Overmind `Pathing.distance`（结果按 `[name1, name2].sort()` 归一化后 memo 进 `Memory.pathing.distances`，并以概率清理）、bonzAI `PathFinder.search(storage.pos, {pos: source.pos, range: 1})` 存进 `this.memory.distanceToStorage`（每次 `pavePath` 成功后覆写）、Winsley `getFranchiseDistance`（memo 200 tick）且**把已建成的路算作 1**：

```ts
cost += road.structureId ? 1 : terrainCostAt(road.pos);
```

—— 即 **franchise 会随着路修好而自动变"近"**，配额跟着往下走。这与 fabot 的 walk（ADR 0029，按 body 与地形定价、路面折价）是同一件事的两种写法。

## 3. reserve 还是不 reserve

### 3.1 引擎算术下的全账（fabot 口径）

按 §1 的常量与社区体型，把一个 remote source 的**运营**开销逐项列出（资本开销单列）。距离取 D 步（有路，空车去 D tick、满车回 D tick，`[Carry;Carry;Move]` 在路上满载正好平价）：

| 项 | 已 reserve（10 e/tick） | 未 reserve（5 e/tick） |
|---|---|---|
| 毛产出 | **+10.0** | **+5.0** |
| 矿工摊销 | `[6W,1C,3M]` 1150/1500 = −0.77 | `[3W,1C,2M]` 550/1500 = −0.37 |
| hauler 摊销（D = 47，往返 94 tick） | 需 `94 × 10 / 50 = 19` 个 Carry ⇒ RCL5 的 1 只 24C/12M（1800 e）→ −1.20 | 需 `94 × 5 / 50 = 10` 个 Carry ⇒ 1 只 10C/5M（750 e）→ −0.50 |
| container 衰减（§1.3） | −0.50 | −0.50 |
| 路面维护（47 格，§1.3） | −0.09 | −0.09 |
| reserver 摊销（`[2Claim;2Move]`，单源房独摊） | **−2.17** | 0 |
| **净** | **≈ +5.3** | **≈ +3.5** |

单源房 reserve 的净增量约 **+1.8 e/tick**；双源房里 reserver 由两个 source 分摊（各 −1.08）、路也共用，净增量约 **+2.9 e/tick/source ≈ +5.9 e/tick/房**。

三条限定：

- 上表用的是 `[2Claim;2Move]`。若用 `[1Claim;1Move]`（1.08 e/tick），单源房的净增量升到约 +2.9 —— 但 §1.2 已证 1 个 CLAIM 攒不出缓冲，**换班空窗一定掉 reservation，且掉的瞬间 source 存量被砍到 1500**。本服实测里 1C1M 与 2C2M 都有人用（25 只 vs 55 只，tick 106,014）。
- 路上时间 T 从 600 tick 的寿命里扣掉，D = 47 的话 `[2Claim;2Move]`（1:1 平地全速）走 47 tick，占 8%。上表未计入这 8%。
- **单源房 +1.8 e/tick 的净增量薄到不该单独去算**，它真正的意义在于 §1.4 的入侵门槛（reserve 后产量翻倍 ⇒ 入侵周期减半）和 §1.4 末尾的出口门（reserve 邻房 ⇒ 本房少一个入侵入口）这两项非能量收益。

### 3.2 bot 的策略：全都不做经济判断

| bot | reserve 条件 | 体型 | 数量 | 之前做什么 |
|---|---|---|---|---|
| **Overmind** `DirectiveOutpost.spawnMoarOverlords` / `ReservingOverlord` | `colony.level >= canSpawnReserversAtRCL = 3` 且房型是 `ROOMTYPE_CONTROLLER`；`needsReserving(reserveBuffer = 2000)` 或（无视野时）`roomReservationRemaining < 1000` | `CreepSetup(Roles.claim, {pattern: [CLAIM, MOVE], sizeLimit: 4})`，按 `energyCapacityAvailable` 缩放，最大 2600 e | 1 | RCL<3 派 `StationaryScoutOverlord`：一只 `[MOVE]` 常驻保视野，**顺手去踩掉敌方 construction site** |
| **bonzAI** `ReserveMission` | 只要 `flag.room.controller` 存在就无条件挂上；`ticksToEnd < 3000` 时补 | `configBody({claim: potency, move: potency})`，`potency = RCL8 ? 5 : 2` → **`[2Claim;2Move]`**，RCL8 为 5:5 | 1 | `ScoutMission`（一只 `[MOVE]`，`blindSpawn`）+ `BodyguardMission`；controller 被墙围住时另派 `dozer` 拆墙 |
| **Winsley** `ReserveMission` | 只 reserve **本 tick 正在产出**的房：`activeFranchises(office, 1)`，且 `reservation < 3000`；`energyCapacityAvailable < 650` 直接返回 0 | `buildFromSegment(energy, [CLAIM, MOVE], {maxSegments: 5})` | 每个"活着的" remote 房 1 只 | harvester 先上，**reserver 跟着 harvester 走，从不领跑** |
| **The International** `SpawnRequests.generalRemoteRoles` | `spawnEnergyCapacity >= 650`，且该 remote **至少有一只矿工活着**（`remoteSourceHarvesters.reduce(...) === 0` 即不派）；`remoteReservers` 在 `ticksToEnd >= max(controllerPathLen × 3, 500)` 时归零 | `extraParts: [MOVE, CLAIM]`，`partsMultiplier` 最大 5，`minCostPerCreep: 650` | `maxCreeps` = controller 可站位数 | **container 要等到真的 reserve 上了才建**（`remoteSourceHarvester.buildContainer`：`if (!this.room.controller.reservation \|\| ... !== Memory.me) return Result.noAction`） |
| **Quorum** `CityMine.reserveRoom` | `controller.reservation.ticksToEnd < 3500` 且不在挨打；数量 `min(RESERVER_COUNT, controller.pos.getSteppableAdjacent().length)`；`RESERVER_COUNT` 按实用房等级 PRL4:3 / PRL6:2 / PRL7:1 | `buildFromTemplate([MOVE, CLAIM], energy)`，`defaultEnergy = 3250`（= 5 对），被本房 `energyCapacityAvailable` 夹下来 —— **PRL4 的 1300 正好给出 2 CLAIM + 2 MOVE** | 见左 | **第一个 mine 是不 reserve 的**：`REMOTE_MINES` 在 PRL3 就是 1，而 `RESERVER_COUNT` 要到 PRL4 才非零 |
| **TooAngel** `Room.prototype.checkAndSpawnReserver` | **总是 reserve —— reservation 就是采矿决定本身**（`checkSourcer` 只从 `handleReservedRoom` 等三处调用，全都要求 `data.reservation`） | `layoutString: 'MK'`，尺寸由**赤字**决定：`maxClaimParts = Math.ceil((CONTROLLER_RESERVE_MAX - reservation.ticksToEnd) / CREEP_CLAIM_LIFE_TIME)`；本房 `≥1300` 给 2 CLAIM、`≥650` 给 1 CLAIM | 无活 reserver 时 1 只 | **`< 650` 时不派 reserver，但 sourcer 照常出** —— 明确的"先采后 reserve"档位 |

**没有任何一家按 source 数决定要不要 reserve**（TooAngel 的公式把 source 数用在"要不要开这个房"上，而不是"开了之后要不要 reserve"）。Overmind、TI 都把"reserve 了"当作前提硬编进产量估计：

```ts
} else if (this.colony.level >= DirectiveOutpost.settings.canSpawnReserversAtRCL) {
    this.energyPerTick = SOURCE_ENERGY_CAPACITY / ENERGY_REGEN_TIME;          // 10
} else {
    this.energyPerTick = SOURCE_ENERGY_NEUTRAL_CAPACITY / ENERGY_REGEN_TIME;  // 5
}
```

—— 即 `energyPerTick` 由 **colony 的 RCL** 推出，而不是由那个房**实际的** reservation 状态推出。TI 一字不差地犯同一个近似（`RemotesManager.initRun`）：

```ts
const possibleReservation = room.energyCapacityAvailable >= 650
...
if (possibleReservation) {
  // We can potentially double our income
  for (const i in remoteMemory[RoomMemoryKeys.remoteSources]) {
    remoteMemory[RoomMemoryKeys.maxSourceIncome][i] *= 2
  }
```

**产量翻倍的判据是"买得起 reserver"，不是"reserve 上了"。**这是一个值得 fabot 避开的近似：它在 reservation lapse 的窗口里会持续超配矿工与 hauler（§9.3 第 3 条）。TI 自己在 container 那一侧就没这么松 —— 建 container 前要检查真实的 `controller.reservation.username === Memory.me`。

### 3.3 本服实测（tick 105,945 / 106,014）

`/api/game/map-stats`（`statName: "owner0"`）扫过 W0–W35 × E0–E35 × N0–N35 × S0–S35 的全部 3,844 个非虚房；map-stats 把 reservation 报成 `own.level === 0`。

- 56 名玩家占着至少一个房；**44 名（79%）至少 reserve 了一个房**。
- 人类自有房 **87** 个，人类 reservation **164** 个（tick 105,945；tick 106,014 复测 163，差额是两个即将到期的 reservation）。**平均每个自有房约 1.9 个 remote。**
- **按最高 RCL 分组**：RCL6+ 的 18 名玩家 **18/18** 都 reserve；RCL5 是 **20/27**；RCL4 是 **4/10**；RCL3 及以下 0/1。
- **单源房照样 reserve**：163 个 reserve 房里 **53 个只有 1 个 source（33%）**，110 个有 2 个。
- **fabot 是 RCL4 那 6 个不 reserve 的玩家之一**（另五个：Kamots、rupper、Vorlock13、Bezoka、BenvolioDAT）。

## 4. 体型与配额 —— `body-design-task-matching.md` 的 remote 行

姊妹篇已经证明本房的三行体型（静态矿体 5–6W、hauler `[C,C,M]`、upgrader WORK-heavy）在五个 bot 里同构。remote 的三行如下，**每一行都与本房那一行不同**。

### 4.1 remote 矿工：WORK 数一致收敛在 5–7，唯一的分歧是 MOVE 的比例

| bot | remote 矿体 | 与本房的差别 |
|---|---|---|
| Overmind `Setups.drones.miners.standard` | `[W,W,W,W,W,W,C,M,M,M]`（6W1C3M，1150 e），`sizeLimit: 1` | **无差别**；本房与 remote 共用。`minersNeeded = min(ceil(miningPowerNeeded / 每体 power), pos.availableNeighbors(true).length)`，`miningPowerNeeded = ceil(energyPerTick / HARVEST_POWER) + 1` |
| bonzAI `MiningMission.getMinerBody` | `workerBody(6, 1, 3)`，由 `work = ceil((max(energyCapacity, 3000) / 300) / 2) + 1` 推出 | 有一条 `if (this.remoteSpawning) return this.workerBody(6, 1, 6)`（1:1），但 `remoteSpawning` 是**远程孵化的自有 colony** 标志，`MiningOperation` 从不传 true —— **这条分支在 remote 采矿路径上是死代码** |
| **The International** `SpawnRequests.remoteSourceRoles` | `spawnEnergyCapacity ≥ 950`：`defaultParts: [CARRY]` + `extraParts: [WORK,MOVE,WORK,MOVE]`，`maxCostPerCreep: 50 + 150*6` → **1C + 6W + 6M（950 e，1:1）**；≥650 档（注释 `// We can start reserving`）：`[WORK,WORK,MOVE]`，`maxCostPerCreep: 50 + 250*3` → **1C + 6W + 3M** | 本房 `sourceHarvester()` 是 6W+3M+2C。**remote 在最高档上把 MOVE 提到 1:1**；`partsMultiplier = maxSourceIncome[i] − remoteSourceHarvesters[i]`，即**按"这个 source 还缺多少产能"下单**，`maxCreeps` = 可站位数 |
| **Quorum** `src/roles/miner.js` | `options.remote` 时 base = `[M,C,W,W,W,W,W,W,M,M,M,W]` = **7W + 4M + 1C（950 e）**；本房 base = `[M,C,W×6,M,M]` = 6W + 3M + 1C（800 e） | **remote 明确多一个 WORK 和一个 MOVE**。数量恒为 1（快死时短暂变 2），挨打或紧缩时归 0 |
| **TooAngel** `roles.sourcer.settings` | 本房 capacity ≥700：`prefixString 'MWC'` + `applyAmount('MW',[2,4])` = **5W + 3M + 1C（700 e）** —— 正好 10 e/tick，对上 reserve 后的 3000/300 | **本房与 remote 同体**（两者都读**基地房**的 `energyCapacityAvailable`）。差别在行为：基地里 `transferToLink`，外房里 `maintainContainer` + `selfHeal`。数量：**每个 source 恰好一只** |
| Winsley `salesman.ts` | `remote` 时 `buildFromSegment(energy, [W,W,W,M], {maxSegments: 2, suffix: [CARRY]})` → 最大 **6W + 2M + 1C**（900 e）；本房是 `[W×5,M] × 2`（5–10W + 1–2M，无 CARRY） | **remote 体更小**，因为它无法把能量顺手倒进 extension；`maxHarvesters = adjacentWalkablePositions(pos, true).length`，`harvestRate >= 10` 即饱和停招 |

**收敛的部分**：WORK 数一律是 **5–7**（= `ceil(10 / HARVEST_POWER)` 加 0–2 个备份），CARRY 一律 **1**（修 container 用），数量一律 **每个 source 一只、以可站位数封顶**。

**分歧的部分只有 MOVE**：Overmind / bonzAI / TooAngel / Winsley 保持 1:2 甚至更低（矿工一生只走一次，MOVE 是一次性成本）；**TI 与 Quorum 明确为 remote 加 MOVE**（TI 到 1:1，Quorum 到 4M:7W）。`body-design-task-matching.md` §1.4 说的"远程矿工 MOVE 提到 1:1"是**对的结论、错的证据** —— 它引的 bonzAI 那条分支是死代码，真正这么做的是 TI 和 Quorum。

**本服实测（tick 106,014，163 个 reserve 房内的全部 creep）站在 1:2 这一边**：出现次数最多的单一体型是 **110 只 `6work + 1carry + 3move`**；其后是 19 只 `5work+1carry+3move`、11 只 `5work+1carry+5move`、9 只纯 `5work`、**9 只 `6work+6move`（1:1）**、6 只 `6work+3move`。**路修好之后 1:2 是主流。**

### 4.2 remote hauler：四家独立实现同一个带宽账本，两家不用它

- **bonzAI** `Mission.analyzeTransport(distance, load, maxSpawnEnergy)`（原文）：
  ```ts
  // cargo units are just 2 CARRY, 1 MOVE, which has a capacity of 100 and costs 150
  let maxUnitsPossible = Math.min(Math.floor(maxSpawnEnergy / ((BODYPART_COST[CARRY] * 2) + BODYPART_COST[MOVE])), 16);
  let bandwidthNeeded = distance * load * 2.1;
  let cargoUnitsNeeded = Math.ceil(bandwidthNeeded / (CARRY_CAPACITY * 2));
  let cartsNeeded = Math.ceil(cargoUnitsNeeded / maxUnitsPossible);
  ```
  `2.1` = 两条腿 + 5% 余量；`16` 是 3 部件一段撞 50 件上限。`load = Mission.loadFromSource(source) = max(source.energyCapacity, 3000) / 300`。
- **Overmind** `TransportOverlord.neededTransportPower` —— **全 colony 一个运输池**，remote 只是加数：
  ```ts
  const scaling = 2; // aggregate round-trip multiplier
  transportPower += o.energyPerTick * scaling * o.distance;
  ...
  const numTransporters = Math.ceil(neededTransportPower / setup.getBodyPotential(CARRY, this.colony));
  ```
  体型按路网覆盖率切换：`ROAD_COVERAGE_THRESHOLD = 0.75`，低于它用 `[CARRY, MOVE]`，高于它用 `[CARRY, CARRY, MOVE]`。一个 remote site 只有在**既有 container 又有活着的矿工**时才计入。
- **Winsley** `franchiseCapacity`：
  ```ts
  const time = (this.missionData.distance ?? 50) * 2;   // round trip
  return time * this.harvestRate();
  ```
  `harvestRate` 里 `noContainer` 时减 1（惩罚 container 缺失带来的衰减）。**同样是"往返 tick × 采出速率 = 需要的 Carry 容量"。**

- **The International** —— 唯一一个**把 remote 与本房的同一个公式并排写出来**的，因此也是唯一能直接读出"remote 该加多少余量"的样本。公式本体（`src/utils/utils.ts`）：
  ```ts
  /**
   * @param distance The number of tiles between the hauling target and source
   * @param income The number of resources added to the pile each tick
   */
  export function findCarryPartsRequired(distance: number, income: number) {
    return Math.ceil((distance * 2 * income) / CARRY_CAPACITY)
  }
  ```
  **remote 的调用**（`RemotesManager.run`）：
  ```ts
  const income = Math.min(
    Math.max(remoteMemory[RoomMemoryKeys.remoteSourceCreditChange][sourceIndex], 0),
    remoteMemory[RoomMemoryKeys.maxSourceIncome][sourceIndex],
  )
  remoteMemory[RoomMemoryKeys.haulers][sourceIndex] += findCarryPartsRequired(
    (remoteMemory[RoomMemoryKeys.remoteSourceFastFillerPaths][sourceIndex].length / packedPosLength) * 2,
    income,
  )
  ```
  **本房的调用**（`HaulerNeedOps.sourceNeed`）：
  ```ts
  room.communeManager.communeHaulerNeed += findCarryPartsRequired(
    packedSourcePaths[index].length / packedPosLength + 3,
    estimatedSourceIncome[index] * 1.1,
  )
  ```
  逐项对照：**距离项 remote 是 `pathLen × 2`、本房是 `pathLen + 3`**；**收入项 remote 是实测的 `remoteSourceCreditChange`（夹在 `maxSourceIncome` 以下）、本房是估算的 `estimatedSourceIncome × 1.1`**；本房那一支在"source 有可用 link + 有 hubLink"时整项跳过，remote 无此出口。因为 `findCarryPartsRequired` 内部已经乘了一次 2，remote 实际是 **`ceil(4 × pathLen × income / 50)`** —— **往返系数被乘了两遍**。这可能是笔误，也可能是有意的 remote 余量；无论哪种，**TI 给 remote 的运力是它给同长度本房路线的两倍**。

**四家（加上 fabot 的 ADR 0012）是同一个公式的五次独立实现。**差别在两处：Overmind、Winsley 与 TI 把 remote 与本房**汇总到一个池**再折成体（因此 remote 不会单独出现"至少 1 只"的地板 —— TI 甚至逐个 remote 从 `communeManager.haulerCarryParts` 这个活体运力池里扣减），bonzAI 与 fabot 是**每个 source 一项、各自向上取整**（因此有地板）。这正是 ADR 0038 论证里那个"地板"的来源，也是它可能被 remote 抬起来的机制。

**两个不用这个公式的反例，各自有理由**：

- **Quorum** 用能量而不是部件下单（`CityMine.mineSource`）：`carryAmount = ceil((distance × multiplier × 20) / carryCost) × carryCost`，`multiplier` **remote 1.8 / 本房 1.3**，超过 `100 × MAX_CREEP_SIZE/2 = 2500` 就劈成两只。体是严格 1:1 的 `buildFromTemplate([MOVE, CARRY], energy)`。**但它的 `distance` 是错的**：缓存来自 `this.room.findPath(storage.pos, source.pos, ...)`，而 `this.room` 是**自有房**，`Room.findPath` 在出口贴片就终止 —— 所以远程那一段根本没被量进去，1.8 这个系数有一部分是在补这个截断。无 storage 时直接退回字面量 `80`。
- **TooAngel 根本不算**：hauler 由矿工按堆量现叫（`Creep.prototype.spawnCarry`：`if (resourceAtPosition > parts.carryParts.carry * CARRY_CAPACITY) { ... checkRoleToSpawn('carry', 0, ...) }`），距离只作为**下单间隔**出现 —— `getCarrySpawnInterval() = this.memory.timeToTravel + config.room.spawnCarryIntervalOffset`（160），而 `timeToTravel = CREEP_LIFE_TIME - ticksToLive` 是 creep 首次抵达时实测的。多了会自己裁：`if (resourceAtPosition < 50 && nearCarries.length > 2) nearCarries[0].memory.recycle = true`。体型表 `config.carry.sizes` 按 **RCL 而不是距离**索引（RCL4 `[7,4]`、RCL5 `[9,5]`、RCL6 `[11,6]`）。remote 与本房的差别只有**返程阈值**：`carryPercentageExtern: 0.5` vs `carryPercentageBase: 0.1`（远程装到一半就回，本房装到一成就回）。远程 hauler 之间还会**接力**（`transferToCreep` / `checkForTransfer`），把有效行程截短 —— 这也是它不需要距离公式的一部分原因。

**一个 fabot 该抄的旋钮**：TI 的 hauler 体型下限 `minHaulerCost` 是一条 **CPU 反馈环**（`CollectiveManager.updateMinHaulerCost`）：
```ts
const targetCPUPercent = (Game.cpu.limit * 0.9) / Game.cpu.limit
Memory.minHaulerCostError = roundTo(targetCPUPercent - Memory.stats.cpu.usage / Game.cpu.limit, 4)
Memory.minHaulerCost -= Math.floor((Memory.minHaulerCost * Memory.minHaulerCostError) / 2)
```
每 1500 tick 更新一次，夹在 `[2×CARRY+MOVE, MOVE × MAX_CREEP_SIZE × 1.2]`。**CPU 紧就造更大更少的 hauler**，而且它先作用在 remote 上。对一台扁平 100 CPU 的赛季服，这是把"CPU 是稀缺资源"翻译成体型决策的最直接写法。

**本服实测的 hauler 体型分布（tick 106,014）**，全部 2:1：`24carry+12move` 23 只、`16carry+8move` 12 只、`20carry+10move` 10 只、`30carry+15move` 9 只、`17carry+9move` 9 只、`8carry+4move` 7 只、`6carry+3move` 14 只、`4carry+2move` 22 只、`2carry+1move` 12 只、`32carry+16move` 5 只。**这三个尺寸不是巧合**（`EXTENSION_ENERGY_CAPACITY` 与 `CONTROLLER_STRUCTURES.extension` 核验）：RCL4 的银行是 `300 + 20×50 = 1300`，买得起 8 段 `[Carry;Carry;Move]` = **16C+8M**（1200 e）；RCL5 是 `300 + 30×50 = 1800`，正好 12 段 = **24C+12M**（1800 e，不多不少）；而 **32C+16M 是 48 件，是 `[Carry;Carry;Move]` 撞上 `MAX_CREEP_SIZE = 50` 的天花板**（17 段就是 51 件），所以 Kalgen 这种 RCL7（银行 5300）的玩家用的是**部件上限体**而不是能量上限体。

**三个玩家的完整远程车队实测（tick 106,088）**，把自有房与全部 remote 房的 creep 一起点清：

| 玩家 | 自有房 | remote 房 | remote source | 可见 hauler | 每个 remote source 的 Carry 部件 | reserver |
|---|---|---|---|---|---|---|
| Kalgen（RCL7） | 1（E18S25） | 11 | 15 | 12 × `32C16M` | 384/15 = **25.6** | 5 × `4claim+4move` |
| Screepburner（RCL6） | 1（E27N15） | 4 | 6 | 3 × `29C15M` + 3 × `30C15M` | 177/6 = **29.5** | 2 × `3claim+3move` |
| Mirroar（RCL6+5） | 2 | 7 | 12 | 6×`12C6M` 3×`24C12M` 3×`28C14M` 2×`22C11M` 1×`21C11M` | 293/12 = **24.4**（含本房用量） | 2 × `3claim+3move` + 1 × `2claim+2move` |

**每个 remote source 约 24–30 个 CARRY 部件**，三家一致。反推：`Carry 数 = 往返 tick × 10 / 50`，25 个 Carry 对应约 **125 tick 的往返 = 单程约 62 格**。这与"remote 一律是邻房"的事实吻合，也与 #83 给 W13S28 估的"约 41 个 CARRY 服务两个源"（≈ 20.5/源）同量级 —— #83 的估计略低，因为它的 BFS 距离比这些玩家的实际路线短。

### 4.3 reserver

见 §3.2 的表。补一条**巡回**的实测证据：Kalgen 用 **5 只 `[4Claim;4Move]` 覆盖 11 个 remote 房**，且这些房的 reservation 余量（`endTime − tick`）实测为 842 / 1305 / 1306 / 1606 / 1609 / 1731 / 1839 / 1850 / 2274 / 2593 / 2627（tick 106,088）—— **全部远低于 5000 的天花板**。4 个 CLAIM 每 tick 净攒 3，一只 600 tick 寿命的 reserver 一生能攒约 1,800 tick 的余量，分给两个房各 900，正好落在观测区间里。**一房一只常驻 reserver 不是唯一解，也不是最省的解。**Mirroar 的余量则是 2049–4681（7 房 3 只，8 个 CLAIM），更接近上限。

### 4.4 spawn 相关的两条 remote 专属规则

- **prespawn 要算上路上时间。**bonzAI：`prespawn = Game.map.getRoomLinearDistance(source.roomName, storage.roomName) * 50 + 50`，且 `Mission.registerPrespawn` 在矿工首次抵达时记录 `CREEP_LIFE_TIME - ticksToLive` 做自适应校正。Winsley 的 `prespawnByArrived` 直接读 creep 首次抵达时写下的 `memory.arrived`：`creep.ticksToLive > creep.memory.arrived`。fabot 的 [[lead]]（ADR 0029/0032：cast walk + 每部件 3 tick 的孵化）已经是同一件事，只是它的 walk 现在算不出跨房距离。
  TooAngel 用同一个量做**下单间隔**而不是提前量：`getCarrySpawnInterval() = this.memory.timeToTravel + 160`，`timeToTravel = CREEP_LIFE_TIME - ticksToLive` 于首次抵达时记下；Quorum 则算成 `respawnAge = respawnTime + distance * 1.2` 喂给 `sizeCluster`。**四家都把"路上时间"实测出来而不是估算**，这是 remote 与本房最实质的一条运行期差别。
- **快死的 hauler 就地自杀或回收，不走回家。**bonzAI `MiningMission`：`if (cart.creep.ticksToLive < this.analysis.distance * 2) { cart.creep.suicide(); }`。Quorum：`if (hauler.ticksToLive < (distance + 30)) { return hauler.recycle() }`。Winsley 的分配账本同理：`if (distance * 2 > maxDistance) continue;`，`maxDistance = (creep.ticksToLive ?? CREEP_LIFE_TIME) * 0.8`。

## 5. 路

### 5.1 什么时候修、修到哪、谁修

| bot | 何时开始铺 | 铺什么 | 谁修 |
|---|---|---|---|
| **Overmind** | `RoadPlanner.settings.buildRoadsAtRCL = 4`。remote 的 `DirectiveHarvest` 在构造时把自己 `push` 进 `colony.destinations`，**remote 路就是本 colony 路网的一部分**，`buildRoadNetwork` 从 storage 出发一次规划 | 路网 `PathFinder.search` 的 `roomCallback` 里 `if (!this.colony.roomNames.includes(roomName)) return false` —— 只穿自己的房；`EXISTING_PATH_COST = PLAIN_COST - 1` 鼓励合流 | **没有专职 repairer**，通用 worker 干，由 `RoadLogistics` 调度：`allowedPaversPerRoom: 1`、`criticalThreshold: 0.25`、`repairThreshold: 0.9`，且**只维护规划里有的路**（`roomPlanner.roadShouldBeHere`，注释 `Roads returning false won't be maintained`）。铺路只在 `colony.defcon == DEFCON.safe` 时进行 |
| **bonzAI** | 无 RCL 门槛（整个 `MiningOperation` 要求 RCL≥4 的 spawn 房）；`Mission.pavePath` 每 1000 tick 醒一次，**一次只放一个工地**（`Game.cache.placedRoad` 全 empire 每 tick 一个） | `findPavedPath` 成本 `ROAD_COST 3 / PLAIN 4 / SWAMP 5 / AVOID 7`，绕开 controller 半径 3、source container 半径 1、keeper lair 半径 1，且 `if (Traveler.checkOccupied(roomName)) return false` | **专职 `paver`**，按损坏召唤：`if (!this.memory.roadRepairIds && (hitsToRepair > A_WHOLE_LOT /* 1000000 */ \|\| road.hits < road.hitsMax * .20))`，修完自动退役 |
| **Winsley** | 进入 office 建造队列的 RCL 门槛是 **3**（`if (rcl >= 3) energyStructures = energyStructures.concat(..., plannedActiveFranchiseRoads(officeName))`），排在能量类结构的队尾 | `planFranchisePath` 用 `cachePath(office + source, storage, {pos: harvestPos, range: 1}, {plainCost: 2, swampCost: 2, roadCost: 1, reusePath: 100000})` —— **`swampCost` 故意等于 `plainCost`**（反正要铺路），`territoryPlannedRoadsCost: 1` 让新 franchise 的路**并到已有干道上** | **hauler 自己修**：`buildAccountant` 的 `suffix = repair ? (roads ? [WORK,CARRY,MOVE] : [WORK,MOVE]) : []`，触发条件 `r.energyToRepair >= (ROAD_HITS / 2) * REPAIR_COST` 且 `Game.cpu.bucket === 10000`。remote 的 **container 由矿工自己修**（`hits < hitsMax - 500`） |
| **The International** | **根本不铺**。`ConstructionManager.preTickRun` 第一行就是 `this.room = this.communeManager.room`，它从不碰 remote；`RoomMemoryKeys.roadsQuota` 那段记账是注释掉的，`roads[i]` 永远是 0 | — | **只修 container，由矿工自己修**（`remoteSourceHarvester.maintainContainer`：`if (container.hits > container.hitsMax * 0.8) return Result.noAction`，即掉到 80% 才修） |
| **TooAngel** | **一路走一路铺，没有 RCL 门槛**。`roles.sourcer.buildRoad = roles.carry.buildRoad = true`，`Creep.prototype.moveByPathMy` 每 tick 调 `this.buildRoad()`，在脚下 `createConstructionSite(STRUCTURE_ROAD)` | 上限只有工地数：`maxConstructionSitesRoom: 3`、`maxConstructionSitesTotal: 80`；停工太久的工地按 `config.constructionSite.maxIdleTime: 5000` 回收 | **每一步都修脚下那格**：`repairRoadOnSpot` 里 `if (structureType === STRUCTURE_ROAD && hits < hitsMax) { this.repair(structure); }` —— **没有阈值**，所以常走的路线上衰减根本攒不起来。修路的 WORK 来自 hauler 体型表的前缀（`prefixString: {600: 'W'}`，capacity ≥600 的每只 hauler 带 1 个 WORK） |

**TooAngel 那条"能量不足就别铺"的闸门只作用在自有房**（`notBuildRoadWithLowEnergyButOnSwamp`：`if (this.room.isMy() && this.room.energyCapacityAvailable < 550 && ...)`）—— 在外房，任何带着能量的 buildRoad creep 都会无条件铺路。

四条共识与一条分歧：

- **(a) 铺路在 RCL3–4 之间开始**（Overmind RCL4、Winsley RCL3、Quorum 自有房 RCL4），**TooAngel 是唯一从第一天就铺的**，而 **TI 与 Quorum 是唯二完全不给 remote 铺路的**（Quorum 的 layout 只作用在 city，`CityConstruct`/`CityPublicWorks` 都绑 `Game.rooms[this.data.room]`）。TI 不铺路的后果是显性的：`HaulerOps.isRemoteValid` 要求 creep 的 `preferRoads` 与 remote 的 `roads[i]` 一致，所以**带路偏好的 hauler 根本不会被派进 remote**。
- **(b) 有铺路的三家（Overmind / Winsley / bonzAI）都把 remote 路当成本房路网的一部分**，不是第二张网。
- **(c) 没有人为 remote 专设常驻 repairer。**要么通用 worker（Overmind 的 `RoadLogistics`），要么按需召唤（bonzAI 的 paver，`road.hits < road.hitsMax * .20` 时上、修完退役），要么让 hauler 顺路带一个 WORK（Winsley、TooAngel）。
- **(d) remote 的 container 一律由矿工自己修**（TI 80% 阈值、Quorum 与 TooAngel 无阈值、Overmind `hits < hitsMax` 且 `WorkerOverlord.repairStructures` 明确拒绝修外房 container）。
- **分歧：修还是记账。**TI 选择**不修、只把衰减记进收益**（下节）。

**TI 把本文 §1.3 的两个数字原样写进了常量文件**（`src/constants/general.ts`）：

```ts
export const roadUpkeepCost = ROAD_DECAY_AMOUNT / REPAIR_POWER / ROAD_DECAY_TIME
export const remoteContainerUpkeepCost = CONTAINER_DECAY / REPAIR_POWER / CONTAINER_DECAY_TIME
```

即每格路 `100/100/1000 = 0.001` e/tick、每个远程 container `5000/100/100 = 0.5` e/tick —— 与 §1.3 的推导逐位相同。而且 TI 真的**把它从收入里扣掉**（`RemotesManager.initRun`）：

```ts
if (hasContainer) {
  // account for repair cost for container
  const creditChange = CONTAINER_DECAY / (CONTAINER_DECAY_TIME * REPAIR_POWER)
  remoteMemory[RoomMemoryKeys.remoteSourceCredit][i] -= creditChange * remoteSourcePathLength
  remoteMemory[RoomMemoryKeys.remoteSourceCreditChange][i] -= creditChange
  remoteMemory[RoomMemoryKeys.maxSourceIncome][i] -= creditChange
  continue
}
// We don't have a container, account for decay cost
const creditChange = Math.ceil(remoteMemory[RoomMemoryKeys.remoteSourceCredit][i] / ENERGY_DECAY)
```

两个分支正好是 §1.3 比较过的两种方案（container 的 0.5 e/tick 固定衰减 vs 地上堆的 `ceil(amount/1000)`），而且这个扣减直接喂给矿工的 `partsMultiplier` 与 hauler 的 `income`。**这是社区里唯一一处把 remote 的运营损耗写进配额的实现。**（引用时注意：`remoteMemory[pathType].length / packedPosLength` 这一行少了 `[i]`，取的是**路径数组的长度**而不是某条路径的长度，所以乘出来的数不是路长 —— 公式的形状对，实现有 bug。）

### 5.2 本服实测：路是修的，但不修满

tick 106,014，163 个人类 reserve 房：

- **136/163 有至少一格路**，27 个完全没路；**141/163 有至少一个 container**。
- 有路的房里，路格数中位数 **41**，最多 90，最少 1。
- **每个房的路平均血量占比（`hits/hitsMax`）从 35% 到 100% 分布，中位数约 80%**。抽样看极端：Mirroar 的 W15S11 有 73 格路，最低那格只剩 **80/5000（1.6%）**，而 Kalgen 的 E18S24 60 格路在 3800–4700（76%–94%）。
- **119 个 construction site** 散在这些房里 —— 路网仍在扩张。

**"路在 remote 房里长期跑在七八成血量"是常态，不是故障。**§1.3 的算术解释了为什么这样也没事：被动衰减 0.1 点/tick/格，一格路从满血到消失要 50,000 tick。

## 6. 防御与撤离

### 6.1 触发条件

| bot | 撤 / 停的条件 | 冷却或黑名单 |
|---|---|---|
| **Overmind** | 唯一会**真正删掉** outpost 的条件是房被别人**占领**：`if (RoomIntel.roomOwnedBy(this.pos.roomName)) { log.warning(...); this.remove(); }`（且 `roomOwnedBy` 自身 25,000 tick 过期）。有敌方玩家 creep 时放 `DirectiveOutpostDefense`；有 NPC 时放 `DirectiveGuard`。creep 层面是 `miner.flee(miner.room.fleeDefaults, {dropEnergy: true})` | 防守指令在**连续 100 tick 无敌人**后自撤。`Memory.rooms[name].AVOID` 由 `Pathing.updateRoomStatus` 顺手写（`owner && !my && towers.length > 0`），**只影响寻路与扩张评分，不影响 outpost 去留**。`RoomIntel` 里的 `CASUALTIES`（阵亡摊销的滚动均值）与 `safety1k / safety10k` **采集了但从来没有人读** |
| **bonzAI** | **没有任何撤离逻辑**：`grep "flag.remove()"` 在 `mining_`/`keeper_` 路径上零命中。只有 creep 级 `fleeHostiles`（半径 6，2 tick 延迟），以及经济性软停 —— storage 超过 `STORAGE_CAPACITY - 50000` 或掉到 RCL4 以下时 `getMaxCarts()` 返回 0（**停运输不停矿工**），`Game.cpu.bucket < 1000` 时 `cartActions` 直接返回 | `Memory.rooms[name].occupied`（`WorldMap.updateMemory`：`controller.owner && !controller.my`）是**永久黑名单**，只有重新看见该房不再被占领时才清 |
| **Winsley** | 四条并列：①房被他人 own 或 reserve（`HarvestMission.disabled()`）；②`franchiseIsThreatened`；③storage > 75%；④`distance > 250`。②是最完整的一套：`Memory.zones` 按**攻击者**聚类，`confirmed` 只在 `EVENT_ATTACK` 证明该玩家真的打了我方目标时置位（路过者不算），Source Keeper 与 Invader 直接排除，`rhythm = 3000` tick 无活动即过期；判据是 `franchiseDefenseRooms(office, franchise)`（**从缓存路径推出的整条走廊**）里的威胁分之和超过 `THREAT_TOLERANCE.remote[rcl(office)]`（`{0:0,1:10,2:10,3:20,4:30,5:40,6:60,7:80,8:120}`） | `FRANCHISE_RETRY_INTERVAL = 100000`（配合 `FRANCHISE_EVALUATE_PERIOD = 10` 的收益窗口重试），但见 §2.2：这段在最后的公开快照里未接线 |
| **The International** | **唯一一个有显式倒计时的**。`RemotesManager.initRun`（注释自称 `// Temporary measure while DynamicSquads are in progress`）：`const score = findLowestScore(enemyAttackers, creep => creep.ticksToLive); RoomNameUtils.abandonRemote(remoteName, randomIntRange(score, score + 100))` —— **放弃时长 ≈ 活得最短的那只敌人的剩余寿命 +0~100**。敌方 reservation + invader core 的组合则不放弃，而是把 `maxSourceIncome` 与 `remoteReservers` 一起清零 | `RoomMemoryKeys.abandonRemote` 是**逐 tick 递减的计数器**；`recurseAbandonment` 把同样的时长**沿 `pathsThrough` 传染给所有借道该房的兄弟 remote**。每个 remote 角色都在开头查它（`shouldKeepRemote` / `hasValidRemote`） |
| **Quorum** | **没有防御，也几乎没有撤离**。`CityMine.defend()` 只写遥测（`recordAggression` + `qlib.notify.send`，`TICKS_BETWEEN_ALERTS = 3000`）。软停：`this.underAttack = this.mine.find(FIND_HOSTILE_CREEPS).length > 0` —— **未按玩家过滤，所以 NPC invader 也触发** —— 它把矿工/hauler/reservist 的数量全部归零，但**不召回已有 creep：矿工就站在矿上等死**。永久移除只在"变成自己的城 / 不在名单里 / 配额缩水 / 无视野且 `findRoute` 无路"四种情况 | **无冷却、无黑名单** —— 唯一的再获取闸门是全 empire 每 2000 tick 一个新 mine 的节流。**完全没有 invader core 处理** |
| **TooAngel** | 放弃的判据是**自家的经济**而不是外房的危险（`handleUnreservedRoomWithReservation`）：本房 `memory.spawnIdle < 0.2`（孵化排队太满），或 `!room.isHealthy()`（= `isStruggling()` 或 storage 能量 < `isHealthyStorageThreshold = 100000`），就 `delete this.data.reservation`。外房被他人 reserve 走则转入 `HostileReserved` 状态（停掉所有 sourcer/carry），并按 `noReservedRoomInterval = 1600` 的冷却派 `attackunreserve` 去打回来 | 没有"放弃 N tick"字段。**真正的失效来源是别的**：`Room.prototype.data` 对外房**只存在于堆里**（accessor 只在 `isMy()` 时从 Memory 复水），而 `cleanRooms` 还会主动删掉非自有房的 `Memory.rooms` 条目 —— 所以**一次 global reset 就会清掉 `data.reservation`，remote 必须重新通过 `isRouteValidForReservedRoom`（要求路线上每个中间房此刻可见）才能复活**。这是 TooAngel 掉 remote 的主要原因，不是敌人 |

军事回应上分成三派：**Winsley 最细**（`DefendRemoteMission` 用 BLINKY，按 `totalCreepStats` 比分决定数量，触发条件含 `Memory.rooms[room].invaderCore`；`KillCoreMission` 一只 GUARD 专拆 core）；**TI 把 core 当工程量**（`remoteCoreAttacker = invaderCore.length * 8`，`extraParts = [ATTACK, MOVE]`，`maxCreeps: 3`；而 remoteDefender 的整段请求是**注释掉的** —— TI 当下不为 remote 派防守 creep）；**TooAngel 让 reserver 叫人**（`role_reserver.js` `callDefender`，`config.creep.reserverDefender: true`，`if (hostiles.length > 0 || invaderCores.length > 0)` 就往基地队列里塞一只 defender，每只 reserver 一生只叫一次）。**Quorum 一个防守 creep 都不派。**

**没有任何一家把"累计伤亡"接进撤离判断。**Overmind 采集了 `_RM.CASUALTIES` 与 `safety1k/safety10k` 却从不读；TooAngel 的 `handleUnexpectedDeadCreeps` 只写日志和外交声誉；Winsley 的收益账在最后的快照里没接线。**"这个 remote 一直在赔钱/一直在死人所以放弃"这条规则，社区里没有一个活着的实现。**

### 6.2 本服实测：护卫是真的，而且守在走廊上

tick 105,995，Kalgen 的 E17S25（1 source，已 reserve）站着两只 `eguardian`：`4move+2ranged_attack+2heal` 与 `10move+5ranged_attack+5heal`；同一网络的 E18S24 两个 source 的 `invaderHarvested` 合计 **82,428**，距 §1.4 的 100,000 门槛只剩不到两成。**这正是 §1.4 的时刻表在实机上的样子：入侵可预测，所以护卫是常备的而不是应急的。**

163 个 reserve 房里还测到 4 只 `1attack+5move+1ranged_attack+2tough+1work` —— 与 `seasonal-threats-safemode.md` §1 逐字核过的 `smallMelee` 体型一致，即 NPC invader 正在这些 remote 房里活动。另测到 1 个 invaderCore 落在人类 reserve 的房里，与 §1.5 推出的"扩张不看 reservation"一致。

## 7. 架构：多房状态与寻路

这是 #83 最贵的一节，因为 ADR 0004/0005 把"一个空间投影"写死了，而 ADR 0005 自己挂了账：*"Deferred, deliberately: honest pricing of targets outside the projected room."*

### 7.1 缓存矩阵还是缓存路径

| | Overmind / bonzAI | Quorum | The International / TooAngel | screeps-cartographer（Winsley 用它） |
|---|---|---|---|---|
| 缓存对象 | **CostMatrix 按房缓存**（堆） | **CostMatrix 序列化 + LZString 压缩后持久化** | TI：**只按 tick 缓存**；TooAngel：自有房持久、外房只在堆里 | **成品路径**，`RoomPosition[]` 用 `screeps-utf15` 打包成**每格 2 个 UTF-16 字符** |
| 位置 | 全局堆（Overmind `GlobalCache`，`CACHE_TIMEOUT = 50`、`SHORT_CACHE_TIMEOUT = 10`，地形矩阵 **10000**；bonzAI `traveler.structureMatrixCache` 按房名，永不过期） | `Memory.sos.cache`，键 `` `${serializeName(room)}_${flags}` ``（房名 + 选项位掩码） | TI：`room._defaultCostMatrix`，每 tick 由 `RoomOps` 清空，跨 tick 序列化那行是**注释掉的**（`/* this.global.defaultCostMatrix = cm.serialize() */`）。TooAngel：`global.data.rooms[name].costMatrix` | `HeapCache`（`moveTo` 默认）或 `MemoryCache`（`Memory['_cg']`，独立 `Memory['_cge']` 存过期时间） |
| 失效 | 定时（并带抖动） | **有视野时 TTL 25 tick；无视野时不设 TTL，但 `maxttl: 3000` 硬顶** | TI：每 tick。**TooAngel 的外房矩阵一旦建好就永不刷新**，只在 global reset 时丢 | 不适用（不缓存矩阵） |
| 路径缓存 | 无（Overmind 只把**房到房的距离**memo 进 `Memory.pathing.distances`） | route 缓存 150 tick（长度 ≥4 的还 `persist`） | TI：creep memory 里的打包路径 + 每 20 tick 复查 `disableCachedPaths`。TooAngel：`config.path.refresh = 2000000`，**等于永不过期** | **有，而且是主力**：`cachePath(key, ...)`，`reusePath` 缺省即"永久重用" |
| 跨 tick 的矩阵？ | 有 | 有 | TI **无**（序列化那行注释掉了）；TooAngel 外房有但从不刷新 | **完全没有**。`grep cachedCostMatrix / generateCostMatrix` 零命中；每次 `PathFinder.search` 的 `roomCallback` 都新建 `PathFinder.CostMatrix()` 并跑最多 4 次 `find()` |

Quorum 的缓存矩阵里还藏着一条对多房很实用的细节（`Room.getStructuresCostmatrix`）：四条边界各格记 15，注释是 `// Penalize exit squares to prevent accidental room changes`，可用 `opts.ignoreExits` 关掉。**这是"不想跨房时别误跨"的最便宜写法** —— fabot 现在的做法是把边界行整条从投影里裁掉（ADR 0036 的 loader 只取 1..48），多房之后这一裁就变成了"跨房走不了"，需要换成这种可选的惩罚。

cartographer 的选择是有意的：矩阵便宜且易变（creep 每 tick 都在动），路径贵且稳定。README 的原文把 remote mining 点名成典型用例：

> *"Instead of calling moveTo each time, you may find it more efficient to save a cached path and reuse it for multiple creeps. One common example would be pathing between storage and remote sources."*

**Winsley 把两条路线合起来**：他在 cartographer 之上自己加了一层 `memoizeByTick` 的 `getCostMatrix`，并且**把记住的远程路从缓存路径注回矩阵**：

```ts
if (opts?.territoryPlannedRoadsCost) {
  const office = Memory.rooms[roomName]?.office;
  if (office) {
    for (const s of activeFranchises(office).flatMap(({ source }) => getCachedPath(office + source) ?? [])) {
      if (s.roomName === roomName) costs.set(s.x, s.y, opts.territoryPlannedRoadsCost);
    }
  }
}
```

这解决了 cartographer 明确不解决的问题：**它对没有视野的房一律按裸地形定价** —— `Game.rooms[room]?.find(...)` 的可选链在无视野时短路，结果矩阵里既没有敌方结构，**也没有路**。

### 7.2 没有视野的房怎么表示

- **地形永远免费**（§1.7）。cartographer 从不调用 `Room.getTerrain()`，只用 `Game.map.getRoomTerrain`，而且真正给 PathFinder 用的矩阵里**根本不写地形**——PathFinder 自己用 `plainCost`/`swampCost` 和它自带的、与视野无关的地形数据。这就是"空矩阵对未见房是正确（只是乐观）的定价"的原因。
- **持久 intel 有三种落点，各有代价。****（i）`Memory.rooms[roomName]` + 短 key**：Overmind 用单字符（下详），**TI 用数字枚举 `RoomMemoryKeys`**（序列化后 1–3 个字符）加 3 字符一格的打包位置，并且按房型裁剪 —— `RoomTypes.remote` 只保留 commune、两套路径、三组 credit 数组、`abandonRemote`、`recursedAbandonment`、`pathsThrough`，其余在房型变化时被 `RoomNameUtils.cleanMemory` 剪掉。**（ii）RawMemory 段**：Quorum 把整份 intel 放进 `SEGMENT_INTEL = 'room_intel'`（`sos.lib.vram.markCritical`），键全是单字母（`INTEL_SOURCES = 's'`、`INTEL_OWNER = 'o'`、`INTEL_WALKABILITY = 'w'`、`INTEL_SWAMPINESS = 'a'`…），并且**把整房地形压成两个标量**：
  ```js
  roominfo[INTEL_WALKABILITY] = Math.round((walkable / 2500) * 1000) / 1000
  roominfo[INTEL_SWAMPINESS] = Math.round((swamps / walkable) * 1000) / 1000
  ```
  过期时间还加了抖动（`roominfo[INTEL_UPDATED] = Game.time - _.random(0, 10)`，注释 `// Add some random variance to this value to spread out intel expirations`），刷新周期 `room.controller.level ? 100 : 2000`。**（iii）纯堆**：TooAngel 的外房状态全在 `global.data.rooms`，`Memory` 只留自有房 —— 代价见 §6.1（global reset 即失忆）。
- **Overmind `RoomIntel` 用单字符 key**（`_RM.SOURCES = 's'`、`CONTROLLER = 'c'`、`AVOID = 'a'`、`INVASION_DATA = 'v'`…），位置存成 `"x:y"` 字符串，刷新按 `_MEM.EXPIRATION`：`RECACHE_TIME = 2500`（自有房 `OWNED_RECACHE_TIME = 1000`），且**过期不作废、而是按"上次看见的 tick"折算**：
  ```ts
  const timeSinceLastSeen = Game.time - (Memory.rooms[roomName][_MEM.TICK] || 0);
  return ticksToEnd - timeSinceLastSeen;    // roomReservationRemaining
  ```
  Winsley 的 `RoomMemory` 是明码字段（`scanned`、`sourceIds`、`rcl`、`owner`、`reserver`、`reservation`、`lastHostileSeen`、`invaderCore`、`threatLevel`、`safeModeEnds`…），另有一张**全局 `Memory.positions: Record<id, packedPos>`**（screepers/packrat，每格 2 个字符）——**这张表就是"没有视野也能对 remote source 做推理"的全部秘密**。扫描按最陈旧优先并有 CPU 预算：`Game.cpu.limit * 0.1`，自有房 `mandatory`。
- **反过来，需要视野的东西一律不缓存。**cartographer 只记两样东西：SK 房的 source/mineral 位置（`MemoryCache` 键 `'_ck' + room`，写一次永不过期）与 portal。**没有路的记忆。**

### 7.3 什么时候用 `findRoute`，什么时候直接多房 `PathFinder.search`

- **Overmind 与 bonzAI 用同一个阈值**：房间线性距离 **> 2** 才先跑 `Game.map.findRoute` 收窄房集合，再把结果当作 `roomCallback` 的硬门（`if (allowedRooms && !allowedRooms[roomName]) return false`）。Overmind 的 `routeCallback` 权重：超出 `restrictDistance = linearDistance + 10` 或 `shouldAvoid` 返回 `Infinity`，`preferHighway` 时 alley 记 1、其余记 2.5。bonzAI 的巧妙处是 **SK 房的成本挂在"有没有视野"上**：`if (!options.allowSK && !Game.rooms[roomName]) { if (isSK) return 10; }` —— 正在开采（因而有视野）的 SK 房按正常 2.5 计价，未知的按 10。
- **cartographer 不用 `Game.map.findRoute`**（全仓库唯一一处 `findRoute` 字样在 `memoize.ts` 的文档注释里）。它自己写了一个基于 `Game.map.describeExits` 的房图 A\*（含 portal），然后**用路由得到的房集合约束一次 `PathFinder.search`**：
  ```ts
  maxOps: Math.min(actualOpts.maxOps ?? 100000, (actualOpts.maxOpsPerRoom ?? 2000) * (rooms?.length ?? 1)),
  roomCallback: configureRoomCallback(actualOpts, rooms)
  ```
  `configureRoomCallback` 的第一行就是 `if (targetRooms && !targetRooms.includes(room)) return false;`。它还做**路由增强**（`enhanceRoute`）：拐角房、只有半边可通的边界、以及泛洪补房到 `maxRooms = 64` —— README 的理由是 *"the shortest route (by room count) is not always the best path (by tiles traversed)"*。
- **默认成本**（cartographer `config.DEFAULT_MOVE_OPTS`）：`defaultRoomCost: 2`、`highwayRoomCost: 1`、`sourceKeeperRoomCost: 2`、`maxRooms: 64`、`maxOps: 100000`、`maxOpsPerRoom: 2000`。**开箱即用是"高速路优先 2:1，SK 房完全不躲"**；敌对房回避明确留给使用者的 `routeCallback`，Winsley 就是这么补的（`config.DEFAULT_MOVE_OPTS.sourceKeeperRoomCost = Infinity`，`routeCallback` 对 `ThreatLevel.OWNED` 返回 `Infinity`，`maxOpsPerRoom` 提到 3000）。
- **The International** 是"先路由后一次搜索"的最完整样本（`src/international/customPathFinder.ts` `CustomPathFinder.findPath`）：先用 `Game.map.findRoute(..., {routeCallback: () => this.weightRoom(...)})` 得到 `allowedRoomNames`（并额外放行所有 `weightRoom` 不为 `Infinity` 的邻房），然后跑**一次** `PathFinder.search`：
  ```ts
  const maxRooms = args.maxRooms ? Math.min(allowedRoomNames.size, args.maxRooms) : allowedRoomNames.size
  const pathFinderResult = PathFinder.search(args.origin, args.goals, {
    plainCost: args.plainCost, swampCost: args.swampCost, maxRooms,
    maxOps: Math.min(100000, (1 + maxRooms) * 2000),
    heuristicWeight: 1, flee: args.flee,
    roomCallback(roomName) { if (!allowedRoomNames.has(roomName)) return false ... }
  ```
  注意 `maxOps` 是**按允许房数线性给的预算**（每房 2000，与官方默认一致），且 `heuristicWeight: 1`（比官方默认的 1.2 保守，换更优的路）。`weightRoom` 对**没见过的房返回 `Infinity`**（除非它就是目标），并对 `roomMemory[danger] >= Game.time` 的房同样返回 `Infinity`。另外 `weightRemoteStructurePlans` 会把**已存的 remote source 路径**每格记 1 —— 让 creep 主动贴着规划好的路线走。
- **TooAngel 是第三条路：完全不做多房 `PathFinder.search`。**全仓库每一次 `PathFinder.search` 都是 `maxRooms: 0` 或 `1`（`Room.prototype.buildPath` 写死 `maxRooms: 1`），跨房移动是把**逐房的单房路段**缝在 `Game.map.findRoute` 的骨架上（`Creep.prototype.getRoute`，失败时用 `useHighWay = true` 重试，高速房记 0.5、其余记 2）。房与房的接缝取**出口贴片的中位数格**：`const nextExit = nextExits[Math.floor(nextExits.length / 2)];`。它的地形成本也自成一格：`config.layout` 的 `plainCost: 5, swampCost: 8`（**1.6:1，而不是引擎默认的 5:1**）。**这条路线对 fabot 很重要**：它证明"每次搜索只看一个房"与"跨房移动"是可以并存的（§9.3）。
- **Quorum 走中间路线**：`Creep.prototype.travelTo` 用 `qlib.map.findRoute`（结果缓存 150 tick）建允许房名单，然后**把活交还给引擎自带的 `moveTo`**，只用 `costCallback` 拦房：
  ```js
  moveToOpts.costCallback = function (roomname) {
    if (moveToOpts.allowedRooms && moveToOpts.allowedRooms.indexOf(roomname) < 0) { return false }
    return Room.getCostmatrix(roomname, opts)
  }
  ```
  参数：`maxRooms` 同房 1、否则 16；`// Compute max operations based on number of rooms.` → `maxOps = Math.min(1500 * maxRooms, 5000)`；`reusePath: 1500`。房级权重表 `PATH_WEIGHT_HALLWAY/SOURCEKEEPER/OWN = 1`、`NEUTRAL = 2`、`HOSTILE_RESERVATION = 5`、`HOSTILE = 10`，`avoidHostileRooms` 把 `> NEUTRAL` 的一律变 `Infinity`。仓库里留着一条长期 TODO：`// Run built in moveTo function for now. Once enough features (costmatrixes, internal pathcaching, global pathcaching) have been developed this can be swapped out for moveByPath.`

### 7.4 交通仲裁**没有**升到多房

cartographer 的 `reconcileTraffic` 按 `intent.creep.pos.roomName` 分房建索引，`getMoveIntentRooms()` 逐房独立求解，`if (!Game.rooms[room]) continue;`。**A 房出口格上准备跨到 B 房的 creep 在 A 房仲裁，B 房里瞄着对应边界格的 creep 在 B 房仲裁，两者互相看不见** —— 同一 tick 可以同时被批准同一个跨界动作。推挤目标也不跨界：`calculateNearbyPositions` 丢掉 0–49 之外的偏移。

**这是本节对 fabot 最直接的一条**：ADR 0001/0008 的 arbitrated movement 是单房语义，而**社区最成熟的多房移动库也把它留在单房**，并接受边界格的争用不被检查。多房投影不必把仲裁一起多房化。

### 7.5 CPU 数字：几乎没人写下来

- 官方：`PathFinder.search` 的 `maxOps` 默认 2000，文档写明 *"1 op ~ 0.001 CPU"* —— 即一次默认搜索最多 2 CPU；`roomCallback` **每房每次搜索只调一次**。
- cartographer：**仓库里没有任何绝对 CPU 数字**。只有一个**相对**断言（`src/tests/testCases/TestCPU.ts`：两只 creep 跨 2 房，一只用 `moveTo`、一只用内置 `creep.moveTo(..., {reusePath: 1000})`，断言前者更省），以及 README 的告诫 *"Be careful to run potentially expensive operations after reconcileTraffic … your creeps will not move if you run out of CPU in a tick before reconcileTraffic gets a chance to run."*
- Overmind：`Movement.ts` 的 `REPORT_CPU_THRESHOLD = 1000`（**单只 creep 一生**累计移动 CPU 超过就报警）；`RoomIntel.run` 每 tick 只重算**一个**房的扩张评分。
- bonzAI：`REPORT_CPU_THRESHOLD = 2000`（同为单 creep 一生）；`RoomHelper.findClosest` 无条件打印自己的开销，两个调用点都用 `helper.randomInterval(10000)` 兜住。
- Winsley 是唯一**把 CPU 做成预算变量**的：每个 spawner 带 `estimatedCpuPerTick`（矿工 0.8、hauler 2、reserver 1.7），mission 带 `initialEstimatedCpuOverhead`，滚动 1000 tick 的 `_cpuLog` 落进 `Memory.missions[id]`，分配时 `if (remaining.cpu <= 0) break missions;`。而领地规模一开始就是按 `(Game.cpu.limit / offices) * 10` 定的 —— **CPU 是他的 remote 系统的一等公民，闭环的**。
- TI 用 CPU **反过来调体型**（§4.2 的 `minHaulerCost` 反馈环），并把 `roomData` 的堆缓存每 100 tick 整表丢弃（`if (Utils.isTickInterval(100)) { delete data.terrainBinary; delete roomData[roomName] }`），地形以 `Uint8Array(2500)` 存（注释 `A temporaly-discrete cached terrain binary`）。
- Quorum 直接打印 intel 扫描的开销（`Intel program scanned ${n} rooms using ${cpu} cpu`），并**用"优先级 4 进程的平均间隔"当作新增 remote 的准入**（`cpuUsage.long <= 1.25`）。
- TooAngel 在 CPU 不够时**整 tick 罢工**（`brain_main.js`：`if (Game.time > 1000 && Game.cpu.bucket < 1.5 * Game.cpu.tickLimit && ...) { 'Skipping tick CPU Bucket too low.'; return; }`），并用**控制器坐标做哈希**把周期性工作摊开（`Room.prototype.executeEveryTicks`：`(Game.time + this.controller.pos.x + this.controller.pos.y) % ticks === 0`）—— 一个零成本的错峰技巧，多房之后每房的周期任务都需要它。

### 7.6 单房投影里，哪些部分成熟 bot 保留、哪些放弃

**保留（全部 bot）**：房内的 CostMatrix 语义、房内的 seat/工位计数（`pos.availableNeighbors` / `adjacentWalkablePositions` / `harvestPositions` / `getSteppableAdjacent`）、房内的交通仲裁、按房名 key 的一切缓存。

**放弃 / 升到多房（全部 bot）**：①**跨房的路径查询**；②**一份按房名 key 的持久 intel**，其中位置用打包字符串存，且"陈旧"用"上次看见的 tick"折算而不是作废；③**配额账本**（hauler 需求、CPU 预算）跨房汇总。

**但第①条有两种做法，而且第二种更便宜。**主流是"先 `findRoute` 收窄房集合，再跑**一次**跨多房的 `PathFinder.search`"（Overmind、bonzAI、TI、cartographer）。**TooAngel 走的是另一条：每次 `PathFinder.search` 都 `maxRooms: 1`，跨房是把逐房路段缝在 `findRoute` 骨架上**，接缝取出口贴片的中位数格。它跑到今天（2026-08 仍在提交），说明"搜索永远只看一个房"不是 remote mining 的阻碍。**对 fabot 这是一条重要的退路**：Atlas 的 flood 可以保持单房，跨房由"到出口 → 换房 → 到目标"的路段链承担（§9.3）。

**没有任何 bot 把"一个房的完整投影"复制成 N 份。**它们的多房状态是**稀疏的**：source/controller/mineral 的位置、owner/reservation、威胁分、一条打包的路径，最多再加一个按房名的地形句柄，而不是 2304 格 × N 房的地形与结构表。Quorum 把整房地形压成两个标量（walkability / swampiness）是这个方向的极端。

## 8. 本服实测汇总（Season #11 / `shardSeason`）

所有数字通过官方只读 HTTP API 取得（`/api/game/map-stats`、`/api/game/room-objects`、`/api/game/time`、`/api/game/shards/info`、`/api/game/world-size`），只发 GET/查询请求，未做任何写操作。

### 8.1 赛季事实复核

`seasonal-threats-safemode.md` §2 的第一方事实**仍然成立**：`/api/auth/me` 实测 `cpu = 100`、`cpuShard = {"shardSeason": 100}`（扁平 100 CPU）；`/api/game/market/orders-index` 返回空列表（与"无市场"一致，但空列表不等于禁用，**unverified**）；GCL 进度 **517,525 / 1,000,000**（仍是 GCL1，#83 记录当时是 297,201）。世界 **102 × 102**；本次扫描的 W0–W35 / E0–E35 × N0–N35 / S0–S35 共 **3,844 个房全部是 `normal`**（W49S49 等已 `out of borders`，所以世界实际范围就在这个框内）。`/api/game/shards/info` 另报 `rooms: 2628`、`users: 61`、`lastTicks` 均值约 **2.4 s/tick**（`rooms` 的口径与上面的扫描不同，含义 **unverified**）。

### 8.2 全服 remote 普及度（tick 105,945；复测 106,014）

| 指标 | 值 |
|---|---|
| 占房玩家 | 56 |
| 至少 reserve 一个房的玩家 | **44（79%）** |
| 人类自有房 | 87 |
| 人类 reservation | **164**（复测 163） |
| 每个自有房平均 remote 数 | **≈ 1.9** |
| reservation 与自有房的房间距离 | **1 房：157；2 房：6** |
| reserve 房的 source 数分布 | **1 源 53（33%）；2 源 110** |
| 有 container 的 reserve 房 | 141 / 163 |
| 有路的 reserve 房 | 136 / 163（路格中位数 41，最多 90） |
| reserve 房里的 construction site | 119 |
| **无主、未 reserve 但带 container 的邻房** | **46**（其中 32 个当下有 creep 站着）；单源 15、双源 31 |
| Invader 持有的房 | 36 个 level>0 的 core（stronghold）+ 若干 level 0 的 reservation |

**按玩家最高 RCL 分组的 reserve 率**（取自第二次扫描，tick ≈ 106,000）：RCL6+ **18/18**；RCL5 **20/27**；RCL4 **4/10**；RCL3 及以下 0/1。两次扫描相隔约 70 tick，其间有两名玩家（slacker、ender2012）的 reservation 到期消失 —— **reservation 会掉，单点快照要当快照读**。

### 8.3 三个强邻的远程网络（tick 105,995 / 106,088）

见 §4.2 的表。补两个具体房：

- **Kalgen（RCL7，E18S25）的 E18S24**：controller 被 reserve（`endTime = 108,681`，余量 2,686），2 个 source（`energyCapacity` 均为 **3000**，`energy` 2,844 / 2,604，`invaderHarvested` 40,956 / 41,472），**2 个 container**（血量 249,250 / 239,550，存量 48 / 1,472），**60 格路**（血量 3,800–4,700 / 5,000），站着 2 只 `6work+1carry+3move` 矿工与 1 只 `32carry+16move` hauler（名字 `shauler-L1600-E17S23h1-104832` —— **名字里带着能量档位与它服务的那个 source 的 id**）。
- **Mirroar（RCL6，W16S11）的 W15S11**：reserve 余量 4,511（接近 5000 上限），2 个 source（3000 cap），2 个 container，**73 格路，最低一格只剩 80/5000**；站着 1 只 `6work+3move`（**无 CARRY**，靠脚下 container 接漏）与 1 只 `2claim+2move` reserver。隔壁 W16S12（单源，同样 reserve）站着 **3 只 hauler**（`22C11M` ×2、`24C12M`）与 1 只 `6work+3move`。

### 8.4 fabot 的候选集：无人问津（tick 105,995–106,014）

`/api/game/room-objects` 逐房实测：

| 房 | owner | reservation | 建筑 | source | `invaderHarvested` | 其他 |
|---|---|---|---|---|---|---|
| **W12S27**（北） | 无 | 无 | **无** | 1 @ (16,45)，cap 1500，满 | **0** | **Thorium 矿 @ (24,16)，density 3，22,000**；U 矿 @ (30,38) density 2, 35,000 |
| **W13S28**（西） | 无 | 无 | **无** | 2 @ (18,4)、(16,7)，cap 1500，满 | **0** | 2 个 mineral |
| W11S28（东） | 无 | 无 | 无 | 1 @ (33,15) | 0 | |
| W12S29（南） | 无 | 无 | 无 | 1 @ (40,43) | 0 | |

四个房都**没有 invader core**、没有任何玩家结构、`invaderHarvested` 全零（**从来没有人在这里挖过**）。W12S27 的 controller 在 (37,43)，W13S28 的在 (24,17)。

**邻里距离（房间 Chebyshev）**：最近的人类是 **6 房外的 Odiodin（W14S22, RCL2）**；再往外 9 房才有 Kamots（W14S19, RCL4）、Odiodin 的 W5S19 与 FR4C74LH3X 的一串。

**最近的 invader stronghold 是 4 房外的 W15S24**（tick 106,529 实测）：一个 source keeper 房（4 个 lair、3 个 source、**无 controller**），里面站着 `level: 4` 的 invaderCore（hits 100,000/100,000）、4 座 tower、25 面 rampart、8 只守军，`nextExpandTime = 107,860`（**约 1,330 tick 后下一次扩张**），并带着 `EFFECT_COLLAPSE_TIMER`（1002）`endTime = 170,283` —— **这座 stronghold 在 tick 170,283 塌掉**（`STRONGHOLD_DECAY_TICKS = 75000`），按当前约 2.4 s/tick 是**约 42 小时之后**。

它的扩张前沿已经吃到 **W14S23 / W15S23 / W16S23**：实测 W14S23 里站着一个 `level: 0` 的 invaderCore（(42,23)，hits 69,130），把 controller reserve 给了 user `"2"`（`endTime = 111,527`）。**W14S23 距 W12S27 只有两房。**扩张落点是从 core 房出发的 BFS 里第一个"无 owner 且无 core"的房，出口顺序是 `_.shuffle` 的（§1.5），所以方向不可预测但可以监视 —— 每 2,500 tick 一次。

按 §1.4 的 sector 门槛：只要 W15S24 这座 core 还活着，**W12S28 及其所有邻房的入侵开关就是开着的**；它一塌，整个 W1xS2x sector 的入侵就停止，直到下一座 stronghold 生成。

fabot 自己（W12S28，tick 106,014）：RCL4，两个 source 的 `invaderHarvested` 各 **9,636**（合计 19,272 / 1,000,000 的新房宽限），`safeModeAvailable = 3`，`ticksToDowngrade` 余 146,038 —— 本房离第一次入侵仍然很远，这与 `seasonal-threats-safemode.md` §5 的结论一致。

## 9. 对 fabot 与 #83 的结论

### 9.1 #83 的哪些问题被本文回答了

1. **"目标房怎么选"** —— 判据基本都是启发式，唯一写下来的公式是 TooAngel 的 `sources / distance * spawns >= 1`（§2.2），而**它对 #83 的两个候选给出的答案是"都通过，而且刚好"**：W12S27 = `1/1×1 = 1`（恰好在线上），W13S28 = `2/1×1 = 2`（宽裕）。同一条式子还顺带回答了"能不能再往外一层"——**不能**：任何距离 2 的房，fabot 以一个 spawn 只有在它有 2 个 source 时才够格，而 #83 底稿里距离 2 的房（W13S29、W11S29）没有一个是"2 源且路便宜"的组合。**必须相邻**这一条又被本服 157/163 的实测坐实（§8.2）。#83 的推进顺序与社区做法完全一致：两者都是直接邻房，两者都不是 SK 房。
2. **"要不要 reserve"** —— **要，而且单源房也要**（§3.1 的账 + §8.2 的 53/163 实测）。但不是第一步：社区的一致做法是**先不 reserve 直接采，等造得起 reserver 再补**（Overmind 的 RCL3 门槛、Winsley 的"reserver 跟着 harvester"），而本服有 46 个活着的无 reserve remote 佐证（§8.2）。fabot 在 RCL4 造得起 `[2Claim;2Move]`（1300 = RCL4 的银行上限），所以这一步不必等。
3. **"体型与配额"** —— remote 三行全部有定论（§4）：矿工 `6W+1C+3M`、hauler `[C,C,M]` 按带宽账本、reserver `[2Claim;2Move]`（或 4 CLAIM 巡回）。**fabot 的 ADR 0012 hauler 配额公式与 bonzAI / Overmind / Winsley / TI 是同一个公式**，直接外推即可，不需要新概念 —— 唯一要决定的是 remote 那一项要不要像 TI 那样加余量（它给 remote 的距离项是本房的两倍，§4.2）。
4. **"路"** —— 三种活法都可行：并进本房路网（RCL3–4 起，Overmind/Winsley/bonzAI）、干脆不铺（TI/Quorum）、或让 hauler 一边走一边铺一边修（TooAngel）。维护费可忽略（§1.3，TI 的常量文件独立复核过），造价是真开销；跑在 70%–80% 血量是常态（§5.2）。
5. **"防御与撤离"** —— 撤离的标准触发只有一条是全社区共识：**房被别人 own/reserve 走了**（§6.1）。**"赔钱就撤"没有一个活着的实现。**invader 不是撤离理由而是**排班理由**，因为它的时刻表可以从自己的采出量算出来（§1.4），社区两次独立复刻了同一个预测器。
6. **"多房状态与寻路"** —— §7。核心结论：**地形跨房免费**，需要视野的只有结构；持久 intel 是稀疏的按房名表；交通仲裁**留在单房**。

### 9.2 #83 正文里需要修正的三处（外加一条补充）

1. **"reserver 摊销（W12S27）~2.2 e/tick"是对的，但 #83 把它当成唯一选项。**`[1Claim;1Move]` 只要 1.08 e/tick（§1.2），代价是攒不出缓冲；本服 25 只 creep 在用它。而**一只 `[4Claim;4Move]` 巡回两个房**（Kalgen 的做法，§4.3）单房摊销约 2.17 e/tick —— 与一房一只 2C2M 同价，却能同时覆盖 W12S27 与 W13S28。**"一只养一个源"不是唯一的记账方式。**TooAngel 还给了第三种：**按赤字定尺寸** —— `maxClaimParts = ceil((CONTROLLER_RESERVE_MAX - reservation.ticksToEnd) / CREEP_CLAIM_LIFE_TIME)`，reservation 掉得越低下一只 reserver 越大，稳态时自动缩到最小。这条规则天然适配 fabot 的"每 tick 重算配额、不留持久状态"。
2. **"体型预算按 RCL5 的 1800 重算"** —— issue 的第二条评论已经这么修正过，本文补上确切数字：1800 花在 `[Carry;Carry;Move]` 上是 **24C+12M**，恰好是本服最常见的 remote hauler 体型（23 只，§4.2）。而 #83 估的"41 个 CARRY 服务 W13S28 两个源"偏低约 20%：实测强邻是每个 remote source 24–30 个 CARRY（§4.2）。
3. **#83 没有提到 W12S27 有一处 Thorium 矿**（density 3，22,000，@ (24,16)，§8.4）。本赛季 Thorium 是唯一的计分资源。这不改变 remote 的能量账，但**改变 W12S27 与 W13S28 的排序理由**：W12S27 除了"低风险试水"之外，还是一条通向赛季分数的路。
4. （补充，不是修正）**#83 的"CPU 会按房间数翻倍"是保守的**。Winsley 把 CPU 做成显式预算变量并用它**决定 remote 规模**（`(Game.cpu.limit / offices) * 10`，§7.5），Overmind/bonzAI 则只在超限时降级。fabot 的 100 CPU 是硬上限且不随 GCL 增长，所以 Winsley 的做法是唯一与本服约束匹配的那个。

### 9.3 架构决定：多房投影 vs 给 remote 单开一层抽象

**建议：把投影按房名分层，不要另起一层 remote 抽象。**理由是这个改动的成本比它看起来小，而另一条路的成本比它看起来大。

**成本侧，具体到 fabot 的类型**（`src/Core/Types.fs`）：`SpatialInfo` 有一个 `RoomName: string option`，四个**按 `Pos` key 或以 `Pos` 为元素**的容器（`Terrain: Map<Pos, Terrain>`、`Obstacles: Set<Pos>`、`Roads: Set<Pos>`，以及值为 `Pos` 的 `TargetPositions` / `CreepPositions`），和三个**按对象 id key**、本身与房无关的容器（`TargetKinds`、`Hits`、`Stores`）。`Pos = { X: int; Y: int }` 没有房维度。多房化只有两种写法：给 `Pos` 加一个 `Room` 字段，或者在前四个容器外面套一层 `Map<roomName, _>`（按 id key 的三个原样不动，因为 id 本来就是全服唯一的）。**后者更便宜**，因为：

- **地形层根本不用改语义。**ADR 0031 已经把地形 memo 成"按房名 key 的表"，而 §1.7 证明 `Game.map.getRoomTerrain` 对任何房都可用且永不失效 —— **多房地形的增量成本是零次 API 调用之外的零**，ADR 0031 的 Consequences 已经写下了这一句（*"A multi-room projection (#83) can key the same table by room name without contention"*）。
- **需要视野的层是稀疏的。**社区没有任何一个 bot 为 remote 维护完整房投影（§7.6）：它们只留 source/controller 位置、owner/reservation、威胁分和一条打包路径。fabot 的 remote 房需要的东西也就这些：source 位置（fixture 已有）、container/road 的 `Hits`、reservation 状态、以及站在里面的自己人。**结构与 store 那几层在 remote 房里几乎是空的。**
- **Atlas 的总体性契约（ADR 0004）恰好是为这件事准备的**：投影放不下的东西"不可定价，永不计入 Task，永不阻挡动作"。一个只在偶尔可见的 remote 房，就是**逐条缺失**的典型 —— 这正是 ADR 0004 已经付过钱的语义，不需要第二套。

**收益侧，另起一层的代价**：ADR 0005 拒绝"两个投影"的三条理由（接口几乎复述实现、每 tick 扫两遍同一个房、两个视图会对同一个房产生分歧）在 remote 上全部成立，而且更糟：remote 的 Task 与本房的 Task 要进**同一个 Task 池**（Harvest、Withdraw、Refill、Repair 都是），Matcher 要对跨房与房内的 Task 排同一个序。两层抽象意味着 Matcher 要问两个几何来源，这正是 ADR 0004 花了整个决定去消灭的东西。

**但要划三条界**，否则这个改动会溢出：

1. **arbitrated movement 留在单房。**ADR 0001/0008 的仲裁与 [[occupancy surcharge]] 是本房语义；cartographer 这个最成熟的多房移动库也把 `reconcileTraffic` 严格按房分解（§7.4）。跨房那一步交给 walk / travel cost 的路径，不交给仲裁。
2. **[[walk]] 与 [[travel cost]] 的 flood 要有房级别的门。**ADR 0029 的 flood memo 键已经是 `(tile, fatigue factor, pricing)`；多房化时它必须再加一个**允许房集合**，否则一次 flood 会漫过整张地图。社区的做法一致：先跑房级路由（`Game.map.findRoute` 或自写房图 A\*），把结果当作 `roomCallback` 的硬门（§7.3）。fabot 的 remote 是**直接邻房**，所以路由这一步可以退化成一个写死的两房集合 —— **第一版不需要房图 A\***。
   **如果连这一步都嫌贵，还有 TooAngel 那条退路**：flood 永远只在一个房里跑，跨房的 walk = "到出口贴片的 walk" + "对面房从该贴片到目标的 walk"，两段相加（§7.6）。它牺牲的是最优性（接缝定死在某个出口格），换来的是 Atlas 的几何语义**一行都不用改**。对只有一个邻房、且出口宽达 36 格（W12S27）或 19 格（W13S28）的 fabot，这条退路的误差很小，值得作为 ADR 的一个 Considered Option 认真写下来而不是略过。
3. **`sourceOutputPerTick = 10` 必须从常量变成 per-source 的事实。**`src/Core/Decide.fs` 现在把它写成 `let private sourceOutputPerTick = 10`，`anchorWorkCap = 10/2 + 1 = 6` 与 hauler 配额都读它。一个**未 reserve** 的 remote source 只产 5 e/tick，用 10 去配矿工与 hauler 会系统性超配一倍。Overmind 正是在这里近似了（§3.2，它按 colony 的 RCL 而不是按房的 reservation 推 `energyPerTick`），这是一个可以直接绕开的已知坑。

### 9.4 对 ADR 0038 的直接影响：算得出来，而且答案是"要看有没有路"

ADR 0038 把翻案条件写死成"hauler 行离开地板"，并点名 #83。用它自己的算术（`ceil(往返 tick × 10 / hauler 容量)`，每个 source container 一项、各自向上取整）配上 #83 的 BFS 距离（W12S27 47 步、W13S28 46 与 56 步），并按 fabot 的 walk 语义把往返拆成"空车去 + 满车回"（`[Carry;Carry;Move]` 空载任何地形全速，满载在路上全速、在平地半速）：

| | 往返 tick | RCL4（16C，800 容量） | RCL5（24C，1200 容量） |
|---|---|---|---|
| W12S27，**有路** | 2×47 = 94 | `ceil(940/800)` = **2** | `ceil(940/1200)` = **1** |
| W12S27，**无路** | 47 + 94 = 141 | `ceil(1410/800)` = **2** | `ceil(1410/1200)` = **2** |
| W13S28 近源，有路 | 92 | **2** | **1** |
| W13S28 远源，有路 | 112 | **2** | **1** |
| W13S28 近/远源，无路 | 138 / 168 | **2 / 3** | **2 / 2** |

（"无路"一列是**下界**：#83 的 BFS 是纯地形 Chebyshev 步数，而一只满载的 `[Carry;Carry;Move]` 踩沼泽是 10 tick 一格，不是 2 tick。路线里每有一格沼泽，无路的往返就再加 8 tick。）

**结论：remote 在 RCL4、以及在"还没铺路"的任何 RCL 下都把 hauler 行抬离地板；铺完路的 RCL5 remote 又落回地板。**ADR 0038 的"the answer flips when the hauler row leaves its floor"因此**成立，但窗口是有限的** —— 而按 #83 第二条评论的排期，实现落地时房间已经在 RCL5。所以：

- 若 remote 先上、路后铺（社区的普遍做法是 RCL3–4 才开始铺，§5.1），**中间态确实会翻案**；
- 路一铺好就翻回来。而 **link 不跨房**（§1.6），所以那对 link 只会建在本房、服务本房的 container —— ADR 0038 里"指向 Storage 的那一对"仍然是正确的那一对，只是它的 prize 仍然是**一个 body**。

**给 ADR 0038 的一句话**：remote 不是它等的那个翻案条件的**稳定**满足者。真正稳定抬起 hauler 行的是 ADR 0038 自己列的第二条 —— "throughput rises past what one hauler's capacity covers" —— 而那要等到 remote 的产量叠进本房的 container 吞吐里，不是 remote 上线的那一刻。

### 9.5 落地顺序建议（按本文的证据，不是按偏好）

1. **先做多房投影的地形层 + 稀疏实体层**，因为它零成本（§9.3）且是别的一切的前提。W12S27 / W13S28 的 fixture 已经躺在 `tests/Core.Tests/rooms/` 里（ADR 0036），whole-room invariants 可以直接扩到"跨房 walk 的下界是多房 Chebyshev"。
2. **W12S27 不 reserve 先采**（社区共识 + 本服 46 个活例）。它的 7 格外房暴露度让这一步几乎无风险，而 `sourceOutputPerTick` 从常量变 per-source 的改动正好在这一步被逼出来，而不是事后补。
3. **补 reserver**，`[2Claim;2Move]`，1300 = RCL4 银行上限。同时监视 §1.4 的入侵时刻表：单源 reserve 后 10 e/tick ⇒ **约 10,000 tick 后第一次入侵**，而且永远是 small 体。
4. **铺路**，与本房路网一张网（§5.1）。路一铺好，W12S27 的 hauler 项就从 2 掉回 1。**修的一侧建议学 TooAngel 而不是 Overmind**：给 remote hauler 的体型前缀加一个 WORK、走一步修一步（`repairRoadOnSpot`），比"通用 worker 定期巡路"便宜得多，也不需要一个新的 Task 行 —— 而 fabot 的 Repair 池已经按"整条线"判血（ADR 0010），把一条 remote 路整体丢进去反而会引来跨房的 Repair Task。
5. **把 invader core 的扩张写成一个可监视的量，而不是一个担心**：4 房外的 W15S24 是 level 4 stronghold，前沿已在两房之外的 W14S23，每 2,500 tick 扩张一次（下一次 tick 107,860），而 `expandStronghold` **不看 reservation**（§1.5）。一个 level-0 core 落进 W12S27 或 W13S28，你的 reservation 每 tick 净掉 3，2 CLAIM 顶不住。撤退不是答案（core 100,000 血、不产兵、不会自己走），Winsley 的 `KillCoreMission` 是。**另一半是好消息**：这座 stronghold 的 `EFFECT_COLLAPSE_TIMER` 在 tick 170,283 到期，之后整个 sector 的入侵开关会关闭，直到下一座生成 —— 这个日期是可读的，值得进观测通道。

## 来源

- 引擎常量：`screeps/common` `lib/constants.js`（`SOURCE_ENERGY_*`、`ENERGY_REGEN_TIME`、`CONTROLLER_RESERVE`、`CONTROLLER_RESERVE_MAX`、`CREEP_CLAIM_LIFE_TIME`、`ROAD_*`、`CONTAINER_DECAY*`、`CONSTRUCTION_COST`、`REPAIR_*`、`ENERGY_DECAY`、`INVADER_CORE_*`、`STRONGHOLD_*`）
- 引擎行为：`screeps/engine` `src/processor/intents/sources/tick.js`、`creeps/reserveController.js`、`controllers/tick.js`、`roads/tick.js`、`containers/tick.js`、`energy/tick.js`、`movement.js`、`links/transfer.js`、`src/game/structures.js`、`src/processor/intents/invader-core/{pretick,reserveController,attackController,create-creep}.js` 与 `invader-core/stronghold/stronghold.js`、`src/processor.js`
- 后端：`screeps/backend-local` `lib/cronjobs.js`（`genInvaders`、`checkExit`、`createRaid`）、`lib/strongholds.js`（`spawnStronghold`、`selectRoom`、`expandStronghold`）、`lib/utils.js`
- 官方文档：`screeps/docs` `api/source/PathFinder.md`（`maxOps`/`maxRooms`/`heuristicWeight`/`roomCallback`）、`api/source/Map.md`（`findRoute`、`getRoomTerrain`）、`api/source/Room.Terrain.md`、`source/control.md`（"reserving a Controller in a neutral room restores energy sources to their full capacity"）。`source/contributed/caching-overview.md`（tedivm, 2017）是官方站上托管的**社区**投稿，非第一方，本文只引其一句缓存策略并如此标注。
- bot 源码：
  - `bencbartlett/Overmind`：`src/Overseer.ts`、`src/Colony.ts`、`src/directives/colony/outpost.ts`、`src/overlords/mining/miner.ts`、`src/overlords/colonization/reserver.ts`、`src/overlords/core/transporter.ts`、`src/overlords/core/worker.ts`、`src/roomPlanner/RoadPlanner.ts`、`src/logistics/RoadLogistics.ts`、`src/intel/RoomIntel.ts`、`src/movement/{Pathing,Movement}.ts`、`src/caching/GlobalCache.ts`、`src/strategy/ExpansionEvaluator.ts`
  - `bonzaiferroni/bonzAI`：`src/ai/missions/{SurveyAnalyzer,MiningMission,ReserveMission,Mission,InvaderGuru,BodyguardMission}.ts`、`src/ai/operations/MiningOperation.ts`、`src/ai/Traveler.ts`、`src/ai/WorldMap.ts`、`src/config/constants.ts`
  - `The-International-Screeps-Bot/The-International-Open-Source`：`src/room/commune/remotesManager.ts`、`src/room/commune/spawning/spawnRequests.ts`、`src/room/commune/haulerNeedOps.ts`、`src/room/roomFunctions.ts`（`scoutMyRemote`）、`src/room/roomNameUtils.ts`、`src/room/room.ts`、`src/room/roomData.ts`、`src/room/roomOps.ts`、`src/room/creeps/roleManagers/remote/remoteSourceHarvester.ts`、`src/room/creeps/roles/haulerOps.ts`、`src/room/construction/construction.ts`、`src/international/{customPathFinder,collective}.ts`、`src/utils/utils.ts`、`src/constants/general.ts`
  - `ScreepsQuorum/screeps-quorum`：`src/programs/city/mine.js`、`src/programs/city.js`、`src/extends/room/{territory,intel,movement,control,economy,construction}.js`、`src/extends/creep/movement.js`、`src/extends/creep.js`、`src/extends/source.js`、`src/roles/{miner,hauler,reservist,spook}.js`、`src/lib/map.js`、`src/lib/cluster.js`、`src/programs/empire/intel.js`、`src/programs/city/publicworks.js`
  - `TooAngel/screeps`：`src/prototype_room_external.js`、`src/prototype_room_routing.js`、`src/prototype_room_costmatrix.js`、`src/prototype_room_memory.js`、`src/prototype_room.js`、`src/prototype_creep_harvest.js`、`src/prototype_creep_routing.js`、`src/prototype_creep_resources.js`、`src/role_{sourcer,carry,reserver}.js`、`src/brain_memory.js`、`src/config.js`
  - `screepers/screeps-cartographer`：`src/lib/Movement/{moveTo,generatePath,cachedPaths}.ts`、`src/lib/WorldMap/{findRoute,portals,selectors}.ts`、`src/lib/CostMatrixes/{index,sourceKeepers}.ts`、`src/lib/TrafficManager/{reconcileTraffic,moveLedger}.ts`、`src/config.ts`、`README.md`、`pages/trafficManagement.md`
  - `glitchassassin/screeps`：`src/Selectors/getTerritoriesByOffice.ts`、`src/Missions/Implementations/{HarvestMission,LogisticsMission,ReserveMission,MainOfficeMission,DefendRemoteMission,KillCoreMission}.ts`、`src/Selectors/Franchises/*`、`src/Selectors/Map/Pathing.ts`、`src/Intel/**`、`src/Minions/Builds/*`、`src/utils/packrat.ts`
- **引用时要当心的死代码/缺陷**（本次读码逐一确认，避免下游把它们当成活着的做法）：bonzAI 的 `workerBody(6,1,6)` remote 分支不可达；Overmind 的 `$.costMatrix` 写键用 `'m'` 而 `costMatrixRecall` 读键用 `':'`，无视野矩阵召回永远落空，且 `ExpansionEvaluator` 算出的 `factor` 从未被乘；TI 的 `remoteMemory[pathType].length / packedPosLength` 少了 `[i]`，remoteBuilder 的请求体第一行就是 `return false`，remoteDefender 整段被注释；Quorum 的 `MINE_WEIGHTS_WALKABILITY = 0`、`removeMine` 无法删除索引 0、hauler 的 `distance` 只覆盖自有房那一段；TooAngel 的 `config.external` 零引用、`routeCallbackRoomHandle` 的 Occupied/Blocked 判据既无人写入 `Memory.rooms[x].state` 又会被后续赋值覆盖、`spawnCarry` 把 `amount[0]/amount[1]` 的 CARRY 与 MOVE 读反、`Creep.buildRoads` 定义后从未调用；Winsley 的 `franchiseDisabled.ts` 及其收益窗口无任何引用。
- 本服实测：官方只读 API `https://screeps.com/season`，shard `shardSeason`，tick 105,897–106,529（2026-09-05）。端点 `/api/game/shards/info`、`/api/game/time`、`/api/game/world-size`、`/api/game/map-stats`（`statName: "owner0"`，reservation 表现为 `own.level === 0`）、`/api/game/room-objects`、`/api/auth/me`（只读 `cpu`/`gcl`）。全部为 GET/查询，无任何写操作。
- 赛季规则：见 `seasonal-threats-safemode.md` §2 的第一方来源（官方 Steam 新闻），本文只复核了扁平 100 CPU 与市场状态。
