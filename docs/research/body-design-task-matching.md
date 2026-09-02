# Creep 身体动态生成与任务分配的互动 — 社区成熟 bot 的做法

Date: 2026-09-03. 所有论断均对照一手来源核验（官方文档源码、bot 默认分支源码——本日 shallow-clone 本地阅读、第一方博文、官方论坛原帖），未核实处标注 **unverified**。动机：fabot 目前只有一种通用 worker 体（`src/Core/Decide.fs` `workerBody`，`[Work; Carry; Move]` 单元按能量重复），而任务池是无角色的——本文调查①按岗位定制体型如何与无固定角色共存，②heavy-WORK 静态体的惯例配置，③"同点挖矿+升级"复合点位有无先例。姊妹篇：`task-matching-travel-cost.md`（分配几何侧）、`creep-positioning-traffic.md`（seat 与交通侧，已含 5-WORK 饱和推导）。

## Summary

- **身体生成的社区标准形态是"每岗位一个 pattern + 按能量重复"**：`numRepeats = min(能量上限, 50件上限, 岗位 sizeLimit)`。Overmind `CreepSetup`、Quorum `buildFromTemplate`、Winsley `buildFromSegment`、bonzAI `bodyRatio` 是同一个算法的四次独立实现；The International 用 `defaultParts + extraParts × partsMultiplier` 再叠一层按 `spawnEnergyCapacity` 的手写阶梯。**没有任何被调查 bot 用单一通用体**——矿工、hauler、upgrader、worker 的 pattern 全部不同。
- **静态矿体惯例高度一致：5–6 WORK + 1 CARRY + ceil(WORK/2) 个 MOVE**（MOVE:WORK ≈ 1:2，即走路半速；只为出生时走到岗位一次）。1 个 CARRY 用于修 container / 喂 link。远程矿工因为要跨房跑路，MOVE 提到 1:1（bonzAI remote 6W1C6M）。
- **Hauler 惯例：有路 [CARRY,CARRY,MOVE]，无路 [CARRY,MOVE]**，且 CARRY 总数由需求账本（带宽 = 距离×流量）倒推，不是"能量能买多大买多大"。**Upgrader 惯例：WORK-heavy 静态体**，RCL8 封顶 15 WORK（引擎上限 15 energy/tick）。
- **①的答案：有先例但只有一个成熟谱系** —— Jon Winsley 的 role-free 任务系统（2020）里任意 minion 接任意任务、匹配时看 "distance + the minion's active parts"（身体感知匹配）；2021 简化后仍保留"身体档案（SALESMAN/ACCOUNTANT/RESEARCH…）按岗位铸造、行为由 mission/任务层决定"的解耦。其余成熟 bot（Overmind、TI、Quorum、bonzAI）全部是出生带角色。
- **③的答案：未找到任何"同点挖矿+升级"复合点位的实现或专门讨论**（标 unverified 的否定）。但引擎层面它是可行的：官方 simultaneous-actions 流水线图中 `harvest` 与 `upgradeController` 不在同一条依赖链上，能量足够时可同 tick 执行。2016 年论坛有过"多职责 creep 不划算"的反对（MyrddinE），其论据（移动模式不同）恰好不适用于静态同点情形。

## 0. 引擎事实（官方文档）

来源：https://docs.screeps.com/creeps.html 、https://docs.screeps.com/api/#Constants 、官方文档源码 https://github.com/screeps/docs/blob/master/source/creeps.md 。

- 部件成本：`MOVE 50, WORK 100, CARRY 50, ATTACK 80, RANGED_ATTACK 150, HEAL 250, TOUGH 10, CLAIM 600`；`MAX_CREEP_SIZE: 50`；`HARVEST_POWER: 2`、`UPGRADE_CONTROLLER_POWER: 1`、`BUILD_POWER: 5`、`CARRY_CAPACITY: 50`。
- fatigue（`source/creeps.md` 原文）："Each body part (except `MOVE`) generates fatigue points when the creep moves: 1 point per body part on roads, 2 on plain land, 10 on swamp." / "Each `MOVE` body part decreases fatigue points by 2 per tick." / **"It's worth noting that empty `CARRY` parts don't generate fatigue."** / "To maintain the maximum movement speed of 1 square per tick, a creep needs to have as many `MOVE` parts as all the other parts of its body combined."
- 由此推出三条社区通用配比（推导，自洽于上述规则）：**1:1** = 任何地形非 swamp 全速（战斗/远程体）；**1:2** = 路上全速、平地半速（经济体标配，路网建成后）；**2C:1M hauler** 空载时 CARRY 不计重 → 空载平地也全速，只有满载离路才半速。
- 升级上限：RCL8 控制器每 tick 最多吃 15 energy —— TI 与 Winsley 的代码注释均以此为封顶依据（见 §1.3；引擎常量 `CONTROLLER_MAX_UPGRADE_PER_TICK: 15`，https://github.com/screeps/common/blob/master/lib/constants.js）。
- **同 tick 动作流水线**（https://docs.screeps.com/simultaneous-actions.html 的 `img/action-priorities.png`，本次直接读图核验）：依赖链一为 `harvest → attack → build → repair → dismantle → attackController → rangedHeal → heal`；链二为 `rangedAttack → rangedMassAttack → build → repair → rangedHeal`；`upgradeController` **不在任何依赖链里**，只出现在"总能量不足时"的仲裁链 `upgradeController → build or repair → withdraw → transfer → drop`（最右者执行）。即 `harvest` 与 `upgradeController` 同 tick 无意图冲突，仅在携带能量不足以同时支付时按仲裁链取舍。

## 1. 身体如何按能量与岗位生成

### 1.1 Overmind（bencbartlett/Overmind）— pattern 重复的原型实现

`src/creepSetups/CreepSetup.ts`：`bodySetup = {pattern, sizeLimit, prefix, suffix, proportionalPrefixSuffix, ordered}`；核心是 `numRepeats = Math.min(energyLimit, maxPartLimit, sizeLimit)`，其中 `energyLimit = floor((availableEnergy - extraCost) / patternCost)`、`maxPartLimit = floor((50 - prefix - suffix) / patternLength)`；`ordered: true` 按部件类型分组（WWW MMM），否则交错（WM WM WM）。

`src/creepSetups/setups.ts` 的岗位 pattern 表（原文）：

| 岗位 | pattern | sizeLimit |
|---|---|---|
| miner standard | `[W,W,W,W,W,W,CARRY,M,M,M]` | **1** |
| miner default（早期/link） | `[W,W,CARRY,M]` | 3 |
| miner double | 同 standard | 2 |
| transporter | `[CARRY,CARRY,M]` | Infinity |
| transporter early | `[CARRY,M]` | Infinity |
| worker | `[W,CARRY,M]` | Infinity |
| worker early | `[W,CARRY,M,M]` | Infinity |
| upgrader | `[W,W,W,CARRY,M]` | Infinity |
| upgrader RCL8 | 同上 | **5**（=15 WORK 封顶） |

`src/overlords/mining/miner.ts`：`miningPowerNeeded = ceil(energyPerTick / HARVEST_POWER) + 1`（10/2+1 = 6 WORK，多 1 备份）；按房间能量选模式 `'early' | 'SK' | 'link' | 'standard' | 'double'`——买不起 standard 体（能量 < 其造价）就退回 default pattern ×3 的小矿工，`minersNeeded = min(ceil(power/每体power), seat数)`。矿工钉死在 `container.pos`（或 link 旁）。注意 **standard 矿体 MOVE:非MOVE = 3:7**，平地半速都不到——静态体一生只走一次，社区不为它付 1:1 的钱。

### 1.2 The International（The-International-Screeps-Bot/The-International-Open-Source，branch Main）— 手写能量阶梯

`src/room/commune/spawning/spawnRequestConstructors.ts`：请求分 `individualUniform`（单只，defaultParts + 尽量多的 extraParts）与 `groupDiverse/groupUniform`（一组，`partsQuota` 记账到部件粒度）；`50 - defaultParts.length` 内按 `extraParts.length × partsMultiplier` 填充。

`src/room/commune/spawning/spawnRequests.ts` 的 `sourceHarvester()` 是最完整的阶梯样本：`spawnEnergyCapacity ≥ 850` 时程序化构造 —— `workAmount = 6`（有 PWR_REGEN_SOURCE 效果时加算），每 2 个 WORK 插 1 MOVE、每 5 件插 1 CARRY，结果 **6W+3M+2C（MOVE:WORK = 1:2）**；≥800 时 `[CARRY] + [W,M,W]×3`；≥750 `[W,M,W]×3`；≥600 `[M,CARRY] + [W]×6`；≥550 `[M] + [W]×6`；最低档退化为 groupDiverse 多只小矿工，`maxCreeps = min(3, harvestPositions.length)`（seat 封顶）。`sourceIndex` 与 `preferRoads` 写进出生 memory——**挖哪个 source 是出生前定死的**（姊妹篇 §3 已证其排序按路径长度预烘焙）。

hauler（`haulerForCommune()`）：`hasSufficientRoads` 时 `[CARRY,CARRY,MOVE]`，否则 `[CARRY,MOVE]`；数量由账本 `carryPartsNeed = communeHaulerNeed - communeHaulerCarryParts` 倒推（`partsQuota = carryPartsNeed × …`），并用 `findCommuneHaulerMaxCost` 保证全体 hauler 同尺寸。upgrader（`controllerUpgraders()`）：RCL8 固定 `15W + 3C + 8M` 单体；有 controller container/link 时按能量档位用 WORK 重 pattern（1400+：`[M,C,W×4,M,W×4,M,W×4]` 即 12W:3M:1C），`maxCreeps = upgradePositions.length`（link 占位再减 1）——静态 upgrader 同样 seat 封顶。mineralHarvester：`[M,M,W×7,CARRY]` 起步、extraParts 纯 WORK 堆——矿物体比能量矿更极端的 WORK 倾斜。

### 1.3 Jon Winsley（glitchassassin/screeps，archived）— role-free 谱系的身体档案

`src/Minions/Builds/salesman.ts`（静态矿工，"SALESMAN"）：能量 <550 时 `[W,W,M]`；<600 时 link/remote `[3W,1C,1M]`、否则 **`[5W,1M]`**；充足时 `buildFromSegment(energy, [W,W,W,W,W,M], {maxSegments: 2, suffix: link ? [CARRY] : []})`——本地静态矿体是 **5 WORK + 1 MOVE（无 container 修缮需求时连 CARRY 都不带；link 模式补 1 CARRY）**，整只 creep 只有 1 个 MOVE。`src/Minions/Builds/accountant.ts`（hauler）：无路 `[CARRY,MOVE]`、有路 `[CARRY,CARRY,MOVE]` 段重复，双 spawn 前 `maxSegments` 压到 13（小体多只）。`src/Minions/Builds/research.ts`（upgrader）：按能量以 **10:1:2 的 W:C:M 预算比**分配，`workParts` 封顶 15，注释原文 "Max for an upgrader at RCL8 is 15 energy/tick, so we'll cap these there"。`src/Minions/Builds/utils.ts` `buildFromSegment`：`segmentCount = min(floor(energy/segmentCost), floor((50-suffix)/segment.length), maxSegments)`——与 Overmind 同构。

### 1.4 bonzAI（bonzaiferroni/bonzAI）— 比例式 + 需求倒推

`src/ai/missions/Mission.ts`：`workerBody(w,c,m)` 直排三段；`bodyRatio(workRatio, carryRatio, moveRatio, spawnFraction, limit)` 按比例买到能量/50件上限。`MiningMission.getMinerBody()`：**`work = ceil((SOURCE_ENERGY_CAPACITY / ENERGY_REGEN_TIME) / HARVEST_POWER) + 1` = 6，体 = `workerBody(6, 1, ceil(6/2)=3)`**——WORK 数从 source 产能推导而非拍死；remote 则 `workerBody(6,1,6)`（1:1，要跨房走）。`Mission.analyzeTransport(distance, load, maxSpawnEnergy)`：注释 "cargo units are just 2 CARRY, 1 MOVE"，`bandwidthNeeded = distance × load × 2.1`，由此得 cart 数与每 cart 的 CARRY 数——**hauler 规模 = 距离×流量的带宽计算**。`UpgradeMission.linkUpgraderBody()`：满配 `workerBody(30, 4, 15)`（30W:4C:15M，MOVE 1:2），`potencyPerCreep` 封顶 30、`findMaxUpgraders` 最多 5 只。

### 1.5 Quorum（ScreepsQuorum/screeps-quorum）— 模板循环填充

`src/extends/creep.js` `Creep.buildFromTemplate(template, energy)`：`while (energy > 0 && parts.length < 50) { next = template[parts.length % template.length]; … }`——模板循环取模填到钱尽。`src/roles/miner.js`：本地 base `[M,C,W×6,M,M]`（= 6W1C3M，又是 1:2），remote base 追加 MOVE 至 4 个 + 第 7 个 WORK；`src/roles/hauler.js` `[MOVE,CARRY]` 循环；`src/roles/upgrader.js` `[MOVE,CARRY,WORK]` 循环。角色目录 `src/roles/`（miner/hauler/upgrader/builder/filler/…）——典型出生带角色。

### 1.6 社区参考件

- Wiki《Creep body setup strategies》（https://wiki.screepspl.us/index.php/Creep_body_setup_strategies）：两种设计法（预定义体表 vs 模板重复）；"Roads speed creep movement by a factor of 2, meaning only half as many MOVE parts are required"；部件排序惯例——TOUGH 永远最前（伤害只由首部件吃）、MOVE 放最后保战斗机动、HEAL 靠末尾；矿物工模板 `[WORK,WORK,MOVE]`（"half of WORK parts amount of MOVE"）。
- Wiki《Static Harvesting》（https://wiki.screepspl.us/index.php/Static_Harvesting）：drop/container/link 三式定义；container 相比 drop 减损约 90%；link 传输损 3%、RCL5+；"若矿工自己维护 container，它至少需要 1 个 CARRY"（修缮用能）；WORK 数的权衡原文大意——大矿体更快抽干 source、少发 harvest intent 省 CPU，小矿体便宜但 intent 多。
- screepers/screeps-snippets 有社区 body calculator 条目（`src/misc/screeps body calculator.md`，nitroevil, 2016，实现在 CodePen）——身体计算器作为通用工具在社区流通。
- 官方论坛《Questions on roles》（https://screeps.com/forum/topic/913/questions-on-roles，2016）：DoctorZuber 的三角色案 —— 矿工 `[W×6,CARRY,MOVE]`、hauler "2 carry / 1 move 的倍数"、upgrader 同矿体尺寸（"one work to spend one energy to upgrade one point … plan on needing a big one"）。**2016 年的论坛惯例与 2023 年的 TI 代码给出同一个矿体**。

### 小结（问题②的答案）

heavy-WORK 静态矿体的社区惯例收敛为：**5–6 WORK（5 饱和 +1 备份）· 0–2 CARRY（0=纯 drop、1=修 container/喂 link）· 1–3 MOVE（≈ WORK/2，即 1:2 半速；Winsley 极端到 1 个）**，`sizeLimit = 1`（一只体面矿工，不是一群小的），买不起时逐级降档、最低档退回"多只小矿工挤满 seats"。静态 upgrader 同理 WORK-heavy（W:M ≈ 2:1 甚至 4:1），RCL8 封 15 WORK。**MOVE 1:1 只保留给要持续跑路的体**（远程矿工、hauler 无路时、战斗体）。

## 2. 任务分配与身体的互动（问题①）

### 2.1 出生带角色是绝对主流

Overmind：creep 由 Overlord 的 wishlist 按 `CreepSetup` 请求，出生即隶属某 Overlord、`Roles.drone/transport/worker/…` 写进名字与 memory；博文自述角色如何被收编——"Each `Role` used to govern the creep control logic … all creep control logic … and spawn request logic has been combined in the new `Overlord` class"（https://bencbartlett.com/blog/screeps-1-overlord-overload/）。值得注意的架构细节：`Zerg` 是"task- and overlord-contextualized wrapper"（仓库 README），即 **creep 本体是泛型的，行为全部来自所属 Overlord + 挂载的 task**——角色是组织层概念而非 creep 属性，这与 fabot 的方向只差"出生时身体已按 Overlord 定制"一步。TI/Quorum/bonzAI：role 字符串/roles 目录/Mission 编制，均为硬角色（§1 已引文件）。

### 2.2 role-free 任务池 + 身体感知匹配的先例：Jon Winsley

（其博客对本代理返回 403，以下博文引文转引自姊妹篇 `task-matching-travel-cost.md` §2，该篇于 2026-09-02 直接核验过原文；仓库代码本次本地重读。）

- 2020 任务系统（https://www.jonwinsley.com/screeps/2020/09/21/screeps-task-management/）：minion 无角色，任务带 `TaskPrerequisite`，不满足展开子任务——身体是先决条件的一部分（能不能 WORK、能不能装载）。
- 2020-10 简化（https://www.jonwinsley.com/notes/screeps-logistics-overhaul）："picking the best minion is a matter of a) distance from the task and b) **the minion's active parts**"——**这是"身体感知任务匹配"的最直接文字先例**：匹配器读部件表，不读角色标签。
- 2021 Great Purge 后的成熟形态（本次读码）：`src/Minions/minionTypes.ts` 枚举身体档案（SALESMAN/ACCOUNTANT/RESEARCH/ENGINEER…），`src/Minions/Builds/*.ts` 每档案一个生成函数（§1.3）；mission 按需求申请某档案的 build，运行时在 mission 内部再做任务分派（如 `LogisticsMission` 的 ledger 用 creep 实际 capacity 记账，见姊妹篇 §2）。**即：身体档案按岗位铸造，但"档案"是 spawn 期的采购规格，不是运行期的行为绑定**——这正是"按岗位定制体型 + creep 无固定角色"组合的现存最佳先例。
- 他的方向变化也值得记录：完全通用的 minion + 任务树被他自己判为过度工程（"a simple hard-coded solution is both more efficient and more effective"，Great Purge 一文），最终停在"少数身体档案 × 任务/mission 分派"的中间点，而不是回到硬角色。

### 2.3 结论

"body 档案（岗位规格）"与"运行期角色"在成熟实践里是**两个可以独立选择的轴**：所有 bot 都做前者；只有 Winsley 谱系（及 Overmind 的 Zerg 抽象一半）放弃后者。没有发现任何 bot 让通用体去干专职（除了兜底 worker），也没有发现"任务池按 creep 部件表计算任务收益"之外更深的身体感知匹配（如按 fatigue 预测行程时间来匹配——**unverified**，未见实现）。

## 3. 静态点位的普遍做法

（详细 seat 机制见姊妹篇 `creep-positioning-traffic.md` §1，此处仅补身体侧结论。）

- container mining 全员采用：Overmind `harvestPos = container.pos` 钉死；TI `maxCreeps = communeSourceHarvestPositions.length`；bonzAI `MiningMission.findContainer()`；Winsley salesman 常驻 franchise 位。link mining 在 RCL5+ 替代（体上加 1 CARRY，Overmind link 模式换小体 default、Winsley link 模式加 CARRY suffix）。
- 静态 upgrader 同构：controller container/link（bonzAI "battery"、TI `upgradeStructure`）+ upgrade seats（TI `upgradePositions`、bonzAI `findMaxUpgraders` ≤5）；`withdraw` 与 `upgradeController` 不在同一依赖链，可同 tick 拉取+升级（官方流水线图，§0）。
- **静态体的身体后果**：seat 一旦钉死，MOVE 立即贬值为一次性出生成本 → 全社区 1:2 以下；CARRY 贬值为修缮/缓冲的 1–2 件 → 省下的钱全部换 WORK。这条因果链是"任务决定身体"最清晰的实例。

## 4. 复合点位：同点挖矿+升级（问题③）

- **未找到实现先例**：在 Overmind/TI/bonzAI/Quorum/Winsley 五个仓库中均无"source 与 controller 同时在射程内 → 一只静态 creep 交替/同时 harvest+upgrade"的代码路径；针对性搜索（论坛/Reddit/wiki）也未见专门讨论此点位的帖子。此为否定性结论，**unverified**（不能排除小众 bot 或未被索引的讨论存在）。
- 最接近的讨论是官方论坛《Questions on roles》（2016，https://screeps.com/forum/topic/913/questions-on-roles）：Fou_Lou 主张"multirole creeps that use renewing"（source 空了就去 build/repair）；MyrddinE 反驳多职责体 "rarely efficient"，论据是移动模式与能耗率不同——"a builder moves a lot and burns through 5 energy per tick (per WORK), while a controller[-upgrader] moves rarely and burns through only 1 energy per tick (per WORK)"。**注意该论据只否定"流动多职责"，不适用于零移动的复合静态点**：同点情形下两职共享同一个 heavy-WORK 静态体，MOVE 浪费为零。另见同期搜索结果中 "longUpgraders use the harvester's energy source when they aren't busy" 的提法（转述，原帖未定位，**unverified**）。
- **引擎允许**（§0 流水线图）：`harvest` 在依赖链一，`upgradeController` 不在任何依赖链——同 tick 二者可并行；仅当携带能量不足以支付 upgrade 时进入"能量不足"仲裁（`upgradeController` 在仲裁链最左，会被右侧动作挤掉——实际影响是需要保证 store 里留有 ≥ WORK 数的能量）。数值上：n 个 WORK 每 tick harvest 2n 入库、upgrade 出库 n，收支 2:1，加 1–2 个 CARRY 做缓冲即可自持；source 侧 10 e/t 的产出上限意味着复合体的 harvest 份额封顶 5 WORK，upgrade 份额则可另配。
- 为什么没人做（推断，非引文）：source 与 controller 距离 ≤4（1+3 射程叠合）的地形罕见；role-based 架构下"半个矿工半个 upgrader"没有归属；container/link 物流把两点解耦后，分离方案的损耗已很小。对无角色任务池而言这些障碍多数不存在。

## 对 fabot 的启示

1. **引入"岗位 pattern 表 + numRepeats"生成器**替换单一 `workerUnit`：`min(energy/patternCost, (50-fixed)/len, sizeLimit)` 五家同构，纯函数、好测；TI 式手写阶梯代码量大且难维护，不建议学。fabot 现有 `bodyFor` 按 `bank.Available` 出体的做法与社区 bootstrap 惯例（无 creep 时用 available 而非 capacity）一致，保留。
2. **静态矿体直接采用惯例配置**：`5–6 WORK + 1 CARRY + WORK/2 个 MOVE`、每 source 一只（seat=container tile）；upgrader 走 WORK-heavy（W:M ≈ 2:1），RCL8 封 15 WORK；hauler `[Carry;Carry;Move]`（有路前 `[Carry;Move]`），CARRY 总量按 距离×10e/t 带宽倒推而非按能量买满。
3. **无角色 ≠ 无身体档案**：学 Winsley——身体档案是 spawn 期的采购规格，Matcher 在运行期做身体感知（按 active WORK/CARRY 计算某 creep 干某任务的产出率），任务池架构不必为定制体引入角色。这个组合有直接先例且是该谱系演化的稳定终点。
4. **Matcher 的身体感知从两个量开始就够**：WORK 数（harvest/upgrade/build 产出率）与 CARRY 容量（物流账本单位）——Winsley 的 "distance + active parts" 与 TI 的 partsQuota 都止步于此，没人按 fatigue 建模行程时间。
5. **复合点位是 fabot 的合法差异化实验**：无先例但引擎许可（harvest 与 upgradeController 不同流水线，可同 tick），且社区对多职责的反对论据不适用于静态同点。先写个地形扫描统计 source–controller 距离 ≤4 的房间比例，再决定是否值得建这种 Seat。
6. **降档阶梯要有底**：所有 bot 在能量不足时逐级换小 pattern、最终退回"多只小矿工挤满 seats"——fabot 的生成器应把"最低可用体"作为显式档位而不是失败分支。
