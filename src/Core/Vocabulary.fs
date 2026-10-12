/// Each union compares by a hand-written ordinal: Fable's generic union
/// equality walks the case structure, and v1 measured it on hot paths.
[<AutoOpen>]
module Fabot.Core.Vocabulary

[<CustomEquality; CustomComparison>]
type BodyPart =
    | Work
    | Carry
    | Move
    | Attack
    | RangedAttack
    | Heal
    | Claim
    | Tough

    member this.Ordinal =
        match this with
        | Work -> 0
        | Carry -> 1
        | Move -> 2
        | Attack -> 3
        | RangedAttack -> 4
        | Heal -> 5
        | Claim -> 6
        | Tough -> 7

    override this.Equals other =
        match other with
        | :? BodyPart as that -> this.Ordinal = that.Ordinal
        | _ -> false

    override this.GetHashCode() = this.Ordinal

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? BodyPart as that -> compare this.Ordinal that.Ordinal
            | _ -> 1

let samePart (a: BodyPart) (b: BodyPart) : bool = a.Ordinal = b.Ordinal

let partName (part: BodyPart) : string =
    match part with
    | Work -> "work"
    | Carry -> "carry"
    | Move -> "move"
    | Attack -> "attack"
    | RangedAttack -> "ranged_attack"
    | Heal -> "heal"
    | Claim -> "claim"
    | Tough -> "tough"

let partOfName (name: string) : BodyPart option =
    match name with
    | "work" -> Some Work
    | "carry" -> Some Carry
    | "move" -> Some Move
    | "attack" -> Some Attack
    | "ranged_attack" -> Some RangedAttack
    | "heal" -> Some Heal
    | "claim" -> Some Claim
    | "tough" -> Some Tough
    | _ -> None

let partLetter (part: BodyPart) : char =
    match part with
    | Work -> 'W'
    | Carry -> 'C'
    | Move -> 'M'
    | Attack -> 'A'
    | RangedAttack -> 'R'
    | Heal -> 'H'
    | Claim -> 'K'
    | Tough -> 'T'

let allBodyParts = [ Work; Carry; Move; Attack; RangedAttack; Heal; Claim; Tough ]

/// A fight's view of a part's boost (`BOOSTS`): a WORK boost for harvest,
/// build or upgrade and a CARRY boost read as `Unboosted`.
type Boost =
    | Unboosted
    | Tier1
    | Tier2
    | Tier3

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Boost =
    let ofCompound (part: BodyPart) (compound: string) : Boost =
        match part, compound with
        | Attack, "UH"
        | RangedAttack, "KO"
        | Heal, "LO"
        | Tough, "GO"
        | Move, "ZO"
        | Work, "ZH" -> Tier1
        | Attack, "UH2O"
        | RangedAttack, "KHO2"
        | Heal, "LHO2"
        | Tough, "GHO2"
        | Move, "ZHO2"
        | Work, "ZH2O" -> Tier2
        | Attack, "XUH2O"
        | RangedAttack, "XKHO2"
        | Heal, "XLHO2"
        | Tough, "XGHO2"
        | Move, "XZHO2"
        | Work, "XZH2O" -> Tier3
        | _ -> Unboosted

    /// `calcBodyEffectiveness`; TOUGH's boost is in `damageRatio`.
    let multiple (part: BodyPart) (boost: Boost) : int =
        match part, boost with
        | (Attack | RangedAttack | Heal | Move | Work), Tier1 -> 2
        | (Attack | RangedAttack | Heal | Move | Work), Tier2 -> 3
        | (Attack | RangedAttack | Heal | Move | Work), Tier3 -> 4
        | _ -> 1

    let damageRatio (part: BodyPart) (boost: Boost) : float =
        match part, boost with
        | Tough, Tier1 -> 0.7
        | Tough, Tier2 -> 0.5
        | Tough, Tier3 -> 0.3
        | _ -> 1.0

let partCountIn (body: BodyPart list) part =
    let mutable count = 0

    for each in body do
        if samePart each part then
            count <- count + 1

    count

[<CustomEquality; CustomComparison>]
type Ore =
    | Hydrogen
    | Oxygen
    | Utrium
    | Keanium
    | Lemergium
    | Zynthium
    | Catalyst

    member this.Ordinal =
        match this with
        | Hydrogen -> 0
        | Oxygen -> 1
        | Utrium -> 2
        | Keanium -> 3
        | Lemergium -> 4
        | Zynthium -> 5
        | Catalyst -> 6

    override this.Equals other =
        match other with
        | :? Ore as that -> this.Ordinal = that.Ordinal
        | _ -> false

    override this.GetHashCode() = this.Ordinal

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Ore as that -> compare this.Ordinal that.Ordinal
            | _ -> 1

let allOres = [ Hydrogen; Oxygen; Utrium; Keanium; Lemergium; Zynthium; Catalyst ]

/// The UH line alone; a compound joins the day a decision names it.
[<CustomEquality; CustomComparison>]
type Compound =
    | Hydroxide
    | UtriumHydride
    | UtriumAcid

    member this.Ordinal =
        match this with
        | Hydroxide -> 0
        | UtriumHydride -> 1
        | UtriumAcid -> 2

    override this.Equals other =
        match other with
        | :? Compound as that -> this.Ordinal = that.Ordinal
        | _ -> false

    override this.GetHashCode() = this.Ordinal

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Compound as that -> compare this.Ordinal that.Ordinal
            | _ -> 1

let allCompounds = [ Hydroxide; UtriumHydride; UtriumAcid ]

/// A season's own resource stays `Season`'s and joins here only as `Other`.
[<CustomEquality; CustomComparison>]
type Resource =
    | Energy
    | Ore of Ore
    | Compound of Compound
    | Other of name: string

    member this.Ordinal =
        match this with
        | Energy -> 0
        | Ore ore -> 1 + ore.Ordinal
        | Compound compound -> 16 + compound.Ordinal
        | Other _ -> 64

    override this.Equals other =
        match other with
        | :? Resource as that ->
            match this, that with
            | Other a, Other b -> a = b
            | _ -> this.Ordinal = that.Ordinal
        | _ -> false

    override this.GetHashCode() =
        match this with
        | Other name -> name.GetHashCode()
        | _ -> this.Ordinal

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Resource as that ->
                match this, that with
                | Other a, Other b -> compare a b
                | _ -> compare this.Ordinal that.Ordinal
            | _ -> 1

let resourceName (resource: Resource) : string =
    match resource with
    | Energy -> "energy"
    | Ore Hydrogen -> "H"
    | Ore Oxygen -> "O"
    | Ore Utrium -> "U"
    | Ore Keanium -> "K"
    | Ore Lemergium -> "L"
    | Ore Zynthium -> "Z"
    | Ore Catalyst -> "X"
    | Compound Hydroxide -> "OH"
    | Compound UtriumHydride -> "UH"
    | Compound UtriumAcid -> "UH2O"
    | Other name -> name

let resourceOfName (name: string) : Resource =
    match name with
    | "energy" -> Energy
    | "H" -> Ore Hydrogen
    | "O" -> Ore Oxygen
    | "U" -> Ore Utrium
    | "K" -> Ore Keanium
    | "L" -> Ore Lemergium
    | "Z" -> Ore Zynthium
    | "X" -> Ore Catalyst
    | "OH" -> Compound Hydroxide
    | "UH" -> Compound UtriumHydride
    | "UH2O" -> Compound UtriumAcid
    | other -> Other other

let isMineral =
    function
    | Ore _
    | Compound _ -> true
    | Energy
    | Other _ -> false
