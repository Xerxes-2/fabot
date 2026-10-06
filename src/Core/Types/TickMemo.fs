/// Indexes derived from one tick's immutable values, kept for that tick alone.
/// A colony's projection is asked the same join a hundred times a tick (which
/// room an id stands in, the ids of one kind); each memo here derives the join
/// once per value and answers every later ask off it. The key is a value no
/// tick mutates — a `Map`, a record of them — so the same key is the same
/// answer, and keys compare by `same`, by reference for an index over one
/// collection. Bounded, and emptied once a tick by the shell (`forgetAll`), so
/// no tick's objects outlive it, like `HostileInfo.forgetSides`.
module Fabot.Core.Types.TickMemo

/// A key's entry: asked this many times and not yet derived
/// (`recallAfter`), or derived.
type private Held<'v> =
    | Asked of int
    | Derived of 'v

/// One memo: the latest entries, latest first, each beside its key.
type Memo<'k, 'v> =
    private
        {
            Same: 'k -> 'k -> bool
            Kept: int
            mutable Entries: ('k * Held<'v>) list
        }

/// Every memo's emptying, registered as it is made.
let mutable private forgets: (unit -> unit) list = []

/// A memo keeping `kept` keys at most, its keys compared by `same`.
let create (kept: int) (same: 'k -> 'k -> bool) : Memo<'k, 'v> =
    let memo =
        {
            Same = same
            Kept = kept
            Entries = []
        }

    forgets <- (fun () -> memo.Entries <- []) :: forgets
    memo

/// Key equality by reference: the same immutable object, the same answer.
let byReference (a: 'k) (b: 'k) : bool = obj.ReferenceEquals(a, b)

let rec private find (same: 'k -> 'k -> bool) (key: 'k) (entries: ('k * Held<'v>) list) =
    match entries with
    | [] -> None
    | (held, value) :: rest -> if same held key then Some value else find same key rest

/// Files a key's entry first, any older entry for it dropped. The entry is
/// whole before it is published, so a reader on another thread (Expecto's)
/// sees it whole or not at all; a race loses an entry, never corrupts one.
let private publish (memo: Memo<'k, 'v>) (key: 'k) (value: Held<'v>) =
    memo.Entries <-
        (key, value)
        :: (memo.Entries
            |> List.filter (fun (held, _) -> not (memo.Same held key))
            |> List.truncate (memo.Kept - 1))

/// The derivation of `key`, built on its first ask and recalled after.
let recall (memo: Memo<'k, 'v>) (key: 'k) (build: 'k -> 'v) : 'v =
    match find memo.Same key memo.Entries with
    | Some(Derived value) -> value
    | _ ->
        let value = build key
        publish memo key (Derived value)
        value

/// The derivation of `key` from its `asks`th ask on, and None before, which
/// the caller answers the direct way: for a key asked a few times, the
/// direct answers cost less than the index.
let recallAfter (asks: int) (memo: Memo<'k, 'v>) (key: 'k) (build: 'k -> 'v) : 'v option =
    match find memo.Same key memo.Entries with
    | Some(Derived value) -> Some value
    | Some(Asked count) when count + 1 >= asks ->
        let value = build key
        publish memo key (Derived value)
        Some value
    | Some(Asked count) ->
        publish memo key (Asked(count + 1))
        None
    | None when asks <= 1 ->
        let value = build key
        publish memo key (Derived value)
        Some value
    | None ->
        publish memo key (Asked 1)
        None

/// Empties every memo: the shell's, once a tick, so none outlives it.
let forgetAll () =
    for forget in forgets do
        forget ()
