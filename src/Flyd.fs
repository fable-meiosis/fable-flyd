module rec MFlyd

open Fable.Core
open Fable.Core.JsInterop

// ts2fable 0.6.1
open System
open Fable.Core
open Fable.Import.JS

type [<AllowNullLiteral>] ICurriedFunction2<'T1, 'T2, 'R> =
    interface end

module Flyd =
    type [<AllowNullLiteral>] Stream<'T> =
        [<Emit "$0($1...)">] abstract Invoke: unit -> 'T
        [<Emit "$0($1...)">] abstract Invoke: value: 'T -> Stream<'T>
        [<Emit "$0($1...)">] abstract Invoke: value: U2<Promise<'T>, PromiseLike<'T>> -> Stream<'T>
        abstract map: project: ('T -> 'V) -> Stream<'V>
        abstract ap: this: Stream<('A -> 'B)> * stream: Stream<'A> -> Stream<'B>
        abstract chain: project: ('T -> Stream<'V>) -> Stream<'V>
        abstract ``of``: [<ParamArray>] values: ResizeArray<'V> -> Stream<'V>
        abstract pipe: operator: (Stream<'T> -> Stream<'V>) -> Stream<'V>
        abstract ``[fantasy-land/map]``: project: ('T -> 'V) -> Stream<'V>
        abstract ``[fantasy-land/ap]``: fn: (Stream<'T> -> 'V) -> Stream<'V>
        abstract ``[fantasy-land/chain]``: project: ('T -> Stream<'V>) -> Stream<'V>
        abstract ``[fantasy-land/of]``: [<ParamArray>] values: ResizeArray<'V> -> Stream<'V>
        abstract ``end``: Stream<bool> with get, set
        abstract ``val``: 'T with get, set
        abstract hasVal: bool with get, set

    type [<AllowNullLiteral>] Combine =
        [<Emit "$0($1...)">] abstract Invoke: fn: (Stream<'T> -> Stream<'R> -> U2<'R, unit>) * streams: Stream<'T> -> Stream<'R>
        [<Emit "$0($1...)">] abstract Invoke: fn: (Stream<'T> -> Stream<'T1> -> Stream<'R> -> U2<'R, unit>) * streams: Stream<'T> * Stream<'T1> -> Stream<'R>
        [<Emit "$0($1...)">] abstract Invoke: fn: (Stream<'T> -> Stream<'T1> -> Stream<'T2> -> Stream<'R> -> U2<'R, unit>) * streams: Stream<'T> * Stream<'T1> * Stream<'T2> -> Stream<'R>
        [<Emit "$0($1...)">] abstract Invoke: fn: (Stream<'T> -> Stream<'T1> -> Stream<'T2> -> Stream<'T3> -> Stream<'R> -> U2<'R, unit>) * streams: Stream<'T> * Stream<'T1> * Stream<'T2> * Stream<'T3> -> Stream<'R>

    type [<AllowNullLiteral>] CreateStream =
        [<Emit "$0($1...)">] abstract Invoke: unit -> Stream<'T>
        [<Emit "$0($1...)">] abstract Invoke: value: 'T -> Stream<'T>
        [<Emit "$0($1...)">] abstract Invoke: value: U2<Promise<'T>, PromiseLike<'T>> -> Stream<'T>

    type [<AllowNullLiteral>] Static =
        abstract stream: CreateStream with get, set
        abstract immediate: stream: Stream<'T> -> Stream<'T>
        abstract isStream: stream: obj option -> bool
        abstract combine: Combine with get, set
        abstract endsOn: ``end$``: Stream<obj option> * stream: Stream<'T> -> Stream<'T>
        abstract map: accessor: ('T -> 'V) -> (Stream<'T> -> Stream<'V>)
        abstract map: accessor: ('T -> 'V) * stream: Stream<'T> -> Stream<'V>
        abstract ap: ``value$``: Stream<'A> * ``transform$``: Stream<('A -> 'B)> -> Stream<'B>
        abstract ap: ``value$``: Stream<'A> -> (Stream<('A -> 'B)> -> Stream<'B>)
        abstract chain: accessor: ('T -> Stream<'V>) -> (Stream<'T> -> Stream<'V>)
        abstract chain: accessor: ('T -> Stream<'V>) * stream: Stream<'T> -> Stream<'V>
        abstract on: onfn: ('T -> unit) -> (Stream<'T> -> Stream<unit>)
        abstract on: onfn: ('T -> unit) * stream: Stream<'T> -> Stream<unit>
        abstract scan: reducer: ('T -> 'V -> 'T) * initial: 'T * stream: Stream<'V> -> Stream<'T>
        abstract scan: reducer: ('T -> 'V -> 'T) * initial: 'T -> (Stream<'V> -> Stream<'T>)
        abstract scan: reducer: ('T -> 'V -> 'T) -> ('T -> (Stream<'V> -> Stream<'T>))
        abstract merge: stream1: Stream<'T> * stream2: Stream<'V> -> Stream<U2<'T, 'V>>
        abstract merge: stream1: Stream<'T> -> (Stream<'V> -> Stream<U2<'T, 'V>>)
        abstract transduce: mapfn: Function * stream: Stream<'T> -> Stream<'V>
        abstract transduce: mapfn: Function -> (Stream<'T> -> Stream<'V>)
        abstract fromPromise: promise: PromiseLike<'T> -> Stream<'T>
        abstract flattenPromise: ``promise$``: Stream<PromiseLike<'T>> -> Stream<'T>
        abstract curryN: length: float * fn: (Array<obj option> -> unit) -> Function

        type [<AllowNullLiteral>] IExports =
            abstract f: Flyd.Static

    module MAfterSilence =

        type [<AllowNullLiteral>] aftersilence =
            [<Emit "$0($1...)">] abstract Invoke: delay: float * stream: Flyd.Stream<'T> -> Flyd.Stream<ResizeArray<'T>>
            [<Emit "$0($1...)">] abstract Invoke: delay: float -> (Flyd.Stream<'T> -> Flyd.Stream<ResizeArray<'T>>)

    module MDropRepeats =
        type [<AllowNullLiteral>] IExports =
            abstract dropRepeats: dropRepeats
            abstract dropRepeatsWith: dropRepeatsWith

        type [<AllowNullLiteral>] dropRepeats =
            [<Emit "$0($1...)">] abstract Invoke: s: Flyd.Stream<'T> -> Flyd.Stream<'T>

        type [<AllowNullLiteral>] dropRepeatsWith =
            [<Emit "$0($1...)">] abstract Invoke: isEqual: ('T -> 'T -> bool) * stream: Flyd.Stream<'T> -> Flyd.Stream<'T>
            [<Emit "$0($1...)">] abstract Invoke: isEqual: ('T -> 'T -> bool) -> (Flyd.Stream<'T> -> Flyd.Stream<'T>)

    module MAfterSilence =

        type [<AllowNullLiteral>] aftersilence =
            [<Emit "$0($1...)">] abstract Invoke: delay: float * stream: Flyd.Stream<'T> -> Flyd.Stream<ResizeArray<'T>>
            [<Emit "$0($1...)">] abstract Invoke: delay: float -> (Flyd.Stream<'T> -> Flyd.Stream<ResizeArray<'T>>)


    module MDropRepeats =
        type [<AllowNullLiteral>] IExports =
            abstract dropRepeats: dropRepeats
            abstract dropRepeatsWith: dropRepeatsWith

        type [<AllowNullLiteral>] dropRepeats =
            [<Emit "$0($1...)">] abstract Invoke: s: Flyd.Stream<'T> -> Flyd.Stream<'T>

        type [<AllowNullLiteral>] dropRepeatsWith =
            [<Emit "$0($1...)">] abstract Invoke: isEqual: ('T -> 'T -> bool) * stream: Flyd.Stream<'T> -> Flyd.Stream<'T>
            [<Emit "$0($1...)">] abstract Invoke: isEqual: ('T -> 'T -> bool) -> (Flyd.Stream<'T> -> Flyd.Stream<'T>)

    module MEvery =
        type [<AllowNullLiteral>] IExports =
            abstract _every: every

        type [<AllowNullLiteral>] every =
            [<Emit "$0($1...)">] abstract Invoke: ms: float -> Flyd.Stream<obj>

    module MFilter =

        type [<AllowNullLiteral>] IExports =
            abstract _Filter: Filter

        type [<AllowNullLiteral>] Filter =
            [<Emit "$0($1...)">] abstract Invoke: project: ('T -> bool) * stream: Flyd.Stream<'T> -> Flyd.Stream<'V>
            [<Emit "$0($1...)">] abstract Invoke: project: ('T -> bool) -> (Flyd.Stream<'T> -> Flyd.Stream<'V>)
            [<Emit "$0($1...)">] abstract Invoke: predicate: ('T -> obj option) * stream: Flyd.Stream<'T> -> Flyd.Stream<'T>
            [<Emit "$0($1...)">] abstract Invoke: predicate: ('T -> obj option) -> (Flyd.Stream<'T> -> Flyd.Stream<'T>)

    module MForwardTo =

        type [<AllowNullLiteral>] IExports =
            abstract _ForwardTo: ForwardTo

        type [<AllowNullLiteral>] ForwardTo =
            [<Emit "$0($1...)">] abstract Invoke: stream: Flyd.Stream<'V> * project: ('V -> 'T) -> Flyd.Stream<'T>
            [<Emit "$0($1...)">] abstract Invoke: stream: Flyd.Stream<'V> -> (('V -> 'T) -> Flyd.Stream<'T>)

    module MInLast =

        type [<AllowNullLiteral>] IExports =
            abstract _InLast: InLast

        type [<AllowNullLiteral>] InLast =
            [<Emit "$0($1...)">] abstract Invoke: ms: float * stream: Flyd.Stream<'T> -> Flyd.Stream<ResizeArray<'T>>
            [<Emit "$0($1...)">] abstract Invoke: ms: float -> (Flyd.Stream<'T> -> Flyd.Stream<ResizeArray<'T>>)

    module MKeepWhen =

        type [<AllowNullLiteral>] IExports =
            abstract _KeepWhen: KeepWhen

        type [<AllowNullLiteral>] KeepWhen =
            [<Emit "$0($1...)">] abstract Invoke: ``when``: Flyd.Stream<bool> * stream: Flyd.Stream<'T> -> Flyd.Stream<'T>
            [<Emit "$0($1...)">] abstract Invoke: ``when``: Flyd.Stream<bool> -> (Flyd.Stream<'T> -> Flyd.Stream<'T>)

    module MLift =
        type [<AllowNullLiteral>] IExports =
            abstract _Lift: Lift

        type [<AllowNullLiteral>] Lift =
            [<Emit "$0($1...)">] abstract Invoke: liftFn: ('T1 -> 'T2 -> 'R) * s1: Flyd.Stream<'T1> * s2: Flyd.Stream<'T2> -> Flyd.Stream<'R>
            [<Emit "$0($1...)">] abstract Invoke: liftFn: ('T1 -> 'T2 -> 'T3 -> 'R) * s1: Flyd.Stream<'T1> * s2: Flyd.Stream<'T2> * s3: Flyd.Stream<'T3> -> Flyd.Stream<'R>
            [<Emit "$0($1...)">] abstract Invoke: liftFn: ('T1 -> 'T2 -> 'T3 -> 'T4 -> 'R) * s1: Flyd.Stream<'T1> * s2: Flyd.Stream<'T2> * s3: Flyd.Stream<'T3> * s4: Flyd.Stream<'T4> -> Flyd.Stream<'R>
            [<Emit "$0($1...)">] abstract Invoke: liftFn: (ResizeArray<obj option> -> 'T) * [<ParamArray>] streams: ResizeArray<Flyd.Stream<obj option>> -> Flyd.Stream<'T>

    module MMergeAll =
        type [<AllowNullLiteral>] IExports =
            abstract _MergeAll: MergeAll

        type [<AllowNullLiteral>] MergeAll =
            [<Emit "$0($1...)">] abstract Invoke: streams: Flyd.Stream<'T1> * Flyd.Stream<'T2> -> Flyd.Stream<U2<'T1, 'T2>>
            [<Emit "$0($1...)">] abstract Invoke: streams: Flyd.Stream<'T1> * Flyd.Stream<'T2> * Flyd.Stream<'T3> -> Flyd.Stream<U3<'T1, 'T2, 'T3>>
            [<Emit "$0($1...)">] abstract Invoke: streams: ResizeArray<Flyd.Stream<'T>> -> Flyd.Stream<'T>


    module MObj =

        type [<AllowNullLiteral>] IExports =
            abstract _ObjModule: ObjModule

        type [<AllowNullLiteral>] ObjModule =
            abstract streamProps: obj: 'T -> obj
            abstract stream: obj: 'T -> Flyd.Stream<obj>
            abstract extractProps: obj: obj option -> obj option

    module MPrevious =

        type [<AllowNullLiteral>] IExports =
            abstract _previous: Previous

        type [<AllowNullLiteral>] Previous =
            [<Emit "$0($1...)">] abstract Invoke: stream: Flyd.Stream<'T> -> Flyd.Stream<'T>

    module MSampleOn =

        type [<AllowNullLiteral>] IExports =
            abstract _SampleOn: SampleOn

        type [<AllowNullLiteral>] SampleOn =
            [<Emit "$0($1...)">] abstract Invoke: samplewhen: Flyd.Stream<obj option> * stream: Flyd.Stream<'T> -> Flyd.Stream<'T>
            [<Emit "$0($1...)">] abstract Invoke: samplewhen: Flyd.Stream<obj option> -> (Flyd.Stream<'T> -> Flyd.Stream<'T>)

    module MScanMerge =

        type [<AllowNullLiteral>] IExports =
            abstract _ScanMerge: ScanMerge

        type [<AllowNullLiteral>] ScanFn<'T, 'V> =
            [<Emit "$0($1...)">] abstract Invoke: acc: 'T * value: 'V -> 'T

        type [<AllowNullLiteral>] ScanMerge =
            [<Emit "$0($1...)">] abstract Invoke: pairs: Array<Flyd.Stream<'V> * ScanFn<'T, 'V>> * initial: 'T -> Flyd.Stream<'T>
            [<Emit "$0($1...)">] abstract Invoke: pairs: Array<Flyd.Stream<'V> * ScanFn<'T, 'V>> -> ('T -> Flyd.Stream<'T>)

    module MSwitchLatest =

        type [<AllowNullLiteral>] IExports =
            abstract _SwitchLatest: SwitchLatest

        type [<AllowNullLiteral>] SwitchLatest =
            [<Emit "$0($1...)">] abstract Invoke: stream: Flyd.Stream<Flyd.Stream<'T>> -> Flyd.Stream<'T>

    module MTakeUntil =

        type [<AllowNullLiteral>] IExports =
            abstract _takeuntil: takeuntil

        type [<AllowNullLiteral>] takeuntil =
            [<Emit "$0($1...)">] abstract Invoke: source: Flyd.Stream<'T> * ``end``: Flyd.Stream<'V> -> Flyd.Stream<'T>
            [<Emit "$0($1...)">] abstract Invoke: source: Flyd.Stream<'T> -> (Flyd.Stream<'V> -> Flyd.Stream<'T>)

let [<Import("*","module")>] flyd: Flyd.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/droprepeats``: Flyd.MDropRepeats.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/every``: Flyd.MEvery.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/filter``: Flyd.MFilter.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/forwardto``: Flyd.MForwardTo.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/inlast``: Flyd.MInLast.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/keepwhen``: Flyd.MKeepWhen.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/lift``: Flyd.MLift.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/mergeall``: Flyd.MMergeAll.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/obj``: Flyd.MObj.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/previous``: Flyd.MPrevious.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/sampleon``: Flyd.MSampleOn.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/scanmerge``: Flyd.MScanMerge.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/switchlatest``: Flyd.MSwitchLatest.IExports = jsNative
let [<Import("*","module")>] ``flyd/module/takeuntil``: Flyd.MTakeUntil.IExports = jsNative
