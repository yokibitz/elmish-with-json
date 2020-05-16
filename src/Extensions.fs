[<AutoOpen>]
module Extensions

open Elmish

module Cmd =
    let fromAsync (operation: Async<'msg>): Cmd<'msg> =
        let delayedCmd (dispatch: 'msg -> unit): unit =
            let delayedDispatch =
                async {
                    let! msg = operation
                    dispatch msg }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress
    | Resolved of 't

type AsyncOperationStatus<'t> =
    | Started
    | Finished of 't

// let resolved =
//     function
//     | HasNotStartedYet -> false
//     | InProgress -> false
//     | Resolved _ -> true

// let inProgress =
//     function
//     | HasNotStartedYet -> false
//     | InProgress -> true
//     | Resolved _ -> false

// let map transform deferred =
//     match deferred with
//     | HasNotStartedYet -> HasNotStartedYet
//     | InProgress -> InProgress
//     | Resolved value -> Resolved(transform value)

// let exists predicate =
//     function
//     | HasNotStartedYet -> false
//     | InProgress -> false
//     | Resolved value -> predicate value

// let bind transform deferred =
//     match deferred with
//     | HasNotStartedYet -> HasNotStartedYet
//     | InProgress -> InProgress
//     | Resolved value -> transform value
