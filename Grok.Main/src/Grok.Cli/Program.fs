// Learn more about F# at http://fsharp.org

open System
open Grok.Lib

// [<EntryPoint>]
// let main argv =
//     argv
//     |> Array.iter (printfn "%s")

//     0 // return an integer exit code

// 1. Either run a REPL window or execute one expression
[<EntryPoint>]
let main(args: string[]) =
    match Array.toList args with
    | [] -> Repl.runRepl ()
    | filename :: args -> Repl.runOne filename args

    0
