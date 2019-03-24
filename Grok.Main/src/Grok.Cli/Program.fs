// Learn more about F# at http://fsharp.org

open System
open Grok.Lib

[<EntryPoint>]
let main argv =
    argv
    |> Array.iter (printfn "%s")

    0 // return an integer exit code
