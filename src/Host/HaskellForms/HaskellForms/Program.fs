// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.Diagnostics

[<EntryPoint>]
let main argv = 
    if not Debugger.IsAttached then Debugger.Launch() |> ignore
    Manager.runUiThread()
    Manager.initialize()
    Interface.startListening()
    0 // return an integer exit code
