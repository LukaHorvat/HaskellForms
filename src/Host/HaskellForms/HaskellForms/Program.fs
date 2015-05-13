// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.Threading
open Interface
open System.Windows.Forms
open System
open System.Text
open System.Diagnostics

[<EntryPoint>]
let main argv = 
    Manager.runUiThread()
    Manager.initialize()
    Interface.startListening()
    0 // return an integer exit code
