module Events

open System.Collections.Generic
open System
open Reflection

let private events = Dictionary<int, Delegate>()

let registerEvent (Evt(evt, tgt)) =
    let currentCount = Table.count
    let handler = EventHandler(fun t args -> Interface.fireEvent currentCount t args)
    evt.AddEventHandler(tgt, handler)
    events.[currentCount] <- handler
    Table.step()

let unregisterEvent evtId =
    if events.ContainsKey evtId then
        events.Remove evtId |> ignore