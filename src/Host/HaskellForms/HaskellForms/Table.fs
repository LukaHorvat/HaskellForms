module Table

open System
open System.Collections.Generic
open System.Windows.Forms

let mutable private count = 0
let private controls = Dictionary<int, Control>()
let registerControl ctrl = 
    controls.[count] <- ctrl
    count <- count + 1
    count - 1
let getControl id : Either<string, 'a> =
    if controls.ContainsKey id then Right (controls.[id] :?> 'a)
    else sprintf "No control with id %d" id |> Left
let unregisterById id = controls.Remove id