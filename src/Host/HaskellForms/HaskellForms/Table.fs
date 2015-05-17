module Table

open System
open System.Collections.Generic
open System.Windows.Forms
open System.Reflection

let mutable count = 0
let private objects = Dictionary<int, obj>()
let private objectIds = Dictionary<obj, int>()

let step () =
    count <- count + 1
    count - 1

let registerObject obj = 
    objects.[count] <- obj
    objectIds.[obj] <- count
    step()

let getObject id : Either<string, 'a> =
    if id = -1 then Right null
    else if objects.ContainsKey id then
        match objects.[id] with
        | :? 'a as a -> Right a
        | _          -> sprintf "The object with id %d isn't of the expected type %A" id typeof<'a> |> Left
    else sprintf "No object with id %d" id |> Left

let getId obj : Either<string, int> =
    if objectIds.ContainsKey obj then objectIds.[obj] |> Right
    else sprintf "The object %A doesn't have an id" obj |> Left

let getIdOrRegister obj =
    getId obj |> Either.unpack (fun _ -> registerObject obj) id

let unregisterById id = 
    if objects.ContainsKey id then
        objectIds.Remove (objects.[id]) |> ignore
        objects.Remove id |> ignore
