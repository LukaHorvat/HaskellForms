module Interface

open System
open System.Collections.Generic
open Microsoft.FSharp.Reflection
open System.Text
open System.Drawing
open System.Text.RegularExpressions

type Value = Int of int | String of string | Color of Color | Point of Point | Size of Size | Font of Font | ObjectId of int
type Message = New of string | Set of int * string * Value | Get of int * string | Invoke of int * string * int * (Value list) | GetEvent of int * string
type Response = Pass | NoResponse | Error of string | Value of Value
type EventMsg = TargetArgs of int * Value * Value
type Outgoing = EventMsg of EventMsg | Response of Response

let (|Unpacked|_|) = function
    | ObjectId _ -> None
    | v          -> FSharpValue.GetUnionFields(v, typeof<Value>) |> snd |> Array.head |> Some

let private valueTypes = 
    FSharpType.GetUnionCases(typeof<Value>) 
    |> Seq.map (fun case -> case, case.GetFields() |> Array.head) 
    |> Seq.map (fun (case, prop) -> prop.PropertyType, fun x -> FSharpValue.MakeUnion(case, [| x |])) 
    |> Seq.toList
let (|Packed|_|) (o : obj) = List.tryFind (fst >> (=) (o.GetType())) valueTypes |> Option.map (fun (_, cons) -> cons o :?> Value)

let pack (o : obj) =
    match o with
    | Packed v -> v
    | o        -> Table.getIdOrRegister o |> ObjectId

let objectId n = ObjectId n |> Value

type Reader(str : string) =
    let mutable words = 
        seq {
            let builder = StringBuilder(str.Length)
            let eatingString = ref false
            let escape = ref false
            for i = 0 to str.Length - 1 do
                let c = str.Chars i
                if !eatingString then
                    if !escape then 
                        escape := false
                        builder.Append c |> ignore
                    else if c = '\\' then
                        escape := true
                        builder.Append c |> ignore
                    else if c = '"' then
                        yield builder.ToString()
                        eatingString := false
                        builder.Clear() |> ignore
                    else
                        builder.Append c |> ignore
                else
                    if c = '"' then
                        if builder.Length > 0 then
                            yield builder.ToString()
                            builder.Clear() |> ignore
                        eatingString := true
                    else if c = ' ' then
                        if builder.Length > 0 then
                            yield builder.ToString()
                            builder.Clear() |> ignore
                    else
                        builder.Append c |> ignore
            if builder.Length > 0 then
                yield builder.ToString()
        } |> Seq.map Regex.Unescape |> List.ofSeq
    let readWith p name =
        match words with
        | w :: ws ->
            try 
                let res = p w
                words <- ws
                Right res
            with e -> sprintf "Cannot parse %s from \"%s\"" name w |> Left
        | _ -> Left "Cannot read past the end of string"
    member x.EndOfString () = words = []
    member x.ReadInt () = readWith Int32.Parse "int"
    member x.ReadFloat () = readWith Single.Parse "float"
    member x.ReadWord () = readWith id "string"
    member x.ReadByte () = readWith (fun s -> s.Chars 0 |> byte) "byte"
    
let private size x y = Drawing.Size(x, y)
let private point x y = Drawing.Point(x, y)
let private color r g b a = Drawing.Color.FromArgb(a, r, g, b)
let private font (name : string) size = new Drawing.Font(name, size)

let rec private read (reader : Reader) (t : Type) : Either<string, obj> =
    let ri = reader.ReadInt
    if      typeof<int>    = t then ri() |> Either.cast
    else if typeof<float>  = t then reader.ReadFloat() |> Either.cast
    else if typeof<byte>   = t then reader.ReadByte() |> Either.cast
    else if typeof<string> = t then reader.ReadWord() |> Either.cast
    else if typeof<Size>   = t then Either.lift2 size (ri()) (ri()) |> Either.cast
    else if typeof<Point>  = t then Either.lift2 point (ri()) (ri()) |> Either.cast
    else if typeof<Color>  = t then Either.lift4 color (ri()) (ri()) (ri()) (ri()) |> Either.cast
    else if typeof<Font>   = t then Either.lift2 font (reader.ReadWord()) (reader.ReadFloat()) |> Either.cast
    else if FSharpType.IsUnion(t) then readUnion reader t
    else failwith "Unsupported type. Cannot be read."

and private readUnion (reader : Reader) (t : Type) = eith {
    let! caseName = reader.ReadWord()
    if t = typeof<Message> && caseName = "Invoke" then
        let! objId = reader.ReadInt()
        let! metName = reader.ReadWord()
        let! numArgs = reader.ReadInt()
        let! argList = [1..numArgs] |> List.map (fun _ -> read reader typeof<Value> |> Either.cast) |> Either.sequence
        return Invoke(objId, metName, numArgs, argList) :> obj
    else
        let cases = FSharpType.GetUnionCases(t)
        let! case = cases |> Seq.tryFind (fun case -> case.Name = caseName) |> Either.ofOption (sprintf "Case name %s not found on type %A" caseName t)
        let! args = case.GetFields() |> Array.map (fun prop -> read reader prop.PropertyType) |> Either.sequence
        return FSharpValue.MakeUnion(case, args |> Seq.toArray)
}

let private readMessage str : Either<string, Message> = read (Reader str) typeof<Message> |> Either.cast

let rec private write (builder : StringBuilder) x =
    match box x with
    | :? int | :? float | :? byte -> builder.Append(x.ToString() + " ")             |> ignore
    | :? string as st -> builder.Append(sprintf "\"%s\"" st)                        |> ignore
    | :? Point  as pt -> builder.Append(sprintf "%d %d " pt.X pt.Y)                 |> ignore
    | :? Size   as sz -> builder.Append(sprintf "%d %d " sz.Width sz.Height)        |> ignore
    | :? Color  as cl -> builder.Append(sprintf "%d %d %d %d " cl.R cl.G cl.B cl.A) |> ignore
    | :? Font   as ft -> builder.Append(sprintf "\"%s\" %f " ft.Name ft.Size)       |> ignore
    | _ when FSharpType.IsUnion(x.GetType()) -> 
        let (caseInfo, args) = FSharpValue.GetUnionFields(x, x.GetType())
        builder.Append(caseInfo.Name + " ") |> ignore
        args |> Seq.iter (fun arg -> write builder arg)

let private handlers = List<Message -> Response>()
let addMessageHandler handler = handlers.Add handler

let send x =
    let builder = StringBuilder()
    write builder x
    builder.ToString() |> Console.WriteLine

let private onMessage e =
    match e with
    | Left err -> Error err
    | Right msg ->
        let proc resp h =
            match resp with
            | Pass -> h msg
            | x    -> x
        let resp = handlers |> Seq.fold proc Pass
        if resp = Pass then sprintf "No handler provided for the message %A (even though the message for recognized)" msg |> Error
        else resp
    |> Response |> send

let rec private checkInput () = 
    let line = Console.ReadLine()
    if line <> null then 
        readMessage line |> onMessage
        checkInput()

let startListening () =
    while true do checkInput ()

let fireEvent evtId evtTgt evtArgs =
    let tgt = pack evtTgt
    let args = pack evtArgs
    TargetArgs(evtId, tgt, args) |> EventMsg |> send