module Manager
type private Marker = interface end

open Microsoft.FSharp.Reflection
open System.Reflection
open System
open System.Windows.Forms
open System.Threading
open Interface
open System.Drawing

let mutable uiForm = null
let inUiThread (f : unit -> 'a) =
    Thread.byControl uiForm f ()

[<STAThread>]
let runUiThread () =
    Thread.inNewThread( fun () ->
        Application.EnableVisualStyles()
        uiForm <- new Form()
        uiForm.Handle |> ignore
        Interface.closing.Publish |> Event.add (fun () -> Application.Exit())
        Application.Run() ) |> ignore

let newControl msg =
    match msg with
    | New "Form" ->
        inUiThread( fun () ->
            let form = new Form()
            let id = Table.registerObject form
            form.Show()
            objectId id )
    | New name ->
        let typ = Assembly.GetAssembly(typeof<Form>).GetType("System.Windows.Forms." + name)
        if typ = null then sprintf "No type named %s exists in System.Windows.Forms" name |> Error
        else if typ.IsSubclassOf(typeof<Control>) |> not then sprintf "The type %s is not a Control" name |> Error 
        else typ |> Activator.CreateInstance :?> Control |> Table.registerObject |> objectId
    | _ -> Pass

let invoke msg =
    match msg with
    | Invoke(_, _, values) | InvokeStatic(_, _, values) ->
        eith {
            let! args = values 
                       |> List.map( function
                       | Unpacked o    -> Right o
                       | ObjectId i    -> Table.getObject i
                       | _             -> Left "This shouldn't happen (Manager.fs 1)" )
                       |> Either.sequence
            match msg with
            | Invoke(objId, meth, _) -> 
                let! obj = Table.getObject objId
                let! res = inUiThread (fun () -> obj |> Method.invoke meth (Array.ofList args))
                return res |> Option.map Interface.pack |> Option.unpack NoResponse Value
            | InvokeStatic(typeName, methName, _) ->
                let! res = inUiThread (fun () -> Method.invokeStatic typeName methName (Array.ofList args))
                return res |> Option.map Interface.pack |> Option.unpack NoResponse Value
            | _ -> return! Left "This shouldn't happen (Manager.fs 2)"
        } |> Either.unpack Error id
    | _ -> Pass

let getOrSet msg : Response =
    match msg with
    | Get(id, prop) ->
        Table.getObject id |> Either.bind (Property.named prop) |> Either.bind Property.get |> Either.unpack Error (Interface.pack >> Value)
    | Set(id, prop, v) ->
        inUiThread( fun () ->
            eith {
                let! o = match v with
                         | Unpacked o -> Right o
                         | ObjectId i -> Table.getObject i
                         | _          -> Left "This shouldn't happen (Manager.fs 3)"
                let! ctrl = Table.getObject id
                return! ctrl |> Property.named prop |> Either.bind (Property.set o |> Thread.byControl ctrl) 
            } |> Either.unpack Error (const' NoResponse) )
    | _ -> Pass

let getEvent msg =
    match msg with
    | GetEvent(id, evtName) ->
        Table.getObject id |> Either.bind (Event.named evtName) |> Either.map (Events.registerEvent) |> Either.unpack Error objectId
    | _ -> Pass

// Automatically loads the methods with the right signature in this module
let initialize () =
    let toHandler (m : MethodInfo) = m.CreateDelegate(typeof<Func<Message, Response>>) :?> Func<Message, Response>
    typeof<Marker>.DeclaringType.GetMethods()
    |> Seq.filter (fun m -> m.ReturnType = typeof<Response> && (m.GetParameters() |> Array.map (fun p -> p.ParameterType)) = [| typeof<Message> |])
    |> Seq.iter (fun m -> toHandler m |> (fun f -> fun msg -> f.Invoke msg) |> addMessageHandler)