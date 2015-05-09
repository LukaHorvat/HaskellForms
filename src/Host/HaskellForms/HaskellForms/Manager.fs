module Manager
type private Marker = interface end

open Microsoft.FSharp.Reflection
open System.Reflection
open System
open System.Windows.Forms
open System.Threading
open Interface

let newControl msg =
    match msg with
    | New "Form" ->
        let id = ref -1
        Thread.inNewThread( fun () ->
            let form = new Form()
            id := Table.registerControl form
            form.ShowDialog() ) |> ignore
        Ref.waitOnRef ((<>) -1) id |> ControlId
    | New name ->
        let typ = Assembly.GetAssembly(typeof<Form>).GetType("System.Windows.Forms." + name)
        if typ = null then sprintf "No control named %s exists in System.Windows.Forms" name |> Error
        else typ |> Activator.CreateInstance :?> Control |> Table.registerControl |> ControlId
    | _ -> Pass

let addChildControl msg =
    match msg with
    | AddChildControl(parentId, childId) -> 
        eith {
            let! parent = Table.getControl parentId
            let! child  = Table.getControl childId
            Thread.byControl parent parent.Controls.Add child
            return NoResponse 
        } |> Either.getOrErr
    | _ -> Pass

let getOrSet msg : Response=
    match msg with
    | Get(id, prop) ->
        Table.getControl id |> Either.bind (Property.named prop) |> Either.bind Property.get |> Either.unpack Error Value
    | Set(id, prop, Unpacked value) ->
        eith {
            let! ctrl = Table.getControl id
            return! ctrl |> Property.named prop |> Either.bind (Property.set value |> Thread.byControl ctrl) } |> Either.unpack Error (const' NoResponse)
    | _ -> Pass

let initialize () =
    let toHandler (m : MethodInfo) = m.CreateDelegate(typeof<Func<Message, Response>>) :?> Func<Message, Response>
    typeof<Marker>.DeclaringType.GetMethods()
    |> Seq.filter (fun m -> m.ReturnType = typeof<Response> && (m.GetParameters() |> Array.map (fun p -> p.ParameterType)) = [| typeof<Message> |])
    |> Seq.iter (fun m -> toHandler m |> (fun f -> fun msg -> f.Invoke msg) |> addMessageHandler)