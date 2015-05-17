[<AutoOpen>]
module Reflection

open System.Reflection
open System.Threading
open System

type Prop = Prop of PropertyInfo * obj

let types = AppDomain.CurrentDomain.GetAssemblies() |> Seq.collect (fun asm -> asm.GetTypes()) |> Seq.map (fun typ -> typ.FullName, typ) |> Map.ofSeq

module Property =
    let named name x =
        if x = null then sprintf "Cannot get property %s of a null value" name |> Left
        else 
            let prop = x.GetType().GetProperty(name, BindingFlags.Public ||| BindingFlags.Instance)
            if prop = null then sprintf "There is no public property %s on objects of type %A" name (x.GetType()) |> Left
            else Prop(prop, x) |> Right
    
    let get (Prop(info, o)) : Either<string, 'a> = 
        match info.GetValue(o) with
        | null       -> Right null
        | :? 'a as a -> Right a
        | _ -> sprintf "The property %s is expected to have the type %A, but has the type %A" info.Name typeof<'a> info.PropertyType |> Left

    let set x (Prop(info, o)) = 
        if info.PropertyType.IsAssignableFrom(x.GetType()) then info.SetValue(o, x) |> Right
        else sprintf "The property %s is expected to have the type %A, but has the type %A" info.Name (x.GetType()) info.PropertyType |> Left

module Method =    
    open System.Windows.Forms
    open System.Diagnostics
    open System

    let private invokeOnType (typ : Type) name (args : obj []) x : Either<string, 'a option> =
        let reqTypes = args |> Array.map (fun o -> o.GetType())
        if name = "new" then
            let cons = typ.GetConstructor(reqTypes)
            if typeof<'a>.IsAssignableFrom(typ) |> not then sprintf "The requested type constructor for %A does not match the requested object's type %A" typ typeof<'a> |> Left
            else if cons = null then sprintf "There is no public constructor for the type %A that takes parameters of types %A" typ reqTypes |> Left
            else cons.Invoke args :?> 'a |> Some |> Right
        else
            let meth = typ.GetMethod(name, reqTypes)
            if meth = null then sprintf "There is no public method %s on objects of type %A that takes parameters of types %A" name (x.GetType()) reqTypes |> Left
            else
                match meth.Invoke(x, args) with
                | :? 'a as a -> Some a |> Right
                | _  when typeof<'a> = typeof<obj> && meth.ReturnType = typeof<System.Void> -> Right None
                | _ -> sprintf "The method %s doesn't return a %A" meth.Name typeof<'a> |> Left

    let invoke name args x : Either<string, 'a option> =
        if x = null then sprintf "Cannot invoke the method %s on a null value" name |> Left
        else invokeOnType (x.GetType()) name args x

    let invokeStatic typeName methName args : Either<string, 'a option> = 
        let typ = Map.find typeName types
        if typ = null then sprintf "No type with the name %s could be found" typeName |> Left
        else invokeOnType typ methName args null

type Evt = Evt of EventInfo * obj

module Event =
    let named name x =
        if x = null then sprintf "Cannot get event %s of a null value" name |> Left
        else
            let evt = x.GetType().GetEvent(name, BindingFlags.Public ||| BindingFlags.Instance)
            if evt = null then sprintf "These is no public event %s on objects of type %A" name (x.GetType()) |> Left
            else Evt(evt, x) |> Right