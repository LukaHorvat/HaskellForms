[<AutoOpen>]
module Reflection

open System.Reflection
open System.Threading

type Prop = Prop of PropertyInfo * obj

module Property =
    let named name x =
        let prop = x.GetType().GetProperty(name, BindingFlags.Public ||| BindingFlags.Instance)
        if prop = null then sprintf "There is no public property %s on objects of type %A" name (x.GetType()) |> Left
        else Prop(prop, x) |> Right
    
    let get (Prop(info, o)) : Either<string, 'a> = 
        match info.GetValue(o) with
        | :? 'a as a -> Right a
        | _ -> sprintf "The property %s is expected to have the type %A, but has the type %A" info.Name typeof<'a> info.PropertyType |> Left

    let set x (Prop(info, o)) = 
        if info.PropertyType.IsAssignableFrom(x.GetType()) then info.SetValue(o, x) |> Right
        else sprintf "The property %s is expected to have the type %A, but has the type %A" info.Name (x.GetType()) info.PropertyType |> Left

module Method =    
    open System.Windows.Forms
    open System.Diagnostics

    let invoke name (args : obj []) x : Either<string, 'a option> =
        let reqTypes = args |> Array.map (fun o -> o.GetType())
        let meth = x.GetType().GetMethod(name, reqTypes)
        if meth = null then sprintf "There is no public method %s on objects of type %A that takes parameters of types %A" name (x.GetType()) reqTypes |> Left
        else
            match meth.Invoke(x, args) with
            | :? 'a as a -> Some a |> Right
            | _  when typeof<'a> = typeof<obj> && meth.ReturnType = typeof<System.Void> -> Right None
            | _ -> sprintf "The method %s doesn't return a %A" meth.Name typeof<'a> |> Left