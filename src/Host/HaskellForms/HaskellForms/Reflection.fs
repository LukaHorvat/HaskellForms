[<AutoOpen>]
module Reflection

open System.Reflection

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