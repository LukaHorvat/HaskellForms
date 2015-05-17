[<AutoOpen>]
module Common

open System
open System.Threading
open System.Windows.Forms

module Thread =
    let inNewThread fn =
        let thread = new Thread(fun () -> fn() |> ignore)
        thread.Start()
        thread

    let byControl (ctrl : Control) (fn : 'a -> 'b) (a : 'a) = ctrl.Invoke (Func<'a, 'b>(fn), [|a :> obj|]) :?> 'b

module Ref =
    let rec waitOnRef pred (r : 'a Ref) =
        if pred r.Value then r.Value
        else Thread.Sleep 10
             waitOnRef pred r

module Option =
    let getOrDefault a opt =
        match opt with
        | None   -> a
        | Some b -> b
    let unpack def f o =
        match o with
        | Some a -> f a
        | None   -> def

type OptionBuilder() =
    member x.Bind(v,f) = Option.bind f v
    member x.Return v = Some v
    member x.ReturnFrom o = o

let opt = OptionBuilder()

type Either<'a, 'b> = Left of 'a | Right of 'b 

type EitherBuilder() =
    member x.Bind(e, f) =
        match e with
        | Right a  -> f a
        | Left err -> Left err
    member x.Return a = Right a
    member x.ReturnFrom e = e

let eith = EitherBuilder()

module Either =
    let bind f eab = eith.Bind(eab, f)
    let map f eab =
        match eab with
        | Right a -> f a |> Right
        | Left e  -> Left e
    let getOrErr e =
        match e with
        | Right a -> a
        | Left  e -> failwith e
    let getOrDefault a e =
        match e with
        | Right b -> b
        | _       -> a
    let filterWith pred v e =
        match e with
        | Left e              -> Left e
        | Right a when pred a -> Right a
        | _                   -> Left v
    let unpack fe fv e =
        match e with
        | Left er -> fe er
        | Right v -> fv v
    let ofOption err o =
        match o with
        | Some a -> Right a
        | None   -> Left err
    let lift = map
    let lift2 f e1 e2 = eith {
        let! ev1 = e1
        let! ev2 = e2
        return f ev1 ev2
    }
    let lift3 f e1 e2 e3 = eith {
        let! ev1 = e1
        let! ev2 = e2
        let! ev3 = e3
        return f ev1 ev2 ev3
    }
    let lift4 f e1 e2 e3 e4 = eith {
        let! ev1 = e1
        let! ev2 = e2
        let! ev3 = e3
        let! ev4 = e4
        return f ev1 ev2 ev3 ev4
    }
    let cast e : Either<'a, 'b> =
        match e with
        | Left err -> Left err
        | Right a  -> box a :?> 'b |> Right
    let rec sequence es = 
        if Seq.isEmpty es then Right []
        else eith {
            let! x = Seq.head es
            let! xs = Seq.skip 1 es |> sequence
            return x :: xs
        }

let const' x _ = x

let flip f x y = f y x

module List =
    let rec skip n l =
        match n, l with
        | _, [] -> []
        | 0, x  -> x
        | n, _ :: xs -> skip (n - 1) xs
    let rec skipWhile p l =
        match l with
        | [] -> []
        | x :: xs when p x -> skipWhile p xs
        | _ -> l
    let splitWhile p l = (List.takeWhile p l, skipWhile p l)