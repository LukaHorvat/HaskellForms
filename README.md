# HaskellForms
WinForms bindings for Haskell

### What
This library provides a nice interface for a subset of Windows Forms.

### How
There is a .NET executable that's ran by the library. It handles the creation and manipulation of the actual controls.
It takes commands through standard input from the Haskell side of the library and it returns information, as well as event
data, through standard output.

The library wraps this communication layer in a nice interface, and it takes care of data marshaling.
Only simple types are transmitted. Controls are given IDs so that they can be referred to in commands.

### Why
Because the Windows GUI in Haskell situation is bad at the moment. This will hopefuly turn out to be a decent
alterntive. I'm also hoping that it will work, through Mono, on other platforms.

### Communication API
There's no real need to know the protocol, but it's documented here anyways.
`Messages` and `Responses` are transmited as strings. Both are union types (owned by F# on the .NET side and Haskell on 
the other side) so their serialization is relatively trivial. 
It's not much more than a preorder of the tree that represents the message object. So this command:
```
Set 0 BackColor Color 255 255 0 255
```
is actually the string form of `Set 0 "BackColor" (Color 255 255 0 255)`

These are the relevant types
```
type Value = Int of int | String of string | Color of Color | Point of Point | Size of Size | Font of Font
type Message = New of string | AddChildControl of int * int | Set of int * string * Value | Get of int * string
type Response = Pass | NoResponse | ControlId of int | Error of string | Value of Value
```

The base types (`int`, `float`, `byte`, `string`, `bool`) are serialized normally (strings are wrapped in quotation marks).
Color, Point, Size and Font are serialized as (Int, Int, Int, Int), (Int, Int), (Int, Int) and (String, Float) respectively.

The API is designed with error handling in mind so the first error that comes up in the handling of a message gets returned-
