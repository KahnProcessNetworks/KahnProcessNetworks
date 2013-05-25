(* Implementation with processes communicating through pipes. *****************)

open Miscellaneous
open Unix


type 'a process = (unit -> 'a)
type 'a in_port = in_channel
type 'a out_port = out_channel

let new_channel () =
    trace "new_channel";
    let (in_file_descr, out_file_descr) = pipe () in
    let in_channel = in_channel_of_descr in_file_descr in
    let out_channel = out_channel_of_descr out_file_descr in
    (in_channel, out_channel)
    
let put (v : 'a) (c : 'a out_port) () =
    trace "put";
    Marshal.to_channel c (v : 'a) [ Marshal.Closures ]

let rec get (c : 'a in_port) () =
    trace "get";
    let v = ((Marshal.from_channel c) : 'a) in
    v

let rec doco l () =
    trace "doco";
    match l with
    | [] -> ()
    | hd :: tl ->
        match fork () with
        | 0 ->
            trace "fork (child)";
            hd ()
        | pid ->
            trace "fork (father)";
            doco tl ();
            let _ = wait () in
            ()

let return v =
    trace "return";
    fun () -> v

let bind e e' () =
    trace "bind";
    let v = e () in
    e' v ()

let run e =
    trace "run";
    e ()
