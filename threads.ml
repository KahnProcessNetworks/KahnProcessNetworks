(* Implementation with processes communicating through threads. ***************)

open Miscellaneous
open Thread
open Unix


type 'a process = (unit -> 'a)
type 'a channel = { q: 'a Queue.t; m: Mutex.t; }
type 'a in_port = 'a channel
type 'a out_port = 'a channel

let new_channel () =
    trace "new_channel";
    let q = { q = Queue.create (); m = Mutex.create (); } in
    q, q

let put v c () =
    trace "put";
    Mutex.lock c.m;
    Queue.push v c.q;
    Mutex.unlock c.m;
    Thread.yield ()

let rec get c () =
    trace "get";
    try
        Mutex.lock c.m;
        let v = Queue.pop c.q in
        Mutex.unlock c.m;
        v
    with
        Queue.Empty ->
            Mutex.unlock c.m;
            Thread.yield ();
            get c ()

let doco l () =
    trace "doco";
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

let return v =
    trace "return";
    fun () -> v

let bind e e' () =
    trace "bind";
    let v = e () in
    Thread.yield ();
    e' v ()

let run e =
    trace "run";
    e ()
