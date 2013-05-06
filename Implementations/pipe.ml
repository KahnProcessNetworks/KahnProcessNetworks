(* Implementation with processes communicating through pipes. *****************)

open Unix
open Kahn

let trace =
	let i = ref 0 in
	fun s ->
		incr i;
		Format.printf "[l: %d, pid: %d] %s@." !i (getpid ()) s;
		()

module S : S =
struct
	type 'a process = (unit -> 'a)
	type 'a in_port = file_descr
	type 'a out_port = file_descr
	
	let new_channel () =
		trace "new_channel";
		pipe ()
		
	let put (v : 'a) (p : 'a out_port) () =
		trace "put";
		let c = out_channel_of_descr p in
		(** To improve **)
		Marshal.to_channel c (v : 'a) [ Marshal.Closures ];
		close_out c
	
	let rec get (p : 'a in_port) () =
		trace "get";
		let c = in_channel_of_descr p in
		trace "loop";
		(** To improve **)
		let v = ((Marshal.from_channel c) : 'a) in
		trace "end";
		close_in c;
		v
	
	let rec doco l () =
		trace "doco";
		match l with
		| [] -> ()
		| hd :: tl ->
			match fork () with
			|  0 ->
				trace "fork";
				hd ()
			|  pid ->
				trace "fork";
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
end
