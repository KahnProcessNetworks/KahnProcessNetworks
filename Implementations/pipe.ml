(* Implementation with processes communicating through pipes. *****************)

open Unix
open Kahn


(* Constant *******************************************************************)

let trace_enable = false


(* Auxilliar function *********************************************************)

let trace =
	let i = ref 0 in
	fun s ->
		if (trace_enable)
		then
			begin
			incr i;
			Format.printf "// l: %d  pid: %d  f: %s@." !i (getpid ()) s
			end
		else ()


(* Implementation *************************************************************)

module S : S =
struct
	type 'a process = (unit -> 'a)
	type 'a in_port = in_channel
	type 'a out_port = out_channel
	
	let new_channel () =
		trace "new_channel";
		let (fd_in, fd_out) = pipe () in
		(in_channel_of_descr fd_in, out_channel_of_descr fd_out)
		
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
			|  0 ->
				trace "fork (child)";
				hd ()
			|  pid ->
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
end
