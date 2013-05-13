(* Implementation simulating parallelism sequentially. ************************)

open Unix
open Kahn


(* Constant *******************************************************************)

let trace_enable = true


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


(* Main ***********************************************************************)

module S : S =
struct
  type 'a process = ('a -> unit) -> unit
  type 'a in_port = in_channel
  type 'a out_port = outchannel
  
  let new_channel () =
    failwith "new_channel not implemented"
  
  let put v c =
    failwith "put not implemented"
  
  let rec get c =
    failwith "get not implemented"
  
  let doco l =
    failwith "doco not implemented"
  
  let return v =
    failwith "return not implemented"
  
  let bind e e' =
    failwith "bind not implemented"
  
  let run e =
    failwith "run not implemented"
       
end
