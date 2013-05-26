(* Parallel counting **********************************************************)

open Kahn


module Lib (K : S) =
struct
    let ( >>= ) x f =
        K.bind x f
    let delay f x =
        (K.return ()) >>= (fun () -> K.return (f x))
end

module Example (K : S) =
struct
    module Lib = Lib(K)
    open Lib
    
    let count : unit K.process =
        let rec loop n =
            (K.return ()) >>= (fun () -> Format.printf "%d@." n; loop (n + 1))
        in
        loop 0
    
    let parallel_count () : unit K.process =
        K.doco [ count; count; count; count; count; count; count; count; ]
        
    let main () : unit =
        K.run (parallel_count ())
end

module Exp = Example(Socket)

let () = Exp.main ()
