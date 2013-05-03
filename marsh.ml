type t = int list

let write n =
  let rec make n acc = if n <= 0 then acc else make (n-1) (n::acc) in
  let l = make n [] in
  Marshal.to_channel stdout (l : t) [ Marshal.Closures ]

let read () =
    let l = ((Marshal.from_channel stdin) : t) in
    List.iter (fun x -> Format.printf "%d@." x) l

let () =
  let n = ref (-1) in
  let reader = ref false in
  Arg.parse
    [ "-writer", Arg.Set_int n,
      "<n> Write a list of n integers on the standard outpout" ;
      "-reader", Arg.Set reader,
      "Read a list of integer on the standard input" ]
    (fun _ -> ())
    (Sys.argv.(0)^" (-writer n | -reader)");
  if !reader then read ()
  else write !n
