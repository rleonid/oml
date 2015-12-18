(* To build exec
ocamlbuild -use-ocamlfind -pkgs lacaml,ocephes,lbfgs -I src/scripts/ -I src/lib/ -I src/lib/datasets softmax_performance.native
*)

open Lacaml.D
open Util

let time s f =
  let n = Sys.time () in
  let r = f () in
  Printf.printf "%-10s:%f\n" s (Sys.time () -. n);
  r

(* Use Mnist
   if in repl
   #require "dsfo"
   if building
    ocamlbuild -use-ocamlfind -pkgs dsfo
let m_train = Mnist.data ~dir:"../dsfo" `Train

let training_width = 28 * 28
let k = 10  (* classes *)

let x = Mat.transpose ~m:training_width m_train
let y =
  let find_index c =
    let rec loop i = if c.{i + training_width} = 1.0 then i else loop (i + 1) in
    loop 1
  in
  Array.init (Mat.dim2 m_train) (fun i -> let j = i + 1 in find_index (Mat.col m_train j))
*)

(* Use Default *)
let x =
  Data.Default.(default
                |> List.map (fun (_, r) ->
                    [| 1.; if r.student then 1. else 0. ;r.balance; r.income |]))
  |> Array.of_list
  |> Mat.of_array
let y =
  Data.Default.(default
                |> List.map (fun (b,_) -> if not b then 1 else 2))
  |> Array.of_list
let k = 2
let training_width = 4

let gren = Softmax_regression.general_eval_and_grad ~newmethod:true ~lambda:0.0 k x y
let greo = Softmax_regression.general_eval_and_grad ~newmethod:false ~lambda:0.0 k x y

let test n =
  let a = training_width * k in
  let g = Vec.make0 a in
  let ws = Array.init n (fun _ -> Vec.random a) in
  let nres = time "new method" (fun () -> Array.map (fun w -> gren w g) ws) in
  let ores = time "old method" (fun () -> Array.map (fun w -> greo w g) ws) in
  ores, nres

let compare (ores, nres) =
  Array.zip ores nres |> Array.all (fun (o, n) -> equal_floats ~d:1e-5 o n)

let () =
  if not (!Sys.interactive) then begin
    let runs  =
      if Array.length Sys.argv < 2 then
        10
      else
        int_of_string Sys.argv.(1)
    in
    Printf.printf "%d runs: %b \n" runs (compare (test runs))
  end
