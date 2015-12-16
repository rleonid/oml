open Lacaml.D

module DefaultEncoded = struct
  type clas = string
  type feature = float
  let encoding x = [| x |]
  let size = 1
end
module DefaultLr = Classify.MulticlassLogisticRegression(DefaultEncoded)

let x = Data.Default.(default |> List.map (fun (_, r) -> [| 1.; r.balance |])) |> Array.of_list |> Mat.of_array
let y = Data.Default.(default |> List.map (fun (b,_) -> if not b then 1 else 2)) |> Array.of_list

let m = Mat.dim1 x
let n = Mat.dim2 x
let k = 2

let nmf = -1. /. float m
let lambda = 1e-4
let ld2 = lambda /. 2.
let ind = Mat.init_cols k m (fun r c -> if r = y.(c-1) then 1. else 0.)

let pct_correct perf =
  let correct =
    List.fold_left (fun c (a,p) -> if a = p then c + 1 else c) 0 perf
  in
  let total = List.length perf in
  (float correct) /. (float total)

let describe msg perf =
  Printf.printf "%s: %0.3f correct\n" msg (pct_correct perf)

let w = Softmax_regression.regress x y
let pct =
  Softmax_regression.classify_m w x
  |> Array.map Classify.most_likely
  |> Array.zip y
  |> Array.to_list
  |> pct_correct

(* Predicting 1 (aka false, no default) the more common class *)
let baseline =
  let n = Array.fold_left (fun c i -> if i = 1 then c + 1 else c) 0 y in
  let d = Array.length y in (* 10000 *)
  (float n) /. (float d)

let () =
  if pct > baseline then
    Printf.printf "meaningful! %0.3f vs %0.3f\n" pct baseline
  else
    Printf.printf "fail %0.3f vs %0.3f\n" pct baseline

let count_sorted lst =
  let rec loop acc = function
    | []     -> acc
    | h :: t ->
      match acc with
      | [] -> loop [(h, 1)] t
      | (ah,ac) :: at ->
        if ah = h
        then loop ((ah,ac + 1) :: at) t
        else loop ((h, 1) :: acc) t
  in
  loop [] lst
  |> List.rev

