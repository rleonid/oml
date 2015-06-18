
open Util

type t =
  | Vector of float array
  | RowVector of float array
  | Matrix of float array array

let vector arr = Vector arr
let row_vector arr = RowVector arr
let matrix m = Matrix m

let rows_and_columns m = Array.length m, Array.length m.(0)

let dimensions = function
  | Vector r    -> Array.length r, 1
  | RowVector r -> 1, Array.length r
  | Matrix m    -> rows_and_columns m

let fold f i = function
  | Vector r    -> Array.fold_left f i r
  | RowVector r -> Array.fold_left f i r
  | Matrix m    -> Array.fold_left (Array.fold_left f) i m

let iter f = fold (fun () e -> f e) ()

(* This is written in an unconventional functional style. *)
type enumerator = unit -> float

let array_enumerator arr =
  let i = ref 0 in
  let s = Array.length arr in
  (fun () ->
    if !i >= s then i:= 0;
    let ret = arr.(!i) in
    incr i;
    ret)

let matrix_enumerator m =
  let rows, cols = rows_and_columns m in
  let ri = ref 0 in
  let ci = ref 0 in
  (fun () ->
    if !ri = rows then ri := 0;
    let ret = m.(!ri).(!ci) in
    if !ci = cols - 1 then (incr ri; ci := 0) else incr ci;
    ret)

(* Would this provide more overhead?
let array_visitor arr =
  let i = ref 0 in
  let s = Array.length arr in
  (fun f ->
    if !i >= s then i:= 0;
    f arr.(!i); 
    incr i)

let matrix_visitor m =
  let rows, cols = rows_and_columns m in
  let ri = ref 0 in
  let ci = ref 0 in
  (fun f ->
    if !ri = rows then ri := 0;
    f m.(!ri).(!ci);
    if !ci = cols - 1 then (incr ri; ci := 0) else incr ci) *)

let enumerator = function
  | Vector r    -> array_enumerator r 
  | RowVector r -> array_enumerator r
  | Matrix m    -> matrix_enumerator m

exception Not_equal

let if_same_size x y ~same ~fail =
  let rx,cx = dimensions x in
  let ry,cy = dimensions y in
  if rx = ry && cx = cy then
    same ()
  else
    fail rx ry cx cy
 
let if_same_size_invalidArg x y m ~same =
  if_same_size x y ~same
    ~fail:(fun rx cx ry cy ->
      invalidArg "Dimensional.%s: improper sizes [%d,%d] [%d,%d]"
                    m rx cx ry cy)
  
let equal ?d x y =
  if_same_size x y
    ~fail:(fun _ _ _ _ -> false)
    ~same:(fun () ->
      let y_enum  = enumerator y in
      let sig_dif = Util.significantly_different_from ?d in
      try
        iter (fun v_x ->
          if sig_dif v_x (y_enum ()) then raise Not_equal) x;
        true
      with Not_equal ->
        false)

let array_modifier arr =
  let i = ref 0 in
  let s = Array.length arr in
  (fun f ->
    if !i >= s then i:= 0;
    arr.(!i) <- f; 
    incr i)

let copy_cons = function
  | Vector r    -> 
      let copy = Array.copy r in
      let cons = array_modifier copy in
      cons, Vector copy
  | RowVector r ->
      let copy = Array.copy r in
      let cons = array_modifier copy in
      cons, RowVector copy
  | Matrix m    ->
      failwith "Not implemented"


let add x y =
  if_same_size_invalidArg x y "add"
    ~same:(fun () ->
      let cons, r = copy_cons y in
      let y_enum  = enumerator y in
      iter (fun v_x -> cons (v_x +. (y_enum ()))) x;
      r)

