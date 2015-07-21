
open Util

type 'a generator = unit -> 'a

let init = function
  | None   -> Random.State.make_self_init ()
  | Some a -> Random.State.make a

let int_upper_bound = 2 lsl (30 - 1)

let uniform_i ?seed n =
  if n <= 0 || n >= int_upper_bound then raise (Invalid_argument "n") else
  let r = init seed in
  fun () -> Random.State.int r n

let uniform_f ?seed n =
  if n <= 0.0 then raise (Invalid_argument "n") else
  let r = init seed in
  fun () -> Random.State.float r n

let normal_std ?seed () =
  let r = init seed in
  let cur_ref = ref None in
  (fun () ->
    let p = fun () -> 2.0 *. (Random.State.float r 1.0) -. 1.0 in
    let rec loop v1 v2  =
      let rsq = v1 *. v1 +. v2 *. v2 in
      if rsq >= 1.0 || rsq = 0.0 then
        loop (p ()) (p ())
      else
        let fac = sqrt ( -2.0 *. (log rsq) /. rsq) in
        cur_ref := Some (v2 *. fac) ;
        v1 *. fac
    in
    match !cur_ref with
    | Some x -> (cur_ref := None; x)
    | None   -> loop (p ()) (p ()))

let normal ?seed ~mean ~std () =
  if std < 0.0 then raise (Invalid_argument "std") else
  if std = 0.0 then (fun () -> mean) else
  let rsn = normal_std ?seed () in
  (fun () -> std *. (rsn ()) +. mean)

let multinomial ?seed weights =
  if not (Array.all (within (Open 0.0, Closed 1.0)) weights) then
    raise (Invalid_argument "weights")
  else
    let sum = Array.sumf weights in
    if significantly_different_from 1.0 sum then
      raise (Invalid_argument "weights")
    else
      let r = init seed in
      let n = Array.length weights - 1 in
      (fun () ->
        let threshold = Random.State.float r 1.0 in
        let rec iter i sum =
          if i = n then i else
            let sum' = sum +. weights.(i) in
            if sum' >= threshold then i else iter (i+1) sum' in
        iter 0 0.0)

let softmax ?seed ?temp weights =
  let f = Functions.softmax ?temp weights in
  multinomial ?seed (Array.init (Array.length weights) f)

module Poly =
  struct
    let uniform ?seed elems =
      let f = uniform_i ?seed (Array.length elems) in
      fun () -> elems.(f())

    let multinomial ?seed elems weights =
      if (Array.length elems != Array.length weights) then
        raise (Invalid_argument "weights") else
          let f = multinomial ?seed weights in
          fun () -> elems.(f())

    let softmax ?seed ?temp elems weights =
      if (Array.length elems != Array.length weights) then
        raise (Invalid_argument "weights") else
          let f = softmax ?seed weights in
          fun () -> elems.(f())
  end
