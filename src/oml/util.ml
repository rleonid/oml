
type iterative_failure_reason =
    | OutOfBounds of float
    | NoConvergence
    | TooManyIterations of int
    | TooFewIterations of int

exception IterationFailure of string * iterative_failure_reason

let invalidArg fmt = Printf.ksprintf (fun s -> raise (Invalid_argument s)) fmt
let pi = 4. *. atan 1.

module Array = struct
  include Array

  let fold2 f i a b =
    let n = Array.length a
    and m = Array.length b in
    if n <> m then
      invalidArg "unequal lengths %d and %d" n m
    else
      begin
        let r = ref i in
        for i = 0 to n - 1 do
          r := f !r a.(i) b.(i)
        done;
        !r
      end

  let map2 f a b =
    let n = Array.length a
    and m = Array.length b in
    if n <> m then
      invalidArg "unequal lengths %d and %d" n m
    else
      Array.mapi (fun i a_i -> f a_i b.(i)) a

  let sumf = Array.fold_left (+.) 0.0

  (* TODO: Are there heuristics to consider when arrays are large? or elements
     in the array are particulary large/small. log transforms? *)
  let prodf = Array.fold_left ( *. ) 1.0

  let max a = Array.fold_left max a.(0) a
  let min a = Array.fold_left min a.(0) a

  let find_index f a =
    let n = Array.length a in
    let rec loop i =
      if i >= n then
        raise Not_found
      else if f a.(i) then
        i
      else
        loop (i + 1)
    in
    loop 0

  let binary_search c a =
    let rec bs_loop mi mx =
      if mx < mi then
        raise Not_found
      else
        let md = (mx + mi) / 2 in
        let cc = c a.(md) in
        if cc < 0
        then bs_loop mi (md - 1)
        else if cc > 0
            then bs_loop (md + 1) mx
            else a.(md)
    in
    bs_loop 0 (Array.length a - 1)

end

let midpoint x y = (x +. y) /. 2.0

