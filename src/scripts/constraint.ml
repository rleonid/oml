
open Util

let f x y z = Float.(4. * x + 5. * y - z - 24.)

let iteration_limit = ref 10
let debug_ref = ref false 

let solve1 f ~start ~stop =
  let rec loop iter stop =
    if iter > !iteration_limit then
      let () = Printf.eprintf "passed iteration limit %d stop: %f" !iteration_limit stop in
      nan
    else
      let _ = Printf.printf "start: %f, stop: %f\n" stop in
      try 
        Solvers.bisection ~epsilon:1e-9 ~lower:start ~upper:stop f
      with Invalid_argument _ ->
        loop iter (stop *. 2.)
  in
  loop 0 stop

let solve2 f ~start ~stop =
  let start1,start2 = start in
  let stop1,stop2 = stop in
  Array.range ~start:start1 ~stop:stop1 () 
  |> Array.map (fun x -> x, solve1 ~start:start2 ~stop:stop2 (f x))
  
let solve3 f ~start ~stop =
  let start1,start2,start3 = start in
  let stop1,stop2,stop3 = stop in
  Array.range ~start:start1 ~stop:stop1 () 
  |> Array.map (fun x ->
      solve2 ~start:(start2,start3) ~stop:(stop2,stop3) (f x)
      |> Array.map (fun (y,z) -> (x,y,z)))
  |> Array.to_list
  |> Array.concat
 


