

let average_ties_f n arr comp =
  let rec loop i p rm rn same =
    let assign j =
      let r,v,p = arr.(j) in
      arr.(j) <- rm, v, p
    in
    let label_ties c = List.iter assign same; c () in
    if i = n then
      label_ties (fun _ -> ())
    else
      let (r_i, v_i, _) = arr.(i) in
      if comp v_i p = 0 then
        let nn = rn +. 1.0 in
        let nm = rm +. (r_i -. rm) /. nn in
        let nsame = match same with [] -> [i;i-1] | _ -> (i :: same) in
        loop (i + 1) v_i nm nn nsame
      else
        label_ties (fun () -> loop (i + 1) v_i r_i 1.0 [])
  in
  loop 0 nan nan nan []

let ranks ?(start=1) ?(average_ties=false) ?(compare=compare) arr =
  (* TODO: This method can be improved upon if we write our own in place
     array sorter, that can operate on both arrays at the same time. *)
  let pos = Array.mapi (fun i v -> v, i) arr in
  Array.sort (fun (v1,_) (v2,_) -> compare v1 v2) pos;
  let rnk = Array.mapi (fun i (v, p) -> (float (start + i), v, p)) pos in
  let len = Array.length arr in
  if average_ties then average_ties_f len rnk compare;
  let res = Array.make len 0.0 in
  Array.iter (fun (rank, _, pos) -> res.(pos) <- rank) rnk;
  res
