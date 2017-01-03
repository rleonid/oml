
module FloatCompare = struct
  type t = float
  let compare (f1:float) f2 = compare f1 f2
end (* FloatCompare *)

open StdLabels
open MoreLabels
module FloatSet = Set.Make(FloatCompare)
module FloatMap = Map.Make(FloatCompare)
module Array = Oml_array

let list_map2_uneq_len ~f ~after =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [],     []      -> List.rev acc
    | [],     h2::t2  -> loop ((after h2) :: acc) [] t2
    | h1::t1, []      -> loop ((after h1) :: acc) t1 []
    | h1::t1, h2::t2  -> loop ((f h1 h2) :: acc)  t1 t2
  in
  loop []

let assoc_to_float_map lst =
  List.fold_left ~init:FloatMap.empty
    ~f:(fun m (x, y) -> FloatMap.add ~key:x ~data:y m)
    lst

module type PolynomialRepr = sig
  type t

  val eval : t -> (float -> float)

  val add : t -> t -> t

  val mult : t -> t -> t

end (* PolynomialRepr *)

module rec CoefficientRepr : sig
  (*include module type of PolynomialRepr *)
  include PolynomialRepr
  val of_list : float list -> t
  val to_point_value : ?points:float list -> t -> PointValueRepr.t

end = struct

  type t = float list
  let of_list x = x

  (* Use Horner's rule. *)
  (* let eval_coefficients_arr a =
    let n = Array.length a in
    if n = 0 then
      fun _ -> 0.
    else if n = 1 then
      fun _ -> a.(0)
    else
      fun x ->
      let rec loop i p =
        if i = n then p else loop (i + 1) (p *. x +. a.(i))
      in
      loop 1 a.(0) *)
  let eval_coefficients_lst = function
    | []     -> fun _ -> 0.
    | [a]    -> fun _ -> a
    | h :: t -> fun x ->
                  let rec loop a = function
                    | []      -> a
                    | v :: t  -> loop (a *. x +. v) t
                  in
                  loop h t

  let eval = eval_coefficients_lst

  (*let to_point_value_from_coeff_arr ?points a =
    let n = Array.length a in
    let xs =
      match points with
      | Some p -> p
      | None   -> Array.init n float
    in
    let ys = Array.map (eval_coefficients_arr a) xs in
    xs, ys  *)

  let to_point_value_from_coeff_lst ?points l =
    let eval = eval l in
    match points with
    | Some p ->
        List.fold_left p ~f:(fun fm x -> FloatMap.add ~key:x ~data:(eval x) fm)
          ~init:FloatMap.empty
    | None   ->
        List.fold_left l ~f:(fun (x, fm) _c ->
          (x +. 1., FloatMap.add ~key:x ~data:(eval x) fm))
          ~init:(0., FloatMap.empty)
          |> snd

  let to_point_value = to_point_value_from_coeff_lst

  (*let add_arr a1 a2 =
    let s1 = Array.length a1 in
    let s2 = Array.length a2 in
    let g1 i = if i >= s1 then 0. else a1.(i) in
    let g2 i = if i >= s2 then 0. else a2.(i) in
    Array.init (max s1 s2) (fun i -> g1 i +. g2 i) *)

  let add =
    list_map2_uneq_len ~f:(+.) ~after:(fun x -> x)

  (*let mult_arr a1 a2 =
    let s1 = Array.length a1 - 1 in
    let s2 = Array.length a2 - 1 in
    let g1 i = if i > s1 then 0. else a1.(i) in
    let g2 i = if i > s2 then 0. else a2.(i) in
    let c j =
      let rec sumc k s =
        if k > j then s else sumc (k + 1) (s +. (g1 k) *. (g2 (j - k)))
      in
      sumc 0 0.
    in
    Array.init (s1 + s2 + 1) c *)

  (* The choice of l1 to base the redundant case's lengths off of is arbitrary. *)
  let mult_lst l1 = function
    | []               -> List.map l1 ~f:(fun _ -> 0.)
    | c1 :: []         -> List.map l1 ~f:(fun x -> x *. c1)
    | (_  :: t2) as l2 ->
      let rec loop acc carry = function
        | []       -> List.rev_append acc carry
        | x1 :: t1 ->
            let ml2 = List.map l2 ~f:(( *. ) x1) in
            match list_map2_uneq_len ~f:(+.) ~after:(fun x -> x) ml2 carry with
            | []          -> assert false
            | h :: ncarry -> loop (h :: acc) ncarry t1
      in
      loop [] (List.map t2 ~f:(( *. ) 0.)) l1

  let mult = mult_lst

end (* CoefficientRepr *)

and PointValueRepr : sig
  include PolynomialRepr
  val of_list : (float * float) list -> t
  val points : t -> float list
end = struct

  type t = float FloatMap.t

  let of_list =
    List.fold_left ~init:FloatMap.empty ~f:(fun fm (x,y) ->
      if FloatMap.mem x fm then
        fm (* ignore dups *)
      else
        FloatMap.add ~key:x ~data:y fm)

  let points fm = List.map ~f:fst (FloatMap.bindings fm)

  (* Lagrange's formula. *)
  (* let eval_point_value_arr xs ys =
    let n = Array.length xs in
    if n = 0 then
      fun _ -> 0.
    else
      let prod x k =
        let x_k = xs.(k) in
        Array.fold_left (fun (i, n, d) x_i ->
          if i = k then (i + 1, n, d) else
            (* What if x is close to x_i ?*)
            i + 1, (x -. x_i) *. n, (x_k -. x_i) *. d)
          (0, 1., 1.)
          xs
      in
      fun x ->
        let prod = prod x in
        Array.fold_left (fun (k, s) y_k ->
          let _, n, d = prod k in
          k + 1, s +. y_k *. n /. d)
          (0, 0.)
          ys
        |> snd *)

  (*let eval_point_value_lst = function
    | []    -> fun _ -> 0.
    | [_,y] -> fun _ -> y
    | lst   ->
        let prod x x_k =
          List.fold_left lst ~init:(1., 1.) ~f:(fun (n,d) (x_i, _) ->
            (* OK to compare floats since they're both elements
              of the point value representation *)
            if x_i = x_k then
              (n, d)
            else
              (x -. x_i) *. n, (x_k -. x_i) *. d)
        in
        fun x ->
          let prod = prod x in
          List.fold_left lst ~init:0. ~f:(fun s (x_k, y_k) ->
            let n, d = prod x_k in
            s +. y_k *. n /. d) *)

  let eval fm =
    let n = FloatMap.cardinal fm in
    if n = 0 then
      fun _ -> 0.
    else if n = 1 then
      let _k, v = FloatMap.choose fm in
      fun _ -> v
    else
      let prod x x_k =
        FloatMap.fold fm ~init:(1., 1.) ~f:(fun ~key:x_i ~data:_ (n,d) ->
          (* OK to compare floats since they're both elements of the point
             value representation. *)
          if x_i = x_k then
            (n, d)
          else
            (x -. x_i) *. n, (x_k -. x_i) *. d)
      in
      fun x ->
        let prod = prod x in
        FloatMap.fold fm ~init:0. ~f:(fun ~key:x_k ~data:y_k s ->
          let n, d = prod x_k in
          s +. y_k *. n /. d)

  let add fm1 fm2 =
    let eval1 = eval fm1 in
    let eval2 = eval fm2 in
    let n1 = FloatMap.cardinal fm1 in
    let n2 = FloatMap.cardinal fm2 in
    (* To make sure that we return only [max n1 n2] elements *)
    let disregard_first = n1 < n2 in
    let disregard_second = n1 > n2 in
    FloatMap.merge fm1 fm2 ~f:(fun x y1o y2o ->
      match y1o, y2o with
      | None,    None                          -> assert false (* or None? *)
      | Some _,  None    when disregard_first  -> None
      | Some y1, None                          -> Some (y1 +. eval2 x)
      | None,    Some _  when disregard_second -> None
      | None,    Some y2                       -> Some (y2 +. eval1 x)
      | Some y1, Some y2                       -> Some (y1 +. y2))

  let mult fm1 fm2 =
    let eval1 = eval fm1 in
    let eval2 = eval fm2 in
    let n1 = FloatMap.cardinal fm1 in
    let n2 = FloatMap.cardinal fm2 in
    (* To make sure that we return only [max n1 n2] elements *)
    let required_size = ref ((n1 - 1) * (n2 - 1) + 1) in
    let m =
      FloatMap.merge fm1 fm2 ~f:(fun x y1o y2o ->
        if !required_size <= 0 then
          None
        else
          match y1o, y2o with
          | None,    None     -> assert false (* or None? *)
          | Some y1, None     -> Some (y1 *. eval2 x)
          | None,    Some y2  -> Some (y2 *. eval1 x)
          | Some y1, Some y2  -> Some (y1 *. y2))
    in
    let x, _v = FloatMap.max_binding m in
    let rec loop x m =
      if !required_size <= 0 then
        m
      else begin
        decr required_size;
        loop (x +. 1.) (FloatMap.add ~key:x ~data:(eval1 x +. eval2 x) m)
      end
    in
    loop (x +. 1.) m

end (* PointValueRepr *)

type t =
  | Coefficients of CoefficientRepr.t
  | PointValue of PointValueRepr.t

let eval = function
  | Coefficients a -> CoefficientRepr.eval a
  | PointValue pv  -> PointValueRepr.eval pv

let add p1 p2 =
  match p1, p2 with
  | Coefficients a1, Coefficients a2  ->
      Coefficients (CoefficientRepr.add a1 a2)
  | PointValue p1, Coefficients a2    ->
      let points = PointValueRepr.points p1 in
      let p2 = CoefficientRepr.to_point_value ~points a2 in
      PointValue (PointValueRepr.add p1 p2)
  | Coefficients a1, PointValue p2    ->
      let points = PointValueRepr.points p2 in
      let p1 = CoefficientRepr.to_point_value ~points a1 in
      PointValue (PointValueRepr.add p1 p2)
  | PointValue p1, PointValue p2      ->
      PointValue (PointValueRepr.add p1 p2)

let complex_roots n =
  let fn = float n in
  let p2 = Oml_util_base.pi *. 2. in
  Array.init n (fun i ->
    let fi = float i in
    Complex.polar 1. (p2 *. fi /. fn))

let indices_lst n =
  let rec loop a1 a2 i =
    if i = n then
      a1, a2
    else
      if i mod 2 = 0 then
        loop (i :: a1) a2 (i + 1)
      else
        loop a1 (i :: a2) (i + 1)
  in
  loop [] [] 0

