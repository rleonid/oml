
let gamma = Ocephes.gamma
let ln_gamma = Ocephes.lgam

let ln_beta, beta =
  let beta x y = ln_gamma x +. ln_gamma y -. ln_gamma (x +. y) in
  beta, fun x y -> (exp (beta x y))

let rec regularized_beta ~alpha:a ~beta:b ?epsilon ?max_iterations =
  let get_b n x =
    if (n mod 2 = 0) then
      let m = float n /. 2.0 in
      (m *. (b -. m) *. x) /.
                            ((a +. (2. *. m) -. 1.) *. (a +. (2. *. m)))
  else
    let m = (float n -. 1.0) /. 2.0 in
    -.((a +. m) *. (a +. b +. m) *. x) /.
                                ((a +. (2. *. m)) *. (a +. (2. *. m) +. 1.0)) in
  let get_a n x = 1.0 in
  let log_beta = ln_beta a b in
  let fraction = Continued_fraction.init ~get_a ~get_b in fun x ->
    if Util.is_nan x || Util.is_nan a || Util.is_nan b ||
            x < 0.0 || x > 1.0 || a <= 0.0 || b <= 0.0 then nan
    else if (x > (a +. 1.) /. (2. +. b +. a) &&
                   1. -. x <= (b +. 1.) /. (2. +. b +. a))
    then 1. -. regularized_beta ~alpha:b ~beta:a ?epsilon ?max_iterations (1. -. x)
    else exp ((a *. log x) +. (b *. log1p (-.x)) -.
                log a -. log_beta) *.
                1.0 /. Continued_fraction.evaluate fraction ?epsilon ?max_iterations x

let erf = Ocephes.erf
let erfc = Ocephes.erfc

let gammap = Ocephes.igam
let gammaq = Ocephes.igamc

let chi_square_less chi_square num_observations =
  gammap ((float num_observations) /. 2.0) (chi_square /. 2.0)
let chi_square_greater chi_square num_observations =
  gammaq ((float num_observations) /. 2.0) (chi_square /. 2.0)

let t_lookup alpha_level dgf = failwith "Not implemented"

let softmax ?(temperature=1.0) weights =
  if Array.length weights = 0 then raise (Invalid_argument "weights") else
  if temperature = 0.0 then raise (Invalid_argument "temperature") else
    let weights = Array.map (fun w -> exp (w /. temperature)) weights in
    let sum = Array.fold_left (+.) 0.0 weights in
    Array.map (fun w -> w /. sum) weights
