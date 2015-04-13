
open Util

(* ln of the gamma function for x > 0
  As of 2015-04-10, I can't find the exact source for this algorithm.
  http://www.rskey.org/CMS/index.php/the-library/11 claims that this was the
  version of Lanczos approximation originally given in Numerical Recipies.
  Since, their code is not free, we should replace this with a different
  implementation.
 *)
let ln_gamma x =
  if x <= 0.0 then
    invalidArg "Invalid argument (=< 0) passed to ln_gamma: %f" x
  else
    let xp55 = x +. 5.5 in
    let cons = -1.0 *. xp55 +. (x +. 0.5) *. (log xp55) in
    let sum = 1.000000000190015
         +.  76.18009172947146 /. ( x +. 1.0)
         +. -86.50532032941677 /. ( x +. 2.0)
         +.  24.01409824083091 /. ( x +. 3.0)
         +.  -1.231739572450155 /. ( x +. 4.0)
         +.   0.1208650973866179e-2 /. ( x +. 5.0)
         +.  -0.5395239384953e-5 /. ( x +. 6.0)
    in
    cons +. log ((sqrt (2.0 *. pi)) *. sum /. x)

let ln_beta_func, beta_func =
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
  let log_beta = ln_beta_func a b in
  let fraction = Continued_fraction.init ~get_a ~get_b in fun x ->
    if Util.is_nan x || Util.is_nan a || Util.is_nan b ||
            x < 0.0 || x > 1.0 || a <= 0.0 || b <= 0.0 then nan
    else if (x > (a +. 1.) /. (2. +. b +. a) &&
                   1. -. x <= (b +. 1.) /. (2. +. b +. a))
    then 1. -. regularized_beta ~alpha:b ~beta:a ?epsilon ?max_iterations (1. -. x)
    else exp ((a *. log x) +. (b *. log1p (-.x)) -.
                log a -. log_beta) *.
                1.0 /. Continued_fraction.evaluate fraction ?epsilon ?max_iterations x

(* Incomplete regularized gamma function P(a, x) *)
let gammap, gammaq =
  let check a x f =
    if x < 0.0 || a <= 0.0
    then invalidArg "Invalid arguments passed to gammap, less than zero. a: %f x: %f" a x
    else f ()
  in
  let eps = 3.0e-7 in
  let max_iterations = 200 in
  (* series computation. *)
  let gser a x =
    let gln = ln_gamma a in
    let rec loop iterations_to_go ap del sum =
      if iterations_to_go = 0 then
        raise (IterationFailure ("gamma", TooManyIterations max_iterations))
      else
        let ap  = ap +. 1.0 in
        let del = del *. (x /. ap) in
        let sum = sum +. del in
        if abs_float (del) < (abs_float sum) *. eps then
          sum *. exp (-1.0 *. x +. a *. (log x) -. gln)
        else
          loop (iterations_to_go - 1) ap del sum
    in
    let a_inv = 1.0 /. a in
    loop max_iterations a a_inv a_inv
  in
  (* continued fraction representation. *)
  let fp_min = 1.0e-30 in
  let gcnf a x =
    let gln = ln_gamma a in
    let rec loop iterations b c d h =
      if iterations > float max_iterations then
        raise (IterationFailure ("gamma", TooFewIterations max_iterations))
      else
        let an = -1.0 *. iterations *. (iterations -. a) in
        let b = 2.0 +. b in
        let d = an *. d +. b in
        let d = if abs_float d < fp_min then fp_min else d in
        let c = b +. an /. c in
        let c = if abs_float c < fp_min then fp_min else c in
        let d = 1.0 /. d in
        let del = d *. c in
        let h = h *. del in
        if abs_float (del -. 1.0) < eps then
          h *. (exp (-1.0 *. x +. a *. (log x) -. gln))
        else
          loop (iterations +. 1.0) b c d h
    in
    let b_i = x +. 1.0 -. a in
    let d_i = 1.0 /. b_i in
    loop 1.0 b_i (1.0 /. fp_min) d_i d_i
  in
  (* gammap *)
  (fun a x -> check a x
    (fun () -> if x < a +. 1.0 then gser a x else 1.0 -.  (gcnf a x))),
  (* gammaq *)
  (fun a x -> check a x
    (fun () -> if x < a +. 1.0 then 1.0 -. (gser a x) else gcnf a x))

(* Taylor series computation of the error function. *)
let erf_taylor x iter =
  let range i n = Array.init (n - i + 1) (fun j -> i + j) in
  let x2    = x ** 2.0 in
  let (s,_) =
    Array.fold_left
      (fun (sum, p) n ->
          let p1 = if n < 1
                   then 1.0
                   else p *. -1.0 *. x2 /. (float n)
          in
          (sum +. x /. float (2*n + 1) *. p1, p1)
          )
        (0.0, 1.0)
        (range 0 iter)
  in
  (2. /. (sqrt pi)) *. s

let erf x = if x < 0.0 then -1.0 *. (gammap 0.5 (x *. x)) else gammap 0.5 (x *. x)
let erfc x = if x < 0.0 then 1.0 +. (gammap 0.5 (x *. x)) else gammaq 0.5 (x *. x)

let chi_square_less chi_square num_observations =
  gammap ((float num_observations) /. 2.0) (chi_square /. 2.0)
let chi_square_greater chi_square num_observations =
  gammaq ((float num_observations) /. 2.0) (chi_square /. 2.0)

let t_lookup alpha_level dgf =
  let alpha_table = [| 0.40; 0.25; 0.10; 0.05; 0.04; 0.025; 0.02; 0.01
                      ; 0.005; 0.0025; 0.001; 0.0005 |]
  in
  let dgf_table = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16
                    ; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30
                    ; 31; 32; 33;  34; 35; 36; 37; 38; 39; 40; 60; 80; 100
                    ; 120; 140; 160; 180; 200; 250; max_int
                    |]
  in
  let dgf_index = Array.find_index (fun x -> x >= dgf) dgf_table in
  let alpha_index = Array.find_index (fun x -> x <= alpha_level) alpha_table in
  let table = [|
      [| 0.325; 1.000; 3.078; 6.314; 7.916; 12.706; 15.894; 31.821; 63.656; 127.321; 318.289; 636.578 |];
      [| 0.289; 0.816; 1.886; 2.920; 3.320; 4.303; 4.849; 6.965; 9.925; 14.089; 22.328; 31.600 |];
      [| 0.277; 0.765; 1.638; 2.353; 2.605; 3.182; 3.482; 4.541; 5.841; 7.453; 10.214; 12.924 |];
      [| 0.271; 0.741; 1.533; 2.132; 2.333; 2.776; 2.999; 3.747; 4.604; 5.598; 7.173; 8.610 |];
      [| 0.267; 0.727; 1.476; 2.015; 2.191; 2.571; 2.757; 3.365; 4.032; 4.773; 5.894; 6.869 |];
      [| 0.265; 0.718; 1.440; 1.943; 2.104; 2.447; 2.612; 3.143; 3.707; 4.317; 5.208; 5.959 |];
      [| 0.263; 0.711; 1.415; 1.895; 2.046; 2.365; 2.517; 2.998; 3.499; 4.029; 4.785; 5.408 |];
      [| 0.262; 0.706; 1.397; 1.860; 2.004; 2.306; 2.449; 2.896; 3.355; 3.833; 4.501; 5.041 |];
      [| 0.261; 0.703; 1.383; 1.833; 1.973; 2.262; 2.398; 2.821; 3.250; 3.690; 4.297; 4.781 |];
      [| 0.260; 0.700; 1.372; 1.812; 1.948; 2.228; 2.359; 2.764; 3.169; 3.581; 4.144; 4.587 |];
      [| 0.260; 0.697; 1.363; 1.796; 1.928; 2.201; 2.328; 2.718; 3.106; 3.497; 4.025; 4.437 |];
      [| 0.259; 0.695; 1.356; 1.782; 1.912; 2.179; 2.303; 2.681; 3.055; 3.428; 3.930; 4.318 |];
      [| 0.259; 0.694; 1.350; 1.771; 1.899; 2.160; 2.282; 2.650; 3.012; 3.372; 3.852; 4.221 |];
      [| 0.258; 0.692; 1.345; 1.761; 1.887; 2.145; 2.264; 2.624; 2.977; 3.326; 3.787; 4.140 |];
      [| 0.258; 0.691; 1.341; 1.753; 1.878; 2.131; 2.249; 2.602; 2.947; 3.286; 3.733; 4.073 |];
      [| 0.258; 0.690; 1.337; 1.746; 1.869; 2.120; 2.235; 2.583; 2.921; 3.252; 3.686; 4.015 |];
      [| 0.257; 0.689; 1.333; 1.740; 1.862; 2.110; 2.224; 2.567; 2.898; 3.222; 3.646; 3.965 |];
      [| 0.257; 0.688; 1.330; 1.734; 1.855; 2.101; 2.214; 2.552; 2.878; 3.197; 3.610; 3.922 |];
      [| 0.257; 0.688; 1.328; 1.729; 1.850; 2.093; 2.205; 2.539; 2.861; 3.174; 3.579; 3.883 |];
      [| 0.257; 0.687; 1.325; 1.725; 1.844; 2.086; 2.197; 2.528; 2.845; 3.153; 3.552; 3.850 |];
      [| 0.257; 0.686; 1.323; 1.721; 1.840; 2.080; 2.189; 2.518; 2.831; 3.135; 3.527; 3.819 |];
      [| 0.256; 0.686; 1.321; 1.717; 1.835; 2.074; 2.183; 2.508; 2.819; 3.119; 3.505; 3.792 |];
      [| 0.256; 0.685; 1.319; 1.714; 1.832; 2.069; 2.177; 2.500; 2.807; 3.104; 3.485; 3.768 |];
      [| 0.256; 0.685; 1.318; 1.711; 1.828; 2.064; 2.172; 2.492; 2.797; 3.091; 3.467; 3.745 |];
      [| 0.256; 0.684; 1.316; 1.708; 1.825; 2.060; 2.167; 2.485; 2.787; 3.078; 3.450; 3.725 |];
      [| 0.256; 0.684; 1.315; 1.706; 1.822; 2.056; 2.162; 2.479; 2.779; 3.067; 3.435; 3.707 |];
      [| 0.256; 0.684; 1.314; 1.703; 1.819; 2.052; 2.158; 2.473; 2.771; 3.057; 3.421; 3.689 |];
      [| 0.256; 0.683; 1.313; 1.701; 1.817; 2.048; 2.154; 2.467; 2.763; 3.047; 3.408; 3.674 |];
      [| 0.256; 0.683; 1.311; 1.699; 1.814; 2.045; 2.150; 2.462; 2.756; 3.038; 3.396; 3.660 |];
      [| 0.256; 0.683; 1.310; 1.697; 1.812; 2.042; 2.147; 2.457; 2.750; 3.030; 3.385; 3.646 |];
      [| 0.256; 0.682; 1.309; 1.696; 1.810; 2.040; 2.144; 2.453; 2.744; 3.022; 3.375; 3.633 |];
      [| 0.255; 0.682; 1.309; 1.694; 1.808; 2.037; 2.141; 2.449; 2.738; 3.015; 3.365; 3.622 |];
      [| 0.255; 0.682; 1.308; 1.692; 1.806; 2.035; 2.138; 2.445; 2.733; 3.008; 3.356; 3.611 |];
      [| 0.255; 0.682; 1.307; 1.691; 1.805; 2.032; 2.136; 2.441; 2.728; 3.002; 3.348; 3.601 |];
      [| 0.255; 0.682; 1.306; 1.690; 1.803; 2.030; 2.133; 2.438; 2.724; 2.996; 3.340; 3.591 |];
      [| 0.255; 0.681; 1.306; 1.688; 1.802; 2.028; 2.131; 2.434; 2.719; 2.990; 3.333; 3.582 |];
      [| 0.255; 0.681; 1.305; 1.687; 1.800; 2.026; 2.129; 2.431; 2.715; 2.985; 3.326; 3.574 |];
      [| 0.255; 0.681; 1.304; 1.686; 1.799; 2.024; 2.127; 2.429; 2.712; 2.980; 3.319; 3.566 |];
      [| 0.255; 0.681; 1.304; 1.685; 1.798; 2.023; 2.125; 2.426; 2.708; 2.976; 3.313; 3.558 |];
      [| 0.255; 0.681; 1.303; 1.684; 1.796; 2.021; 2.123; 2.423; 2.704; 2.971; 3.307; 3.551 |];
      [| 0.254; 0.679; 1.296; 1.671; 1.781; 2.000; 2.099; 2.390; 2.660; 2.915; 3.232; 3.460 |];
      [| 0.254; 0.678; 1.292; 1.664; 1.773; 1.990; 2.088; 2.374; 2.639; 2.887; 3.195; 3.416 |];
      [| 0.254; 0.677; 1.290; 1.660; 1.769; 1.984; 2.081; 2.364; 2.626; 2.871; 3.174; 3.390 |];
      [| 0.254; 0.677; 1.289; 1.658; 1.766; 1.980; 2.076; 2.358; 2.617; 2.860; 3.160; 3.373 |];
      [| 0.254; 0.676; 1.288; 1.656; 1.763; 1.977; 2.073; 2.353; 2.611; 2.852; 3.149; 3.361 |];
      [| 0.254; 0.676; 1.287; 1.654; 1.762; 1.975; 2.071; 2.350; 2.607; 2.847; 3.142; 3.352 |];
      [| 0.254; 0.676; 1.286; 1.653; 1.761; 1.973; 2.069; 2.347; 2.603; 2.842; 3.136; 3.345 |];
      [| 0.254; 0.676; 1.286; 1.653; 1.760; 1.972; 2.067; 2.345; 2.601; 2.838; 3.131; 3.340 |];
      [| 0.254; 0.675; 1.285; 1.651; 1.758; 1.969; 2.065; 2.341; 2.596; 2.832; 3.123; 3.330 |];
      [| 0.253; 0.674; 1.282; 1.645; 1.751; 1.960; 2.054; 2.326; 2.576; 2.807; 3.090; 3.290 |]; |]
  in
  table.(dgf_index).(alpha_index)

let softmax ?(temp=1.0) weights =
  if Array.length weights = 0 then raise (Invalid_argument "weights") else
  if temp = 0.0 then raise (Invalid_argument "temp") else
    let weights = Array.map (fun w -> exp (w /. temp)) weights in
    let sum = Array.fold_left (+.) 0.0 weights in
    let weights = Array.map (fun w -> w /. sum) weights in
    Array.get weights
