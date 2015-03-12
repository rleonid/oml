
open Descriptive

(* When we do not know the mean or standard deviation of a distribution
 * we can still create a prediction interval based off distribution stat.
 *)
let prediction_interval alpha dist_stat =
  let n = dist_stat.size in
  let t_value = Functions.t_lookup alpha n in
  let std = sqrt dist_stat.var in
  let m   = sqrt (1.0 +. (1.0 /. (float n))) in
  let dev = t_value *. std *. m in
  (dist_stat.mean -. dev, dist_stat.mean +. dev)

type test = { standard_error      : float
            ; degrees_of_freedom  : float
            ; stat                : float
            (* the probability that |t_stat| could be this large (or larger) by
               chance, for distributions with equal means. *)
            ; prob_by_chance      : float;
            }

let test_to_string t =
  Printf.sprintf "standard error: %.3f, degrees of freedom: %.3f, \
                  stat: %.3f, probability observation by chance: %.3f"
        t.standard_error t.degrees_of_freedom t.stat t.prob_by_chance

type null_hypothesis =
  | TwoTail   (* the sample mean equals the population mean. *)
  | OneTail   (* the sample mean is less than or greater than the population mean. *)

  (*
let simple_t_test hypothesis standard_error degrees_of_freedom stat_diff =
  let stat = stat_diff / standard_error in
  let pbc =
    let p = 1.0 -. (student_t_test_sig (abs stat) degrees_of_freedom) in
    match hypothesis with
    | TwoTail -> p
    | OneTail -> p /. 2.0
  in
  { standard_error     = standard_error;
    degrees_of_freedom = degrees_of_freedom;
    stat               = stat;
    prob_by_chance     = pbc;
  }

(* i am going to ignore the tests where you 'know' the population variance. *)
let mean_t_test population_mean hypothesis arr =
    let m = mean arr in
    let std = sqrt (unbiased_var arr) in
    let n = float (Array.length arr) in
    let se = std /. (sqrt n) in
    let t_stat = (m -. population_mean) /. se in
    let deg_free = n -. 1.0 in
    let prob_by_chance =
        match hypothesis with
        | TwoTail -> 1.0 -. (student_t_test_sig (abs t_stat) deg_free)
        | OneTail -> (1.0 -. (student_t_test_sig (abs t_stat) deg_free)) /. 2.0;
    in
    { standard_error = se;
      degrees_of_freedom = deg_free;
      stat = t_stat;
      prob_by_chance = prob_by_chance;
    }

let equal_means_same_variance_test hypothesis arr1 arr2 =
    let m1 = mean arr1 in
    let m2 = mean arr2 in
    let n1 = float (Array.length arr1) in
    let n2 = float (Array.length arr2) in
    let deg_free = n1 +. n2 -. 2.0 in
    (* standard error. *)
    let se =
        let mo1 = moment 2 arr1 in
        let mo2 = moment 2 arr2 in
        sqrt (((n1 -. 1.0) *. mo1 +. (n2 -. 1.0) *. mo2)/.
                deg_free *. ((1.0 /.  n1) +. (1.0 /. n2)))
    in
    let t_stat = (m1 -. m2) /. se in
    let prob_by_chance =
        match hypothesis with
        | TwoTail -> 1.0 -. (student_t_test_sig (abs t_stat) deg_free);
        | OneTail -> (1.0 -. (student_t_test_sig (abs t_stat) deg_free) /. 2.0);
    in
    { standard_error = se;
      degrees_of_freedom = deg_free;
      stat = t_stat;
      prob_by_chance = prob_by_chance;
    }

let unequal_variance_test hypothesis arr1 arr2 =
    let m1 = mean arr1 in
    let m2 = mean arr2 in
    let n1 = float (Array.length arr1) in
    let n2 = float (Array.length arr2) in
    let vn1 = (var arr1) /. n1 in
    let vn2 = (var arr2) /. n2 in
    (* standard error, kind of ? *)
    let se = sqrt (vn1 +. vn2) in
    let t_stat = (m1 -. m2) /. se in
    let deg_free = ((vn1 +. vn2) ** 2.0) /.
      ((vn1 ** 2.0) /. (n1 -. 1.0) +. (vn2 ** 2.0) /. (n2 - 1.0))
    in
    let se =
        let mo1 = moment 2 arr1 in
        let mo2 = moment 2 arr2 in
        sqrt (((n1 -. 1.0) *. mo1 +. (n2 -. 1.0) *. mo2) /.
          deg_free *. ((1.0 /.  n1) +. (1.0 /. n2)))
    in
    (* this distribution according to Numerical recipes is approximately Student. *)
    let prob_by_chance =
        match hypothesis with
        | TwoTail -> 1.0 -. (student_t_test_sig (abs t_stat) deg_free);
        | OneTail -> (1.0 -. (student_t_test_sig (abs t_stat) deg_free) /. 2.0);
    in
    { standard_error = se;
      degrees_of_freedom = deg_free;
      stat = t_stat;
      prob_by_chance = prob_by_chance;
    }

let different_variances_test arr1 arr2 =
    let v1 = var arr1 in
    let v2 = var arr2 in
    let f, dgf1, dgf2 =
        if v1 > v2 then
            v1 /. v2, float (Array.length arr1), float (Array.length arr2)
        else
            v2 /. v1, float (Array.length arr2), float (Array.length arr1)
    in
    let p = 2.0 *. f_test_sig f dgf1 dgf2 in
    let p = if p > 1.0 then 2.0 -. p else p in
    { standard_error = 0.0;
      degrees_of_freedom = dgf1 +. dgf2;
      stat = f;
      prob_by_chance = p;
    }

let correlation_test arr1 arr2 =
    let r = correlation arr1 arr2 in
    let deg_free = float (Array.length arr1 - 2) in (* test of correlation assures that the sizes are the same. *)
    let t_stat = r * sqrt ( deg_free  /. (1.0 -. r *. r)) in
    { standard_error = 0.0;
      degrees_of_freedom = deg_free;
      stat = t_stat;
      prob_by_chance = 1.0 -. (student_t_test_sig (abs t_stat) deg_free);
    }
    *)
