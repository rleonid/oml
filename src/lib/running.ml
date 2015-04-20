(* Running descriptive statistics; keep track of the hard bits
   via recurrence equations. *)

type t = { size   : int
         ; last   : float
         ; max    : float
         ; min    : float
         ; sum    : float
         ; sum_sq : float
         ; mean   : float
         ; var    : float
         }

(* Mutators *)
let empty = { size   = 0
            ; last   = nan
            ; max    = nan
            ; min    = nan
            ; sum    = nan
            ; sum_sq = nan
            ; mean   = nan
            ; var    = nan
            }

let init o = { size   = 1
             ; last   = o
             ; max    = o
             ; min    = o
             ; sum    = o
             ; sum_sq = o *. o
             ; mean   = o
             ; var    = 0.0
             }

let update t v =
  if t.size = 0
  then init v
  else let n_sum = t.sum +. v in
       let n_sum_sq = t.sum_sq +. v *. v in
       let n_size = float t.size +. 1.0 in
       let n_mean = n_sum /. n_size in
       let n_var =
         let num = n_sum_sq
                 -. 2.0 *. n_mean *. n_sum
                 +. n_size *. n_mean *.  n_mean
         and den = n_size -. 1.0 in
         num /. den
      in
      { size   = t.size + 1
      ; last   = v
      ; max    = max t.max v
      ; min    = min t.min v
      ; sum    = n_sum
      ; sum_sq = n_sum_sq
      ; mean   = n_mean
      ; var    = n_var
      }

let join rs1 rs2 =
  if rs1.size = 0
  then rs2
  else if rs2.size = 0
       then rs1
       else let new_size = float (rs1.size + rs2.size) in
            let new_mean =
              let p1 = (float rs1.size) /. new_size
              and p2 = (float rs2.size) /. new_size in
              rs1.mean *. p1 +. rs2.mean *. p2
              (* let num = rs1.mean *. (float rs1.size)
                      +. rs2.mean *. (float rs2.size) in
              num /. new_size *)
            in
            let new_var =
              let num = rs1.sum_sq
                      +. rs2.sum_sq
                      -. 2.0 *. new_mean *. (rs1.sum +. rs2.sum)
                      +. new_mean *. new_mean *. new_size
              and den = new_size -. 1.0 in
              num /. den
            in
            { size = rs1.size + rs2.size
            ; last = rs2.last              (* dangerous, *)
            ; max  = max rs1.max rs2.max
            ; min  = min rs1.min rs2.min
            ; sum  = rs1.sum +. rs2.sum
            ; sum_sq = rs1.sum_sq +. rs2.sum_sq
            ; mean = new_mean
            ; var  = new_var
            }
