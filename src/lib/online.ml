(*
   Copyright 2015,2016:
     Leonid Rozenberg <leonidr@gmail.com>
     Carmelo Piccione <carmelo.piccione@gmail.com>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

(* Running descriptive statistics; keep track of the hard bits
   via recurrence equations. *)

type t =
  { size   : int
  ; last   : float
  ; max    : float
  ; min    : float
  ; sum    : float
  ; sum_sq : float
  ; mean   : float
  ; var    : float
  }

(* Mutators *)
let empty =
  { size   = 0
  ; last   = nan
  ; max    = nan
  ; min    = nan
  ; sum    = nan
  ; sum_sq = nan
  ; mean   = nan
  ; var    = nan
  }

let init ?(size=1) o =
  { size
  ; last   = o
  ; max    = o
  ; min    = o
  ; sum    = o *. float size
  ; sum_sq = o *. o *. float size
  ; mean   = o
  ; var    = 0.0
  }


type update = {n_mean:float;n_var:float}

module type Update_rules = sig

  val apply : size:float -> n_sum:float -> n_sum_sq:float -> n_size:float ->
              t -> float -> update

end

module Make (Update : Update_rules) = struct

  let update ?(size=1) t v =
    if t.size = 0 then
      init ~size v
    else
      let size_f = float size in
      let n_sum = t.sum +. size_f *. v in
      let n_sum_sq = t.sum_sq +. size_f *. v *. v in
      let n_size_i = t.size + size in
      let n_size = float n_size_i in
      let {n_mean; n_var} =
        Update.apply ~size:size_f ~n_sum ~n_sum_sq ~n_size t v
      in
      { size   = n_size_i
      ; last   = v
      ; max    = max t.max v
      ; min    = min t.min v
      ; sum    = n_sum
      ; sum_sq = n_sum_sq
      ; mean   = n_mean
      ; var    = n_var
      }

  let join rs1 rs2 =
    if rs1.size = 0 then
      rs2
    else if rs2.size = 0 then
      rs1
    else
      let n_size_i = rs1.size + rs2.size in
      let size_f = float rs2.size in
      let n_size = float n_size_i in
      let n_sum  = rs1.sum +. rs2.sum in
      let n_sum_sq = rs1.sum_sq +. rs2.sum_sq in
      let {n_mean; n_var} =
        Update.apply ~size:size_f ~n_sum ~n_sum_sq ~n_size rs1 rs2.mean
      in
      { size = n_size_i
      ; last = rs2.last              (* dangerous, *)
      ; max  = max rs1.max rs2.max
      ; min  = min rs1.min rs2.min
      ; sum  = n_sum
      ; sum_sq = n_sum_sq
      ; mean = n_mean
      ; var  = n_var
      }

end

module Default = struct

  let apply ~size ~n_sum ~n_sum_sq ~n_size t v =
    let n_mean = t.mean +. size *. (v -. t.mean) /. n_size in
    let num = n_sum_sq -. 2.0 *. n_mean *. n_sum +. n_mean *. n_mean *. n_size in
    let den = n_size -. 1.0  in
    { n_mean; n_var  = num /. den }

end

module D = Make(Default)
include D

