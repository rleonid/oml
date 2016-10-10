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

(** Compute running statitics using recurrence equations. *)
type t =
  { size   : int       (** Number of observations. *)
  ; last   : float     (** Last observation. *)
  ; max    : float     (** Maxiumum. *)
  ; min    : float     (** Minimum. *)
  ; sum    : float     (** Sum . *)
  ; sum_sq : float     (** Sum of squares. *)
  ; mean   : float     (** Mean. *)
  ; var    : float     (** {i Unbiased} variance. *)
  }

(** An empty [t], useful for initializing the fold. *)
val empty : t

(** [init ?size x] initializes a [t] with [x] and initial [size]
    which defaults to [1]. *)
val init : ?size:int -> float -> t

type update = { n_mean:float; n_var:float }

(** Defines an update rule. *)
module type Update_rules = sig

  (** Given an existing running statistic [t], number of samples [size],
      the already computed updated statistics [n_sum], [n_sum_sq], [n_size],
      and the newly observed value, return a new estimate of the mean and
      var. *)
  val apply : size:float -> n_sum:float -> n_sum_sq:float -> n_size:float ->
                t -> float -> update

end

(** Create custom {!update} and {!join} rules depending on how the [mean] and
    [var] are update. *)
module Make (Update : Update_rules) : sig

  (* [update ?size t x] incorporate [x] with given [size] (defaulting to [1])
     and update rules [Update.mean], [Update.var]. into the statistics tracked
     in [t]. *)
  val update : ?size:int -> t -> float -> t

  (** [join t1 t2] return a {Online.t} if you had first observed the elements
      by [t1] and then [t2]. *)
  val join : t -> t -> t
end

(** [update ?size t x] incorporate [x] with given [size] (defaulting to [1])
    into the statistics tracked in [t], using Welford std update rule. *)
val update : ?size:int -> t -> float -> t

(** [join t1 t2] return a [Online.t] if you had first observed the elements by
    [t1] and then [t2] using Welford std update rules. *)
val join : t -> t -> t
