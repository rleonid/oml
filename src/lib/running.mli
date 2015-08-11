(*
   Copyright 2015:
     Leonid Rozenberg <leonidr@gmail.com>

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
type t = { size : int         (** Number of observations. *)
         ; last : float       (** Last observation. *)
         ; max : float        (** Maxiumum. *)
         ; min : float        (** Minimum. *)
         ; sum : float        (** Sum . *)
         ; sum_sq : float     (** Sum of squares. *)
         ; mean : float       (** Mean. *)
         ; var : float        (** _Unbiased_ variance. *)
         }

(** Defines a mean update function. Given an existing running statistic [t],
    number of samples [size], the already computed updated statistics
    [n_sum], [n_sum_sq], [n_size], and the newly observed value, return
    a new estimate of the mean. *)
type mean_update = size:int -> n_sum:float ->
  n_sum_sq:float -> n_size:float -> t -> float -> float

(** Defines a variance update function. Given an existing running
    statistic [t], the already computed updated statistics [n_mean]
    [n_sum], [n_sum_sq], and [n_size], return a new estimate of the variance. *)
type var_update = n_mean:float -> n_sum:float ->
  n_sum_sq:float -> n_size:float -> t -> float

(** [empty] an empty [t], useful for initializing the fold. *)
val empty : t

(** [init ?size x] initializes a [t] with [x] and initial [size]
    which defaults to [1]. *)
val init : ?size:int -> float -> t

(** [update ?size ?mean_update ?var_update t x] incorporate [x] with given [size]
    (defaulting to [1]) and update rules [mean_update], [var_update] (defaulting
    to unbiased) into the statistics tracked in [t]. *)
val update : ?size:int -> ?mean_update:mean_update ->  ?var_update:var_update -> t -> float -> t

(** [join ?mean_update ?var_update t1 t2] return a [Running.t] if you had
    first observed the elements by [t1] and then [t2] using update rules
    [mean_update] and [var_update] (defaulting to unbiased) *)
val join : ?mean_update:mean_update -> ?var_update:var_update -> t -> t -> t
