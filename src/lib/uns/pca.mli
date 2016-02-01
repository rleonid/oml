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

(** Principal Components Analysis. *)
open Lacaml.D

type t

(** [variances t] returns a vector of the amount of variance explained by each
    PC in [t]. *)
val variances : t -> float array

(** [components t] *)
val components : t -> float array array

(** [scalings t] returns an array of tuples representing the subtracted mean and
    divided standard deviation applied to the data before broken into PC's. *)
val scalings : t -> (float * float) array

(** [pca ?demean ?scale ?unbiased data] break down [data] into its principal
    components. [demean] will subtract the mean from each column (defaults
    to [true]). [scale] will divide each column by the standard deviation
    (defaults to [true]) and unbiased controls whether to use the unbaised
    standard deviation calculation (defaults to [true]).
 *)
val pca : ?demean:bool -> ?scale:bool -> ?unbiased:bool -> mat -> t

type pca_reduction_method =
  | Num_comp of int       (** Only use this many components. *)
  | Varience_exp of float (** Choose as many components as needed to explain this much variance. *)

(** [reduce t prm] restrict the number of stored principal components via [prm].*)
val reduce : t -> pca_reduction_method -> t
