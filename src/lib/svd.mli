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

(** Perform the Singular Value Decomposition via {{:https://github.com/mmottl/lacaml}Lacaml}. *)
open Lacaml.D

type t

(** {2 Accessors} *)

(** [u t] *)
val u : t -> Matrices.t

(** [s t] singular values. *)
val s : t -> Vectors.t

(** [vt t]. *)
val vt : t -> Matrices.t

(** [svd m] separate [m] into its singular value decomposition.
  [mat] is destroyed in the process. *)
val svd : mat -> t

(** [solve_linear a b] solve for [x] in [a x = b] using principal components
    regression, when [a] is the Singular Value Decomposition. *)
val solve_linear : ?lambda:float -> t -> vec -> vec

(** [covariance_matrix a b] computes the covariance matrix [A * A^t]. *)
val covariance_matrix : ?lambda:float -> t -> mat

(** [looe t y] returns a function to efficiently computes the Leave-One-Out-Error
    vector, for computing the optimal regularization parameter. *)
val looe : t -> vec -> (float -> vec)

(** [h t] computes the Hat matrix of the original matrix passed to Svd. *)
val h : t -> mat

(** [h_diag t] computes the diagonal of the Hat matrix of the original matrix
    passed to Svd. *)
val h_diag : t -> vec
