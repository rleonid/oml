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

(** Common values used throughout the library. *)

(** Some of the iterative routines can fail for the following reasons. *)
type iterative_failure_reason =
  | Out_of_bounds of float
  | No_convergence
  | Too_many_iterations of int
  | Too_few_iterations of int

(** The exception raised when an iterative function fails to converge. *)
exception Iteration_failure of string * iterative_failure_reason

(** [invalid_arg ?m ?f format msg] raise [Invalid_argument] exception with a
    message based on [format] and [msg]. Prepend [m]odule and [f]unction name
    if specified.

    @raise Invalid_argument with the formatted message.
*)
val invalid_arg : ?m:string -> ?f:string -> ('a, unit, string, 'b) format4 -> 'a

(** 3.14159265358979312  *)
val pi : float

val midpoint : float -> float -> float

(** A really small value. Also known as the machine epsilon, the smallest
    distance between two representable floats. *)
val dx : float

(** [significantly_different_from ?d x y] will check if [y] is more than [d]
    (defaults to [dx]) away from [x]. *)
val significantly_different_from : ?d:float -> float -> float -> bool

(** [equal_floats d x y] compares [x] and [y] too see if they are equal,
    specifically [not (significantly_different_from ~d x y)].*)
val equal_floats : d:float -> float -> float -> bool

(** [is_nan x] robustly determine if a float represents NaN (aka Not a number). *)
val is_nan : float -> bool

(** [is_degenerate x] determine if a value is [nan] or [neg_infinity] or
    [infinity]. *)
val is_degenerate : float -> bool

type 'a bound = Open of 'a
              | Closed of 'a

(** [within bound_pair x] evaluates whether [x] is inside the interval described
    by the [bound_pair]. *)
val within : ('a bound * 'a bound) -> 'a -> bool

val fst3 : ('a * 'b * 'c) -> 'a
val snd3 : ('a * 'b * 'c) -> 'b
val thr3 : ('a * 'b * 'c) -> 'c

(** Extend the [Array] module with useful functions. *)
module Array : sig include module type of Oml_array end

(** Provide operator overloads for conveniently expressing float arithmatic.*)
module Float : sig
  val ( + ) : float -> float -> float
  val ( - ) : float -> float -> float
  val ( / ) : float -> float -> float
  val ( * ) : float -> float -> float
end

(** When passing optional arguments to procedures. *)
module type Optional_arg_intf = sig

  type opt            (** type of default argument. *)
  val default : opt   (** A default value used when not specified.*)
end
