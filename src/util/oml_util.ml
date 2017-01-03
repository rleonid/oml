(*
   Copyright 2015,2016:
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

include Oml_util_base

module Array = Oml_array

module List = ListLabels

module Float = struct
  let ( + ) x y = x +. y
  let ( - ) x y = x -. y
  let ( * ) x y = x *. y
  let ( / ) x y = x /. y
end

module type Optional_arg_intf = sig

  type opt            (** type of default argument. *)
  val default : opt   (** A default value used when not specified.*)
end

module Kahan = Oml_kahan

module Polynomial = Oml_poly
