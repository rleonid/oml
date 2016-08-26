(*
   Copyright 2015:2016
     Carmelo Piccione <carmelo.piccione@gmail.com>
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

(** [softmax ?temperature weights] transforms [weights] into softmax weights dependent
    on [temperature].

    @raise Invalid_argument if [weights] is empty or [temperature = 0]. *)
val softmax : ?temperature:float -> float array -> float array
