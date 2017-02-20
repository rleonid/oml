open Oml
open Util
open Classification

let feature_map = function | `shortbread -> 0 | `lager -> 1 | `whiskey -> 2 | `porridge -> 3 | `football-> 4 ;;
let data =
  [
    (`English, [`whiskey; `porridge; `football]);
    (`English, [`shortbread; `whiskey; `porridge]);
    (`English, [`shortbread; `lager; `football]);
    (`English, [`shortbread; `lager]);
    (`English, [`lager; `football]);
    (`English, [`porridge]);
    (`Scottish, [`shortbread; `porridge; `football]);
    (`Scottish, [`shortbread; `lager; `football]);
    (`Scottish, [`shortbread; `lager; `whiskey; `porridge]);
    (`Scottish, [`shortbread; `lager; `porridge]);
    (`Scottish, [`shortbread; `lager; `porridge; `football]);
    (`Scottish, [`shortbread; `whiskey; `porridge]);
    (`Scottish, [`shortbread; `whiskey])
  ] ;;
let to_feature l = l |> List.map ~f:feature_map |> Array.of_list ;;
module NB = Naive_bayes.Binomial(
  struct
    type feature = [ `shortbread | `lager | `whiskey | `porridge | `football ] list
    type class_ = [ `English | `Scottish]
    let encoding = to_feature
    let size = 5
  end);;
let naiveb = NB.estimate ~opt:( NB.opt ~bernoulli:true ()) data ;;
let sample = [ `shortbread ; `whiskey; `porridge  ]  ;;
let result = NB.eval naiveb sample;;

let () =
  if !Sys.interactive then () else
    Printf.printf "Probability of English: %f\t(expecting 19%%)\n" (List.assoc `English result);
    Printf.printf "Probability of Scottish: %f\t(expecting 81%%)\n" (List.assoc `Scottish result)
