let data_skew ftf =
  Array.init 10000 (fun i -> if i mod ftf = 0 then n2 () else n1())
  |> Descriptive.unbiased_summary
  |> fun s -> s.Descriptive.skew
  |> snd ;;
let count_big_skew =
  Array.fold_left (fun c s ->
      if s = `Positive || s = `Negative then c + 1 else c) 0 ;;
Array.init 100 (fun _ -> data_skew 10) |> count_big_skew ;;
Array.init 100 (fun _ -> data_skew 2) |> count_big_skew ;;
