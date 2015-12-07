
module IrisEncoded =
  struct
    type clas = [ `setosa | `versicolor | `virginica ]
    type feature = float array
    let encoding arr = arr
    let size = 4
  end

module IrisMlr = Classify.MulticlassLogisticRegression(IrisEncoded)

let irismlr = IrisMlr.estimate Data.Iris.iris
let perf =
  Data.Iris.iris
  |> List.map (fun (c, f) ->
      c, Classify.most_likely (IrisMlr.eval irismlr f))

let describe msg perf =
  let correct =
    List.fold_left (fun c (a,p) -> if a = p then c + 1 else c) 0 perf
  in
  let total = List.length perf in
  let pct_c = (float correct) /. (float total) in
  Printf.printf "%s: %0.3f correct\n" msg pct_c

let () = describe "Multiclass" perf

module IrisLr = Classify.LogisticRegression(IrisEncoded)

let irislr = IrisLr.estimate Data.Iris.iris
let twoclassperf =
  Data.Iris.iris
  |> List.map (fun (c, f) ->
      c, Classify.most_likely (IrisLr.eval irislr f))

let () = describe "Two class" twoclassperf

module IrisJustSetosa =
  struct
    type clas = [ `setosa | `notsetosa ]
    type feature = float array
    let encoding arr = arr
    let size = 4
  end

module IrisLr_Js = Classify.LogisticRegression(IrisJustSetosa)

let just_setosa =
  Data.Iris.iris
  |> List.map (fun (c,f) ->
      match c with
      | `setosa -> `setosa, f
      | _       -> `notsetosa, f)

let irislr_js = IrisLr_Js.estimate just_setosa
let justsetosaperf =
  just_setosa
  |> List.map (fun (c, f) ->
      c, Classify.most_likely (IrisLr_Js.eval irislr_js f))

let () = describe "Just Setosa" justsetosaperf

