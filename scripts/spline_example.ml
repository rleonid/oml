
open Gnuplot
open Oml
open Util

let data = [| (0.,0.); (1.,0.); (3.,2.); (4.,2.) |]

let fits () =
  let module S = Interpolate.Spline in
  let sn = S.fit data in
  let sc = S.fit ~bc:(S.Clamped (0.,0.)) data in
  let xs = Array.range ~incr:0.01 ~start:(-1.) ~stop:5. () in
  let natural_fit = Array.zip xs (S.eval_arr sn xs) in
  let clamped_fit = Array.zip xs (S.eval_arr sc xs) in
  natural_fit, clamped_fit

let plot fname =
  let gp = Gp.create ~verbose:true () in
  let natural_fit, clamped_fit = fits () in
  Gp.plot_many gp 
    ~output:(Output.create (`Png fname))
    ~use_grid:true 
    ~range:(Range.XY (-1., 5., -1.0, 3.0))
   [ Series.points_xy ~title:"Knots"   ~color:`Blue  ~weight:10 (Array.to_list data)
   ; Series.points_xy ~title:"Natural" ~color:`Red   ~weight:1  (Array.to_list natural_fit)
   ; Series.points_xy ~title:"Clamped" ~color:`Green ~weight:1  (Array.to_list clamped_fit)
   ];
  Gp.close gp
