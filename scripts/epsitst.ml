let dx = 2.22044e-16

let () =
  Printf.printf "add eps to 1.0 %b\n" (1.0 +. epsilon_float = 1.0);
  Printf.printf "sub eps from 1.0 %b\n" (1.0 -. epsilon_float = 1.0);
  Printf.printf "add dx to 1.0 %b\n" (1.0 +. dx = 1.0);
  Printf.printf "sub dx from 1.0 %b\n" (1.0 -. dx = 1.0);
  Printf.printf "dx is less %b\n" (dx < epsilon_float)
