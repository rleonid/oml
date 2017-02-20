(* When you want print shorter floats *)

let shorter_float_printer fr = Format.fprintf fr "%0.4f"  ;;
#install_printer shorter_float_printer ;;
