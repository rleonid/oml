(** Methods to estimate derivatives of functions. *)

(** [secant f] estimates the derivative of [f] by evaluating a the slope of a
    secant between the desired point and one close by
    (ie. [(x,f(x))] and [(x+h,f(x+h))]). Errors are [O(h)]. *)
val secant : (float -> float) -> (float -> float)

(** [second_order f] estimates the derivatrive of [f] by evaluating the line
    connecting two points around [x], specifically
    [(x-h,f(x-h))] and [(x+h,f(x+h))]. Errors are [O(h^2)]. *)
val second_order : (float -> float) -> (float -> float)

