let n1 = Sampling.normal ~mean:2. ~std:1. ()
let n2 = Sampling.normal ~mean:2. ~std:10. ()
let data = Array.init 10000 (fun i -> if i mod 10 = 0 then n2 () else n1 ())
