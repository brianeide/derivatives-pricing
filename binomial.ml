open Printf;;

let european vol s k r div t steps call = (* vol, current spot, strike, risk-free rate, expiration, steps *)
  let h = t /. steps
  in let u = exp ((r -. div) *. h +. vol *. (sqrt h))
  in let d = exp ((r -. div) *. h -. vol *. (sqrt h))
  in let p = ((exp ((r -. div) *. h)) -. d) /. (u -. d) (* risk-neutral probability *)
  in let q = 1.0 -. p
  in let rec node s n = (* node spot price, step *)
    if n = steps
      then max 0.0 (if call then s -. k else k -. s)
    else
      let up = node (s *. u) (n +. 1.0)
      in let down = node (s *. d) (n +. 1.0)
      in (exp (~-.r *. h)) *. (p *. up +. q *. down)
  in node s 0.0
;;

let american vol s k r div t steps call =
  let h = t /. steps
  in let u = exp ((r -. div) *. h +. vol *. (sqrt h))
  in let d = exp ((r -. div) *. h -. vol *. (sqrt h))
  in let p = ((exp ((r -. div) *. h)) -. d) /. (u -. d)
  in let q = 1.0 -. p
  in let rec node s n =
    let exercised = max 0.0 (if call then s -. k else k -. s)
    in if n = steps
        then exercised
      else
        let up = max exercised (node (s *. u) (n +. 1.0))
        in let down = max exercised (node (s *. d) (n +. 1.0))
        in (exp (~-.r *. h)) *. (p *. up +. q *. down)
  in node s 0.0
;;

let euroCall = european 0.2979 181.11 180.0 0.05 0.0051 0.013699 24.0 true
  in printf "European Call Price = %f\n" euroCall ;; (* 3.193079 *)

let americanCall = american 0.2979 181.11 180.0 0.05 0.0051 0.013699 24.0 true
  in printf "American Call Price = %f\n" americanCall ;; (* 3.842689 *)

let euroPut = european 0.2979 181.11 180.0 0.05 0.0051 0.013699 24.0 false
  in printf "European Put Price = %f\n" euroPut ;; (* 1.972483 *)
  
let americanPut = american 0.2979 181.11 180.0 0.05 0.0051 0.013699 24.0 false
  in printf "American Put Price = %f\n" americanPut ;; (* 2.427060 *)