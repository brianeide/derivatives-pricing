open Printf;;

let binomial vol s k r t steps = (* vol, current spot, strike, risk-free rate, expiration, steps *)
  let h = t /. steps
  in let u = exp (vol *. (sqrt h))
  in let d = exp (~-.vol *. (sqrt h))
  in let p = ((exp (r *. h)) -. d) /. (u -. d) (* risk-neutral probability *)
  in let q = 1.0 -. p
  in let rec node s n = (* node spot price, step *)
    if n = steps
      then max 0.0 (s -. k)
    else
      let up = node (s *. u) (n +. 1.0)
      in let down = node (s *. d) (n +. 1.0)
      in (exp (~-.r *. h)) *. (p *. up +. q *. down)
  in 
  printf "h = %f\n" h;
  printf "u = %f\n" u;
  printf "d = %f\n" d;
  printf "p = %f\n" p;
  printf "q = %f\n" q;
  node s 0.0
;;

let call = binomial 0.354686 129.93 120.0 0.05 1.0 20.0
  in printf "Call Price = %f\n" call ;; (* 26.3825593697785656; pretty good *)
