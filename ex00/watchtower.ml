module type W = sig
  type hour = int

  val zero : hour
  val add : hour -> hour -> hour
  val sub : hour -> hour -> hour
end

module Watchtower : W = struct
  type hour = int

  let zero = 0

  let full_revolution f a b =
    let n = f a b mod 12 in
    if n < 0 then n + 12 else n

  let add a b = full_revolution ( + ) a b
  let sub a b = full_revolution ( - ) a b
end

let () =
  let open Watchtower in
  let test_add_loop start limit =
    let rec loop acc =
      if acc > limit then ()
      else (
        Printf.printf "%d : %d\n" acc (add 0 acc);
        loop (acc + 1))
    in
    loop start
  in
  test_add_loop 0 30;

  let test_sub_loop start limit =
    let rec loop acc =
      if acc > limit then ()
      else (
        Printf.printf "%d : %d\n" acc (sub 0 acc);
        loop (acc + 1))
    in
    loop start
  in
  test_sub_loop 0 30
