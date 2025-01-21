(*
Monoidのシグネチャを作成
*)
module type MONOID = sig
  type element

  val zero1 : element
  val zero2 : element
  val mul : element -> element -> element
  val add : element -> element -> element
  val div : element -> element -> element
  val sub : element -> element -> element
end

(*
Monoidの signature を使い、INTモジュールを作成
その際にwith type element = intを
描かないとINT.elementがintだとわからずにコンパイルできない。
INT.element型が抽象型と解釈される。

with type: 「MONOIDモジュール型の中で定義されている抽象型 element を具体的な型 int として公開する」という意味
*)
(* module INT : MONOID  = struct *)
module INT : MONOID with type element = int = struct
  type element = int

  let zero1 = 0
  let zero2 = 1
  let mul e1 e2 = e1 * e2
  let add e1 e2 = e1 + e2
  let div e1 e2 = e1 / e2
  let sub e1 e2 = e1 - e2
end

module FLOAT : MONOID with type element = float = struct
  type element = float

  let zero1 = 0.
  let zero2 = 1.
  let mul e1 e2 = e1 *. e2
  let add e1 e2 = e1 +. e2
  let div e1 e2 = e1 /. e2
  let sub e1 e2 = e1 -. e2
end

(* functorの signatureを作成
   入力にMONOIDのsignatureをもとに作成したモジュールを受け取る
*)
module type C = functor (M : MONOID) -> sig
  val add : M.element -> M.element -> M.element
  val sub : M.element -> M.element -> M.element
  val mul : M.element -> M.element -> M.element
  val div : M.element -> M.element -> M.element
  val power : M.element -> int -> M.element
  val fact : M.element -> M.element
end

(*
Cの signatureをもとに Calcファンクターを作成
*)
module Calc : C =
functor
  (M : MONOID)
  ->
  struct
    let add = M.add
    let sub = M.sub
    let mul = M.mul
    let div = M.div

    let power x y =
      let rec loop acc cnt =
        if cnt >= y then acc else loop (mul acc x) (cnt + 1)
      in
      loop M.zero2 0

    let fact (e : M.element) : M.element =
      let rec loop (acc : M.element) (cnt : M.element) =
        if cnt <= M.zero2 then acc else loop (mul acc cnt) (sub cnt M.zero2)
      in
      loop M.zero2 e
  end

module Calc_int = Calc (INT)
module Calc_float = Calc (FLOAT)

let () =
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
  Printf.printf "-------------------------\n";
  Printf.printf "%d\n" (Calc_int.sub 3 3);
  Printf.printf "%d\n" (Calc_int.div 3 3);
  Printf.printf "%f\n" (Calc_float.sub 3.0 3.0);
  Printf.printf "%f\n" (Calc_float.div 3.0 3.0);

  Printf.printf "%d\n" (Calc_int.fact 6 );
  Printf.printf "%f\n" (Calc_float.fact 6. );
