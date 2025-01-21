module type A = sig
  type project = string * string * int

  val zero : string * string * int
  val combine : project -> project -> project
  val fail : project -> project
  val success : project -> project
end

module App : A = struct
  type project = string * string * int

  let zero = ("", "", 0)

  let combine p1 p2 =
    let name =
      (match p1 with name, status, grade -> name)
      ^ match p2 with name, status, grade -> name
    in
    let grade =
      ((match p1 with name, status, grade -> grade)
      + match p2 with name, status, grade -> grade)
      / 2
    in
    if grade >= 80 then (name, "succeed", grade) else (name, "failed", grade)

  let fail p = ((match p with name, _, _ -> name), "failed", 0)
  let success p = ((match p with name, _, _ -> name), "succeed", 80)
end

let () =
  let open App in
  let print_proj p =
    match p with
    | name, status, grade ->
        Printf.printf "name: %s, status: %s, grade: %d\n" name status grade
  in
  let p1 = ("42", "succeed", 85) in
  let p2 = ("42", "failed", 35) in
  print_proj p1;
  print_proj p2;
  print_proj (combine p1 p2);
  print_proj (fail p1);
  print_proj (success p2)
