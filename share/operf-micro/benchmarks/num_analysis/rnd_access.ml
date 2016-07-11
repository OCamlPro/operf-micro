let get a i = a.(i - 1)

let init_arr len =
  Random.self_init ();
  Array.init len (fun _ -> Random.bool ())

let a = [|1|]
let b = [|1.0|]

let main len =
  let arr = init_arr len in
  for i = 1 to Array.length arr do
    if get arr i then ignore (get a 1) else ignore (get b 1)
  done
