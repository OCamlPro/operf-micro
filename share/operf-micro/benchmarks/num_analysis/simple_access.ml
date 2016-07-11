open Unix

let main len =
  let r = ref 0. in
  let arr = Simple_access_a.arr len in
  for i = 1 to Array.length arr do
    r := !r +. Simple_access_a.sum (Simple_access_a.get arr i);
  done
