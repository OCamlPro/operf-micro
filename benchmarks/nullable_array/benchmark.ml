
open Micro_bench_types

module type M = module type of Nullable_array_interface

type 'a prepared_array =
  { option_array : 'a Option_array.t;
    nullable_array_closure : 'a Nullable_array_closure.t;
    nullable_array_marker : 'a Nullable_array_marker.t; }

module type Get = sig
  type 'a t
  val name : string
  val get : 'a prepared_array -> 'a t
end

let random_prepare n = Random.State.make [|n|]

let random_permutation state a =
  let len = Array.length a in
  for i = 0 to Array.length a - 1 do
    let n = Random.State.int state (len - i) + i in
    let v1 = a.(i) in
    let v2 = a.(n) in
    a.(i) <- v2;
    a.(n) <- v1;
  done;
  a

let random_indices state n =
  let a = Array.init n (fun i -> i) in
  random_permutation state a

let random_selection state size n =
  let indices = random_indices state size in
  Array.init n (fun i -> indices.(i))

let prepare_random_option_array density size =
  let state = random_prepare size in
  let n = int_of_float (float size *. density) in
  let selection = random_selection state size n in
  let a = Option_array.make size in
  Array.iter (fun i -> Option_array.set a i i) selection;
  a

let prepare_random_path density size =
  let state = random_prepare size in
  let size = max 3 size in
  (* Shifted by 1: the 0-th field is the entry point *)
  let n = int_of_float (float size *. density) - 1 in
  let n = min (size - 2) (max 1 n) in
  let selection = random_selection state (size - 2) n in
  let a = Option_array.make size in
  Option_array.set a 0 (selection.(0) + 1);
  for i = 0 to n - 2 do
    Option_array.set a (selection.(i) + 1) (selection.(i + 1) + 1);
  done;
  a

module Bench(M:M)(Get:Get with type 'a t := 'a M.t) = struct

  let name = Get.name

  let copy_option_array oa =
    let a = M.make (Option_array.length oa) in
    Option_array.iteri_sparse oa ~f:(fun i v ->
      M.set a i v);
    a

  let sum a =
    let r = ref 0 in
    M.iteri_sparse a ~f:(fun _ i -> r := i + !r);
    !r

  let sum_prepared_array a =
    sum (Get.get a)

  let walk a =
    let rec loop i count =
      match M.get a i with
      | None -> count
      | Some next ->
        loop next (count + 1)
    in
    loop 0 0

  let walk_prepared_array a =
    walk (Get.get a)

  let walk_k a =
    let rec loop i count =
      M.get_k a i
        (fun () -> count)
        (fun next -> loop next (count + 1))
    in
    loop 0 0

  let walk_k_prepared_array a =
    walk_k (Get.get a)

  let sum_bench = name, sum_prepared_array
  let walk_bench = name, walk_prepared_array
  let walk_k_bench = name ^ "_k" , walk_k_prepared_array

end

module OA =
  Bench
    (Option_array)
    (struct
      let name = "option_array"
      let get { option_array } = option_array
    end)
module NAC =
  Bench
    (Nullable_array_closure)
    (struct
      let name = "nullable_array_closure"
      let get { nullable_array_closure } = nullable_array_closure
    end)
module NAM =
  Bench
    (Nullable_array_marker)
    (struct
      let name = "nullable_array_marker"
      let get { nullable_array_marker } = nullable_array_marker
    end)

let copy_prepared_option_array base_array =
  { option_array = OA.copy_option_array base_array;
    nullable_array_closure = NAC.copy_option_array base_array;
    nullable_array_marker = NAM.copy_option_array base_array; }

let check_sum_option_array density size res =
  let oa = prepare_random_option_array density size in
  let sum = OA.sum oa in
  if sum = res then
    Ok
  else
    Error (Printf.sprintf "Incorrect sum: %i, expected %i" res sum)

let prepare_random_array density size =
  copy_prepared_option_array
    (prepare_random_option_array density size)

let sum density =
  Printf.sprintf "sum %0.2f" density,
  Int_group (
    [
      OA.sum_bench;
      NAC.sum_bench;
      NAM.sum_bench;
    ],
       prepare_random_array density,
       check_sum_option_array density,
       [Range (100, 100000), Short])

let check_walk density size res =
  let oa = prepare_random_path density size in
  let length = OA.walk oa in
  if length = res then
    Ok
  else
    Error (Printf.sprintf "Incorrect walk length: %i, expected %i" res length)

let prepare_walk density size =
  copy_prepared_option_array
    (prepare_random_path density size)

let walk density =
  Printf.sprintf "walk %0.2f" density,
  Int_group (
    [
      OA.walk_bench;
      OA.walk_k_bench;
      NAC.walk_bench;
      NAC.walk_k_bench;
      NAM.walk_bench;
      NAM.walk_k_bench;
    ],
       prepare_walk density,
       check_walk density,
       [Range (100, 100000), Short])

let functions =
  let densities = [1.; 0.3; 0.01] in
  List.map sum densities @
  List.map walk densities


let () = add functions
