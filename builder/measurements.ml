open Command
open Detect_config
open Files
open Utils

type measurement_sample = {
  runs              : int;
  cycles            : int;
  nanos             : int;
  compactions       : int;
  minor_allocated   : int;
  major_allocated   : int;
  promoted          : int;
  major_collections : int;
  minor_collections : int;
}

type measurements = string * measurement_sample list

let measurement_dict
    {
      runs;
      cycles;
      nanos;
      compactions;
      minor_allocated;
      major_allocated;
      promoted;
      major_collections;
      minor_collections;
    }
  =
  let f s i d = (s, Int i) :: d in
  let dict =
    []
    |> f "runs" runs
    |> f "cycles" cycles
    |> f "nanos" nanos
    |> f "compactions" compactions
    |> f "minor_allocated" minor_allocated
    |> f "major_allocated" major_allocated
    |> f "promoted" promoted
    |> f "major_collections" major_collections
    |> f "minor_collections" minor_collections
  in
  Dict dict

let measurement_file context bench measurements =
  let f name v d = (name,v) :: d in
  let fo name f v d =
    match v with
    | None -> d
    | Some v -> (name, f v) :: d in
  let runs =
    let aux (func, m) =
      let dict =
        []
        |> f "list" (List (List.map measurement_dict m))
        |> f "name" (String func)
      in
      Dict dict
    in
    List.map aux measurements in
  let dict =
    []
    |> f "runs" (List runs)
    |> fo "date" (fun v -> String v) context.timestamp
    |> f "name" (String bench.Benchmark.bench_name)
    |> f "suite_name" (String context.config.name)
  in
  Dict dict

(* let output_measurements oc m = *)
(*   let aux m = *)
(*     let out i = *)
(*       output_string oc (string_of_int i); *)
(*       output_char oc ' ' in *)
(*     out m.runs; *)
(*     out m.cycles; *)
(*     out m.nanos; *)
(*     out m.compactions; *)
(*     out m.minor_allocated; *)
(*     out m.major_allocated; *)
(*     out m.promoted; *)
(*     out m.major_collections; *)
(*     out m.minor_collections; *)
(*     output_char oc '\n' *)
(*   in *)
(*   List.iter (fun (s, l) -> *)
(*       Printf.fprintf oc "%s\n" s; *)
(*       List.iter aux l; *)
(*       output_char oc '\n') *)
(*     m *)

let words s =
  List.filter (function "" -> false | _ -> true)
    (split ' ' s)

let read_measurement l : measurements list =
  let error s =
    Printf.eprintf "malformed input line: %s\n%!" s;
    raise Exit
  in
  let rec aux l curr_name curr acc =
    match l with
    | [] ->
      (curr_name, curr) :: acc
    | "" :: t ->
      aux_name t ((curr_name, curr) :: acc)
    | h :: t ->
      if h.[0] = '#'
      then aux t curr_name curr acc
      else
        try
          let ints = List.map int_of_string (words h) in
          match ints with
          | [ i0; i1; i2; i3; i4; i5; i6; i7; i8 ] ->
            let m =
              { runs = i0;
                cycles = i1;
                nanos = i2;
                compactions = i3;
                minor_allocated = i4;
                major_allocated = i5;
                promoted = i6;
                major_collections = i7;
                minor_collections = i8 } in
            aux t curr_name (m::curr) acc
          | _ -> error h
        with _ -> error h

  and aux_name l acc =
    match l with
    | [] -> acc
    | "" :: t ->
      aux_name t acc
    | name :: t ->
      aux t name [] acc
  in
  aux_name l []
