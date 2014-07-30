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

type runs = {
  name : string;
  list : measurement_sample list;
}

type recorded_measurements = {
  suite_name : string;
  name : string;
  date : string;
  run_list : runs list;
}

module Reader = struct
  type measurement_sample' = {
    runs              : int option;
    cycles            : int option;
    nanos             : int option;
    compactions       : int option;
    minor_allocated   : int option;
    major_allocated   : int option;
    promoted          : int option;
    major_collections : int option;
    minor_collections : int option;
  }

  type runs' = {
    name : string option;
    list : measurement_sample list option;
  }

  type recorded_measurements' = {
    suite_name : string option;
    name : string option;
    date : string option;
    run_list : runs list option;
  }

  let add_measurement_field (m:measurement_sample') (s, v) =
    let v =
      match v with
      | Int i -> i
      | _ -> failwith "parse error"
    in
    match s with
    | "runs" -> { m with runs = Some v }
    | "cycles" -> { m with cycles = Some v }
    | "nanos" -> { m with nanos = Some v }
    | "compactions" -> { m with compactions = Some v }
    | "minor_allocated" -> { m with minor_allocated = Some v }
    | "major_allocated" -> { m with major_allocated = Some v }
    | "promoted" -> { m with promoted = Some v }
    | "major_collections" -> { m with major_collections = Some v }
    | "minor_collections" -> { m with minor_collections = Some v }
    | _ -> m

  let read_measurement_sample v : measurement_sample =
    let empty : measurement_sample' =
      {
        runs = None;
        cycles = None;
        nanos = None;
        compactions = None;
        minor_allocated = None;
        major_allocated = None;
        promoted = None;
        major_collections = None;
        minor_collections = None;
      } in
    let r = match v with
      | Dict l -> List.fold_left add_measurement_field empty l
      | _ -> failwith "parse error" in
    match r with
    | { runs = Some runs;
        cycles = Some cycles;
        nanos = Some nanos;
        compactions = Some compactions;
        minor_allocated = Some minor_allocated;
        major_allocated = Some major_allocated;
        promoted = Some promoted;
        major_collections = Some major_collections;
        minor_collections = Some minor_collections } ->
       { runs; cycles; nanos; compactions; minor_allocated;
         major_allocated; promoted; major_collections; minor_collections; }
    | _ -> failwith "parse error"

  let add_runs_field r (s, v) : runs' =
    match s, v with
    | "name", String v -> { r with name = Some v }
    | "list", List l ->
       { r with list = Some (List.map read_measurement_sample l) }
    | _ -> r

  let read_runs v : runs =
    let empty : runs' = { name = None; list = None } in
    let r = match v with
      | Dict l -> List.fold_left add_runs_field empty l
      | _ -> failwith "parse error" in
    match r with
    | { name = Some name; list = Some list } -> { name; list }
    | _ -> failwith "parse error"

  let add_record_field r (s, v) : recorded_measurements' =
    match s, v with
    | "suite_name", String v -> { r with suite_name = Some v }
    | "name", String v -> { r with name = Some v }
    | "date", String v -> { r with date = Some v }
    | "runs", List l ->
       { r with run_list = Some (List.map read_runs l) }
    | _ -> r

  let read_record v : recorded_measurements =
    let empty : recorded_measurements' =
      { suite_name = None; name = None; date = None; run_list = None } in
    let r = match v with
      | Dict l -> List.fold_left add_record_field empty l
      | _ -> failwith "parse error" in
    match r with
    | { suite_name = Some suite_name;
        name = Some name;
        date = Some date;
        run_list = Some run_list } ->
       { suite_name; name; run_list; date }
    | _ -> failwith "parse error"

end

let read_measurement_file ~filename:file =
  let file = load_config_file file in
  Reader.read_record file

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

let read_measurement ~contents:text : measurements list =
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
  aux_name (lines text) []


type results_mat = (int * float) array

let affine_adjustment (r:results_mat) =
  let len = float (Array.length r) in
  let mean_x =
    let sum_x = Array.fold_right (fun (x,_) acc -> x + acc) r 0 in
    (float sum_x) /. len in
  let mean_y =
    let sum_y = Array.fold_right (fun (_,y) acc -> y +. acc) r 0. in
    sum_y /. len in
  let variance_x =
    let sumvar =
      Array.fold_right
        (fun (x,_) acc ->
         let v = (float x) -. mean_x in v *. v +. acc)
        r 0.
    in
    sumvar /. len
  in
  let covariance_x_y =
    let sumcovar =
      Array.fold_right
        (fun (x,y) acc ->
         let v = ((float x) -. mean_x) *. (y -. mean_y) in
         v +. acc)
        r 0.
    in
    sumcovar /. len
  in
  let a = covariance_x_y /. variance_x in
  let b = mean_y -. a *. mean_x in
  a, b

type column =
  | Cycles
  | Nanos
  | Compactions
  | Minor_allocated
  | Major_allocated
  | Promoted
  | Major_collections
  | Minor_collections

let result_column c m =
  let v =
    match c with
    | Cycles -> m.cycles
    | Nanos -> m.nanos
    | Compactions -> m.compactions
    | Minor_allocated -> m.minor_allocated
    | Major_allocated -> m.major_allocated
    | Promoted -> m.promoted
    | Major_collections -> m.major_collections
    | Minor_collections -> m.minor_collections
  in
  m.runs, float v

type result =
    { mean_value : float;
      constant : float }

let analyse_measurements ml c =
  let a = Array.of_list (List.map (result_column c) ml) in
  let mean_value, constant = affine_adjustment a in
    { mean_value;
      constant }
