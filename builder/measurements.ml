open Detect_config
open Files
open Utils

type error =
  | Missing_field of string

exception Error of error

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

type measurements = string * (string * string) list * measurement_sample list

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
    let aux (func, params, m) =
      let dict =
        []
        |> f "list" (List (List.map measurement_dict m))
        |> f "name" (String func)
        |> f "properties" (Dict (List.map (fun (k, v) -> k, String v) params))
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

type 'a group =
  | Simple of 'a
  | Group of (string * 'a) list

type runs = {
  name : string;
  list : (measurement_sample list) group;
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
    group : string option;
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
    | "properties", Dict l ->
      List.fold_left (fun r field ->
        match field with
        | "group", String v -> { r with group = Some v }
        | _ -> r)
        r l
    | "list", List l ->
       { r with list = Some (List.map read_measurement_sample l) }
    | _ -> r

  let read_add_runs (no_group, groups) v =
    let empty : runs' = { name = None; group = None; list = None } in
    let r = match v with
      | Dict l -> List.fold_left add_runs_field empty l
      | _ -> failwith "parse error" in
    match r with
    | { name = Some name; group; list = Some list } ->
      begin match group with
        | None -> { name; list = Simple list } :: no_group, groups
        | Some group ->
          let l = try StringMap.find group groups with Not_found -> [] in
          no_group, StringMap.add group ((name, list) :: l) groups
      end
    | _ -> failwith "parse error"

  let add_record_field r (s, v) : recorded_measurements' =
    match s, v with
    | "suite_name", String v -> { r with suite_name = Some v }
    | "name", String v -> { r with name = Some v }
    | "date", String v -> { r with date = Some v }
    | "runs", List l ->
      let no_groups, groups =
        List.fold_left read_add_runs ([],StringMap.empty) l in
      let groups = StringMap.fold
          (fun group l acc -> { name = group; list = Group l } :: acc)
          groups no_groups in
      { r with run_list = Some groups }
    | _ -> r

  let missing_field s =
    raise (Error (Missing_field s))

  let read_record v : recorded_measurements =
    let empty : recorded_measurements' =
      { suite_name = None; name = None; date = None; run_list = None } in
    let r = match v with
      | Dict l -> List.fold_left add_record_field empty l
      | _ -> failwith "Not a dictionnary" in
    match r with
    | { suite_name = Some suite_name;
        name = Some name;
        date = Some date;
        run_list = Some run_list } ->
       { suite_name; name; run_list; date }
    | { suite_name = None; _ } ->
       missing_field "suite_name"
    | { name = None; _ } ->
       missing_field "name"
    | { date = None; _ } ->
       missing_field "name"
    | { run_list = None; _ } ->
       missing_field "runs"

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

(* cut_field "field_name: value" = "field_name", "value" *)
let cut_field s =
  let i = String.index s ':' in
  let len = String.length s in
  let rec last_space i =
    if i >= len
    then i
    else if s.[i] = ' '
    then last_space (i+1)
    else i
  in
  let j = last_space (i+1) in
  String.sub s 0 i,
  String.sub s j (String.length s - j)

let read_measurement ~contents:text : measurements list =
  let error s =
    Printf.eprintf "malformed input line: %s\n%!" s;
    raise Exit
  in
  let rec aux l curr_name curr_params curr acc : measurements list =
    match l with
    | [] ->
      (curr_name, curr_params, curr) :: acc
    | "" :: t ->
      aux_name t ((curr_name, curr_params, curr) :: acc)
    | h :: t ->
      if h.[0] = '#'
      then aux t curr_name curr_params curr acc
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
            aux t curr_name curr_params (m::curr) acc
          | _ -> error h
        with _ -> error h

  and aux_name l acc : measurements list =
    match l with
    | [] -> acc
    | "" :: t ->
      aux_name t acc
    | name :: t ->
      aux_params name [] t acc

  and aux_params name params l acc : measurements list =
    match l with
    | [] ->
      Printf.eprintf "Warning: unexpected end of file\n%!";
      acc
    | "" :: t ->
      aux t name params [] acc
    | param :: t ->
      let params =
        try
          let param_name, param_value = cut_field param in
          (param_name, param_value) :: params
        with Not_found -> error param
      in
      aux_params name params t acc
  in
  aux_name (lines text) []


type results_mat = (int * float) array

(* returns [a, b] such that [f(x) = a*x + b] minimize
   the distance between [sum(fun (x -> (f(x) - v(x))^2)] *)
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

let sum a = Array.fold_left (+.) 0. a

let standard_error ~a ~b (r:results_mat) =
  let estimate x = a *. float x +. b in
  let dy (x, y) =
    let d = y -. estimate x in
    d *. d
  in
  let sum_dy = sum (Array.map dy r) in
  let mean_x =
    sum (Array.map (fun (x, _) -> float x) r) /. float (Array.length r)
  in
  let dx (x,_) = let d = float x -. mean_x in d *. d in
  sqrt (sum_dy /. float (Array.length r - 2)) /.
  sqrt (sum (Array.map dx r))

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
      constant : float;
      max_value : int * float;
      min_value : int * float;
      standard_error : float }

let analyse_measurement c ml =
  let a = Array.of_list (List.map (result_column c) ml) in
  let mean_value, constant = affine_adjustment a in
  let min_value =
    Array.fold_left (fun (row_min, val_min) (row,value) ->
      let value = (value -. constant) /. float row in
      if val_min < value
      then (row_min, val_min)
      else (row,value))
      (0, max_float) a
  in
  let max_value =
    Array.fold_left (fun (row_max, val_max) (row,value) ->
      let value = (value -. constant) /. float row in
      if val_max > value
      then (row_max, val_max)
      else (row,value))
      (0, min_float) a
  in
  let standard_error = standard_error ~a:mean_value ~b:constant a in
  { mean_value;
    constant;
    min_value;
    max_value;
    standard_error }

let analyse_measurements c (rm:recorded_measurements) =
  List.fold_left
    (fun map { name; list } ->
       let elt =
         match list with
         | Simple list ->
           Simple (analyse_measurement c list)
         | Group functions ->
           Group (List.map (fun (name, list) -> name, analyse_measurement c list) functions)
       in
       StringMap.add name elt map)
    StringMap.empty rm.run_list

let load_results c files =
  let aux map filename =
    let name = Filename.chop_extension (Filename.basename filename) in
    let file = read_measurement_file ~filename in
    StringMap.add name (analyse_measurements c file) map
  in
  List.fold_left aux StringMap.empty files

let compare_measurements ?reference results =
  let reference, ref_value =
    match reference with
    | Some reference when StringMap.mem reference results ->
      reference, StringMap.find reference results
    | Some reference ->
      Format.eprintf "Warning: %s does not belongs to the results" reference;
      StringMap.min_binding results
    | None ->
      StringMap.min_binding results
  in
  let merge _key reference value =
    match reference, value with
    | None, _ -> None
    | Some (Simple reference), Some (Simple result) ->
      Some (Simple (Some (result.mean_value /. reference.mean_value)))
    | Some (Group reference), Some (Group result) ->
      let reference = stringmap_of_list reference in
      let result = stringmap_of_list result in
      let aux _key reference result =
        match reference, result with
        | None, _ -> None
        | Some _, None -> Some None
        | Some reference, Some result ->
          Some (Some (result.mean_value /. reference.mean_value))
      in
      let map = StringMap.merge aux reference result in
      Some (Group (StringMap.bindings map))
    | Some (Simple _), _ -> Some (Simple None)
    | Some (Group l), _ -> Some (Group (List.map (fun (name, _) -> name, None) l))
  in
  let merge2 _key reference value =
    match reference, value with
    | None, _ -> None
    | Some reference, None -> Some (StringMap.merge merge reference StringMap.empty)
    | Some reference, Some result -> Some (StringMap.merge merge reference result)
  in
  let aux results = StringMap.merge merge2 ref_value results in
  reference, StringMap.map aux results

