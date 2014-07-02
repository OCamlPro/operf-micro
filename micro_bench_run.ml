(* Extracted from JaneStreet's core-bench library.
   Modified to fit in a single file *)

let compare_cost c1 c2 =
  let open Micro_bench_types in
  match c1, c2 with
  | Short, Short -> 0
  | Short, (Long | Longer) -> -1
  | (Long | Longer), Short -> 1
  | Long, Long -> 0
  | Long, Longer -> -1
  | Longer, Long -> 1
  | Longer, Longer -> 0

module StringSet = struct
  include Set.Make(String)
  let of_list l = List.fold_right add l empty
end

module Float = struct
  let iround_lbound = float_of_int min_int
  let iround_ubound = min (float_of_int max_int) (2.0 ** 62.0 -. 512.)
  let iround_towards_zero_exn t =
    if t >= iround_lbound && t <= iround_ubound then
      int_of_float t
    else
      invalid_arg
        (Printf.sprintf
           "Float.iround_towards_zero_exn: argument (%f) is out of range or NaN" t)
end

module Time : sig
  type t
  val now : unit -> t

  module Span : sig
    type t
    val of_sec : float -> t
    val to_float : t -> float
    val to_ns : t -> float
    val to_string : t -> string
  end

  val diff : t -> t -> Span.t

end = struct

  type t = float
  let now () = Sys.time ()
  let diff t1 t2 = t1 -. t2

  module Constant = struct
    let nanoseconds_per_second = 1E9
    let microseconds_per_second = 1E6
    let milliseconds_per_second = 1E3
  end

  module Span = struct
    type t = float
    let of_sec v = v
    let to_float v = v
    let to_ns v = v *. Constant.nanoseconds_per_second
    let to_ms v = v *. Constant.milliseconds_per_second
    let to_string v = Printf.sprintf "%fs" v
  end
end

module Defaults =
struct

  (* how to measure *)
  let geometric_scale = 1.01
  let stabilize_gc_between_runs = false
  let no_compactions = false

  (* how long to measure *)
  let time_quota_float = 10.0
  let time_quota = Time.Span.of_sec time_quota_float

  (* which test to run *)
  let number_of_different_values = 1
  let maximal_cost = Micro_bench_types.Short
  let selection = None
  let test_only = false

  let verbosity = `Low
end

module Config =
struct
  type t = {
    verbosity:[ `High | `Low ];
    no_compactions:bool;
    time_quota:Time.Span.t;
    sampling_type:[`Geometric of float | `Linear of int];
    stabilize_gc_between_runs:bool;
    number_of_different_values:int;
    maximal_cost:Micro_bench_types.cost;
    selection:StringSet.t option;
    test_only:bool;
  }

  let create
      ?(verbosity=Defaults.verbosity)
      ?(no_compactions=Defaults.no_compactions)
      ?(time_quota=Defaults.time_quota)
      ?(sampling_type=`Geometric Defaults.geometric_scale)
      ?(stabilize_gc_between_runs=Defaults.stabilize_gc_between_runs)
      ?(number_of_different_values=Defaults.number_of_different_values)
      ?(maximal_cost=Defaults.maximal_cost)
      ?(selection=Defaults.selection)
      ?(test_only=Defaults.test_only)
      ()
    =
    { verbosity;
      no_compactions;
      time_quota;
      sampling_type;
      stabilize_gc_between_runs;
      number_of_different_values;
      maximal_cost;
      selection;
      test_only }

  open Arg

  let parse () =
    let verbosity = ref None in
    let no_compactions = ref None in
    let time_quota = ref None in
    let sampling_type = ref None in
    let stabilize_gc_between_runs = ref None in
    let number_of_different_values = ref None in
    let maximal_cost = ref Defaults.maximal_cost in
    let test_only = ref None in

    let set r v = Unit (fun () -> r := Some v) in
    let set_quota = Float (fun v -> time_quota := Some (Time.Span.of_sec v)) in
    let set_sampling = Int (fun i -> sampling_type := Some (`Linear i)) in
    let set_int r = Int (fun i -> r := Some i) in
    let set_cost c = Unit (fun () ->
        if compare_cost !maximal_cost c < 0
        then maximal_cost := c) in

    let list = ref false in

    let selection = ref None in
    let spec =
      [ "-v", set verbosity `High, " high verbosity";
        "--no-compaction", set no_compactions true, " no compaction";
        "--time-quota", set_quota, "t time_quota";
        "-q", set_quota, " alias of --time-quota";
        "--linear-sampling", set_sampling, "n set linear sampling type with the given step size";
        "--stabilize-gc", set stabilize_gc_between_runs true, " stabilize gc between runs";
        "--different-values", set_int number_of_different_values, "n number of different values";
        "-n", set_int number_of_different_values, " alias of --different-values";
        "--long", set_cost Micro_bench_types.Long, " allow running long test";
        "--longer", set_cost Micro_bench_types.Longer, " allow running longer test";

        "--test", set test_only true, " don't run benchmarks";
        "-t", set test_only true, " alias of --test";

        "--list", Set list, " list available benchmarks";
        "-l", Set list, " alias of --list";
      ] in

    let doc =
      Sys.executable_name ^ " [options] [selection]" in

    Arg.parse (Arg.align spec)
      (fun s ->
         match !selection with
         | None -> selection := Some (StringSet.singleton s)
         | Some l -> selection := Some (StringSet.add s l))
      doc;

    if !list
    then `List
    else
      `Run
        (create
           ?verbosity:!verbosity
           ?no_compactions:!no_compactions
           ?time_quota:!time_quota
           ?sampling_type:!sampling_type
           ?stabilize_gc_between_runs:!stabilize_gc_between_runs
           ?number_of_different_values:!number_of_different_values
           ~maximal_cost:!maximal_cost
           ?test_only:!test_only
           ~selection:!selection
           ())

end

module Measurement_sample = struct
  type t = {
    mutable runs              : int;
    mutable cycles            : int;
    mutable nanos             : int;
    mutable compactions       : int;
    mutable minor_allocated   : int;
    mutable major_allocated   : int;
    mutable promoted          : int;
    mutable major_collections : int;
    mutable minor_collections : int;
  }

  let create () = {
    runs              = 0;
    cycles            = 0;
    nanos             = 0;
    compactions       = 0;
    minor_allocated   = 0;
    major_allocated   = 0;
    promoted          = 0;
    major_collections = 0;
    minor_collections = 0;
  }
end

module Measurement = struct
  type t = {
    name         : string;
    largest_run  : int;
    sample_count : int;
    samples      : Measurement_sample.t array
  }

  let create ~name ~largest_run ~sample_count ~samples = {
    name; largest_run; sample_count; samples;
  }

end

let stabilize_gc () =
  let rec loop failsafe last_heap_live_words =
    if failsafe <= 0 then
      failwith "unable to stabilize the number of live words in the major heap";
    Gc.compact ();
    let stat = Gc.stat () in
    if stat.Gc.live_words <> last_heap_live_words
    then loop (failsafe - 1) stat.Gc.live_words
  in
  loop 10 0

module Tester = struct
  open Micro_bench_types

  let pick_any n =
    let num k = (k * k) mod (2003 * 2011) in
    let rd n0 =
      let n1 = num n0 in
      let n2 = num n1 in
      let n3 = num n2 in
      let n4 = num n3 in
      (n1 mod 16) + ((n2 mod 16) lsl 4) +
      ((n3 mod 16) lsl 8) + ((n4 mod 16) lsl 12), n4 in
    let rec aux n k =
      if n = 0
      then []
      else
        let v, k = rd k in
        (v - 32768) :: aux (n-1) k
    in
    aux n 42

  let rec range x y =
    if x > y
    then []
    else x :: range (x+1) y

  let pick_range n (x, y) =
    match n with
    | 0 -> []
    | 1 -> [x]
    | _ ->
      let (x, y) = if x < y then x, y else y, x in
      let d = y - x in
      if d < n
      then range x y
      else
        let v = d / (n-1) in
        List.map (fun i -> x + i * v) (range 0 (n-1))

  let rec pick_list n l =
    if n <= 0
    then []
    else match l with
      | [] -> []
      | h::t -> h :: (pick_list (n-1) t)

  let pick_values n = function
    | Any -> pick_any n
    | Range (x,y) -> pick_range n (x,y)
    | List l -> pick_list n l

  let pick n max_cost l =
    let l = List.filter (fun (_,c) -> compare_cost c max_cost <= 0) l in
    let l = List.sort (fun (_,c1) (_,c2) -> compare_cost c1 c2) l in
    let l = List.map fst l in
    let l = pick_list n l in
    let len = List.length l in
    if len = 0
    then []
    else
      let q, r =
        if n <= len
        then 1, 0
        else n / len, n mod len
      in
      let a =
        Array.mapi (fun i range ->
            let n = if i < r then q + 1 else q in
            pick_values n range) (Array.of_list l) in
      List.concat (Array.to_list a)

  let errors = ref false

  let report name = function
    | Ok -> ()
    | Error e ->
      errors := true;
      Printf.eprintf "test %s failed with message %s\n%!"
        name e

  let report_exception name exn =
    errors := true;
    Printf.eprintf "test %s failed with exception %s\n%!"
      name (Printexc.to_string exn)

  let report_n name n = function
    | Ok -> ()
    | Error e ->
      errors := true;
      Printf.eprintf "test %s with parameter %i failed with message %s\n%!"
        name n e

  let report_exception_n name n exn =
    errors := true;
    Printf.eprintf "test %s with parameter %i failed with exception %s\n%!"
      name n (Printexc.to_string exn)

  let in_selection name = function
    | None -> true
    | Some selection -> StringSet.mem name selection

  let test
      { Config.maximal_cost = max_cost;
        number_of_different_values = n;
        verbosity = verbosity;
        selection = selection } = function
    | name, Unit (f, test, cost) ->
      if compare_cost cost max_cost <= 0 && in_selection name selection
      then begin
        if verbosity = `High
        then Printf.printf "running test %s\n%!" name;
        try
          match test (f ()) with
          | Ok -> ()
          | Error e ->
            errors := true;
            Printf.eprintf "test %s failed with message %s\n%!"
              name e
        with exn ->
          Printf.eprintf "test %s failed with exception %s\n%!"
            name (Printexc.to_string exn)
      end

    | name, Int (f, prepare, test, costs) ->
      let aux v =
        if verbosity = `High
        then Printf.printf "running test %s with argument %i\n%!" name v;
        try
          match test (f (prepare v)) with
          | Ok -> ()
          | Error e ->
            errors := true;
            Printf.eprintf "test %s with parameter %i failed with message %s\n%!"
              name n e
        with exn ->
          Printf.eprintf "test %s with parameter %i failed with exception %s\n%!"
            name n (Printexc.to_string exn)
      in
      if in_selection name selection
      then List.iter aux (pick n max_cost costs)

  let measure_functions
      { Config.maximal_cost = max_cost;
        number_of_different_values = n;
        selection = selection }
      l =
    let aux (name, bench) =
      match bench with
      | Unit (f, test, cost) ->
        if compare_cost cost max_cost <= 0
        then [name, fun () -> ignore (f ())]
        else []
      | Int (f, prepare, test, costs) ->
        let args = pick n max_cost costs in
        List.map (fun arg ->
            let name = Printf.sprintf "%s: %i" name arg in
            let v = prepare arg in
            name, fun () -> ignore (f v))
          args
    in
    let l = match selection with
      | None -> l
      | Some selection -> List.filter (fun (v,_) -> StringSet.mem v selection) l in
    List.concat (List.map aux l)

  let string_of_cost = function
    | Short -> "short"
    | Long -> "long"
    | Longer -> "longer"

  let string_of_range = function
    | Any -> "any"
    | Range(x, y) -> Printf.sprintf "[%i ... %i]" x y
    | List l -> "[" ^ (String.concat "; " (List.map string_of_int l)) ^ "]"

  let list l =
    List.iter (function
        | name, Unit(_,_,cost) ->
          Printf.printf "  %s: %s\n" name (string_of_cost cost)
        | name, Int(_,_,_,costs) ->
          Printf.printf "  %s:\n" name;
          List.iter (fun (range, cost) ->
              Printf.printf "    %s: %s\n"
                (string_of_cost cost)
                (string_of_range range))
            costs)
      l

end

let exceeded_allowed_time allowed_time_span t1 =
  let t2 = Time.now () in
  Time.diff t2 t1 > allowed_time_span

(* The main benchmarking function *)
let measure config (name, f) =
  let module C = Config in
  let module M  = Measurement_sample in

  (* the samples *)
  let max_samples = 3_000 in
  let results = Array.init max_samples (fun _ -> M.create ()) in

  (* counters *)
  let index = ref 0 in
  let runs = ref 0 in

  (* get the old Gc settings *)
  let old_gc = Gc.get () in

  (* THE MAIN TEST LOOP *)
  let init_t1 = Time.now () in
  while not (exceeded_allowed_time (config.C.time_quota) init_t1)
        && !index < Array.length results
  do
    let current_runs = !runs in
    let current_index = !index in

    (* Stabilize gc if required.

       We stabilize the gc through the first pass through this loop anyway. If we don't do
       this the incoming GC state (some data may be on the minor heap that is partly full)
       will cause an early collection or two which will not happen subsequently. These
       early collections are just noise.

       While benchmarking functions that do not allocate any memory this early noise is
       the only significant input. In these cases, these spurious early collections will
       give the allocation stats (major and promoted words) a slight negative value. *)
    if (config.C.stabilize_gc_between_runs) || current_runs = 0 then
      stabilize_gc ();

    (* make any Gc changes required. *)
    if config.C.no_compactions
    then Gc.set { (Gc.get ()) with Gc.max_overhead = 1_000_000 };

    (* pre-run measurements *)
    let gc1 = Gc.quick_stat () in
    let t1 = Time.now () in
    let c1 = Time_stamp_counter.now () in

    (* MEASURE A SINGLE SAMPLE *)
    for _i = 1 to current_runs do
      f ();
    done;
    (* END OF MEASUREMENT *)

    (* post-run measurements *)
    let c2 = Time_stamp_counter.now () in
    let t2 = Time.now () in
    let gc2 = Gc.quick_stat () in

    (* reset the old Gc now that we are done with measurements *)
    Gc.set old_gc;

    (* save measurements *)
    let s = results.(current_index) in
    s.M.runs  <- current_runs;
    s.M.cycles  <- c2 - c1;
    s.M.nanos  <- (Float.iround_towards_zero_exn
                          (Time.Span.to_ns (Time.diff t2 t1)));
    s.M.minor_allocated <- Float.iround_towards_zero_exn
      (gc2.Gc.minor_words -. gc1.Gc.minor_words);
    s.M.major_allocated <- Float.iround_towards_zero_exn
      (gc2.Gc.major_words -. gc1.Gc.major_words);
    s.M.promoted <- Float.iround_towards_zero_exn
      (gc2.Gc.promoted_words -. gc1.Gc.promoted_words);
    s.M.compactions <-
      (gc2.Gc.compactions - gc1.Gc.compactions);
    s.M.major_collections <-
      (gc2.Gc.major_collections - gc1.Gc.major_collections);
    s.M.minor_collections <-
      (gc2.Gc.minor_collections - gc1.Gc.minor_collections);
    incr index;

    (* determine the next number of runs *)
    let next =
      match (config.C.sampling_type) with
      | `Linear k -> current_runs + k
      | `Geometric scale ->
        let next_geometric =
          Float.iround_towards_zero_exn ((float_of_int current_runs) *. scale) in
        max next_geometric (current_runs + 1)
    in
    runs := next;

  done;
  let end_time = Time.now () in
  (* END OF MAIN TEST LOOP *)

  let total_samples = !index in
  let largest_run = !runs in
  let measurement = Measurement.create
                      ~name
                      ~largest_run
                      ~sample_count:total_samples
                      ~samples:results
  in
  if config.C.verbosity = `High
  then
    Printf.eprintf "%s: Total time taken %s (%d samples, max runs %d).\n%!"
      name
      (Time.Span.to_string (Time.diff end_time init_t1))
      total_samples
      largest_run;
  measurement

let measure_all config tests =
  List.map (measure config) tests

let run_tests config functions =
  List.iter (Tester.test config) functions

let run_measures config functions =
  let l = Tester.measure_functions config functions in
  List.map (measure config) l

let run_all config functions =
  begin match config.Config.selection with
    | None -> ()
    | Some selection ->
      let set = StringSet.of_list (List.map fst functions) in
      let absent = StringSet.diff selection set in
      if not (StringSet.is_empty absent)
      then begin
        Printf.eprintf "Those selected functions are not present in this benchmark:\n";
        StringSet.iter (fun s -> Printf.eprintf "  %s\n" s) absent;
        exit 1
      end
  end;
  run_tests config functions;
  if !Tester.errors
  then exit 1
  else
  if config.Config.test_only
  then []
  else run_measures config functions

let run functions =
  match Config.parse () with
  | `List ->
    Printf.printf "benchmarks:\n";
    Tester.list functions
  | `Run config ->
    ignore (run_all config functions)

let () = run (Micro_bench_types.functions ())
