let gather t =
  t.Unix.tms_utime +. t.Unix.tms_stime +.
  t.Unix.tms_cutime +. t.Unix.tms_cstime

let () =

  let c = Gc.get () in
  Gc.set
    { c with Gc.minor_heap_size = 32000000;
             Gc.space_overhead = max_int };

  Gc.major ();

  let t1 = Unix.times () in

  ignore (Bench.main ());

  let t2 = Unix.times () in

  Format.printf "%f\n" (gather t2 -. gather t1)
