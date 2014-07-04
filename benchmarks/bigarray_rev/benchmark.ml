open Micro_bench_types

let functions =
  [ "reverse int bigarray",
    Int (Utils.rev,
         Utils.create_int,
         Utils.check_int,
         [Range (0,10000), Short;
          Range (10001, 1000000), Long;
          Range (1000001, 100000000), Longer]);
    "reverse int32 bigarray",
    Int (Utils.rev,
         Utils.create_int32,
         Utils.check_int32,
         [Range (0,10000), Short;
          Range (10001, 1000000), Long;
          Range (1000001, 100000000), Longer]) ]

let () = add functions
