open Micro_bench_types

let functions =
  [ "reverse int bigarray",
    Int (Utils.rev,
         Utils.create_int,
         Utils.check_int,
         [Range (0,10000), Fast;
          Range (10001, 1000000), Slow;
          Range (1000001, 100000000), Slower]);
    "reverse int32 bigarray",
    Int (Utils.rev,
         Utils.create_int32,
         Utils.check_int32,
         [Range (0,10000), Fast;
          Range (10001, 1000000), Slow;
          Range (1000001, 100000000), Slower]) ]
