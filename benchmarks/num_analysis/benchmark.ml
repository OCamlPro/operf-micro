open Micro_bench_types

let prepare i = i
let check i n = Ok

let functions =
  [ "durand-kerner-aberth", Int (Durand_kerner_aberth.main, prepare, check,
                                 [ Range (10, 28), Short;
                                   Range (29, 40), Long;
                                   Range (40, 100), Longer ]);
    "k-means", Unit (K_means.main, (fun _ -> Ok), Longer);
    "fft", Int (Fft.main, prepare, check,
                 [ Range (1024, (1024 * 1024)), Longer ]);
    "levinson-durbin", Int (Levinson_durbin.main, prepare, check,
                           [ Range (1000, 10000), Longer]);
    "lu-decomposition", Unit (Lu_decomposition.main, (fun _ -> Ok), Longer);
    "naive-multilayer", Unit (Naive_multilayer.main, (fun _ -> Ok), Longer);
    "qr-decomposition", Unit (Qr_decomposition.main, (fun _ -> Ok), Longer);
    "rnd-access", Int (Rnd_access.main, prepare, check,
                           [ Range (1000000, 2000000), Longer]);
    "simple-access", Int (Simple_access.main, prepare, check,
                           [ Range (1000000, 2000000), Longer]);
  ]

let () = add functions
