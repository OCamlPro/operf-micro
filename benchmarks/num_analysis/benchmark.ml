open Micro_bench_types

let prepare i = i
let check i n = Ok
let do_not_check _ _ = Ok

let functions =
  [ "durand-kerner-aberth-test", Unit ((fun () -> Durand_kerner_aberth.main 50),
                                       Dka_result.check, Longer);
    "durand-kerner-aberth", Int (Durand_kerner_aberth.main, prepare, 
                                 do_not_check, [ Range (90, 110), Longer ]);
    "k-means", Unit (K_means.main, (fun _ -> Ok), Longer);
    "fft-test", Unit ((fun () -> Fft.main (32 * 32)), Fft_result.check,
                      Long);
    "fft", Int (Fft.main, prepare, do_not_check,
                [ Range (1024, (1024 * 1024)), Long ]);
    "levinson-durbin-test", Unit ((fun () -> Levinson_durbin.main 100),
                                  Levinson_durbin_result.check, Long);
    "levinson-durbin", Int (Levinson_durbin.main, prepare, check,
                           [ Range (1000, 10000), Longer]);
    "lu-decomposition", Unit (Lu_decomposition.main, Lu_decomposition_result.check, Longer);
    "naive-multilayer", Unit (Naive_multilayer.main, (fun _ -> Ok), Longer);
    "qr-decomposition", Unit (Qr_decomposition.main, 
                              Qr_decomposition_result.check, Longer);
    "rnd-access", Int (Rnd_access.main, prepare, check,
                           [ Range (1000000, 2000000), Longer]);
    "simple-access", Int (Simple_access.main, prepare, check,
                           [ Range (1000000, 2000000), Longer]);
  ]

let () = add functions
