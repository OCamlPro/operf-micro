#include <x86intrin.h>
#include <caml/mlvalues.h>

CAMLprim value ocaml_read_time_stamp_counter()
{
  return Val_long( __rdtsc() );
}
