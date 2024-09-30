#define CAML_INTERNALS

#include <caml/version.h>
#include <caml/mlvalues.h>
#if OCAML_VERSION_MAJOR >= 5
# include <caml/fiber.h>
#endif

CAMLprim value caml_picos_bench_stack_size(value _) {
#if OCAML_VERSION_MAJOR >= 5
  struct stack_info *stk = Caml_state->current_stack;
  return Val_int((Stack_high(stk) - (value*)(stk->sp)) * sizeof(value));
#else
  return Val_int(0);
#endif
}
