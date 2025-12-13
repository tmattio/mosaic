#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include "tree_sitter/api.h"

/* Declaration of the tree-sitter JSON language function */
const TSLanguage *tree_sitter_json(void);

CAMLprim value caml_tree_sitter_json_language(value unit) {
  CAMLparam1(unit);
  const TSLanguage *lang = tree_sitter_json();
  CAMLreturn(caml_copy_nativeint((intnat)lang));
}
