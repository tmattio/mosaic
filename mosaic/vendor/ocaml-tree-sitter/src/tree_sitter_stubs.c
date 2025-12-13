#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "tree_sitter/api.h"

#if defined(__unix__) || defined(__APPLE__) || defined(__linux__)
#include <dlfcn.h>
#define TS_OCAML_HAVE_DLOPEN 1
#endif

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "alloc.h"

struct ts_language_handle {
  const TSLanguage* language;
#ifdef TS_OCAML_HAVE_DLOPEN
  void* dl_handle;
#endif
};

struct ts_parser_handle {
  TSParser* parser;
};

struct ts_tree_handle {
  TSTree* tree;
};

struct ts_node_handle {
  TSNode node;
};

struct ts_query_handle {
  TSQuery* query;
};

struct ts_query_cursor_handle {
  TSQueryCursor* cursor;
};

#define Language_val(v) ((struct ts_language_handle*)Data_custom_val(v))
#define Parser_val(v) ((struct ts_parser_handle*)Data_custom_val(v))
#define Tree_val(v) ((struct ts_tree_handle*)Data_custom_val(v))
#define Node_val(v) ((struct ts_node_handle*)Data_custom_val(v))
#define Query_val(v) ((struct ts_query_handle*)Data_custom_val(v))
#define QueryCursor_val(v) ((struct ts_query_cursor_handle*)Data_custom_val(v))

static void finalize_language(value v) {
  struct ts_language_handle* handle = Language_val(v);
#ifdef TS_OCAML_HAVE_DLOPEN
  if (handle->dl_handle != NULL) {
    dlclose(handle->dl_handle);
    handle->dl_handle = NULL;
  }
#endif
  handle->language = NULL;
}

static void finalize_parser(value v) {
  struct ts_parser_handle* handle = Parser_val(v);
  if (handle->parser != NULL) {
    ts_parser_delete(handle->parser);
    handle->parser = NULL;
  }
}

static void finalize_tree(value v) {
  struct ts_tree_handle* handle = Tree_val(v);
  if (handle->tree != NULL) {
    ts_tree_delete(handle->tree);
    handle->tree = NULL;
  }
}

static void finalize_query(value v) {
  struct ts_query_handle* handle = Query_val(v);
  if (handle->query != NULL) {
    ts_query_delete(handle->query);
    handle->query = NULL;
  }
}

static void finalize_query_cursor(value v) {
  struct ts_query_cursor_handle* handle = QueryCursor_val(v);
  if (handle->cursor != NULL) {
    ts_query_cursor_delete(handle->cursor);
    handle->cursor = NULL;
  }
}

static struct custom_operations language_ops = {
    .identifier = "tree_sitter.language",
    .finalize = finalize_language,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static struct custom_operations parser_ops = {
    .identifier = "tree_sitter.parser",
    .finalize = finalize_parser,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static struct custom_operations tree_ops = {
    .identifier = "tree_sitter.tree",
    .finalize = finalize_tree,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static struct custom_operations node_ops = {
    .identifier = "tree_sitter.node",
    .finalize = NULL,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static struct custom_operations query_ops = {
    .identifier = "tree_sitter.query",
    .finalize = finalize_query,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static struct custom_operations query_cursor_ops = {
    .identifier = "tree_sitter.query_cursor",
    .finalize = finalize_query_cursor,
    .compare = custom_compare_default,
    .compare_ext = custom_compare_ext_default,
    .hash = custom_hash_default,
    .serialize = custom_serialize_default,
    .deserialize = custom_deserialize_default,
    .fixed_length = false,
};

static value alloc_language(const TSLanguage* language) {
  value v = caml_alloc_custom_mem(&language_ops,
                                  sizeof(struct ts_language_handle), 0);
  struct ts_language_handle* handle = Language_val(v);
  handle->language = language;
#ifdef TS_OCAML_HAVE_DLOPEN
  handle->dl_handle = NULL;
#endif
  return v;
}

static value alloc_parser(TSParser* parser) {
  value v =
      caml_alloc_custom_mem(&parser_ops, sizeof(struct ts_parser_handle), 0);
  struct ts_parser_handle* handle = Parser_val(v);
  handle->parser = parser;
  return v;
}

static value alloc_tree(TSTree* tree) {
  value v = caml_alloc_custom_mem(&tree_ops, sizeof(struct ts_tree_handle), 0);
  struct ts_tree_handle* handle = Tree_val(v);
  handle->tree = tree;
  return v;
}

static value alloc_node(TSNode node) {
  value v = caml_alloc_custom_mem(&node_ops, sizeof(struct ts_node_handle), 0);
  struct ts_node_handle* handle = Node_val(v);
  handle->node = node;
  return v;
}

static value alloc_query(TSQuery* query) {
  value v =
      caml_alloc_custom_mem(&query_ops, sizeof(struct ts_query_handle), 0);
  struct ts_query_handle* handle = Query_val(v);
  handle->query = query;
  return v;
}

static value alloc_query_cursor(TSQueryCursor* cursor) {
  value v = caml_alloc_custom_mem(&query_cursor_ops,
                                  sizeof(struct ts_query_cursor_handle), 0);
  struct ts_query_cursor_handle* handle = QueryCursor_val(v);
  handle->cursor = cursor;
  return v;
}

static value caml_ts_optional_node(TSNode node) {
  CAMLparam0();
  CAMLlocal2(result, node_value);
  if (ts_node_is_null(node)) {
    CAMLreturn(Val_int(0));
  }
  node_value = alloc_node(node);
  result = caml_alloc(1, 0);
  Store_field(result, 0, node_value);
  CAMLreturn(result);
}

static value caml_ts_copy_point(TSPoint point) {
  CAMLparam0();
  CAMLlocal1(res);
  res = caml_alloc_tuple(2);
  Store_field(res, 0, Val_int(point.row));
  Store_field(res, 1, Val_int(point.column));
  CAMLreturn(res);
}

static TSPoint caml_ts_point_of_value(value v) {
  TSPoint point = {0, 0};
  if (Is_block(v) && Wosize_val(v) == 2) {
    point.row = (uint32_t)Int_val(Field(v, 0));
    point.column = (uint32_t)Int_val(Field(v, 1));
  }
  return point;
}

static value caml_ts_range_to_value(const TSRange* range) {
  CAMLparam0();
  CAMLlocal1(record);
  record = caml_alloc(4, 0);
  Store_field(record, 0, Val_int(range->start_byte));
  Store_field(record, 1, Val_int(range->end_byte));
  Store_field(record, 2, caml_ts_copy_point(range->start_point));
  Store_field(record, 3, caml_ts_copy_point(range->end_point));
  CAMLreturn(record);
}

CAMLprim value caml_ts_language_of_address(value addr) {
  CAMLparam1(addr);
  const TSLanguage* language =
      (const TSLanguage*)(uintptr_t)Nativeint_val(addr);
  if (language == NULL) {
    caml_failwith("Tree_sitter.Language.of_address: null pointer");
  }
  CAMLreturn(alloc_language(language));
}

CAMLprim value caml_ts_language_load(value path, value symbol) {
  CAMLparam2(path, symbol);
#ifdef TS_OCAML_HAVE_DLOPEN
  const char* path_c = String_val(path);
  const char* symbol_c = String_val(symbol);
  void* handle = dlopen(path_c, RTLD_NOW | RTLD_LOCAL);
  if (handle == NULL) {
    const char* err = dlerror();
    caml_failwith(err ? err : "dlopen failed");
  }
  const TSLanguage* (*lang_fn)(void) =
      (const TSLanguage* (*)())dlsym(handle, symbol_c);
  if (lang_fn == NULL) {
    const char* err = dlerror();
    dlclose(handle);
    caml_failwith(err ? err : "dlsym failed");
  }
  const TSLanguage* language = lang_fn();
  if (language == NULL) {
    dlclose(handle);
    caml_failwith("tree-sitter symbol returned null language");
  }
  value v = caml_alloc_custom_mem(&language_ops,
                                  sizeof(struct ts_language_handle), 0);
  struct ts_language_handle* lang_handle = Language_val(v);
  lang_handle->dl_handle = handle;
  lang_handle->language = language;
  CAMLreturn(v);
#else
  caml_failwith(
      "Tree_sitter.Language.load_shared: dlopen not supported on this "
      "platform");
#endif
}

CAMLprim value caml_ts_language_version(value language_v) {
  CAMLparam1(language_v);
  const TSLanguage* language = Language_val(language_v)->language;
  CAMLreturn(Val_int(ts_language_version(language)));
}

CAMLprim value caml_ts_language_name(value language_v) {
  CAMLparam1(language_v);
  const TSLanguage* language = Language_val(language_v)->language;
  const char* name = ts_language_name(language);
  CAMLreturn(caml_copy_string(name == NULL ? "" : name));
}

CAMLprim value caml_ts_language_symbol_count(value language_v) {
  CAMLparam1(language_v);
  const TSLanguage* language = Language_val(language_v)->language;
  CAMLreturn(Val_int(ts_language_symbol_count(language)));
}

CAMLprim value caml_ts_language_state_count(value language_v) {
  CAMLparam1(language_v);
  const TSLanguage* language = Language_val(language_v)->language;
  CAMLreturn(Val_int(ts_language_state_count(language)));
}

CAMLprim value caml_ts_language_symbol_name(value language_v, value symbol_v) {
  CAMLparam2(language_v, symbol_v);
  const TSLanguage* language = Language_val(language_v)->language;
  TSSymbol symbol = (TSSymbol)Int_val(symbol_v);
  const char* name = ts_language_symbol_name(language, symbol);
  CAMLreturn(caml_copy_string(name == NULL ? "" : name));
}

CAMLprim value caml_ts_language_symbol_for_name(value language_v, value name_v,
                                                value is_named_v) {
  CAMLparam3(language_v, name_v, is_named_v);
  CAMLlocal1(some);
  const TSLanguage* language = Language_val(language_v)->language;
  const char* name = String_val(name_v);
  uint32_t length = (uint32_t)caml_string_length(name_v);
  bool is_named = Bool_val(is_named_v);
  TSSymbol symbol =
      ts_language_symbol_for_name(language, name, length, is_named);
  const char* resolved = ts_language_symbol_name(language, symbol);
  value ret;
  if (symbol == 0 && (resolved == NULL || strcmp(resolved, name) != 0)) {
    ret = Val_int(0);
  } else {
    some = caml_alloc(1, 0);
    Store_field(some, 0, Val_int(symbol));
    ret = some;
  }
  CAMLreturn(ret);
}

CAMLprim value caml_ts_language_symbol_type(value language_v, value symbol_v) {
  CAMLparam2(language_v, symbol_v);
  const TSLanguage* language = Language_val(language_v)->language;
  TSSymbol symbol = (TSSymbol)Int_val(symbol_v);
  TSSymbolType type = ts_language_symbol_type(language, symbol);
  switch (type) {
    case TSSymbolTypeRegular:
      CAMLreturn(Val_int(0));
    case TSSymbolTypeAnonymous:
      CAMLreturn(Val_int(1));
    case TSSymbolTypeSupertype:
      CAMLreturn(Val_int(2));
    case TSSymbolTypeAuxiliary:
      CAMLreturn(Val_int(3));
    default:
      caml_failwith("Tree_sitter.Language.symbol_type: unknown symbol type");
  }
}

CAMLprim value caml_ts_language_field_count(value language_v) {
  CAMLparam1(language_v);
  const TSLanguage* language = Language_val(language_v)->language;
  CAMLreturn(Val_int(ts_language_field_count(language)));
}

CAMLprim value caml_ts_language_field_name_for_id(value language_v,
                                                  value id_v) {
  CAMLparam2(language_v, id_v);
  CAMLlocal2(result, some);
  const TSLanguage* language = Language_val(language_v)->language;
  TSFieldId field_id = (TSFieldId)Int_val(id_v);
  const char* name = ts_language_field_name_for_id(language, field_id);
  value ret;
  if (name == NULL) {
    ret = Val_int(0);
  } else {
    result = caml_copy_string(name);
    some = caml_alloc(1, 0);
    Store_field(some, 0, result);
    ret = some;
  }
  CAMLreturn(ret);
}

CAMLprim value caml_ts_language_field_id_for_name(value language_v,
                                                  value name_v) {
  CAMLparam2(language_v, name_v);
  CAMLlocal1(some);
  const TSLanguage* language = Language_val(language_v)->language;
  const char* name = String_val(name_v);
  uint32_t length = (uint32_t)caml_string_length(name_v);
  TSFieldId field_id = ts_language_field_id_for_name(language, name, length);
  value ret;
  if (field_id == 0) {
    ret = Val_int(0);
  } else {
    some = caml_alloc(1, 0);
    Store_field(some, 0, Val_int(field_id));
    ret = some;
  }
  CAMLreturn(ret);
}

CAMLprim value caml_ts_tree_root_node(value tree_v) {
  CAMLparam1(tree_v);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.root_node: tree is closed");
  }
  TSNode root = ts_tree_root_node(tree->tree);
  CAMLreturn(alloc_node(root));
}

CAMLprim value caml_ts_parser_new(value unit) {
  CAMLparam1(unit);
  TSParser* parser = ts_parser_new();
  if (parser == NULL) {
    caml_failwith("ts_parser_new returned NULL");
  }
  CAMLreturn(alloc_parser(parser));
}

CAMLprim value caml_ts_parser_set_language(value parser_v, value language_v) {
  CAMLparam2(parser_v, language_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  const TSLanguage* language = Language_val(language_v)->language;
  if (parser->parser == NULL || language == NULL) {
    caml_failwith(
        "Tree_sitter.Parser.set_language: invalid parser or language");
  }
  if (!ts_parser_set_language(parser->parser, language)) {
    caml_failwith("ts_parser_set_language failed");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_parser_parse_string(value parser_v, value source_v) {
  CAMLparam2(parser_v, source_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser == NULL) {
    caml_failwith("Tree_sitter.Parser.parse_string: parser is closed");
  }
  const char* source = String_val(source_v);
  size_t length = caml_string_length(source_v);
  TSTree* tree =
      ts_parser_parse_string(parser->parser, NULL, source, (uint32_t)length);
  if (tree == NULL) {
    caml_failwith("ts_parser_parse_string returned NULL");
  }
  CAMLreturn(alloc_tree(tree));
}

CAMLprim value caml_ts_parser_parse_string_old(value parser_v,
                                               value old_tree_opt,
                                               value source_v) {
  CAMLparam3(parser_v, old_tree_opt, source_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser == NULL) {
    caml_failwith("Tree_sitter.Parser.parse_string: parser is closed");
  }
  const TSTree* old_tree = NULL;
  if (old_tree_opt != Val_int(0)) {
    struct ts_tree_handle* old_handle = Tree_val(Field(old_tree_opt, 0));
    old_tree = old_handle->tree;
  }
  const char* source = String_val(source_v);
  size_t length = caml_string_length(source_v);
  TSTree* tree = ts_parser_parse_string(parser->parser, old_tree, source,
                                        (uint32_t)length);
  if (tree == NULL) {
    caml_failwith("ts_parser_parse_string returned NULL");
  }
  CAMLreturn(alloc_tree(tree));
}

CAMLprim value caml_ts_parser_set_timeout_micros(value parser_v,
                                                 value timeout_v) {
  CAMLparam2(parser_v, timeout_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser == NULL) {
    caml_failwith("Tree_sitter.Parser.set_timeout_micros: parser is closed");
  }
  uint64_t timeout = (uint64_t)Int64_val(timeout_v);
  ts_parser_set_timeout_micros(parser->parser, timeout);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_parser_timeout_micros(value parser_v) {
  CAMLparam1(parser_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser == NULL) {
    caml_failwith("Tree_sitter.Parser.timeout_micros: parser is closed");
  }
  uint64_t timeout = ts_parser_timeout_micros(parser->parser);
  CAMLreturn(caml_copy_int64((int64_t)timeout));
}

CAMLprim value caml_ts_parser_reset(value parser_v) {
  CAMLparam1(parser_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser == NULL) {
    caml_failwith("Tree_sitter.Parser.reset: parser is closed");
  }
  ts_parser_reset(parser->parser);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_node_is_null(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_is_null(node)));
}

CAMLprim value caml_ts_node_type(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  const char* type = ts_node_type(node);
  CAMLreturn(caml_copy_string(type == NULL ? "" : type));
}

CAMLprim value caml_ts_node_symbol(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_int(ts_node_symbol(node)));
}

CAMLprim value caml_ts_node_start_byte(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(caml_copy_int32((int32_t)ts_node_start_byte(node)));
}

CAMLprim value caml_ts_node_end_byte(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(caml_copy_int32((int32_t)ts_node_end_byte(node)));
}

CAMLprim value caml_ts_node_start_point(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSPoint point = ts_node_start_point(node);
  CAMLreturn(caml_ts_copy_point(point));
}

CAMLprim value caml_ts_node_end_point(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSPoint point = ts_node_end_point(node);
  CAMLreturn(caml_ts_copy_point(point));
}

CAMLprim value caml_ts_node_child_count(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(caml_copy_int32((int32_t)ts_node_child_count(node)));
}

CAMLprim value caml_ts_node_named_child_count(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(caml_copy_int32((int32_t)ts_node_named_child_count(node)));
}

CAMLprim value caml_ts_node_child(value node_v, value index_v) {
  CAMLparam2(node_v, index_v);
  CAMLlocal2(result, child_value);
  TSNode node = Node_val(node_v)->node;
  uint32_t index = (uint32_t)Unsigned_long_val(index_v);
  TSNode child = ts_node_child(node, index);
  if (ts_node_is_null(child)) {
    CAMLreturn(Val_int(0));
  }
  child_value = alloc_node(child);
  result = caml_alloc(1, 0);
  Store_field(result, 0, child_value);
  CAMLreturn(result);
}

CAMLprim value caml_ts_node_named_child(value node_v, value index_v) {
  CAMLparam2(node_v, index_v);
  CAMLlocal2(result, child_value);
  TSNode node = Node_val(node_v)->node;
  uint32_t index = (uint32_t)Unsigned_long_val(index_v);
  TSNode child = ts_node_named_child(node, index);
  if (ts_node_is_null(child)) {
    CAMLreturn(Val_int(0));
  }
  child_value = alloc_node(child);
  result = caml_alloc(1, 0);
  Store_field(result, 0, child_value);
  CAMLreturn(result);
}

CAMLprim value caml_ts_node_parent(value node_v) {
  CAMLparam1(node_v);
  CAMLlocal2(result, parent_value);
  TSNode node = Node_val(node_v)->node;
  TSNode parent = ts_node_parent(node);
  if (ts_node_is_null(parent)) {
    CAMLreturn(Val_int(0));
  }
  parent_value = alloc_node(parent);
  result = caml_alloc(1, 0);
  Store_field(result, 0, parent_value);
  CAMLreturn(result);
}

CAMLprim value caml_ts_node_descendant_for_byte_range(value node_v,
                                                      value start_v,
                                                      value end_v) {
  CAMLparam3(node_v, start_v, end_v);
  CAMLlocal2(result, node_value);
  TSNode node = Node_val(node_v)->node;
  uint32_t start_byte = (uint32_t)Unsigned_long_val(start_v);
  uint32_t end_byte = (uint32_t)Unsigned_long_val(end_v);
  TSNode descendant =
      ts_node_descendant_for_byte_range(node, start_byte, end_byte);
  if (ts_node_is_null(descendant)) {
    CAMLreturn(Val_int(0));
  }
  node_value = alloc_node(descendant);
  result = caml_alloc(1, 0);
  Store_field(result, 0, node_value);
  CAMLreturn(result);
}

CAMLprim value caml_ts_node_descendant_for_point_range(value node_v,
                                                       value start_point_v,
                                                       value end_point_v) {
  CAMLparam3(node_v, start_point_v, end_point_v);
  CAMLlocal2(result, node_value);
  TSNode node = Node_val(node_v)->node;
  TSPoint start_point = caml_ts_point_of_value(start_point_v);
  TSPoint end_point = caml_ts_point_of_value(end_point_v);
  TSNode descendant =
      ts_node_descendant_for_point_range(node, start_point, end_point);
  if (ts_node_is_null(descendant)) {
    CAMLreturn(Val_int(0));
  }
  node_value = alloc_node(descendant);
  result = caml_alloc(1, 0);
  Store_field(result, 0, node_value);
  CAMLreturn(result);
}

CAMLprim value caml_ts_node_to_sexp(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  char* sexp = ts_node_string(node);
  if (sexp == NULL) {
    caml_failwith("ts_node_string returned NULL");
  }
  value result = caml_copy_string(sexp);
  free(sexp);
  CAMLreturn(result);
}

CAMLprim value caml_ts_node_is_named(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_is_named(node)));
}

CAMLprim value caml_ts_node_is_missing(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_is_missing(node)));
}

CAMLprim value caml_ts_node_is_extra(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_is_extra(node)));
}

CAMLprim value caml_ts_node_has_changes(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_has_changes(node)));
}

CAMLprim value caml_ts_node_has_error(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_has_error(node)));
}

CAMLprim value caml_ts_node_is_error(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_bool(ts_node_is_error(node)));
}

CAMLprim value caml_ts_node_parse_state(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_int(ts_node_parse_state(node)));
}

CAMLprim value caml_ts_node_next_parse_state(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(Val_int(ts_node_next_parse_state(node)));
}

CAMLprim value caml_ts_node_child_with_descendant(value node_v,
                                                  value descendant_v) {
  CAMLparam2(node_v, descendant_v);
  TSNode node = Node_val(node_v)->node;
  TSNode descendant = Node_val(descendant_v)->node;
  TSNode result = ts_node_child_with_descendant(node, descendant);
  CAMLreturn(caml_ts_optional_node(result));
}

CAMLprim value caml_ts_node_field_name_for_child(value node_v, value index_v) {
  CAMLparam2(node_v, index_v);
  CAMLlocal2(result, some);
  TSNode node = Node_val(node_v)->node;
  uint32_t index = (uint32_t)Unsigned_long_val(index_v);
  const char* name = ts_node_field_name_for_child(node, index);
  if (name == NULL) {
    CAMLreturn(Val_int(0));
  }
  result = caml_copy_string(name);
  some = caml_alloc(1, 0);
  Store_field(some, 0, result);
  CAMLreturn(some);
}

CAMLprim value caml_ts_node_field_name_for_named_child(value node_v,
                                                       value index_v) {
  CAMLparam2(node_v, index_v);
  CAMLlocal2(result, some);
  TSNode node = Node_val(node_v)->node;
  uint32_t index = (uint32_t)Unsigned_long_val(index_v);
  const char* name = ts_node_field_name_for_named_child(node, index);
  if (name == NULL) {
    CAMLreturn(Val_int(0));
  }
  result = caml_copy_string(name);
  some = caml_alloc(1, 0);
  Store_field(some, 0, result);
  CAMLreturn(some);
}

CAMLprim value caml_ts_node_child_by_field_name(value node_v, value name_v) {
  CAMLparam2(node_v, name_v);
  TSNode node = Node_val(node_v)->node;
  const char* name = String_val(name_v);
  uint32_t length = (uint32_t)caml_string_length(name_v);
  TSNode child = ts_node_child_by_field_name(node, name, length);
  CAMLreturn(caml_ts_optional_node(child));
}

CAMLprim value caml_ts_node_child_by_field_id(value node_v, value id_v) {
  CAMLparam2(node_v, id_v);
  TSNode node = Node_val(node_v)->node;
  TSFieldId field_id = (TSFieldId)Int_val(id_v);
  TSNode child = ts_node_child_by_field_id(node, field_id);
  CAMLreturn(caml_ts_optional_node(child));
}

CAMLprim value caml_ts_node_next_sibling(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSNode sibling = ts_node_next_sibling(node);
  CAMLreturn(caml_ts_optional_node(sibling));
}

CAMLprim value caml_ts_node_prev_sibling(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSNode sibling = ts_node_prev_sibling(node);
  CAMLreturn(caml_ts_optional_node(sibling));
}

CAMLprim value caml_ts_node_next_named_sibling(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSNode sibling = ts_node_next_named_sibling(node);
  CAMLreturn(caml_ts_optional_node(sibling));
}

CAMLprim value caml_ts_node_prev_named_sibling(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  TSNode sibling = ts_node_prev_named_sibling(node);
  CAMLreturn(caml_ts_optional_node(sibling));
}

CAMLprim value caml_ts_node_first_child_for_byte(value node_v, value byte_v) {
  CAMLparam2(node_v, byte_v);
  TSNode node = Node_val(node_v)->node;
  uint32_t byte = (uint32_t)Unsigned_long_val(byte_v);
  TSNode child = ts_node_first_child_for_byte(node, byte);
  CAMLreturn(caml_ts_optional_node(child));
}

CAMLprim value caml_ts_node_first_named_child_for_byte(value node_v,
                                                       value byte_v) {
  CAMLparam2(node_v, byte_v);
  TSNode node = Node_val(node_v)->node;
  uint32_t byte = (uint32_t)Unsigned_long_val(byte_v);
  TSNode child = ts_node_first_named_child_for_byte(node, byte);
  CAMLreturn(caml_ts_optional_node(child));
}

CAMLprim value caml_ts_node_descendant_count(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  CAMLreturn(caml_copy_int32((int32_t)ts_node_descendant_count(node)));
}

CAMLprim value caml_ts_node_named_descendant_for_byte_range(value node_v,
                                                            value start_v,
                                                            value end_v) {
  CAMLparam3(node_v, start_v, end_v);
  TSNode node = Node_val(node_v)->node;
  uint32_t start_byte = (uint32_t)Unsigned_long_val(start_v);
  uint32_t end_byte = (uint32_t)Unsigned_long_val(end_v);
  TSNode descendant =
      ts_node_named_descendant_for_byte_range(node, start_byte, end_byte);
  CAMLreturn(caml_ts_optional_node(descendant));
}

CAMLprim value caml_ts_node_named_descendant_for_point_range(
    value node_v, value start_point_v, value end_point_v) {
  CAMLparam3(node_v, start_point_v, end_point_v);
  TSNode node = Node_val(node_v)->node;
  TSPoint start_point = caml_ts_point_of_value(start_point_v);
  TSPoint end_point = caml_ts_point_of_value(end_point_v);
  TSNode descendant =
      ts_node_named_descendant_for_point_range(node, start_point, end_point);
  CAMLreturn(caml_ts_optional_node(descendant));
}

CAMLprim value caml_ts_node_eq(value lhs_v, value rhs_v) {
  CAMLparam2(lhs_v, rhs_v);
  TSNode lhs = Node_val(lhs_v)->node;
  TSNode rhs = Node_val(rhs_v)->node;
  CAMLreturn(Val_bool(ts_node_eq(lhs, rhs)));
}

CAMLprim value caml_ts_tree_root_sexp(value tree_v) {
  CAMLparam1(tree_v);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.root_sexp: tree is closed");
  }
  TSNode root = ts_tree_root_node(tree->tree);
  char* sexp = ts_node_string(root);
  if (sexp == NULL) {
    caml_failwith("ts_node_string returned NULL");
  }
  value result = caml_copy_string(sexp);
  free(sexp);
  CAMLreturn(result);
}

CAMLprim value caml_ts_tree_copy(value tree_v) {
  CAMLparam1(tree_v);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.copy: tree is closed");
  }
  TSTree* copy = ts_tree_copy(tree->tree);
  if (copy == NULL) {
    caml_failwith("ts_tree_copy returned NULL");
  }
  CAMLreturn(alloc_tree(copy));
}

CAMLprim value caml_ts_tree_language(value tree_v) {
  CAMLparam1(tree_v);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.language: tree is closed");
  }
  const TSLanguage* language = ts_tree_language(tree->tree);
  if (language == NULL) {
    caml_failwith("ts_tree_language returned NULL");
  }
  CAMLreturn(alloc_language(language));
}

CAMLprim value caml_ts_tree_included_ranges(value tree_v) {
  CAMLparam1(tree_v);
  CAMLlocal1(array);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.included_ranges: tree is closed");
  }
  uint32_t length = 0;
  TSRange* ranges = ts_tree_included_ranges(tree->tree, &length);
  if (ranges == NULL) {
    array = caml_alloc(0, 0);
    CAMLreturn(array);
  }
  array = caml_alloc(length, 0);
  for (uint32_t i = 0; i < length; ++i) {
    Store_field(array, i, caml_ts_range_to_value(&ranges[i]));
  }
  ts_free(ranges);
  CAMLreturn(array);
}

CAMLprim value caml_ts_tree_get_changed_ranges(value old_tree_v,
                                               value new_tree_v) {
  CAMLparam2(old_tree_v, new_tree_v);
  CAMLlocal1(array);
  struct ts_tree_handle* old_tree = Tree_val(old_tree_v);
  struct ts_tree_handle* new_tree = Tree_val(new_tree_v);
  if (old_tree->tree == NULL || new_tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.changed_ranges: tree is closed");
  }
  uint32_t length = 0;
  TSRange* ranges =
      ts_tree_get_changed_ranges(old_tree->tree, new_tree->tree, &length);
  if (ranges == NULL) {
    array = caml_alloc(0, 0);
    CAMLreturn(array);
  }
  array = caml_alloc(length, 0);
  for (uint32_t i = 0; i < length; ++i) {
    Store_field(array, i, caml_ts_range_to_value(&ranges[i]));
  }
  ts_free(ranges);
  CAMLreturn(array);
}

static const char* ts_query_error_to_string(TSQueryError error_type) {
  switch (error_type) {
    case TSQueryErrorNone:
      return "none";
    case TSQueryErrorSyntax:
      return "syntax";
    case TSQueryErrorNodeType:
      return "node_type";
    case TSQueryErrorField:
      return "field";
    case TSQueryErrorCapture:
      return "capture";
    case TSQueryErrorStructure:
      return "structure";
    case TSQueryErrorLanguage:
      return "language";
    default:
      return "unknown";
  }
}

CAMLprim value caml_ts_query_new(value language_v, value source_v) {
  CAMLparam2(language_v, source_v);
  const TSLanguage* language = Language_val(language_v)->language;
  const char* source = String_val(source_v);
  uint32_t length = (uint32_t)caml_string_length(source_v);
  uint32_t error_offset = 0;
  TSQueryError error_type = TSQueryErrorNone;
  TSQuery* query =
      ts_query_new(language, source, length, &error_offset, &error_type);
  if (query == NULL) {
    char buffer[128];
    snprintf(buffer, sizeof(buffer),
             "Tree_sitter.Query.create: %s error at offset %u",
             ts_query_error_to_string(error_type), error_offset);
    caml_failwith(buffer);
  }
  CAMLreturn(alloc_query(query));
}

CAMLprim value caml_ts_query_delete(value query_v) {
  CAMLparam1(query_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query != NULL) {
    ts_query_delete(handle->query);
    handle->query = NULL;
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_capture_count(value query_v) {
  CAMLparam1(query_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.capture_count: query disposed");
  }
  CAMLreturn(caml_copy_int32((int32_t)ts_query_capture_count(handle->query)));
}

CAMLprim value caml_ts_query_pattern_count(value query_v) {
  CAMLparam1(query_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.pattern_count: query disposed");
  }
  CAMLreturn(caml_copy_int32((int32_t)ts_query_pattern_count(handle->query)));
}

CAMLprim value caml_ts_query_string_count(value query_v) {
  CAMLparam1(query_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.string_count: query disposed");
  }
  CAMLreturn(caml_copy_int32((int32_t)ts_query_string_count(handle->query)));
}

CAMLprim value caml_ts_query_start_byte_for_pattern(value query_v,
                                                    value pattern_v) {
  CAMLparam2(query_v, pattern_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.start_byte_for_pattern: query disposed");
  }
  uint32_t pattern_index = (uint32_t)Int_val(pattern_v);
  CAMLreturn(caml_copy_int32(
      (int32_t)ts_query_start_byte_for_pattern(handle->query, pattern_index)));
}

CAMLprim value caml_ts_query_end_byte_for_pattern(value query_v,
                                                  value pattern_v) {
  CAMLparam2(query_v, pattern_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.end_byte_for_pattern: query disposed");
  }
  uint32_t pattern_index = (uint32_t)Int_val(pattern_v);
  CAMLreturn(caml_copy_int32(
      (int32_t)ts_query_end_byte_for_pattern(handle->query, pattern_index)));
}

CAMLprim value caml_ts_query_is_pattern_rooted(value query_v, value pattern_v) {
  CAMLparam2(query_v, pattern_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.is_pattern_rooted: query disposed");
  }
  uint32_t pattern_index = (uint32_t)Int_val(pattern_v);
  CAMLreturn(
      Val_bool(ts_query_is_pattern_rooted(handle->query, pattern_index)));
}

CAMLprim value caml_ts_query_is_pattern_non_local(value query_v,
                                                  value pattern_v) {
  CAMLparam2(query_v, pattern_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.is_pattern_non_local: query disposed");
  }
  uint32_t pattern_index = (uint32_t)Int_val(pattern_v);
  CAMLreturn(
      Val_bool(ts_query_is_pattern_non_local(handle->query, pattern_index)));
}

CAMLprim value caml_ts_query_is_pattern_guaranteed_at_step(
    value query_v, value byte_offset_v) {
  CAMLparam2(query_v, byte_offset_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith(
        "Tree_sitter.Query.is_pattern_guaranteed_at_step: query disposed");
  }
  uint32_t offset = (uint32_t)Unsigned_long_val(byte_offset_v);
  CAMLreturn(
      Val_bool(ts_query_is_pattern_guaranteed_at_step(handle->query, offset)));
}

CAMLprim value caml_ts_query_capture_name_for_id(value query_v, value id_v) {
  CAMLparam2(query_v, id_v);
  CAMLlocal2(result, some);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.capture_name_for_id: query disposed");
  }
  uint32_t length = 0;
  uint32_t id = (uint32_t)Int_val(id_v);
  const char* name = ts_query_capture_name_for_id(handle->query, id, &length);
  if (name == NULL) {
    CAMLreturn(Val_int(0));
  }
  result = caml_alloc_initialized_string(length, name);
  some = caml_alloc(1, 0);
  Store_field(some, 0, result);
  CAMLreturn(some);
}

CAMLprim value caml_ts_query_capture_quantifier_for_id(value query_v,
                                                       value pattern_v,
                                                       value capture_v) {
  CAMLparam3(query_v, pattern_v, capture_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith(
        "Tree_sitter.Query.capture_quantifier_for_id: query disposed");
  }
  uint32_t pattern_index = (uint32_t)Int_val(pattern_v);
  uint32_t capture_index = (uint32_t)Int_val(capture_v);
  TSQuantifier quantifier = ts_query_capture_quantifier_for_id(
      handle->query, pattern_index, capture_index);
  switch (quantifier) {
    case TSQuantifierZero:
      CAMLreturn(Val_int(0));
    case TSQuantifierZeroOrOne:
      CAMLreturn(Val_int(1));
    case TSQuantifierZeroOrMore:
      CAMLreturn(Val_int(2));
    case TSQuantifierOneOrMore:
      CAMLreturn(Val_int(3));
    default:
      caml_failwith(
          "Tree_sitter.Query.capture_quantifier_for_id: unknown quantifier");
  }
}

CAMLprim value caml_ts_query_capture_index_for_name(value query_v,
                                                    value name_v) {
  CAMLparam2(query_v, name_v);
  CAMLlocal2(result, some);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.capture_index_for_name: query disposed");
  }
  size_t length = caml_string_length(name_v);
  const char* name = String_val(name_v);
  uint32_t capture_count = ts_query_capture_count(handle->query);
  for (uint32_t i = 0; i < capture_count; ++i) {
    uint32_t len = 0;
    const char* current = ts_query_capture_name_for_id(handle->query, i, &len);
    if (current != NULL && len == length &&
        strncmp(current, name, length) == 0) {
      some = caml_alloc(1, 0);
      Store_field(some, 0, Val_int(i));
      CAMLreturn(some);
    }
  }
  CAMLreturn(Val_int(0));
}

CAMLprim value caml_ts_query_cursor_new(value unit) {
  CAMLparam1(unit);
  TSQueryCursor* cursor = ts_query_cursor_new();
  if (cursor == NULL) {
    caml_failwith("ts_query_cursor_new returned NULL");
  }
  CAMLreturn(alloc_query_cursor(cursor));
}

CAMLprim value caml_ts_query_cursor_delete(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_query_cursor_handle* handle = QueryCursor_val(cursor_v);
  if (handle->cursor != NULL) {
    ts_query_cursor_delete(handle->cursor);
    handle->cursor = NULL;
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_cursor_exec(value cursor_v, value query_v,
                                         value node_v) {
  CAMLparam3(cursor_v, query_v, node_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  struct ts_query_handle* query = Query_val(query_v);
  TSNode node = Node_val(node_v)->node;
  if (cursor->cursor == NULL || query->query == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.exec: disposed cursor or query");
  }
  ts_query_cursor_exec(cursor->cursor, query->query, node);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_cursor_set_byte_range(value cursor_v,
                                                   value start_v, value end_v) {
  CAMLparam3(cursor_v, start_v, end_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.set_byte_range: cursor disposed");
  }
  uint32_t start_byte = (uint32_t)Unsigned_long_val(start_v);
  uint32_t end_byte = (uint32_t)Unsigned_long_val(end_v);
  ts_query_cursor_set_byte_range(cursor->cursor, start_byte, end_byte);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_cursor_set_point_range(value cursor_v,
                                                    value start_point_v,
                                                    value end_point_v) {
  CAMLparam3(cursor_v, start_point_v, end_point_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.set_point_range: cursor disposed");
  }
  TSPoint start_point = caml_ts_point_of_value(start_point_v);
  TSPoint end_point = caml_ts_point_of_value(end_point_v);
  ts_query_cursor_set_point_range(cursor->cursor, start_point, end_point);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_cursor_reset(value cursor_v, value query_v,
                                          value node_v) {
  CAMLparam3(cursor_v, query_v, node_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  struct ts_query_handle* query = Query_val(query_v);
  if (cursor->cursor == NULL || query->query == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.reset: disposed cursor or query");
  }
  TSNode node = Node_val(node_v)->node;
  ts_query_cursor_exec(cursor->cursor, query->query, node);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_cursor_set_timeout_micros(value cursor_v,
                                                       value timeout_v) {
  CAMLparam2(cursor_v, timeout_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith(
        "Tree_sitter.Query_cursor.set_timeout_micros: cursor disposed");
  }
  uint64_t timeout = (uint64_t)Int64_val(timeout_v);
  ts_query_cursor_set_timeout_micros(cursor->cursor, timeout);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_cursor_timeout_micros(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.timeout_micros: cursor disposed");
  }
  uint64_t timeout = ts_query_cursor_timeout_micros(cursor->cursor);
  CAMLreturn(caml_copy_int64((int64_t)timeout));
}

CAMLprim value caml_ts_query_cursor_set_match_limit(value cursor_v,
                                                    value limit_v) {
  CAMLparam2(cursor_v, limit_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.set_match_limit: cursor disposed");
  }
  ts_query_cursor_set_match_limit(cursor->cursor, (uint32_t)Int_val(limit_v));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_cursor_match_limit(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.match_limit: cursor disposed");
  }
  CAMLreturn(Val_int(ts_query_cursor_match_limit(cursor->cursor)));
}

CAMLprim value caml_ts_query_cursor_did_exceed_match_limit(value cursor_v) {
  CAMLparam1(cursor_v);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith(
        "Tree_sitter.Query_cursor.did_exceed_match_limit: cursor disposed");
  }
  CAMLreturn(Val_bool(ts_query_cursor_did_exceed_match_limit(cursor->cursor)));
}

CAMLprim value caml_ts_query_cursor_next_match(value cursor_v) {
  CAMLparam1(cursor_v);
  CAMLlocal4(result, ocaml_match, captures, capture_tuple);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  if (cursor->cursor == NULL) {
    caml_failwith("Tree_sitter.Query_cursor.next_match: cursor disposed");
  }
  TSQueryMatch match;
  if (!ts_query_cursor_next_match(cursor->cursor, &match)) {
    CAMLreturn(Val_int(0));
  }
  captures = caml_alloc(match.capture_count, 0);
  for (uint32_t i = 0; i < match.capture_count; ++i) {
    capture_tuple = caml_alloc_tuple(2);
    Store_field(capture_tuple, 0, Val_int(match.captures[i].index));
    Store_field(capture_tuple, 1, alloc_node(match.captures[i].node));
    Store_field(captures, i, capture_tuple);
  }
  ocaml_match = caml_alloc_tuple(3);
  Store_field(ocaml_match, 0, caml_copy_int32((int32_t)match.id));
  Store_field(ocaml_match, 1, Val_int(match.pattern_index));
  Store_field(ocaml_match, 2, captures);
  result = caml_alloc(1, 0);
  Store_field(result, 0, ocaml_match);
  CAMLreturn(result);
}

CAMLprim value caml_ts_query_cursor_next_capture(value cursor_v,
                                                 value query_v) {
  CAMLparam2(cursor_v, query_v);
  CAMLlocal3(result, tup, node_value);
  struct ts_query_cursor_handle* cursor = QueryCursor_val(cursor_v);
  struct ts_query_handle* query = Query_val(query_v);
  if (cursor->cursor == NULL || query->query == NULL) {
    caml_failwith(
        "Tree_sitter.Query_cursor.next_capture: disposed cursor or query");
  }
  TSQueryMatch match;
  uint32_t capture_index = 0;
  if (!ts_query_cursor_next_capture(cursor->cursor, &match, &capture_index)) {
    CAMLreturn(Val_int(0));
  }
  if (capture_index >= match.capture_count) {
    CAMLreturn(Val_int(0));
  }
  TSQueryCapture capture = match.captures[capture_index];
  node_value = alloc_node(capture.node);
  tup = caml_alloc_tuple(4);
  Store_field(tup, 0, Val_int(capture.index));
  Store_field(tup, 1, Val_int(match.pattern_index));
  Store_field(tup, 2, caml_copy_int32((int32_t)match.id));
  Store_field(tup, 3, node_value);
  result = caml_alloc(1, 0);
  Store_field(result, 0, tup);
  CAMLreturn(result);
}

CAMLprim value caml_ts_tree_edit_native(value tree_v, value start_byte_v,
                                        value old_end_byte_v,
                                        value new_end_byte_v,
                                        value start_point_v,
                                        value old_end_point_v,
                                        value new_end_point_v) {
  CAMLparam5(tree_v, start_byte_v, old_end_byte_v, new_end_byte_v,
             start_point_v);
  CAMLxparam2(old_end_point_v, new_end_point_v);
  struct ts_tree_handle* tree = Tree_val(tree_v);
  if (tree->tree == NULL) {
    caml_failwith("Tree_sitter.Tree.edit: tree is closed");
  }
  TSInputEdit edit = {
      .start_byte = Unsigned_long_val(start_byte_v),
      .old_end_byte = Unsigned_long_val(old_end_byte_v),
      .new_end_byte = Unsigned_long_val(new_end_byte_v),
      .start_point = caml_ts_point_of_value(start_point_v),
      .old_end_point = caml_ts_point_of_value(old_end_point_v),
      .new_end_point = caml_ts_point_of_value(new_end_point_v),
  };
  ts_tree_edit(tree->tree, &edit);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_tree_edit_bytecode(value* argv, int argn) {
  (void)argn;
  return caml_ts_tree_edit_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                                  argv[5], argv[6]);
}

CAMLprim value caml_ts_parser_delete(value parser_v) {
  CAMLparam1(parser_v);
  struct ts_parser_handle* parser = Parser_val(parser_v);
  if (parser->parser != NULL) {
    ts_parser_delete(parser->parser);
    parser->parser = NULL;
  }
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_node_language(value node_v) {
  CAMLparam1(node_v);
  TSNode node = Node_val(node_v)->node;
  const TSLanguage* language = ts_node_language(node);
  if (language == NULL) {
    caml_failwith("Tree_sitter.Node.language: node has no associated language");
  }
  CAMLreturn(alloc_language(language));
}

CAMLprim value caml_ts_query_string_value_for_id(value query_v, value id_v) {
  CAMLparam2(query_v, id_v);
  CAMLlocal2(result, some);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.string_value_for_id: query disposed");
  }
  uint32_t length = 0;
  uint32_t index = (uint32_t)Int_val(id_v);
  const char* str = ts_query_string_value_for_id(handle->query, index, &length);
  if (str == NULL) {
    CAMLreturn(Val_int(0));
  }
  result = caml_alloc_initialized_string(length, str);
  some = caml_alloc(1, 0);
  Store_field(some, 0, result);
  CAMLreturn(some);
}

CAMLprim value caml_ts_query_disable_capture(value query_v, value name_v) {
  CAMLparam2(query_v, name_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.disable_capture: query disposed");
  }
  const char* name = String_val(name_v);
  uint32_t length = (uint32_t)caml_string_length(name_v);
  ts_query_disable_capture(handle->query, name, length);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_ts_query_disable_pattern(value query_v, value pattern_v) {
  CAMLparam2(query_v, pattern_v);
  struct ts_query_handle* handle = Query_val(query_v);
  if (handle->query == NULL) {
    caml_failwith("Tree_sitter.Query.disable_pattern: query disposed");
  }
  uint32_t pattern_index = (uint32_t)Int_val(pattern_v);
  ts_query_disable_pattern(handle->query, pattern_index);
  CAMLreturn(Val_unit);
}
