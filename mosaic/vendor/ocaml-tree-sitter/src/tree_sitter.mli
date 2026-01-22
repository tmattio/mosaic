(** OCaml bindings for Tree-sitter.

    Tree-sitter is an incremental parsing library that builds concrete syntax
    trees for source code files and efficiently updates them as the code
    changes. This module provides idiomatic OCaml bindings to the Tree-sitter C
    library, enabling syntax tree parsing, querying, and traversal.

    {1 Overview}

    A typical workflow involves:

    1. Load a language grammar with {!Language.load_shared} 2. Create a parser
    with {!Parser.create} and configure it 3. Parse source code into a syntax
    tree with {!Parser.parse_string} 4. Traverse nodes with {!Node} functions or
    query with {!Query} 5. Update the tree incrementally using {!Tree.edit} and
    reparse

    Incremental parsing is a key feature: after editing a tree with
    {!Tree.edit}, reparsing with the old tree allows Tree-sitter to reuse
    unchanged subtrees, making reparsing significantly faster than parsing from
    scratch.

    {1 Usage Basics}

    Parse JSON source code:
    {[
      let parser = Parser.create () in
      Parser.set_language parser (Tree_sitter_json.language ());
      let tree = Parser.parse_string parser {|{"key": "value"}|} in
      let root = Tree.root_node tree in
      assert (Node.kind root = "document");
      Parser.delete parser
    ]}

    Traverse the syntax tree:
    {[
      let root = Tree.root_node tree in
      match Node.named_child root 0 with
      | Some obj ->
          Printf.printf "Node kind: %s\n" (Node.kind obj);
          Printf.printf "Start byte: %d\n" (Node.start_byte obj)
      | None -> ()
    ]}

    Query for specific patterns:
    {[
      let query = Query.create lang ~source:"(string) @str" in
      let cursor = Query_cursor.create () in
      Query_cursor.exec cursor query (Tree.root_node tree);

      let rec collect acc =
        match Query_cursor.next_capture cursor query with
        | Some cap -> collect (cap.node :: acc)
        | None -> List.rev acc
      in
      let strings = collect [] in

      Query_cursor.delete cursor;
      Query.delete query
    ]}

    Incremental parsing after edits:
    {[
      let tree1 = Parser.parse_string parser {|{"a": 1}|} in

      (* Edit: change "1" to "2" at byte offset 6-7 *)
      Tree.edit tree1
        ~start_byte:6 ~old_end_byte:7 ~new_end_byte:7
        ~start_point:(0, 6) ~old_end_point:(0, 7) ~new_end_point:(0, 7);

      (* Reparse with old tree for incremental update *)
      let tree2 = Parser.parse_string ~old:tree1 parser {|{"a": 2}|}
    ]}

    {1 Key Concepts}

    {2 Named vs Anonymous Nodes}

    Nodes are either {e named} or {e anonymous}. Named nodes represent
    significant syntactic constructs (like [object], [array], [string] in JSON),
    while anonymous nodes represent punctuation and keywords (like [\{], [,],
    [:] in JSON). Most traversal functions have both named and unnamed variants.

    {2 Node Positions}

    Each node has both byte offsets ({!Node.start_byte}, {!Node.end_byte}) and
    row-column points ({!Node.start_point}, {!Node.end_point}). Points are
    represented as [(row, column)] tuples with zero-based indexing.

    {2 Error Nodes}

    When parsing invalid source code, Tree-sitter creates error nodes and marks
    nodes containing errors. Use {!Node.has_error} to check if a subtree
    contains errors and {!Node.is_error} to identify error nodes directly.

    {2 Memory Management}

    Parsers, queries, and query cursors require explicit cleanup with their
    respective [delete] functions. Trees are automatically garbage collected.
    Nodes hold references to their parent trees to prevent premature collection
    of the tree while nodes are in use; this means trees remain alive as long as
    any node referencing them exists.

    {2 Thread Safety}

    Parsers, queries, and query cursors are {e not} thread-safe; each should be
    used from a single thread or protected by external synchronization. Trees
    and nodes are immutable after creation and can be shared across threads, but
    {!Tree.edit} mutates the tree and requires synchronization.

    {2 Fields}

    Many grammars define named fields for accessing specific children. For
    example, a [pair] node in JSON might have [key] and [value] fields. Use
    {!Node.child_by_field_name} to access children by field name. *)

(** Language grammars. *)
module Language : sig
  type t
  (** A Tree-sitter language grammar.

      Languages define the parsing rules for a specific programming language or
      file format. They are typically loaded from dynamically linked shared
      libraries containing pre-compiled parser code. *)

  type symbol_type =
    | Regular
    | Anonymous
    | Supertype
    | Auxiliary
        (** Symbol classification.

            - [Regular]: Named nodes representing significant syntax constructs
            - [Anonymous]: Literal tokens and punctuation
            - [Supertype]: Abstract node types used in grammar rules
            - [Auxiliary]: Internal implementation nodes *)

  val of_address : nativeint -> t
  (** [of_address addr] creates a language from a native pointer address.

      {b Warning}: This is an unsafe FFI function. The pointer [addr] must point
      to a valid [TSLanguage] struct returned by a [tree_sitter_<lang>()] C
      function, and must remain valid for the lifetime of the returned language.
      Passing an invalid address causes undefined behavior, including segfaults.

      Most users should use language-specific binding functions instead (e.g.,
      [Tree_sitter_json.language], [Tree_sitter_ocaml.ocaml]). This function is
      exposed for FFI interop with custom or dynamically loaded grammars. *)

  val load_shared : path:string -> symbol:string -> t
  (** [load_shared ~path ~symbol] loads a language from a shared library.

      Dynamically loads the shared library at [path] and resolves the symbol
      [symbol] to obtain the language grammar. The symbol name typically follows
      the pattern [tree_sitter_<language>].

      On Unix systems, this uses [dlopen(3)] to load the library. The [path] is
      resolved according to the system linker: absolute paths are used directly,
      while relative paths search [LD_LIBRARY_PATH] on Linux or
      [DYLD_LIBRARY_PATH] on macOS. The symbol must have default visibility in
      the shared library.

      Example:
      {[
        let json_lang =
          Language.load_shared ~path:"./libtree-sitter-json.so"
            ~symbol:"tree_sitter_json"
      ]}

      @raise Failure
        with a message indicating the error. Common failures include: library
        file not found, symbol not exported, or architecture mismatch. *)

  val version : t -> int
  (** [version lang] returns the ABI version of the language.

      The version must be compatible with the Tree-sitter runtime. This library
      requires version 14 or higher; grammars with lower versions may cause
      parse failures or undefined behavior. Modern grammars typically use
      version 14. *)

  val name : t -> string
  (** [name lang] returns the language name.

      This is the human-readable identifier specified when the grammar was
      generated (e.g., ["json"], ["python"], ["ocaml"]). *)

  val symbol_count : t -> int
  (** [symbol_count lang] returns the total number of symbols in the grammar.

      This includes all node types, tokens, and internal symbols used by the
      parser. *)

  val state_count : t -> int
  (** [state_count lang] returns the number of parser states.

      This reflects the complexity of the grammar's LR parser table. *)

  val symbol_name : t -> int -> string
  (** [symbol_name lang id] returns the name of the symbol with ID [id].

      Symbol IDs are internal numeric identifiers used by the parser. Valid IDs
      range from [0] to [symbol_count lang - 1]; behavior is undefined for
      out-of-bounds IDs. *)

  val symbol_for_name : ?is_named:bool -> t -> string -> int option
  (** [symbol_for_name ?is_named lang name] looks up the symbol ID for [name].

      Returns [Some id] if a symbol with the given name exists, [None]
      otherwise. When [is_named] is [true] (default: [false]), only searches
      named symbols.

      Example:
      {[
        match Language.symbol_for_name ~is_named:true lang "object" with
        | Some id -> Printf.printf "Object symbol ID: %d\n" id
        | None -> Printf.printf "No such symbol\n"
      ]} *)

  val symbol_type : t -> int -> symbol_type
  (** [symbol_type lang id] returns the type classification of symbol [id]. *)

  val field_count : t -> int
  (** [field_count lang] returns the number of named fields in the grammar.

      Named fields provide semantic access to specific children of a node (e.g.,
      [key] and [value] fields in a key-value pair). *)

  val field_name_for_id : t -> int -> string option
  (** [field_name_for_id lang id] returns the name of field [id].

      Returns [None] if [id] does not correspond to a valid field. *)

  val field_id_for_name : t -> string -> int option
  (** [field_id_for_name lang name] looks up the field ID for [name].

      Returns [Some id] if a field with the given name exists, [None] otherwise.
  *)
end

(** Syntax tree nodes. *)
module Node : sig
  type t
  (** A syntax tree node.

      Nodes represent individual syntax elements in the parsed source code. Each
      node spans a range of the source text and may have children representing
      nested structure.

      Nodes hold an internal reference to their parent tree to ensure the tree
      is not garbage collected while the node is in use. This prevents dangling
      pointers and use-after-free errors. *)

  val is_null : t -> bool
  (** [is_null node] checks if [node] is null.

      Null nodes are returned by Tree-sitter C APIs when no node exists. This
      should rarely occur in the OCaml bindings since most functions return
      [option] types instead. *)

  val is_named : t -> bool
  (** [is_named node] checks if [node] is a named node.

      Named nodes represent significant syntactic constructs. Anonymous nodes
      represent literals, keywords, and punctuation. *)

  val is_missing : t -> bool
  (** [is_missing node] checks if [node] represents missing expected syntax.

      Missing nodes are inserted by the parser's error recovery to represent
      required syntax that was not present in the source. For example, a missing
      closing bracket might be represented as a missing node. *)

  val is_extra : t -> bool
  (** [is_extra node] checks if [node] was marked as extra.

      Extra nodes represent syntax that is valid but not part of the core
      grammar, such as comments or whitespace in languages where these are not
      syntactically significant. *)

  val has_changes : t -> bool
  (** [has_changes node] checks if [node] was modified during incremental
      parsing.

      After editing a tree and reparsing, this returns [true] for nodes that
      were affected by the edit. Nodes outside the edited range that were reused
      from the old tree return [false]. *)

  val has_error : t -> bool
  (** [has_error node] checks if [node] or any descendant contains a syntax
      error.

      This is more efficient than traversing the entire subtree to look for
      error nodes. *)

  val is_error : t -> bool
  (** [is_error node] checks if [node] itself is an error node.

      Error nodes are synthetic nodes inserted when the parser encountered
      invalid syntax. To check if a subtree contains any errors, use
      {!has_error} instead. *)

  val kind : t -> string
  (** [kind node] returns the node's type name.

      The kind is the symbolic name of the grammar rule that produced this node
      (e.g., ["string"], ["object"], ["identifier"]). *)

  val symbol : t -> int
  (** [symbol node] returns the node's symbol ID.

      This is the internal numeric identifier for the node's type. Use {!kind}
      for a human-readable name. *)

  val language : t -> Language.t
  (** [language node] returns the language grammar that produced [node]. *)

  val parse_state : t -> int
  (** [parse_state node] returns the parser state after this node.

      This is an internal parse table state used for advanced parser
      introspection. *)

  val next_parse_state : t -> int
  (** [next_parse_state node] returns the parser state that follows this node.

      This represents the state the parser would be in immediately after
      processing this node. *)

  val start_byte : t -> int
  (** [start_byte node] returns the starting byte offset of [node] in the
      source.

      Byte offsets are zero-indexed and count from the beginning of the parsed
      source string. *)

  val end_byte : t -> int
  (** [end_byte node] returns the ending byte offset of [node] in the source.

      The end offset is exclusive: the node spans from [start_byte] to
      [end_byte - 1]. *)

  val start_point : t -> int * int
  (** [start_point node] returns the starting position as [(row, column)].

      Both row and column are zero-indexed. The column is measured in bytes, not
      characters, so multi-byte UTF-8 characters may span multiple columns. *)

  val end_point : t -> int * int
  (** [end_point node] returns the ending position as [(row, column)].

      The end position is exclusive: it points to the first position after the
      node's text. *)

  val child_count : t -> int
  (** [child_count node] returns the total number of children, including
      anonymous nodes. *)

  val named_child_count : t -> int
  (** [named_child_count node] returns the number of named children.

      This counts only named nodes, excluding anonymous tokens like punctuation.
  *)

  val child : t -> int -> t option
  (** [child node n] returns the [n]-th child of [node].

      Returns [None] if [n] is out of bounds. Negative indices are invalid and
      return [None]. This includes both named and anonymous children. *)

  val named_child : t -> int -> t option
  (** [named_child node n] returns the [n]-th named child of [node].

      Returns [None] if [n] is out of bounds. Only counts named children,
      skipping anonymous tokens. *)

  val child_with_descendant : t -> t -> t option
  (** [child_with_descendant parent desc] finds the child of [parent] that
      contains [desc].

      Returns [None] if [desc] is not a descendant of [parent]. This is useful
      for navigating upward from a descendant to find its containing child at a
      specific level. *)

  val child_by_field_name : t -> string -> t option
  (** [child_by_field_name node field] returns the child with the named field
      [field].

      Returns [None] if no such field exists for this node type. Field names are
      defined in the grammar and provide semantic access to specific children.

      Example:
      {[
        (* Access the key of a JSON pair *)
        match Node.child_by_field_name pair "key" with
        | Some key_node -> Node.kind key_node
        | None -> "no key"
      ]} *)

  val child_by_field_id : t -> int -> t option
  (** [child_by_field_id node field_id] returns the child with field ID
      [field_id].

      This is the field-ID-based equivalent of {!child_by_field_name}. Returns
      [None] if no child exists for this field. *)

  val field_name_for_child : t -> int -> string option
  (** [field_name_for_child node n] returns the field name of the [n]-th child.

      Returns [None] if the child has no field name or [n] is out of bounds. *)

  val field_name_for_named_child : t -> int -> string option
  (** [field_name_for_named_child node n] returns the field name of the [n]-th
      named child.

      Returns [None] if the named child has no field name or [n] is out of
      bounds. Only considers named children. *)

  val parent : t -> t option
  (** [parent node] returns the parent of [node].

      Returns [None] for the root node. *)

  val next_sibling : t -> t option
  (** [next_sibling node] returns the next sibling of [node].

      Returns [None] if [node] is the last child of its parent. Includes both
      named and anonymous siblings. *)

  val prev_sibling : t -> t option
  (** [prev_sibling node] returns the previous sibling of [node].

      Returns [None] if [node] is the first child of its parent. Includes both
      named and anonymous siblings. *)

  val next_named_sibling : t -> t option
  (** [next_named_sibling node] returns the next named sibling of [node].

      Returns [None] if no named sibling follows. Skips anonymous nodes. *)

  val prev_named_sibling : t -> t option
  (** [prev_named_sibling node] returns the previous named sibling of [node].

      Returns [None] if no named sibling precedes. Skips anonymous nodes. *)

  val first_child_for_byte : t -> byte:int -> t option
  (** [first_child_for_byte node ~byte] returns the first child that extends
      beyond [byte].

      Returns [None] if no child extends beyond the given byte offset. This is
      useful for finding the child containing a specific position. *)

  val first_named_child_for_byte : t -> byte:int -> t option
  (** [first_named_child_for_byte node ~byte] returns the first named child that
      extends beyond [byte].

      Returns [None] if no named child extends beyond the given byte offset. *)

  val descendant_count : t -> int
  (** [descendant_count node] returns the total number of descendants.

      This includes all nodes in the subtree rooted at [node], excluding [node]
      itself. *)

  val descendant_for_byte_range : t -> start:int -> end_:int -> t option
  (** [descendant_for_byte_range node ~start ~end_] finds the smallest
      descendant that spans [\[start, end_)].

      Returns [None] if no descendant spans the given range. The range is
      inclusive of [start] and exclusive of [end_]. This is useful for locating
      the syntax element at a specific source location. *)

  val descendant_for_point_range :
    t -> start_point:int * int -> end_point:int * int -> t option
  (** [descendant_for_point_range node ~start_point ~end_point] finds the
      smallest descendant that spans the given point range.

      Returns [None] if no descendant spans the range. Points are
      [(row, column)] tuples. The range is inclusive of [start_point] and
      exclusive of [end_point]. *)

  val named_descendant_for_byte_range : t -> start:int -> end_:int -> t option
  (** [named_descendant_for_byte_range node ~start ~end_] finds the smallest
      named descendant that spans [\[start, end_)].

      Returns [None] if no named descendant spans the range. Only considers
      named nodes. *)

  val named_descendant_for_point_range :
    t -> start_point:int * int -> end_point:int * int -> t option
  (** [named_descendant_for_point_range node ~start_point ~end_point] finds the
      smallest named descendant that spans the given point range.

      Returns [None] if no named descendant spans the range. Only considers
      named nodes. *)

  val to_sexp : t -> string
  (** [to_sexp node] returns an S-expression representation of the subtree.

      The S-expression is a human-readable text representation showing the
      structure and node types. It starts with [(kind ...)] for the node and
      recursively includes all children.

      Example output: [(document (object (pair (string) (string))))] *)

  val equal : t -> t -> bool
  (** [equal n1 n2] checks structural equality of nodes.

      Two nodes are equal if they represent the same position in the same tree.
      This compares node identity, not subtree content. *)
end

(** Parsed syntax trees. *)
module Tree : sig
  type t
  (** A parsed syntax tree.

      Trees are immutable once created but can be edited in-place to mark
      regions as changed. After editing, reparse with the edited tree to perform
      incremental parsing. Trees are automatically garbage collected. *)

  type range = {
    start_byte : int;
    end_byte : int;
    start_point : int * int;
    end_point : int * int;
  }
  (** A source range with both byte offsets and row-column points.

      Ranges are inclusive of [start_byte] and [start_point], exclusive of
      [end_byte] and [end_point]. *)

  val copy : t -> t
  (** [copy tree] creates a shallow copy of [tree].

      The copy shares the underlying parse data but can be edited independently.
      This is useful when multiple versions of a tree need to coexist. *)

  val root_node : t -> Node.t
  (** [root_node tree] returns the root node of [tree].

      The root node spans the entire source text and contains all other nodes as
      descendants. *)

  val root_sexp : t -> string
  (** [root_sexp tree] returns an S-expression representation of the entire
      tree.

      This is equivalent to [Node.to_sexp (root_node tree)] but potentially more
      efficient. *)

  val edit :
    t ->
    start_byte:int ->
    old_end_byte:int ->
    new_end_byte:int ->
    start_point:int * int ->
    old_end_point:int * int ->
    new_end_point:int * int ->
    unit
  (** [edit tree ~start_byte ~old_end_byte ~new_end_byte ~start_point
       ~old_end_point ~new_end_point] marks a region of [tree] as edited.

      This prepares [tree] for incremental reparsing. The edit replaces the
      region from [\[start_byte, old_end_byte)] with a new region ending at
      [new_end_byte]. Both byte offsets and point coordinates must be provided.

      After editing, pass [tree] as the [~old] parameter to
      {!Parser.parse_string} to reparse incrementally. Tree-sitter will reuse
      unchanged subtrees, making reparsing faster than parsing from scratch.

      Example:
      {[
        (* Original: {"a": 1} *)
        Tree.edit tree
          ~start_byte:6 ~old_end_byte:7 ~new_end_byte:7
          ~start_point:(0, 6) ~old_end_point:(0, 7) ~new_end_point:(0, 7);
        (* Now reparse with new source: {"a": 2} *)
        let new_tree = Parser.parse_string ~old:tree parser {|{"a": 2}|}
      ]} *)

  val language : t -> Language.t
  (** [language tree] returns the language grammar used to parse [tree]. *)

  val included_ranges : t -> range array
  (** [included_ranges tree] returns the source ranges that were parsed.

      By default, this is a single range spanning the entire input. If the
      parser was configured to parse only specific ranges, those ranges are
      returned here. *)

  val changed_ranges : old:t -> t -> range array
  (** [changed_ranges ~old new_tree] computes the ranges that changed between
      [old] and [new_tree].

      Returns an array of ranges where the syntax tree structure differs. This
      is useful for updating downstream tools that depend on the parse tree,
      allowing them to process only the changed regions. *)
end

(** Tree-sitter queries for pattern matching on syntax trees. *)
module Query : sig
  type t
  (** A compiled query.

      Queries are written in Tree-sitter's S-expression-based pattern language
      and compiled against a specific grammar. They enable efficient pattern
      matching on syntax trees. Queries must be explicitly deleted with
      {!delete} when no longer needed. *)

  type quantifier =
    | Zero
    | Zero_or_one
    | Zero_or_more
    | One_or_more
        (** Capture quantifiers specify how many times a capture can match.

            - [Zero]: Capture is optional and not present in this match
            - [Zero_or_one]: Capture is optional ([?] in query syntax)
            - [Zero_or_more]: Capture can repeat ([*] in query syntax)
            - [One_or_more]: Capture must occur at least once ([+] in query
              syntax) *)

  val create : Language.t -> source:string -> t
  (** [create lang ~source] compiles a query from [source] for [lang].

      The query is written in Tree-sitter's pattern language, which uses
      S-expressions to match syntax tree structures. Captures are named with
      [@name] syntax.

      Example:
      {[
        let query =
          Query.create lang
            ~source:
              {|
          (pair
            key: (string) @key
            value: (string) @value)
        |}
      ]}

      @raise Failure
        if [source] contains syntax errors or references non-existent node
        types. *)

  val delete : t -> unit
  (** [delete query] releases resources associated with [query].

      The query must not be used after deletion. *)

  val capture_count : t -> int
  (** [capture_count query] returns the number of captures defined in [query].

      Each [@name] in the query source defines one capture. *)

  val pattern_count : t -> int
  (** [pattern_count query] returns the number of top-level patterns in [query].

      Multiple patterns in a query are separated by the query language syntax.
      Each top-level S-expression is one pattern. *)

  val string_count : t -> int
  (** [string_count query] returns the number of string literals in [query].

      This counts literal strings used in the query patterns. *)

  val capture_name_for_id : t -> int -> string option
  (** [capture_name_for_id query id] returns the name of capture [id].

      Returns [None] if [id] is out of bounds. Capture IDs are assigned
      sequentially in the order captures appear in the query source. *)

  val capture_index_for_name : t -> string -> int option
  (** [capture_index_for_name query name] returns the ID of capture [name].

      Returns [None] if no capture with the given name exists. *)

  val capture_quantifier : t -> pattern:int -> capture:int -> quantifier
  (** [capture_quantifier query ~pattern ~capture] returns the quantifier for
      [capture] in [pattern].

      The quantifier indicates whether the capture is optional or can repeat. *)

  val string_value_for_id : t -> int -> string option
  (** [string_value_for_id query id] returns the string literal with ID [id].

      Returns [None] if [id] is out of bounds. *)

  val start_byte_for_pattern : t -> pattern:int -> int
  (** [start_byte_for_pattern query ~pattern] returns the starting byte offset
      of [pattern] in the query source.

      This is the position in the original query string where the pattern
      begins. *)

  val end_byte_for_pattern : t -> pattern:int -> int
  (** [end_byte_for_pattern query ~pattern] returns the ending byte offset of
      [pattern] in the query source. *)

  val is_pattern_rooted : t -> pattern:int -> bool
  (** [is_pattern_rooted query ~pattern] checks if [pattern] is rooted.

      Rooted patterns can only match starting from the root of the queried
      subtree. For example, a pattern [(document (object))] is rooted because it
      must match at the document root. In contrast, [(object)] is not rooted and
      can match any [object] node anywhere in the tree. This affects how the
      query engine optimizes pattern matching. *)

  val is_pattern_non_local : t -> pattern:int -> bool
  (** [is_pattern_non_local query ~pattern] checks if [pattern] is non-local.

      Non-local patterns use constructs like wildcard descendants [(_ ...)] that
      require traversing the entire subtree. For example,
      [(object (_ (string) @s))] is non-local because [_] matches any node type.
      Local patterns match within a bounded context and allow more optimization.
  *)

  val is_pattern_guaranteed_at_step : t -> byte_offset:int -> bool
  (** [is_pattern_guaranteed_at_step query ~byte_offset] checks if any pattern
      is guaranteed to match at [byte_offset].

      This is a query engine introspection function used internally for
      optimization. It returns [true] if the grammar structure guarantees a
      pattern will match at the given position. Most users do not need this
      function; it is exposed for advanced query analysis and debugging. *)

  val disable_capture : t -> name:string -> unit
  (** [disable_capture query ~name] disables the capture [name].

      Disabled captures are not included in query results. This is useful for
      temporarily filtering out unwanted captures without recompiling the query.
  *)

  val disable_pattern : t -> pattern:int -> unit
  (** [disable_pattern query ~pattern] disables [pattern].

      Disabled patterns do not match and are skipped during query execution.
      This allows dynamic filtering of patterns without recompiling. *)
end

(** Parsers for converting source code into syntax trees. *)
module Parser : sig
  type t
  (** A parser instance.

      Parsers are stateful and maintain internal buffers. They must be
      configured with a language before parsing. Parsers must be explicitly
      deleted with {!delete} when no longer needed. *)

  val create : unit -> t
  (** [create ()] creates a new parser.

      The parser must be configured with {!set_language} before use. *)

  val delete : t -> unit
  (** [delete parser] releases resources associated with [parser].

      The parser must not be used after deletion. *)

  val set_language : t -> Language.t -> unit
  (** [set_language parser lang] configures [parser] to use [lang].

      This must be called before parsing. Changing the language resets the
      parser state. *)

  val parse_string : ?old:Tree.t -> t -> string -> Tree.t
  (** [parse_string ?old parser source] parses [source] into a syntax tree.

      If [old] is provided, performs incremental parsing by reusing unchanged
      portions of the old tree. The [old] tree should have been edited with
      {!Tree.edit} to mark changed regions.

      Incremental parsing is significantly faster than parsing from scratch for
      small edits to large files. The speedup is proportional to the ratio of
      unchanged to changed content.

      Example:
      {[
        let tree1 = Parser.parse_string parser source1 in
        Tree.edit tree1 (* ... mark edited region ... *);
        let tree2 = Parser.parse_string ~old:tree1 parser source2
      ]} *)

  val set_timeout_micros : t -> int64 -> unit
  (** [set_timeout_micros parser timeout] sets the parsing timeout to [timeout]
      microseconds.

      If parsing takes longer than the timeout, the parser returns a partial
      tree with error nodes. A timeout of [0L] disables the timeout. *)

  val timeout_micros : t -> int64
  (** [timeout_micros parser] returns the current parsing timeout in
      microseconds.

      Returns [0L] if no timeout is set. *)

  val reset : t -> unit
  (** [reset parser] resets [parser] to its initial state.

      This clears the internal parse state but preserves the language
      configuration. Use this to reuse a parser for unrelated parse operations.
  *)
end

(** Query cursors for executing queries and iterating over results. *)
module Query_cursor : sig
  type t
  (** A query cursor for executing queries.

      Cursors maintain state during query execution and provide iterators over
      matches and captures. They can be configured with byte ranges, point
      ranges, timeouts, and match limits. Cursors must be explicitly deleted
      with {!delete} when no longer needed. *)

  type capture = {
    capture_index : int;
    pattern_index : int;
    match_id : int;
    node : Node.t;
  }
  (** A single capture from a query match.

      - [capture_index]: The ID of the capture within the query
      - [pattern_index]: The ID of the pattern that matched
      - [match_id]: A unique identifier for the match
      - [node]: The syntax tree node that was captured *)

  type match_result = {
    match_id : int;
    pattern_index : int;
    captures : (int * Node.t) array;
  }
  (** A complete match result containing all captures.

      - [match_id]: A unique identifier for this match
      - [pattern_index]: The ID of the pattern that matched
      - [captures]: An array of [(capture_index, node)] pairs for all captures
        in this match *)

  val create : unit -> t
  (** [create ()] creates a new query cursor.

      The cursor must be initialized with {!exec} or {!reset} before use. *)

  val delete : t -> unit
  (** [delete cursor] releases resources associated with [cursor].

      The cursor must not be used after deletion. *)

  val exec : t -> Query.t -> Node.t -> unit
  (** [exec cursor query node] executes [query] on the subtree rooted at [node].

      This initializes [cursor] to iterate over matches of [query] within [node]
      and its descendants. After execution, use {!next_match} or {!next_capture}
      to retrieve results.

      Example:
      {[
        Query_cursor.exec cursor query (Tree.root_node tree);
        let rec collect acc =
          match Query_cursor.next_capture cursor query with
          | Some cap -> collect (cap.node :: acc)
          | None -> List.rev acc
        in
        let results = collect []
      ]} *)

  val reset : t -> query:Query.t -> node:Node.t -> unit
  (** [reset cursor ~query ~node] reinitializes [cursor] to re-execute [query]
      on [node].

      This is equivalent to {!exec} but reuses the cursor's internal state,
      potentially avoiding allocations. Use this to iterate over the same query
      results multiple times. *)

  val set_byte_range : t -> start:int -> end_:int -> unit
  (** [set_byte_range cursor ~start ~end_] restricts [cursor] to
      [\[start, end_)].

      Only matches within the specified byte range are returned. The range is
      inclusive of [start] and exclusive of [end_]. This must be called before
      {!exec} or {!reset}. *)

  val set_point_range :
    t -> start_point:int * int -> end_point:int * int -> unit
  (** [set_point_range cursor ~start_point ~end_point] restricts [cursor] to the
      given point range.

      Only matches within the range are returned. Points are [(row, column)]
      tuples. The range is inclusive of [start_point] and exclusive of
      [end_point]. This must be called before {!exec} or {!reset}. *)

  val set_timeout_micros : t -> int64 -> unit
  (** [set_timeout_micros cursor timeout] sets the query execution timeout to
      [timeout] microseconds.

      If query execution exceeds the timeout, iteration stops early. A timeout
      of [0L] disables the timeout. *)

  val timeout_micros : t -> int64
  (** [timeout_micros cursor] returns the current query timeout in microseconds.

      Returns [0L] if no timeout is set. *)

  val set_match_limit : t -> int -> unit
  (** [set_match_limit cursor limit] limits the maximum number of matches.

      After [limit] matches are found, iteration stops. Use
      {!did_exceed_match_limit} to check if the limit was reached. *)

  val match_limit : t -> int
  (** [match_limit cursor] returns the current match limit.

      Returns [0] if no limit is set. *)

  val did_exceed_match_limit : t -> bool
  (** [did_exceed_match_limit cursor] checks if the match limit was exceeded.

      Returns [true] if iteration stopped due to reaching the match limit set by
      {!set_match_limit}. *)

  val next_match : t -> match_result option
  (** [next_match cursor] returns the next complete match.

      Returns [None] when no more matches exist. Each match contains all
      captures for that match. This is useful when you need to process all
      captures of a match together.

      Example:
      {[
        let rec process_matches () =
          match Query_cursor.next_match cursor with
          | Some m ->
              Array.iter
                (fun (idx, node) ->
                  Printf.printf "Capture %d: %s\n" idx (Node.kind node))
                m.captures;
              process_matches ()
          | None -> ()
      ]} *)

  val next_capture : t -> Query.t -> capture option
  (** [next_capture cursor query] returns the next capture.

      Returns [None] when no more captures exist. Captures are returned one at a
      time across all matches. This is useful for streaming processing of query
      results without assembling complete matches.

      Example:
      {[
        let rec collect acc =
          match Query_cursor.next_capture cursor query with
          | Some cap -> collect (cap.node :: acc)
          | None -> List.rev acc
      ]} *)
end
