let ocaml_interface =
`Assoc ([("name", `String ("OCaml Interface"));
          ("scopeName", `String ("source.ocaml.interface"));
          ("fileTypes", `List ([`String ("mli"); `String ("eliomi")]));
          ("patterns",
           `List ([`Assoc ([("include", `String ("source.ocaml#directives"))]);
                    `Assoc ([("include", `String ("source.ocaml#comments"))]);
                    `Assoc ([("include", `String ("source.ocaml#attributes"))
                              ]);
                    `Assoc ([("include", `String ("source.ocaml#extensions"))
                              ]);
                    `Assoc ([("include", `String ("#bindings"))]);
                    `Assoc ([("include", `String ("source.ocaml#operators"))]);
                    `Assoc ([("include", `String ("#keywords"))]);
                    `Assoc ([("include", `String ("source.ocaml#types"))]);
                    `Assoc ([("include", `String ("source.ocaml#identifiers"))
                              ])
                    ]));
          ("repository",
           `Assoc ([("bindings",
                     `Assoc ([("comment",
                               `String ("bindings that are shared between .ml and .mli syntaxes"));
                               ("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("optional labeled argument"));
                                                  ("name",
                                                   `String ("variable.parameter.optional.ocaml"));
                                                  ("match",
                                                   `String ("\\?([[:lower:]_][[:word:]']*)?"))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("labeled argument"));
                                                   ("name",
                                                    `String ("variable.parameter.labeled.ocaml"));
                                                   ("match",
                                                    `String ("~([[:lower:]_][[:word:]']*)?"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("type declaration"));
                                                   ("match",
                                                    `String ("\\b(type)[[:space:]]+(nonrec[[:space:]]+)?(_[[:space:]]+|[+-]?'[[:alpha:]][[:word:]']*[[:space:]]+|\\(.*\\)[[:space:]]+)?([[:lower:]_][[:word:]']*)"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("patterns",
                                                                 `List (
                                                                 [`Assoc (
                                                                   [("include",
                                                                    `String (
                                                                    "$base"))
                                                                    ])
                                                                   ]))
                                                                 ]));
                                                              ("4",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("and declaration for let bindings, type declarations, class bindings, class type definitions, or module constraints"));
                                                   ("match",
                                                    `String ("\\b(and)[[:space:]]+(?!(?:module|type|lazy)\\b(?!'))(virtual[[:space:]]+)?(_[[:space:]]+|'[[:alpha:]][[:word:]']*[[:space:]]+|\\(.*\\)[[:space:]]+)?([[:lower:]_][[:word:]']*)(?![[:word:]'])[[:space:]]*(?!,|::|[[:space:]])"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("patterns",
                                                                 `List (
                                                                 [`Assoc (
                                                                   [("include",
                                                                    `String (
                                                                    "$base"))
                                                                    ])
                                                                   ]))
                                                                 ]));
                                                              ("4",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("external declaration"));
                                                   ("begin",
                                                    `String ("\\b(external)[[:space:]]+([[:lower:]_][[:word:]']*)?"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]));
                                                   ("end",
                                                    `String ("(?<=]|\")[[:space:]]*(?:$|(?=]))"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("comment",
                                                                    `String (
                                                                    "string literal"));
                                                                    ("name",
                                                                    `String (
                                                                    "string.quoted.double.ocaml"));
                                                                    ("begin",
                                                                    `String (
                                                                    "\""));
                                                                    ("end",
                                                                    `String (
                                                                    "\""))]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "$base"))])
                                                             ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("val declaration for class instance variables"));
                                                   ("match",
                                                    `String ("\\b(val)[[:space:]]+(virtual)[[:space:]]+(mutable)[[:space:]]+([[:lower:]_][[:word:]']*)"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("4",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("val declaration for let bindings or class instance variables"));
                                                   ("match",
                                                    `String ("\\b(val|val!)[[:space:]]+(mutable[[:space:]]+)?(virtual[[:space:]]+)?([[:lower:]_][[:word:]']*)"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("4",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("class method declaration"));
                                                   ("match",
                                                    `String ("\\b(method)[[:space:]]+(virtual)[[:space:]]+(private)[[:space:]]+([[:lower:]_][[:word:]']*)"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("4",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("class method declaration"));
                                                   ("match",
                                                    `String ("\\b(method|method!)[[:space:]]+(private[[:space:]]+)?(virtual[[:space:]]+)?([[:lower:]_][[:word:]']*)"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("4",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("class specification or class type definition with type parameters"));
                                                   ("match",
                                                    `String ("\\b(class)[[:space:]]*([[:space:]]+type)?([[:space:]]+virtual)?[[:space:]]*(\\[.*\\])[[:space:]]*([[:lower:]_][[:word:]']*)"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("4",
                                                               `Assoc (
                                                               [("patterns",
                                                                 `List (
                                                                 [`Assoc (
                                                                   [("include",
                                                                    `String (
                                                                    "$base"))
                                                                    ])
                                                                   ]))
                                                                 ]));
                                                              ("5",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("class specification or class type definition"));
                                                   ("match",
                                                    `String ("\\b(class)[[:space:]]+(type[[:space:]]+)?(virtual[[:space:]]+)?([[:lower:]_][[:word:]']*)"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("4",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("named self in object"));
                                                   ("match",
                                                    `String ("\\b(object)[[:space:]]*\\([[:space:]]*([[:lower:]_][[:word:]']*)"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("module type of"));
                                                   ("begin",
                                                    `String ("\\b(module)[[:space:]]+(type)[[:space:]]+(of)\\b"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]))
                                                              ]));
                                                   ("end",
                                                    `String ("(?=val|external|type|exception|class|module|open|include|=)"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "source.ocaml"))
                                                                    ])
                                                             ]))
                                                   ])
                                         ]))
                               ]));
                     ("keywords",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("reserved ocaml keyword (in interfaces)"));
                                                  ("name",
                                                   `String ("keyword.other.ocaml.interface"));
                                                  ("match",
                                                   `String ("\\b(and|as|class|constraint|end|exception|external|functor|in|include|inherit|let[[:space:]]+open|method|module|mutable|nonrec|object|of|open|private|rec|sig|type|val|virtual|with)\\b(?!')"))
                                                  ])
                                         ]))
                                ]))
                     ]))
          ])


let ocaml =
`Assoc ([("name", `String ("OCaml"));
          ("scopeName", `String ("source.ocaml"));
          ("fileTypes",
           `List ([`String ("ml"); `String ("eliom"); `String (".ocamlinit")]));
          ("patterns",
           `List ([`Assoc ([("include", `String ("#directives"))]);
                    `Assoc ([("include", `String ("#comments"))]);
                    `Assoc ([("include", `String ("#strings"))]);
                    `Assoc ([("include", `String ("#characters"))]);
                    `Assoc ([("include", `String ("#attributes"))]);
                    `Assoc ([("include", `String ("#extensions"))]);
                    `Assoc ([("include", `String ("#modules"))]);
                    `Assoc ([("include", `String ("#bindings"))]);
                    `Assoc ([("include", `String ("#operators"))]);
                    `Assoc ([("include", `String ("#keywords"))]);
                    `Assoc ([("include", `String ("#literals"))]);
                    `Assoc ([("include", `String ("#types"))]);
                    `Assoc ([("include", `String ("#identifiers"))])]));
          ("repository",
           `Assoc ([("directives",
                     `Assoc ([("patterns",
                               `List ([`Assoc ([("comment",
                                                 `String ("line number directive"));
                                                 ("begin",
                                                  `String ("^[[:space:]]*(#)[[:space:]]*([[:digit:]]+)"));
                                                 ("end", `String ("$"));
                                                 ("beginCaptures",
                                                  `Assoc ([("1",
                                                            `Assoc ([("name",
                                                                    `String (
                                                                    "keyword.other.ocaml"))
                                                                    ]));
                                                            ("2",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "constant.numeric.decimal.integer.ocaml"))
                                                               ]))
                                                            ]));
                                                 ("contentName",
                                                  `String ("comment.line.directive.ocaml"))
                                                 ]);
                                        `Assoc ([("comment",
                                                  `String ("general, loading codes"));
                                                  ("begin",
                                                   `String ("^[[:space:]]*(#)[[:space:]]*(help|quit|cd|directory|remove_directory|load_rec|load|use|mod_use)"));
                                                  ("end", `String ("$"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.other.ocaml"))
                                                               ]));
                                                             ("2",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.other.ocaml"))
                                                                ]))
                                                             ]));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#strings"))
                                                                    ])
                                                            ]))
                                                  ]);
                                        `Assoc ([("comment",
                                                  `String ("environment queries"));
                                                  ("begin",
                                                   `String ("^[[:space:]]*(#)[[:space:]]*(show_class_type|show_class|show_exception|show_module_type|show_module|show_type|show_val|show)"));
                                                  ("end", `String ("$"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.other.ocaml"))
                                                               ]));
                                                             ("2",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.other.ocaml"))
                                                                ]))
                                                             ]));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#types"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#identifiers"))
                                                                    ])
                                                            ]))
                                                  ]);
                                        `Assoc ([("comment",
                                                  `String ("pretty-printing, tracing"));
                                                  ("begin",
                                                   `String ("^[[:space:]]*(#)[[:space:]]*(install_printer|print_depth|print_length|remove_printer|trace|untrace_all|untrace)"));
                                                  ("end", `String ("$"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.other.ocaml"))
                                                               ]));
                                                             ("2",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.other.ocaml"))
                                                                ]))
                                                             ]));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#literals"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#identifiers"))
                                                                    ])
                                                            ]))
                                                  ]);
                                        `Assoc ([("comment",
                                                  `String ("compiler options"));
                                                  ("begin",
                                                   `String ("^[[:space:]]*(#)[[:space:]]*(labels|ppx|principal|rectypes|warn_error|warnings)"));
                                                  ("end", `String ("$"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.other.ocaml"))
                                                               ]));
                                                             ("2",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.other.ocaml"))
                                                                ]))
                                                             ]));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#strings"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#literals"))
                                                                    ])
                                                            ]))
                                                  ]);
                                        `Assoc ([("comment",
                                                  `String ("topfind directives"));
                                                  ("begin",
                                                   `String ("^[[:space:]]*(#)[[:space:]]*(require|list|camlp4o|camlp4r|predicates|thread)"));
                                                  ("end", `String ("$"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.other.ocaml"))
                                                               ]));
                                                             ("2",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.other.ocaml"))
                                                                ]))
                                                             ]));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#strings"))
                                                                    ])
                                                            ]))
                                                  ]);
                                        `Assoc ([("comment",
                                                  `String ("cppo directives"));
                                                  ("begin",
                                                   `String ("^[[:space:]]*(#)[[:space:]]*(define|undef|ifdef|ifndef|if|else|elif|endif|include|warning|error|ext|endext)"));
                                                  ("end", `String ("$"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.other.ocaml"))
                                                               ]));
                                                             ("2",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.other.ocaml"))
                                                                ]))
                                                             ]));
                                                  ("patterns",
                                                   `List ([`Assoc ([("name",
                                                                    `String (
                                                                    "keyword.other.ocaml"));
                                                                    ("match",
                                                                    `String (
                                                                    "\\b(defined)\\b"))
                                                                    ]);
                                                            `Assoc ([("name",
                                                                    `String (
                                                                    "keyword.other.ocaml"));
                                                                    ("match",
                                                                    `String (
                                                                    "\\\\"))]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#comments"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#strings"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#characters"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#keywords"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#operators"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#literals"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#types"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#identifiers"))
                                                                    ])
                                                            ]))
                                                  ])
                                        ]))
                               ]));
                     ("comments",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("empty comment"));
                                                  ("name",
                                                   `String ("comment.block.ocaml"));
                                                  ("match",
                                                   `String ("\\(\\*\\*\\)"))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("ocamldoc comment"));
                                                   ("name",
                                                    `String ("comment.doc.ocaml"));
                                                   ("begin",
                                                    `String ("\\(\\*\\*"));
                                                   ("end", `String ("\\*\\)"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "source.ocaml.ocamldoc#markup"))
                                                                    ]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "#strings-in-comments"))
                                                               ]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "#comments"))])
                                                             ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("Cinaps comment"));
                                                   ("begin",
                                                    `String ("\\(\\*\\$"));
                                                   ("end", `String ("\\*\\)"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "comment.cinaps.ocaml"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "comment.cinaps.ocaml"))
                                                                ]))
                                                              ]));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("block comment"));
                                                   ("name",
                                                    `String ("comment.block.ocaml"));
                                                   ("begin",
                                                    `String ("\\(\\*"));
                                                   ("end", `String ("\\*\\)"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#strings-in-comments"))
                                                                    ]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "#comments"))])
                                                             ]))
                                                   ])
                                         ]))
                                ]));
                     ("strings-in-comments",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("char literal"));
                                                  ("match",
                                                   `String ("'(\\\\)?.'"))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("string literal"));
                                                   ("begin", `String ("\""));
                                                   ("end", `String ("\""));
                                                   ("patterns",
                                                    `List ([`Assoc ([("match",
                                                                    `String (
                                                                    "\\\\\\\\"))
                                                                    ]);
                                                             `Assoc (
                                                             [("match",
                                                               `String (
                                                               "\\\\\""))])
                                                             ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("quoted string literal"));
                                                   ("begin",
                                                    `String ("\\{[[:lower:]_]*\\|"));
                                                   ("end",
                                                    `String ("\\|[[:lower:]_]*\\}"))
                                                   ])
                                         ]))
                                ]));
                     ("strings",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("quoted string literal"));
                                                  ("name",
                                                   `String ("string.quoted.braced.ocaml"));
                                                  ("begin",
                                                   `String ("\\{(%%?[[:alpha:]_][[:word:]']*(\\.[[:alpha:]_][[:word:]']*)*[[:space:]]*)?[[:lower:]_]*\\|"));
                                                  ("end",
                                                   `String ("\\|[[:lower:]_]*\\}"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.other.extension.ocaml"))
                                                               ]))
                                                             ]))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("string literal"));
                                                   ("name",
                                                    `String ("string.quoted.double.ocaml"));
                                                   ("begin", `String ("\""));
                                                   ("end", `String ("\""));
                                                   ("patterns",
                                                    `List ([`Assoc ([("comment",
                                                                    `String (
                                                                    "escaped newline"));
                                                                    ("name",
                                                                    `String (
                                                                    "constant.character.escape.ocaml"));
                                                                    ("match",
                                                                    `String (
                                                                    "\\\\$"))
                                                                    ]);
                                                             `Assoc (
                                                             [("comment",
                                                               `String (
                                                               "escaped backslash"));
                                                               ("name",
                                                                `String (
                                                                "constant.character.escape.ocaml"));
                                                               ("match",
                                                                `String (
                                                                "\\\\\\\\"))
                                                               ]);
                                                             `Assoc (
                                                             [("comment",
                                                               `String (
                                                               "escaped quote or whitespace"));
                                                               ("name",
                                                                `String (
                                                                "constant.character.escape.ocaml"));
                                                               ("match",
                                                                `String (
                                                                "\\\\[\"'ntbr ]"))
                                                               ]);
                                                             `Assoc (
                                                             [("comment",
                                                               `String (
                                                               "character from decimal ASCII code"));
                                                               ("name",
                                                                `String (
                                                                "constant.character.escape.ocaml"));
                                                               ("match",
                                                                `String (
                                                                "\\\\[[:digit:]]{3}"))
                                                               ]);
                                                             `Assoc (
                                                             [("comment",
                                                               `String (
                                                               "character from hexadecimal ASCII code"));
                                                               ("name",
                                                                `String (
                                                                "constant.character.escape.ocaml"));
                                                               ("match",
                                                                `String (
                                                                "\\\\x[[:xdigit:]]{2}"))
                                                               ]);
                                                             `Assoc (
                                                             [("comment",
                                                               `String (
                                                               "character from octal ASCII code"));
                                                               ("name",
                                                                `String (
                                                                "constant.character.escape.ocaml"));
                                                               ("match",
                                                                `String (
                                                                "\\\\o[0-3][0-7]{2}"))
                                                               ]);
                                                             `Assoc (
                                                             [("comment",
                                                               `String (
                                                               "unicode character escape sequence"));
                                                               ("name",
                                                                `String (
                                                                "constant.character.escape.ocaml"));
                                                               ("match",
                                                                `String (
                                                                "\\\\u\\{[[:xdigit:]]{1,6}\\}"))
                                                               ]);
                                                             `Assoc (
                                                             [("comment",
                                                               `String (
                                                               "printf format string"));
                                                               ("name",
                                                                `String (
                                                                "constant.character.printf.ocaml"));
                                                               ("match",
                                                                `String (
                                                                "%[-0+ #]*([[:digit:]]+|\\*)?(.([[:digit:]]+|\\*))?[lLn]?[diunlLNxXosScCfFeEgGhHBbat!%@,]"))
                                                               ]);
                                                             `Assoc (
                                                             [("comment",
                                                               `String (
                                                               "unknown escape sequence"));
                                                               ("name",
                                                                `String (
                                                                "invalid.illegal.unknown-escape.ocaml"));
                                                               ("match",
                                                                `String (
                                                                "\\\\."))
                                                               ])
                                                             ]))
                                                   ])
                                         ]))
                                ]));
                     ("characters",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("character literal from escaped backslash"));
                                                  ("name",
                                                   `String ("string.quoted.single.ocaml"));
                                                  ("match",
                                                   `String ("'(\\\\\\\\)'"));
                                                  ("captures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "constant.character.escape.ocaml"))
                                                               ]))
                                                             ]))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("character literal from escaped quote or whitespace"));
                                                   ("name",
                                                    `String ("string.quoted.single.ocaml"));
                                                   ("match",
                                                    `String ("'(\\\\[\"'ntbr ])'"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "constant.character.escape.ocaml"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("character literal from decimal ASCII code"));
                                                   ("name",
                                                    `String ("string.quoted.single.ocaml"));
                                                   ("match",
                                                    `String ("'(\\\\[[:digit:]]{3})'"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "constant.character.escape.ocaml"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("character literal from hexadecimal ASCII code"));
                                                   ("name",
                                                    `String ("string.quoted.single.ocaml"));
                                                   ("match",
                                                    `String ("'(\\\\x[[:xdigit:]]{2})'"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "constant.character.escape.ocaml"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("character literal from octal ASCII code"));
                                                   ("name",
                                                    `String ("string.quoted.single.ocaml"));
                                                   ("match",
                                                    `String ("'(\\\\o[0-3][0-7]{2})'"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "constant.character.escape.ocaml"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("character literal from unknown escape sequence"));
                                                   ("name",
                                                    `String ("string.quoted.single.ocaml"));
                                                   ("match",
                                                    `String ("'(\\\\.)'"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "invalid.illegal.unknown-escape.ocaml"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("character literal"));
                                                   ("name",
                                                    `String ("string.quoted.single.ocaml"));
                                                   ("match", `String ("'.'"))
                                                   ])
                                         ]))
                                ]));
                     ("attributes",
                      `Assoc ([("begin",
                                `String ("\\[(@|@@|@@@)[[:space:]]*([[:alpha:]_]+(\\.[[:word:]']+)*)"));
                                ("end", `String ("\\]"));
                                ("beginCaptures",
                                 `Assoc ([("1",
                                           `Assoc ([("name",
                                                     `String ("keyword.operator.attribute.ocaml"))
                                                     ]));
                                           ("2",
                                            `Assoc ([("name",
                                                      `String ("keyword.other.attribute.ocaml"));
                                                      ("patterns",
                                                       `List ([`Assoc (
                                                                [("name",
                                                                  `String (
                                                                  "keyword.other.ocaml punctuation.other.period punctuation.separator.period"));
                                                                  ("match",
                                                                   `String (
                                                                   "\\."))
                                                                  ])
                                                                ]))
                                                      ]))
                                           ]));
                                ("patterns",
                                 `List ([`Assoc ([("include",
                                                   `String ("$self"))])
                                          ]))
                                ]));
                     ("extensions",
                      `Assoc ([("begin",
                                `String ("\\[(%|%%)[[:space:]]*([[:alpha:]_]+(\\.[[:word:]']+)*)"));
                                ("end", `String ("\\]"));
                                ("beginCaptures",
                                 `Assoc ([("1",
                                           `Assoc ([("name",
                                                     `String ("keyword.operator.extension.ocaml"))
                                                     ]));
                                           ("2",
                                            `Assoc ([("name",
                                                      `String ("keyword.other.extension.ocaml"));
                                                      ("patterns",
                                                       `List ([`Assoc (
                                                                [("name",
                                                                  `String (
                                                                  "keyword.other.ocaml punctuation.other.period punctuation.separator.period"));
                                                                  ("match",
                                                                   `String (
                                                                   "\\."))
                                                                  ])
                                                                ]))
                                                      ]))
                                           ]));
                                ("patterns",
                                 `List ([`Assoc ([("include",
                                                   `String ("$self"))])
                                          ]))
                                ]));
                     ("modules",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("begin",
                                                  `String ("\\b(sig)\\b"));
                                                  ("end",
                                                   `String ("\\b(end)\\b"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.other.ocaml"))
                                                               ]))
                                                             ]));
                                                  ("endCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.other.ocaml"))
                                                               ]))
                                                             ]));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "source.ocaml.interface"))
                                                                    ])
                                                            ]))
                                                  ]);
                                         `Assoc ([("begin",
                                                   `String ("\\b(struct)\\b"));
                                                   ("end",
                                                    `String ("\\b(end)\\b"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.other.ocaml"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.other.ocaml"))
                                                                ]))
                                                              ]));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]))
                                                   ])
                                         ]))
                                ]));
                     ("bindings",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("for loop"));
                                                  ("match",
                                                   `String ("\\b(for)[[:space:]]+([[:lower:]_][[:word:]']*)"));
                                                  ("captures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.ocaml"))
                                                               ]));
                                                             ("2",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "entity.name.function.binding.ocaml"))
                                                                ]))
                                                             ]))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("local open/exception/module"));
                                                   ("match",
                                                    `String ("\\b(let)[[:space:]]+(open|exception|module)\\b(?!')"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("let expression"));
                                                   ("match",
                                                    `String ("\\b(let)[[:space:]]+(?!lazy\\b(?!'))(rec[[:space:]]+)?(?!rec\\b(?!'))([[:lower:]_][[:word:]']*)(?![[:word:]'])[[:space:]]*(?!,|::|[[:space:]])"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("using binding operators"));
                                                   ("match",
                                                    `String ("\\b(let|and)([$&*+\\-/=>@^|<][!?$&*+\\-/=>@^|%:]*)[[:space:]]*(?!lazy\\b(?!'))([[:lower:]_][[:word:]']*)(?![[:word:]'])[[:space:]]*(?!,|::|[[:space:]])"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("first class module packing"));
                                                   ("match",
                                                    `String ("\\([[:space:]]*(val)[[:space:]]+([[:lower:]_][[:word:]']*)"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("patterns",
                                                                 `List (
                                                                 [`Assoc (
                                                                   [("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                                   ]))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("locally abstract types"));
                                                   ("match",
                                                    `String ("(?:\\(|(:))[[:space:]]*(type)((?:[[:space:]]+[[:lower:]_][[:word:]']*)+)"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.other.ocaml punctuation.other.colon punctuation.colon"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.ocaml"))
                                                                 ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "entity.name.function.binding.ocaml"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("optional labeled argument with type"));
                                                   ("begin",
                                                    `String ("(\\?)\\([[:space:]]*([[:lower:]_][[:word:]']*)"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "variable.parameter.optional.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "variable.parameter.optional.ocaml"))
                                                                 ]))
                                                              ]));
                                                   ("end", `String ("\\)"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("labeled argument with type"));
                                                   ("begin",
                                                    `String ("(~)\\([[:space:]]*([[:lower:]_][[:word:]']*)"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "variable.parameter.labeled.ocaml"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "variable.parameter.labeled.ocaml"))
                                                                 ]))
                                                              ]));
                                                   ("end", `String ("\\)"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]))
                                                   ]);
                                         `Assoc ([("include",
                                                   `String ("source.ocaml.interface#bindings"))
                                                   ])
                                         ]))
                                ]));
                     ("operators",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("binding operator"));
                                                  ("name",
                                                   `String ("keyword.ocaml"));
                                                  ("match",
                                                   `String ("\\b(let|and)[$&*+\\-/=>@^|<][!?$&*+\\-/=>@^|%:]*"))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("infix symbol"));
                                                   ("name",
                                                    `String ("keyword.operator.ocaml"));
                                                   ("match",
                                                    `String ("[$&*+\\-/=>@^%<][~!?$&*+\\-/=>@^|%<:.]*"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("infix symbol that begins with vertical bar"));
                                                   ("name",
                                                    `String ("keyword.operator.ocaml"));
                                                   ("match",
                                                    `String ("\\|[~!?$&*+\\-/=>@^|%<:.]+"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("vertical bar"));
                                                   ("name",
                                                    `String ("keyword.other.ocaml"));
                                                   ("match",
                                                    `String ("(?<!\\[)(\\|)(?!\\])"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("infix symbol"));
                                                   ("name",
                                                    `String ("keyword.operator.ocaml"));
                                                   ("match",
                                                    `String ("#[~!?$&*+\\-/=>@^|%<:.]+"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("prefix symbol"));
                                                   ("name",
                                                    `String ("keyword.operator.ocaml"));
                                                   ("match",
                                                    `String ("![~!?$&*+\\-/=>@^|%<:.]*"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("prefix symbol"));
                                                   ("name",
                                                    `String ("keyword.operator.ocaml"));
                                                   ("match",
                                                    `String ("[?~][~!?$&*+\\-/=>@^|%<:.]+"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("named operator"));
                                                   ("name",
                                                    `String ("keyword.operator.ocaml"));
                                                   ("match",
                                                    `String ("\\b(or|mod|land|lor|lxor|lsl|lsr|asr)\\b"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("method invocation"));
                                                   ("name",
                                                    `String ("keyword.other.ocaml"));
                                                   ("match", `String ("#"))]);
                                         `Assoc ([("comment",
                                                   `String ("type annotation"));
                                                   ("name",
                                                    `String ("keyword.other.ocaml punctuation.other.colon punctuation.colon"));
                                                   ("match", `String (":"))]);
                                         `Assoc ([("comment",
                                                   `String ("field accessor"));
                                                   ("name",
                                                    `String ("keyword.other.ocaml punctuation.other.period punctuation.separator.period"));
                                                   ("match", `String ("\\."))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("semicolon separator"));
                                                   ("name",
                                                    `String ("keyword.other.ocaml punctuation.separator.terminator punctuation.separator.semicolon"));
                                                   ("match", `String (";"))]);
                                         `Assoc ([("comment",
                                                   `String ("comma separator"));
                                                   ("name",
                                                    `String ("keyword.other.ocaml punctuation.comma punctuation.separator.comma"));
                                                   ("match", `String (","))])
                                         ]))
                                ]));
                     ("keywords",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("reserved ocaml keyword"));
                                                  ("name",
                                                   `String ("keyword.other.ocaml"));
                                                  ("match",
                                                   `String ("\\b(and|as|assert|begin|class|constraint|do|done|downto|else|end|exception|external|for|fun|function|functor|if|in|include|inherit|initializer|lazy|let|match|method|module|mutable|new|nonrec|object|of|open|private|rec|sig|struct|then|to|try|type|val|virtual|when|while|with)\\b(?!')"))
                                                  ])
                                         ]))
                                ]));
                     ("literals",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("boolean literal"));
                                                  ("name",
                                                   `String ("constant.language.boolean.ocaml"));
                                                  ("match",
                                                   `String ("\\b(true|false)\\b"))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("floating point decimal literal with exponent"));
                                                   ("name",
                                                    `String ("constant.numeric.decimal.float.ocaml"));
                                                   ("match",
                                                    `String ("\\b([[:digit:]][[:digit:]_]*(\\.[[:digit:]_]*)?[eE][+-]?[[:digit:]][[:digit:]_]*[g-zG-Z]?)\\b"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("floating point decimal literal"));
                                                   ("name",
                                                    `String ("constant.numeric.decimal.float.ocaml"));
                                                   ("match",
                                                    `String ("\\b([[:digit:]][[:digit:]_]*)(\\.[[:digit:]_]*[g-zG-Z]?\\b|\\.)"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("floating point hexadecimal literal with exponent part"));
                                                   ("name",
                                                    `String ("constant.numeric.hexadecimal.float.ocaml"));
                                                   ("match",
                                                    `String ("\\b((0x|0X)[[:xdigit:]][[:xdigit:]_]*(\\.[[:xdigit:]_]*)?[pP][+-]?[[:digit:]][[:digit:]_]*[g-zG-Z]?)\\b"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("floating point hexadecimal literal"));
                                                   ("name",
                                                    `String ("constant.numeric.hexadecimal.float.ocaml"));
                                                   ("match",
                                                    `String ("\\b((0x|0X)[[:xdigit:]][[:xdigit:]_]*)(\\.[[:xdigit:]_]*[g-zG-Z]?\\b|\\.)"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("decimal integer literal"));
                                                   ("name",
                                                    `String ("constant.numeric.decimal.integer.ocaml"));
                                                   ("match",
                                                    `String ("\\b([[:digit:]][[:digit:]_]*[lLng-zG-Z]?)\\b"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("hexadecimal integer literal"));
                                                   ("name",
                                                    `String ("constant.numeric.hexadecimal.integer.ocaml"));
                                                   ("match",
                                                    `String ("\\b((0x|0X)[[:xdigit:]][[:xdigit:]_]*[lLng-zG-Z]?)\\b"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("octal integer literal"));
                                                   ("name",
                                                    `String ("constant.numeric.octal.integer.ocaml"));
                                                   ("match",
                                                    `String ("\\b((0o|0O)[0-7][0-7_]*[lLng-zG-Z]?)\\b"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("binary integer literal"));
                                                   ("name",
                                                    `String ("constant.numeric.binary.integer.ocaml"));
                                                   ("match",
                                                    `String ("\\b((0b|0B)[0-1][0-1_]*[lLng-zG-Z]?)\\b"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("unit literal"));
                                                   ("name",
                                                    `String ("constant.language.unit.ocaml"));
                                                   ("match",
                                                    `String ("\\(\\)"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("parentheses"));
                                                   ("begin", `String ("\\("));
                                                   ("end", `String ("\\)"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("empty array"));
                                                   ("name",
                                                    `String ("constant.language.array.ocaml"));
                                                   ("match",
                                                    `String ("\\[\\|\\|\\]"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("array"));
                                                   ("begin",
                                                    `String ("\\[\\|"));
                                                   ("end", `String ("\\|\\]"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("empty list"));
                                                   ("name",
                                                    `String ("constant.language.list.ocaml"));
                                                   ("match",
                                                    `String ("\\[\\]"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("list"));
                                                   ("begin", `String ("\\["));
                                                   ("end", `String ("]"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("braces"));
                                                   ("begin", `String ("\\{"));
                                                   ("end", `String ("\\}"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]))
                                                   ])
                                         ]))
                                ]));
                     ("types",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("type parameter"));
                                                  ("name",
                                                   `String ("storage.type.ocaml"));
                                                  ("match",
                                                   `String ("'[[:alpha:]][[:word:]']*\\b|'_\\b"))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("weak type parameter"));
                                                   ("name",
                                                    `String ("storage.type.weak.ocaml"));
                                                   ("match",
                                                    `String ("'_[[:alpha:]][[:word:]']*\\b"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("builtin type"));
                                                   ("name",
                                                    `String ("support.type.ocaml"));
                                                   ("match",
                                                    `String ("\\b(unit|bool|int|int32|int64|nativeint|float|char|bytes|string)\\b"))
                                                   ])
                                         ]))
                                ]));
                     ("identifiers",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("wildcard underscore"));
                                                  ("name",
                                                   `String ("constant.language.ocaml"));
                                                  ("match",
                                                   `String ("\\b_\\b"))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("capital identifier for constructor, exception, or module"));
                                                   ("name",
                                                    `String ("constant.language.capital-identifier.ocaml"));
                                                   ("match",
                                                    `String ("\\b[[:upper:]][[:word:]']*('|\\b)"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("lowercase identifier"));
                                                   ("name",
                                                    `String ("source.ocaml"));
                                                   ("match",
                                                    `String ("\\b[[:lower:]_][[:word:]']*('|\\b)"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("polymorphic variant tag"));
                                                   ("name",
                                                    `String ("constant.language.polymorphic-variant.ocaml"));
                                                   ("match",
                                                    `String ("\\`[[:alpha:]][[:word:]']*\\b"))
                                                   ]);
                                         `Assoc ([("comment",
                                                   `String ("empty list (can be used as a constructor)"));
                                                   ("name",
                                                    `String ("constant.language.list.ocaml"));
                                                   ("match",
                                                    `String ("\\[\\]"))
                                                   ])
                                         ]))
                                ]))
                     ]))
          ])


let dune =
`Assoc ([("name", `String ("Dune")); ("scopeName", `String ("source.dune"));
          ("fileTypes", `List ([`String ("dune"); `String ("jbuild")]));
          ("patterns",
           `List ([`Assoc ([("include", `String ("#comments"))]);
                    `Assoc ([("name", `String ("meta.stanza.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(library|rule|executable|executables|rule|ocamllex|ocamlyacc|menhir|install|alias|copy_files|copy_files#|jbuild_version|include)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("meta.class.stanza.dune"))
                                                   ]))
                                         ]));
                              ("patterns",
                               `List ([`Assoc ([("include", `String ("$self"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name",
                              `String ("meta.stanza.library.field.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(name|public_name|synopsis|install_c_headers|ppx_runtime_libraries|c_flags|cxx_flags|c_names|cxx_names|library_flags|c_library_flags|virtual_deps|modes|kind|wrapped|optional|self_build_stubs_archive|no_dynlink|ppx\\.driver)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("keyword.other.dune"))
                                                   ]))
                                         ]));
                              ("patterns",
                               `List ([`Assoc ([("include", `String ("$self"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name", `String ("meta.stanza.rule.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(targets|deps|locks|loc|mode|action)[[:space:]]"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("keyword.other.dune"))
                                                   ]))
                                         ]));
                              ("end", `String ("\\)"));
                              ("patterns",
                               `List ([`Assoc ([("include", `String ("$self"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name", `String ("meta.mono-sexp.dune"));
                              ("match",
                               `String ("\\([[:space:]]*(fallback|optional)[[:space:]]*\\)"));
                              ("captures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("keyword.other.dune"))
                                                   ]))
                                         ]))
                              ]);
                    `Assoc ([("name",
                              `String ("meta.stanza.rule.action.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(run|chdir|setenv|with-stdout-to|with-stderr-to|with-outputs-to|ignore-stdout|ignore-stderr|ignore-outputs|progn|echo|cat|copy|copy#|system|bash|write-file|diff|diff\\?)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("entity.name.function.action.dune"))
                                                   ]))
                                         ]));
                              ("patterns",
                               `List ([`Assoc ([("include", `String ("$self"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name", `String ("meta.stanza.install.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(section)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("keyword.other.dune"))
                                                   ]))
                                         ]));
                              ("patterns",
                               `List ([`Assoc ([("name",
                                                 `String ("constant.language.rule.mode.dune"));
                                                 ("match",
                                                  `String ("\\b(lib|libexec|bin|sbin|toplevel|share|share_root|etc|doc|stublibs|man|misc)\\b"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name", `String ("meta.stanza.install.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(files)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("keyword.other.dune"))
                                                   ]))
                                         ]));
                              ("patterns",
                               `List ([`Assoc ([("include", `String ("$self"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name", `String ("meta.library.kind.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(normal|ppx_deriver|ppx_rewriter)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("constant.language.rule.mode.dune"))
                                                   ]))
                                         ]))
                              ]);
                    `Assoc ([("name",
                              `String ("meta.stanza.executables.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(name|link_executables|link_flags|modes)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("keyword.other.dune"))
                                                   ]))
                                         ]));
                              ("patterns",
                               `List ([`Assoc ([("include", `String ("$self"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name",
                              `String ("meta.stanza.lib-or-exec.buildable.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(preprocess|preprocessor_deps|lint|modules|modules_without_implementation|libraries|flags|ocamlc_flags|ocamlopt_flags|js_of_ocaml|allow_overlapping_dependencies|per_module)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("keyword.other.dune"))
                                                   ]))
                                         ]));
                              ("patterns",
                               `List ([`Assoc ([("include", `String ("$self"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name",
                              `String ("meta.stanza.lib-or-exec.buildable.preprocess.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(no_preprocessing|action|pps)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("keyword.other.dune"))
                                                   ]))
                                         ]));
                              ("patterns",
                               `List ([`Assoc ([("include", `String ("$self"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name",
                              `String ("meta.stanza.lib-or-exec.buildable.preprocess_deps.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(file|alias|alias_rec|glob_files|files_recursively_in)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("keyword.other.dune"))
                                                   ]))
                                         ]));
                              ("patterns",
                               `List ([`Assoc ([("include", `String ("$self"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name",
                              `String ("meta.stanza.lib-or-exec.buildable.libraries.dune"));
                              ("begin",
                               `String ("\\([[:space:]]*(select)[[:space:]]"));
                              ("end", `String ("\\)"));
                              ("beginCaptures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("keyword.other.dune"))
                                                   ]))
                                         ]));
                              ("patterns",
                               `List ([`Assoc ([("include", `String ("$self"))
                                                 ])
                                        ]))
                              ]);
                    `Assoc ([("name", `String ("constant.numeric.dune"));
                              ("match", `String ("\\b\\d+\\b"))]);
                    `Assoc ([("name", `String ("constant.language.dune"));
                              ("match", `String ("(true|false)"))]);
                    `Assoc ([("name", `String ("keyword.other.dune"));
                              ("match",
                               `String ("[[:space:]](as|from|->)[[:space:]]"))
                              ]);
                    `Assoc ([("name", `String ("keyword.other.dune"));
                              ("match", `String ("(\\!)"))]);
                    `Assoc ([("name", `String ("constant.language.flag.dune"));
                              ("match", `String ("(:\\w+)\\b"))]);
                    `Assoc ([("name",
                              `String ("constant.language.rule.mode.dune"));
                              ("match",
                               `String ("\\b(standard|fallback|promote|promote-until-then)\\b"))
                              ]);
                    `Assoc ([("include", `String ("#string"))]);
                    `Assoc ([("include", `String ("#variable"))]);
                    `Assoc ([("include", `String ("#list"))]);
                    `Assoc ([("include", `String ("#atom"))])]));
          ("repository",
           `Assoc ([("comments",
                     `Assoc ([("patterns",
                               `List ([`Assoc ([("name",
                                                 `String ("comment.block.dune"));
                                                 ("begin", `String ("#\\|"));
                                                 ("beginCaptures",
                                                  `Assoc ([("0",
                                                            `Assoc ([("name",
                                                                    `String (
                                                                    "punctuation.definition.comment.begin.dune"))
                                                                    ]))
                                                            ]));
                                                 ("end", `String ("\\|#"));
                                                 ("endCaptures",
                                                  `Assoc ([("0",
                                                            `Assoc ([("name",
                                                                    `String (
                                                                    "punctuation.definition.comment.end.dune"))
                                                                    ]))
                                                            ]));
                                                 ("patterns",
                                                  `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#comments"))
                                                                    ])
                                                           ]))
                                                 ]);
                                        `Assoc ([("name",
                                                  `String ("comment.sexp.dune"));
                                                  ("begin",
                                                   `String ("#;[[:space:]]*\\("));
                                                  ("end", `String ("\\)"));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#comment-inner"))
                                                                    ])
                                                            ]))
                                                  ]);
                                        `Assoc ([("name",
                                                  `String ("comment.line.dune"));
                                                  ("match", `String (";.*$"))
                                                  ])
                                        ]))
                               ]));
                     ("comment-inner",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("name",
                                                  `String ("comment.sexp.inner.dune"));
                                                  ("begin", `String ("\\("));
                                                  ("end", `String ("\\)"));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#comment-inner"))
                                                                    ])
                                                            ]))
                                                  ])
                                         ]))
                                ]));
                     ("string",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("name",
                                                  `String ("string.quoted.double.dune"));
                                                  ("begin",
                                                   `String ("(?=[^\\\\])(\")"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.string.begin.dune"))
                                                               ]))
                                                             ]));
                                                  ("end", `String ("(\")"));
                                                  ("endCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.string.end.dune"))
                                                               ]))
                                                             ]));
                                                  ("patterns",
                                                   `List ([`Assoc ([("name",
                                                                    `String (
                                                                    "constant.character.string.escape.dune"));
                                                                    ("match",
                                                                    `String (
                                                                    "\\\\\""))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "#variable"))
                                                                    ])
                                                            ]))
                                                  ])
                                         ]))
                                ]));
                     ("variable",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("name",
                                                  `String ("variable.other.dune"));
                                                  ("match",
                                                   `String ("\\${[^}]*}"))
                                                  ])
                                         ]))
                                ]));
                     ("list",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("name",
                                                  `String ("meta.list.dune"));
                                                  ("begin", `String ("(\\()"));
                                                  ("end", `String ("(\\))"));
                                                  ("captures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "entity.tag.list.parenthesis.dune"))
                                                               ]))
                                                             ]));
                                                  ("comment",
                                                   `String ("ok, for this one, I didn't know what to choose"));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                            ]))
                                                  ])
                                         ]))
                                ]));
                     ("atom",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("name",
                                                  `String ("meta.atom.dune"));
                                                  ("match",
                                                   `String ("\\b[^[[:space:]]]+\\b"))
                                                  ])
                                         ]))
                                ]))
                     ]))
          ])


let shell =
`Assoc ([("fileTypes",
          `List ([`String ("sh"); `String ("bash"); `String ("zsh");
                   `String ("bashrc"); `String ("bash_profile");
                   `String ("bash_login"); `String ("profile");
                   `String ("bash_logout"); `String (".textmate_init")]));
          ("firstLineMatch",
           `String ("^#!.*\\b(bash|zsh|sh|tcsh)|^#.*-\\*-.*\\bshell-script\\b.*-\\*-"));
          ("repository",
           `Assoc ([("keyword",
                     `Assoc ([("patterns",
                               `List ([`Assoc ([("match",
                                                 `String ("(?<=^|;|&|\\s)(?:if|then|else|elif|fi|for|in|do|done|select|case|continue|esac|while|until|return|coproc)(?=\\s|;|&|$)"));
                                                 ("name",
                                                  `String ("keyword.control.shell"))
                                                 ]);
                                        `Assoc ([("match",
                                                  `String ("(?<=^|;|&|\\s)(?:export|declare|typeset|local|readonly)(?=\\s|;|&|$)"));
                                                  ("name",
                                                   `String ("storage.modifier.shell"))
                                                  ])
                                        ]))
                               ]));
                     ("function-definition",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("begin",
                                                  `String ("(?<=^|;|&)\\s*(function)\\s+([^\\s<>;'\"\\\\|$&()]+)(?:\\s*(\\(\\)))?"));
                                                  ("endCaptures",
                                                   `Assoc ([("0",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.function.shell"))
                                                               ]))
                                                             ]));
                                                  ("end", `String (";|&|\\n"));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                            ]));
                                                  ("name",
                                                   `String ("meta.function.shell"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "storage.type.function.shell"))
                                                               ]));
                                                             ("2",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "entity.name.function.shell"))
                                                                ]));
                                                             ("3",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.arguments.shell"))
                                                                ]))
                                                             ]))
                                                  ]);
                                         `Assoc ([("begin",
                                                   `String ("(?<=^|;|&)\\s*([^\\s<>;'\"\\\\|$&()]+)\\s*(\\(\\))"));
                                                   ("endCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.function.shell"))
                                                                ]))
                                                              ]));
                                                   ("end",
                                                    `String (";|&|\\n"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]));
                                                   ("name",
                                                    `String ("meta.function.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "entity.name.function.shell"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "punctuation.definition.arguments.shell"))
                                                                 ]))
                                                              ]))
                                                   ])
                                         ]))
                                ]));
                     ("herestring",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("match",
                                                  `String ("(<<<)\\s*((')[^']*('))"));
                                                  ("name",
                                                   `String ("meta.herestring.shell"));
                                                  ("captures",
                                                   `Assoc ([("3",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.string.begin.shell"))
                                                               ]));
                                                             ("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.herestring.shell"))
                                                                ]));
                                                             ("4",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.end.shell"))
                                                                ]));
                                                             ("2",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "string.quoted.single.herestring.shell"))
                                                                ]))
                                                             ]))
                                                  ]);
                                         `Assoc ([("match",
                                                   `String ("(<<<)\\s*((\")(\\\\(\"|\\\\)|[^\"])*(\"))"));
                                                   ("name",
                                                    `String ("meta.herestring.shell"));
                                                   ("captures",
                                                    `Assoc ([("3",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.begin.shell"))
                                                                ]));
                                                              ("1",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.operator.herestring.shell"))
                                                                 ]));
                                                              ("6",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "punctuation.definition.string.end.shell"))
                                                                 ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "string.quoted.double.herestring.shell"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("match",
                                                   `String ("(<<<)\\s*(([^\\s\\\\]|\\\\.)+)"));
                                                   ("name",
                                                    `String ("meta.herestring.shell"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.herestring.shell"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "string.unquoted.herestring.shell"))
                                                                 ]))
                                                              ]))
                                                   ])
                                         ]))
                                ]));
                     ("pathname",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("match",
                                                  `String ("(?<=\\s|:|=|^)~"));
                                                  ("name",
                                                   `String ("keyword.operator.tilde.shell"))
                                                  ]);
                                         `Assoc ([("match",
                                                   `String ("\\*|\\?"));
                                                   ("name",
                                                    `String ("keyword.operator.glob.shell"))
                                                   ]);
                                         `Assoc ([("begin",
                                                   `String ("([?*+@!])(\\()"));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.extglob.shell"))
                                                                ]))
                                                              ]));
                                                   ("end", `String ("(\\))"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]));
                                                   ("name",
                                                    `String ("meta.structure.extglob.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.extglob.shell"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "punctuation.definition.extglob.shell"))
                                                                 ]))
                                                              ]))
                                                   ])
                                         ]))
                                ]));
                     ("pipeline",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("match",
                                                  `String ("(?<=^|;|&|\\s)(time)(?=\\s|;|&|$)"));
                                                  ("name",
                                                   `String ("keyword.other.shell"))
                                                  ]);
                                         `Assoc ([("match", `String ("[|!]"));
                                                   ("name",
                                                    `String ("keyword.operator.pipe.shell"))
                                                   ])
                                         ]))
                                ]));
                     ("compound-command",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("begin",
                                                  `String ("(\\[{2})"));
                                                  ("end",
                                                   `String ("(\\]{2})"));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#logical-expression"))
                                                                    ]);
                                                            `Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                            ]));
                                                  ("name",
                                                   `String ("meta.scope.logical-expression.shell"));
                                                  ("captures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.logical-expression.shell"))
                                                               ]))
                                                             ]))
                                                  ]);
                                         `Assoc ([("begin",
                                                   `String ("(\\({2})"));
                                                   ("endCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.end.shell"))
                                                                ]))
                                                              ]));
                                                   ("end",
                                                    `String ("(\\){2})"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#math"))
                                                                    ])
                                                             ]));
                                                   ("name",
                                                    `String ("string.other.math.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.begin.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("begin", `String ("(\\()"));
                                                   ("end", `String ("(\\))"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]));
                                                   ("name",
                                                    `String ("meta.scope.subshell.shell"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.subshell.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("begin",
                                                   `String ("(?<=\\s|^)(\\{)(?=\\s|$)"));
                                                   ("end",
                                                    `String ("(?<=^|;)\\s*(\\})"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]));
                                                   ("name",
                                                    `String ("meta.scope.group.shell"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.group.shell"))
                                                                ]))
                                                              ]))
                                                   ])
                                         ]))
                                ]));
                     ("lines",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("match",
                                                  `String ("\\\\\\n"));
                                                  ("name",
                                                   `String ("constant.character.escape.newline.shell"))
                                                  ])
                                         ]))
                                ]));
                     ("heredoc",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("end",
                                                  `String ("^\\t*(RUBY)(?=\\s|;|&|$)"));
                                                  ("begin",
                                                   `String ("(<<)-\\s*(\"|'|)(RUBY)\\2"));
                                                  ("beginCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.operator.heredoc.shell"))
                                                               ]));
                                                             ("3",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                             ]));
                                                  ("contentName",
                                                   `String ("source.ruby.embedded.shell"));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "source.ruby"))
                                                                    ])
                                                            ]));
                                                  ("captures",
                                                   `Assoc ([("0",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.string.shell"))
                                                               ]))
                                                             ]));
                                                  ("endCaptures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.control.heredoc-token.shell"))
                                                               ]))
                                                             ]));
                                                  ("name",
                                                   `String ("string.unquoted.heredoc.no-indent.ruby.shell"))
                                                  ]);
                                         `Assoc ([("end",
                                                   `String ("^(RUBY)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)\\s*(\"|'|)(RUBY)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("source.ruby.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "source.ruby"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.ruby.shell"))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("^\\t*(PYTHON)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)-\\s*(\"|'|)(PYTHON)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("source.python.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "source.python"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.no-indent.python.shell"))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("^(PYTHON)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)\\s*(\"|'|)(PYTHON)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("source.python.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "source.python"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.python.shell"))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("^\\t*(APPLESCRIPT)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)-\\s*(\"|'|)(APPLESCRIPT)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("source.applescript.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "source.applescript"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.no-indent.applescript.shell"))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("^(APPLESCRIPT)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)\\s*(\"|'|)(APPLESCRIPT)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("source.applescript.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "source.applescript"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.applescript.shell"))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("^\\t*(HTML)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)-\\s*(\"|'|)(HTML)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("text.html.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "text.html.basic"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.no-indent.html.shell"))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("^(HTML)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)\\s*(\"|'|)(HTML)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("text.html.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "text.html.basic"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.html.shell"))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("^\\t*(MARKDOWN)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)-\\s*(\"|'|)(MARKDOWN)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("text.html.markdown.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "text.html.markdown"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.no-indent.markdown.shell"))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("^(MARKDOWN)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)\\s*(\"|'|)(MARKDOWN)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("text.html.markdown.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "text.html.markdown"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.markdown.shell"))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("^\\t*(TEXTILE)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)-\\s*(\"|'|)(TEXTILE)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("text.html.textile.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "text.html.textile"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.no-indent.textile.shell"))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("^(TEXTILE)(?=\\s|;|&|$)"));
                                                   ("begin",
                                                    `String ("(<<)\\s*(\"|'|)(TEXTILE)\\2"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("contentName",
                                                    `String ("text.html.textile.embedded.shell"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "text.html.textile"))
                                                                    ])
                                                             ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.textile.shell"))
                                                   ]);
                                         `Assoc ([("begin",
                                                   `String ("(<<)-\\s*(\"|'|)\\\\?(\\w+)\\2"));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("end",
                                                    `String ("^\\t*(\\3)(?=\\s|;|&|$)"));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.no-indent.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("begin",
                                                   `String ("(<<)\\s*(\"|'|)\\\\?(\\w+)\\2"));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.heredoc-token.shell"))
                                                                ]))
                                                              ]));
                                                   ("end",
                                                    `String ("^(\\3)(?=\\s|;|&|$)"));
                                                   ("name",
                                                    `String ("string.unquoted.heredoc.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.operator.heredoc.shell"))
                                                                ]));
                                                              ("3",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "keyword.control.heredoc-token.shell"))
                                                                 ]))
                                                              ]));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.shell"))
                                                                ]))
                                                              ]))
                                                   ])
                                         ]))
                                ]));
                     ("string",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("match", `String ("\\\\."));
                                                  ("name",
                                                   `String ("constant.character.escape.shell"))
                                                  ]);
                                         `Assoc ([("begin", `String ("'"));
                                                   ("endCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.end.shell"))
                                                                ]))
                                                              ]));
                                                   ("end", `String ("'"));
                                                   ("name",
                                                    `String ("string.quoted.single.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.begin.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("begin",
                                                   `String ("\\$?\""));
                                                   ("endCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.end.shell"))
                                                                ]))
                                                              ]));
                                                   ("end", `String ("\""));
                                                   ("patterns",
                                                    `List ([`Assoc ([("match",
                                                                    `String (
                                                                    "\\\\[\\$`\"\\\\\\n]"));
                                                                    ("name",
                                                                    `String (
                                                                    "constant.character.escape.shell"))
                                                                    ]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "#variable"))]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "#interpolation"))
                                                               ])
                                                             ]));
                                                   ("name",
                                                    `String ("string.quoted.double.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.begin.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("begin", `String ("\\$'"));
                                                   ("endCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.end.shell"))
                                                                ]))
                                                              ]));
                                                   ("end", `String ("'"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("match",
                                                                    `String (
                                                                    "\\\\(a|b|e|f|n|r|t|v|\\\\|')"));
                                                                    ("name",
                                                                    `String (
                                                                    "constant.character.escape.ansi-c.shell"))
                                                                    ]);
                                                             `Assoc (
                                                             [("match",
                                                               `String (
                                                               "\\\\[0-9]{3}"));
                                                               ("name",
                                                                `String (
                                                                "constant.character.escape.octal.shell"))
                                                               ]);
                                                             `Assoc (
                                                             [("match",
                                                               `String (
                                                               "\\\\x[0-9a-fA-F]{2}"));
                                                               ("name",
                                                                `String (
                                                                "constant.character.escape.hex.shell"))
                                                               ]);
                                                             `Assoc (
                                                             [("match",
                                                               `String (
                                                               "\\\\c."));
                                                               ("name",
                                                                `String (
                                                                "constant.character.escape.control-char.shell"))
                                                               ])
                                                             ]));
                                                   ("name",
                                                    `String ("string.quoted.single.dollar.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.begin.shell"))
                                                                ]))
                                                              ]))
                                                   ])
                                         ]))
                                ]));
                     ("logical-expression",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("comment",
                                                  `String ("do we want a special rule for ( expr )?"));
                                                  ("match",
                                                   `String ("=[=~]?|!=?|<|>|&&|\\|\\|"));
                                                  ("name",
                                                   `String ("keyword.operator.logical.shell"))
                                                  ]);
                                         `Assoc ([("match",
                                                   `String ("(?<!\\S)-(nt|ot|ef|eq|ne|l[te]|g[te]|[a-hknoprstuwxzOGLSN])"));
                                                   ("name",
                                                    `String ("keyword.operator.logical.shell"))
                                                   ])
                                         ]))
                                ]));
                     ("comment",
                      `Assoc ([("begin",
                                `String ("(^[ \\t]+)?(?<!\\S)(?=#)(?!#\\{)"));
                                ("end", `String ("(?!\\G)"));
                                ("patterns",
                                 `List ([`Assoc ([("begin", `String ("^(#!)"));
                                                   ("end", `String ("\\n"));
                                                   ("name",
                                                    `String ("comment.line.shebang.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.comment.line.shebang.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                          `Assoc ([("begin", `String ("#"));
                                                    ("end", `String ("\\n"));
                                                    ("name",
                                                     `String ("comment.line.number-sign.shell"));
                                                    ("beginCaptures",
                                                     `Assoc ([("0",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "punctuation.definition.comment.shell"))
                                                                 ]))
                                                               ]))
                                                    ])
                                          ]));
                                ("beginCaptures",
                                 `Assoc ([("1",
                                           `Assoc ([("name",
                                                     `String ("punctuation.whitespace.comment.leading.shell"))
                                                     ]))
                                           ]))
                                ]));
                     ("support",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("match",
                                                  `String ("(?<=^|;|&|\\s)(?::|\\.)(?=\\s|;|&|$)"));
                                                  ("name",
                                                   `String ("support.function.builtin.shell"))
                                                  ]);
                                         `Assoc ([("match",
                                                   `String ("(?<=^|;|&|\\s)(?:alias|bg|bind|break|builtin|caller|cd|command|compgen|complete|dirs|disown|echo|enable|eval|exec|exit|false|fc|fg|getopts|hash|help|history|jobs|kill|let|logout|mapfile|popd|printf|pushd|pwd|read(array)?|readonly|set|shift|shopt|source|suspend|test|times|trap|true|type|ulimit|umask|unalias|unset|wait)(?=\\s|;|&|$)"));
                                                   ("name",
                                                    `String ("support.function.builtin.shell"))
                                                   ])
                                         ]))
                                ]));
                     ("math",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("include",
                                                  `String ("#variable"))]);
                                         `Assoc ([("match",
                                                   `String ("\\+{1,2}|-{1,2}|!|~|\\*{1,2}|/|%|<[<=]?|>[>=]?|==|!=|\\^|\\|{1,2}|&{1,2}|\\?|\\:|,|=|[*/%+\\-&^|]=|<<=|>>="));
                                                   ("name",
                                                    `String ("keyword.operator.arithmetic.shell"))
                                                   ]);
                                         `Assoc ([("match",
                                                   `String ("0[xX]\\h+"));
                                                   ("name",
                                                    `String ("constant.numeric.hex.shell"))
                                                   ]);
                                         `Assoc ([("match", `String ("0\\d+"));
                                                   ("name",
                                                    `String ("constant.numeric.octal.shell"))
                                                   ]);
                                         `Assoc ([("match",
                                                   `String ("\\d{1,2}#[0-9a-zA-Z@_]+"));
                                                   ("name",
                                                    `String ("constant.numeric.other.shell"))
                                                   ]);
                                         `Assoc ([("match", `String ("\\d+"));
                                                   ("name",
                                                    `String ("constant.numeric.integer.shell"))
                                                   ])
                                         ]))
                                ]));
                     ("variable",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("match",
                                                  `String ("(\\$)[a-zA-Z_][a-zA-Z0-9_]*"));
                                                  ("name",
                                                   `String ("variable.other.normal.shell"));
                                                  ("captures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.variable.shell"))
                                                               ]))
                                                             ]))
                                                  ]);
                                         `Assoc ([("match",
                                                   `String ("(\\$)[-*@#?$!0_]"));
                                                   ("name",
                                                    `String ("variable.other.special.shell"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.variable.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("match",
                                                   `String ("(\\$)[1-9]"));
                                                   ("name",
                                                    `String ("variable.other.positional.shell"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.variable.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("begin",
                                                   `String ("\\$\\{"));
                                                   ("end", `String ("\\}"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("match",
                                                                    `String (
                                                                    "!|:[-=?+]?|\\*|@|#{1,2}|%{1,2}|/"));
                                                                    ("name",
                                                                    `String (
                                                                    "keyword.operator.expansion.shell"))
                                                                    ]);
                                                             `Assoc (
                                                             [("match",
                                                               `String (
                                                               "(\\[)([^\\]]+)(\\])"));
                                                               ("captures",
                                                                `Assoc (
                                                                [("1",
                                                                  `Assoc (
                                                                  [("name",
                                                                    `String (
                                                                    "punctuation.section.array.shell"))
                                                                    ]));
                                                                  ("3",
                                                                   `Assoc (
                                                                   [("name",
                                                                    `String (
                                                                    "punctuation.section.array.shell"))
                                                                    ]))
                                                                  ]))
                                                               ]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "#string"))]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "#variable"))]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "#interpolation"))
                                                               ])
                                                             ]));
                                                   ("name",
                                                    `String ("variable.other.bracket.shell"));
                                                   ("captures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.variable.shell"))
                                                                ]))
                                                              ]))
                                                   ])
                                         ]))
                                ]));
                     ("list",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("match",
                                                  `String (";|&&|&|\\|\\|"));
                                                  ("name",
                                                   `String ("keyword.operator.list.shell"))
                                                  ])
                                         ]))
                                ]));
                     ("redirection",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("begin",
                                                  `String ("[><]\\("));
                                                  ("endCaptures",
                                                   `Assoc ([("0",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.string.end.shell"))
                                                               ]))
                                                             ]));
                                                  ("end", `String ("\\)"));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                            ]));
                                                  ("name",
                                                   `String ("string.interpolated.process-substitution.shell"));
                                                  ("beginCaptures",
                                                   `Assoc ([("0",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.string.begin.shell"))
                                                               ]))
                                                             ]))
                                                  ]);
                                         `Assoc ([("comment",
                                                   `String ("valid: &>word >&word >word [n]>&[n] [n]<word [n]>word [n]>>word [n]<&word (last one is duplicate)"));
                                                   ("match",
                                                    `String ("&>|\\d*>&\\d*|\\d*(>>|>|<)|\\d*<&|\\d*<>"));
                                                   ("name",
                                                    `String ("keyword.operator.redirect.shell"))
                                                   ])
                                         ]))
                                ]));
                     ("interpolation",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("begin",
                                                  `String ("\\$\\({2}"));
                                                  ("endCaptures",
                                                   `Assoc ([("0",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.string.end.shell"))
                                                               ]))
                                                             ]));
                                                  ("end", `String ("\\){2}"));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "#math"))
                                                                    ])
                                                            ]));
                                                  ("name",
                                                   `String ("string.other.math.shell"));
                                                  ("beginCaptures",
                                                   `Assoc ([("0",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.definition.string.begin.shell"))
                                                               ]))
                                                             ]))
                                                  ]);
                                         `Assoc ([("begin", `String ("`"));
                                                   ("endCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.end.shell"))
                                                                ]))
                                                              ]));
                                                   ("end", `String ("`"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("match",
                                                                    `String (
                                                                    "\\\\[`\\\\$]"));
                                                                    ("name",
                                                                    `String (
                                                                    "constant.character.escape.shell"))
                                                                    ]);
                                                             `Assoc (
                                                             [("begin",
                                                               `String (
                                                               "(?<=^|;|&|\\s|`)(#)(?!\\{)"));
                                                               ("end",
                                                                `String (
                                                                "(?=`)|\\n"));
                                                               ("name",
                                                                `String (
                                                                "comment.line.number-sign.shell"));
                                                               ("beginCaptures",
                                                                `Assoc (
                                                                [("1",
                                                                  `Assoc (
                                                                  [("name",
                                                                    `String (
                                                                    "punctuation.definition.comment.shell"))
                                                                    ]))
                                                                  ]))
                                                               ]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "$self"))])
                                                             ]));
                                                   ("name",
                                                    `String ("string.interpolated.backtick.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.begin.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("begin",
                                                   `String ("\\$\\("));
                                                   ("endCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.end.shell"))
                                                                ]))
                                                              ]));
                                                   ("end", `String ("\\)"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("begin",
                                                                    `String (
                                                                    "(?<=^|;|&|\\s|\\()(#)(?!\\{)"));
                                                                    ("end",
                                                                    `String (
                                                                    "(?=\\))|\\n"));
                                                                    ("name",
                                                                    `String (
                                                                    "comment.line.number-sign.shell"));
                                                                    ("beginCaptures",
                                                                    `Assoc (
                                                                    [("1",
                                                                    `Assoc (
                                                                    [("name",
                                                                    `String (
                                                                    "punctuation.definition.comment.shell"))
                                                                    ]))]))]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "$self"))])
                                                             ]));
                                                   ("name",
                                                    `String ("string.interpolated.dollar.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("0",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "punctuation.definition.string.begin.shell"))
                                                                ]))
                                                              ]))
                                                   ])
                                         ]))
                                ]));
                     ("loop",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("begin",
                                                  `String ("(?<=^|;|&|\\s)(for)\\s+(?=\\({2})"));
                                                  ("end",
                                                   `String ("(?<=^|;|&|\\s)(done)(?=\\s|;|&|$|\\))"));
                                                  ("patterns",
                                                   `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                            ]));
                                                  ("name",
                                                   `String ("meta.scope.for-loop.shell"));
                                                  ("captures",
                                                   `Assoc ([("1",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "keyword.control.shell"))
                                                               ]))
                                                             ]))
                                                  ]);
                                         `Assoc ([("begin",
                                                   `String ("(?<=^|;|&|\\s)(for)\\s+([^\\s\\\\]+)(?=\\s|;|&|$)"));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.shell"))
                                                                ]))
                                                              ]));
                                                   ("end",
                                                    `String ("(?<=^|;|&|\\s)(done)(?=\\s|;|&|$|\\))"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]));
                                                   ("name",
                                                    `String ("meta.scope.for-in-loop.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.shell"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "variable.other.loop.shell"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("begin",
                                                   `String ("(?<=^|;|&|\\s)(while|until)(?=\\s|;|&|$)"));
                                                   ("end",
                                                    `String ("(?<=^|;|&|\\s)(done)(?=\\s|;|&|$|\\))"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]));
                                                   ("name",
                                                    `String ("meta.scope.while-loop.shell"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("begin",
                                                   `String ("(?<=^|;|&|\\s)(select)\\s+([^\\s\\\\]+)(?=\\s|;|&|$)"));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.shell"))
                                                                ]))
                                                              ]));
                                                   ("end",
                                                    `String ("(?<=^|;|&|\\s)(done)(?=\\s|;|&|$|\\))"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]));
                                                   ("name",
                                                    `String ("meta.scope.select-block.shell"));
                                                   ("beginCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.shell"))
                                                                ]));
                                                              ("2",
                                                               `Assoc (
                                                               [("name",
                                                                 `String (
                                                                 "variable.other.loop.shell"))
                                                                 ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("begin",
                                                   `String ("(?<=^|;|&|\\s)(case)(?=\\s|;|&|$)"));
                                                   ("end",
                                                    `String ("(?<=^|;|&|\\s)(esac)(?=\\s|;|&|$|\\))"));
                                                   ("patterns",
                                                    `List ([`Assoc ([("begin",
                                                                    `String (
                                                                    "(?<=^|;|&|\\s)(?:in)(?=\\s|;|&|$)"));
                                                                    ("end",
                                                                    `String (
                                                                    "(?<=^|;|&|\\s)(?=(?:esac)(?:\\s|;|&|$|\\)))"));
                                                                    ("patterns",
                                                                    `List (
                                                                    [`Assoc (
                                                                    [("include",
                                                                    `String (
                                                                    "#comment"))
                                                                    ]);
                                                                    `Assoc (
                                                                    [("include",
                                                                    `String (
                                                                    "#case-clause"))
                                                                    ]);
                                                                    `Assoc (
                                                                    [("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])]));
                                                                    ("name",
                                                                    `String (
                                                                    "meta.scope.case-body.shell"));
                                                                    ("beginCaptures",
                                                                    `Assoc (
                                                                    [("1",
                                                                    `Assoc (
                                                                    [("name",
                                                                    `String (
                                                                    "keyword.control.shell"))
                                                                    ]))]))]);
                                                             `Assoc (
                                                             [("include",
                                                               `String (
                                                               "$self"))])
                                                             ]));
                                                   ("name",
                                                    `String ("meta.scope.case-block.shell"));
                                                   ("captures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.shell"))
                                                                ]))
                                                              ]))
                                                   ]);
                                         `Assoc ([("end",
                                                   `String ("(?<=^|;|&|\\s)(fi)(?=\\s|;|&|$|\\))"));
                                                   ("begin",
                                                    `String ("(^|(?<=[&;|]))\\s*(if)(?=\\s|;|&|$)"));
                                                   ("beginCaptures",
                                                    `Assoc ([("2",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.shell"))
                                                                ]))
                                                              ]));
                                                   ("patterns",
                                                    `List ([`Assoc ([("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])
                                                             ]));
                                                   ("comment",
                                                    `String ("Restrict match to avoid matching in lines like `dd if=/dev/sda1 \226\128\166`"));
                                                   ("endCaptures",
                                                    `Assoc ([("1",
                                                              `Assoc (
                                                              [("name",
                                                                `String (
                                                                "keyword.control.shell"))
                                                                ]))
                                                              ]));
                                                   ("name",
                                                    `String ("meta.scope.if-block.shell"))
                                                   ])
                                         ]))
                                ]));
                     ("case-clause",
                      `Assoc ([("patterns",
                                `List ([`Assoc ([("begin",
                                                  `String ("(?=\\S)"));
                                                  ("endCaptures",
                                                   `Assoc ([("0",
                                                             `Assoc (
                                                             [("name",
                                                               `String (
                                                               "punctuation.terminator.case-clause.shell"))
                                                               ]))
                                                             ]));
                                                  ("end", `String (";;"));
                                                  ("patterns",
                                                   `List ([`Assoc ([("begin",
                                                                    `String (
                                                                    "(\\(|(?=\\S))"));
                                                                    ("end",
                                                                    `String (
                                                                    "\\)"));
                                                                    ("patterns",
                                                                    `List (
                                                                    [`Assoc (
                                                                    [("match",
                                                                    `String (
                                                                    "\\|"));
                                                                    ("name",
                                                                    `String (
                                                                    "punctuation.separator.pipe-sign.shell"))
                                                                    ]);
                                                                    `Assoc (
                                                                    [("include",
                                                                    `String (
                                                                    "#string"))
                                                                    ]);
                                                                    `Assoc (
                                                                    [("include",
                                                                    `String (
                                                                    "#variable"))
                                                                    ]);
                                                                    `Assoc (
                                                                    [("include",
                                                                    `String (
                                                                    "#interpolation"))
                                                                    ]);
                                                                    `Assoc (
                                                                    [("include",
                                                                    `String (
                                                                    "#pathname"))
                                                                    ])]));
                                                                    ("name",
                                                                    `String (
                                                                    "meta.scope.case-pattern.shell"));
                                                                    ("captures",
                                                                    `Assoc (
                                                                    [("0",
                                                                    `Assoc (
                                                                    [("name",
                                                                    `String (
                                                                    "punctuation.definition.case-pattern.shell"))
                                                                    ]))]))]);
                                                            `Assoc ([("begin",
                                                                    `String (
                                                                    "(?<=\\))"));
                                                                    ("end",
                                                                    `String (
                                                                    "(?=;;)"));
                                                                    ("patterns",
                                                                    `List (
                                                                    [`Assoc (
                                                                    [("include",
                                                                    `String (
                                                                    "$self"))
                                                                    ])]));
                                                                    ("name",
                                                                    `String (
                                                                    "meta.scope.case-clause-body.shell"))
                                                                    ])
                                                            ]));
                                                  ("name",
                                                   `String ("meta.scope.case-clause.shell"))
                                                  ])
                                         ]))
                                ]))
                     ]));
          ("keyEquivalent", `String ("^~S"));
          ("uuid", `String ("DDEEA3ED-6B1C-11D9-8B10-000D93589AF6"));
          ("patterns",
           `List ([`Assoc ([("include", `String ("#comment"))]);
                    `Assoc ([("include", `String ("#pipeline"))]);
                    `Assoc ([("include", `String ("#list"))]);
                    `Assoc ([("include", `String ("#compound-command"))]);
                    `Assoc ([("include", `String ("#loop"))]);
                    `Assoc ([("include", `String ("#string"))]);
                    `Assoc ([("include", `String ("#function-definition"))]);
                    `Assoc ([("include", `String ("#variable"))]);
                    `Assoc ([("include", `String ("#interpolation"))]);
                    `Assoc ([("include", `String ("#heredoc"))]);
                    `Assoc ([("include", `String ("#herestring"))]);
                    `Assoc ([("include", `String ("#redirection"))]);
                    `Assoc ([("include", `String ("#pathname"))]);
                    `Assoc ([("include", `String ("#keyword"))]);
                    `Assoc ([("include", `String ("#support"))]);
                    `Assoc ([("include", `String ("#lines"))])]));
          ("scopeName", `String ("source.shell"));
          ("name", `String ("Shell Script"))])


let diff =
`Assoc ([("fileTypes",
          `List ([`String ("patch"); `String ("diff"); `String ("rej")]));
          ("firstLineMatch",
           `String ("(?x)^\n\t\t(===\\ modified\\ file\n\t\t|==== \\s* // .+ \\s - \\s .+ \\s+ ====\n\t\t|Index:\\ \n\t\t|---\\ [^%\\n]\n\t\t|\\*\\*\\*.*\\d{4}\\s*$\n\t\t|\\d+(,\\d+)* (a|d|c) \\d+(,\\d+)* $\n\t\t|diff\\ --git\\ \n\t\t|commit\\ [0-9a-f]{40}$\n\t\t)"));
          ("keyEquivalent", `String ("^~D")); ("name", `String ("Diff"));
          ("patterns",
           `List ([`Assoc ([("captures",
                             `Assoc ([("1",
                                       `Assoc ([("name",
                                                 `String ("punctuation.definition.separator.diff"))
                                                 ]))
                                       ]));
                             ("match",
                              `String ("^((\\*{15})|(={67})|(-{3}))$\\n?"));
                             ("name", `String ("meta.separator.diff"))]);
                    `Assoc ([("match",
                              `String ("^\\d+(,\\d+)*(a|d|c)\\d+(,\\d+)*$\\n?"));
                              ("name", `String ("meta.diff.range.normal"))]);
                    `Assoc ([("captures",
                              `Assoc ([("1",
                                        `Assoc ([("name",
                                                  `String ("punctuation.definition.range.diff"))
                                                  ]));
                                        ("2",
                                         `Assoc ([("name",
                                                   `String ("meta.toc-list.line-number.diff"))
                                                   ]));
                                        ("3",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.range.diff"))
                                                   ]))
                                        ]));
                              ("match",
                               `String ("^(@@)\\s*(.+?)\\s*(@@)($\\n?)?"));
                              ("name", `String ("meta.diff.range.unified"))]);
                    `Assoc ([("captures",
                              `Assoc ([("3",
                                        `Assoc ([("name",
                                                  `String ("punctuation.definition.range.diff"))
                                                  ]));
                                        ("4",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.range.diff"))
                                                   ]));
                                        ("6",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.range.diff"))
                                                   ]));
                                        ("7",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.range.diff"))
                                                   ]))
                                        ]));
                              ("match",
                               `String ("^(((\\-{3}) .+ (\\-{4}))|((\\*{3}) .+ (\\*{4})))$\\n?"));
                              ("name", `String ("meta.diff.range.context"))]);
                    `Assoc ([("match", `String ("^diff --git a/.*$\\n?"));
                              ("name", `String ("meta.diff.header.git"))]);
                    `Assoc ([("match",
                              `String ("^diff (-|\\S+\\s+\\S+).*$\\n?"));
                              ("name", `String ("meta.diff.header.command"))]);
                    `Assoc ([("captures",
                              `Assoc ([("4",
                                        `Assoc ([("name",
                                                  `String ("punctuation.definition.from-file.diff"))
                                                  ]));
                                        ("6",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.from-file.diff"))
                                                   ]));
                                        ("7",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.from-file.diff"))
                                                   ]))
                                        ]));
                              ("match",
                               `String ("(^(((-{3}) .+)|((\\*{3}) .+))$\\n?|^(={4}) .+(?= - ))"));
                              ("name", `String ("meta.diff.header.from-file"))
                              ]);
                    `Assoc ([("captures",
                              `Assoc ([("2",
                                        `Assoc ([("name",
                                                  `String ("punctuation.definition.to-file.diff"))
                                                  ]));
                                        ("3",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.to-file.diff"))
                                                   ]));
                                        ("4",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.to-file.diff"))
                                                   ]))
                                        ]));
                              ("match",
                               `String ("(^(\\+{3}) .+$\\n?| (-) .* (={4})$\\n?)"));
                              ("name", `String ("meta.diff.header.to-file"))]);
                    `Assoc ([("captures",
                              `Assoc ([("3",
                                        `Assoc ([("name",
                                                  `String ("punctuation.definition.inserted.diff"))
                                                  ]));
                                        ("6",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.inserted.diff"))
                                                   ]))
                                        ]));
                              ("match",
                               `String ("^(((>)( .*)?)|((\\+).*))$\\n?"));
                              ("name", `String ("markup.inserted.diff"))]);
                    `Assoc ([("captures",
                              `Assoc ([("1",
                                        `Assoc ([("name",
                                                  `String ("punctuation.definition.inserted.diff"))
                                                  ]))
                                        ]));
                              ("match", `String ("^(!).*$\\n?"));
                              ("name", `String ("markup.changed.diff"))]);
                    `Assoc ([("captures",
                              `Assoc ([("3",
                                        `Assoc ([("name",
                                                  `String ("punctuation.definition.deleted.diff"))
                                                  ]));
                                        ("6",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.deleted.diff"))
                                                   ]))
                                        ]));
                              ("match",
                               `String ("^(((<)( .*)?)|((-).*))$\\n?"));
                              ("name", `String ("markup.deleted.diff"))]);
                    `Assoc ([("begin", `String ("^(#)"));
                              ("captures",
                               `Assoc ([("1",
                                         `Assoc ([("name",
                                                   `String ("punctuation.definition.comment.diff"))
                                                   ]))
                                         ]));
                              ("comment",
                               `String ("Git produces unified diffs with embedded comments\""));
                              ("end", `String ("\\n"));
                              ("name",
                               `String ("comment.line.number-sign.diff"))
                              ]);
                    `Assoc ([("match",
                              `String ("^index [0-9a-f]{7,40}\\.\\.[0-9a-f]{7,40}.*$\\n?"));
                              ("name", `String ("meta.diff.index.git"))]);
                    `Assoc ([("captures",
                              `Assoc ([("1",
                                        `Assoc ([("name",
                                                  `String ("punctuation.separator.key-value.diff"))
                                                  ]));
                                        ("2",
                                         `Assoc ([("name",
                                                   `String ("meta.toc-list.file-name.diff"))
                                                   ]))
                                        ]));
                              ("match", `String ("^Index(:) (.+)$\\n?"));
                              ("name", `String ("meta.diff.index"))]);
                    `Assoc ([("match", `String ("^Only in .*: .*$\\n?"));
                              ("name", `String ("meta.diff.only-in"))])
                    ]));
          ("scopeName", `String ("source.diff"));
          ("uuid", `String ("7E848FF4-708E-11D9-97B4-0011242E4184"))])

