module C = Configurator.V1

let () =
  C.main ~name:"harfbuzz" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = [ "-lharfbuzz" ]; cflags = [] }
      in

      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc -> (
            match C.Pkg_config.query pc ~package:"harfbuzz freetype2" with
            | None -> default
            | Some deps -> deps)
      in

      (* Write flags for dune *)
      C.Flags.write_sexp "c_flags" conf.cflags;
      C.Flags.write_sexp "c_library_flags" conf.libs)
