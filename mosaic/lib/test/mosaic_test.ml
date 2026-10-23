let run ?config ?now ?process_perform ?quiesce term app body =
  Matrix_test.run ?config ?now ?quiesce term
    (fun s ->
      let process_perform =
        match process_perform with
        | Some executor -> executor
        | None -> Matrix_test.perform s
      in
      Mosaic.run ~config:(Matrix_test.config s) ~process_perform app)
    body
