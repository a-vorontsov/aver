class table =
  object (self)
    val mutable counter = 0

    val tbl = Hashtbl.create 32

    method new_var = counter <- counter + 1

    method insert_var (name : string) : int =
      let var = Hashtbl.find_opt tbl name in
      match var with
      | Some var' -> var'
      | None ->
          let var' = counter in
          Hashtbl.add tbl name var';
          self#new_var;
          var'
  end
