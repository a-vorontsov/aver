class table =
  object (self)
    val mutable counter = 0

    val tbl = Hashtbl.create 32

    method inc = counter <- counter + 1

    method insert (name : string) : int =
      match Hashtbl.find_opt tbl name with
      | Some _ -> assert false
      | None ->
          let var' = counter in
          Hashtbl.add tbl name var';
          self#inc;
          var'

    method get (name : string) : int = Hashtbl.find tbl name

    method exists (name : string) : bool = Hashtbl.mem tbl name
  end
