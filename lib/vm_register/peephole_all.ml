let list = [ Peephole_const_fold.apply; Peephole_dce.apply ]
let run = list |> Peephole.merge |> Peephole.find_fixed_point |> Peephole.apply
