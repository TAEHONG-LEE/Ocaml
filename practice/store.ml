type t = (string * value) list
and value = NumV of int | ClosureV of string * Ast.expr * t

let empty = []

let rec find str mem = 
        match mem with
        | [] -> failwith ("[Error] Free identifier: " ^ str)
        | h::t -> begin
                match h with
                | (name, v) -> begin
                        if name = str then
                                v
                        else
                                find str t
                end
        end

let add str v mem =
        let rec find_pos lst str i = match lst with
        | [] -> -1
        | h::t -> begin
                match h with
                | (name, _) -> begin
                        if name = str then
                                i
                        else
                                find_pos t str (i + 1)
                end
        end in
        let pos = find_pos mem str 0 in
        if pos = -1 then
                (str, v)::mem
        else
                List.mapi (fun i x -> if i = pos then (str, v) else x) mem
