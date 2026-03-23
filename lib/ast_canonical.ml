(* The ast after vars subtitutions and expr evaluation *)

type portion = Portion of int * int

type source =
  | SrcAccount of string
  | SrcMax of int * source
  | SrcInorder of source list
  | SrcAllotment of (portion * source) list

type dest =
  | DestAccount of string
  | DestInorder of dest_inorder_clause list * kept_or_dest
  | DestAllotment of (portion * dest) list

and kept_or_dest =
  | Kept
  | Dest of dest

and dest_inorder_clause =
  { cap : int
  ; dest : kept_or_dest
  }

type statement =
  | StatementSend of
      { asset : string
      ; amount : int
      ; source : source
      ; destination : dest
      }
  | StatementSendAll of
      { asset : string
      ; source : source
      ; destination : dest
      }
