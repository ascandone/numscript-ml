(* The ast after vars subtitutions and expr evaluation *)

type portion = Portion of int64 * int64

type source =
  | SrcAccount of string
  | SrcAccountOverdraft of
      { account : string
      ; max_overdraft : (string * int64) option
      }
  | SrcMax of int64 * source
  | SrcInorder of source list
  | SrcAllotment of (portion * source) list

type dest =
  | DestAccount of string
  | DestInorder of dest_inorder_clause list * kept_or_dest
  | DestAllotment of (portion * kept_or_dest) list

and kept_or_dest =
  | Kept
  | Dest of dest

and dest_inorder_clause =
  { cap : int64
  ; dest : kept_or_dest
  }

type statement =
  | StmtSend of
      { asset : string
      ; amount : int64
      ; source : source
      ; destination : dest
      }
  | StmtSendAll of
      { asset : string
      ; source : source
      ; destination : dest
      }
  | Save of
      { asset : string
      ; amount : int64
      ; account : string
      }

type program = { statements : statement list }
