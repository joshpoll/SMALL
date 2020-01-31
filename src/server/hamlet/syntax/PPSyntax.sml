(*
 * (c) Andreas Rossberg 2007-2013
 *
 * Auxiliaries for printing syntax
 *)

structure PPSyntax : PP_SYNTAX =
struct
  open TextIO

  fun ppIndent(out, i) = output(out, CharVector.tabulate(2*i, fn _ => #" "))
  fun ppBegin out = output(out, "{")
  fun ppEnd out = output(out, "}")

  fun ppAnnot(out, A) =
      let
        val {file, region = ((line1, col1), (line2, col2))} = Annotation.loc A
      in
        output(out, "\"sourceMap\": { ");
        output(out, "\"file\": ");
        (case file of
          NONE   => ( output(out, "null") )
        | SOME f => ( output(out, "\""); output(out, f); output(out, "\"") ));
        output(out, ", \"line1\": ");
        output(out, Int.toString line1);
        output(out, ", \"col1\": ");
        output(out, Int.toString col1);
        output(out, ", \"line2\": ");
        output(out, Int.toString line2);
        output(out, ", \"col2\": ");
        output(out, Int.toString col2);
        output(out, " },")
      end

  fun ppHead'(out, i, s, A_opt) =
      ( ppIndent(out, i);
        ppBegin out;
        output(out, "\"node\": \"");
        output(out, s);
        output(out, "\",");
        (case A_opt of
          NONE   => ()
        | SOME A => ( output(out, " "); ppAnnot(out, A) ));
        output(out, " \"args\": [")
      )

  fun ppFoot'(out, i, A_opt) = ( output(out, "]"); ppEnd out; output(out, "\n") )

  fun ppHead(out, i, s, A) = ( ppHead'(out, i, s, SOME A); output(out, "\n") )
  fun ppFoot(out, i, A) = ( ppIndent(out, i); ppFoot'(out, i, SOME A) )
  fun ppHeadFoot(out, i, s, A) =
      ( ppHead'(out, i, s, SOME A); ppFoot'(out, i, SOME A) )

  fun ppAtom(out, i, s1, s2) =
      ( ppHead'(out, i, s1, NONE);
        output(out, s2);
        ppFoot'(out, i, NONE) )

  fun ppElem(out, i, s, A, []) =
        ppHeadFoot(out, i, s, A)
    | ppElem(out, i, s, A, [sub]) =
      ( ppHead(out, i, s, A);
        sub(out, i+1);
        ppFoot(out, i, A) )
    | ppElem(out, i, s, A, subs) =
      ( ppHead(out, i, s, A);
        (List.hd subs)(out, i+1);
        List.app (fn pp => (output(out, ","); pp(out, i+1))) (List.tl subs);
        ppFoot(out, i, A) )

  fun ppOpt ppX (out, i, NONE)   = output(out, "null")
    | ppOpt ppX (out, i, SOME x) = ppX(out, i, x)

  fun sub ppX x (out, i) = ppX(out, i, x)
  fun subs ppX xs (out, i) = List.app (fn x => ppX(out, i, x)) xs
  fun subo ppX x_opt (out, i) = ppOpt ppX (out, i, x_opt)
end;
