/* Parse and compile HaMLet SML ASTs to SMALL */
type sourceMap = {
  file: string,
  line1: int,
  col1: int,
  line2: int,
  col2: int,
};

type ast =
  | INTSCon(int)
  | SCONAtExp(sourceMap, ast)
  | ATExp(sourceMap, ast)
  | LongVId(string)
  | IDAtPat(sourceMap, ast)
  | ATPat(sourceMap, ast)
  | PLAINValBind(sourceMap, (ast, ast, option(ast)))
  | Seq(sourceMap)
  | VALDec(sourceMap, (ast, ast))
  | DECStrDec(sourceMap, ast)
  | STRDECTopDec(sourceMap, (ast, option(ast)))
  | Program(sourceMap, (ast, option(ast)))
  | LETAtExp(sourceMap, (ast, ast))
  | IDAtExp(sourceMap, ast)
  | APPExp(sourceMap, (ast, ast))
  | PARAtExp(sourceMap, ast)
  | FNExp(sourceMap, ast)
  | Match(sourceMap, (ast, option(ast)))
  | Mrule(sourceMap, (ast, ast))
  | RECORDAtExp(sourceMap, option(ast))
  | ExpRow(sourceMap, (ast, ast, option(ast)))
  | Lab(string)
  | RECValBind(sourceMap, ast)
  | PARAtPat(sourceMap, ast)
  | RECORDAtPat(sourceMap, option(ast))
  | FIELDPatRow(sourceMap, (ast, ast, option(ast)))
  | DOTSPatRow(sourceMap)
  | WILDCARDAtPat(sourceMap)
  | CONPat(sourceMap, (ast, ast))
  | RAISEExp(sourceMap, ast);

module Decode = {
  open Json.Decode;

  let sourceMap = json => {
    {
      file: json |> field("file", string),
      line1: json |> field("line1", int),
      col1: json |> field("col1", int),
      line2: json |> field("line2", int),
      col2: json |> field("col2", int),
    };
  };

  let rec intscon = json => INTSCon(json |> field("args", list(int)) |> List.hd)

  and scontatexp = json =>
    SCONAtExp(
      json |> field("sourceMap", sourceMap),
      json |> field("args", list(node)) |> List.hd,
    )

  and atexp = json =>
    ATExp(json |> field("sourceMap", sourceMap), json |> field("args", list(node)) |> List.hd)

  and longvid = json => LongVId(json |> field("args", list(string)) |> List.hd)

  and idatpat = json =>
    IDAtPat(
      json |> field("sourceMap", sourceMap),
      json |> field("args", list(node)) |> List.hd,
    )

  and atpat = json =>
    ATPat(json |> field("sourceMap", sourceMap), json |> field("args", list(node)) |> List.hd)

  and plainvalbind = json =>
    PLAINValBind(
      json |> field("sourceMap", sourceMap),
      /* TOOD: assumes last argument is null always */
      json |> field("args", tuple3(node, node, optional(node))),
    )

  and seq = json => Seq(json |> field("sourceMap", sourceMap))

  and valdec = json =>
    VALDec(json |> field("sourceMap", sourceMap), json |> field("args", pair(node, node)))

  and decstrdec = json =>
    DECStrDec(
      json |> field("sourceMap", sourceMap),
      json |> field("args", list(node)) |> List.hd,
    )

  and strdectopdec = json =>
    STRDECTopDec(
      json |> field("sourceMap", sourceMap),
      json |> field("args", pair(node, optional(node))),
    )

  and program = json =>
    Program(
      json |> field("sourceMap", sourceMap),
      json |> field("args", pair(node, optional(node))),
    )

  and letatexp = json =>
    LETAtExp(json |> field("sourceMap", sourceMap), json |> field("args", pair(node, node)))

  and idatexp = json =>
    IDAtExp(
      json |> field("sourceMap", sourceMap),
      json |> field("args", list(node)) |> List.hd,
    )

  and appexp = json =>
    APPExp(json |> field("sourceMap", sourceMap), json |> field("args", pair(node, node)))

  and paratexp = json =>
    PARAtExp(
      json |> field("sourceMap", sourceMap),
      json |> field("args", list(node)) |> List.hd,
    )

  and fnexp = json =>
    FNExp(json |> field("sourceMap", sourceMap), json |> field("args", list(node)) |> List.hd)

  and match = json =>
    Match(
      json |> field("sourceMap", sourceMap),
      json |> field("args", pair(node, optional(node))),
    )

  and mrule = json =>
    Mrule(json |> field("sourceMap", sourceMap), json |> field("args", pair(node, node)))

  and recordatexp = json =>
    RECORDAtExp(
      json |> field("sourceMap", sourceMap),
      json |> field("args", list(optional(node))) |> List.hd,
    )

  and exprow = json =>
    ExpRow(
      json |> field("sourceMap", sourceMap),
      json |> field("args", tuple3(lab, node, optional(node))),
    )

  and lab = json => {
    Lab(json |> field("args", list(string)) |> List.hd);
  }

  and recvalbind = json =>
    RECValBind(
      json |> field("sourceMap", sourceMap),
      json |> field("args", list(node)) |> List.hd,
    )

  and paratpat = json =>
    PARAtPat(
      json |> field("sourceMap", sourceMap),
      json |> field("args", list(node)) |> List.hd,
    )

  and recordatpat = json =>
    RECORDAtPat(
      json |> field("sourceMap", sourceMap),
      json |> field("args", list(optional(node))) |> List.hd,
    )

  and fieldpatrow = json =>
    FIELDPatRow(
      json |> field("sourceMap", sourceMap),
      json |> field("args", tuple3(lab, node, optional(node))),
    )

  and dotspatrow = json => DOTSPatRow(json |> field("sourceMap", sourceMap))

  and wildcardatpat = json => WILDCARDAtPat(json |> field("sourceMap", sourceMap))

  and conpat = json =>
    CONPat(json |> field("sourceMap", sourceMap), json |> field("args", pair(node, node)))

  and raiseexp = json =>
    RAISEExp(
      json |> field("sourceMap", sourceMap),
      json |> field("args", list(node)) |> List.hd,
    )

  and node = json => {
    (
      field("node", string)
      |> andThen(s =>
           switch (s) {
           | "INTSCon" => intscon
           | "SCONAtExp" => scontatexp
           | "ATExp" => atexp
           | "LongVId" => longvid
           | "IDAtPat" => idatpat
           | "ATPat" => atpat
           | "PLAINValBind" => plainvalbind
           | "Seq" => seq
           | "VALDec" => valdec
           | "DECStrDec" => decstrdec
           | "STRDECTopDec" => strdectopdec
           | "Program" => program
           | "LETAtExp" => letatexp
           | "IDAtExp" => idatexp
           | "APPExp" => appexp
           | "PARAtExp" => paratexp
           | "FNExp" => fnexp
           | "Match" => match
           | "Mrule" => mrule
           | "RECORDAtExp" => recordatexp
           | "ExpRow" => exprow
           | "Lab" => lab
           | "RECValBind" => recvalbind
           | "PARAtPat" => paratpat
           | "RECORDAtPat" => recordatpat
           | "FIELDPatRow" => fieldpatrow
           | "DOTSPatRow" => dotspatrow
           | "WILDCARDAtPat" => wildcardatpat
           | "CONPat" => conpat
           | "RAISEExp" => raiseexp
           | _ => failwith("Unknown node type: " ++ s)
           }
         )
    )(
      json,
    );
  };
};

let rec compileProgram = p =>
  switch (p) {
  | Program(_, (td, None)) => SML.PROGRAM(compileTopDec(td), None)
  | Program(_, (td, Some(p))) => SML.PROGRAM(compileTopDec(td), Some(compileProgram(p)))
  }

and compileTopDec = td =>
  switch (td) {
  | STRDECTopDec(_, (sd, None)) => SML.STRDEC(compileStrDec(sd), None)
  | STRDECTopDec(_, (sd, Some(td))) => SML.STRDEC(compileStrDec(sd), Some(compileTopDec(td)))
  }

and compileStrDec = sd =>
  switch (sd) {
  | DECStrDec(_, d) => SML.DEC(compileDec(d))
  }

and compileDec = d =>
  switch (d) {
  | VALDec(_, (_, vb)) => SML.VAL(compileValBind(vb))
  }

and compileValBind = vb =>
  switch (vb) {
  | PLAINValBind(_, (p, e, None)) => SML.PLAIN(compilePat(p), compileExp(e), None)
  | PLAINValBind(_, (p, e, Some(vb))) =>
    SML.PLAIN(compilePat(p), compileExp(e), Some(compileValBind(vb)))
  | RECValBind(_, vb) => SML.REC(compileValBind(vb))
  }

and compilePat = p =>
  switch (p) {
  | ATPat(_, ap) => SML.ATPAT(compileAtPat(ap))
  | CONPat(_, (x, ap)) => SML.CON(compileLongVId(x), compileAtPat(ap))
  }

and compileAtPat = ap =>
  switch (ap) {
  | WILDCARDAtPat(_) => SML.WILDCARD
  | IDAtPat(_, x) => SML.ID(compileLongVId(x))
  | RECORDAtPat(_, None) => SML.RECORD(None)
  | RECORDAtPat(_, Some(pr)) => SML.RECORD(Some(compilePatRow(pr)))
  | PARAtPat(_, p) => SML.PAR(compilePat(p))
  }

and compilePatRow = pr =>
  switch (pr) {
  | DOTSPatRow(_) => SML.DOTS
  | FIELDPatRow(_, (l, p, None)) => SML.FIELD(compileLab(l), compilePat(p), None)
  | FIELDPatRow(_, (l, p, Some(pr))) =>
    SML.FIELD(compileLab(l), compilePat(p), Some(compilePatRow(pr)))
  }

and compileLongVId = x =>
  switch (x) {
  | LongVId(x) => x
  }

and compileMatch = m =>
  switch (m) {
  | Match(_, (mr, None)) => SML.MATCH(compileMRule(mr), None)
  | Match(_, (mr, Some(m))) => SML.MATCH(compileMRule(mr), Some(compileMatch(m)))
  }

and compileMRule = mr =>
  switch (mr) {
  | Mrule(_, (p, e)) => SML.MRULE(compilePat(p), compileExp(e))
  }

and compileExp = e =>
  switch (e) {
  | ATExp(_, a) => SML.ATEXP(compileAtExp(a))
  | APPExp(_, (e, a)) => SML.APP(compileExp(e), compileAtExp(a))
  | RAISEExp(_, e) => SML.RAISE(compileExp(e))
  | FNExp(_, m) => SML.FN(compileMatch(m))
  }

and compileAtExp = a =>
  switch (a) {
  | SCONAtExp(_, sc) => SML.SCON(compileSCon(sc))
  | LETAtExp(_, (d, e)) => SML.LET(compileDec(d), compileExp(e))
  | IDAtExp(_, x) => SML.ID(compileLongVId(x))
  | PARAtExp(_, e) => SML.PAR(compileExp(e))
  | RECORDAtExp(_, None) => SML.RECORD(None)
  | RECORDAtExp(_, Some(er)) => SML.RECORD(Some(compileExpRow(er)))
  }

and compileExpRow = er =>
  switch (er) {
  | ExpRow(_, (l, e, None)) => SML.EXPROW(compileLab(l), compileExp(e), None)
  | ExpRow(_, (l, e, Some(er))) =>
    SML.EXPROW(compileLab(l), compileExp(e), Some(compileExpRow(er)))
  }

and compileLab = l =>
  switch (l) {
  | Lab(l) => l
  }

and compileSCon = sc =>
  switch (sc) {
  | INTSCon(n) => SML.INT(n)
  };