/* resugar traces */
type hole = unit;

type vid = string;
type lab = string;
type basVal = string;

type sCon =
  | INT(int);

type atExp =
  | SCON(sCon)
  | ID(vid)
  | RECORD(option(expRow))
  | LET(dec, exp)
  | PAR(exp)

and expRow =
  | EXPROW(lab, exp, option(expRow))

and exp =
  | ATEXP(atExp)
  | APP(exp, atExp)
  | RAISE(exp)
  | FN(match)

and match =
  | MATCH(mrule, option(match))

and mrule =
  | MRULE(pat, exp)

and dec =
  /* no tyvar seq */
  | VAL(valBind)

and valBind =
  | PLAIN(pat, exp, option(valBind))
  | REC(valBind)

and atPat =
  | WILDCARD
  | ID(vid) /* TODO: add op */
  | RECORD(option(patRow))
  | PAR(pat)

and patRow =
  | DOTS
  | FIELD(lab, pat, option(patRow))

and pat =
  | ATPAT(atPat)
  | CON(vid, atPat);

type sVal =
  | INT(int);

type idStatus =
  | Var
  | Con
  | Exc;

type record = list((lab, val_))

and recordEnv = list((lab, valEnv))

and tuple = list(val_)

and val_ =
  | SVAL(sVal)
  | BASVAL(basVal)
  | VID(vid)
  | VIDVAL(vid, val_) /* constructor value(?) */
  | RECORD(record)
  | TUPLE(tuple)
  /* TODO: second argument should be an entire env */
  | FCNCLOSURE(match, valEnv, valEnv)

and valEnv = list((vid, (val_, idStatus)));

type strDec =
  | DEC(dec)
  | SEQ(strDec, strDec);

type topDec =
  | STRDEC(strDec, option(topDec));

type program =
  | PROGRAM(topDec, option(program));

type focus =
  | AtExp(atExp)
  | Exp(exp)
  | Val(val_)
  | Dec(dec)
  | ValBind(valBind)
  | StrDec(strDec)
  | TopDec(topDec)
  | ExpRow(expRow)
  | Record(record)
  | Program(program)
  | Match(match, val_)
  | MRule(mrule, val_)
  | Pat(pat, val_)
  | AtPat(atPat, val_)
  | PatRow(patRow, record, recordEnv)
  | FAIL(val_)
  | ValEnv(valEnv)
  | Empty;

type ctxt =
  | LETD(hole, exp)
  | VALBINDE(pat, hole, option(valBind))
  | SEQL(hole, strDec)
  | DECD(hole)
  | APPL(hole, atExp)
  | APPR(val_, hole)
  /* is that a... */
  | RECORDER(hole)
  | EXPROWE(record, lab, hole, option(expRow))
  | PROGRAML(hole, program)
  | MATCHMR(hole, option(match))
  | MRULEP(hole, exp)
  | RECVB(hole)
  | RECORDPR(hole)
  | STRDECSD(hole, option(topDec))
  | FIELDP((lab, hole, option(patRow)), record, recordEnv);

type ctxts = list(ctxt);

type rewrite = {
  focus,
  ctxts,
};

type frame = {
  rewrite,
  env: valEnv,
};

type configuration = list(frame);

let rec resugarSCon = (sc: SML.sCon): sCon =>
  switch (sc) {
  | SML.INT(n) => INT(n)
  }

and resugarAtExp = ae =>
  switch (ae) {
  | SML.SCON(sc) => SCON(resugarSCon(sc))
  | SML.ID(vid) => ID(vid)
  | SML.RECORD(None) => RECORD(None)
  | SML.RECORD(Some(er)) => RECORD(Some(resugarExpRow(er)))
  | SML.LET(d, e) => LET(resugarDec(d), resugarExp(e))
  | SML.PAR(e) => PAR(resugarExp(e))
  }

and resugarExpRow = er =>
  switch (er) {
  | SML.EXPROW(l, e, None) => EXPROW(l, resugarExp(e), None)
  | SML.EXPROW(l, e, Some(er)) => EXPROW(l, resugarExp(e), Some(resugarExpRow(er)))
  }

and resugarExp = e =>
  switch (e) {
  | SML.ATEXP(ae) => ATEXP(resugarAtExp(ae))
  | SML.APP(e, ae) => APP(resugarExp(e), resugarAtExp(ae))
  | SML.RAISE(e) => RAISE(resugarExp(e))
  | SML.FN(m) => FN(resugarMatch(m))
  }

and resugarMatch = m =>
  switch (m) {
  | SML.MATCH(mr, None) => MATCH(resugarMRule(mr), None)
  | SML.MATCH(mr, Some(m)) => MATCH(resugarMRule(mr), Some(resugarMatch(m)))
  }

and resugarMRule = mr =>
  switch (mr) {
  | SML.MRULE(p, e) => MRULE(resugarPat(p), resugarExp(e))
  }

and resugarDec = d =>
  switch (d) {
  | SML.VAL(vb) => VAL(resugarValBind(vb))
  }

and resugarValBind = vb =>
  switch (vb) {
  | SML.PLAIN(p, e, None) => PLAIN(resugarPat(p), resugarExp(e), None)
  | SML.PLAIN(p, e, Some(vb)) =>
    PLAIN(resugarPat(p), resugarExp(e), Some(resugarValBind(vb)))
  | SML.REC(vb) => REC(resugarValBind(vb))
  }

and resugarAtPat = ap =>
  switch (ap) {
  | SML.WILDCARD => WILDCARD
  | SML.ID(vid) => ID(vid)
  | SML.RECORD(None) => RECORD(None)
  | SML.RECORD(Some(pr)) => RECORD(Some(resugarPatRow(pr)))
  | SML.PAR(p) => PAR(resugarPat(p))
  }

and resugarPatRow = pr =>
  switch (pr) {
  | SML.DOTS => DOTS
  | SML.FIELD(l, p, None) => FIELD(l, resugarPat(p), None)
  | SML.FIELD(l, p, Some(pr)) => FIELD(l, resugarPat(p), Some(resugarPatRow(pr)))
  }

and resugarPat = p =>
  switch (p) {
  | SML.ATPAT(ap) => ATPAT(resugarAtPat(ap))
  | SML.CON(vid, ap) => CON(vid, resugarAtPat(ap))
  };

let resugarSVal = sv =>
  switch (sv) {
  | SML.INT(n) => INT(n)
  };

let resugarIdStatus = id =>
  switch (id) {
  | SML.Var => Var
  | SML.Con => Con
  | SML.Exc => Exc
  };

let rec tupleLikeLabels = (i, ls) =>
  switch (ls) {
  | [] => true
  | [h, ...t] when h == string_of_int(i) => tupleLikeLabels(i + 1, t)
  | _ => false
  };

let tupleLike = labels => tupleLikeLabels(1, labels);

let rec resugarRecord = r => r |> List.map(((l, v)) => (l, resugarVal_(v)))

and resugarRecordEnv = re => re |> List.map(((l, ve)) => (l, resugarValEnv(ve)))

and resugarVal_ = v =>
  switch (v) {
  | SML.SVAL(sv) => SVAL(resugarSVal(sv))
  | SML.BASVAL(bv) => BASVAL(bv)
  | SML.VID(vid) => VID(vid)
  | SML.VIDVAL(vid, v) => VIDVAL(vid, resugarVal_(v))
  | SML.RECORD(r) =>
    let resugarR = resugarRecord(r);
    let (labels, fields) = List.split(resugarR);
    if (tupleLike(labels)) {
      TUPLE(fields);
    } else {
      RECORD(resugarR);
    };
  | SML.FCNCLOSURE(m, e, ve) =>
    FCNCLOSURE(resugarMatch(m), resugarValEnv(e), resugarValEnv(ve))
  }

and resugarValEnv = ve =>
  ve |> List.map(((vid, (v, i))) => (vid, (resugarVal_(v), resugarIdStatus(i))));

let rec resugarStrDec = sd =>
  switch (sd) {
  | SML.DEC(d) => DEC(resugarDec(d))
  | SML.SEQ(sd1, sd2) => SEQ(resugarStrDec(sd1), resugarStrDec(sd2))
  };

let rec resugarTopDec = td =>
  switch (td) {
  | SML.STRDEC(sd, None) => STRDEC(resugarStrDec(sd), None)
  | SML.STRDEC(sd, Some(td)) => STRDEC(resugarStrDec(sd), Some(resugarTopDec(td)))
  };

let rec resugarProgram = p =>
  switch (p) {
  | SML.PROGRAM(td, None) => PROGRAM(resugarTopDec(td), None)
  | SML.PROGRAM(td, Some(p)) => PROGRAM(resugarTopDec(td), Some(resugarProgram(p)))
  };

let resugarFocus = f =>
  switch (f) {
  | SML.AtExp(ae) => AtExp(resugarAtExp(ae))
  | SML.Exp(e) => Exp(resugarExp(e))
  | SML.Val(v) => Val(resugarVal_(v))
  | SML.Dec(d) => Dec(resugarDec(d))
  | SML.ValBind(vb) => ValBind(resugarValBind(vb))
  | SML.StrDec(sd) => StrDec(resugarStrDec(sd))
  | SML.TopDec(td) => TopDec(resugarTopDec(td))
  | SML.ExpRow(er) => ExpRow(resugarExpRow(er))
  | SML.Record(r) => Record(resugarRecord(r))
  | SML.Program(p) => Program(resugarProgram(p))
  | SML.Match(m, v) => Match(resugarMatch(m), resugarVal_(v))
  | SML.MRule(mr, v) => MRule(resugarMRule(mr), resugarVal_(v))
  | SML.Pat(p, v) => Pat(resugarPat(p), resugarVal_(v))
  | SML.AtPat(ap, v) => AtPat(resugarAtPat(ap), resugarVal_(v))
  | SML.PatRow(pr, r, re) => PatRow(resugarPatRow(pr), resugarRecord(r), resugarRecordEnv(re))
  | SML.FAIL(v) => FAIL(resugarVal_(v))
  | SML.ValEnv(ve) => ValEnv(resugarValEnv(ve))
  | SML.Empty => Empty
  };

let resugarCtxt = c =>
  switch (c) {
  | SML.LETD((), e) => LETD((), resugarExp(e))
  | SML.VALBINDE(p, (), None) => VALBINDE(resugarPat(p), (), None)
  | SML.VALBINDE(p, (), Some(vb)) => VALBINDE(resugarPat(p), (), Some(resugarValBind(vb)))
  | SML.SEQL((), sd) => SEQL((), resugarStrDec(sd))
  | SML.DECD () => DECD()
  | SML.APPL((), ae) => APPL((), resugarAtExp(ae))
  | SML.APPR(v, ()) => APPR(resugarVal_(v), ())
  | SML.RECORDER () => RECORDER()
  | SML.EXPROWE(r, l, (), None) => EXPROWE(resugarRecord(r), l, (), None)
  | SML.EXPROWE(r, l, (), Some(er)) =>
    EXPROWE(resugarRecord(r), l, (), Some(resugarExpRow(er)))
  | SML.PROGRAML((), p) => PROGRAML((), resugarProgram(p))
  | SML.MATCHMR((), None) => MATCHMR((), None)
  | SML.MATCHMR((), Some(m)) => MATCHMR((), Some(resugarMatch(m)))
  | SML.MRULEP((), e) => MRULEP((), resugarExp(e))
  | SML.RECVB () => RECVB()
  | SML.RECORDPR () => RECORDPR()
  | SML.STRDECSD((), None) => STRDECSD((), None)
  | SML.STRDECSD((), Some(td)) => STRDECSD((), Some(resugarTopDec(td)))
  | SML.FIELDP((l, (), None), r, re) =>
    FIELDP((l, (), None), resugarRecord(r), resugarRecordEnv(re))
  | SML.FIELDP((l, (), Some(pr)), r, re) =>
    FIELDP((l, (), Some(resugarPatRow(pr))), resugarRecord(r), resugarRecordEnv(re))
  };

let resugarCtxts = cs => cs |> List.map(resugarCtxt(_));

let resugarRewrite = (SML.{focus, ctxts}) => {
  focus: resugarFocus(focus),
  ctxts: resugarCtxts(ctxts),
};

let resugarFrame = (SML.{rewrite, env}) => {
  rewrite: resugarRewrite(rewrite),
  env: resugarValEnv(env),
};

let resugarFrames = fs => fs |> List.map(resugarFrame);
let resugarConfiguration = resugarFrames;