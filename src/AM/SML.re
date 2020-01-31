/* Hardfork of previous version with a key difference: No tiered environments. Tiered environments
   were introduced for let expressions; however, their role should be subsumed by CoW. Also may
   bring this implementation closer to what would come out of the actual interpreter since each
   frame may literally be a recursive call (though not all recursive calls in the interpreter are
   represented).

   also returns val envs explicitly in many places to more closely match semantics and make
   modularity work better. Consider a list of val bindings. Each one must be evaluated in a new
   frame. That frame should return a val env so it can be added to the basis by the root frame.
   */

/* OBSERVATION: Labels take precedence over no labels in children. This is because the presence of a
   label signals a change whereas the absence of a label indicates no change. */

/* NOTE: annotating ENTIRE subexpressions! This could be "fixed" by making a shallow annotator, but
   I'm not sure it actually requires fixing yet, because the annotation still seems to be correct anyway. */

/* TODO: need a lot more annotations to capture deletion properly. really these annotations should
   be automatically derived from pattern matching! */
/* TODO: need multiple LHS annotations to capture lookups properly, but also not sure how to
   visualize them. */

/* Following SML 97 and HaMLet closely.
 * Big difference: closure representation uses sharing like Sorin's OCaml visualizations.
 *   Should hopefully also enable simpler recursive function representation? Maybe?
 *
 * ignoring id status, op prefix, vid is string
 *
 * for now E = VE (obviously not sustainable)
 * for now no state
 */
/* HACK: unfolding all recursive functions to small-step AM. definitely has to be a better way to do
   this!*/
/* TODO: how to represent derived forms??? */
/* TODO: how to make variable lookup more granular?? */
/* TODO: separate initial basis from rest of the environment so it's easier to hide.
     may be possible to achieve by ALWAYS creating a new frame at the beginning/set up rules so this
     happens.
   */
/* TODO: desugaring toggling per sugar type (e.g. buttons for ite, orelse, andalso) */

/* TODO: figure out how to do monads in reason/ocaml. there's some ppx stuff. */

/* TODO: highlight code blocks, too? Might be useful for nested let expressions. */
type hole = unit;

/* TODO: these should be annotated too */
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

and val_ =
  | SVAL(sVal)
  | BASVAL(basVal)
  | VID(vid)
  | VIDVAL(vid, val_) /* constructor value(?) */
  | RECORD(record)
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

/* annotated state */
type sConAnno('a) =
  | INT_A(int, 'a);

type atExpAnno('a) =
  | SCON_A(sConAnno('a), 'a)
  | ID_A(vid, 'a)
  | RECORD_A(option(expRowAnno('a)), 'a)
  | LET_A(decAnno('a), expAnno('a), 'a)
  | PAR_A(expAnno('a), 'a)

and expRowAnno('a) =
  | EXPROW_A(lab, expAnno('a), option(expRowAnno('a)), 'a)

and expAnno('a) =
  | ATEXP_A(atExpAnno('a), 'a)
  | APP_A(expAnno('a), atExpAnno('a), 'a)
  | RAISE_A(expAnno('a), 'a)
  | FN_A(matchAnno('a), 'a)

and matchAnno('a) =
  | MATCH_A(mruleAnno('a), option(matchAnno('a)), 'a)

and mruleAnno('a) =
  | MRULE_A(patAnno('a), expAnno('a), 'a)

and decAnno('a) =
  /* no tyvar seq */
  | VAL_A(valBindAnno('a), 'a)

and valBindAnno('a) =
  | PLAIN_A(patAnno('a), expAnno('a), option(valBindAnno('a)), 'a)
  | REC_A(valBindAnno('a), 'a)

and atPatAnno('a) =
  | WILDCARD_A('a)
  | ID_A(vid, 'a) /* TODO: add op */
  | RECORD_A(option(patRowAnno('a)), 'a)
  | PAR_A(patAnno('a), 'a)

and patRowAnno('a) =
  | DOTS_A('a)
  | FIELD_A(lab, patAnno('a), option(patRowAnno('a)), 'a)

and patAnno('a) =
  | ATPAT_A(atPatAnno('a), 'a)
  | CON_A(vid, atPatAnno('a), 'a);

type sValAnno('a) =
  | INT_A(int, 'a);

type idStatusAnno('a) =
  | Var_A('a)
  | Con_A('a)
  | Exc_A('a);

type recordAnno('a) = list((lab, val_Anno('a)))

and recordEnvAnno('a) = list((lab, valEnvAnno('a)))

and val_Anno('a) =
  | SVAL_A(sValAnno('a), 'a)
  | BASVAL_A(basVal, 'a)
  | VID_A(vid, 'a)
  | VIDVAL_A(vid, val_Anno('a), 'a) /* constructor value(?) */
  | RECORD_A(recordAnno('a), 'a)
  /* TODO: second argument should be an entire env */
  | FCNCLOSURE_A(matchAnno('a), valEnvAnno('a), valEnvAnno('a), 'a)

and valEnvAnno('a) = list((vid, (val_Anno('a), idStatusAnno('a))));

type strDecAnno('a) =
  | DEC_A(decAnno('a), 'a)
  | SEQ_A(strDecAnno('a), strDecAnno('a), 'a);

type topDecAnno('a) =
  | STRDEC_A(strDecAnno('a), option(topDecAnno('a)), 'a);

type programAnno('a) =
  | PROGRAM_A(topDecAnno('a), option(programAnno('a)), 'a);

type focusAnno('a) =
  | AtExp_A(atExpAnno('a))
  | Exp_A(expAnno('a))
  | Val_A(val_Anno('a))
  | Dec_A(decAnno('a))
  | ValBind_A(valBindAnno('a))
  | StrDec_A(strDecAnno('a))
  | TopDec_A(topDecAnno('a))
  | ExpRow_A(expRowAnno('a))
  | Record_A(recordAnno('a))
  | Program_A(programAnno('a))
  | Match_A(matchAnno('a), val_Anno('a))
  | MRule_A(mruleAnno('a), val_Anno('a))
  | Pat_A(patAnno('a), val_Anno('a))
  | AtPat_A(atPatAnno('a), val_Anno('a))
  | PatRow_A(patRowAnno('a), recordAnno('a), recordEnvAnno('a))
  | FAIL_A(val_Anno('a))
  | ValEnv_A(valEnvAnno('a))
  | Empty_A;

type ctxtAnno('a) =
  | LETD_A(hole, expAnno('a), 'a)
  | VALBINDE_A(patAnno('a), hole, option(valBindAnno('a)), 'a)
  | SEQL_A(hole, strDecAnno('a), 'a)
  | DECD_A(hole, 'a)
  | APPL_A(hole, atExpAnno('a), 'a)
  | APPR_A(val_Anno('a), hole, 'a)
  /* is that a... */
  | RECORDER_A(hole, 'a)
  | EXPROWE_A(recordAnno('a), lab, hole, option(expRowAnno('a)), 'a)
  | PROGRAML_A(hole, programAnno('a), 'a)
  | MATCHMR_A(hole, option(matchAnno('a)), 'a)
  | MRULEP_A(hole, expAnno('a), 'a)
  | RECVB_A(hole, 'a)
  | RECORDPR_A(hole, 'a)
  | STRDECSD_A(hole, option(topDecAnno('a)), 'a)
  | FIELDP_A((lab, hole, option(patRowAnno('a))), recordAnno('a), recordEnvAnno('a), 'a);

type ctxtsAnno('a) = list(ctxtAnno('a));

type rewriteAnno('a) = {
  focusAnno: focusAnno('a),
  ctxtsAnno: ctxtsAnno('a),
};

type frameAnno('a) = {
  rewriteAnno: rewriteAnno('a),
  envAnno: valEnvAnno('a),
};

type configurationAnno('a) = list(frameAnno('a));

/* TODO: write default annotation function and annotation stripper */
let rec annotateSCon = (sc: sCon, a): sConAnno('a) =>
  switch (sc) {
  | INT(n) => INT_A(n, a)
  }

and annotateAtExp = (ae, a) =>
  switch (ae) {
  | SCON(sc) => SCON_A(annotateSCon(sc, a), a)
  | ID(vid) => ID_A(vid, a)
  | RECORD(None) => RECORD_A(None, a)
  | RECORD(Some(er)) => RECORD_A(Some(annotateExpRow(er, a)), a)
  | LET(d, e) => LET_A(annotateDec(d, a), annotateExp(e, a), a)
  | PAR(e) => PAR_A(annotateExp(e, a), a)
  }

and annotateExpRow = (er, a) =>
  switch (er) {
  | EXPROW(l, e, None) => EXPROW_A(l, annotateExp(e, a), None, a)
  | EXPROW(l, e, Some(er)) => EXPROW_A(l, annotateExp(e, a), Some(annotateExpRow(er, a)), a)
  }

and annotateExp = (e, a) =>
  switch (e) {
  | ATEXP(ae) => ATEXP_A(annotateAtExp(ae, a), a)
  | APP(e, ae) => APP_A(annotateExp(e, a), annotateAtExp(ae, a), a)
  | RAISE(e) => RAISE_A(annotateExp(e, a), a)
  | FN(m) => FN_A(annotateMatch(m, a), a)
  }

and annotateMatch = (m, a) =>
  switch (m) {
  | MATCH(mr, None) => MATCH_A(annotateMRule(mr, a), None, a)
  | MATCH(mr, Some(m)) => MATCH_A(annotateMRule(mr, a), Some(annotateMatch(m, a)), a)
  }

and annotateMRule = (mr, a) =>
  switch (mr) {
  | MRULE(p, e) => MRULE_A(annotatePat(p, a), annotateExp(e, a), a)
  }

and annotateDec = (d, a) =>
  switch (d) {
  | VAL(vb) => VAL_A(annotateValBind(vb, a), a)
  }

and annotateValBind = (vb, a) =>
  switch (vb) {
  | PLAIN(p, e, None) => PLAIN_A(annotatePat(p, a), annotateExp(e, a), None, a)
  | PLAIN(p, e, Some(vb)) =>
    PLAIN_A(annotatePat(p, a), annotateExp(e, a), Some(annotateValBind(vb, a)), a)
  | REC(vb) => REC_A(annotateValBind(vb, a), a)
  }

and annotateAtPat = (ap, a) =>
  switch (ap) {
  | WILDCARD => WILDCARD_A(a)
  | ID(vid) => ID_A(vid, a)
  | RECORD(None) => RECORD_A(None, a)
  | RECORD(Some(pr)) => RECORD_A(Some(annotatePatRow(pr, a)), a)
  | PAR(p) => PAR_A(annotatePat(p, a), a)
  }

and annotatePatRow = (pr, a) =>
  switch (pr) {
  | DOTS => DOTS_A(a)
  | FIELD(l, p, None) => FIELD_A(l, annotatePat(p, a), None, a)
  | FIELD(l, p, Some(pr)) => FIELD_A(l, annotatePat(p, a), Some(annotatePatRow(pr, a)), a)
  }

and annotatePat = (p, a) =>
  switch (p) {
  | ATPAT(ap) => ATPAT_A(annotateAtPat(ap, a), a)
  | CON(vid, ap) => CON_A(vid, annotateAtPat(ap, a), a)
  };

let annotateSVal = (sv, a) =>
  switch (sv) {
  | INT(n) => INT_A(n, a)
  };

let annotateIdStatus = (id, a) =>
  switch (id) {
  | Var => Var_A(a)
  | Con => Con_A(a)
  | Exc => Exc_A(a)
  };

let rec annotateRecord = (r, a) => r |> List.map(((l, v)) => (l, annotateVal_(v, a)))

and annotateRecordEnv = (re, a) => re |> List.map(((l, ve)) => (l, annotateValEnv(ve, a)))

and annotateVal_ = (v, a) =>
  switch (v) {
  | SVAL(sv) => SVAL_A(annotateSVal(sv, a), a)
  | BASVAL(bv) => BASVAL_A(bv, a)
  | VID(vid) => VID_A(vid, a)
  | VIDVAL(vid, v) => VIDVAL_A(vid, annotateVal_(v, a), a)
  | RECORD(r) => RECORD_A(annotateRecord(r, a), a)
  | FCNCLOSURE(m, e, ve) =>
    FCNCLOSURE_A(annotateMatch(m, a), annotateValEnv(e, a), annotateValEnv(ve, a), a)
  }

and annotateValEnv = (ve, a) =>
  ve |> List.map(((vid, (v, i))) => (vid, (annotateVal_(v, a), annotateIdStatus(i, a))));

let rec annotateStrDec = (sd, a) =>
  switch (sd) {
  | DEC(d) => DEC_A(annotateDec(d, a), a)
  | SEQ(sd1, sd2) => SEQ_A(annotateStrDec(sd1, a), annotateStrDec(sd2, a), a)
  };

let rec annotateTopDec = (td, a) =>
  switch (td) {
  | STRDEC(sd, None) => STRDEC_A(annotateStrDec(sd, a), None, a)
  | STRDEC(sd, Some(td)) => STRDEC_A(annotateStrDec(sd, a), Some(annotateTopDec(td, a)), a)
  };

let rec annotateProgram = (p, a) =>
  switch (p) {
  | PROGRAM(td, None) => PROGRAM_A(annotateTopDec(td, a), None, a)
  | PROGRAM(td, Some(p)) => PROGRAM_A(annotateTopDec(td, a), Some(annotateProgram(p, a)), a)
  };

let annotateFocus = (f, a) =>
  switch (f) {
  | AtExp(ae) => AtExp_A(annotateAtExp(ae, a))
  | Exp(e) => Exp_A(annotateExp(e, a))
  | Val(v) => Val_A(annotateVal_(v, a))
  | Dec(d) => Dec_A(annotateDec(d, a))
  | ValBind(vb) => ValBind_A(annotateValBind(vb, a))
  | StrDec(sd) => StrDec_A(annotateStrDec(sd, a))
  | TopDec(td) => TopDec_A(annotateTopDec(td, a))
  | ExpRow(er) => ExpRow_A(annotateExpRow(er, a))
  | Record(r) => Record_A(annotateRecord(r, a))
  | Program(p) => Program_A(annotateProgram(p, a))
  | Match(m, v) => Match_A(annotateMatch(m, a), annotateVal_(v, a))
  | MRule(mr, v) => MRule_A(annotateMRule(mr, a), annotateVal_(v, a))
  | Pat(p, v) => Pat_A(annotatePat(p, a), annotateVal_(v, a))
  | AtPat(ap, v) => AtPat_A(annotateAtPat(ap, a), annotateVal_(v, a))
  | PatRow(pr, r, re) =>
    PatRow_A(annotatePatRow(pr, a), annotateRecord(r, a), annotateRecordEnv(re, a))
  | FAIL(v) => FAIL_A(annotateVal_(v, a))
  | ValEnv(ve) => ValEnv_A(annotateValEnv(ve, a))
  | Empty => Empty_A
  };

let annotateCtxt = (c, a) =>
  switch (c) {
  | LETD((), e) => LETD_A((), annotateExp(e, a), a)
  | VALBINDE(p, (), None) => VALBINDE_A(annotatePat(p, a), (), None, a)
  | VALBINDE(p, (), Some(vb)) =>
    VALBINDE_A(annotatePat(p, a), (), Some(annotateValBind(vb, a)), a)
  | SEQL((), sd) => SEQL_A((), annotateStrDec(sd, a), a)
  | DECD () => DECD_A((), a)
  | APPL((), ae) => APPL_A((), annotateAtExp(ae, a), a)
  | APPR(v, ()) => APPR_A(annotateVal_(v, a), (), a)
  | RECORDER () => RECORDER_A((), a)
  | EXPROWE(r, l, (), None) => EXPROWE_A(annotateRecord(r, a), l, (), None, a)
  | EXPROWE(r, l, (), Some(er)) =>
    EXPROWE_A(annotateRecord(r, a), l, (), Some(annotateExpRow(er, a)), a)
  | PROGRAML((), p) => PROGRAML_A((), annotateProgram(p, a), a)
  | MATCHMR((), None) => MATCHMR_A((), None, a)
  | MATCHMR((), Some(m)) => MATCHMR_A((), Some(annotateMatch(m, a)), a)
  | MRULEP((), e) => MRULEP_A((), annotateExp(e, a), a)
  | RECVB () => RECVB_A((), a)
  | RECORDPR () => RECORDPR_A((), a)
  | STRDECSD((), None) => STRDECSD_A((), None, a)
  | STRDECSD((), Some(td)) => STRDECSD_A((), Some(annotateTopDec(td, a)), a)
  | FIELDP((l, (), None), r, re) =>
    FIELDP_A((l, (), None), annotateRecord(r, a), annotateRecordEnv(re, a), a)
  | FIELDP((l, (), Some(pr)), r, re) =>
    FIELDP_A(
      (l, (), Some(annotatePatRow(pr, a))),
      annotateRecord(r, a),
      annotateRecordEnv(re, a),
      a,
    )
  };

let annotateCtxts = (cs, a) => cs |> List.map(annotateCtxt(_, a));

let annotateRewrite = ({focus, ctxts}, a) => {
  focusAnno: annotateFocus(focus, a),
  ctxtsAnno: annotateCtxts(ctxts, a),
};

let annotateFrame = ({rewrite, env}, a) => {
  rewriteAnno: annotateRewrite(rewrite, a),
  envAnno: annotateValEnv(env, a),
};

let annotateFrames = (fs, a) => fs |> List.map(annotateFrame(_, a));
let annotateConfiguration = annotateFrames;

let stripSCon = (sc: sConAnno('a)): sCon =>
  switch (sc) {
  | INT_A(n, _) => INT(n)
  };

let rec stripAtExp = e =>
  switch (e) {
  | SCON_A(sc_a, _) => SCON(stripSCon(sc_a))
  | ID_A(vid, _) => ID(vid)
  | RECORD_A(None, _) => RECORD(None)
  | RECORD_A(Some(er_a), _) => RECORD(Some(stripExpRow(er_a)))
  | LET_A(d_a, e_a, _) => LET(stripDec(d_a), stripExp(e_a))
  | PAR_A(e_a, _) => PAR(stripExp(e_a))
  }

and stripExpRow = er =>
  switch (er) {
  | EXPROW_A(l, e_a, None, _) => EXPROW(l, stripExp(e_a), None)
  | EXPROW_A(l, e_a, Some(er_a), _) => EXPROW(l, stripExp(e_a), Some(stripExpRow(er_a)))
  }

and stripExp = e =>
  switch (e) {
  | ATEXP_A(ae_a, _) => ATEXP(stripAtExp(ae_a))
  | APP_A(e_a, ae_a, _) => APP(stripExp(e_a), stripAtExp(ae_a))
  | RAISE_A(e_a, _) => RAISE(stripExp(e_a))
  | FN_A(m_a, _) => FN(stripMatch(m_a))
  }

and stripMatch = m =>
  switch (m) {
  | MATCH_A(mr_a, None, _) => MATCH(stripMRule(mr_a), None)
  | MATCH_A(mr_a, Some(m_a), _) => MATCH(stripMRule(mr_a), Some(stripMatch(m_a)))
  }

and stripMRule = mr =>
  switch (mr) {
  | MRULE_A(p_a, e_a, _) => MRULE(stripPat(p_a), stripExp(e_a))
  }

and stripDec = d =>
  switch (d) {
  | VAL_A(vb_a, _) => VAL(stripValBind(vb_a))
  }

and stripValBind = vb =>
  switch (vb) {
  | PLAIN_A(p_a, e_a, None, _) => PLAIN(stripPat(p_a), stripExp(e_a), None)
  | PLAIN_A(p_a, e_a, Some(vb_a), _) =>
    PLAIN(stripPat(p_a), stripExp(e_a), Some(stripValBind(vb_a)))
  | REC_A(vb_a, _) => REC(stripValBind(vb_a))
  }

and stripAtPat = ap =>
  switch (ap) {
  | WILDCARD_A(_) => WILDCARD
  | ID_A(vid, _) => ID(vid)
  | RECORD_A(None, _) => RECORD(None)
  | RECORD_A(Some(pr_a), _) => RECORD(Some(stripPatRow(pr_a)))
  | PAR_A(p_a, _) => PAR(stripPat(p_a))
  }

and stripPatRow = pr =>
  switch (pr) {
  | DOTS_A(_) => DOTS
  | FIELD_A(l, p_a, None, _) => FIELD(l, stripPat(p_a), None)
  | FIELD_A(l, p_a, Some(pr_a), _) => FIELD(l, stripPat(p_a), Some(stripPatRow(pr_a)))
  }

and stripPat = p =>
  switch (p) {
  | ATPAT_A(ap_a, _) => ATPAT(stripAtPat(ap_a))
  | CON_A(vid, ap_a, _) => CON(vid, stripAtPat(ap_a))
  };

let stripSVal = sv =>
  switch (sv) {
  | INT_A(n, _) => INT(n)
  };

let stripIdStatus = is =>
  switch (is) {
  | Var_A(_) => Var
  | Con_A(_) => Con
  | Exc_A(_) => Exc
  };

let rec stripRecord = r => r |> List.map(((l, v_a)) => (l, stripVal_(v_a)))

and stripRecordEnv = re => re |> List.map(((l, ve_a)) => (l, stripValEnv(ve_a)))

and stripVal_ = v =>
  switch (v) {
  | SVAL_A(sv_a, _) => SVAL(stripSVal(sv_a))
  | BASVAL_A(bv, _) => BASVAL(bv)
  | VID_A(vid, _) => VID(vid)
  | VIDVAL_A(vid, v_a, _) => VIDVAL(vid, stripVal_(v_a))
  | RECORD_A(r_a, _) => RECORD(stripRecord(r_a))
  | FCNCLOSURE_A(m_a, e_a, ve_a, _) =>
    FCNCLOSURE(stripMatch(m_a), stripValEnv(e_a), stripValEnv(ve_a))
  }

and stripValEnv = ve =>
  ve |> List.map(((vid, (v_a, is_a))) => (vid, (stripVal_(v_a), stripIdStatus(is_a))));

let rec stripStrDec = sd =>
  switch (sd) {
  | DEC_A(d_a, _) => DEC(stripDec(d_a))
  | SEQ_A(sd1_a, sd2_a, _) => SEQ(stripStrDec(sd1_a), stripStrDec(sd2_a))
  };

let rec stripTopDec = td =>
  switch (td) {
  | STRDEC_A(sd_a, None, _) => STRDEC(stripStrDec(sd_a), None)
  | STRDEC_A(sd_a, Some(td_a), _) => STRDEC(stripStrDec(sd_a), Some(stripTopDec(td_a)))
  };

let rec stripProgram = p =>
  switch (p) {
  | PROGRAM_A(td_a, None, _) => PROGRAM(stripTopDec(td_a), None)
  | PROGRAM_A(td_a, Some(p_a), _) => PROGRAM(stripTopDec(td_a), Some(stripProgram(p_a)))
  };

let stripFocus = f =>
  switch (f) {
  | AtExp_A(ae_a) => AtExp(stripAtExp(ae_a))
  | Exp_A(e_a) => Exp(stripExp(e_a))
  | Val_A(v_a) => Val(stripVal_(v_a))
  | Dec_A(d_a) => Dec(stripDec(d_a))
  | ValBind_A(vb_a) => ValBind(stripValBind(vb_a))
  | StrDec_A(sd_a) => StrDec(stripStrDec(sd_a))
  | TopDec_A(td_a) => TopDec(stripTopDec(td_a))
  | ExpRow_A(er_a) => ExpRow(stripExpRow(er_a))
  | Record_A(r_a) => Record(stripRecord(r_a))
  | Program_A(p_a) => Program(stripProgram(p_a))
  | Match_A(m_a, v_a) => Match(stripMatch(m_a), stripVal_(v_a))
  | MRule_A(mr_a, v_a) => MRule(stripMRule(mr_a), stripVal_(v_a))
  | Pat_A(p_a, v_a) => Pat(stripPat(p_a), stripVal_(v_a))
  | AtPat_A(ap_a, v_a) => AtPat(stripAtPat(ap_a), stripVal_(v_a))
  | PatRow_A(pr_a, r_a, re_a) =>
    PatRow(stripPatRow(pr_a), stripRecord(r_a), stripRecordEnv(re_a))
  | FAIL_A(v_a) => FAIL(stripVal_(v_a))
  | ValEnv_A(ve_a) => ValEnv(stripValEnv(ve_a))
  | Empty_A => Empty
  };

let stripCtxt = c =>
  switch (c) {
  | LETD_A((), e_a, _) => LETD((), stripExp(e_a))
  | VALBINDE_A(p_a, (), None, _) => VALBINDE(stripPat(p_a), (), None)
  | VALBINDE_A(p_a, (), Some(vb_a), _) =>
    VALBINDE(stripPat(p_a), (), Some(stripValBind(vb_a)))
  | SEQL_A((), sd_a, _) => SEQL((), stripStrDec(sd_a))
  | DECD_A((), _) => DECD()
  | APPL_A((), ae_a, _) => APPL((), stripAtExp(ae_a))
  | APPR_A(v, (), _) => APPR(stripVal_(v), ())
  | RECORDER_A((), _) => RECORDER()
  | EXPROWE_A(r_a, l, (), None, _) => EXPROWE(stripRecord(r_a), l, (), None)
  | EXPROWE_A(r_a, l, (), Some(er_a), _) =>
    EXPROWE(stripRecord(r_a), l, (), Some(stripExpRow(er_a)))
  | PROGRAML_A((), p_a, _) => PROGRAML((), stripProgram(p_a))
  | MATCHMR_A((), None, _) => MATCHMR((), None)
  | MATCHMR_A((), Some(m_a), _) => MATCHMR((), Some(stripMatch(m_a)))
  | MRULEP_A((), e_a, _) => MRULEP((), stripExp(e_a))
  | RECVB_A((), _) => RECVB()
  | RECORDPR_A((), _) => RECORDPR()
  | STRDECSD_A((), None, _) => STRDECSD((), None)
  | STRDECSD_A((), Some(td_a), _) => STRDECSD((), Some(stripTopDec(td_a)))
  | FIELDP_A((l, (), None), r_a, re_a, _) =>
    FIELDP((l, (), None), stripRecord(r_a), stripRecordEnv(re_a))
  | FIELDP_A((l, (), Some(pr_a)), r_a, re_a, _) =>
    FIELDP((l, (), Some(stripPatRow(pr_a))), stripRecord(r_a), stripRecordEnv(re_a))
  };

let stripCtxts = List.map(stripCtxt);

let stripRewrite = ({focusAnno, ctxtsAnno}) => {
  focus: stripFocus(focusAnno),
  ctxts: stripCtxts(ctxtsAnno),
};

let stripFrame = ({rewriteAnno, envAnno}) => {
  rewrite: stripRewrite(rewriteAnno),
  env: stripValEnv(envAnno),
};

let stripFrames = List.map(stripFrame);
let stripConfiguration = stripFrames;

let apply = (f, v) =>
  switch (f, v) {
  | (
      "=",
      RECORD_A(
        [("1", SVAL_A(INT_A(a, anno0), _)), ("2", SVAL_A(INT_A(b, anno1), _))],
        anno2,
      ),
    ) =>
    if (a == b) {
      VID_A("true", anno0 @ anno1 @ anno2);
    } else {
      VID_A("false", anno0 @ anno1 @ anno2);
    }
  | (
      "+",
      RECORD_A(
        [("1", SVAL_A(INT_A(a, anno0), _)), ("2", SVAL_A(INT_A(b, anno1), _))],
        anno2,
      ),
    ) =>
    SVAL_A(INT_A(a + b, anno0 @ anno1 @ anno2), [])
  | (
      "-",
      RECORD_A(
        [("1", SVAL_A(INT_A(a, anno0), _)), ("2", SVAL_A(INT_A(b, anno1), _))],
        anno2,
      ),
    ) =>
    SVAL_A(INT_A(a - b, anno0 @ anno1 @ anno2), [])
  | (
      "*",
      RECORD_A(
        [("1", SVAL_A(INT_A(a, anno0), _)), ("2", SVAL_A(INT_A(b, anno1), _))],
        anno2,
      ),
    ) =>
    SVAL_A(INT_A(a * b, anno0 @ anno1 @ anno2), [])
  | (
      "<",
      RECORD_A(
        [("1", SVAL_A(INT_A(a, anno0), _)), ("2", SVAL_A(INT_A(b, anno1), _))],
        anno2,
      ),
    ) =>
    if (a < b) {
      VID_A("true", anno0 @ anno1 @ anno2);
    } else {
      VID_A("false", anno0 @ anno1 @ anno2);
    }
  | _ => failwith("unknown built-in function: " ++ f)
  };

let recEnv = ve =>
  List.map(
    fun
    | (x, (FCNCLOSURE_A(m, e, _, anno0), Var_A(anno1))) => (
        x,
        (FCNCLOSURE_A(m, e, ve, anno0), Var_A(anno1)),
      )
    | xv => xv,
    ve,
  );

let uid = ref(0);

let genFresh = () => {
  uid := uid^ + 1;
  uid^;
};

type label = int;

type transition = {
  lhs: configurationAnno(list(label)),
  rhs: configurationAnno(list(label)),
};

let step = (c: configuration): option(transition) =>
  switch (c) {
  /* frame pop */
  // TODO: maybe this one should check for an empty focus and push the env?
  // | [
  //     {rewrite: {focus: ValEnv(ve), ctxts: []}, env: _},
  //     {rewrite: {focus: Empty, ctxts}, env},
  //     ...frames,
  //   ] =>
  //   Js.log("frame pop ValEnv");
  //   Some([{
  //           rewrite: {
  //             focus: ValEnv(ve),
  //             ctxts,
  //           },
  //           env,
  //         }, ...frames]);

  /* TODO: can't tag the deleted frame!!!! */
  | [
      {rewrite: {focus: Val(v), ctxts: []}, env: oldEnv},
      {rewrite: {focus: Empty, ctxts}, env},
      ...frames,
    ] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Js.log("frame pop Val");
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(annotateVal_(v, [anno1])),
            ctxtsAnno: [],
          },
          envAnno: annotateValEnv(oldEnv, [anno0]),
        },
        {
          rewriteAnno: {
            focusAnno: Empty_A,
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(annotateVal_(v, [anno1])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // | [
  //     {rewrite: {focus: FAIL(v), ctxts: []}, env: _},
  //     {rewrite: {focus: Empty, ctxts}, env},
  //     ...frames,
  //   ] =>
  //   Js.log("frame pop FAIL");
  //   Some([{
  //           rewrite: {
  //             focus: FAIL(v),
  //             ctxts,
  //           },
  //           env,
  //         }, ...frames]);

  /* Atomic Expressions */
  // [90]
  | [{rewrite: {focus: AtExp(SCON(INT(n))), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(SCON_A(INT_A(n, [anno0]), [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(SVAL_A(INT_A(n, [anno0]), [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // [91]
  /* TODO: doesn't properly annotate things!!! */
  | [{rewrite: {focus: AtExp(ID(x)), ctxts}, env}, ...frames] =>
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    switch (Util.lookupOne(x, env)) {
    | None => None
    | Some((v, _)) =>
      Some({
        lhs: [
          {
            rewriteAnno: {
              focusAnno: AtExp_A(ID_A(x, [])),
              ctxtsAnno,
            },
            envAnno,
          },
          ...framesAnno,
        ],
        rhs: [
          {
            rewriteAnno: {
              focusAnno: Val_A(annotateVal_(v, [])),
              ctxtsAnno,
            },
            envAnno,
          },
          ...framesAnno,
        ],
      })
    };

  // [92]
  /* empty record */
  | [{rewrite: {focus: AtExp(RECORD(None)), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(RECORD_A(None, [anno0])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(RECORD_A([], [anno0])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  /* start non-empty record */
  | [{rewrite: {focus: AtExp(RECORD(Some(er))), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(RECORD_A(Some(annotateExpRow(er, [anno0])), [anno1])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: ExpRow_A(annotateExpRow(er, [anno0])),
            ctxtsAnno: [RECORDER_A((), [anno1]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  /* complete non-empty record */
  | [{rewrite: {focus: Record(r), ctxts: [RECORDER (), ...ctxts]}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Record_A(annotateRecord(r, [anno0])),
            ctxtsAnno: [RECORDER_A((), [anno1]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(RECORD_A(annotateRecord(r, [anno0]), [anno1])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // [93]
  /* begin let traversal */
  | [{rewrite: {focus: AtExp(LET(d, e)), ctxts: []}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(LET_A(annotateDec(d, [anno0]), annotateExp(e, [anno1]), [])),
            ctxtsAnno: [],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Dec_A(annotateDec(d, [anno0])),
            ctxtsAnno: [LETD_A((), annotateExp(e, [anno1]), [])],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  /* continue with exp */
  | [{rewrite: {focus: ValEnv(ve), ctxts: [LETD((), e), ...ctxts]}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: ValEnv_A(annotateValEnv(ve, [anno0])),
            ctxtsAnno: [LETD_A((), annotateExp(e, [anno1]), []), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Exp_A(annotateExp(e, [anno1])),
            ctxtsAnno,
          },
          envAnno: annotateValEnv(ve, [anno0]) @ envAnno,
        },
        ...framesAnno,
      ],
    });
  /* push LET into a new frame */
  | [{rewrite: {focus: AtExp(LET(d, e)), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, [anno1]);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(LET_A(annotateDec(d, []), annotateExp(e, []), [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(LET_A(annotateDec(d, []), annotateExp(e, []), [])),
            ctxtsAnno: [],
          },
          envAnno,
        },
        {
          rewriteAnno: {
            focusAnno: Empty_A,
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // [94]
  | [{rewrite: {focus: AtExp(PAR(e)), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(PAR_A(annotateExp(e, [anno0]), [anno1])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Exp_A(annotateExp(e, [anno0])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  /* Expression Rows */
  /* TODO: can't annotate the label!!!! */
  /* TODO: focus probably doesn't need a top-level annotation. */
  /* TODO: would like lhs and rhs annotation types to be equal so could hoist optional. or at least
     to have monadic stuff I guess. */
  // [95]
  // start visiting
  | [{rewrite: {focus: ExpRow(EXPROW(l, e, r)), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno:
              ExpRow_A(
                EXPROW_A(
                  l,
                  annotateExp(e, [anno0]),
                  switch (r) {
                  | None => None
                  | Some(er) => Some(annotateExpRow(er, [anno1]))
                  },
                  [anno2],
                ),
              ),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Exp_A(annotateExp(e, [anno0])),
            ctxtsAnno: [
              EXPROWE_A(
                [],
                l,
                (),
                switch (r) {
                | None => None
                | Some(er) => Some(annotateExpRow(er, [anno1]))
                },
                [anno2],
              ),
              ...ctxtsAnno,
            ],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  // mid visiting
  | [
      {
        rewrite: {
          focus: Val(v),
          ctxts: [EXPROWE(r, l1, (), Some(EXPROW(l2, e, rest))), ...ctxts],
        },
        env,
      },
      ...frames,
    ] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let anno3 = genFresh();
    let anno4 = genFresh();
    let anno5 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(annotateVal_(v, [anno0])),
            ctxtsAnno: [
              EXPROWE_A(
                annotateRecord(r, [anno1]),
                l1,
                (),
                Some(
                  EXPROW_A(
                    l2,
                    annotateExp(e, [anno4]),
                    switch (rest) {
                    | None => None
                    | Some(rest) => Some(annotateExpRow(rest, [anno5]))
                    },
                    [anno3],
                  ),
                ),
                [anno2],
              ),
              ...ctxtsAnno,
            ],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Exp_A(annotateExp(e, [anno4])),
            ctxtsAnno: [
              EXPROWE_A(
                annotateRecord(r, [anno1]) @ [(l1, annotateVal_(v, [anno0]))],
                l2,
                (),
                switch (rest) {
                | None => None
                | Some(rest) => Some(annotateExpRow(rest, [anno5]))
                },
                [anno3],
              ),
              ...ctxtsAnno,
            ],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  // complete visiting
  | [{rewrite: {focus: Val(v), ctxts: [EXPROWE(r, l, (), None), ...ctxts]}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(annotateVal_(v, [anno0])),
            ctxtsAnno: [
              EXPROWE_A(annotateRecord(r, [anno1]), l, (), None, [anno2]),
              ...ctxtsAnno,
            ],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno:
              Record_A(annotateRecord(r, [anno1]) @ [(l, annotateVal_(v, [anno0]))]),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  /* Expressions */
  // [96]
  | [{rewrite: {focus: Exp(ATEXP(a)), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Exp_A(ATEXP_A(annotateAtExp(a, [anno0]), [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(annotateAtExp(a, [anno0])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // helper rule for function application
  | [{rewrite: {focus: Exp(APP(f, x)), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let anno3 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno:
              Exp_A(APP_A(annotateExp(f, [anno0]), annotateAtExp(x, [anno1]), [anno2])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Exp_A(annotateExp(f, [anno0])),
            ctxtsAnno: [APPL_A((), annotateAtExp(x, [anno1]), [anno2]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // /* TODO: consolidate some of these rules? */
  // [97]: no ref check
  | [{rewrite: {focus: Val(VID(vid)), ctxts: [APPL((), a), ...ctxts]}, env}, ...frames] =>
    Js.log("97a");
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(VID_A(vid, [anno0])),
            ctxtsAnno: [APPL_A((), annotateAtExp(a, [anno1]), [anno2]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(annotateAtExp(a, [anno1])),
            ctxtsAnno: [APPR_A(VID_A(vid, [anno0]), (), [anno2]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  /* TODO: can't annotate vid!! */
  | [{rewrite: {focus: Val(v), ctxts: [APPR(VID(vid), ()), ...ctxts]}, env}, ...frames] =>
    Js.log("97b");
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let anno3 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(annotateVal_(v, [anno0])),
            ctxtsAnno: [APPR_A(VID_A(vid, [anno1]), (), [anno2]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(VIDVAL_A(vid, annotateVal_(v, [anno0]), [anno3])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // [101]
  /* TODO: may want a more coarse-grained traversal, not sure */
  | [{rewrite: {focus: Val(BASVAL(f)), ctxts: [APPL((), a), ...ctxts]}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let aAnno = annotateAtExp(a, [genFresh()]);
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(BASVAL_A(f, [anno0])),
            ctxtsAnno: [APPL_A((), aAnno, [anno1]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(aAnno),
            ctxtsAnno: [APPR_A(BASVAL_A(f, [anno0]), (), [anno1]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  | [{rewrite: {focus: Val(v), ctxts: [APPR(BASVAL(f), ()), ...ctxts]}, env}, ...frames] =>
    let vAnno = annotateVal_(v, [genFresh()]);
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(vAnno),
            ctxtsAnno: [APPR_A(BASVAL_A(f, [anno0]), (), [anno1]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(apply(f, vAnno)),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // [102]
  | [
      {rewrite: {focus: Val(FCNCLOSURE(m, e, ve)), ctxts: [APPL((), a), ...ctxts]}, env},
      ...frames,
    ] =>
    let anno1 = genFresh();
    let fcnClosureAnno = annotateVal_(FCNCLOSURE(m, e, ve), [genFresh()]);
    let aAnno = annotateAtExp(a, [genFresh()]);
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(fcnClosureAnno),
            ctxtsAnno: [APPL_A((), aAnno, [anno1]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: AtExp_A(aAnno),
            ctxtsAnno: [APPR_A(fcnClosureAnno, (), [anno1]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  /* TODO: NEED TO BE ABLE TO LABEL FRAMES!!!! */
  | [
      {rewrite: {focus: Val(v), ctxts: [APPR(FCNCLOSURE(m, e, ve), ()), ...ctxts]}, env},
      ...frames,
    ] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let anno3 = genFresh();
    let anno4 = genFresh();
    let anno5 = genFresh();
    let anno6 = genFresh();
    let anno7 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, [genFresh()]);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(annotateVal_(v, [anno0])),
            ctxtsAnno: [
              APPR_A(
                FCNCLOSURE_A(
                  annotateMatch(m, [anno1]),
                  annotateValEnv(e, [anno2]),
                  annotateValEnv(ve, [anno3]),
                  [anno5],
                ),
                (),
                [anno4],
              ),
              ...ctxtsAnno,
            ],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Match_A(annotateMatch(m, [anno1]), annotateVal_(v, [anno0])),
            ctxtsAnno: [],
          },
          envAnno: recEnv(annotateValEnv(ve, [anno3])) @ envAnno /* "backwards" compared to spec b/c 4.2 says lookup happens in RHS first */
        },
        {
          rewriteAnno: {
            focusAnno: Empty_A,
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // [108]
  | [{rewrite: {focus: Exp(FN(m)), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let anno3 = genFresh();
    let envAnno = annotateValEnv(env, [anno3]);
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let framesAnno = annotateFrames(frames, []);
    let matchAnno = annotateMatch(m, [anno0]);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Exp_A(FN_A(matchAnno, [anno1])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(FCNCLOSURE_A(annotateMatch(m, [anno0]), envAnno, [], [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  /* Matches */
  /* begin Match traversal */
  | [{rewrite: {focus: Match(MATCH(mr, om), v), ctxts: []}, env}, ...frames] =>
    let mrAnno = annotateMRule(mr, [genFresh()]);
    let omAnno =
      switch (om) {
      | None => None
      | Some(m) => Some(annotateMatch(m, [genFresh()]))
      };
    let vAnno = annotateVal_(v, [genFresh()]);
    let anno0 = genFresh();
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Match_A(MATCH_A(mrAnno, omAnno, [anno0]), vAnno),
            ctxtsAnno: [],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: MRule_A(mrAnno, vAnno),
            ctxtsAnno: [MATCHMR_A((), omAnno, [anno0])],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  | [{rewrite: {focus: Match(m, v), ctxts}, env}, ...frames] =>
    let envAnno = annotateValEnv(env, [genFresh()]);
    let framesAnno = annotateFrames(frames, []);
    let mAnno = annotateMatch(m, [genFresh()]);
    let vAnno = annotateVal_(v, [genFresh()]);
    let ctxtsAnno = annotateCtxts(ctxts, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Match_A(mAnno, vAnno),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Match_A(mAnno, vAnno),
            ctxtsAnno: [],
          },
          envAnno,
        },
        {
          rewriteAnno: {
            focusAnno: Empty_A,
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // [109]
  /* mrule success */
  | [{rewrite: {focus: Val(v), ctxts: [MATCHMR((), oldM), ...ctxts]}, env}, ...frames] =>
    let vAnno = annotateVal_(v, [genFresh()]);
    let oldMAnno =
      switch (oldM) {
      | None => None
      | Some(m) => Some(annotateMatch(m, [genFresh()]))
      };
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(vAnno),
            ctxtsAnno: [MATCHMR_A((), oldMAnno, [genFresh()]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [{
              rewriteAnno: {
                focusAnno: Val_A(vAnno),
                ctxtsAnno,
              },
              envAnno,
            }, ...framesAnno],
    });

  // [110]
  | [{rewrite: {focus: FAIL(v), ctxts: [MATCHMR((), None), ...ctxts]}, env}, ...frames] =>
    let vAnno = annotateVal_(v, [genFresh()]);
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: FAIL_A(vAnno),
            ctxtsAnno: [MATCHMR_A((), None, [genFresh()]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [{
              rewriteAnno: {
                focusAnno: FAIL_A(vAnno),
                ctxtsAnno,
              },
              envAnno,
            }, ...framesAnno],
    });

  // [111]
  | [{rewrite: {focus: FAIL(v), ctxts: [MATCHMR((), Some(m)), ...ctxts]}, env}, ...frames] =>
    let vAnno = annotateVal_(v, [genFresh()]);
    let mAnno = annotateMatch(m, [genFresh()]);
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: FAIL_A(vAnno),
            ctxtsAnno: [MATCHMR_A((), Some(mAnno), [genFresh()]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Match_A(mAnno, vAnno),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  /* Match Rules */
  // [112]
  | [{rewrite: {focus: MRule(MRULE(p, e), v), ctxts}, env}, ...frames] =>
    let pAnno = annotatePat(p, [genFresh()]);
    let eAnno = annotateExp(e, [genFresh()]);
    let vAnno = annotateVal_(v, [genFresh()]);
    let anno0 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: MRule_A(MRULE_A(pAnno, eAnno, [anno0]), vAnno),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Pat_A(pAnno, vAnno),
            ctxtsAnno: [MRULEP_A((), eAnno, [anno0]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  | [{rewrite: {focus: ValEnv(ve), ctxts: [MRULEP((), e), ...ctxts]}, env}, ...frames] =>
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, [genFresh()]);
    let framesAnno = annotateFrames(frames, []);
    let veAnno = annotateValEnv(ve, [genFresh()]);
    let eAnno = annotateExp(e, [genFresh()]);
    let anno0 = genFresh();
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: ValEnv_A(veAnno),
            ctxtsAnno: [MRULEP_A((), eAnno, [anno0]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Exp_A(eAnno),
            ctxtsAnno,
          },
          envAnno: veAnno @ envAnno,
        },
        ...framesAnno,
      ],
    });

  // [113]
  | [{rewrite: {focus: FAIL(v), ctxts: [MRULEP((), oldE), ...ctxts]}, env}, ...frames] =>
    let vAnno = annotateVal_(v, [genFresh()]);
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    let oldEAnno = annotateExp(oldE, []);

    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: FAIL_A(vAnno),
            ctxtsAnno: [MRULEP_A((), oldEAnno, [genFresh()]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [{
              rewriteAnno: {
                focusAnno: FAIL_A(vAnno),
                ctxtsAnno,
              },
              envAnno,
            }, ...framesAnno],
    });

  /* Declarations */
  // [114ish]: should lift valenv into env
  | [{rewrite: {focus: Dec(VAL(vb)), ctxts}, env}, ...frames] =>
    Js.log("114 in");
    let anno0 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Dec_A(VAL_A(annotateValBind(vb, [anno0]), [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: ValBind_A(annotateValBind(vb, [anno0])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  /* Value Bindings */
  // [124ish]: doesn't support `and`
  | [{rewrite: {focus: ValBind(PLAIN(p, e, None)), ctxts}, env}, ...frames] =>
    Js.log("124 in e");
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno:
              ValBind_A(PLAIN_A(annotatePat(p, [anno0]), annotateExp(e, [anno1]), None, [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Exp_A(annotateExp(e, [anno1])),
            ctxtsAnno: [VALBINDE_A(annotatePat(p, [anno0]), (), None, []), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  | [{rewrite: {focus: ValBind(PLAIN(p, e, Some(vb))), ctxts}, env}, ...frames] =>
    Js.log("124 in e");
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno:
              ValBind_A(
                PLAIN_A(
                  annotatePat(p, [anno0]),
                  annotateExp(e, [anno1]),
                  Some(annotateValBind(vb, [anno2])),
                  [],
                ),
              ),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Exp_A(annotateExp(e, [anno1])),
            ctxtsAnno: [
              VALBINDE_A(
                annotatePat(p, [anno0]),
                (),
                Some(annotateValBind(vb, [anno2])),
                [],
              ),
              ...ctxtsAnno,
            ],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  | [{rewrite: {focus: Val(v), ctxts: [VALBINDE(p, (), None), ...ctxts]}, env}, ...frames] =>
    Js.log("124 in p");
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Val_A(annotateVal_(v, [anno0])),
            ctxtsAnno: [VALBINDE_A(annotatePat(p, [anno1]), (), None, []), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Pat_A(annotatePat(p, [anno1]), annotateVal_(v, [anno0])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // [126]
  | [{rewrite: {focus: ValBind(REC(vb)), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: ValBind_A(REC_A(annotateValBind(vb, [anno0]), [anno1])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: ValBind_A(annotateValBind(vb, [anno0])),
            ctxtsAnno: [RECVB_A((), [anno1]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  | [{rewrite: {focus: ValEnv(ve), ctxts: [RECVB (), ...ctxts]}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: ValEnv_A(annotateValEnv(ve, [anno0])),
            ctxtsAnno: [RECVB_A((), [anno1]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: ValEnv_A(recEnv(annotateValEnv(ve, [anno0]))),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  /* Type Bindings */
  /* Datatype Bindings */
  /* Constructor Bindings */
  /* Exception Bindings */
  /* Atomic Patterns */
  // // [132]
  // | [{rewrite: {focus: AtPat(WILDCARD, _), ctxts}, env}, ...frames] =>
  //   Some([{
  //           rewrite: {
  //             focus: ValEnv([]),
  //             ctxts,
  //           },
  //           env,
  //         }, ...frames])

  /* TODO: need to have annotations for vids */
  /* sort of needs multiple annotations on the left? lookup is still completely opaque */
  // [135-137ish]
  | [{rewrite: {focus: AtPat(ID(x), v), ctxts}, env}, ...frames] =>
    switch (Util.lookupOne(x, env)) {
    // [135]
    | None
    | Some((_, Var)) =>
      let anno0 = genFresh();
      let anno1 = genFresh();
      let ctxtsAnno = annotateCtxts(ctxts, []);
      let envAnno = annotateValEnv(env, []);
      let framesAnno = annotateFrames(frames, []);
      Some({
        lhs: [
          {
            rewriteAnno: {
              focusAnno: AtPat_A(ID_A(x, []), annotateVal_(v, [anno1])),
              ctxtsAnno,
            },
            envAnno,
          },
          ...framesAnno,
        ],
        rhs: [
          {
            rewriteAnno: {
              focusAnno: ValEnv_A([(x, (annotateVal_(v, [anno1]), Var_A([])))]),
              ctxtsAnno,
            },
            envAnno,
          },
          ...framesAnno,
        ],
      });
    // [136]
    | Some((v', id)) when v == v' =>
      let anno0 = genFresh();
      let ctxtsAnno = annotateCtxts(ctxts, []);
      let envAnno = annotateValEnv(env, []);
      let framesAnno = annotateFrames(frames, []);
      Some({
        lhs: [
          {
            rewriteAnno: {
              focusAnno: AtPat_A(ID_A(x, []), annotateVal_(v, [anno0])),
              ctxtsAnno,
            },
            envAnno,
          },
          ...framesAnno,
        ],
        rhs: [{
                rewriteAnno: {
                  focusAnno: ValEnv_A([]),
                  ctxtsAnno,
                },
                envAnno,
              }, ...framesAnno],
      });
    // [137]
    | Some((v', id)) =>
      let anno1 = genFresh();
      let ctxtsAnno = annotateCtxts(ctxts, []);
      let envAnno = annotateValEnv(env, []);
      let framesAnno = annotateFrames(frames, []);
      Some({
        lhs: [
          {
            rewriteAnno: {
              focusAnno: AtPat_A(ID_A(x, []), annotateVal_(v, [anno1])),
              ctxtsAnno,
            },
            envAnno,
          },
          ...framesAnno,
        ],
        rhs: [
          {
            rewriteAnno: {
              focusAnno: FAIL_A(annotateVal_(v, [anno1])),
              ctxtsAnno,
            },
            envAnno,
          },
          ...framesAnno,
        ],
      });
    }

  // [138]
  /* empty record pat */
  /* TODO: special-casing RECORD(None) may be overkill. not sure */
  | [{rewrite: {focus: AtPat(RECORD(None), RECORD([])), ctxts}, env}, ...frames] =>
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: AtPat_A(RECORD_A(None, []), RECORD_A([], [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [{
              rewriteAnno: {
                focusAnno: ValEnv_A([]),
                ctxtsAnno,
              },
              envAnno,
            }, ...framesAnno],
    });
  | [{rewrite: {focus: AtPat(RECORD(None), v), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: AtPat_A(RECORD_A(None, []), annotateVal_(v, [anno0])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: FAIL_A(annotateVal_(v, [anno0])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  /* start non-empty record pat */
  | [{rewrite: {focus: AtPat(RECORD(Some(pr)), RECORD(r)), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let anno2 = genFresh();
    let anno3 = genFresh();
    let anno4 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno:
              AtPat_A(
                RECORD_A(Some(annotatePatRow(pr, [anno0])), [anno1]),
                RECORD_A(annotateRecord(r, [anno2]), [anno3]),
              ),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: PatRow_A(annotatePatRow(pr, [anno0]), annotateRecord(r, []), []),
            ctxtsAnno: [RECORDPR_A((), [anno4]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  /* complete non-empty record pat */
  | [{rewrite: {focus: ValEnv(ve), ctxts: [RECORDPR (), ...ctxts]}, env}, ...frames] =>
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    let veAnno = annotateValEnv(ve, [genFresh()]);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: ValEnv_A(veAnno),
            ctxtsAnno: [RECORDPR_A((), [genFresh()]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [{
              rewriteAnno: {
                focusAnno: ValEnv_A(veAnno),
                ctxtsAnno,
              },
              envAnno,
            }, ...framesAnno],
    });
  | [{rewrite: {focus: FAIL(v), ctxts: [RECORDPR (), ...ctxts]}, env}, ...frames] =>
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    let vAnno = annotateVal_(v, [genFresh()]);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: FAIL_A(vAnno),
            ctxtsAnno: [RECORDPR_A((), [genFresh()]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [{
              rewriteAnno: {
                focusAnno: FAIL_A(vAnno),
                ctxtsAnno,
              },
              envAnno,
            }, ...framesAnno],
    });

  // [139]
  | [{rewrite: {focus: AtPat(PAR(p), v), ctxts}, env}, ...frames] =>
    let pAnno = annotatePat(p, [genFresh()]);
    let vAnno = annotateVal_(v, [genFresh()]);
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: AtPat_A(PAR_A(pAnno, [genFresh()]), vAnno),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Pat_A(pAnno, vAnno),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  // /* Pattern Rows */
  // // [140]
  // | [{rewrite: {focus: PatRow(DOTS, _, rve), ctxts}, env}, ...frames] =>
  //   Some([
  //     {
  //       rewrite: {
  //         focus: ValEnv(rve |> List.map(((_, ve)) => ve) |> List.flatten),
  //         ctxts,
  //       },
  //       env,
  //     },
  //     ...frames,
  //   ])

  // [141-142]
  /* NOTE: SML '97 says each field should be evaluated in the original environment, but we extend it
     after every field. This simplifies our implementation since we don't need to maintain a
     temporary environment outside of `env`. This still agrees with SML '97 thanks to the
     left-linear pattern restriction. */
  /* start visiting */
  | [{rewrite: {focus: PatRow(FIELD(l, p, opr), r, ve), ctxts}, env}, ...frames] =>
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    let pAnno = annotatePat(p, [genFresh()]);
    let oprAnno =
      switch (opr) {
      | None => None
      | Some(pr) => Some(annotatePatRow(pr, [genFresh()]))
      };
    let rAnno = annotateRecord(r, [genFresh()]);
    let veAnno = annotateRecordEnv(ve, [genFresh()]);
    let anno0 = genFresh(); /* TODO: not sure if this is used correctly. */
    /* NOTE: Shouldn't fail for well-typed programs per SML '97 comments. */
    let Some(v) = Util.lookupOne(l, r);
    let vAnno = annotateVal_(v, [genFresh()]);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: PatRow_A(FIELD_A(l, pAnno, oprAnno, [anno0]), rAnno, veAnno),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Pat_A(pAnno, vAnno),
            ctxtsAnno: [FIELDP_A((l, (), oprAnno), rAnno, veAnno, [anno0]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  // [141]
  | [
      {rewrite: {focus: FAIL(v), ctxts: [FIELDP((_l, (), _opr), _r, _rve), ...ctxts]}, env},
      ...frames,
    ] =>
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    let vAnno = annotateVal_(v, []);
    let _oprAnno =
      switch (_opr) {
      | None => None
      | Some(pr) => Some(annotatePatRow(pr, [genFresh()]))
      };
    let _rAnno = annotateRecord(_r, [genFresh()]);
    let _rveAnno = annotateRecordEnv(_rve, [genFresh()]);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: FAIL_A(vAnno),
            ctxtsAnno: [
              FIELDP_A((_l, (), _oprAnno), _rAnno, _rveAnno, [genFresh()]),
              ...ctxtsAnno,
            ],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [{
              rewriteAnno: {
                focusAnno: FAIL_A(vAnno),
                ctxtsAnno,
              },
              envAnno,
            }, ...framesAnno],
    });
  // [142]
  | [
      {rewrite: {focus: ValEnv(ve), ctxts: [FIELDP((_l, (), None), _r, rve), ...ctxts]}, env},
      ...frames,
    ] =>
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    let veAnno = annotateValEnv(ve, [genFresh()]);
    let _rAnno = annotateRecord(_r, [genFresh()]);
    let rveAnno = annotateRecordEnv(rve, [genFresh()]);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: ValEnv_A(veAnno),
            ctxtsAnno: [FIELDP_A((_l, (), None), _rAnno, rveAnno, [genFresh()]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno:
              ValEnv_A(veAnno @ (rveAnno |> List.map(((_, ve)) => ve) |> List.flatten)),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  | [
      {
        rewrite: {focus: ValEnv(ve), ctxts: [FIELDP((l, (), Some(pr)), r, rve), ...ctxts]},
        env,
      },
      ...frames,
    ] =>
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    let veAnno = annotateValEnv(ve, [genFresh()]);
    let prAnno = annotatePatRow(pr, [genFresh()]);
    let rAnno = annotateRecord(r, [genFresh()]);
    let rveAnno = annotateRecordEnv(rve, [genFresh()]);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: ValEnv_A(veAnno),
            ctxtsAnno: [
              FIELDP_A((l, (), Some(prAnno)), rAnno, rveAnno, [genFresh()]),
              ...ctxtsAnno,
            ],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: PatRow_A(prAnno, rAnno, [(l, veAnno), ...rveAnno]),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  /* Patterns */
  // [143]
  | [{rewrite: {focus: Pat(ATPAT(ap), v), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno:
              Pat_A(ATPAT_A(annotateAtPat(ap, [anno0]), []), annotateVal_(v, [anno1])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: AtPat_A(annotateAtPat(ap, [anno0]), annotateVal_(v, [anno1])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });

  /* TODO: maybe consolidate 145a and b somehow? */
  | [{rewrite: {focus: Pat(CON(con, ap), VIDVAL(vid, v)), ctxts}, env}, ...frames] =>
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    let apAnno = annotateAtPat(ap, [genFresh()]);
    let vAnno = annotateVal_(v, [genFresh()]);
    let anno0 = genFresh();
    let lhs = [
      {
        rewriteAnno: {
          focusAnno: Pat_A(CON_A(con, apAnno, [genFresh()]), VIDVAL_A(vid, vAnno, [anno0])),
          ctxtsAnno,
        },
        envAnno,
      },
      ...framesAnno,
    ];
    let Some((VID(vidC), Con)) = Util.lookupOne(con, env);
    if (vidC == vid) {
      // [144]: ignores ref check
      Js.log("144");
      Js.log(con);
      Js.log(ap);
      Some({
        lhs,
        rhs: [
          {
            rewriteAnno: {
              focusAnno: AtPat_A(apAnno, vAnno),
              ctxtsAnno,
            },
            envAnno,
          },
          ...framesAnno,
        ],
      });
    } else {
      // [145a]
      Js.log("145a");
      Some({
        lhs,
        rhs: [
          {
            rewriteAnno: {
              focusAnno: FAIL_A(VIDVAL_A(vid, vAnno, [anno0])),
              ctxtsAnno,
            },
            envAnno,
          },
          ...framesAnno,
        ],
      });
    };

  // // [145b]
  // | [{rewrite: {focus: Pat(CON(_, _), v), ctxts}, env}, ...frames] =>
  //   Js.log("145b");
  //   Some([{
  //           rewrite: {
  //             focus: FAIL(v),
  //             ctxts,
  //           },
  //           env,
  //         }, ...frames]);

  /* ... */

  /* Structure-level Declarations */
  // [156]
  | [{rewrite: {focus: StrDec(DEC(d)), ctxts}, env}, ...frames] =>
    Js.log("156 in");
    let anno0 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: StrDec_A(DEC_A(annotateDec(d, [anno0]), [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: Dec_A(annotateDec(d, [anno0])),
            ctxtsAnno: [DECD_A((), []), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  | [{rewrite: {focus: ValEnv(ve), ctxts: [DECD (), ...ctxts]}, env}, ...frames] =>
    Js.log("156 out");
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: ValEnv_A(annotateValEnv(ve, [anno0])),
            ctxtsAnno: [DECD_A((), [anno1]), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: ValEnv_A(annotateValEnv(ve, [anno0])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  // // [160]
  // | [{rewrite: {focus: StrDec(SEQ(sd1, sd2)), ctxts}, env}, ...frames] =>
  //   Some([{
  //           rewrite: {
  //             focus: StrDec(sd1),
  //             ctxts: [SEQL((), sd2), ...ctxts],
  //           },
  //           env,
  //         }, ...frames])
  // /* TODO: propagate ValEnv */
  // | [{rewrite: {focus: ValEnv(ve), ctxts: [SEQL((), sd2), ...ctxts]}, env}, ...frames] =>
  //   Some([{
  //           rewrite: {
  //             focus: StrDec(sd2),
  //             ctxts,
  //           },
  //           env,
  //         }, ...frames])

  // /* ... */

  /* Top-level Declarations */
  // [184ish]
  | [{rewrite: {focus: TopDec(STRDEC(sd, None)), ctxts}, env}, ...frames] =>
    Js.log("184 in");
    let anno0 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: TopDec_A(STRDEC_A(annotateStrDec(sd, [anno0]), None, [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: StrDec_A(annotateStrDec(sd, [anno0])),
            ctxtsAnno: [STRDECSD_A((), None, []), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  | [{rewrite: {focus: TopDec(STRDEC(sd, Some(td))), ctxts}, env}, ...frames] =>
    Js.log("184 in");
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno:
              TopDec_A(
                STRDEC_A(annotateStrDec(sd, [anno0]), Some(annotateTopDec(td, [anno1])), []),
              ),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: StrDec_A(annotateStrDec(sd, [anno0])),
            ctxtsAnno: [STRDECSD_A((), Some(annotateTopDec(td, [anno1])), []), ...ctxtsAnno],
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  | [{rewrite: {focus: ValEnv(ve), ctxts: [STRDECSD((), td), ...ctxts]}, env}, ...frames] =>
    Js.log("184 out");
    let anno0 = genFresh();
    let anno1 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: ValEnv_A(annotateValEnv(ve, [anno0])),
            ctxtsAnno: {
              let td =
                switch (td) {
                | None => None
                | Some(td) => Some(annotateTopDec(td, [anno1]))
                };
              [STRDECSD_A((), td, []), ...ctxtsAnno];
            },
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno:
              switch (td) {
              | None => Empty_A
              | Some(td) => TopDec_A(annotateTopDec(td, [anno1]))
              },
            ctxtsAnno,
          },
          envAnno: annotateValEnv(ve, [anno0]) @ envAnno,
        },
        ...framesAnno,
      ],
    });

  /* Programs */
  // [189ish]
  | [{rewrite: {focus: Program(PROGRAM(td, None)), ctxts}, env}, ...frames] =>
    let anno0 = genFresh();
    let ctxtsAnno = annotateCtxts(ctxts, []);
    let envAnno = annotateValEnv(env, []);
    let framesAnno = annotateFrames(frames, []);
    Some({
      lhs: [
        {
          rewriteAnno: {
            focusAnno: Program_A(PROGRAM_A(annotateTopDec(td, [anno0]), None, [])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
      rhs: [
        {
          rewriteAnno: {
            focusAnno: TopDec_A(annotateTopDec(td, [anno0])),
            ctxtsAnno,
          },
          envAnno,
        },
        ...framesAnno,
      ],
    });
  // | [{rewrite: {focus: Program(PROGRAM(td, Some(p))), ctxts}, env}, ...frames] =>
  //   Some([
  //     {
  //       rewrite: {
  //         focus: TopDec(td),
  //         ctxts: [PROGRAML((), p), ...ctxts],
  //       },
  //       env,
  //     },
  //     ...frames,
  //   ])
  // | [{rewrite: {focus: Empty, ctxts: [PROGRAML((), p), ...ctxts]}, env}, ...frames] =>
  //   Some([{
  //           rewrite: {
  //             focus: Program(p),
  //             ctxts,
  //           },
  //           env,
  //         }, ...frames])
  | _ => None
  };

/* TODO: need to detect end using program segment somehow */
let isFinal = c =>
  switch (c) {
  /* | {frames: []} => true */
  // | {frames: [{ rewrite: {rewrite: Value(_), ctxs: []} }]} => true
  | _ => false
  };

// let injectExp = (e) => { rewrite: { focus: Exp(e), ctxts: [] }, env: [] };

let inject = e => [
  {
    rewrite: {
      focus: e,
      ctxts: [],
    },
    env: [
      ("=", (BASVAL("="), Var)),
      ("+", (BASVAL("+"), Var)),
      ("-", (BASVAL("-"), Var)),
      ("*", (BASVAL("*"), Var)),
      ("<", (BASVAL("<"), Var)),
      ("true", (VID("true"), Con)),
      ("false", (VID("false"), Con)),
      ("nil", (VID("nil"), Con)),
      ("::", (VID("::"), Con)),
    ],
  },
];

let advance = c =>
  switch (step(c)) {
  | None => None
  | Some({lhs: _, rhs: c_anno}) => Some(stripConfiguration(c_anno))
  };

let interpretTraceBounded = (~maxDepth=100, p) =>
  TheiaUtil.takeWhileInclusive(
    c => !isFinal(c),
    TheiaUtil.iterateMaybeMaxDepth(maxDepth, advance, inject(p)),
  );
let interpretTrace = p =>
  TheiaUtil.takeWhileInclusive(c => !isFinal(c), TheiaUtil.iterateMaybe(advance, inject(p)));