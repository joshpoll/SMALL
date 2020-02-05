type program = {
  name: string,
  text: string,
};

let traceProgram: program => list(Js.Promise.t(SML.configuration));