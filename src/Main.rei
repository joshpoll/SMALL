type program = {
  name: string,
  text: string,
};

let traceProgram: program => Js.Promise.t(list(SML.configuration));