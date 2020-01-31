module Fetch = Bs_node_fetch;

type test = {
  name: string,
  example: SML.focus,
};

let jsonToProgram = json => {
  Js.Console.log(json);
  json |> HaMLet2SMALL.Decode.node |> HaMLet2SMALL.compileProgram;
};

type program = {
  name: string,
  text: string,
};

let trace = ({name, example}) => example |> SML.interpretTrace; /* |> List.map(SMLToTheiaAM.smlToTheiaAM) */

let traceProgram = ({name, text}) => {
  let payload = Js.Dict.empty();
  Js.Dict.set(payload, "file_name", Js.Json.string(name));
  Js.Dict.set(payload, "program", Js.Json.string(text));
  Js.Promise.(
    Fetch.fetchWithInit(
      "http://localhost:5000",
      Fetch.RequestInit.make(
        ~method=Post,
        ~body=Fetch.BodyInit.make(Js.Json.stringify(Js.Json.object_(payload))),
        ~headers=Fetch.HeadersInit.make({"Content-Type": "application/json"}),
        (),
      ),
    )
    |> then_(Fetch.Response.json)
    |> then_(json => Js.Promise.resolve(trace({name, example: Program(jsonToProgram(json))})))
  );
};

let traces =
  SMLExamples.(
    [|
      {name: "ex0", text: ex0},
      // {name: "ex1", text: ex1},
      // {name: "ex2", text: ex2},
      // {name: "ex3", text: ex3},
      // {name: "ex4", text: ex4},
      // {name: "ex5", text: ex5},
      // {name: "ex5.5", text: ex5_5},
      // {name: "ex6", text: ex6},
      // {name: "ex7", text: ex7},
      // {name: "ex8", text: ex8},
      // {name: "ex9", text: ex9},
      // {name: "ex10", text: ex10},
      // {name: "ex11", text: ex11},
      // {name: "ex11.5", text: ex11_5},
      // {name: "ex12", text: ex12},
      // {name: "id", text: idEx},
      /* shows off tail recursion behavior! no TCO currently. that would be a semantic change */
      // {name: "lec02-1-simple", text: lec02_1_simpler},
      // {name: "lec02-2-simple", text: lec02_2_simple},
      // {name: "lec02-3", text: lec02_3},
      /* rendered output requires too much memory! recursion too deep(?) not sure how to circumvent yet */
      // {name: "lec02-4-sum_list", text: lec02_4_sum_list},
      /* shows off call stacks and list pattern matching */
      {name: "lec02-4-sum_list-good-style", text: lec02_4_sum_list_good_style},
      // {name: "valbind pattern", text: valbind_pattern},
      // {name: "nested lets", text: nestedLets},
    |]
  )
  |> Array.map(traceProgram);

Js.Promise.all(traces)
|> Js.Promise.then_(theiaIRTraces => Js.Promise.resolve(Js.log2("theiaIRTraces", theiaIRTraces)));