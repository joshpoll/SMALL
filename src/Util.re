let rec interleave = (xs, ys) =>
  switch (xs, ys) {
  | ([], _) => ys
  | ([x, ...xs], _) => [x, ...interleave(ys, xs)]
  };

let split = (list, n) => {
  let rec aux = (i, acc) =>
    fun
    | [] => (List.rev(acc), [])
    | [h, ...t] as l =>
      if (i == 0) {
        (List.rev(acc), l);
      } else {
        aux(i - 1, [h, ...acc], t);
      };
  aux(n, [], list);
};

let insert = (x, xs, i) => {
  let (xs, ys) = split(xs, i);
  xs @ [x, ...ys];
};

/* https://stackoverflow.com/a/244104 */
let (--) = (i, j) => {
  let rec aux = (n, acc) =>
    if (n < i) {
      acc;
    } else {
      aux(n - 1, [n, ...acc]);
    };
  aux(j, []);
};

let rec lookupOne = (key, oneStack) =>
  switch (oneStack) {
  | [] => None
  | [(k, v), ...oneStack] =>
    if (k == key) {
      Some(v);
    } else {
      lookupOne(key, oneStack);
    }
  };

let rec lookup = (key, stack) =>
  switch (stack) {
  | [] => None
  | [os, ...stack] =>
    switch (lookupOne(key, os)) {
    | None => lookup(key, stack)
    | Some(v) => Some(v)
    }
  };

/* https://stackoverflow.com/a/244104 */
let (--) = (i, j) => {
  let rec aux = (n, acc) =>
    if (n < i) {
      acc;
    } else {
      aux(n - 1, [n, ...acc]);
    };
  aux(j, []);
};