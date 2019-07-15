module Cache = {
  type checkResult('key, 'value) =
    | Hit('value)
    | Miss(('key, 'value) => 'value);

  type t('key, 'value) = 'key => checkResult('key, 'value);

  let make: (('key, 'key) => bool) => t('key, 'value) =
    isSame => {
      let cell = ref(None);
      let set = (key, value) => {
        cell := Some((key, value));
        value;
      };
      let check = key =>
        switch (cell^) {
        | None => Miss(set)
        | Some((cachedKey, value)) =>
          if (isSame(cachedKey, key)) {
            Hit(value);
          } else {
            Miss(set);
          }
        };
      check;
    };
};

type t(_, _, _) =
  // | Curry(
  //     t('input, 'mid2 => 'output2, ('mid1, 'mid2) => 'output2),
  //     t('input, 'output2, 'mid2 => 'output2),
  //     bool,
  //   )
  //   : t('input, 'output2, ('mid1, 'mid2) => 'output2)
  // Original Curry pre-memoization
  // We need to pull out the 'mid1/'mid2 parameter types in order to inject
  // memoization into the processor function
  | Curry(
      t('input, 'processor2, 'processor1),
      t('input, 'output2, 'processor2),
      bool,
    )
    : t('input, 'output2, 'processor1)
  | Memo('input => 'output1, bool): t('input, 'output, 'output1 => 'output);

let uncached:
  type input output proc. t(input, output, proc) => t(input, output, proc) =
  lhs => {
    switch (lhs) {
    | Memo(fn, true) => Memo(fn, false)
    | Memo(_, false) => lhs
    | Curry(lhs, rhs, true) => Curry(lhs, rhs, false)
    | Curry(_, _, false) => lhs
    };
  };

let rec create:
  type input output processor.
    (t(input, output, processor), processor, input) => output =
  (selector, processor) => {
    switch (selector) {
    | Memo(inputToProcInput, cache) =>
      if (!cache) {
        (
          input => {
            let procInput = inputToProcInput(input);
            processor(procInput);
          }
        );
      } else {
        let cache = Cache.make((===));
        let r: input => output = (
          input => {
            let procInput = inputToProcInput(input);
            switch (cache(procInput)) {
            | Cache.Miss(set) => set(procInput, processor(procInput))
            | Cache.Hit(output) => output
            };
          }
        );
        r;
      }
    | Curry(lhs, rhs, _cached) =>
      let next = create(uncached(lhs), processor);
      let r: input => output = (
        input => {
          let cont = next(input);
          let rhsout = create(uncached(rhs), cont, input);
          rhsout;
        }
      );
      r;
    };
  };

let (=>>) = (a, b) => Curry(a, b, true);

let select = stateToValue => Memo(stateToValue, true);
let curry = (=>>);
