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

/**
  Use these phantom types to ensure that the Curry constructor is only applied
  with a RHS of Select. This ensures that we have a reverse linked list
  structure such as:
    Curry(Curry(Select, Select), Select)
  And we exclude structures like:
    Curry(Curry(Select, Select), Curry(Select, Select))
    and
    Curry(Select, Curry(Select, Select))
 */
type curry;
type select;

type t(_, _, _, _) =
  | Curry(
      t('either, 'input, 'processor2, 'processor1),
      t(select, 'input, 'output2, 'processor2),
      bool,
    )
    : t(curry, 'input, 'output2, 'processor1)
  | Select('input => 'output1, bool)
    : t(select, 'input, 'output, 'output1 => 'output);

let uncached:
  type constructor input output proc.
    t(constructor, input, output, proc) =>
    t(constructor, input, output, proc) =
  lhs => {
    switch (lhs) {
    | Select(fn, true) => Select(fn, false)
    | Select(_, false) => lhs
    | Curry(lhs, rhs, true) => Curry(lhs, rhs, false)
    | Curry(_, _, false) => lhs
    };
  };

let rec create:
  type constructor input output processor.
    (t(constructor, input, output, processor), processor, input) => output =
  (selector, processor) => {
    switch (selector) {
    | Select(inputToProcInput, cache) =>
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
    | Curry(Curry(_, _, _) as lhs, Select(inputToProcInput2, _), _cached) =>
      let next = create(lhs, processor);

      let inputCache =
        Cache.make(((cachedKey1, cachedKey2), (key1, key2)) =>
          cachedKey1 === key1 && cachedKey2 === key2
        );

      let r: input => output = (
        input => {
          let cont = next(input);
          let input2 = inputToProcInput2(input);
          switch (inputCache((cont, input2))) {
          | Cache.Hit(output) => output
          | Cache.Miss(set) => set((cont, input2), cont(input2))
          };
        }
      );
      r;
    | Curry(
        Select(inputToProcInput1, _),
        Select(inputToProcInput2, _),
        cached,
      ) =>
      if (cached) {
        let inputCache =
          Cache.make(((cachedKey1, cachedKey2), (key1, key2)) =>
            cachedKey1 === key1 && cachedKey2 === key2
          );

        let r: input => output = (
          input => {
            let input1 = inputToProcInput1(input);
            let input2 = inputToProcInput2(input);
            switch (inputCache((input1, input2))) {
            | Cache.Hit(output) => output
            | Cache.Miss(set) =>
              set((input1, input2), processor(input1, input2))
            };
          }
        );
        r;
      } else {
        let r: input => output = (
          input => {
            let input1 = inputToProcInput1(input);
            let input2 = inputToProcInput2(input);
            let output = processor(input1, input2);
            output;
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

let select = stateToValue => Select(stateToValue, true);
let curry = (=>>);
