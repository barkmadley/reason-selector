open Jest;

open Selector;

type inner1 = {
  inner1field1: int,
  inner1field2: string,
};
type inner2 = {
  inner2field1: int,
  inner2field2: string,
};
type state = {
  // mutable fields so bucklescript doesn't inline the fields during spread
  mutable field1: inner1,
  mutable field2: inner2,
};
let state = {
  field1: {
    inner1field1: 1,
    inner1field2: "1",
  },
  field2: {
    inner2field1: 10,
    inner2field2: "10",
  },
};
let state2 = {
  ...state,
  field2: {
    inner2field1: 2,
    inner2field2: "2",
  },
};

let id = x => x;

describe("select composition", () => {
  open Expect;

  test(
    "create(select(create(select(id), field2)), inner2field1)(state) == 10", () => {
    let field2 = state => state.field2;
    let inner2field1 = inner2 => inner2.inner2field1;
    let selectField2 = create(select(id), field2);
    let selector = create(select(selectField2), inner2field1);

    expect(selector(state)) |> toBe(10);
  });

  test(
    "create(select(create(select(id), field2)), inner2field1)(state2) == 2", () => {
    let field2 = state => state.field2;
    let inner2field1 = inner2 => inner2.inner2field1;
    let selectField2 = create(select(id), field2);
    let selector = create(select(selectField2), inner2field1);

    expect(selector(state2)) |> toBe(2);
  });

  test(
    "calling selector twice with the same input only executes field2 and inner2field1 once",
    () => {
      let field2s = ref(0);
      let field2 = state => {
        incr(field2s);
        state.field2;
      };
      let inner2field2s = ref(0);
      let inner2field1 = inner2 => {
        incr(inner2field2s);
        inner2.inner2field1;
      };
      let selectField2 = create(select(id), field2);
      let selector = create(select(selectField2), inner2field1);

      let _ = selector(state);
      let _ = selector(state);

      let _ = expect(field2s^) |> toBe(1);
      expect(inner2field2s^) |> toBe(1);
    },
  );

  test(
    "calling selector twice with different states executes field1 twice and inner1field1 only once",
    () => {
      let field1s = ref(0);
      let field1 = state => {
        incr(field1s);
        state.field1;
      };
      let inner1field1s = ref(0);
      let inner1field1 = inner1 => {
        incr(inner1field1s);
        inner1.inner1field1;
      };
      let selectField1 = create(select(id), field1);
      let selector = create(select(selectField1), inner1field1);

      let _ = selector(state);
      let _ = selector(state2);

      let _ = expect(field1s^) |> toBe(2);
      expect(inner1field1s^) |> toBe(1);
    },
  );
});
