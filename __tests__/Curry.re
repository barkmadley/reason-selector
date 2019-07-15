open Jest;

open Selector;

let id: int => int = x => x;

describe("Curry", () => {
  open Expect;

  test("create(curry(select(id), select(id)), (+))(1) == 2", () => {
    let r = create(curry(select(id), select(id)), (+));
    expect(r(1)) |> toBe(2);
  });

  test("create(select(id) =>> select(id)), (+))(1) == 2", () => {
    let r = create(select(id) =>> select(id), (+));
    expect(r(1)) |> toBe(2);
  });

  test("can combine more than 12 selectors", () => {
    let r =
      create(
        select(id)
        =>> select(id)
        =>> select(id)
        =>> select(id)
        =>> select(id)
        =>> select(id)
        =>> select(id)
        =>> select(id)
        =>> select(id)
        =>> select(id)
        =>> select(id)
        =>> select(id)
        =>> select(id),
        (p1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13) =>
        p1
      );
    expect(r(1)) |> toBe(1);
  });

  test(
    "calling selector twice with the same input only executes callback once",
    () => {
    let selectorCalls = ref(0);
    let callback = (a, b) => {
      incr(selectorCalls);
      a + b;
    };
    let selector = create(curry(select(id), select(id)), callback);

    let _ = selector(1);
    let _ = selector(1);

    expect(selectorCalls^) |> toBe(1);
  });

  test(
    "calling multi curried selector twice with the same input only executes callback once",
    () => {
      let selectorCalls = ref(0);
      let callback = (a, b, c) => {
        incr(selectorCalls);
        a + b + c;
      };
      let selector =
        create(
          // curry(curry(select(id), select(id)), select(id)),
          select(id) =>> select(id) =>> select(id),
          callback,
        );

      let _ = selector(1);
      let _ = selector(1);

      expect(selectorCalls^) |> toBe(1);
    },
  );

  test(
    "calling selector twice with different inputs executes callback twice", () => {
    let selectorCalls = ref(0);
    let callback = (a, b) => {
      incr(selectorCalls);
      a + b;
    };
    let selector = create(curry(select(id), select(id)), callback);

    let _ = selector(1);
    let _ = selector(2);

    expect(selectorCalls^) |> toBe(2);
  });

  test(
    "calling uncached selectors twice with the same inputs executes callback twice",
    () => {
      let selectorCalls = ref(0);
      let callback = (a, b) => {
        incr(selectorCalls);
        a + b;
      };
      let selector =
        create(uncached(curry(select(id), select(id))), callback);

      let _ = selector(1);
      let _ = selector(1);

      expect(selectorCalls^) |> toBe(2);
    },
  );
});
