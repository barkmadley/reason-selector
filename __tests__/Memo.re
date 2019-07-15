open Jest;

open Selector;

let id = x => x;

describe("Memo", () => {
  open Expect;

  test("create(select(id), id)(1) == 1", () => {
    let r = create(select(id), id);
    expect(r(1)) |> toBe(1);
  });

  test(
    "calling selector twice with the same input only executes callback once",
    () => {
    let selectorCalls = ref(0);
    let callback = param => {
      incr(selectorCalls);
      param;
    };
    let selector = create(select(id), callback);

    let _ = selector(1);
    let _ = selector(1);

    expect(selectorCalls^) |> toBe(1);
  });

  test(
    "calling selector twice with different inputs executes callback twice", () => {
    let selectorCalls = ref(0);
    let callback = param => {
      incr(selectorCalls);
      param;
    };
    let selector = create(select(id), callback);

    let _ = selector(1);
    let _ = selector(2);

    expect(selectorCalls^) |> toBe(2);
  });

  test(
    "calling uncached selectors twice with the same inputs executes callback twice",
    () => {
      let selectorCalls = ref(0);
      let callback = param => {
        incr(selectorCalls);
        param;
      };
      let selector = create(uncached(select(id)), callback);

      let _ = selector(1);
      let _ = selector(1);

      expect(selectorCalls^) |> toBe(2);
    },
  );
});
