# reason-selector

GADT based reimplementation of [reselect](https://github.com/reduxjs/reselect) in [ReasonML](https://reasonml.github.io/).

[![npm](https://img.shields.io/npm/v/reason-selector.svg)](https://npmjs.org/reason-selector)
[![Issues](https://img.shields.io/github/issues/barkmadley/reason-selector.svg)](https://github.com/barkmadley/reason-selector/issues)
[![Last Commit](https://img.shields.io/github/last-commit/barkmadley/reason-selector.svg)]()

`reason-selector` is a re-imagining of the types used by reselect, such that
the number of `select`s you pass to the `create` function has to equal the number
of parameters of the callback given to `create`. The benefit is that it is not
necessary to create verbose/redundant/error prone repetition in the bindings.
It achieves this by using GADTs to generate the expected callback function type
from the composition of the `select`s you provide.

The implementation is pure ReasonML, so this library could in theory be used by
projects targeting native compilation targets.

## Example

```reason
type item = {
  name: string,
  value: float,
};
type shop = {
  taxPercent: float,
  items: array(item),
};
type state = {shop};

let shopItemsSelector = state => state.shop.items;
let taxPercentSelector = state => state.shop.taxPercent;

let subtotalSelector =
  Selector.(
    create(
      select(shopItemsSelector),
      items => Array.fold_left((acc, item) => acc +. item.value, 0.0, items)
    )
  );

let taxSelector =
  Selector.(
    create(
      select(subtotalSelector) =>> select(taxPercentSelector),
      (subtotal, taxPercent) => subtotal *. (taxPercent /. 100.0)
    )
  );

type total = {total: float};

let totalSelector =
  Selector.(
    create(
      select(subtotalSelector) =>> select(taxSelector),
      (subtotal, tax) => {total: subtotal +. tax}
    )
  );

let exampleState = {
  shop: {
    taxPercent: 8.0,
    items: [|{name: "apple", value: 1.20}, {name: "orange", value: 0.95}|],
  },
};

Js.log(subtotalSelector(exampleState)); // 2.15
Js.log(taxSelector(exampleState)); // 0.172
Js.log(totalSelector(exampleState)); // { total: 2.322 }
```

## Installation

```sh
npm install --save reason-selector
```

Then add `reason-selector` to `bs-dependencies` in your `bsconfig.json`:

```js
{
  ...
  "bs-dependencies": ["reason-selector"]
}
```

## Usage

See usage examples in [`the tests`](https://github.com/barkmadley/reason-selector/blob/master/__tests__). The source is a [single file](https://github.com/barkmadley/reason-selector/blob/master/src/Selector.re) that implements the simple Cache module and the selector `create` and composition operators `select` and `=>>`.

## Changes

### 0.2.0

- Enforce the reverse Cons structure by using another type parameter and phantom types

### 0.1.0

- Initial release
