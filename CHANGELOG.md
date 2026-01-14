# Transducers

### 1.6.0 (2026-01-15)

#### Added

- Reducer: `ratio` for the percentage of items that pass a predicate.
- Reducer: `variance`, the result of which can be applied to `sqrt` to find Standard Deviation.
- Source: `chain` to compose any number of Generators.
- Source: The `generator` type and its functions have been exposed so that
  others can write their own Generators.

### 1.5.1 (2025-11-13)

#### Fixed

- `from-csv` now accounts for quotes, which ignore internal commas. Example:
  `Foo,"10,000",Bar`.

### 1.5.0 (2025-08-16)

#### Added

- Transducer: `sexp` for interpretting streams of chars or strings as separate
  SEXP strings.
- Transducer: `safe` for handling conditions within individual transducers. 
- Reducer: `quantities` for counting unique occurrences of streamed items.
- Reducer: `partition` for splitting the stream results.
- Reducer: `base-string` for reducing into a `simple-base-string`.
- Reducer: `bit-vector` for reducing into a `bit-vector`.
- Can now `transduce` over a `bit-vector` (e.g. `#*0101`).
- Can now `transduce` specially over `simple-string` and `simple-bit-vector`,
  but only with certain compilers.

#### Changed

- Reducer: `vector` and `average` are now faster and use less memory.
- Reducer: `string` is now more efficient and yields the specialized
  `(simple-array character (*))` type.
- `comp` is now a macro and uses less memory for long composition chains.
- All iterations are no longer implicitly wrapped in a `restart-case`. Doing so
  improves performance (2x in the case of SBCL, 10x in the case of ECL, etc.)
  and drastically reduces memory usage. As mentioned above, if you know that
  specific transducers may fail, you can now wrap them in `safe`.

#### Fixed

- A forgotten `all?` export.

### 1.4.0 (2025-02-15)

#### Added

- `unique-by` for more control over how uniqueness is determined.
- `for` as a better pattern for doing something effectful over the stream.
- `any?`, `all?`, and `reduced?` as modern aliases.

#### Deprecated

- `for-each`: use `for` instead.

### 1.3.1 (2025-01-13)

#### Added

- `reduced` as a better wrapper than calling the constructor `make-reduced` manually.

#### Fixed

- Purged usage of `uiop` which was causing problems downstream.
- `once` handles a `nil` argument better.
- Apply `(optimize (speed 3))` on all `*-reduce` functions.

### 1.3.0 (2024-11-02)

#### Added

- lib: The `median` reducer.
- lib: The `reversed` source to iterate from the end of a vector.

#### Changed

- `concatenate` and `flatten` now support vectors/strings.
- Dropped dependency on `fset` within the main lib.
- Licenses further relaxed to MPL.

### 1.2.0 (2024-07-20)

#### Added

- lib: The `find` reducer now accepts an optional `:default` value for when
  nothing could be found in the transduction that matched the predicate.
- fset: `transduce` implementations for `set`, `map`, `seq`, and `bag`.
- fset: Reducers for `set`, `map`, `seq`, and `bag`.

#### Changed

- lib: Conditions signalled somewhere in the transduction chain can now be
  caught per-item, with various restarts available to rehandle or skip the
  problematic item.
- jzon: plists now `write` as JS objects.

### 1.1.1 (2024-02-08)

#### Fixed

- `allp` and `anyp` now yield `t` in the case of success, and not just "non-nil".

### 1.1.0 (2024-02-02)

#### Added

- New condition: `no-transduce-implementation`

#### Changed

- If `transduce` is called on a type which has no `defmethod` for it, a custom
  condition is thrown with a more helpful error message.
- The JSON support system/package is now named `transducers/jzon` to avoid
  compiler warnings.

### 1.0.1 (2023-12-25)

Merry Christmas! I am jetlagged and writing this at 3:30 in the morning.

#### Fixed

- `for-each` now yields `t` instead of `nil`, since `nil` can interact poorly
  with various control structures.

### 1.0.0 (2023-09-07)

#### Added

- An `uncons` transducer for splitting up a stream of cons cells.
- A `from-csv` transducer to interpret a string stream as CSV data, splitting
  the values into a hash table.
- An `into-csv` transducer that, given some `headers`, reverses the stream of
  hash tables back into strings of CSV.
- A `snoc` reducer for reducing into a list without performing a final reverse.
- A `hash-table` reducer for reducing into a Hash Table. Requires the
  transduction items to be cons cells, with `car` being the key and `cdr` being
  the value.
- A `plist` source for transducing over key-value pairs in a Property List.
- New conditions:
  - `empty-argument`
  - `empty-transduction` (see below)
  - `imbalanced-plist`
  - `non-positive-integer`

#### Changed

- The library is now distributed under the terms of the LGPL to promote wider use.
- Transducing over a `hash-table` now yields the key-value pair as a cons cell,
  instead of just the value. The former behaviour is considered a bug.
- The `seed` argument of the `fold` reducer is now `&optional`. When missing,
  the first value that makes it through the transduction will be used as the
  seed. If the transduction turns out to be empty, then the condition
  `empty-transduction` will be raised.
- The `cons` reducer now uses `nreverse` internally. You better not have been
  saving the intermediate results anywhere!
- `segment`, `window`, and `step` now offer restarts when a bad initial value is
  passed.
- **BREAKING:** `average`, `first`, and `last` no longer accept a fallback
  value. Instead, a condition is raised when the transduction was empty. This
  means that calls that used to look like:

```common-lisp
(t:transduce #'t:pass (t:average 'whatever) '(1 2 3 4 5))
```

should now look like:

```common-lisp
(t:transduce #'t:pass #'t:average '(1 2 3 4 5))
```

Likewise for `first` and `last`.

#### Fixed

- List transduction fixed on Clasp. Clasp doesn't (seem to) support tail-call
  optimization, but it does support `labels`-based TCO. This is also critical
  for the `jzon` support.

#### Deprecated

- `max` and `min` have been deprecated in favour of calling `fold`. The seed
  value for `fold` is optional now, so:

```common-lisp
(t:transduce #'t:pass (t:fold #'max)    '(1 2 3)) ;; => 3
(t:transduce #'t:pass (t:fold #'max 10) '(1 2 3)) ;; => 10
(t:transduce #'t:pass (t:fold #'max 10) '())      ;; => 10
(t:transduce #'t:pass (t:fold #'max)    '())      ;; => Condition!
```

### 0.1.1 (2023-08-11)

#### Added

- A `once` transducer to inject a single value into any point in the
  transduction chain.
- A `for-each` reducer to ignore all results and just consume the stream for its
  side effects.

#### Deprecated

- `all` and `any` should now be called as `allp` and `anyp`. The old names have
  been marked deprecated and you will be warned at compile time.

### 0.1.0 (2023-03-04)

Initial version.
