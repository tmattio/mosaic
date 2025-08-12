# to do

mosaic

- bug: fix style leaking to button border
- static content: add api in mosaic for perstisted content
- bug: in non-alt screen, application still take the full height (scrolls down and hide content)
- perf: we clear Screen.t on each render because Ui doesn't clear content that's not used anymore, that's inneficient, ideally ui would clear what it needs to clear so we can have incremental rendering
- perf: we don't see to be re-rendering only what's needed, despite having dirty flag to our element tree in mosaic.ml

---

vcr
- fix jqp example

---

- finish toffee port - tests pass
- fix mosaic program rendering - tests pass
