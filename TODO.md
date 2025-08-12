# to do

- bug: fix style leaking to button border
- static content: add api in mosaic for perstisted content
- bug: in non-alt screen, application still take the full height (scrolls down and hide content)
- perf: we clear Screen.t on each render because Ui doesn't clear content that's not used anymore, that's inneficient, ideally ui would clear what it needs to clear so we can have incremental rendering
- perf: we don't see to be re-rendering only what's needed, despite having dirty flag to our element tree in mosaic.ml

vcr
- should have padding
- cursor not moved after command output
- slow: takes 1s for excution of demo

---

- finish toffee port - tests pass
- fix vcr
- fix mosaic program rendering - tests pass
