# c218Tools 0.2.1

## New features

* `cuteArt()`: print a cute ASCII art (cat / bear / rabbit / dog / panda) to the
  console. Just for fun / testing.

# c218Tools 0.2.0

## New features

* `gmTable()`: one-stop wrapper that turns a data frame (or a flextable) into a
  three-line academic table with a table name (caption) and footnote(s). In HTML
  R Markdown output the table is wrapped in a zoomable container with a toolbar
  (fit-to-width / slider / +- buttons / Ctrl + mouse wheel), so wide tables can
  be viewed as a whole and zoomed for detail instead of being trapped behind a
  horizontal scrollbar. Non-HTML output (Word / PDF) falls back to a plain
  static three-line table. A `theme` argument lets you skip the built-in styling
  when passing an already styled flextable.
* `zoomWrap()`: helper that wraps a flextable in the zoomable HTML container,
  exported for reuse.

## Bug fixes

* `addTableName()` now declares its `x` and `tableName` arguments and defaults
  `colwidths` to span all columns; it previously referenced undefined variables
  and errored on call.
* `formatTable()` now declares its `x`, `tableName`, `align` and `colwidths`
  arguments instead of relying on undefined variables.

## Dependencies

* Added `htmltools` to Imports (zoom container) and `knitr` to Suggests (output
  format detection).
