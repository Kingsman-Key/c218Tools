# gmTable examples ------------------------------------------------------------
# In an HTML R Markdown chunk, simply call gmTable() as the last expression of
# the chunk. The table renders with a zoom toolbar (fit-to-width / slider /
# +- buttons / Ctrl + mouse wheel).

# 1. minimal: data frame + table name + footnote
gmTable(
  head(iris, 10),
  tableName = "Table 1. The iris dataset (first 10 rows)",
  footnote  = "Note. Data from Fisher (1936)."
)

# 2. multiple footnote lines
gmTable(
  mtcars,
  tableName = "Table 2. Motor Trend car road tests",
  footnote  = c("a. mpg = miles per gallon.",
                "b. Source: 1974 Motor Trend US magazine.")
)

# 3. feed an already styled flextable (e.g. from sumReg output)
# ft <- flextable::flextable(your_regression_table)
# gmTable(ft, tableName = "Table 3. Regression results", footnote = "* p < 0.05")

# 4. disable zoom (plain three-line flextable even in HTML)
gmTable(head(iris), tableName = "Table 4", zoom = FALSE)
