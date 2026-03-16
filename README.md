# c218Tools
<!-- badges: start -->
<!-- badges: end -->

`c218Tools` tidies regression model output into academic paper-ready tables. It wraps `broom::tidy()` and formats results (β/OR with 95% CI, significance marks) directly into the layout you'd paste into a manuscript.

## Installation

You can install the development version of c218Tools from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Kingsman-Key/c218Tools")
```

## Core functions

| Function | What it does |
|---|---|
| `sumReg()` | Tidies a single fitted model into a one- or few-row result table |
| `regDisplay()` | Runs models in batch across multiple exposures and covariate sets |
| `genForm()` | Generates a list of model formulas with progressively added covariates |

---

## `sumReg()` — tidy a single model

`sumReg()` takes a fitted model and returns only the rows you care about — by default the first independent variable (your exposure of interest). It automatically detects whether the exposure is continuous or categorical and formats the result accordingly.

### Linear regression (`lm`)

``` r
library(c218Tools)

model <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
sumReg(model)
#>         term      betase.mark.latex
#> 1 Sepal.Width  0.60 (0.07) ^$\ast$^
```

### Logistic regression (`glm`)

``` r
data(mtcars)
model <- glm(am ~ cyl + hp + wt, data = mtcars, family = "binomial")
sumReg(model)
#>   term      or95.mark.latex
#> 1  cyl  0.59 (0.15, 2.35)
```

By default `sumReg()` returns a LaTeX-ready string with significance marks (`^$\ast$^` for p<0.05, `^$\dag$^` for p<0.01, `^$\ddag$^` for p<0.001). You can switch to plain numeric p-values or Excel-friendly asterisks:

``` r
# plain p-value columns
sumReg(model, pType = "value")

# asterisk marks for Excel / Word
sumReg(model, latex = FALSE, pType = "mark")
```

---

## `regDisplay()` — batch regression across exposures and covariate sets

In epidemiology papers it's common to report the same exposure under several progressively adjusted models (Model 1: crude; Model 2: + demographics; Model 3: + clinical variables). `regDisplay()` automates this entire workflow.

### Single exposure, multiple covariate sets

``` r
library(c218Tools)

# Model 1: Sepal.Length ~ Sepal.Width  (crude)
# Model 2: Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
# Model 3: Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species
regDisplay(
  outcome   = "Sepal.Length",
  exposure  = "Sepal.Width",
  covariate = list(c("Petal.Length", "Petal.Width"), c("Species")),
  data      = iris,
  regType   = "lm"
)
```

The output is a single row with columns for each model side by side — ready to copy into a manuscript table.

### Multiple exposures at once

When you have several candidate exposures, pass them as a vector. Each exposure gets its own row, each model gets its own column pair.

``` r
df <- iris
df$SW.binary <- factor(ifelse(df$Sepal.Width >= median(df$Sepal.Width), "high", "low"),
                       levels = c("low", "high"))

regDisplay(
  outcome   = "Sepal.Length",
  exposure  = c("Sepal.Width", "SW.binary"),
  covariate = list(c("Petal.Length", "Petal.Width"), c("Species")),
  data      = df,
  regType   = "lm"
)
```

### Logistic regression

``` r
data(mtcars)

regDisplay(
  outcome   = "am",
  exposure  = c("cyl", "hp"),
  covariate = list(c("wt"), c("gear")),
  data      = mtcars,
  regType   = "glm"
)
```

`regType` accepts `"lm"`, `"glm"`, `"coxph"`, `"multinom"`, and `"gee"`. The function detects binary outcomes automatically and adds `family = "binomial"` when needed.

---

## `genForm()` — generate model formulas

`genForm()` is the formula-building engine behind `regDisplay()`. You can call it directly to inspect or customise formulas before fitting.

``` r
genForm(
  outcome   = "Sepal.Length",
  exposure  = "Sepal.Width",
  covariate = list(c("Petal.Length", "Petal.Width"), c("Species"))
)
#> [1] "Sepal.Length~Sepal.Width"
#> [2] "Sepal.Length~Sepal.Width+Petal.Length+Petal.Width"
#> [3] "Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species"
```
