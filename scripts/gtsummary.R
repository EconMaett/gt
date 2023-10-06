# gtsummary ----

# The `gtsummary` package provides helpers to create publication-ready
# analytical and summary tables in R.

# The package leverages the `gt`, `broom`, and `labelled` packages.

library(gtsummary)


## Examples ----

### Summary table ----

# Use `tbl_summary()` to summarize a data frame

table1 <- trial |> 
  tbl_summary(include = c(age, grade, response))

print(table1)


# Use customization options to add information and format results.
table2 <- tbl_summary(
  trial,
  include = c(age, grade, response),
  by = trt, # split table by group
  missing = "no" # don't list missing data separately
) |> 
  add_n() |> # add column with total number of non-missing observations
  add_p() |> # test for a difference between groups
  modify_header(label = "**Variable**") |> # update column header
  bold_labels()


## Regression models ----

# Use `tbl_regression()` to easily display regression model results.
mod1 <- glm(response ~ trt + age + grade, trial, family = binomial)

t1 <- tbl_regression(mod1, exponentiate = TRUE)

print(t1)


## Side-by-side regression models ----

# Present side-by-side regression models with `tbl_merge()`.
library(survival)

# build survival model table
t2 <- coxph(Surv(ttdeath, death) ~ trt + grade + age, trial) |> 
  tbl_regression(exponentiate = TRUE)

# merge tables
tbl_merge_ex1 <- tbl_merge(
  tbls = list(t1, t2),
  tab_spanner = c("**Tumor Response**", "**Time to Death**")
)

print(tbl_merge_ex1)


## gtsummary + R Markdown ----

# The `gtsummary` is a companion to Posit's `gt` pacakge.
# Not all output types are supported by the `gt` package though.
# `gtsummary` can be pritned with various engines:
# - `gt`: `as_gt()`
# - `flextable`: `as_flex_table()`
# - `huxtable`: `as_hux_table()`
# - `kableExtra`: `as_kable_extra()`
# - `kable`: `as_kable()`
# - `tibble`: `as_tibble()`


## Save individual tables ----

# `gtsummary` tables can be saved directly as an image, HTML, Word, RTF,
# and LaTeX file.

# Use the syntax
if (FALSE) {
  tbl |> 
    as_gt() |> 
    gt::gtsave(filename = ".") # use extensions .png, .html, .docx, .rtf, .tex, .ltx
  
}

# END