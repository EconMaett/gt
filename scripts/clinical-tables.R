# clinical tables ----

# The gt package contains the `rx_adsl` dataset that resembles the structure of a 
# common ADSL ADaM dataset for clinical trial data.

library(gt)

str(rx_adsl)


## Demographic Summary Tables ----

# In a first step, we use dplyr and tidyr to create a tibble with the shape
# of our desired table and then use gt functions to create the output table:

custom_summary <- function(df, group_var, sum_var) {
  
  group_var <- rlang::ensym(group_var)
  sum_var <- rlang::ensym(sum_var)
  
  is_categorical <- 
    is.character(eval(rlang::expr(`$`(df, !!sum_var)))) |
    is.factor(eval(rlang::expr(`$`(df, !!sum_var)))) 
  
  if (is_categorical) {
    
    category_lbl <- 
      sprintf("%s, n (%%)", attr(eval(rlang::expr(`$`(df, !!sum_var))), "label"))
    
    df_out <-
      df |>
      dplyr::group_by(!!group_var)  |> 
      dplyr::mutate(N = dplyr::n()) |> 
      dplyr::ungroup() |> 
      dplyr::group_by(!!group_var, !!sum_var) |> 
      dplyr::summarize(
        val = dplyr::n(),
        pct = dplyr::n()/mean(N),
        .groups = "drop"
      ) |> 
      tidyr::pivot_wider(
        id_cols = !!sum_var, names_from = !!group_var,
        values_from = c(val, pct)
      ) |> 
      dplyr::rename(label = !!sum_var) |> 
      dplyr::mutate(
        across(where(is.numeric), ~ifelse(is.na(.), 0, .)),
        category = category_lbl
      )
    
  } else {
    
    category_lbl <-
      sprintf(
        "%s (%s)",
        attr(eval(rlang::expr(`$`(df, !!sum_var))), "label"),
        attr(eval(rlang::expr(`$`(df, !!sum_var))), "units")
      )
    
    df_out <- 
      df |> 
      dplyr::group_by(!!group_var) |> 
      dplyr::summarize(
        n = sum(!is.na(!!sum_var)),
        mean = mean(!!sum_var, na.rm = TRUE),
        sd = sd(!!sum_var, na.rm = TRUE),
        median = median(!!sum_var, na.rm = TRUE),
        min = min(!!sum_var, na.rm = TRUE),
        max = max(!!sum_var, na.rm = TRUE),
        min_max = NA,
        .groups = "drop"
      ) |> 
      tidyr::pivot_longer(
        cols = c(n, mean, median, min_max),
        names_to = "label",
        values_to = "val"
      ) |> 
      dplyr::mutate(
        sd = ifelse(label == "mean", sd, NA),
        max = ifelse(label == "min_max", max, NA),
        min = ifelse(label == "min_max", min, NA),
        label = dplyr::recode(
          label,
          "mean" = "Mean (SD)",
          "min_max" = "Min - Max",
          "median" = "Median"
        )
      ) |> 
      tidyr::pivot_wider(
        id_cols = label,
        names_from = !!group_var,
        values_from = c(val, sd, min, max)
      ) |> 
      dplyr::mutate(category = category_lbl)
  }
  
  return(df_out)
}

adsl_summary <- 
  dplyr::filter(rx_adsl, ITTFL == "Y") |> 
  (\(data) purrr::map_df(
    .x = dplyr::vars(AGE, AAGEGR1, SEX, ETHNIC, BLBMI),
    .f = \(x) custom_summary(df = data, group_var = TRTA, sum_var = !!x)
  ))()

print(adsl_summary)


# We can now use the gt package to create a table from the tibble:
rx_adsl_tbl <- adsl_summary |> 
  gt(
    rowname_col = "label",
    groupname_col = "category"
  ) |> 
  tab_header(
    title = "x.x: Demographic Characteristics",
    subtitle = "x.x.x: Demographic Characteristics - ITT Analysis Set"
  )

print(rx_adsl_tbl)


## Response / Event Rate analysis Tables ----

# In another table, we summarize the number of subjects with an event per 
# intervention in the subgroup defined by the age groups.

# Within each intervention group, we are counting the number and percentage
# of participants with an event (`EVNTFL == "Y"`) as well as the total number
# of participants.

# The number of participants with an event divided by the number without an event are
# the odds of experiencing the event per study intervention.

# The odds ratio is then computed as the odds under Drug 1 divided by the odds
# under Placebo.

# The below code performs the calculation outlined above within the subgroup defined
# by `AAGEGR1`, where confidence intervals around the event rates are computed
# using the Clopper Pearson method.
rx_responders <- 
  rx_adsl |> 
  dplyr::filter(ITTFL == "Y") |> 
  dplyr::group_by(TRTA, AAGEGR1) |> 
  dplyr::summarize(
    n_resp = sum(EVNTFL == "Y"),
    n_total = dplyr::n(),
    pct = 100 * sum(EVNTFL == "Y") / dplyr::n(),
    ci_up = 100 * (
      1 + (dplyr::n() - sum(EVNTFL == "Y")) / (
        (sum(EVNTFL == "Y") + 1) * qf(
          0.975,
          2 * (sum(EVNTFL == "Y") + 1),
          2 * (dplyr::n() - sum(EVNTFL == "Y"))
        )
      )
    )^(-1),
    ci_low = ifelse(
      sum(EVNTFL == "Y") == 0,
      0,
      100 * (
        1 + (dplyr::n() - sum(EVNTFL == "Y") + 1) /
          (sum(EVNTFL == "Y") * qf(
            0.025,
            2 * sum(EVNTFL == "Y"),
            2 * (dplyr::n() - sum(EVNTFL == "Y") + 1)
          )
          )
      )^(-1)
    ),
    odds = sum(EVNTFL == "Y") / (dplyr::n() - sum(EVNTFL == "Y")),
    .groups = "drop"
  ) |> 
  tidyr::pivot_wider(
    id_cols = AAGEGR1,
    names_from = TRTA,
    values_from = c(n_resp, n_total, pct, ci_up, ci_low, odds)
  ) |> 
  dplyr::mutate(
    or = ifelse(
      odds_Placebo == 0,
      NA_real_,
      !! rlang::sym("odds_Drug 1") / odds_Placebo
    ),
    or_ci_low = exp(
      log(or) - qnorm(0.975) * sqrt(
        1 / n_resp_Placebo +
          1 / !!rlang::sym("n_resp_Drug 1") + 
          1 / (n_total_Placebo - n_resp_Placebo) + 
          1 / (!!rlang::sym("n_total_Drug 1") - !!rlang::sym("n_resp_Drug 1"))
      )
    ),
    or_ci_up = exp(
      log(or) + qnorm(0.975) * sqrt(
        1 / n_resp_Placebo + 
          1 / !!rlang::sym("n_resp_Drug 1") +
          1 / (n_total_Placebo - n_resp_Placebo) +
          1 / (!!rlang::sym("n_total_Drug 1") - !!rlang::sym("n_resp_Drug 1"))
      )
    )
  ) |> 
  dplyr::select(-tidyselect::starts_with("odds_"))

print(rx_responders)

# Let0s first create a basic gt table with a left-aligned table title and subtitle.
rx_resp_tbl <- rx_responders |> 
  gt() |> 
  tab_header(
    title = "x.x: Efficacy Data",
    subtitle = "x.x.x: Occurrence of Event per Subgroup - {gt} Analysis Set"
  ) |> 
  opt_align_table_header(align = "left")

print(rx_resp_tbl)


# Next, we format the columns for counts to integers with `fmt_integer()`,
# percentages and CI's around percentages as numbers with one decimal
# and odds ratio and the CI around the odds ratio as numbers with two decimals,
# in both cases using `fmt_number()`.
rx_resp_tbl <- rx_resp_tbl |> 
  fmt_integer(columns = dplyr::starts_with("n_")) |> 
  fmt_number(columns = dplyr::starts_with(c("pct_", "ci_")), decimals = 1) |> 
  fmt_number(columns = dplyr::starts_with("or"), decimals = 2)

rx_resp_tbl


# We can now merge the columns for participants with events, total number of participants
# and percentage of participants with events, as well as the 95% CIs around the
# event rate using `cols_merge()`.
# To indicate the intervention group, we are adding tab spanners with
# `tab_spanner()`.
rx_resp_tbl <- rx_resp_tbl |> 
  cols_merge(
    columns = c("n_resp_Placebo", "n_total_Placebo", "pct_Placebo"),
    pattern = "{1}/{2} ({3})"
  ) |> 
  cols_merge(
    columns = c("ci_low_Placebo", "ci_up_Placebo"),
    pattern = "[{1}, {2}]"
  ) |> 
  cols_merge(
    columns = c("ci_low_Drug 1", "ci_up_Drug 1"),
    pattern = "[{1}, {2}]"
  ) |> 
  cols_merge(
    columns = c("or_ci_low", "or_ci_up"),
    pattern = "[{1}, {2}]"
  ) |> 
  tab_spanner(
    label = "Drug 1",
    columns = c("n_resp_Drug 1", "ci_low_Drug 1")
  ) |> 
  tab_spanner(
    label = "Placebo",
    columns = c("n_resp_Placebo", "ci_low_Placebo")
  )

print(rx_resp_tbl)


# Let's group the two categories and highlight the fact that these are actually
# age subgroups.
# We use `tab_row_group()` to manually add a row group label *Age*.
rx_resp_tbl <- rx_resp_tbl |> 
  tab_row_group(
    label = "Age",
    rows = dplyr::everything()
  )

print(rx_resp_tbl)

# As we now have the `tab_row_group()` label in place, we no longer need
# the label for the first column and can assign and empty string.

# Also, because of the two tab spanners, we can assign equal column labels
# for event rates and 95% CIs in both intervention groups.
rx_resp_tbl <- rx_resp_tbl |> 
  cols_align(
    align = "center",
    columns = dplyr::starts_with(c("n_", "ci", "or"))
  ) |> 
  cols_label(
    .list = c(
      "AAGEGR1" = "",
      "n_resp_Placebo" = "Event Rate (%)",
      "ci_low_Placebo" = "[95% CI]",
      "n_resp_Drug 1" = "Event Rate (%)",
      "ci_low_Drug 1" = "[95% CI]",
      "or" = "Odds ratio",
      "or_ci_low" = "[95% CI]"
    )
  ) |> 
  cols_width(
    1 ~ px(80),
    dplyr::everything() ~ px(120)
  ) |> 
  cols_align(align = "left", columns = 1)

print(rx_resp_tbl)


# Finally, we make use of `tab_footnote()` and add a footnote to the columns
# with the 95% CIs around event rates, indicating that these were derived
# from the Clopper-Pearson method.
# To change the default symbol choice of `tab_footnote()` from numbers to letters,
# we add `tab_options(footnote.marks = letters)`.
rx_resp_tbl <- rx_resp_tbl |> 
  tab_footnote(
    footnote = "Event rate 95% exact confidence interval uses the Clopper-Pearson method.",
    locations = cells_column_labels(
      columns = c("ci_low_Placebo", "ci_low_Drug 1")
    ),
    placement = "right"
  ) |> 
  tab_options(footnotes.marks = letters)

print(rx_resp_tbl)


## Protocol Deviation Table ----

# For the summary table for protocol deviations (PDs) we use a second
# CDISC-flavored dataset, `rx_addv`. It contains a summary row, indicating
# if the IIT population in `rx_adsl` experienced at least one major PD or not.
# Individual PDs are summarized.
str(rx_addv)

# We first use dplyr and tidyr to crate a tidy data set
addv_sum <- rx_addv |> 
  dplyr::group_by(TRTA) |> 
  dplyr::mutate(
    NTOT = dplyr::n_distinct(USUBJID),
    .groups = "drop"
  ) |> 
  dplyr::group_by(TRTA, PARCAT1, PARAM, CRIT1FL) |> 
  dplyr::summarise(
    n = sum(AVAL, na.rm = TRUE),
    pct = 100 * sum(AVAL, na.rm = TRUE) / mean(NTOT),
    .groups = "drop"
  ) |> 
  tidyr::pivot_wider(
    id_cols = c(PARCAT1, PARAM),
    names_from = c(TRTA, CRIT1FL),
    values_from = c(n, pct)
  ) |> 
  dplyr::mutate(dplyr::across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) |> 
  dplyr::add_row(PARAM = "Subjects with at least:", .before = 1)

print(addv_sum)

# We start by exposing this dataset to `gt()` and add our usual left-aligned headers
addv_tbl <- addv_sum |> 
  gt(rowname_col = "PARAM") |> 
  tab_header(
    title = "xx.x: Demographic and Baseline Data",
    subtitle = "xx.x.x: Major Protocol Deviations and Relationship to COVID-19 - ITT Set"
  ) |> 
  opt_align_table_header(align = "left")

addv_tbl

# Next, we create a summary row for all individual PDs to get the overall
# number of individual PDs as well as the corresponding percentage.

# For this, we first create a row group for individual PDs using
# `tab_row_group()` applied to all rows where `PARCAT1` equals `PROTOCOL DEVIATIONS`.
# Then we arrange the order of the row groups to list individual PDs after
# the overall summary.
# Finally, we create a summary row with `summary_rows()` to sum up all
# columns with `n`'s and percentages.
addv_tbl <- addv_tbl |> 
  tab_row_group(
    label = " ",
    rows = PARCAT1 == "PROTOCOL DEVIATION"
  ) |> 
  row_group_order(groups = c(NA, " ")) |> 
  summary_rows(
    groups = " ",
    columns = where(is.numeric),
    fns = list(
      label = "Study Procedure Deviations",
      fn = "sum"
    ), 
    side = "top"
  )

print(addv_tbl)

# We only kept the column `PARCAT1` to facilitate the generation of the row group.
# We can hide this column with `cols_hide()`:
addv_tbl <- addv_tbl |> 
  cols_hide(columns = "PARCAT1")

print(addv_tbl)

# The table has roughly the right shape and we can start to format all
# numeric columns and merge columns for  `n`s and percentages by
# intervention group and COVID-19 relationship flag.
addv_tbl <- addv_tbl |> 
  sub_missing(
    rows = 1,
    missing_text = ""
  ) |> 
  fmt_number(
    columns = dplyr::starts_with("pct"),
    decimals = 1
  ) |> 
  cols_merge_n_pct(
    col_n = "n_Placebo_Y",
    col_pct = "pct_Placebo_Y"
  ) |> 
  cols_merge_n_pct(
    col_n = "n_Placebo_N",
    col_pct = "pct_Placebo_N"
  ) |> 
  cols_merge_n_pct(
    col_n = "n_Placebo_N",
    col_pct = "pct_Placebo_N"
  ) |> 
  cols_merge_n_pct(
    col_n = "n_Drug 1_Y",
    col_pct = "pct_Drug 1_Y"
  ) |> 
  cols_merge_n_pct(
    col_n = "n_Drug 1_N",
    col_pct = "pct_Drug 1_N"
  )

print(addv_tbl)

# We can now modify the column names and create a cascade of column spanners.
addv_tbl <- addv_tbl |> 
  tab_spanner(
    label = md("COVID-19 Related"),
    columns = c("n_Placebo_Y", "n_Placebo_N"),
    id = "cov_pla"
  ) |> 
  tab_spanner(
    label = md("COVID-19 Related"),
    columns = c("n_Drug 1_Y", "n_Drug 1_N"),
    id = "cov_dru"
  ) |> 
  tab_spanner(
    label = md("Placebo \n N=90 (100%) \n n (%)"),
    columns = c("n_Placebo_Y", "n_Placebo_N")
  ) |> 
  cols_label(
    .list = list(
      "n_Placebo_Y" = "Yes",
      "n_Placebo_N" = "No",
      "n_Drug 1_Y" = "Yes",
      "n_Drug 1_N" = "No"
    ) 
  )|> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_spanners(spanners = dplyr::everything())
  )

print(addv_tbl)

# We can now add a footnote, indicating that subjects can haave more than one
# PD during the course of the study.

# The footnote is added with `tab_footnote()` to the row
# "At least one major Protocol Deviation"
addv_tbl <- addv_tbl |> 
  tab_footnote(
    footnote = "Subjects can have more than one Protocol Deviation throughout the study.",
    locations = cells_stub(rows = c("At least one major Protocol Deviation")),
    placement = "right"
  )

print(addv_tbl)

# Finally, we can style the table, indenting the individual PDs under 
# "Study Procedure Deviations", left-aligning the first columns and
# centering all other columns.
# Note that for the indentation, we can still use the hidden
# column `PARCAT1` to identify individual PDs.
addv_tbl |> 
  cols_align(
    align = "center",
    columns = 3:6
  ) |> 
  cols_align(
    align = "left",
    columns = 1:2
  ) |> 
  tab_stub_indent(
    rows = PARCAT1 == "PROTOCOL DEVIATION",
    indent = 5
  )

# END