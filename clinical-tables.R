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

# END