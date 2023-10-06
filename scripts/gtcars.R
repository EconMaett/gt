# Case Study: gtcars ----

# Let's make a display table using the `gtcars` dataset.

# We all know `mtcars`... what is `gtcars`?

# It is a modernized `mtcars` for the gt age.

# It comes with the `gt` package.

library(tidyverse)
library(gt)

glimpse(gtcars)

# We reduce this 47-row tibble to one with only 8 rows

# Get a subset of 8 cars from the `gtcars` dataset:
# two from each manufacturer country of origin except the UK
gtcars_8 <- gtcars |> 
  group_by(ctry_origin) |> 
  slice_head(n = 2) |> 
  ungroup() |> 
  filter(ctry_origin != "United Kingdom")

# Show the `gtcars_8` tibble
glimpse(gtcars_8)

# We want to create a display table that fulfills the following 10 requirements:
# 1. putting the cars into characteristic groups (by the car manufacturer's country of origin).
# 2. removing some of the columns that we don't want to present.
# 3. incorporating some columns into a column group.
# 4. formatting the currency data using a monospaced font for easier reading.
# 5. giving the table a title and a subtitle.
# 6. adding footnotes.
# 7. placing a citation in the table footer.
# 8. transform the transmission (`trsmn`) codes
# 9. style some cells
# 10. highlight the cars considered *grand tourers*.


## Row groups ----

# Use `dplyr` to create groupings by the `ctry_origin` column with
# `dplyr::group_by()`.
gtcars_8 |> 
  group_by(ctry_origin) |> 
  gt()

# Use `dplyr`'s `arrange()` to bring the row groups in the preferred order.
gtcars_8 |> 
  group_by(ctry_origin) |> 
  arrange(mfr, desc(msrp)) |> 
  gt()

# We can use factor levels to get a more particular ordering within
# `arrange()`.

# Define the preferred order for `ctry_origin`
order_countries <- c("Germany", "Italy", "United States", "Japan")

# Reorder the table rows by our specific group ordering
gtcars_8 |> 
  arrange(
    factor(ctry_origin, levels = order_countries),
    mfr, 
    desc(msrp)
  ) |> 
  group_by(ctry_origin) |> 
  gt()

# We can combine the manufacturer name with the model name.

# Reorder the table rows by our specific ordering of groups
tab <- gtcars_8 |> 
  arrange(
    factor(ctry_origin, levels = order_countries),
    mfr, desc(msrp)
  ) |> 
  mutate(
    car = paste(mfr, model)
  ) |> 
  select(-mfr, - model) |> 
  group_by(ctry_origin) |> 
  gt(rowname_col = "car")

# Show the table
print(tab)


## Hiding and moving some columns ----

# Use `cols_hide()` to hide the columns `drivetrain` and `bdy_style`.

# Use a few `cols_*()` functions to hide and move columns
tab <- tab |> 
  cols_hide(columns = c(drivetrain, bdy_style)) |> 
  cols_move(
    columns = c(trsmn, mpg_c, mpg_h),
    after = trim
  )

# Show the table
print(tab)


## Putting columns into groups ----

# Use `tab_spanner()` to arrange columns into groups.

# Put the first three columns under a spanner column with the label "Performance"
tab <- tab |> 
  tab_spanner(
    label = "Performance",
    columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
  )

# Show the table
print(tab)


## Merging columns together and labeling them

# Use the `cols_merge()` function to combine the data from two columns
# into a single column.

# we want to combine the following pairs of columns

# - `mpg_c` and `mpg_h`
# - `hp` and `hp_rpm`
# - `trq` and `trq_rpm`

# The `cols_merge()` function needs the arguments `col_1` and `col_2`.
# Once combined, the `col_1` column will be retained and the `col_2` column is dropped.

# The `pattern` argument uses `{1}` and `{2}` to represent the content
# of `col_1` and `col_2`.

# We can use string literals to add text like `rpm` or the `@` sign.

# Because we are targeting an HTML table, we can use the `<br>` tag
# to insert a linebreak.

 # Inside the `pattern` argument, we can wrap columns with `<<` / `>>`.
# This is a special pattern syntax that tells `gt` to remove anything
# inside those double braces when there is an `NA` value.

# The `cols_label()` function makes relabeling possible and accepts
# a series of named arguments in the form of `<column_name> = <column_label>, ...`.

# Perform three column merges to better present MPG, HP, and torgue;
# relabel all the remaining columns
tab <- tab |> 
  cols_merge(
    columns = c(mpg_c, mpg_h),
    pattern = "<<{1}c<br>{2}h>>"
  ) |> 
  cols_merge(
    columns = c(hp, hp_rpm),
    pattern = "{1}<br>@{2}rpm"
  ) |> 
  cols_merge(
    columns = c(trq, trq_rpm),
    pattern = "{1}<br>@{2}rpm"
  ) |> 
  cols_label(
    mpg_c = "MPG",
    hp = "HP",
    trq = "Torque",
    year = "Year",
    trim = "Trim",
    trsmn = "Transmission",
    msrp = "MSRP"
  )

# Show the table
print(tab)


## Using formatter functions ----

# Use the family of `fmt_*()` formatter functions to format the `msrp` column
# to USD currency with no display of the currency sub units
tab <- tab |> 
  fmt_currency(columns = msrp, decimals = 0)

# Show the table
print(tab)


## Column alignment and style changes ----

# Center-align three columns in the gt table and modify the text size
# of a few columns of data
tab <- tab |> 
  cols_align(
    align = "center",
    columns = c(mpg_c, hp, trq)
  ) |> 
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = c(trim, trsmn, mpg_c, hp, trq)
    )
  )

# Show the table
print(tab)


## Text transformas -----

# Transform the column of text in `trsmn` using a custom funciton within
# `text_transform()`; 
# here `x` represents a character vector defined in the `cells_body()` function
tab <- tab |> 
  text_transform(
    locations = cells_body(columns = trsmn),
    fn = function(x) {
      
      # The first character of `x` always indicates the number of transmission speeds
      speed <- substr(x, 1, 1)
      
      # We can carefully determine which transmission type we have in `x`
      # with a `dplyr::case_when()` statement
      type <- case_when(
        substr(x, 2, 3) == "am" ~ "Automatic/Manual",
        substr(x, 2, 2) == "m" ~ "Manual",
        substr(x, 2, 2) == "a" ~ "Automatic",
        substr(x, 2, 3) == "dd" ~ "Direct Drive"
      )
      
      # Let's paste `speed` and `type` together to create HTML text replacing `x`
      paste(speed, " Speed<br><em>", type, "</em>")
    }
  )

# Show the table
print(tab)


## Table header: Title and subtitle ----

# Add a table title and subtitle; 
# we use markdown with the `md()` helper function
tab <- tab |> 
  tab_header(
    title = md("The Cars of **gtcars**"),
    subtitle = "These are some fine automobiles"
  )

# Show the table
print(tab)


## Adding a source citation ----

# Add a source note to the bottom of the table;
# this appears below the footnotes
tab <- tab |> 
  tab_source_note(
    source_note = md(
      "Source: Various pages within the Edmonds website."
    )
  )

# Show the table
print(tab)


## Using the complete `gtcars` table and adding footnotes ----

# Use `dplyr` functions to get the car with the best city gas mileage.
# this will be used to target the correct cell for a footnote
best_gas_mileage_city <- gtcars |> 
  arrange(desc(mpg_c)) |> 
  slice(1) |> 
  mutate(car = paste(mfr, model)) |> 
  pull(car)

print(best_gas_mileage_city)
# "BMW i8"

# Use `dplyr` to get the car with the highest horsepower.
# thiss will be used to target the correct cell for a footnote.
highest_horsepower <- gtcars |> 
  arrange(desc(hp)) |> 
  slice(1) |> 
  mutate(car = paste(mfr, model)) |> 
  pull(car)

print(highest_horesepower)
# "Ferrari LaFerrari"


# Define our preferred order for `ctry_origin`
order_countries <- c("Germany", "Italy", "United States", "Japan")

# Create a display table with `gtcars`, using all of the previous statements piped together
# and additional `tab_footnote()` statements
tab <- gtcars |>
  arrange(
    factor(ctry_origin, levels = order_countries),
    mfr, desc(msrp)
  ) |>
  mutate(car = paste(mfr, model)) |>
  select(-mfr, -model) |>
  group_by(ctry_origin) |>
  gt(rowname_col = "car") |>
  cols_hide(columns = c(drivetrain, bdy_style)) |>
  cols_move(
    columns = c(trsmn, mpg_c, mpg_h),
    after = trim
  ) |>
  tab_spanner(
    label = "Performance",
    columns = c(mpg_c, mpg_h, hp, hp_rpm, trq, trq_rpm)
  ) |>
  cols_merge(
    columns = c(mpg_c, mpg_h),
    pattern = "<<{1}c<br>{2}h>>"
  ) |>
  cols_merge(
    columns = c(hp, hp_rpm),
    pattern = "{1}<br>@{2}rpm"
  ) |>
  cols_merge(
    columns = c(trq, trq_rpm),
    pattern = "{1}<br>@{2}rpm"
  ) |>
  cols_label(
    mpg_c = "MPG",
    hp = "HP",
    trq = "Torque",
    year = "Year",
    trim = "Trim",
    trsmn = "Transmission",
    msrp = "MSRP"
  ) |>
  fmt_currency(columns = msrp, decimals = 0) |>
  cols_align(
    align = "center",
    columns = c(mpg_c, hp, trq)
  ) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(
      columns = c(trim, trsmn, mpg_c, hp, trq)
    )
  ) |>
  text_transform(
    locations = cells_body(columns = trsmn),
    fn = function(x) {
      
      speed <- substr(x, 1, 1)
      
      type <-
        dplyr::case_when(
          substr(x, 2, 3) == "am" ~ "Automatic/Manual",
          substr(x, 2, 2) == "m" ~ "Manual",
          substr(x, 2, 2) == "a" ~ "Automatic",
          substr(x, 2, 3) == "dd" ~ "Direct Drive"
        )
      
      paste(speed, " Speed<br><em>", type, "</em>")
    }
  ) |>
  tab_header(
    title = md("The Cars of **gtcars**"),
    subtitle = "These are some fine automobiles"
  ) |>
  tab_source_note(
    source_note = md(
      "Source: Various pages within the Edmonds website."
    )
  ) |>
  tab_footnote(
    footnote = md("Best gas mileage (city) of all the **gtcars**."),
    locations = cells_body(
      columns = mpg_c,
      rows = best_gas_mileage_city
    )
  ) |>
  tab_footnote(
    footnote = md("The highest horsepower of all the **gtcars**."),
    locations = cells_body(
      columns = hp,
      rows = highest_horsepower
    )
  ) |>
  tab_footnote(
    footnote = "All prices in U.S. dollars (USD).",
    locations = cells_column_labels(columns = msrp)
  )

# Show the table
print(tab)

# You can use this table in R Mrkdown, Shiny, email messages, ...
# wherever HTML is accepted!

# END