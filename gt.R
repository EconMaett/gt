# gt ----

# With the gt package, anyone can make wonderful-looking tables using the R programming
# language.

# The gt philosophy: we can construct a wide variety of useful tables with
# a cohesive set of table parts.

# These include the
# - table header
# - stub
# - column labels
# - spanner column labels
# - table body
# - table footer

# It all begins with table data, be it a tibble or a data frame.

# You then decide how to compose your gt table with the elements
# and formatting you need for the task at hand.

# Finally, the table is rendered by printing it at the console,
# including it in an R Markdown document, or exporting to a file
# using `gtsave()`.

# Currently, gt supports the HTML, LaTeX, and RTF output formats.


# Here is a brief example of how to use gt to crate a table from the included
# `sp500` data set:
library(gt)

# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table based on preprocessed `sp500` table data
sp500 |> 
  dplyr::filter(date >= start_date & date <= end_date) |> 
  dplyr::select(-adj_close) |> 
  gt() |> 
  tab_header(
    title = "S&P 500",
    subtitle = glue::glue("{start_date} to {end_date}")
  ) |> 
  fmt_currency() |> 
  fmt_date(columns = date, date_style = "wd_m_day_year") |> 
  fmt_number(columns = volume, suffixing = TRUE)

# There are ten datasets provided by gt:
# `countrypops`, `sza`, `gtcars`, `sp500`, `pizzaplace`, `exibble`, `towny`,
# `metro`, `rx_adsl`, and `rx_addv`.

# There are many functions available in gt for creating super-customized tables.
# Check out the documentation website to get started via introductory articles
# for making gt tables.
# https://gt.rstudio.com/


# With the gt *Test Drive*, you can try gt in the *Posit Cloud* environment
# that features the RStudio IDE and a large collection of ready-to-run examples.

# There is no charge to use this platform!

# END