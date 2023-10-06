# Introduction ----

library(gt)
library(tidyverse)
## A Walkthrough of the **gt*3 Basics with a Simple Table ----

# Let's use a less common dataset that is available in the R **datasets** package:
# `islands`.
# It's actually not a data frame but a named vector
head(islands)


# We can use the **dplyr** and prepare a tibble from it:

# Take the `islands` dataset and use some dplyr functionality to obtain
# the ten biggest islands in the world
islands_tbl <- tibble(
  name = names(islands),
  size = islands
) |> 
  arrange(desc(size)) |> 
  slice(1:10)

# Display the table
print(islands_tbl)


# Given that `islands_tbl` is a tibble, we now have a suitable input for gt.

# The main entry point into the gt API is the `gt()` function. If we pass
# `islands_tbl` to the *function* `gt()`, we'll get a **gt Table** as output.

# As an aside, we could have easily used a data frame instead as valid **Table Data** for gt.

# Create a display table showing ten of the largest islands in the world
gt_tbl <- gt(islands_tbl)

# Show the gt Table
gt_tbl

class(gt_tbl)
# "gt_tbl", "list"

# Oftentimes you'll want a **Table header**,
# a **Stub**, and sometimes *footnotes** and
# *source* notes in the **Table Footer** part.


## Adding parts to this simple table ----

# The gt package makes it easy to add parts so that the resulting
# gt Table better conveys the information you want to present.

# The previous gt Table had only two parts:
# - The Column Lables
# - The Table Body


# The parts of a gt Table are:

# - The Table Header, optionally with a Title and possibly a Subtitle
# - The Stub and the Stub Head, optionally containing row labels, possibly within row groups
# - The Column Labels
# - The Table Body, contains columns and rows of cells
# - The Table Footer, possibly with footnotes and source notes


# We add parts like the Table Header and footnotes in the Table Footer
# using the `tab_*()` family of functions.

# A Table Header is easy to add with the `tab_header()` function:

# Make a display table with the `islands_tbl` table;
# put a heading just above the column labels
gt_tbl <- gt_tbl |> 
  tab_header(
    title = "Large Landmasses of the World",
    subtitle = "The top ten largest are presented"
  )

# Show the gt Table
gt_tbl


# The Header table part provides an opportunity to describe the data that's presented.
# We may also *style* the `title`and `subtitle` using Markdown!
# We do this by wrapping the values passed to the `title` and `subtitle`
# arguments of the `tab_header()` function in the `md()` function.

# Use markdown for the heading's `title` and `subtitle` to add
# bold and italicized characters
gt(islands_tbl[1:2, ]) |> 
  tab_header(
    title = md("**Large Landmasses of the World**"),
    subtitle = md("The *top two* largest are presented")
  )


# A **source note** can be added to the table's **footer** through use of the
# `tab_source_note()` function.
# It can be called multiple times - each invocation results in the addition
# of a source note.

# Display the `islands_tbl` data with a heading and two source notes
gt_tbl <- gt_tbl |> 
  tab_source_note(
    source_note = "Source: The World Alamanc and Book of Facts, 1975, page 406."
  ) |> 
  tab_source_note(
    source_note = "Reference: McNeil, D. R. (1977) *Interactive Data Analysis*. Wiley."
  )

# Show the gt Table
print(gt_tbl)


# Footnotes live inside the **Footer** part and their footnote marks are
# attached to cell data.

# Footnotes are added with the `tab_footnote()` function.

# The helper function `cells_body()` can be used with the `location` argument
# to specify which data cells should be the target of the footnote.

# `cells_body()` has two arguments, `columns` and `rows`.
# For each of these, we can supply 
#   (1) a vector of colnames or rownames
#   (2) a vector of column/row indices
#   (3) bare column names wrapped in `c()` or row labels within `c()`
#   (4) a select helper function like `starts_with()`, `ends_with()`, and `everything()`

# For `rows` specifically, we can use a conditional statement with column names,
# e.g. `size > 15000`.


# Here is a simple example of how footnotes can be added to a table cell.
# Let's add a footnote that references the `North America` and `South America` cells 
# in the `name` column:

# Add footnotes (the same text) in two different cells;
# data cells are targeted with `data_cells()`
gt_tbl <- gt_tbl |> 
  tab_footnote(
    footnote = "The Americas.",
    locations = cells_body(columns = name, rows = 3:4)
  )

# Show the gt table
print(gt_tbl)


# A slightly more complex example of adding footnotes uses expressions
# in the `rows` argument of `cells_body()`.

# A set of **dplyr** verbs are used to obtain the name of the "island" by largest landmass.

# This is assigned to the `largest` object ("Asia").

# The second `tab_footnote()` is similar, except we are supplying a conditional
# statement that gets the lowest population.

# Determine the row that contains the largest landmass ('Asia')
largest <- islands_tbl |> 
  arrange(desc(size)) |> 
  slice(1) |> 
  pull(name)
largest
# "Asia"

# Create two additional footnotes, using the `columns` and `where` arguments
# of `data_cells()`
gt_tbl <- gt_tbl |> 
  tab_footnote(
    footnote = md("The **largest** by area."),
    locations = cells_body(
      columns = size,
      rows = name == largest
    )
  ) |> 
  tab_footnote(
    footnote = "The lowest by population.",
    locations = cells_body(
      columns = size,
      rows = size == min(size)
    )
  )

# Show the gt table
print(gt_tbl)


## The Stub ----

# The **Stub** is the area to the left in a table that contains *row labels*,
# and may contain *row group labels*, and *summary labels*.

# Those subparts can be grouped in a sequence of *row groups*.

# The **Stub Head** provides a location for a label that describes the **Stub**.

# The **Stub** is optional since there are cases where a **Stub** wouldn't be useful.


# We specify a stub column in the `gt()` function with the `rowname_col` argument.

# This will signal to **gt** that the named column should be used as the stub,
# making *row labels*.


# Create a gt table showing ten of the largest islands in the world;
# this time with a stub
gt_tbl <- islands_tbl |> 
  gt(rowname_col = "name")

# Show the gt table
print(gt_tbl)

# We can apply a **stubhead label**
gt_tbl <- gt_tbl |> 
  tab_stubhead(label = "landmass")

# Show the gt table
print(gt_tbl)

# To apply our table parts as before, up to and including the footnotes,
# we use the following statements:

gt_tbl <- gt_tbl |> 
  tab_header(
    title = "Large Landmasses of the World",
    subtitle = "Teh top ten largest are presented"
  ) |> 
  tab_source_note(
    source_note = "Source: The World Almanac and Book of Facts, 1975, page 406."
  ) |> 
  tab_source_note(
    source_note = md("Reference: McNeil, D. R. (1977) *Interactive Data Analysis*. Wiley.")
  ) |> 
  tab_footnote(
    footnote = md("The **largest*3 by area."),
    locations = cells_body(
      columns = size, rows = largest
    )
  ) |> 
  tab_footnote(
    footnote = "The lowest by population.",
    locations = cells_body(
      columns = size, rows = contains("arc")
    )
  )

# Show the gt table
print(gt_tbl)


# We can incorporate row groups into the display table. 
# This divides rows into groups, creating *row groups*,
# resulting *row group labels* right above each group.

# We make a new *row group* with each call of `tab_row_group()`.

# The inputs are row group names in the `label` argument.

# Create three row groups with the `tab_row_group()` function
gt_tbl <- gt_tbl |> 
  tab_row_group(
    label = "continent",
    rows = 1:6
  ) |> 
  tab_row_group(
    label = "country",
    rows = c("Australia", "Greenland")
  ) |> 
  tab_row_group(
    label = "subregion",
    rows = c("New Guinea", "Borneo")
  )

# Show the gt table
print(gt_tbl)


## The Column Labels ----

# To better demonstrate how Column Labels are displayed, we use the
# `airquality` dataset. It has the following columns:
# - `Ozone`: mean ground-level ozone in parts per billion by volume (ppbV), measured between 13:00 and 15:00
# - `solar.R`: solar radiation in Langley units (cal/m^2), measured between 08:00 and noon
# - `Wind`: mean wind speed in miles per hour (mph)
# - `Temp`: maximum daily air temperature in degrees Fahrenheit (F)
# - `Month`, `Day`: the numeric month and day of month for the record

# We know that all measurements took place in 1973, so a `year` column will be
# added to the dataset before it is passed to `gt()`.

# We organize the time information under a `Time` *spanner column label*,
# and put the other columns under a `Measurement` *spanner column label*.

# Modify the `airquality` dataset by adding the year of the measurements (1973)
# and limiting to 10 rows
airquality_m <- airquality |> 
  mutate(Year = 1973L) |> 
  slice(1:10)

# Create a display table using the `airquality` dataset;
# arrange columns into groups
gt_tbl <- gt(airquality_m) |> 
  tab_header(
    title = "New York Air Quality Measurements",
    subtitle = "Daily measurements in New York City (May 1-10, 1973)"
  ) |> 
  tab_spanner(
    label = "Time",
    columns = c(Year, Month, Day)
  ) |> 
  tab_spanner(
    label = "Measurement",
    columns = c(Ozone, Solar.R, Wind, Temp)
  )

# Show the gt table
print(gt_tbl)


# To make the table more presentable, we move the `Time` columns to the
# beginning of the series, using `cols_move_to_start()` and we
# customize the column labels so that they are more descriptive,
# using `cols_label()`
gt_tbl <- gt_tbl |> 
  cols_move_to_start(
    columns = c(Year, Month, Day)
  ) |> 
  cols_label(
    Ozone = html("Ozone,<br>ppbV"),
    Solar.R = html("Solar R.,<br>cal/m<sup>2</sup>"),
    Wind = html("Wind,<br>mph"),
    Temp = html("Temp,<br>&deg;F")
  )

# Show the gt table
print(gt_tbl)

# END