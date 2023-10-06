# Plotting with gtExtras ----

## Graphs vs tables ----

# Per Stephen Few in his book, "Show Me the Numbers",
# the difference between tables and graphs:

#   Tables: Display used to look up and compare individual values
#   Graphs: Display used to reveal relationships among whole sets of values and their overall shape.


## Get started ----
library(tidyverse)
library(gt)
library(gtExtras)


## Sparklines ----

# A sparkline is a small line chart, typically drawn without axes or coordinates.
# It presents the general shape of the variation of some measurement over time,
# such as a temperature or stock market price.

# We can use `gtExtras::gt_plt_sparkline()` to add an inline sparkline.
# First we need to convert the data from long format to a summarized wide format,
# where each row represents one group and the column is a vector of values.
mtcars |> 
  head()

# By using `summarize(list_data = list(col_name))` we can create a list-column
# of ALL the data for that group.
car_summary <- mtcars |> 
  group_by(cyl) |> 
  summarize(
    mean = mean(mpg),
    sd = sd(mpg),
    # must end up with a list of data for each row in the input data
    mpg_data = list(mpg),
    .groups = "drop"
  )

print(car_summary)

car_summary |> 
  arrange(desc(cyl)) |> 
  gt() |> 
  gtExtras::gt_plt_sparkline(mpg_data) |> 
  fmt_number(columns = mean:sd, decimals = 1)


## Sparkline alternatives ----

# You can also plot a density-plot or histogram instead of a line plot.
# We switch to `gt_plt_dist()` for that.
car_summary |> 
  arrange(desc(cyl)) |> 
  gt() |> 
  gtExtras::gt_plt_dist(mpg_data, type = "density", line_color = "blue", fill_color = "red") |> 
  fmt_number(columns = mean:sd, decimals = 1)

car_summary |> 
  arrange(desc(cyl)) |> 
  gt() |> 
  gtExtras::gt_plt_dist(mpg_data, type = "histogram", line_color = "blue", fill_color = "green", bw = 4) |> 
  fmt_number(columns = mean:sd, decimals = 1)


## Inline bars ----

# You can generate `ggplot2` bar plots inline.
# `keep_column` allows you to keep the raw values in the plot inline.
mtcars |> 
  select(cyl:wt, mpg) |> 
  head() |> 
  gt() |> 
  gt_plt_bar(column = mpg, keep_column = TRUE, width = 35)


## Percent bars ----

# Alternatively, you may prefer the HTML-generated bar plots from 
# `gt_plt_bar_pct()`.
# Note that the bars represent a percentage of the total so that
# the largest value will indicate 100% of the row.
# You can pass raw values that are scaled to a 0-100% range,
# or pass values between 0 and 100 that represent an existing percentage.
mtcars |> 
  head() |> 
  select(cyl, mpg) |> 
  mutate(
    mpg_pct_max = round(mpg / max(mpg) * 100, digits = 2),
    mpg_scaled = mpg / max(mpg) * 100
  ) |> 
  mutate(mpg_unscaled = mpg) |> 
  gt() |> 
  gt_plt_bar_pct(column = mpg_scaled, scaled = TRUE) |> 
  gt_plt_bar_pct(column = mpg_unscaled, scaled = FALSE,
                 fill = "blue", background = "lightblue") |>
  cols_align(align = "center", columns = contains("scale")) |> 
  cols_width(
    4 ~ px(125),
    5 ~ px(125)
  )


## Inline Win-Loss plots ----

# Generate "Winn-Loss" plots similar to how The Guardian reports Soccer outcomes.
# Use the `nflreadr` package to get NFL data.
games_df <- nflreadr::load_schedules() |> 
  filter(season == 2020, game_type == "REG") |> 
  select(game_id, team_home = home_team, team_away = away_team, result, week) |> 
  pivot_longer(
    cols = contains("team"), 
    names_to = "home_away",
    values_to = "team",
    names_prefix = "team_"
    ) |> 
  mutate(
    result = ifelse(home_away == "home", result, -result),
    win = ifelse(result == 0, 0.5, ifelse(result > 0, 1, 0))
  ) |> 
  select(week, team, win) |> 
  mutate(
    team = case_when(
      team == "STL" ~ "LA",
      team == "OAK" ~ "LV",
      team == "SD" ~ "LAC",
      TRUE ~ team
    )
  )


team_df <- nflreadr::load_teams() %>%
  select(team_wordmark, team_abbr, team_conf, team_division)


joined_df <- games_df %>%
  group_by(team) %>%
  summarise(
    Wins = length(win[win==1]),
    Losses = length(win[win==0]),
    outcomes = list(win), .groups = "drop") %>%
  left_join(team_df, by = c("team" = "team_abbr")) %>%
  select(team_wordmark, team_conf, team_division, Wins:outcomes)


final_df <- joined_df %>%
  filter(team_conf == "AFC") %>%
  group_by(team_division) %>%
  arrange(desc(Wins)) %>%
  ungroup() %>%
  arrange(team_division) %>%
  select(-team_conf) %>%
  mutate(team_division = stringr::str_remove(team_division, "AFC |NFC ")) %>%
  mutate(
    team_division = factor(team_division,
                           levels = c("North", "South", "East", "West")
    )
  ) %>%
  arrange(team_division)

glimpse(final_df)


final_df %>%
  gt(groupname_col = "team_division") %>%
  cols_label(team_wordmark = "") %>%
  cols_align("left", team_division) %>%
  gtExtras::gt_plt_winloss(outcomes, max_wins = 16, type = "pill") %>%
  gtExtras::gt_img_rows(columns = team_wordmark, height = 20) %>%
  gtExtras::gt_theme_538() %>%
  tab_header(
    title = gtExtras::add_text_img(
      "2020 Results by Division",
      url = "https://github.com/nflverse/nflfastR-data/raw/master/AFC.png",
      height = 30
    )
  ) %>%
  tab_options(data_row.padding = px(2))


## Inline bar plots ----

# We can do inline bar plots purely with HTML.
gt_bar_plot_tab <- mtcars |> 
  head() |> 
  select(cyl, mpg) |> 
  mutate(
    mpg_pct_max = round(mpg / max(mpg) * 100, digits = 2),
    mpg_scaled = mpg / max(mpg) * 100
  ) |> 
  mutate(
    mpg_unscaled = mpg
  ) |> 
  gt() |> 
  gtExtras::gt_plt_bar_pct(
    column = mpg_scaled,
    scaled = TRUE
  ) |> 
  gtExtras::gt_plt_bar_pct(
    column = mpg_unscaled,
    scaled = FALSE,
    fill = "blue",
    background = "lightblue"
  ) |> 
  cols_align(align = "center", columns = contains("scale")) |> 
  cols_width(
    4 ~ px(125),
    5 ~ px(125)
  )

print(gt_bar_plot_tab)


## Stacked percent bar chart ----

# We can create a horizontal stacked percent bar chart inline like so.

### Data preparation ----
player_df <- tibble(
  player = c(
    "Evan Mobley",
    "Sandro Mamukelashvili",
    "Charles Bassey",
    "Luke Garza",
    "Moses Wright",
    "Neemias Queta",
    "Isaiah Jackson",
    "Day'Ron Sharpe"
  ),
  team = c(
    "USC", "Seton Hall", "Western Kentucky",
    "Iowa", "Georgia Tech", "Utah St", "Kentucky",
    "North Carolina"
  ),
  ht = c(
    "7'0\"",
    "6'10\"",
    "6'10\"",
    "6'11\"",
    "6'9\"",
    "7'1\"",
    "6'11\"",
    "6'10\""
  ),
  dk_pct_time = c(40, 48, 50, 50, 51, 55, 60, 66),
  dk_pps = c(1.62, 1.02, 1.54,1.33,1.46,1.37,1.33,1.18),
  tip_pct_time = c(26, 10, 19, 15, 25, 27, 15, 24),
  tip_pps = c(0.88, .97,1,1.05, .63, .85, .76, .84),
  jmp_pct_time = c(33, 42, 31, 35, 25, 18, 25, 10),
  jmp_pps = c(.91, .91, .78, 1.04, .86, .74, .71, .42)
) %>%
  left_join(
    tibble(
      player = c(
        "Evan Mobley",
        "Sandro Mamukelashvili",
        "Charles Bassey",
        "Luke Garza",
        "Moses Wright",
        "Neemias Queta",
        "Isaiah Jackson",
        "Day'Ron Sharpe"
      ) %>% rep(each = 3),
      shot_type = c("Dunks + Lays", "Hooks + Floats", "Jumpers") %>% rep(8)
    ) %>%
      mutate(
        shot_type = factor(shot_type, levels = c("Jumpers", "Hooks + Floats", "Dunks + Lays")),
        shot_mix = c(
          40, 26, 33,
          48, 10, 42,
          50, 19, 31,
          50, 15, 35,
          51, 25, 25,
          55, 27, 18,
          60, 15, 25,
          66, 24, 10
        )
      ),
    by = "player"
  )

### Plotting ----
basic_tb <- player_df |> 
  group_by(player) |> 
  summarise(dunks = shot_mix[1], list_data = list(shot_mix)) |> 
  arrange(dunks) |> 
  gt()

basic_tb |> 
  gtExtras::gt_plt_bar_stack(
    list_data, width = 65,
    labels = c("DUNKS", "HOOKS/FLOATS", "JUMPERS"),
    palette = c("#ff4343", "#bfbfbf", "#0a1c2b")
    ) |> 
  gtExtras::gt_theme_538()


## Bullet chart ----

# Bullet charts represent a core value and a target metric.

set.seed(37)

bullet_df <- rownames_to_column(mtcars) |> 
  select(rowname, cyl:drat, mpg) |> 
  group_by(cyl) |> 
  mutate(target_col = mean(mpg)) |> 
  slice_sample(n = 3) |> 
  ungroup()

bullet_df |> 
  gt() |> 
  gtExtras::gt_plt_bullet(
    column = mpg,
    target = target_col,
    width = 45,
    palette = c("lightblue", "black")
  )

# If you want to use `gt::fmt_*()` functions on your `column` of interest,
# you need to create a duplicate column ahead of time:
bullet_df |> 
  mutate(plot_column = mpg) |> 
  gt() |> 
  gtExtras::gt_plt_bullet(
    column = plot_column,
    target = target_col,
    width = 45
  ) |> 
  fmt_number(mpg, decimals = 1)

# END