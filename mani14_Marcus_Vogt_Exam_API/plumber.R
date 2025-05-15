# ===================================================================
# Plumber API: Superliga Player Stats 2024/2025
# ===================================================================

library(plumber)
library(readxl)
library(dplyr)
library(jsonlite)
library(stringr)
library(ggplot2)

# ---------------------------
# Load and clean data once
# ---------------------------

# Læs Excel-data (filen skal ligge i samme mappe som denne .R-fil)
football_data_raw <- readxl::read_excel("football_stats.xlsx")

# List of known Danish Superliga teams (2024/2025)
superliga_teams <- c(
  "København", "Brøndby", "Midtjylland", "AGF", "Nordsjælland",
  "Viborg", "Vejle", "AaB", "Silkeborg", "Randers",
  "Lyngby", "OB", "Sønderjyske", "SønderjyskE"
)

# Definér relevante kolonner
metrics <- c("xG per 90", "xA per 90", "Touches in box per 90")
team_col <- "Current team"

# Filter players to only include non-U19 players in current Superliga teams
# Function to normalize columns
normalize_column <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

football_data <- football_data_raw |>
  filter(
    `Current team` %in% superliga_teams,
    !grepl("U19", `Current team`, ignore.case = TRUE)
  ) |>
  mutate(across(all_of(metrics), as.numeric)) |>
  mutate(across(all_of(metrics), ~ normalize_column(.x))) |>
  rowwise() |>
  mutate(total_score = mean(c_across(all_of(metrics)), na.rm = TRUE)) |>
  ungroup()

# One player has "Sønderjyske" spelled "SønderjyskE"
football_data <- football_data |>  mutate(`Current team` = ifelse(`Current team` == "SønderjyskE", "Sønderjyske", `Current team`))

# write_csv(football_data, "football_data.csv")

# Endpoint 1.1 – Flexible top players (with total_score)

#* Get top players based on a chosen stat or combined score
#* @param stat One of: "xG per 90", "xA per 90", "Touches in box per 90", or "all" [default]
#* @param n Number of players to return [default = 10]
#* @get /top_players
function(stat = "all", n = 10) {
  allowed_stats <- c("xG per 90", "xA per 90", "Touches in box per 90", "all")
  n <- as.numeric(n)
  
  if (!(stat %in% allowed_stats)) {
    return(list(error = "Invalid stat. Choose one of: 'xG per 90', 'xA per 90', 'Touches in box per 90', 'all'"))
  }
  
  if (stat == "all") {
    df <- football_data |>
      arrange(desc(total_score)) |>
      select(Player, `Current team`, all_of(metrics), total_score) |>
      head(n)
  } else {
    df <- football_data |>
      arrange(desc(.data[[stat]])) |>
      select(Player, `Current team`, all_of(stat)) |>
      head(n)
  }
  
  return(df)
}

# ---------------------------
# Endpoint 1.2 – Top players for a specific team (flexible)
# ---------------------------

#* Get top players from a specific Superliga team based on chosen stat or combined score
#* @param team Team name (partial match allowed)
#* @param stat One of: "xG per 90", "xA per 90", "Touches in box per 90", or "all" [default]
#* @param n Number of players to return [default = 10]
#* @get /top_players_by_team
function(team = "", stat = "all", n = 10) {
  allowed_stats <- c("xG per 90", "xA per 90", "Touches in box per 90", "all")
  n <- as.numeric(n)
  
  if (team == "") {
    return(list(error = "Please provide a team name."))
  }
  if (!(stat %in% allowed_stats)) {
    return(list(error = "Invalid stat. Choose one of: 'xG per 90', 'xA per 90', 'Touches in box per 90', 'all'"))
  }
  
  team_filtered <- football_data |>
    filter(stringr::str_detect(`Current team`, regex(team, ignore_case = TRUE)))
  
  if (nrow(team_filtered) == 0) {
    return(list(error = paste("No players found for team:", team)))
  }
  
  if (stat == "all") {
    df <- team_filtered |>
      arrange(desc(total_score)) |>
      select(Player, `Current team`, all_of(metrics), total_score) |>
      head(n)
  } else {
    df <- team_filtered |>
      arrange(desc(.data[[stat]])) |>
      select(Player, `Current team`, all_of(stat)) |>
      head(n)
  }
  
  return(df)
}

# ---------------------------
# Endpoint 1.3 – Average stat(s) per team (with stat param)
# ---------------------------

#* Get average stat(s) per Superliga team
#* @param stat One of: "xG per 90", "xA per 90", "Touches in box per 90", or "all" [default]
#* @get /team_averages
function(stat = "all") {
  allowed_stats <- c("xG per 90", "xA per 90", "Touches in box per 90", "all")
  
  if (!(stat %in% allowed_stats)) {
    return(list(error = "Invalid stat. Choose one of: 'xG per 90', 'xA per 90', 'Touches in box per 90', 'all'"))
  }
  
  if (stat == "all") {
    result <- football_data |>
      group_by(`Current team`) |>
      summarise(
        avg_xG = mean(`xG per 90`, na.rm = TRUE),
        avg_xA = mean(`xA per 90`, na.rm = TRUE),
        avg_Touches = mean(`Touches in box per 90`, na.rm = TRUE),
        avg_total_score = mean(total_score, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(avg_total_score))
  } else {
    result <- football_data |>
      group_by(`Current team`) |>
      summarise(
        avg_stat = mean(.data[[stat]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(avg_stat))
  }
  
  return(result)
}

# ---------------------------
# Part 2.1 – Top and worst finishers (Goals - xG)
# ---------------------------

#* Show a plot of the 10 best and 10 worst finishers (Goals - xG)
#* @serializer png
#* @get /finishers_plot
function() {
  df <- football_data |>
    filter(!is.na(Goals) & !is.na(xG)) |> 
    mutate(finishing_diff = Goals - xG) |> 
    arrange(desc(finishing_diff))
  
  top10 <- head(df, 10)
  bottom10 <- tail(df, 10)
  combined <- bind_rows(top10, bottom10)
  
  p <- ggplot(combined, aes(x = reorder(Player, finishing_diff), y = finishing_diff, fill = finishing_diff > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(
      title = "Top 10 Best and Worst Finishers",
      x = "Player",
      y = "Goals minus Expected Goals (xG)"
    ) +
    scale_fill_manual(values = c("TRUE" = "#2ca25f", "FALSE" = "#de2d26"))
  print(p)
}

# ---------------------------
# Part 2.2 – Visualize top players based on stat
# ---------------------------

#* Plot top players by selected stat or total score
#* @param stat One of: "xG per 90", "xA per 90", "Touches in box per 90", or "all" [default]
#* @param n Number of players to show [default = 10]
#* @serializer png
#* @get /top_players_plot
function(stat = "all", n = 10) {
  allowed_stats <- c("xG per 90", "xA per 90", "Touches in box per 90", "all")
  if (!(stat %in% allowed_stats)) {
    return(plot(ggplot() + theme_void() + ggtitle("Invalid stat parameter")))
  }
  
  n <- as.numeric(n)
  
  if (stat == "all") {
    df <- football_data |>
      arrange(desc(total_score)) |>
      slice_head(n = n)
    
    p <- ggplot(df, aes(x = reorder(Player, total_score), y = total_score)) +
      geom_col(fill = "#3182bd") +
      coord_flip() +
      labs(
        title = "Top Players by Total Score",
        x = "Player",
        y = "Total Score"
      ) +
      theme_minimal()
  } else {
    df <- football_data |>
      arrange(desc(.data[[stat]])) |>
      slice_head(n = n)
    
    p <- ggplot(df, aes(x = reorder(Player, .data[[stat]]), y = .data[[stat]])) +
      geom_col(fill = "#74c476") +
      coord_flip() +
      labs(
        title = paste("Top Players by", stat),
        x = "Player",
        y = stat
      ) +
      theme_minimal()
  }
  
  print(p)
}


# ---------------------------
# Part 2.3 – Visualize team averages (total score)
# ---------------------------

#* Plot average stat per team
#* @param stat One of: "xG per 90", "xA per 90", "Touches in box per 90", or "all" [default]
#* @serializer png
#* @get /team_averages_plot
function(stat = "all") {
  allowed_stats <- c("xG per 90", "xA per 90", "Touches in box per 90", "all")
  if (!(stat %in% allowed_stats)) {
    return(plot(ggplot() + theme_void() + ggtitle("Invalid stat parameter")))
  }
  
  if (stat == "all") {
    df <- football_data |>
      group_by(`Current team`) |>
      summarise(avg_total_score = mean(total_score, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(avg_total_score))
    
    p <- ggplot(df, aes(x = reorder(`Current team`, avg_total_score), y = avg_total_score)) +
      geom_col(fill = "#756bb1") +
      coord_flip() +
      labs(
        title = "Average Total Score per Team",
        x = "Team",
        y = "Average Score"
      ) +
      theme_minimal()
  } else {
    df <- football_data |>
      group_by(`Current team`) |>
      summarise(avg_stat = mean(.data[[stat]], na.rm = TRUE), .groups = "drop") |>
      arrange(desc(avg_stat))
    
    p <- ggplot(df, aes(x = reorder(`Current team`, avg_stat), y = avg_stat)) +
      geom_col(fill = "#fdae61") +
      coord_flip() +
      labs(
        title = paste("Average", stat, "per Team"),
        x = "Team",
        y = stat
      ) +
      theme_minimal()
  }
  
  print(p)
}