# ══════════════════════════════════════════════════════════════════════════
# NBA PROJECTION PIPELINE
#   1. Load saved per-game model coefficients (from nba_fit_models.R)
#   2. Load current-season data (hoopR) + saved win totals; project forward
#   3. Build a standings table image
# ══════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(hoopR)
library(gt)

# ── Config ───────────────────────────────────────────────────────────────
target_season   <- 2027
coefs_csv       <- "projection_coefs_all_predictors.csv"
lines_2027_csv  <- "Covers Win Totals NBA 2027.csv"
games_in_season <- 82

# ══════════════════════════════════════════════════════════════════════════
# HELPERS
# ══════════════════════════════════════════════════════════════════════════

# Coerce a percentage column to 0–1 (handles 0–100 input); invalid → NA
normalize_pct <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  med <- median(x, na.rm = TRUE)
  if (!is.na(med) && med > 1) x <- x / 100
  x[!is.na(x) & (x < 0 | x > 1)] <- NA
  x
}

# SRS through game n for one team: avg margin + avg of opponents' avg margins
srs_through <- function(tg, team, n) {
  cur <- filter(tg, team_abbr == team, game_number <= n)
  opp_avg <- map_dbl(cur$opponent_abbr, function(opp) {
    og <- filter(tg, team_abbr == opp, game_number <= n)
    if (nrow(og) > 0) mean(og$margin, na.rm = TRUE) else 0
  })
  mean(cur$margin, na.rm = TRUE) + mean(opp_avg, na.rm = TRUE)
}

srs_through_all <- function(tg, team, games) {
  map_dbl(games, ~ srs_through(tg, team, .x))
}

# ══════════════════════════════════════════════════════════════════════════
# PART 1 — LOAD MODEL COEFFICIENTS
# ══════════════════════════════════════════════════════════════════════════

fit_multi <- read_csv(coefs_csv, show_col_types = FALSE)
cat("Loaded coefficients for", nrow(fit_multi), "game checkpoints\n")

# ══════════════════════════════════════════════════════════════════════════
# PART 2 — CURRENT-SEASON PROJECTIONS
# ══════════════════════════════════════════════════════════════════════════

# ── Current-season win totals (saved from Covers) ────────────────────────
wintotals_2027 <- read_csv(lines_2027_csv, show_col_types = FALSE) %>%
  select(team_abbr, Win.Total)

cat("Loaded", nrow(wintotals_2027), "teams' 2027 win totals\n")

# ── Projection from per-game coefficients ────────────────────────────────
project_final <- function(game_x, srs_now, wpct_now, d3p_now, dft_now, wintot) {
  row <- filter(fit_multi, game == game_x)
  if (nrow(row) == 0) {
    row <- slice_max(fit_multi, game, n = 1)
    warning("No model for game ", game_x, "; using game ", row$game)
  }
  tibble(
    proj_srs_final  = row$srs_icpt  + row$srs_b_srs  * srs_now  +
      row$srs_b_d3p  * d3p_now  + row$srs_b_dft  * dft_now +
      row$srs_b_wpct * wpct_now + row$srs_b_wintot * wintot,
    proj_wpct_final = row$wpct_icpt + row$wpct_b_srs * srs_now  +
      row$wpct_b_d3p * d3p_now + row$wpct_b_dft * dft_now +
      row$wpct_b_wpct * wpct_now + row$wpct_b_wintot * wintot
  )
}

empty_projections <- tibble(
  team_abbr = character(), game_number = integer(),
  Win.Total = numeric(),
  srs_current = numeric(), win_pct_current = numeric(),
  d3p_current = numeric(), dft_current = numeric(),
  proj_srs_final = numeric(), proj_wpct_final = numeric()
)

build_projections <- function(season, wintotals) {
  team_box <- tryCatch(load_nba_team_box(seasons = season),
                       error = function(e) tibble())
  schedule <- tryCatch(load_nba_schedule(seasons = season),
                       error = function(e) tibble())
  
  if (nrow(team_box) == 0 || nrow(schedule) == 0) {
    cat("No games available for", season, "yet. Empty projection.\n")
    return(empty_projections)
  }
  
  # Regular-season, completed games only
  team_box <- team_box %>%
    filter(season_type == 2, !is.na(team_score)) %>%
    mutate(
      three_point_pct      = normalize_pct(three_point_field_goal_pct),
      free_throw_pct       = normalize_pct(free_throw_pct),
      three_point_attempts = suppressWarnings(as.numeric(three_point_field_goals_attempted)),
      free_throw_attempts  = suppressWarnings(as.numeric(free_throws_attempted)),
      game_date            = as.Date(game_date)
    ) %>%
    filter(!is.na(three_point_pct), !is.na(free_throw_pct),
           !is.na(three_point_attempts), !is.na(free_throw_attempts),
           between(three_point_attempts, 0, 100),
           between(free_throw_attempts,  0, 100))
  
  games <- schedule %>%
    filter(season_type == 2, status_type_completed == TRUE,
           !is.na(home_score), !is.na(away_score)) %>%
    mutate(game_date   = as.Date(game_date),
           home_margin = as.numeric(home_score) - as.numeric(away_score))
  
  # One row per team per game
  home_side <- games %>% transmute(
    game_id, game_date,
    team_abbr     = as.character(home_abbreviation),
    opponent_abbr = as.character(away_abbreviation),
    margin        = home_margin,
    win           = as.integer(home_margin > 0)
  )
  away_side <- games %>% transmute(
    game_id, game_date,
    team_abbr     = as.character(away_abbreviation),
    opponent_abbr = as.character(home_abbreviation),
    margin        = -home_margin,
    win           = as.integer(home_margin < 0)
  )
  
  # Shooting-luck points vs. league average to date
  shooting <- team_box %>%
    transmute(game_id, game_date,
              team_abbr = as.character(team_abbreviation),
              three_point_pct, free_throw_pct,
              three_point_attempts, free_throw_attempts)
  
  lg_by_date <- shooting %>%
    distinct(game_date) %>%
    arrange(game_date) %>%
    mutate(
      lg_3p_pct = map_dbl(game_date, ~ mean(shooting$three_point_pct[shooting$game_date <= .x], na.rm = TRUE)),
      lg_ft_pct = map_dbl(game_date, ~ mean(shooting$free_throw_pct [shooting$game_date <= .x], na.rm = TRUE))
    )
  
  shooting <- shooting %>%
    left_join(lg_by_date, by = "game_date") %>%
    mutate(
      game_3p_diff = (three_point_pct - lg_3p_pct) * three_point_attempts * 3,
      game_ft_diff = (free_throw_pct  - lg_ft_pct)  * free_throw_attempts
    )
  
  tg <- bind_rows(home_side, away_side) %>%
    arrange(team_abbr, game_date) %>%
    group_by(team_abbr) %>%
    mutate(game_number = row_number()) %>%
    ungroup() %>%
    left_join(select(shooting, game_id, team_abbr, game_3p_diff, game_ft_diff),
              by = c("game_id", "team_abbr"))
  
  # Cumulative metrics through each game
  team_cumulative <- tg %>%
    group_by(team_abbr) %>%
    arrange(game_number) %>%
    mutate(
      win_pct_current = cummean(win),
      d3p_current     = cummean(game_3p_diff),
      dft_current     = cummean(game_ft_diff),
      srs_current     = srs_through_all(tg, first(team_abbr), game_number)
    ) %>%
    ungroup() %>%
    mutate(team_abbr = recode(team_abbr,
                              "GS" = "GSW", "NO" = "NOP", "NY" = "NYK",
                              "SA" = "SAS", "UTAH" = "UTA", "WSH" = "WAS"))
  
  # Latest game per team, joined to Vegas lines, then projected
  team_cumulative %>%
    group_by(team_abbr) %>%
    slice_max(game_number, n = 1) %>%
    ungroup() %>%
    left_join(wintotals, by = "team_abbr") %>%
    filter(!is.na(Win.Total), !is.na(srs_current), !is.na(win_pct_current)) %>%
    rowwise() %>%
    mutate(proj = list(project_final(game_number, srs_current, win_pct_current,
                                     d3p_current, dft_current, Win.Total))) %>%
    unnest(proj) %>%
    ungroup() %>%
    arrange(desc(proj_wpct_final))
}

projections_2027 <- build_projections(target_season, wintotals_2027)

if (nrow(projections_2027) > 0) {
  projections_2027 %>%
    select(team_abbr, game_number, Win.Total,
           srs_current, win_pct_current,
           proj_srs_final, proj_wpct_final) %>%
    head(30) %>%
    print()
}

write_csv(projections_2027, "nba_2027_projections.csv")
cat("\nSaved", nrow(projections_2027), "team projections to nba_2027_projections.csv\n")

# ══════════════════════════════════════════════════════════════════════════
# PART 3 — STANDINGS TABLE
# ══════════════════════════════════════════════════════════════════════════

team_display_names <- tribble(
  ~team_abbr, ~Team,
  "ATL", "Atlanta Hawks",        "BOS", "Boston Celtics",       "BKN", "Brooklyn Nets",
  "CHA", "Charlotte Hornets",    "CHI", "Chicago Bulls",        "CLE", "Cleveland Cavaliers",
  "DAL", "Dallas Mavericks",     "DEN", "Denver Nuggets",       "DET", "Detroit Pistons",
  "GSW", "Golden State Warriors","HOU", "Houston Rockets",      "IND", "Indiana Pacers",
  "LAC", "LA Clippers",          "LAL", "Los Angeles Lakers",   "MEM", "Memphis Grizzlies",
  "MIA", "Miami Heat",           "MIL", "Milwaukee Bucks",      "MIN", "Minnesota Timberwolves",
  "NOP", "New Orleans Pelicans", "NYK", "New York Knicks",      "OKC", "Oklahoma City Thunder",
  "ORL", "Orlando Magic",        "PHI", "Philadelphia 76ers",   "PHX", "Phoenix Suns",
  "POR", "Portland Trail Blazers","SAC", "Sacramento Kings",    "SAS", "San Antonio Spurs",
  "TOR", "Toronto Raptors",      "UTA", "Utah Jazz",            "WAS", "Washington Wizards"
)

table_data <- projections_2027 %>%
  left_join(team_display_names, by = "team_abbr") %>%
  mutate(
    Proj.Wins = proj_wpct_final * games_in_season,
    Win.Pct   = scales::percent(win_pct_current, accuracy = 0.1),
    Proj.MOV  = proj_srs_final
  ) %>%
  arrange(desc(Proj.Wins)) %>%
  select(
    Team,
    GP                    = game_number,
    `Current Win %`       = Win.Pct,
    `Vegas Expected Wins` = Win.Total,
    `Proj Wins`           = Proj.Wins,
    `Proj Opp Adj MOV`    = Proj.MOV
  )

nba_table <- table_data %>%
  gt() %>%
  tab_header(
    title    = md("**2026-27 NBA Projected Final Standings**"),
    subtitle = "Projections based on Vegas pre-season projections, current opponent-adjusted MOV, HCA, win %, and shooting variance adjusted."
  ) %>%
  fmt_number(
    columns  = c(`Vegas Expected Wins`, `Proj Wins`, `Proj Opp Adj MOV`),
    decimals = 1
  ) %>%
  cols_align(align = "left",   columns = Team) %>%
  cols_align(align = "center",
             columns = c(GP, `Current Win %`, `Vegas Expected Wins`,
                         `Proj Wins`, `Proj Opp Adj MOV`)) %>%
  data_color(
    columns = c(`Proj Wins`, `Proj Opp Adj MOV`),
    fn = scales::col_numeric(
      palette = c("#5b1b74", "#9b59b6", "#ffffff", "#b2df8a", "#1f7a38"),
      domain  = NULL
    )
  ) %>%
  opt_row_striping() %>%
  opt_table_lines(extent = "all") %>%
  tab_options(
    heading.align                 = "center",
    table.font.names              = "sans-serif",
    column_labels.font.weight     = "bold",
    row.striping.background_color = "gray98"
  ) %>%
  tab_source_note(
    source_note = "Data via hoopR and Covers.com. Modeling by Kevin Ferrigan."
  )

gtsave(nba_table, "nba_2027_projected_standings.png")
