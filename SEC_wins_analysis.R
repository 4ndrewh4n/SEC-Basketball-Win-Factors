#----------------------------------------------------------------------
#Which stats best predict SEC wins?
#Data: ESPN team box scores via the hoopR package (free, no scraping)
#----------------------------------------------------------------------

#install.packages(c("hoopR", "dplyr", "ggplot2", "broom", "tidyr"))
library(hoopR)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)

#---- 1. Load data --------------------------------------
# Seasons are named by the year they end (2026 = 2025-26 season)
box <- load_mbb_team_box(seasons = 2019:2026)

# CHECK THIS FIRST: column names can differ slightly between hoopR versions
names(box)


needed <- c("game_id", "season", "team_id", "team_location", "team_score",
            "team_winner", "field_goals_made", "field_goals_attempted",
            "three_point_field_goals_made", "free_throws_attempted",
            "offensive_rebounds", "defensive_rebounds", "turnovers")
missing_cols <- setdiff(needed, names(box))
if (length(missing_cols) > 0)
{
  stop("These columns weren't found, rename them to match names(box): ", 
       paste(missing_cols, collapse = ", "))
}

# ---- 2. Keep SEC vs SEC games ----------------------------
# (Includes SEC tournament games; a few non-conference SEC matchups
# are possible but rare.)
sec_teams <- c("Alabama", "Arkansas", "Auburn", "Florida", "Georgia",
               "Kentucky", "LSU", "Mississippi State", "Missouri",
               "Ole Miss", "Oklahoma", "South Carolina", "Tennessee",
               "Texas", "Texas A&M", "Vanderbilt")

team_rows <- box |>
  filter(team_location %in% sec_teams) |>
  select(game_id, season, team_id, team = team_location, win = team_winner,
         pts = team_score, fgm = field_goals_made, fga = field_goals_attempted,
         tpm = three_point_field_goals_made, fta = free_throws_attempted,
         orb = offensive_rebounds, drb = defensive_rebounds, tov = turnovers) |>
  distinct(game_id, team_id, .keep_all = TRUE)

# Self-join so each row has the opponent's stats alongside the team's
games <- team_rows |>
  inner_join(team_rows, by = "game_id", suffix = c("", "_opp"),
             relationship = "many-to-many") |>
  filter(team_id != team_id_opp)

table(table(games$game_id))

# ---- 3. Build the "Four Factors" (Dean OLiver) -------------
games <- games |>
  mutate(
    poss     = fga - orb + tov + 0.475 * fta,
    poss_opp = fga_opp - orb_opp + tov_opp + 0.475 * fta_opp,
    efg      = (fgm + 0.5 * tpm) / fga,
    efg_opp  = (fgm_opp + 0.5 * tpm_opp) / fga_opp,
    tov_rate     = tov / poss,
    tov_rate_opp = tov_opp / poss_opp,
    orb_pct      = orb / (orb + drb_opp),
    orb_pct_opp  = orb_opp / (orb_opp + drb),
    ft_rate      = fta / fga,
    ft_rate_opp  = fta_opp / fga_opp,
    # Differentials: positive = team did better than its opponent
    d_efg = efg - efg_opp,
    d_tov = tov_rate_opp - tov_rate,   # flipped: positive = fewer turnovers than opp
    d_orb = orb_pct - orb_pct_opp,
    d_ftr = ft_rate - ft_rate_opp,
    win   = as.integer(win)
  ) |>
  filter(!is.na(win), is.finite(d_efg), is.finite(d_tov), 
         is.finite(d_orb), is.finite(d_ftr))

# Keep one row per game (each game currently appears once per team)
games_one <- games |> filter(team_id < team_id_opp)

cat("Games in sample:", nrow(games_one), "\n")
cat("Mizzou games:", sum(games$team == "Missouri"), "\n")

# ---- 4. Logistic regression on standardized predictors ------
# Standardizing puts every stat on the same scale, so coefficient size
# = "how much does a 1 standard deviation edge in this stat move win odds?"
model_data <- games |>
  mutate(across(c(d_efg, d_tov, d_orb, d_ftr), ~ as.numeric(scale(.x))))

fit <- glm(win ~ d_efg + d_tov + d_orb + d_ftr,
           data = model_data, family = binomial)

results <- tidy(fit, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    factor = recode(term,
                    d_efg = "Shooting (eFG%)",
                    d_tov = "Turnover rate",
                    d_orb = "Offensive rebounding",
                    d_ftr = "Free throw rate"),
    odds_ratio = exp(estimate)
  ) |>
  arrange(desc(estimate))

print(results |> select(factor, estimate, odds_ratio, p.value))

# ---- 5. Plot -------------------------------------------------
p <- ggplot(results, aes(x = reorder(factor, estimate), y = estimate)) +
  geom_col(fill = "#F1B82D") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(title = "What wins SEC games?",
       subtitle = "Logistic regression, standardized stat differentials vs. opponent (2019-2026)",
       x = NULL, y = "Effect on log-odds of winning (per 1 SD edge)") +
  theme_minimal(base_size = 13)

ggsave("sec_wins_factors.png", p, width = 8, height = 4.5, dpi = 200)

# ---- 6. Mizzou angle -------------------------------------------
# How do Mizzou's wins and losses differ on each factor?
mizzou <- games |>
  filter(team == "Missouri") |>
  group_by(win) |>
  summarise(games = n(),
            across(c(d_efg, d_tov, d_orb, d_ftr), mean),
            .groups = "drop") |>
  mutate(result = ifelse(win == 1, "Wins", "Losses"))

print(mizzou)

# ---- 7. Quick accuracy check --------------------------
# Note: this uses same-game stats, so it explains wins rather than
# truly predicting them (see "next step" in the chat).
pred <- ifelse(predict(fit, type = "response") > 0.5, 1, 0)
cat("In-sample accuracy:", round(mean(pred == model_data$win), 3), "\n")

