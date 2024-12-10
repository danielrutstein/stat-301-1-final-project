# Expedited Database Setup ----
library(tidyverse)
library(nflplotR)
library(tidymodels)

draft_prospects <- read_csv("data/nfl_draft_prospects.csv", guess_max = 13000)|>
  select(
    player_id, player_name, overall, school_name, school_abbr, pick, link, traded, trade_note, pick, weight, height, pos_rk, ovr_rk, draft_year, grade
  ) |> rename(
    pick_rd = pick, college_mascot = school_name, college_abbr = school_abbr 
  )
draft_fbr_11_20 <- readxl::read_xlsx("data/nfldraft_11_20.xlsx")
coaches_fbr_11_20 <- readxl::read_xlsx("data/nfl_coaches_11_20.xlsx")
draft <- draft_fbr_11_20 |>
  inner_join(draft_prospects, join_by(pick == overall, year == draft_year)) |>
  left_join(coaches_fbr_11_20, join_by(year, team))
draft <- draft |>
  complete(fill = list(rel_w_av = 0, w_av = 0, dr_av = 0, gp = 0)) 
draft <- draft |>
  mutate(
    team = fct_recode(team,
                      "LAC" = "SDG",
                      "LAR" = "STL",
                      "LVR" = "OAK"
    )
  ) 
std_pos_order <- c("QB", "RB", "WR", "TE", "OL", "IDL", "EDGE", "LB", "DB", "ST")
draft <- draft |>
  mutate(
    pos_group = fct_collapse(pos,
                             "RB" = c("RB", "FB"),
                             "OL" = c("C", "G", "T", "OL"),
                             "IDL" = c("DL", "DT", "NT"),
                             "EDGE" = c("DE", "OLB"),
                             "LB" = c("ILB", "LB"),
                             "DB" = c("DB", "CB","S"),
                             "ST" = c("K", "LS", "P")
    ),
    pos_group = fct_relevel(pos_group, std_pos_order)
  )
draft <- draft |>
  mutate(
    career_length = played_to - year, 
    active = if_else(played_to == 2024, TRUE, FALSE)
  ) 

draft <- draft |>
  mutate(
    log_w_av = (w_av / (2.275 + 7.054 * log(2024 - year))),
    rel_w_av = log_w_av/mean(log_w_av),
    exp_pick_av = (rel_w_av / (4.263 - 0.7171 * log(pick))),
    rel_pick_av = exp_pick_av - mean(exp_pick_av),
    avg_w_av = if_else(career_length > 0, w_av / career_length, 0)
  ) 

### Load Datasets 
#load and join datasets to main "draft" data frame
draft_prospects <- read_csv("data/nfl_draft_prospects.csv", guess_max = 13000)|>
  select(
    player_id, player_name, overall, school_name, school_abbr, pick, link, traded, trade_note, pick, weight, height, pos_rk, ovr_rk, draft_year, grade
  ) |> rename(
    pick_rd = pick, college_mascot = school_name, college_abbr = school_abbr 
  )

draft_fbr_11_20 <- readxl::read_xlsx("data/nfldraft_11_20.xlsx")
coaches_fbr_11_20 <- readxl::read_xlsx("data/nfl_coaches_11_20.xlsx")

### Join Datasets
#official join statement
draft <- draft_fbr_11_20 |>
  inner_join(draft_prospects, join_by(pick == overall, year == draft_year)) |>
  left_join(coaches_fbr_11_20, join_by(year, team))

draft <- draft |>
  complete(fill = list(rel_w_av = 0, w_av = 0, dr_av = 0, gp = 0)) 

### Clean/Add Variables
# ignore relocation for Chargers, Raiders and Rams strings (they're the same team for our purposes)
draft <- draft |>
  mutate(
    team = fct_recode(team,
                      "LAC" = "SDG",
                      "LAR" = "STL",
                      "LVR" = "OAK"
    )
  ) 

#simplify position groups to standard groupings
std_pos_order <- c("QB", "RB", "WR", "TE", "OL", "IDL", "EDGE", "LB", "DB", "ST")
draft <- draft |>
  mutate(
    pos_group = fct_collapse(pos,
                             "RB" = c("RB", "FB"),
                             "OL" = c("C", "G", "T", "OL"),
                             "IDL" = c("DL", "DT", "NT"),
                             "EDGE" = c("DE", "OLB"),
                             "LB" = c("ILB", "LB"),
                             "DB" = c("DB", "CB","S"),
                             "ST" = c("K", "LS", "P")
    ),
    pos_group = fct_relevel(pos_group, std_pos_order)
  )

# add helpful variables
draft <- draft |>
  mutate(
    career_length = played_to - year, 
    active = if_else(played_to == 2024, TRUE, FALSE),
    dr_day = fct_collapse(as.factor(round),
                          "Day 1" = "1",
                          "Day 2" = c("2","3"),
                          "Day 3" = c("4","5", "6", "7")
    )
  ) 

# weight player value relative to log expectation (for draft year)
draft <- draft |>
  mutate(
    log_w_av = (w_av / (2.275 + 7.054 * log(2024 - year))),
    rel_w_av = log_w_av/mean(log_w_av),
    avg_w_av = if_else(career_length > 0, w_av / career_length, 0)
  ) 

# weight player value relative to linear expectation (for draft year)
draft <- draft |>
  mutate(
    rel_pick_av = rel_w_av - (4.263 - 0.7171 * log(pick))
  ) 











# Database setup ---- 
#load packages
library(tidyverse)
library(nflplotR)
library(tidymodels)

draft_prospects <- read_csv("data/nfl_draft_prospects.csv", guess_max = 13000)|>
  select(
    player_id, player_name, overall, school_name, school_abbr, pick, link, traded, trade_note, pick, weight, height, pos_rk, ovr_rk, draft_year, grade
  ) |> rename(
    pick_rd = pick, college_mascot = school_name, college_abbr = school_abbr 
  )
draft_fbr_11_20 <- readxl::read_xlsx("data/nfldraft_11_20.xlsx")
coaches_fbr_11_20 <- readxl::read_xlsx("data/nfl_coaches_11_20.xlsx")
draft <- draft_fbr_11_20 |>
  inner_join(draft_prospects, join_by(pick == overall, year == draft_year)) |>
  left_join(coaches_fbr_11_20, join_by(year, team))
draft <- draft |>
  complete(fill = list(rel_w_av = 0, w_av = 0, dr_av = 0, gp = 0)) 
draft <- draft |>
  mutate(
    team = fct_recode(team,
                      "LAC" = "SDG",
                      "LAR" = "STL",
                      "LVR" = "OAK"
    )
  ) 
std_pos_order <- c("QB", "RB", "WR", "TE", "OL", "IDL", "EDGE", "LB", "DB", "ST")
draft <- draft |>
  mutate(
    pos_group = fct_collapse(pos,
                             "RB" = c("RB", "FB"),
                             "OL" = c("C", "G", "T", "OL"),
                             "IDL" = c("DL", "DT", "NT"),
                             "EDGE" = c("DE", "OLB"),
                             "LB" = c("ILB", "LB"),
                             "DB" = c("DB", "CB","S"),
                             "ST" = c("K", "LS", "P")
    ),
    pos_group = fct_relevel(pos_group, std_pos_order)
  )
draft <- draft |>
  mutate(
    career_length = played_to - year, 
    active = if_else(played_to == 2024, TRUE, FALSE)
  ) 

draft <- draft |>
  mutate(
    log_w_av = (w_av / (2.275 + 7.054 * log(2024 - year))),
    rel_w_av = log_w_av/mean(log_w_av),
    exp_pick_av = (rel_w_av / (4.263 - 0.7171 * log(pick))),
    rel_pick_av = exp_pick_av - mean(exp_pick_av),
    avg_w_av = if_else(career_length > 0, w_av / career_length, 0)
  ) 

draft |> summarize(
    mean = mean(rel_pick_av),
    .by = round
  )






### Load Datasets ----
#load and join datasets to main "draft" data frame
draft_prospects <- read_csv("data/nfl_draft_prospects.csv", guess_max = 13000)|>
  select(
    player_id, player_name, overall, school_name, school_abbr, pick, link, traded, trade_note, pick, weight, height, pos_rk, ovr_rk, draft_year, grade
  ) |> rename(
    pick_rd = pick, college_mascot = school_name, college_abbr = school_abbr 
  )

draft_fbr_11_20 <- readxl::read_xlsx("data/nfldraft_11_20.xlsx")
coaches_fbr_11_20 <- readxl::read_xlsx("data/nfl_coaches_11_20.xlsx")

#need to set team name equal to team drafted
#draft_contracts <- read_csv("data/combined_data_2000-2023.csv") |>
# filter(year_signed == draft_year & draft_year >= 2011 & draft_year <= 2020) |>
#filter()
#select(pick, draft_year, value, value_norm, gtd, gtd_norm) 

# view(draft_contracts)

### Join Datasets ----
#official join statement
draft <- draft_fbr_11_20 |>
  inner_join(draft_prospects, join_by(pick == overall, year == draft_year)) |>
  left_join(coaches_fbr_11_20, join_by(year, team))



view(draft)
skimr::skim_without_charts(draft)


### Evaluate Missingness ----
naniar::gg_miss_var(draft)


draft |>
  filter(is.na(w_av)) |>
  arrange(desc(gp)) |>
  select(player, gp, w_av)


# what does pos_rk NA mean?
draft |>
  filter(is.na(pos_rk)) |>
  arrange(desc(gp)) |>
  select(player, pick, year, pos_rk, ovr_rk)


draft <- draft |>
  complete(fill = list(rel_w_av = 0, w_av = 0, dr_av = 0, gp = 0)) 

### Clean/Add Variables  ----

# ignore relocation for Chargers, Raiders and Rams strings (they're the same team for our purposes)
draft <- draft |>
  mutate(
    team = fct_recode(team,
                      "LAC" = "SDG",
                      "LAR" = "STL",
                      "LVR" = "OAK"
    )
  ) 

#simplify position groups to standard groupings
std_pos_order <- c("QB", "RB", "WR", "TE", "OL", "IDL", "EDGE", "LB", "DB", "ST")
length(std_pos_order)
draft <- draft |>
  mutate(
    pos_group = fct_collapse(pos,
                             "RB" = c("RB", "FB"),
                             "OL" = c("C", "G", "T", "OL"),
                             "IDL" = c("DL", "DT", "NT"),
                             "EDGE" = c("DE", "OLB"),
                             "LB" = c("ILB", "LB"),
                             "DB" = c("DB", "CB","S"),
                             "ST" = c("K", "LS", "P")
    ),
    pos_group = fct_relevel(pos_group, std_pos_order)
  )

draft |>
  ggplot(aes(x = as.factor(round), y = pos_group)) +
  geom_tile(aes(fill = n))


# add helpful variables
draft <- draft |>
  mutate(
    career_length = played_to - year, 
    active = if_else(played_to == 2024, TRUE, FALSE),
    dr_day = fct_collapse(as.factor(round),
      "Day 1" = "1",
      "Day 2" = c("2","3"),
      "Day 3" = c("4","5", "6", "7")
    )
  ) 

draft |>
  filter( w_av > 0) |>
  group_by(year) |>
  summarize(
    mean = mean(w_av, na.rm = TRUE),
    median = median(w_av, na.rm = TRUE),
    sd = sd(w_av, na.rm = TRUE),
    IQR = IQR(w_av, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = mean)) +
  geom_point() +
  geom_smooth(method="lm", formula= (mean ~ log(2024 - year)), se=FALSE, color=2)


#seems to be exponential model of expected value
draft |>
  mutate (year_since = 2024 - year) |>
  lm(formula = w_av ~ log(year_since))

# weight player value relative to log expectation (for draft year)
draft <- draft |>
  mutate(
    log_w_av = (w_av / (2.275 + 7.054 * log(2024 - year))),
    rel_w_av = log_w_av/mean(log_w_av),
    avg_w_av = if_else(career_length > 0, w_av / career_length, 0)
  ) 

draft |>
  group_by(pick) |>
  summarize(
    avg_value = sum(rel_w_av, na.rm = TRUE)/n()
  ) |>
  ggplot(aes(x = pick, y = avg_value)) +
  geom_point()

#seems to be exponential model of expected value, just like the picks themselves
draft |>
  lm(formula = rel_w_av ~ log(pick))

# weight player value relative to linear expectation (for draft year)
draft <- draft |>
  mutate(
    rel_pick_av = rel_w_av - (4.263 - 0.7171 * log(pick))
  ) 


draft |> 
  summarize(
    meanrel_w_av = mean(rel_w_av, na.rm = TRUE)
  )

draft |>
  group_by(year) |>
  summarize(
    mean = mean(w_av)
  )

# Initial Analysis ----
draft |>
  group_by(year) |>
  summarize(mean = mean(w_av, na.rm = TRUE)) |>
  ggplot(aes(x = as.factor(year), y = mean)) +
  geom_point() +
  labs(
    title = "Expected value (mean w_av) by draft class", 
    x = "draft class", 
    y = "expected value"
  )
ggsave(filename = "exp_w_av_1120.png")
## Univariate analysis ----

#distribution of expected value in draft
draft |>
  ggplot(aes(x = rel_w_av)) +
  geom_density() +
  labs(
    title = "Distribution of player value", 
    x = "player value (as measured by rel_w_av)"
  )
ggsave(filename = "dist_value.png")


#distribution of expected value in draft by round, and year
draft |>
  ggplot(aes(x = as.factor(round), y = rel_w_av)) +
  geom_boxplot() +
  labs(
    title = "Distribution of player value by round", 
    x = "round",
    y = "player value"
  )
ggsave(filename = "dist_value_rnd.png")

draft |>
  ggplot(aes(x = rel_w_av)) +
  geom_density() +
  facet_grid(year ~ round)

## Bivariate Analysis ----
# by position and round
draft |>
  ggplot(aes(x = as.factor(round), y = rel_w_av)) +
  geom_boxplot() +
  facet_wrap(~pos_group)
ggsave(filename = "pos_group_value.png")

draft |>
  count(round, pos_group) |>
  ggplot(aes(x = as.factor(round), y = pos_group)) +
  geom_tile(aes(fill = n)) +
  labs(
    title = "Density of position targeted by round", 
    x = "round",
    y = "position"
  )
ggsave(filename = "pos_group_tile.png")


draft |>
  ggplot(aes(x = rel_w_av)) +
  geom_density() +
  facet_wrap(~pos_group) +
  labs(
    title = "Distribution of player value by position group", 
    x = "player value"
  )

draft |>
  ggplot(aes(x = as.factor(round), y = rel_w_av)) +
  geom_boxplot() +
  facet_wrap(~pos_group) +
  labs(
    title = "Distribution of player value by round, grouped by position", 
    x = "round",
    y = "player value"
  )
ggsave(filename = "pos_group_value.png")


# Round 1 ----
## Draft Success vs Team Success ----
draft |>
  group_by(team, year) |>
  summarize(
    avg_value = sum(rel_pick_av, na.rm = TRUE),
    games_won = mean(win)
  ) |> arrange(desc(avg_value))|> ggplot(aes(x = games_won, y = avg_value)) +
  geom_boxplot(aes(group = cut_width(games_won, 1))) +
  labs(
    x = "average draft pick value",
    y = "wins",
    caption = "Data: Sports Reference",
    title = "Mapping of relationship between draft success and team success"
  ) 


draft |> 
  summarize(
    avg_value = sum(rel_w_av, na.rm = TRUE)/n(),
    win_pct = (mean(win) + (mean(tie)/2))/ 16,
    .by = team
  ) |> ggplot(aes(x = avg_value, y = win_pct)) +
  geom_smooth(method = "lm", formula = y~x, alpha = 0.3, color = "grey75") +
  geom_mean_lines(aes(x0 = 1, y0 = .5)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "average draft pick value",
    y = "win percentage",
    caption = "Data: Sports Reference",
    title = "Mapping of relationship between draft success and team success"
  ) 



draft |> 
  summarize(
    avg_value = sum(rel_pick_av, na.rm = TRUE)/n(),
    win_pct = (mean(win) + (mean(tie)/2))/ 16,
    .by = team
  ) |> ggplot(aes(x = avg_value, y = win_pct)) +
  geom_smooth(method = "lm", formula = y~x, alpha = 0.3, color = "grey75") +
  geom_mean_lines(aes(x0 = 0, y0 = .5)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "average draft pick value over expectation",
    y = "win percentage",
    caption = "Data: Sports Reference",
    title = "Mapping of relationship between draft success and team success"
  ) 

## Playoffs & Super Bowls ----
# does the rookie contract players correlate w/ super bowl wins
mutate a first_pick variable so we can see 
draft_fp <- draft |> 
  group_by(year, team) |>
  mutate(
      first_pick = if_else(pick == min(pick), TRUE, FALSE)
    ) 

draft_fp

draft_fp |> 
  summarize(
    avg_value = sum(rel_pick_av, na.rm = TRUE)/n(),
    tot_post_win = sum(post_win, first_pick == TRUE),
    .by = team
  ) |>  ggplot(aes(x = avg_value, y = tot_post_win)) +
  geom_smooth(method = "lm", formula = y~x, alpha = 0.3, color = "grey75") +
  geom_mean_lines(aes(x0 = 1, y0 = .5)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "average draft pick value vs expectation",
    y = "post win",
    caption = "Data: Sports Reference",
    title = "Mapping of relationship between draft success and postseason success"
  ) 




class_value <- funciton (x, var) {
  group_by(x, y) |>
    summarize(
      sum(y)
    )
}
  


years <- c(2011,2012,2013,2014,2015,2016,2017,2018, 2019, 2020)
  

rookie_contract <- function(df, x) {
  filter(x )
  group_by(team) |>
    summarize(
      rookie_contracts = sum(rel_w_av),
    ) 
}

rookie_contracts <- NULL
draft |>
  filter(year <= 2014 & year >= 2011) |>
  summarize(
    rc_value = sum(rel_pick_av),
    playoff_wins = mean(post_win),
    .by = team
  ) |> ggplot(aes(x = rc_value, y = playoff_wins)) +
  geom_point()






for (i in years){
  if (i > 2013){
    draft |>
      filter(year <= i & year >= i - 3)
      group_by(team) |>
      summarize(
        class_value = sum(rel_w_av),
      )
  } else {
    draft |>
    group_by(team) |>
      summarize(
        class_value = 0,
      )
  }
}

if_else(year > 2013,
  draft |>
    filter(year <= 2014)
  group_by(team) |>
    summarize(
      class_value = sum(rel_w_av),
    )
} else {
  draft |>
    group_by(team) |>
    summarize(
      class_value = 0,
    )
}
}



|> pivot_wider(
    names_from = year,
    values_from = class_value
  ) summarize(
    across
  )

# Round 2: Age Exploration ----
draft_age <- draft |>
filter(!is.na(age)) |>
mutate(
  age_fct = fct_collapse(
    as.factor(age),
    "<22" = c("20","21"),
    ">24" = c("25", "26", "27", "28"),
    )
  ) 

#overall summary
draft_age |>
  summarize(
    mean = mean(rel_w_av),
    rel_mean = mean(rel_pick_av),
    median = median(rel_w_av),
    rel_median = median(rel_pick_av),
    sd = sd(rel_w_av),
    rel_sd = sd(rel_pick_av),
    .by = age_fct
  ) |> arrange(age_fct)

#summary by draft day
draft_age |>
  summarize(
    mean = mean(rel_w_av),
    rel_mean = mean(rel_pick_av),
    median = median(rel_w_av),
    rel_median = median(rel_pick_av),
    sd = sd(rel_w_av),
    rel_sd = sd(rel_pick_av),
    .by = c(age_fct, dr_day)
  ) |> arrange(dr_day, age_fct)

#grid density
draft_age |>
  ggplot(aes(x = rel_w_av)) +
  geom_density() +
  facet_grid(dr_day~age_fct) +
  labs(
    title = "Distribution of player value by age and round", 
    x = "draft day",
    y = "player value"
  ) 

# relationship between draft age and win percentage
draft |> 
  summarize(
    avg_age = sum(age, na.rm = TRUE)/n(),
    win_pct = (mean(win) + (mean(tie)/2))/ 16,
    .by = team
  ) |> ggplot(aes(x = avg_age, y = win_pct)) +
  geom_smooth(method = "lm", formula = y~x, alpha = 0.3, color = "grey75") +
  geom_mean_lines(aes(x0 = mean(avg_age), y0 = .5)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "average age",
    y = "win percentage",
    caption = "Data: Sports Reference",
    title = "Mapping of relationship between average age and team success"
  ) 

# relationship between draft age and draft success
draft |> 
  summarize(
    avg_age = sum(age, na.rm = TRUE)/n(),
    avg_value = sum(rel_pick_av, na.rm = TRUE)/n(),
    .by = team
  ) |> ggplot(aes(x = avg_age, y = avg_value)) +
  geom_smooth(method = "lm", formula = y~x, alpha = 0.3, color = "grey75") +
  geom_mean_lines(aes(x0 = mean(avg_age), y0 = mean(avg_value))) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "average age",
    y = "average draft pick value over expectation",
    caption = "Data: Sports Reference",
    title = "Mapping of relationship between average age and draft success"
  ) 

# relationship between draft age and draft success by position
draft_age |>
  ggplot(aes(x = age_fct, y = rel_w_av)) +
  geom_boxplot() +
  facet_wrap(~pos_group) +
  labs(
    title = "Distribution of position group value by age", 
    x = "age group",
    y = "player value"
  )

draft_age |>
  ggplot(aes(x = age_fct, y = rel_pick_av)) +
  geom_boxplot() +
  facet_wrap(~pos_group) +
  labs(
    title = "Distribution of position group value by age", 
    x = "age group",
    y = "player value vs expectation"
  )

## Testing "Pro-Ready" Hypothesis ----
# starter rate
draft_age |>
  summarize(
    mean_start = mean(start_yrs),
    median_start = median(start_yrs),
    mean_gp = mean(gp),
    median_gp = median(gp),
    .by = age_fct
  ) |> arrange(age_fct)

# boom rate
draft_age |>
  mutate(
    honors = if_else(
      all_pro > 0, "All-Pro",
      if_else(pro_bowl > 0, "Pro Bowl","none"),
      ),
    honors = fct_relevel(honors, "none", "Pro Bowl")
  ) |> 
  ggplot(aes(x = age_fct, alpha = honors)) +
  geom_bar()  +
  labs(
    title = "Visualizing superstar outcomes by age", 
    x = "age group",
    y = "players"
  )


# Round 3: Coaches ----
#setup
fired_2020 <- c("Dan Quinn", "Matt Patricia", "Bill O'Brien", "Doug Marrone", "Doug Pederson", "Anthony Lynn", "Adam Gase")
fired_team <- c("ATL", "DET", "HOU", "JAX", "PHI", "LAC", "NYJ")
hired_2011 <- c("Jim Harbaugh", "Ron Rivera", "Leslie Frazier", "Jason Garrett", "John Fox", "Pat Shurmur")
hired_team <- c("SFO", "CAR", "MIN", "DAL", "DEN", "CLE")

draft_coach <- draft |>
  mutate(
    fired_20 = if_else(coach %in% fired_2020 & team %in% fired_team, TRUE, FALSE),
    hired_11 = if_else(coach %in% hired_2011 & team %in% hired_team, TRUE, FALSE)
  ) |> group_by(coach, team) |>
  mutate(
    tail_coach = case_when(
      max(year) == 2020 & min(year) == 2011 ~ "Pre-2011 AND Post-2020",
      max(year) == 2020 & fired_20 == FALSE ~ "Post-2020",
      min(year) == 2011 & hired_11 == FALSE ~ "Pre-2011",
      .default = "Full Term"
      ),
    start = if_else(min(year) == 2011, TRUE, FALSE),
    tenure = n_distinct(year),
  ) 

## Tenure Analysis ----
draft_coach |>
  ggplot(aes(x = as.factor(tenure), y = rel_pick_av)) +
  geom_boxplot() +
  facet_wrap(~tail_coach) 

#trends excluding the new hires to reduce variance
draft_coach |>
  filter(!(tail_coach == "Post-2020")) |> 
  group_by(tenure) |>
  summarize(
    win_pct = (mean(win) + (mean(tie)/2))/ 16,
    avg_value = mean(rel_w_av),
    rel_value = mean(rel_pick_av),
    n = n()
  )



## getting fired analysis ----
# no correlation between bad last draft and getting fired
draft_coach |>
  mutate(
    fired = if_else(fired_20 == TRUE | (year == max(year) & !(year == 2020)), TRUE, FALSE)
  ) |> group_by(coach, team, year) |>
  ggplot(aes(x = rel_pick_av, color = fired)) +
  geom_density()

# how does first draft go vs expectation
draft_coach |> 
  mutate(
    hired = if_else(hired_11 == TRUE | (year == min(year) & !(year == 2011)), TRUE, FALSE)
  ) |> group_by(coach, team, year) |>
  ggplot(aes(x = rel_pick_av, color = hired)) +
  geom_density()


# Round 4 ----
## Make our tibble ----

#find state using college_rankings database
college <- levels(as.factor(draft$college))
college_st <- college
c <- read_csv("data/college_rankings.csv") 
colleges <- c$institution
colleges_st <- c$state_abbr

#remove . from abbr
college_st = str_replace_all(college_st, "[\\.]", "")
#Virginia will run before West Virginia
college_st = str_replace_all(college_st, str_c(".*", "West Virginia", ".*"), "WV")
#Notre Dame will match to Notre Dame Namur (CA)
college_st = str_replace_all(college_st, str_c(".*", "Notre Dame", ".*"), "IN")

#fb reference abbreviations
for (i in state.abb) {
  college_st = str_replace_all(
    college_st, 
    str_c(".*", i, ".*"), 
    i)
}

#detect state names
for (i in seq_along(state.name)) {
  college_st = str_replace_all(
    college_st, 
    str_c(".*", state.name[[i]], ".*"), 
    state.abb[[i]])
}

#use college database to find state abbrs
for(i in seq_along(colleges)) {
  college_st = if_else(
    str_detect(colleges[[i]], college_st) == TRUE, 
    colleges_st[[i]],
    college_st
  )
}

#manually enter few that don't fit in algorithm
college_st = str_replace(college_st, "Air Force", "CO")
college_st = str_replace(college_st, "Ala-Birmingham|Jacksonville St|Troy", "AL")
college_st = str_replace(college_st, "Ark-Pine Bluff", "AR")
college_st = str_replace(college_st, "BYU", "UT")
college_st = str_replace(college_st, "Cal Poly-San Luis Obispo|Fresno St|Long Beach CC", "CA")
college_st = str_replace(college_st, "Chadron St", "NE")
college_st = str_replace(college_st, "Fort Hays St|Pittsburg St|Washburn", "KS")
college_st = str_replace(college_st, "Grambling St|La-Monroe|LSU", "LA")
college_st = str_replace(college_st, "Lamar|Midwestern St|Prairie View A&M|SF Austin|SMU|TCU|Tarleton St", "TX")
college_st = str_replace(college_st, "Manitoba|McGill", "Canada")
college_st = str_replace(college_st, "Mars Hill", "NC")
college_st = str_replace(college_st, "Middle Tenn St", "TN")
college_st = str_replace(college_st, "Morgan St|Navy", "MD")
college_st = str_replace(college_st, "Newberry|Presbyterian|The Citadel", "SC")
college_st = str_replace(college_st, "Sioux Falls", "SD")

college_geo <- tibble(college, college_st) 


draft_geo <- draft |>
  left_join(college_geo) 

draft_geo <- draft_geo |>
  filter(college_st %in% state.abb) |>
  mutate(team_st = fct_collapse(team,
      "CA" = c("SFO", "LAR", "LAC"),
      "FL" = c("MIA", "TAM", "JAX"),
      "PA" = c("PHI", "PIT"),
      "NJ" = c("NYG", "NYJ"),
      "OH" = c("CLE", "CIN"),
      "TX" = c("HOU", "DAL"),
      "MD" = c("BAL", "WAS"),
      "GA" = "ATL",
      "NC" = "CAR",
      "MA" = "NWE",
      "NY" = "BUF",
      "LA" = "NOR",
      "TN" = "TEN",
      "NY" = "BUF",
      "MI" = "DET",
      "IN" = "IND",
      "IL" = "CHI",
      "WI" = "GNB",
      "MN" = "MIN",
      "MO" = "KAN",
      "CO" = "DEN",
      "AZ" = "ARI",
      "NV" = "LVR",
      "WA" = "SEA"
    )
  )

## by state ----
#how many players are drafted in the state they played college football, and does it relate to performance?
draft_geo |>
  mutate(
    state_match = if_else(team_st == college_st, TRUE, FALSE)
  ) |> group_by(state_match) |>
  summarize(
    n = n(),
    exp_value = mean(rel_w_av),
    rel_value = mean(rel_pick_av)
  )

draft_geo |>
  mutate(
    state_match = if_else(team_st == college_st, TRUE, FALSE)
  ) |> ggplot(aes(x = rel_pick_av, color = state_match)) +
  geom_density() 
  

## by region ----
#lets expand it to region since borders can differ (Providence and Boston vs Sacramento and San Diego)
state.division
#function for regions
player_id <- draft_geo$player_id
college_region <- draft_geo$college_st
team_region <- draft_geo$team_st

for(i in seq_along(state.abb)) {
  college_region = str_replace_all(
    college_region, state.abb[[i]], as.character(state.division[[i]]))
  team_region = str_replace_all(
    team_region, state.abb[[i]], as.character(state.division[[i]]))
}
region <- tibble(player_id,college_region,team_region)

draft_geo_r <- draft_geo |>
  left_join(region)

#let's analyze
#do teams target "hometown" players more frequently at different times of the draft?"
draft_geo_r <- draft_geo_r |>
  mutate(
    region_match = if_else(team_region == college_region, TRUE, FALSE)
  ) 

draft_geo_r |>
  ggplot(aes(x = round, fill = region_match)) +
  geom_bar()

draft_geo_r |>
  ggplot(aes(x = rel_pick_av, color = region_match)) +
  geom_density() +
  facet_wrap(~round)

draft_geo_r |>
  filter(region_match == TRUE) |>
  ggplot(aes(x = team)) +
  geom_bar(aes(color = region_match, fill = team), width = 0.5) +
  scale_color_nfl(type = "secondary") +
  scale_fill_nfl(alpha = 0.4)


#but some areas of the country produce more draft prospects than others, so we must adjust for the strength of the region the team plays in
region_freq <- draft_geo_r |>
  summarize(
    match_pct = n()/nrow(draft_geo_r),
    .by = college_region
  ) 

exp_match_pct <- draft_geo_r$team_region
v_college_region <- region_freq$college_region
v_match_pct <- as.character(region_freq$match_pct)
for(i in seq_along(v_college_region)) {
    exp_match_pct = str_replace_all(
      exp_match_pct, 
      v_college_region[[i]], 
      v_match_pct[[i]]
    )
}
draft_geo_r <- tibble(draft_geo_r, exp_match_pct)

# hometown bias as % drafted from local region - % of players in draft pool from local region
hometown_bias <- draft_geo_r |>
  group_by(team) |>
  summarize(
    matches = sum(region_match == TRUE),
    match_pct = matches/n(),
    mean_match_pct = mean(as.double(exp_match_pct)),
    rel_match_pct = match_pct - mean_match_pct,
    rel_value = mean(rel_pick_av),
    rel_ht_pick_av = sum(if_else(region_match == TRUE, rel_pick_av, 0))/matches
  ) |> arrange(desc(rel_match_pct))

#there is a slight hometown bias
hometown_bias |>
  summarize(
    mean = mean(rel_match_pct),
    median = median(rel_match_pct),
    sd = sd(rel_match_pct)
  )

#on aggregate
hometown_bias |>
  filter(matches >= quantile(matches, 0.25)) |>
  ggplot(aes(x = rel_match_pct, y = rel_value)) +
  geom_smooth(method = "lm", formula = y~x, alpha = 0.3, color = "grey75") +
  geom_mean_lines(aes(x0 = mean(rel_match_pct), y0 = 0)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "hometown bias",
    y = "average draft pick value over expectation",
    caption = "Data: ESPN, Sports Reference",
    title = "Mapping of relationship between agreement with ESPN and draft success"
  ) 

hometown_bias |>
  filter(matches >= quantile(matches, 0.25)) |>
  ggplot(aes(x = rel_match_pct, y = rel_ht_pick_av)) +
  geom_smooth(method = "lm", formula = y~x, alpha = 0.3, color = "grey75") +
  geom_mean_lines(aes(x0 = mean(rel_match_pct), y0 = 0)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "hometown bias",
    y = "average hometown pick value over expectation",
    caption = "Data: ESPN, Sports Reference",
    title = "Mapping of relationship between agreement with ESPN and draft success"
  ) 
# variance pattern reverse of expected since less hometown bias means smaller sample size



# Looks like they don't become superstars. Let's check that.
draft_geo_r |>
  mutate(
    honors = if_else(
      all_pro > 0, "All-Pro",
      if_else(pro_bowl > 0, "Pro Bowl","none"),
    ),
    honors = fct_relevel(honors, "none", "Pro Bowl")
  ) |> group_by(honors) |>
  summarize(
    n = n(),
    match_pct = sum(region_match)/n()
  )


#Round 5: College Conference/Performance ----
draft_cfb <- draft_geo |>
  mutate(
    conference = fct_collapse(college,
      "ACC" = c("Boston Col.", "Clemson", "Louisville", "Miami (FL)", "North Carolina St.", "North Carolina", "Pittsburgh", "Virginia", "Virginia Tech", "Wake Forest", "Duke", "Florida St.", "Georgia Tech", "Syracuse"),
      "Big Ten" = c("Iowa", "Maryland", "Michigan", "Michigan St.", "Minnesota", "Ohio St.", "Penn St.", "Purdue", "Wisconsin", "Illinois", "Indiana", "Nebraska", "Northwestern", "Rutgers"),
      "Big 12" = c("Baylor", "Iowa St.", "Kansas St.", "Oklahoma", "Oklahoma St.", "Texas Tech", "West Virginia", "Kansas", "TCU", "Texas"),
      "Pac-12" = c("Arizona St.", "Oregon", "Oregon St.", "UCLA", "Utah", "Washington St.", "Arizona", "California", "Colorado", "Stanford", "USC", "Washington"),
      "SEC" = c("Alabama", "Arkansas", "Auburn", "Florida", "Georgia", "Kentucky", "LSU", "Mississippi St.", "Missouri", "Mississippi", "South Carolina", "Tennessee", "Texas A&M"),
      other_level = "Other"
    )
  )

# is there a difference in value?
draft_cfb |>
  summarize(
    n = n(),
    exp_value = mean(rel_w_av),
    rel_value = mean(rel_pick_av),
    .by = conference
  )

draft_cfb |>
  ggplot(aes(x = conference, y = rel_pick_av)) +
  geom_boxplot()

# let's see by position
draft_cfb |>
  ggplot(aes(x = rel_pick_av, y = conference)) +
  geom_boxplot() +
  geom_mean_lines(aes(x0 = median(rel_pick_av))) +
  facet_wrap(~pos_group)

## National Champs Analysis ----
champs <- c("Auburn 2011", "Alabama 2012", "Alabama 2013", "Florida St. 2014", "Ohio St. 2015", "Alabama 2016", "Clemson 2017", "Alabama 2018", "Clemson 2019", "LSU 2020")
draft_cfb <- draft_cfb |>
  mutate(
      cfb_champ = if_else(str_c(college, " ", year) %in% champs , TRUE, FALSE)
  ) 

draft_cfb |> 
  summarize(
    n = n(),
    exp_value = mean(rel_w_av),
    rel_value = mean(rel_pick_av),
    .by = cfb_champ
  )

draft_cfb |>
  ggplot(aes(x = rel_w_av, color = cfb_champ)) +
  geom_density()

draft_cfb |>
  mutate(
    cfb_champ = if_else(str_c(college, " ", year) %in% champs , TRUE, FALSE)
  ) |> ggplot(aes(x = cfb_champ, y = pick)) +
  geom_boxplot()

## let's see how this changes with "big", or "feeder", schools
draft_cfb |>
  summarize() |>
  arrange(desc(draft_picks)) |>
  slice_head(n = 20)


# take feeder schools as top 10 schools by draft picks in decade
feeders <- c("Alabama", "LSU", "Ohio St.", "Florida", "Clemson", "Georgia", "Oklahoma", "Miami (FL)", "Florida St.", "Notre Dame")
draft_cfb <- draft_cfb |>
  mutate(
    feeder = if_else(college %in% feeders, TRUE, FALSE)
  )

feeders <- c("Alabama", "LSU", "Ohio St.", "Florida", "Clemson", "Georgia", "Oklahoma", "Miami (FL)", "Florida St.", "Notre Dame")
draft_cfb <- draft_cfb |>
  mutate(
    feeder = if_else(college %in% feeders, TRUE, FALSE)
  )

draft_cfb |>
  summarize(
    n = n(),
    exp_value = mean(rel_w_av),
    rel_value = mean(rel_pick_av),
    .by = feeder
  ) 

draft_cfb |>
  filter(feeder == TRUE) |>
  ggplot(aes(x = rel_pick_av)) +
  geom_density() +
  facet_wrap(~college)

# Let's expand to overall school size. We'll define schools as big, medium, or small
draft_cfb |>
  summarize(
    draft_picks = n(),
    size = if_else(n() > 30, "big", if_else(n() > 5, "medium", "small")),
    draft_pick = pick,
    .by = college
  ) |> ggplot(aes(x = size, y = draft_pick)) +
  geom_boxplot() +
  scale_y_reverse()

#what about relative value?
draft_cfb |>
  summarize(
    draft_picks = n(),
    size = if_else(n() > 30, "big", if_else(n() > 5, "medium", "small")),
    rel_value = rel_pick_av,
    .by = college
  ) |> ggplot(aes(x = size, y = rel_value)) +
  geom_boxplot() 

# looks like draft pick range the driver of changes, so let's compare by round 
draft_cfb |>
  summarize(
    draft_picks = n(),
    size = if_else(n() > 30, "big", if_else(n() > 5, "medium", "small")),
    rel_value = rel_pick_av,,
    round = round,
    .by = college
  ) |> ggplot(aes(x = rel_value, color = size, fill = size)) +
  geom_density(alpha = 0.3) + 
  facet_wrap(~round)

#and let's see position
draft_cfb |>
  summarize(
    draft_picks = n(),
    size = if_else(n() > 30, "big", if_else(n() > 5, "medium", "small")),
    rel_value = rel_pick_av,
    pos_group = pos_group,
    .by = college
  ) |> ggplot(aes(x = rel_value, color = size, fill = size)) +
  geom_density(alpha = 0.3) + 
  facet_wrap(~pos_group)

# Round 6: Measurables ----
#height
draft |>
  ggplot(aes(x = height, y = rel_pick_av)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  facet_wrap(~pos_group)

#weight
draft |>
  ggplot(aes(x = weight, y = rel_pick_av)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  facet_wrap(~pos_group)

#bmi
draft |>
  mutate(
    bmi = weight * 730 / (height^2)
  ) |>
  ggplot(aes(x = bmi, y = rel_pick_av)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  facet_wrap(~pos_group)

# are players getting bigger or smaller?
draft |>
  mutate(
    bmi = weight * 730 / (height^2)
  ) |>
  ggplot(aes(x = as.factor(year), y = bmi)) +
  geom_boxplot() 
  facet_wrap(~pos_group)


# Round 7: The "Experts" ----
## Setup ----
# To calculate the difference between the media's perception of value and the NFL's, 
# we will use our model of expected value by pick and assume the media "drafts" in the order of their rankings
# calculate weighted difference as ranking plugged into draft pick model - draft pick in draft pick model
draft |>
  mutate(
    diff_rk = pick - ovr_rk,
    w_diff = (4.263 - 0.7171 * log(ovr_rk)) - (4.263 - 0.7171 * log(pick))
  ) |> arrange(w_diff) |> select(player, pick, diff_rk, w_diff)
  
# ovr_rk appears to be a variable where missing values are significant, let's figure out what the NAs mean
draft |> 
  filter(is.na(ovr_rk)) |> 
  arrange(pick) |>
  select(player, year, pick, ovr_rk)

#except for D.J. Hayden and Gareon Conley, it appears NA means the player was not good enough in ESPN's eyes to be ranked
# for our model, we will rank these players 1 higher than the highest (worst) ranking in their class
max_espn <- draft |> summarize(max_rk = max(ovr_rk, na.rm = TRUE), .by = year)

draft |>
  left_join(max_espn) |>
  mutate(
    ovr_rk = if_else(player == "D.J. Hayden", 28, ovr_rk),
    ovr_rk = if_else(player == "Gareon Conley", 16, ovr_rk),
    ovr_rk = if_else(is.na(ovr_rk), max_rk + 1, ovr_rk)
  ) |> arrange(desc(ovr_rk))
  
# With these concerns taken care of, let's form a new tibble
max_espn <- draft |> summarize(max_rk = max(ovr_rk, na.rm = TRUE), .by = year)

draft_espn <- draft |>
  left_join(max_espn) |>
  mutate(
    ovr_rk = if_else(player == "D.J. Hayden", 28, ovr_rk),
    ovr_rk = if_else(player == "Gareon Conley", 16, ovr_rk),
    ovr_rk = if_else(is.na(ovr_rk), max_rk + 1, ovr_rk),
    diff_rk = pick - ovr_rk,
    w_diff = (4.263 - 0.7171 * log(ovr_rk)) - (4.263 - 0.7171 * log(pick)),
  )

#let's see distribution
draft_espn |>
  ggplot(aes(x = w_diff)) +
  geom_histogram(bins = 100)

# makes sense since ESPN players can be surpassed by undrafted players in ranking
# so NFL expectation should be lower than ESPN's leading to a center below 0

#let's see how this shakes out
draft_espn |>
  ggplot(aes(x = w_diff, y = rel_pick_av)) +
  geom_point(alpha = 0.3)

draft_espn |>
  mutate(espn_likes = if_else(w_diff >= median(w_diff), "ESPN", "NFL")) |> 
  ggplot(aes(x = rel_w_av, color = espn_likes, fill = espn_likes)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~pos_group)

#qb has a pretty significant difference
draft_espn |>
  filter(pos_group == "QB") |>
  ggplot(aes(x = w_diff, y = rel_pick_av)) +
  geom_point(alpha = 0.3)

# But is this really accounting for our question? We only want strong differences.
draft_espn |>
  mutate(
    strong_preference = if_else(
      w_diff >= quantile(w_diff, .9), "ESPN",
      if_else(w_diff <= quantile(w_diff, .1), "NFL", "Neutral"))
  ) |> 
  ggplot(aes(x = rel_pick_av, color = strong_preference, fill = strong_preference)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~pos_group)

draft_espn |>
  mutate(
    strong_preference = if_else(
      w_diff >= quantile(w_diff, .9), "ESPN",
      if_else(w_diff <= quantile(w_diff, .1), "NFL", "Neutral"))
  ) |>  
  ggplot(aes(x = strong_preference, y = rel_pick_av)) +
  geom_boxplot() +
  facet_wrap(~pos_group)

# What about teams that ESPN consensus "likes" and "doesn't like", how do they fare?
draft_espn |> 
  summarize(
    consensus_rating = sum(w_diff)/n(),
    avg_value = sum(rel_pick_av, na.rm = TRUE)/n(),
    .by = team
  ) |> ggplot(aes(x = consensus_rating, y = avg_value)) +
  geom_smooth(method = "lm", formula = y~x, alpha = 0.3, color = "grey75") +
  geom_mean_lines(aes(x0 = mean(consensus_rating), y0 = 0)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "agreement with ESPN",
    y = "average draft pick value over expectation",
    caption = "Data: ESPN, Sports Reference",
    title = "Mapping of relationship between agreement with ESPN and draft success"
  ) 

# seems like teams teams that agree more with consensus do better, good for ESPN!
draft_espn |> 
  group_by(team, year) |>
  summarize(
    consensus_rating = sum(w_diff)/n(),
    avg_value = sum(rel_pick_av, na.rm = TRUE)/n()
  ) |> ggplot(aes(x = consensus_rating, y = avg_value)) +
  geom_smooth(method = "loess", formula = y~x, alpha = 0.3, color = "grey75") +
  coord_cartesian(xlim = c(-0.4, 0.2)) +
  geom_point() +
  labs(
    x = "agreement with ESPN",
    y = "average draft pick value over expectation",
    caption = "Data: ESPN, Sports Reference",
    title = "Mapping of relationship between agreement with ESPN and draft success"
  ) 

# Appears strong agreement relates with more success, but strong disagreement does not
# Consensus "steals" more predictive than consensus "reaches"

  



# scratch work ----
## region ----
draft_geo_r |>
  summarize(
    match_pct = sum(region_match)/n(),
    match_value = mean(rel_pick_av, region_match == TRUE),
    .by = team
  ) |> ggplot(aes(x = match_pct, y = rel_value)) +
  geom_smooth(method = "lm", formula = y~x, alpha = 0.3, color = "grey75") +
  geom_mean_lines(aes(x0 = 1, y0 = .5)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "average draft pick value",
    y = "win percentage",
    caption = "Data: Sports Reference",
    title = "Mapping of relationship between draft success and team success"
  ) 


count(team_region, college_region) |>
  ggplot(aes(x = team_region, y = college_region)) +
  geom_tile(aes(fill = n))

draft_geo_r |> group_by(region_match) |>
  summarize(
    n = n(),
    exp_value = mean(rel_w_av),
    rel_value = mean(rel_pick_av)
  )

exp_region_pct <- draft_geo_r |>
  count(college_region) 

draft_geo_r |>
  left_join(exp_region_pct)


draft_geo_r |>
  group_by(team, team_region) |>
  summarize(
    match_pct = sum(region_match)/n()
    
  ) |> arrange(desc(match_pct))


## other ----
draft |>
  ggplot(aes(x = dr_av, y = w_av)) +
  geom_point(alpha = 0.3)

draft |>
  ggplot(aes(x = pos_group, y = rel_w_av)) +
  geom_boxplot()

# mutate w_av by yr
draft |>
  mutate(
    rel_w_av = w_av / (2024 + 10 - year),
    .by = year
  ) |> arrange(desc(rel_w_av)) |> 
  select(player, rel_w_av)

draft |>
  mutate(
    rel_w_av = w_av / (2024 + 10 - year),
    .by = year
  ) |> ggplot(aes(x = rel_w_av)) +
  geom_de

# summary statistics

draft |>
  group_by(year) |>
  summarize(
    mean = mean(w_av),
    median = median(w_av),
    sd = sd(w_av),
    IQR = IQR(w_av)
  )

draft |>
  group_by(year) |>
  summarize(
    mean = mean(rel_w_av),
    median = median(rel_w_av),
    sd = sd(rel_w_av),
    IQR = IQR(rel_w_av)
  )


# let's do the basics
draft |>
  ggplot(aes(x = pick, y = rel_w_av)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~year)

draft |>
  ggplot(aes(x = as.factor(round), y = rel_w_av)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 6)) +
  facet_wrap(~team) +
  element_nfl_logo(team)





draft |>
  summarize(
    started = sum(start_yrs > 3),
    prop_start = started /n()
  )

draft |>
  filter (rel_w_av > 0.99 & rel_w_av < 1.01) |>
  select(player)



draft |>
  summarize(
    avg_rel_w_av = sum(rel_w_av, na.rm = TRUE) / n(),
    .by = team
  ) |> arrange(desc(avg_rel_w_av)) |>
  ggplot(aes(x = team, y= avg_rel_w_av)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x = team, 
                   xend = team, 
                   y = min(avg_rel_w_av), 
                   yend = max(avg_rel_w_av)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip()


draft |>
  group_by(team, year) |>
  summarize(
    avg_value = sum(rel_pick_av, na.rm = TRUE),
    games_won = mean(win)+(mean(tie)/2)
  ) |> arrange(desc(avg_value))|> ggplot(aes(x = avg_value, y = games_won)) +
  geom_point() +
  labs(
    x = "average draft pick value",
    y = "wins",
    caption = "Data: Sports Reference",
    title = "Mapping of relationship between draft success and team success"
  ) 


draft |>
  group_by(team, year) |>
  summarize(
    avg_value = sum(rel_w_av, na.rm = TRUE),
    games_won = mean(win)+(mean(tie)/2)
  ) |> arrange(desc(avg_value))|> ggplot(aes(x = avg_value, y = games_won)) +
  geom_point() +
  labs(
    x = "average draft pick value",
    y = "wins",
    caption = "Data: Sports Reference",
    title = "Mapping of relationship between draft success and team success"
  ) 

## Draft Success and Team Success ----














college_st = str_replace_all(college_st, ".*Florida.*", "Florida")

for (i in state.name) {
  
}


college_st



}



















