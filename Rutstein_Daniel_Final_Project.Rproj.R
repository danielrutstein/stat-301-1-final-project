#load packages
library(tidyverse)
library(nflplotR)
library(tidymodels)

# Database setup ---- 
# run this
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
    rel_pick_av = exp_pick_av/mean(exp_pick_av),
    avg_w_av = if_else(career_length > 0, w_av / career_length, 0)
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
    active = if_else(played_to == 2024, TRUE, FALSE)
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
draft |>
  mutate(
    exp_pick_av = (rel_w_av / (4.263 - 0.7171 * log(pick))),
    rel_pick_av = exp_pick_av/mean(exp_pick_av)
  ) |> select(player, rel_w_av, rel_pick_av) |>
  arrange(desc(rel_pick_av))


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


## Team Success ----
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
  summarize(
    avg_value = sum(rel_w_av, na.rm = TRUE)/n(),
    win_pct = (mean(win) + (mean(tie)/2))/ 16,
    .by = team
  ) |> arrange(desc(avg_value))|> ggplot(aes(x = avg_value, y = win_pct)) +
  geom_abline(slope = -1, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
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
  ) |> arrange(desc(avg_value))|> ggplot(aes(x = avg_value, y = win_pct)) +
  geom_abline(slope = -1, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
  geom_mean_lines(aes(x0 = 1, y0 = .5)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "average draft pick value",
    y = "win percentage",
    caption = "Data: Sports Reference",
    title = "Mapping of relationship between draft success and team success"
  ) 



# scratch work ----

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





























