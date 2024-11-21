
#load packages
library(tidyverse)
library(nflplotR)


#load and join datasets to main "draft" data frame
draft_prospects <- read_csv("data/nfl_draft_prospects.csv", guess_max = 13000) |>
  select(
    player_id, player_name, school_name, school_abbr, pick, link, traded, trade_note, pick, weight, height, pos_rk, ovr_rk, grade
  ) |> rename(
    pick_rd = pick, college_mascot = school_name, college = school_abbr, 
  )
draft_fbr_11_20 <- readxl::read_xlsx("data/nfldraft_11_20.xlsx")

draft <- draft_fbr_11_20 |>
  inner_join(draft_prospects, join_by(player == player_name))

view(draft)
skimr::skim_without_charts(draft)

# clean variables, NA values
naniar::gg_miss_var(draft)


# when w_av = NA, player never played a game in the NFL. For purposes, 
# I would rather consider these values as 0s since 0 gp and 0 value is the same as saying they never played
draft |>
  filter(is.na(w_av)) |>
  arrange(desc(gp)) |>
  select(player, gp, w_av)

draft <- draft |>
  
  
  


# univariate analysis of dr_av
draft |>
  ggplot(aes(x = dr_av)) +
  geom_density()



# mutate weighted dr_av and w_av by yr
draft |>
  group_by (year) |>
  summarize(exp_w_av = mean(w_av), exp_dr_av = mean(dr_av))





































