library(tidyverse)
library(rvest)
library(reticulate)
use_python('/opt/homebrew/opt/python@3/libexec/bin/python')
source_python('scrape-page.py')

page <- scrape_page(
  'https://hsquizbowl.org/db/tournaments/9801/stats/all_games/games/'
) %>%
  read_html()

match_meta <- page %>%
  html_elements('div[id]') %>%
  html_attr('id') %>%
  tibble(el = .) %>%
  separate(el, c("type", "id"), sep = '-') %>%
  mutate(round = ifelse(type == "Round", id, NA)) %>%
  fill(round) %>%
  filter(type != "Round") %>%
  select(-type)

tossup_tables <- page %>%
  html_elements('.boxScoreTable') %>%
  map(html_table)

bonus_tables <- page %>%
  html_elements('table[width="50%"]') %>%
  map(html_table)

process_player_stats <- function(df) {
  if (ncol(df) == 6) {
    column_names <- c("player", "tuh", "powers", "gets", "negs", "pts")
  } else {
    column_names <- c("player", "tuh", "gets", "negs", "pts")
  }

  df %>%
    set_names(column_names) %>%
    mutate(team = ifelse(tuh == "TUH", player, NA), .before = player) %>%
    fill(team) %>%
    filter(tuh != "TUH", tuh != "")
}

process_team_stats <- function(tossups_df, bonuses_df) {
  if (ncol(tossups_df) == 6) {
    column_names <- c("player", "tuh", "powers", "gets", "negs", "pts")
  } else {
    column_names <- c("player", "tuh", "gets", "negs", "pts")
  }

  tossup_stats <- tossups_df %>%
    set_names(column_names) %>%
    filter(tuh %in% c("", "TUH")) %>%
    mutate(team = ifelse(tuh == "TUH", player, NA), .before = player) %>%
    fill(team) %>%
    filter(tuh != "TUH") %>%
    select(-player, -tuh) %>%
    rename(tossup_pts = pts) %>%
    mutate(opponent = rev(team), .after = team)

  bonus_stats <- bonuses_df %>%
    set_names(c("team", "bonuses_heard", "bonus_pts", "PPB")) %>%
    filter(PPB != "PPB") %>%
    select(-PPB)

  left_join(tossup_stats, bonus_stats, by = join_by(team)) %>%
    mutate(
      total_pts = as.numeric(tossup_pts) + as.numeric(bonus_pts),
      .after = bonus_pts
    )
}


matches <- match_meta %>%
  mutate(
    player_stats = map(tossup_tables, process_player_stats),
    team_stats = map2(tossup_tables, bonus_tables, process_team_stats)
  )

return(
  list(
    "player_stats" = matches %>%
      select(id, round, player_stats) %>%
      unnest(player_stats),
    "team_stats" = matches %>%
      select(id, round, team_stats) %>%
      unnest(team_stats)
  )
)
