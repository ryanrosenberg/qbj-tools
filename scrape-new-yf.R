library(tidyverse)
library(rvest)
library(reticulate)
use_python('/home/ryanr345/.pyenv/shims/python')
source_python('scrape-page.py')

page <- scrape_page(
  'https://hsquizbowl.org/db/tournaments/9907/stats/all_games/games/'
  # 'https://hsquizbowl.org/db/tournaments/9801/stats/all_games/games/'
) %>%
  read_html()

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

match_meta <- page %>%
  html_elements('div[id]') %>%
  html_attr('id') %>%
  tibble(el = .) %>%
  mutate(
    round = ifelse(
      str_detect(el, "Round\\-\\d"),
      el %>%
        str_replace('-', ' '),
      NA
    )
  ) %>%
  fill(round) %>%
  filter(str_detect(el, "Round\\-\\d", negate = T)) %>%
  rename(id = el)

html_element(page, css = '#Match_1130\\~WilliamsBDartmouthA')

scorelines <- page %>%
  html_elements('h3')

tossup_tables <- page %>%
  html_elements('.boxScoreTable') %>%
  map(html_table)

bonus_tables <- page %>%
  html_table() %>%
  keep(\(x) x[[1, 1]] == "Bonuses")

matches <- match_meta %>%
  mutate(
    scorelines = scorelines %>% html_text()
  ) %>%
  filter(str_detect(scorelines, 'by forfeit$', negate = T)) %>%
  mutate(
    player_stats = map(tossup_tables, process_player_stats),
    team_stats = map2(tossup_tables, bonus_tables, process_team_stats)
  )

list(
  "player_stats" = matches %>%
    select(id, round, player_stats) %>%
    unnest(player_stats),
  "team_stats" = matches %>%
    select(id, round, team_stats) %>%
    unnest(team_stats)
)
