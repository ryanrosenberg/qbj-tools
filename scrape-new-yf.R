library(tidyverse)
library(rvest)
library(reticulate)
library(magrittr)
# use_python('/home/ryanr345/.pyenv/shims/python')

use_python('/opt/homebrew/bin/python3')
source_python('scrape-page.py')

page <- scrape_page(
  'https://hsquizbowl.org/db/tournaments/10010/stats/results/games/'
  # 'https://hsquizbowl.org/db/tournaments/9801/stats/all_games/games/'
) %>%
  read_html()

old_page <- scrape_page(
  'https://hsquizbowl.org/db/tournaments/9879/stats/complete/games/'
) %>%
  read_html()

clean_team_name <- function(str) {
  str %>%
    str_sub(end = min(25, nchar(str))) %>%
    trimws()
}

process_player_stats <- function(df, team1, team2) {
  if (ncol(df) == 6) {
    column_names <- c("player", "tuh", "powers", "gets", "negs", "pts")
  } else {
    column_names <- c("player", "tuh", "gets", "negs", "pts")
  }

  df <- df %>%
    set_names(column_names) %>%
    mutate(team = ifelse(tuh == "TUH", player, NA), .before = player) %>%
    fill(team) %>%
    filter(tuh != "TUH", tuh != "")

  df %>%
    mutate(
      team = ifelse(
        team %>%
          trimws() %>%
          str_remove_all('\\.\\.\\.$') %>%
          str_detect(clean_team_name(team1)),
        team1,
        team2
      )
    )
}

process_team_stats <- function(tossups_df, bonuses_df, team1, team2) {
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
    rename(tossup_pts = pts)

  bonus_stats <- bonuses_df %>%
    set_names(c("team", "bonuses_heard", "bonus_pts", "PPB")) %>%
    filter(PPB != "PPB") %>%
    select(-PPB)

  df <- left_join(tossup_stats, bonus_stats, by = join_by(team)) %>%
    mutate(
      total_pts = as.numeric(tossup_pts) + as.numeric(bonus_pts),
      .after = bonus_pts
    )

  df %>%
    mutate(
      team = ifelse(
        team %>%
          trimws() %>%
          str_remove_all('\\.\\.\\.$') %>%
          str_detect(clean_team_name(team1)),
        team1,
        team2
      )
    ) %>%
    relocate(team, .before = 1) %>%
    mutate(opponent = rev(team), .after = team)
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
  filter(str_detect(el, "[rR]ound\\-\\d", negate = T)) %>%
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

tossup_tables

matches <- match_meta %>%
  mutate(
    scorelines = scorelines %>% html_text() %>% trimws()
  ) %>%
  filter(
    str_detect(scorelines, 'by forfeit$', negate = T),
    str_detect(scorelines, 'Not played$', negate = T)
  ) %>%
  mutate(
    team1_name = str_extract(scorelines, ".+(?=\\d+,)") %>%
      str_remove_all("[-\\d]+$") %>%
      trimws(),
    scorelines = str_remove(scorelines, team1_name) %>%
      str_remove('[^,]*,\\s'),
    team2_name = scorelines %>%
      str_remove("\\s\\(OT\\)$") %>%
      str_remove("[-\\d]+$") %>%
      trimws()
  ) %>%
  select(id, round, team1_name, team2_name)

matches <- matches %>%
  mutate(
    player_stats = pmap(
      list(
        tossup_tables,
        matches$team1_name,
        matches$team2_name
      ),
      process_player_stats
    ),
    team_stats = pmap(
      list(
        tossup_tables,
        bonus_tables,
        matches$team1_name,
        matches$team2_name
      ),
      process_team_stats
    )
  ) %>%
  rename(report_game_id = id)

matches

matches %>%
  select(report_game_id, round, player_stats) %>%
  unnest(player_stats) %>%
  mutate(across(tuh:pts, as.numeric))

matches %>%
  select(report_game_id, round, player_stats) %>%
  unnest(player_stats) %>%
  mutate(across(tuh:pts, as.numeric)) %>%
  distinct(player, team) %>%
  arrange(player)

list(
  "player_stats" = matches %>%
    select(report_game_id, round, player_stats) %>%
    unnest(player_stats) %>%
    mutate(across(tuh:pts, as.numeric)),
  "team_stats" = matches %>%
    select(report_game_id, round, team_stats) %>%
    unnest(team_stats) %>%
    mutate(across(gets:total_pts, as.numeric))
)

standings_page <- 'https://hsquizbowl.org/db/tournaments/10013/stats/championships/' %>%
  scrape_page() %>%
  read_html()

num_brackets <- standings_page %>%
  html_element(xpath = '/html/body/div/div[1]/div[4]/div') %>%
  html_children() %>%
  html_name() %>%
  keep(\(x) x %in% c("div", "table")) %>%
  tibble(el = .) %>%
  mutate(div = ifelse(el == "div", row_number(), NA)) %>%
  fill(div) %>%
  filter(div == 1, el == "table") %>%
  nrow()

num_brackets

tables <- standings_page %>%
  html_table() %>%
  discard(~ .[[1, 2]] == "Individuals")

full_tbl <- tables[1:num_brackets] %>%
  imap(
    \(x, y) {
      x %>%
        janitor::row_to_names(1) %>%
        filter(Rank != "Rank") %>%
        mutate(
          bracket = y
        )
    }
  ) %>%
  reduce(rbind)

full_tbl
standings_page
