library(tidyverse)
library(jsonlite)

## Folder where your qbj files are
qbjs <- Sys.glob('../../../Downloads/bhsu-berkeley/*.qbj')

get_buzzes <- function(match_q, q_num){
  map_df(match_q$buzzes,
         function(x){
           tibble(player = pluck(x, "player", "name"),
                  team = pluck(x, "team", "name"),
                  buzz_position = pluck(x, "buzz_position", "word_index"),
                  value = pluck(x, "result", "value")) 
         }) %>% 
    mutate(tossup = q_num, .before = 1)
}

get_bonuses <- function(match_q, q_num){
  tibble(tossup = q_num,
         bonus = pluck(match_q, "bonus", "question", "question_number"),
         part1 = pluck(match_q, "bonus", "parts", 1, "controlled_points"),
         part2 = pluck(match_q, "bonus", "parts", 2, "controlled_points"),
         part3 = pluck(match_q, "bonus", "parts", 3, "controlled_points"))
}

get_question_stats <- function(qbj, num){
  print(glue::glue("Processing file {num} of {length(qbjs)}"))
  test <- read_json(qbj)
  round <- as.numeric(str_extract(qbj, "(?<=Round_)\\d+(?=_)"))
  
  buzzes <- test %>% 
    pluck("match_questions") %>% 
    imap_dfr(get_buzzes) %>% 
    mutate(game_id = num, packet = round, .before = 1)
  
  bonuses <- test %>% 
    pluck("match_questions") %>% 
    imap_dfr(get_bonuses) %>% 
    left_join(buzzes %>% 
                filter(value %in% c(15, 10)) %>% 
                distinct(tossup, team)) %>% 
    relocate(team, .after = bonus) %>% 
    mutate(game_id = num, packet = round, .before = 1) %>% 
    filter(!is.na(bonus))
  
  return(list("buzzes" = buzzes,
              "bonuses" = bonuses))
}

arcadia_question_stats <- imap(qbjs, get_question_stats)

map_df(arcadia_question_stats, "buzzes") %>% 
  write_csv('../stats/bhsat/bhsat-combined-buzzes.csv')

map_df(arcadia_question_stats, "bonuses") %>% 
  write_csv('../stats/bhsat/bhsat-combined-bonuses.csv')




