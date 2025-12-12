library(tidyverse)
library(DBI)

bp <- dbConnect(RSQLite::SQLite(), "database.db")

buzzes <- dbGetQuery(
  bp,
  "
  SELECT b.*, 
  pq.question_number, p.name as player, t.name as team
  from buzz b
  left join player p on b.player_id = p.id
  left join team t on t.id = p.team_id
  left join tossup tu on b.tossup_id = tu.id
  left join question q on tu.question_id = q.id 
  left join packet_question pq on pq.question_id = q.id 
  where b.game_id = 1
  "
) %>%
  as_tibble()

dbReadTable(bp, "bonus_part") %>%
  as_tibble()

bonuses <- dbGetQuery(
  bp,
  "
  SELECT bpd.*, t.name as team, bp.part_number, pq.question_number
  from bonus_part_direct bpd
  left join team t on t.id = bpd.team_id
  left join bonus_part bp on bpd.bonus_part_id = bp.id
  left join bonus b on bp.bonus_id = b.id
  left join question q on b.question_id = q.id 
  left join packet_question pq on pq.question_id = q.id 
  where bpd.game_id = 1
  "
) %>%
  as_tibble()


process_buzz <- function(player, team, buzz_position, value) {
  return(
    list(
      "buzz_position" = list(
        "word_index" = buzz_position
      ),
      "player" = list(
        "name" = player
      ),
      "team" = list(
        "name" = team
      ),
      "result" = list(
        "value" = value
      )
    )
  )
}

process_bonus <- function(values) {
  return(
    list(
      list("controlled_points" = values[[1]]),
      list("controlled_points" = values[[2]]),
      list("controlled_points" = values[[3]])
    )
  )
}

bonuses %>%
  split(.$question_number) %>%
  unname() %>%
  map(
    \(x) {
      x %>%
        arrange(part_number) %>%
        pull(value) %>%
        process_bonus()
    }
  )

buzzes %>%
  split(.$question_number) %>%
  unname() %>%
  map(
    \(x) {
      x %>%
        select(player, team, buzz_position, value) %>%
        pmap(process_buzz)
    }
  )

process_game <- function(game_id, tournament_slug) {
  if (!dir.exists('test_qbjs')) {
    dir.create('test_qbjs')
  }

  if (!dir.exists(glue::glue('test_qbjs/{tournament_slug}'))) {
    dir.create(glue::glue('test_qbjs/{tournament_slug}'))
    dir.create(glue::glue('test_qbjs/{tournament_slug}/game_files'))
  }
  buzzes <- dbGetQuery(
    bp,
    glue::glue(
      "
    SELECT b.*, 
    pq.question_number, p.name as player, t.name as team
    from buzz b
    left join player p on b.player_id = p.id
    left join team t on t.id = p.team_id
    left join tossup tu on b.tossup_id = tu.id
    left join question q on tu.question_id = q.id 
    left join packet_question pq on pq.question_id = q.id 
    where b.game_id = {game_id}
    "
    )
  ) %>%
    as_tibble()

  bonuses <- dbGetQuery(
    bp,
    glue::glue(
      "
    SELECT bpd.*, t.name as team, bp.part_number, pq.question_number
    from bonus_part_direct bpd
    left join team t on t.id = bpd.team_id
    left join bonus_part bp on bpd.bonus_part_id = bp.id
    left join bonus b on bp.bonus_id = b.id
    left join question q on b.question_id = q.id 
    left join packet_question pq on pq.question_id = q.id 
    where bpd.game_id = {game_id}
    "
    )
  ) %>%
    as_tibble()

  bonus_num <- 1

  match_questions <- map(
    1:20,
    function(qnum) {
      tu <- list(
        "question_number" = qnum,
        "buzzes" = buzzes %>%
          filter(question_number == qnum) %>%
          select(player, team, buzz_position, value) %>%
          pmap(process_buzz),
        "tossup_question" = list(
          "parts" = 1,
          "type" = "tossup",
          "question_number" = qnum
        )
      )

      if (
        qnum %in%
          buzzes$question_number &
          buzzes %>%
            filter(question_number == qnum) %>%
            pull(value) %>%
            max() >
            0
      ) {
        tu$bonus = list(
          "question" = list(
            "parts" = 3,
            "type" = "bonus",
            "question_number" = bonus_num
          ),
          "parts" = list(
            bonuses %>%
              filter(question_number == bonus_num) %>%
              pull(value) %>%
              process_bonus()
          )
        )
        bonus_num = bonus_num + 1
      }
      return(
        tu
      )
    }
  )

  team_bonuses <- dbGetQuery(
    bp,
    "
    SELECT bpd.*, t.name as team, bp.part_number, pq.question_number
    from bonus_part_direct bpd
    left join team t on t.id = bpd.team_id
    left join bonus_part bp on bpd.bonus_part_id = bp.id
    left join bonus b on bp.bonus_id = b.id
    left join question q on b.question_id = q.id 
    left join packet_question pq on pq.question_id = q.id 
    where bpd.game_id = 1
    "
  ) %>%
    as_tibble() %>%
    group_by(team) %>%
    summarize(
      bonus_points = sum(value)
    )

  team_buzzes <- dbGetQuery(
    bp,
    "
      SELECT b.*, 
      pq.question_number, p.name as player, t.name as team
      from buzz b
      left join player p on b.player_id = p.id
      left join team t on t.id = p.team_id
      left join tossup tu on b.tossup_id = tu.id
      left join question q on tu.question_id = q.id 
      left join packet_question pq on pq.question_id = q.id 
      where b.game_id = 1
      "
  ) %>%
    as_tibble() %>%
    count(player, team, value)

  teams <- dbGetQuery(
    bp,
    glue::glue(
      "
    SELECT
    t1.name as team1,
    t2.name as team2
    from game g
    left join team t1 on g.team_one_id = t1.id
    left join team t2 on g.team_two_id = t2.id
    where g.id = {game_id}
    "
    )
  ) %>%
    gather(spot, team)

  match_teams <- map(
    teams$team,
    \(x) {
      list(
        "bonus_points" = team_bonuses %>%
          filter(team == x) %>%
          pull(bonus_points) %>%
          pluck(1),
        "match_players" = pmap(
          team_buzzes %>%
            filter(team == x) %>%
            select(player, team),
          \(player, team) {
            list(
              "player" = list(
                "name" = player
              ),
              "answer_counts" = team_buzzes %>%
                set_names(c("tplayer", "tteam", "value", "n")) %>%
                filter(tteam == team, tplayer == player) %>%
                select(value, n) %>%
                pmap(
                  \(value, n) {
                    list(
                      "answer" = list(
                        "value" = value
                      ),
                      "number" = n
                    )
                  }
                )
            )
          }
        ),
        "team" = list(
          "name" = x,
          "players" = map(
            team_buzzes %>%
              filter(team == x) %>%
              pull(player) %>%
              unique(),
            \(y) list("name" = y)
          )
        )
      )
    }
  )

  meta <- dbGetQuery(
    bp,
    glue::glue(
      "
    SELECT r.number as round, p.name as packet
    from game g
    left join round r on g.round_id = r.id
    left join packet p on r.packet_id = p.id
      where g.id = {game_id}
    "
    )
  )

  qbj <-
    list(
      "tossups_read" = 20,
      "match_teams" = match_teams,
      "match_questions" = match_questions,
      "_round" = meta$round[[1]],
      "packets" = meta$packet[[1]]
    ) %>%
    jsonlite::toJSON(auto_unbox = T)

  write_file(
    qbj,
    glue::glue(
      'test_qbjs/{tournament_slug}/game_files/Round_{meta$round[[1]]}_{teams$team[[1]]}_{teams$team[[2]]}.qbj'
    )
  )
}

dbGetQuery(
  bp,
  "SELECT 
  g.id as game_id ,
  t.slug as tournament_slug
    FROM game g
    LEFT JOIN round r on g.round_id = r.id
    LEFT JOIN tournament t on t.id = r.tournament_id"
) %>%
  pwalk(process_game)


process_packet <- function(packet_id, set_name, set_edition_name, packet_name) {
  if (!dir.exists('test_packets')) {
    dir.create('test_packets')
  }

  if (!dir.exists(glue::glue('test_packets/{set_name}'))) {
    dir.create(glue::glue('test_packets/{set_name}'))
    dir.create(glue::glue('test_packets/{set_name}/editions'))
  }

  if (
    !dir.exists(glue::glue(
      'test_packets/{set_name}/editions/{set_edition_name}'
    ))
  ) {
    dir.create(glue::glue(
      'test_packets/{set_name}/editions/{set_edition_name}'
    ))
    dir.create(glue::glue(
      'test_packets/{set_name}/editions/{set_edition_name}/packet_files'
    ))
  }

  tossups <- dbGetQuery(
    bp,
    glue::glue(
      "
    SELECT 
    distinct t.question, t.answer, q.metadata
    from 
    question_set_edition qse 
    left join packet p on qse.id = p.question_set_edition_id
    left join round r on r.packet_id = p.id
    left join tournament tourn on r.tournament_id = tourn.id
    left join packet_question pq on pq.packet_id = p.id
    left join question q on pq.question_id = q.id
    left join tossup t on t.question_id = q.id
    where p.id = {packet_id}
    "
    )
  ) %>%
    as_tibble() %>%
    filter(!is.na(question))

  bonuses <- dbGetQuery(
    bp,
    glue::glue(
      "
    SELECT 
    b.leadin, bp.part, bp.answer, bp.difficulty_modifier, q.metadata
    from question_set_edition qse 
    left join packet p on qse.id = p.question_set_edition_id
    left join round r on r.packet_id = p.id
    left join tournament tourn on r.tournament_id = tourn.id
    left join packet_question pq on pq.packet_id = p.id
    left join question q on pq.question_id = q.id
    left join bonus b on b.question_id = q.id    
    left join bonus_part bp on bp.bonus_id = b.id
    where p.id = {packet_id}
    "
    )
  ) %>%
    as_tibble() %>%
    filter(
      !is.na(leadin)
    )

  packet <- list(
    "tossups" = tossups %>%
      pmap(
        \(question, answer, metadata) {
          list(
            "question" = question,
            "answer" = answer,
            "metadata" = metadata
          )
        }
      ),
    "bonuses" = bonuses %>%
      split(.$leadin) %>%
      unname() %>%
      map(
        \(x) {
          list(
            "leadin" = x$leadin[[1]],
            "parts" = x$part,
            "answers" = x$answer,
            "values" = list(10, 10, 10),
            "difficultyModifiers" = x$difficulty_modifier,
            "metadata" = x$metadata[[1]]
          )
        }
      )
  ) %>%
    jsonlite::toJSON(auto_unbox = T)

  write_file(
    packet,
    glue::glue(
      'test_packets/{set_name}/editions/{set_edition_name}/packet_files/{packet_name}.qbj'
    )
  )
}
dbGetQuery(
  bp,
  "SELECT 
  p.id as packet_id,
  qs.slug as set_name,
  qse.slug as set_edition_name,
  p.name as packet_name
    from question_set_edition qse
    left join question_set qs on qse.question_set_id = qs.id
    left join packet p on qse.id = p.question_set_edition_id"
) %>%
  as_tibble() %>%
  pwalk(
    process_packet
  )
