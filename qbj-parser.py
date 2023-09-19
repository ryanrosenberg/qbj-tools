import glob
import pandas as pd
import json
import re

qbjs = glob.glob('../arcadia-app/qbjs/arcadia/*.qbj')


def get_buzzes(match_q, q_num):
    buzzes = []

    for buzz in match_q['buzzes']:
        buzzes.append(
            {
                'tossup': q_num,
                'player': buzz['player']['name'],
                'team': buzz['team']['name'],
                'buzz_position': buzz['buzz_position']['word_index'],
                'value': buzz['result']['value']
            }
        )
    return pd.DataFrame(buzzes)


def get_bonuses(match_q, q_num):
    if 'bonus' in match_q:
        return pd.DataFrame(
            [
                {
                    'tossup': q_num,
                    'bonus': match_q['bonus']['question']['question_number'],
                    'part1': match_q['bonus']['parts'][0]['controlled_points'],
                    'part2': match_q['bonus']['parts'][1]['controlled_points'],
                    'part3': match_q['bonus']['parts'][2]['controlled_points']
                }
            ]
        )
    else:
        return pd.DataFrame(
            []
        )


def get_question_stats(qbj, num):
    print(f"Processing file {num + 1} of {len(qbjs)}")
    with open(qbj, "rb") as f:
        test = json.load(f)
    num_round = int(re.search(r'(?<=Round_)\d+(?=_)', qbj).group(0))

    buzzes = []

    for i, q in enumerate(test['match_questions']):
        buzzes.append(get_buzzes(q, i))

    buzzes = pd.concat(buzzes)
    buzzes['game_id'] = num
    buzzes['packet'] = num_round

    bonuses = []

    for i, q in enumerate(test['match_questions']):
        bonuses.append(get_bonuses(q, i))

    bonuses = pd.concat(bonuses).merge(
        buzzes[buzzes['value'].isin((15, 10))].drop_duplicates(
            ['tossup', 'team']
        )
    )
    bonuses['game_id'] = num
    bonuses['packet'] = num_round

    bonuses = bonuses.dropna(subset=['bonus'])

    return ({'buzzes': buzzes, 'bonuses': bonuses})


stats = []
for i, qbj in enumerate(qbjs):
    stats.append(get_question_stats(qbj, i))

all_buzzes = pd.concat([s['buzzes'] for s in stats])
all_bonuses = pd.concat([s['bonuses'] for s in stats])

print(all_buzzes)
