from bs4 import BeautifulSoup
import re
import pandas as pd
import numpy as np
import cloudscraper
from io import StringIO

def process_game(i, game):
    split_score = re.split('(?<=\d),\s(?!I\sMarried)', re.sub('\sOT$', '', game['score']))
    team1 = re.match('^.*(?=\s[-\d]+$)', split_score[0])[0]
    team2 = re.match('^.*(?=\s[-\d]+$)', re.sub(' Tie$', '', split_score[1]))[0]

    line_split = [el for el in re.split(
        '\\n', game['line']) if re.search(':', el)]
    player_lines = {re.split('(?<!Guardians):\s', el, maxsplit=1)[0]: re.split('(?<!True):\s', el, maxsplit=1)[
        1] for el in line_split if not re.match('^Bonuses:', el)}

    team1_players = [re.split('\s(?=-?\d)', player)
                        for player in re.split('(?<=\d),\s', player_lines[team1])]
    team2_players = [re.split('\s(?=-?\d)', player)
                        for player in re.split('(?<=\d),\s', player_lines[team2])]

    if len(team1_players[0]) == 4:
        team1_players = pd.DataFrame(team1_players, columns=[
                                        'player', 'gets', 'negs', 'pts']).assign(team=team1)
        team2_players = pd.DataFrame(team2_players, columns=[
                                        'player', 'gets', 'negs', 'pts']).assign(team=team2)
        player_stats = pd.concat([team1_players, team2_players])
        player_stats[['gets', 'negs', 'pts']] = player_stats[[
            'gets', 'negs', 'pts']].astype(int)
        player_stats['player'] = player_stats['player'].str.strip()
    else:
        team1_players = pd.DataFrame(team1_players, columns=[
                                        'player', 'powers', 'gets', 'negs', 'pts']).assign(team=team1)
        team2_players = pd.DataFrame(team2_players, columns=[
                                        'player', 'powers', 'gets', 'negs', 'pts']).assign(team=team2)
        player_stats = pd.concat([team1_players, team2_players])
        player_stats[['powers', 'gets', 'negs', 'pts']] = player_stats[[
            'powers', 'gets', 'negs', 'pts']].astype(int)
        player_stats['player'] = player_stats['player'].str.strip()
        

    bonuses = [re.split('(?<=\d),\s(?!I\sMarried)', el[9:])
                for el in line_split if re.match('^Bonuses:', el)][0]
    boni = []
    for team in bonuses:
        m = re.match(r'(.*)\s([\d]+)\s([\d]+)\s([\d\.]+)$', team.strip())
        bon = {
            'team': m.group(1),
            'bonuses_heard': m.group(2),
            'bonus_pts': m.group(3),
            'PPB': m.group(4)
        }
        boni.append(bon)
    bonuses = pd.DataFrame(boni)
    bonuses[['bonuses_heard', 'bonus_pts']] = bonuses[[
        'bonuses_heard', 'bonus_pts']].astype(int)

    if 'powers' not in player_stats.columns:
        team_stats = pd.DataFrame([
            {
                'team': team,
                'gets': sum(player_stats[player_stats['team'] == team]['gets']),
                'negs': sum(player_stats[player_stats['team'] == team]['negs']),
                'tu_pts': sum(player_stats[player_stats['team'] == team]['pts'])
            }
            for team in [team1, team2]
        ]).merge(bonuses, on='team')
    else:
        team_stats = pd.DataFrame([
            {
                'team': team,
                'powers': sum(player_stats[player_stats['team'] == team]['powers']),
                'gets': sum(player_stats[player_stats['team'] == team]['gets']),
                'negs': sum(player_stats[player_stats['team'] == team]['negs']),
                'tu_pts': sum(player_stats[player_stats['team'] == team]['pts'])
            }
            for team in [team1, team2]
        ]).merge(bonuses, on='team')

    team_stats['total_pts'] = team_stats['tu_pts'] + \
        team_stats['bonus_pts']
    team_stats['opponent'] = team_stats['team'].values[::-1]
    player_stats['report_game_id'] = i
    team_stats['report_game_id'] = i

    return (player_stats, team_stats)

def process_game_cmst(i, game):
    split_score = re.split('(?<=\d),\s(?!I\sMarried)', re.sub('\sOT$', '', game['score']))
    print(split_score)
    team1 = re.match('^.*(?=\s[-\d]+$)', split_score[0])[0]
    team2 = re.match('^.*(?=\s[-\d]+$)', re.sub(' Tie$', '', split_score[1]))[0]

    line_split = [el for el in re.split(
        '\\n', game['line']) if re.search(':', el)]
    player_lines = {re.split('(?<!Guardians):\s', el, maxsplit=1)[0]: re.split('(?<!True):\s', el, maxsplit=1)[
        1] for el in line_split if not re.match('^Bonuses:', el)}
    print(player_lines.keys())
    team1_players = [re.split('\s(?=-?\d)', player)
                        for player in re.split('(?<=\d),\s', player_lines[team1])]
    team2_players = [re.split('\s(?=-?\d)', player)
                        for player in re.split('(?<=\d),\s', player_lines[team2])]

    team1_players = pd.DataFrame(team1_players, columns=[
                                    'player', 'powers', 'gets', 'pts']).assign(team=team1)
    team2_players = pd.DataFrame(team2_players, columns=[
                                    'player', 'powers', 'gets', 'pts']).assign(team=team2)
    player_stats = pd.concat([team1_players, team2_players])
    player_stats['negs'] = 0
    player_stats[['powers', 'gets', 'pts']] = player_stats[[
        'powers', 'gets', 'pts']].astype(int)
    player_stats['player'] = player_stats['player'].str.strip()
    bonuses = [re.split('(?<=\d),\s(?!I\sMarried)', el[9:])
                for el in line_split if re.match('^Bonuses:', el)][0]
    print(bonuses)
    boni = []
    for team in bonuses:
        m = re.match(r'(.*)\s([\d]+)\s([\d]+)\s([\d\.]+)$', team.strip())
        bon = {
            'team': m.group(1),
            'bonuses_heard': m.group(2),
            'bonus_pts': m.group(3),
            'PPB': m.group(4)
        }
        boni.append(bon)
    bonuses = pd.DataFrame(boni)
    bonuses[['bonuses_heard', 'bonus_pts']] = bonuses[[
        'bonuses_heard', 'bonus_pts']].astype(int)

    team_stats = pd.DataFrame([
        {
            'team': team,
            'powers': sum(player_stats[player_stats['team'] == team]['powers']),
            'gets': sum(player_stats[player_stats['team'] == team]['gets']),
            'negs': 0,
            'tu_pts': sum(player_stats[player_stats['team'] == team]['pts'])
        }
        for team in [team1, team2]
    ]).merge(bonuses, on='team')

    team_stats['total_pts'] = team_stats['tu_pts'] + \
        team_stats['bonus_pts']
    team_stats['opponent'] = team_stats['team'].values[::-1]
    player_stats['report_game_id'] = i
    team_stats['report_game_id'] = i

    return (player_stats, team_stats)

def process_game_yf(i, game, url):
    raw_player_stats = pd.read_html(StringIO(str(game['table'])), header=0)
    if raw_player_stats[0].shape[1] == 11:
        team1_players = raw_player_stats[0].iloc[:, 0:5].melt(
            id_vars=['TUH', '10', '-5', 'Tot'],
            var_name='team',
            value_name='player'
        )
        team2_players = raw_player_stats[0].iloc[:, 6:12].melt(
            id_vars=['TUH.1', '10.1', '-5.1', 'Tot.1'],
            var_name='team',
            value_name='player').rename(columns={'TUH.1': 'TUH', '10.1': '10', '-5.1': '-5', 'Tot.1': 'Tot'})
        player_stats = pd.concat([team1_players, team2_players])
        player_stats = player_stats[~player_stats['player'].isin(
            ['Total,', 'Total'])]
        player_stats = player_stats[~player_stats['player'].isna()]
        player_stats = player_stats.rename(
            columns={'10': 'gets', '-5': 'negs', 'Tot': 'pts'})

        bonuses = re.split('\;\s', game['bonuses'][9:])
        bonuses = pd.DataFrame(
            [
                {
                    'team': re.search('.*(?=\s\d+\sheard,)', team)[0].strip(),
                    'bonuses_heard': int(re.search('\d+(?=\sheard,)', team)[0]),
                    'bonus_pts': int(re.search('\d+(?=\spts,)', team)[0])
                }
                for team in bonuses
            ],
            columns=['team', 'bonuses_heard', 'bonus_pts']
        )
        bonuses[['bonuses_heard', 'bonus_pts']] = bonuses[[
            'bonuses_heard', 'bonus_pts']].astype(int)
        team_stats = pd.DataFrame([
            {
                'team': team.strip(),
                'gets': sum(player_stats[player_stats['team'] == team]['gets']),
                'negs': sum(player_stats[player_stats['team'] == team]['negs']),
                'tu_pts': sum(player_stats[player_stats['team'] == team]['pts'])
            }
            for team in np.unique(player_stats['team'])
        ])

        team_stats = team_stats.merge(bonuses, on='team')
        team_stats['total_pts'] = team_stats['tu_pts'] + \
            team_stats['bonus_pts']
        team_stats['opponent'] = team_stats['team'].values[::-1]
        player_stats['report_game_id'] = i
        team_stats['report_game_id'] = i

        return player_stats, team_stats
    elif url in (
        'https://hsquizbowl.org/db/tournaments/7985/stats/combined/games/',
        'https://hsquizbowl.org/db/tournaments/7905/stats/all_games/games/',
        'https://hsquizbowl.org/db/tournaments/8536/stats/combined/games/',
        'https://hsquizbowl.org/db/tournaments/8327/stats/combined/games/',
        'https://hsquizbowl.org/db/tournaments/8328/stats/playoffs/games/',
        'https://hsquizbowl.org/db/tournaments/8340/stats/all_games/games/',
        'https://hsquizbowl.org/db/tournaments/8627/stats/combined/games/',
        'https://hsquizbowl.org/db/tournaments/8698/stats/prelims/games/',
        'https://hsquizbowl.org/db/tournaments/9151/stats/all_games/games/',
        'https://hsquizbowl.org/db/tournaments/9045/stats/combined_all/games/',
        'https://hsquizbowl.org/db/tournaments/9129/stats/final_%28all_games%29/games/'
    ):
        team1_players = raw_player_stats[0].iloc[:, 0:6].melt(
            id_vars=['TUH', '0', '10', '-5', 'Tot'],
            var_name='team',
            value_name='player'
        )
        team2_players = raw_player_stats[0].iloc[:, 7:13].melt(
            id_vars=['TUH.1', '0.1', '10.1', '-5.1', 'Tot.1'],
            var_name='team',
            value_name='player'
        ).rename(columns={'TUH.1': 'TUH', '0.1': '0', '10.1': '10', '-5.1': '-5', 'Tot.1': 'Tot'})
        player_stats = pd.concat([team1_players, team2_players])
        player_stats = player_stats[~player_stats['player'].isin(
            ['Total,', 'Total'])]
        player_stats = player_stats[~player_stats['player'].isna()]
        player_stats = player_stats.rename(
            columns={'10': 'gets', '-5': 'negs', 'Tot': 'pts'})

        bonuses = re.split('\;\s', game['bonuses'][9:])
        bonuses = pd.DataFrame(
            [
                {
                    'team': re.search('.*(?=\s\d+\sheard,)', team)[0].strip(),
                    'bonuses_heard': int(re.search('\d+(?=\sheard,)', team)[0]),
                    'bonus_pts': int(re.search('\d+(?=\spts,)', team)[0])
                }
                for team in bonuses
            ],
            columns=['team', 'bonuses_heard', 'bonus_pts']
        )
        bonuses[['bonuses_heard', 'bonus_pts']] = bonuses[[
            'bonuses_heard', 'bonus_pts']].astype(int)
        team_stats = pd.DataFrame([
            {
                'team': team.strip(),
                'gets': sum(player_stats[player_stats['team'] == team]['gets']),
                'negs': sum(player_stats[player_stats['team'] == team]['negs']),
                'tu_pts': sum(player_stats[player_stats['team'] == team]['pts'])
            }
            for team in np.unique(player_stats['team'])
        ])

        team_stats = team_stats.merge(bonuses, on='team')
        team_stats['total_pts'] = team_stats['tu_pts'] + \
            team_stats['bonus_pts']
        team_stats['opponent'] = team_stats['team'].values[::-1]
        player_stats['report_game_id'] = i
        team_stats['report_game_id'] = i

        return player_stats, team_stats
    else:
        team1_players = raw_player_stats[0].iloc[:, 0:6].melt(
            id_vars=['TUH', '15', '10', '-5', 'Tot'],
            var_name='team',
            value_name='player'
        )
        team2_players = raw_player_stats[0].iloc[:, 7:13].melt(
            id_vars=['TUH.1', '15.1', '10.1', '-5.1', 'Tot.1'],
            var_name='team',
            value_name='player'
        ).rename(columns={'TUH.1': 'TUH', '15.1': '15', '10.1': '10', '-5.1': '-5', 'Tot.1': 'Tot'})
        player_stats = pd.concat([team1_players, team2_players])
        player_stats = player_stats[~player_stats['player'].isin(
            ['Total,', 'Total'])]
        player_stats = player_stats[~player_stats['player'].isna()]
        player_stats = player_stats.rename(
            columns={'15': 'powers', '10': 'gets', '-5': 'negs', 'Tot': 'pts'})

        bonuses = re.split('(?<=PPB)\;\s', game['bonuses'][9:])
        bonuses = pd.DataFrame(
            [
                {
                    'team': re.search('.*(?=\s\d+\sheard,)', team)[0].strip(),
                    'bonuses_heard': int(re.search('\d+(?=\sheard,)', team)[0]),
                    'bonus_pts': int(re.search('\d+(?=\spts,)', team)[0])
                }
                for team in bonuses
            ],
            columns=['team', 'bonuses_heard', 'bonus_pts']
        )
        bonuses[['bonuses_heard', 'bonus_pts']] = bonuses[[
            'bonuses_heard', 'bonus_pts']].astype(int)
        team_stats = pd.DataFrame([
            {
                'team': team.strip(),
                'powers': sum(player_stats[player_stats['team'] == team]['powers']),
                'gets': sum(player_stats[player_stats['team'] == team]['gets']),
                'negs': sum(player_stats[player_stats['team'] == team]['negs']),
                'tu_pts': sum(player_stats[player_stats['team'] == team]['pts'])
            }
            for team in np.unique(player_stats['team'])
        ])

        team_stats = team_stats.merge(bonuses, on='team')
        team_stats['total_pts'] = team_stats['tu_pts'] + \
            team_stats['bonus_pts']
        team_stats['opponent'] = team_stats['team'].values[::-1]

        player_stats['report_game_id'] = i
        team_stats['report_game_id'] = i

        return player_stats, team_stats

def process_game_yf_cmst(i, game, url):
    raw_player_stats = pd.read_html(StringIO(str(game['table'])), header=0)
    team1_players = raw_player_stats[0].iloc[:, 0:5].melt(
        id_vars=['TUH', '20', '10', 'Tot'],
        var_name='team',
        value_name='player'
    )
    team2_players = raw_player_stats[0].iloc[:, 6:12].melt(
        id_vars=['TUH.1', '20.1', '10.1', 'Tot.1'],
        var_name='team',
        value_name='player'
    ).rename(columns={'TUH.1': 'TUH', '20.1': '20', '10.1': '10', 'Tot.1': 'Tot'})
    player_stats = pd.concat([team1_players, team2_players])
    player_stats['negs'] = 0
    player_stats = player_stats[~player_stats['player'].isin(
        ['Total,', 'Total'])]
    player_stats = player_stats[~player_stats['player'].isna()]
    player_stats = player_stats.rename(
        columns={'20': 'powers', '10': 'gets', 'Tot': 'pts'})

    bonuses = re.split('\;\s', game['bonuses'][9:])
    bonuses = pd.DataFrame(
        [
            {
                'team': re.search('.*(?=\s\d+\sheard,)', team)[0].strip(),
                'bonuses_heard': int(re.search('\d+(?=\sheard,)', team)[0]),
                'bonus_pts': int(re.search('\d+(?=\spts,)', team)[0])
            }
            for team in bonuses
        ],
        columns=['team', 'bonuses_heard', 'bonus_pts']
    )
    bonuses[['bonuses_heard', 'bonus_pts']] = bonuses[[
        'bonuses_heard', 'bonus_pts']].astype(int)
    team_stats = pd.DataFrame([
        {
            'team': team.strip(),
            'powers': sum(player_stats[player_stats['team'] == team]['powers']),
            'gets': sum(player_stats[player_stats['team'] == team]['gets']),
            'negs': 0,
            'tu_pts': sum(player_stats[player_stats['team'] == team]['pts'])
        }
        for team in np.unique(player_stats['team'])
    ])

    team_stats = team_stats.merge(bonuses, on='team')
    team_stats['total_pts'] = team_stats['tu_pts'] + \
        team_stats['bonus_pts']
    team_stats['opponent'] = team_stats['team'].values[::-1]
    player_stats['report_game_id'] = i
    team_stats['report_game_id'] = i

    return player_stats, team_stats
        

def process_url(url):
    scraper = cloudscraper.create_scraper()
    r = scraper.get(url)
    soup = BeautifulSoup(r.content, features="lxml")

    if len([el.get_text() for el in soup.find_all(['span', 'h5']) if re.search('YellowFruit', el.get_text())]) == 0:
        def chunks(lst, n):
            for i in range(0, len(lst), n):
                yield lst[i:i + n]

        if len([el for el in soup.find_all("font") if re.search('^Round\s[-\d]+$', el.get_text())]) == 0:
            rounds = {'All Rounds': [el.get_text() for el in soup.find_all(
                "font") if not re.search('\sby\sforfeit', el.get_text())]}
        else:
            rounds = {}
            cur_round = []
            cur_round_name = ''
            for el in soup.find_all("font"):
                if re.search('^Round\s[-\d]+$', el.get_text()):
                    rounds[cur_round_name] = cur_round
                    cur_round = []
                    cur_round_name = el.get_text()
                elif not re.search('\sby\sforfeit', el.get_text()):
                    cur_round.append(el.get_text())
            rounds[cur_round_name] = cur_round

        cleaned_rounds = {}
        for k, v in rounds.items():
            if k != '':
                clv = list(chunks(v, 2))
                cleaned_rounds[k] = [{'score': game[0], 'line': game[1]}
                                        for game in clv if len(game) == 2]
                
        all_player_stats = []
        all_team_stats = []
        for k, v in cleaned_rounds.items():
            player_stats = pd.concat([process_game(i, game)[0] for i, game in enumerate(v)])
            team_stats = pd.concat([process_game(i, game)[1] for i, game in enumerate(v)])

            player_stats['round'] = k
            team_stats['round'] = k

            player_stats['report_game_id'] = player_stats['report_game_id'].map(lambda x: f"{k}-{x}")
            team_stats['report_game_id'] = team_stats['report_game_id'].map(lambda x: f"{k}-{x}")

            all_player_stats.append(player_stats)
            all_team_stats.append(team_stats)

        all_player_stats = pd.concat(all_player_stats)
        if 'powers' in all_player_stats.columns:
            all_player_stats = all_player_stats[[
                'report_game_id', 'player', 'team', 'round', 'powers', 'gets', 'negs', 'pts']]
        else:
            all_player_stats = all_player_stats[[
                'report_game_id', 'player', 'team', 'round', 'gets', 'negs', 'pts']]
        all_team_stats = pd.concat(all_team_stats)
        if 'powers' in all_team_stats.columns:
            all_team_stats = all_team_stats[[
                'report_game_id', 'team', 'round', 'opponent', 'powers', 'gets', 'negs', 'bonuses_heard', 'bonus_pts', 'total_pts']]
        else:
            all_team_stats = all_team_stats[[
                'report_game_id', 'team', 'round', 'opponent', 'gets', 'negs', 'bonuses_heard', 'bonus_pts', 'total_pts']]

    else:
        bonuses = soup.find_all(string=re.compile("Bonuses"))
        tables = soup.find_all('table', width='70%')[1:]
        scores = [el.get_text() for el in soup.find_all(
            "h3") if not re.search("by forfeit$", el.get_text())]

        games = {}
        for i in range(len(bonuses)):
            games[scores[i].replace('\n\xa0\xa0\xa0\xa0\n', '')] = {
                'table': tables[i], 'bonuses': bonuses[i]}

        rounds = {}
        cur_round = []
        cur_round_name = ''
        headers = soup.find_all(["h3", "h2"])
        for el in [el.get_text().replace('Round 8.1', 'Round 81').replace('Round 8.2', 'Round 82') for el in headers]:
            if re.search('^Round\s[-\d]+$', el):
                rounds[cur_round_name] = cur_round
                cur_round = []
                cur_round_name = el
            elif not re.search('\sby\sforfeit', el):
                cur_round.append(el.replace(
                    '\n\xa0\xa0\xa0\xa0\n', ''))
        rounds[cur_round_name] = cur_round
        cleaned_rounds = {k: [games[game] for game in v]
                            for k, v in rounds.items() if k != ''}

        all_player_stats = []
        all_team_stats = []
        for k, v in cleaned_rounds.items():
            player_stats = pd.concat([process_game_yf(i, game, url)[0] for i, game in enumerate(v)])
            team_stats = pd.concat([process_game_yf(i, game, url)[1] for i, game in enumerate(v)])

            player_stats['round'] = k
            team_stats['round'] = k

            player_stats['report_game_id'] = player_stats['report_game_id'].map(lambda x: f"{k}-{x}")
            team_stats['report_game_id'] = team_stats['report_game_id'].map(lambda x: f"{k}-{x}")

            all_player_stats.append(player_stats)
            all_team_stats.append(team_stats)

        all_player_stats = pd.concat(all_player_stats)
        if 'powers' in all_player_stats.columns:
            all_player_stats = all_player_stats[[
                'report_game_id', 'player', 'team', 'round', 'powers', 'gets', 'negs', 'pts']]
            all_player_stats[['powers', 'gets', 'negs', 'pts']] = all_player_stats[[
                'powers', 'gets', 'negs', 'pts']].astype(int)
        else:
            all_player_stats = all_player_stats[[
                'report_game_id', 'player', 'team', 'round', 'gets', 'negs', 'pts']]
            all_player_stats[['gets', 'negs', 'pts']] = all_player_stats[[
                'gets', 'negs', 'pts']].astype(int)
        all_team_stats = pd.concat(all_team_stats)
        if 'powers' in all_team_stats.columns:
            all_team_stats = all_team_stats[[
                'report_game_id', 'team', 'round', 'opponent', 'powers', 'gets', 'negs', 'bonuses_heard', 'bonus_pts', 'total_pts']]
            all_team_stats[['powers', 'gets', 'negs', 'total_pts']] = all_team_stats[[
                'powers', 'gets', 'negs', 'total_pts']].astype(int)
        else:
            all_team_stats = all_team_stats[[
                'report_game_id', 'team', 'round', 'opponent', 'gets', 'negs', 'bonuses_heard', 'bonus_pts', 'total_pts']]
        all_team_stats[['gets', 'negs', 'total_pts']] = all_team_stats[[
            'gets', 'negs', 'total_pts']].astype(int)
    return {'player_stats': all_player_stats, 'team_stats': all_team_stats}
        
# urls = pd.read_csv('tournaments.csv')

# player = []
# team = []
# for i, row in urls.iterrows():
#     print(row['url'])
#     player_stats, team_stats = process_url(row['url'])
#     player_stats['set'] = row['set']
#     team_stats['set'] = row['set']

#     player.append(player_stats)
#     team.append(team_stats)

# pd.concat(player).to_csv('player-stats.csv')
# pd.concat(team).to_csv('team-stats.csv')
