import csv
import pandas as pd
import numpy as np
import math

MRkarma = dict()

Femkarma = dict()

master_user_data=pd.read_csv("./user_statistics.csv",low_memory=False)
master_users=set(master_user_data.author.tolist())

df = pd.read_csv("userMonthlyScore.csv")

for index, row in df.iterrows():
    if row.user in master_users:
        if row['numPostsMR'] != 0:
            if row['month'] in MRkarma:
                MRkarma[row['month']].append(row['karmaMR'])
            else:
                MRkarma[row['month']] = [row['karmaMR']]
        if row['numPostsFem'] != 0:
            if row['month'] in Femkarma:
                Femkarma[row['month']].append(row['karmaFem'])
            else:
                Femkarma[row['month']] = [row['karmaFem']]


with open('userMonthlyScoreZScores.csv', 'w') as fp:
    wr = csv.writer(fp, delimiter=',', lineterminator='\n')
    wr.writerow(['month', 'user', 'numPostsMR', 'cumPostsMR', 'karmaMR', 'cumKarmaMR', 'ZScoreMR', 'numPostsFem', 'cumPostsFem', 'karmaFem', 'cumKarmaFem', 'ZScoreFem'])
    for index, row in df.iterrows():
        month = row['month']
        if row.user in master_users:
            if row['numPostsMR'] != 0:
                MRscore = row['karmaMR']
                avergeScoreMR = np.mean(np.array(MRkarma[month]))
                stdScoreMR = np.std(np.array(MRkarma[month]))
                zScoreMR = (MRscore - avergeScoreMR)/(stdScoreMR/math.sqrt(len(MRkarma[month])))
            else:
                zScoreMR = 0

            if row['numPostsFem'] != 0:
                Femscore = row['karmaFem']
                avergeScoreFem = np.mean(np.array(Femkarma[month]))
                stdScoreFem = np.std(np.array(Femkarma[month]))
                zScoreFem = (Femscore - avergeScoreFem)/(stdScoreFem/math.sqrt(len(Femkarma[month])))
            else:
                zScoreFem = 0
            wr.writerow([row['month'], row['user'], row['numPostsMR'], row['cumPostsMR'], row['karmaMR'], row['cumKarmaMR'], zScoreMR, row['numPostsFem'], row['cumPostsFem'], row['karmaFem'], row['cumKarmaFem'], zScoreFem])
