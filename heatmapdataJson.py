import pandas as pd
import csv
import datetime

master_user_data=pd.read_csv("./user_statistics.csv",low_memory=False)
master_users=set(master_user_data.author.tolist())

monthNums = dict()
monthMRKarma = dict()
monthMRNumPosts = dict()
cumMRKarma = dict()
cumMRPosts = dict()
monthFemKarma = dict()
monthFemNumPosts = dict()
cumFemKarma = dict()
cumFemPosts = dict()
users = set()

'''monthNums = {(3,2008):1, (4,2008):2, (5,2008):3, (6,2008):4, (7,2008):5, (8,2008):6, (9,2008):7, (10,2008):8, (11,2008):9, (12,2008):10,
             (1,2009):11, (2,2009):12, (3,2009):13, (4,2009):14, (5,2009):15, (6,2009):16, (7,2009):17, (8,2009):18, (9,2009):19, (10,2009):20,(11,2009):21, (12,2009):22,
             (1,2010):23, (2,2010):24, (3,2010):25, (4,2010):26, (5,2010):27, (6,2010):28, (7,2010):29, (8,2010):30, (9,2010):31, (10,2010):32,(11,2010):33, (12,2010):34,
             (1,2011):35, (2,2011):36, (3,2011):37, (4,2011):38, (5,2011):39, (6,2011):40, (7,2011):41, (8,2011):42, (9,2011):43, (10,2010):44,(11,2011):45, (12,2011):46,
             (1,2012):47, (2,2012):48, (3,2012):49, (4,2012):50, (5,2012):51, (6,2012):52, (7,2012):53, (8,2012):54, (9,2012):55, (10,2012):56,(11,2012):57, (12,2012):58,
             (1,2013):59, (2,2013):60, (3,2013):61, (4,2013):62, (5,2013):63, (6,2013):64, (7,2013):65, (8,2013):66, (9,2013):67, (10,2013):68,(11,2013):69, (12,2013):70,
             (1,2014):71, (2,2014):72, (3,2014):73, (4,2014):74, (5,2014):75, (6,2014):76, (7,2014):77, (8,2014):78, (9,2014):79, (10,2014):80,(11,2014):81, (12,2014):82,
             (1,2015):83, (2,2015):84, (3,2015):85, (4,2015):86, (5,2015):87, (6,2015):88, (7,2015):89, (8,2015):90, (9,2015):91, (10,2015):92,(11,2015):93, (12,2015):94,
             (1,2016):95, (2,2016):96, (3,2016):97, (4,2016):98, (5,2016):99, (6,2016):100, (7,2016):101
             }'''
monthNums = { 1:(3,2008), 2:(4,2008), 3:(5,2008), 4:(6,2008), 5:(7,2008), 6:(8,2008), 7:(9,2008), 8:(10,2008), 9:(11,2008), 10:(12,2008),
             11:(1,2009), 12:(2,2009), 13:(3,2009), 14:(4,2009), 15:(5,2009), 16:(6,2009), 17:(7,2009), 18:(8,2009), 19:(9,2009), 20:(10,2009),21:(11,2009), 22:(12,2009),
             23:(1,2010), 24:(2,2010), 25:(3,2010), 26:(4,2010), 27:(5,2010), 28:(6,2010), 29:(7,2010), 30:(8,2010), 31:(9,2010), 32:(10,2010),33:(11,2010), 34:(12,2010),
             35:(1,2011), 36:(2,2011), 37:(3,2011), 38:(4,2011), 39:(5,2011), 40:(6,2011), 41:(7,2011), 42:(8,2011), 43:(9,2011), 44:(10,2010),45:(11,2011), 46:(12,2011),
             47:(1,2012), 48:(2,2012), 49:(3,2012), 50:(4,2012), 51:(5,2012), 52:(6,2012), 53:(7,2012), 54:(8,2012), 55:(9,2012), 56:(10,2012),57:(11,2012), 58:(12,2012),
             59:(1,2013), 60:(2,2013), 61:(3,2013), 62:(4,2013), 63:(5,2013), 64:(6,2013), 65:(7,2013), 66:(8,2013), 67:(9,2013), 68:(10,2013),69:(11,2013), 70:(12,2013),
             71:(1,2014), 72:(2,2014), 73:(3,2014), 74:(4,2014), 75:(5,2014), 76:(6,2014), 77:(7,2014), 78:(8,2014), 79:(9,2014), 80:(10,2014),81:(11,2014), 82:(12,2014),
             83:(1,2015), 84:(2,2015), 85:(3,2015), 86:(4,2015), 87:(5,2015), 88:(6,2015), 89:(7,2015), 90:(8,2015), 91:(9,2015), 92:(10,2015),93:(11,2015), 94:(12,2015),
             95:(1,2016), 96:(2,2016), 97:(3,2016), 98:(4,2016), 99:(5,2016), 100:(6,2016), 101:(7,2016)
             }

df = pd.read_csv("allcommentsnobots.csv")

for index, row in df.iterrows():
    user = row['author']
    if user in master_users:
        score = row['score']
        timestamp = row['created_utc']
        subreddit = row['subreddit']

        timestamp = (str(timestamp))
        '''if timestamp == 'nan':
            continue   '''
        try:
            dt = datetime.datetime.utcfromtimestamp(int(float(timestamp)))
        except:
            #print(timestamp)
            continue
        monthKey = (dt.month,dt.year)
        users.add(user)

        #Mens Rights Post
        if subreddit == 'MensRights':
            #Did the user already comment this month
            if (user, monthKey) in monthMRKarma:
                #update the month score for the user
                monthMRKarma[(user, monthKey)] += score
                monthMRNumPosts[(user, monthKey)] += 1
            else:
                #create the month score for the user
                monthMRKarma[(user, monthKey)] = score
                monthMRNumPosts[(user, monthKey)] = 1
        #Feminism post
        else:
            #Did user already comment this month
            if user in monthFemKarma:
                #update score
                monthFemKarma[(user, monthKey)] += score
                monthFemNumPosts[(user, monthKey)] += 1
            else:
                #create score
                monthFemKarma[(user, monthKey)] = score
                monthFemNumPosts[(user, monthKey)] = 1


with open('userMonthlyScore.csv', 'w', encoding='utf-8') as fp:
    wr = csv.writer(fp, delimiter=',', lineterminator='\n')
    wr.writerow(['month', 'user', 'numPostsMR', 'cumPostsMR', 'karmaMR', 'cumKarmaMR', 'numPostsFem', 'cumPostsFem', 'karmaFem', 'cumKarmaFem'])
    for user in users:
        count = 1
        inMonth = False
        while count <= 101:
            monthYear = monthNums[count]
            #Get MR Data
            if (user,monthYear) in monthMRKarma:
                inMonth = True
                karmaMR = monthMRKarma[(user,monthYear)]
                numPostsMR = monthMRNumPosts[(user,monthYear)]
                if user in cumMRKarma:
                    cumKarmaMR = cumMRKarma[user] + karmaMR
                    cumMRKarma[user] = cumKarmaMR
                    cumPostsMR = cumMRPosts[user] + numPostsMR
                    cumMRPosts[user] = cumPostsMR
                else:
                    cumKarmaMR = karmaMR
                    cumMRKarma[user] = cumKarmaMR
                    cumPostsMR = numPostsMR
                    cumMRPosts[user] = numPostsMR
            else:
                karmaMR = 0
                numPostsMR = 0
                if user in cumMRKarma:
                    cumKarmaMR = cumMRKarma[user]
                    cumPostsMR = cumMRPosts[user]
                else:
                    cumKarmaMR = 0
                    cumPostsMR = 0
            #Get Fem Data
            if (user,monthYear) in monthFemKarma:
                inMonth = True
                karmaFem = monthFemKarma[(user,monthYear)]
                numPostsFem = monthFemNumPosts[(user,monthYear)]
                if user in cumFemKarma:
                    cumKarmaFem = cumFemKarma[user] + karmaFem
                    cumFemKarma[user] = cumKarmaFem
                    cumPostsFem = cumFemPosts[user] + numPostsFem
                    cumFemPosts[user] = cumPostsFem
                else:
                    cumKarmaFem = karmaFem
                    cumFemKarma[user] = cumKarmaFem
                    cumPostsFem = numPostsFem
                    cumFemPosts[user] = cumPostsFem
            else:
                karmaFem = 0
                numPostsFem = 0
                if user in cumFemKarma:
                    cumKarmaFem = cumFemKarma[user]
                    cumPostsFem = cumFemPosts[user]
                else:
                    cumKarmaFem = 0
                    cumPostsFem = 0
            if inMonth:
                wr.writerow([count, user, numPostsMR, cumPostsMR, karmaMR, cumKarmaMR, numPostsFem, cumPostsFem, karmaFem, cumKarmaFem])
            count += 1
            inMonth = False
