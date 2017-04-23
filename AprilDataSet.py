
# coding: utf-8

# In[94]:

import pandas as pd
import time
import numpy as np
from datetime import datetime
from IPython.display import display


# In[2]:

tic = time.time()
all_data=pd.read_json("/scratch/madhavas/author_analuysis/femmr.json")
toc = time.time()-tic
print "Data frame loading completed in ",round(toc,2)," seconds."


# In[3]:

all_data.shape

def logged_apply(g, func, *args, **kwargs):
    step_percentage = 100. / len(g)
    import sys
    sys.stdout.write('apply progress:   0%')
    sys.stdout.flush()

    def logging_decorator(func):
        def wrapper(*args, **kwargs):
            progress = wrapper.count * step_percentage
            sys.stdout.write('\033[D \033[D' * 4 + format(progress, '3.0f') + '%')
            sys.stdout.flush()
            wrapper.count += 1
            return func(*args, **kwargs)
        wrapper.count = 0
        return wrapper

    logged_func = logging_decorator(func)
    res = g.apply(logged_func, *args, **kwargs)
    sys.stdout.write('\033[D \033[D' * 4 + format(100., '3.0f') + '%' + '\n')
    sys.stdout.flush()
    return res

# In[4]:

master_author_stats=pd.read_csv("/scratch/madhavas/author_analuysis/user_statistics.csv")


# In[5]:

crossposter_data=all_data[all_data.author.isin(master_author_stats.author.tolist())]


# In[6]:

crossposter_data.shape


# In[11]:

start_date= datetime.strptime('2005-06-01', '%Y-%m-%d')
end_date= datetime.strptime('2016-07-01', '%Y-%m-%d')
from calendar import monthrange
from datetime import datetime, timedelta
def monthdelta(d2, d1):
    delta = 0
    while True:
        mdays = monthrange(d1.year, d1.month)[1]
        d1 += timedelta(days=mdays)
        if d1 <= d2:
            delta += 1
        else:
            break
    return delta


# In[12]:

#%% Useful functions
def ymdToUnixTime(y,m,d):
    """ Function to convert month day year to unix timestamp"""
    t = datetime.date(year=y,day=d,month=m)
    t_u = int(time.mktime(t.timetuple()))
    return t,t_u

def dtFromUnixTime(ut):
    """ Function to return date time object from unix time"""
    return datetime.utcfromtimestamp(ut)


# In[13]:

tic = time.time()
""" NEED TO CONVERT DATA TYPES --- SOME VALUES ARE SHOWING UP AS UNICODE STRINGS""" 
crossposter_data[u'created_date']=crossposter_data[u'created_utc'].map(lambda x: dtFromUnixTime(float(x)))
crossposter_data[u'score'] = crossposter_data[u'score'].map(lambda x: int(x))
crossposter_data[u'ups'] = crossposter_data[u'ups'].map(lambda x: int(x))
toc = time.time()-tic
print "Data frame creation completed in ",round(toc,2)," seconds."


# In[14]:

tic = time.time()
crossposter_data["month_num"] = logged_apply (crossposter_data.created_date, lambda x: monthdelta(x, start_date))
toc = time.time()-tic
print "Month number calculated in ",round(toc,2)," seconds."


# In[15]:

crossposter_data.head()


# In[16]:

month_level_gp=crossposter_data.groupby(["month_num"])


# In[17]:

month_frame=pd.DataFrame(month_level_gp.agg({"author":[len,lambda x: len(np.unique(x))]}).to_records())


# In[29]:

month_frame.columns=["month_num","posts_in_month","authors_in_month"]


# In[18]:

group_level_gp=crossposter_data.groupby(["subreddit","month_num"])


# In[19]:

group_frame=pd.DataFrame(group_level_gp.agg({"author":[len,lambda x: len(np.unique(x))]}).to_records())


# In[49]:

group_frame.columns=["subreddit","month_num","posts_in_month","authors_in_month"]
group_pivoted_frame=group_frame.pivot_table(index="month_num", columns="subreddit", values=["posts_in_month","authors_in_month"])
group_pivoted_frame[group_pivoted_frame.isnull()]=0
group_pivoted_frame.reset_index(inplace=True)
group_pivoted_frame.columns=["month_num","feminism_posts_in_month","mensrights_posts_in_month", "feminism_authors_in_month","mensrights_authors_in_month"]


# In[50]:

group_pivoted_frame.head()


# In[20]:

gp=crossposter_data.groupby(["author","month_num","subreddit"])


# In[21]:

user_frame=pd.DataFrame(gp.agg({"score":[np.median,np.mean, np.sum], "author":len}).to_records())


# In[22]:

user_frame.columns=["author","month_num","subreddit","median","mean","total_score","number_of_posts"]


# In[23]:

user_pivoted_frame=user_frame.pivot_table(index=["author","month_num"], columns="subreddit", values=["median","mean","total_score","number_of_posts"])


# In[24]:

user_pivoted_frame[user_pivoted_frame.isnull()]=0


# In[25]:

user_pivoted_frame.reset_index(inplace=True)


# In[32]:

user_pivoted_frame.columns=["author","month_num","feminism_median","mensrights_median","feminism_mean","mensrights_mean","feminism_total_score","mensrights_total_score","feminism_number_of_posts","mensrights_number_of_posts"]


# In[40]:

first_level_join=pd.merge(user_pivoted_frame, month_frame, how="left", left_on ="month_num", right_on="month_num")


# In[53]:

non_lagged_frame=pd.merge(first_level_join, group_pivoted_frame, how="left", left_on =["month_num"], right_on=["month_num"],)


# In[180]:




# In[277]:

def get_prev_month(x, complete_frame):
    first_lag_frame = complete_frame[(complete_frame.author==x.author) & (complete_frame.month_num ==x.month_num - 1)]  [[ 'author', 'month_num', 'feminism_median', 'mensrights_median', 'feminism_mean', 'mensrights_mean', 'feminism_total_score', 'mensrights_total_score', 'feminism_number_of_posts', 'mensrights_number_of_posts' ]]
    first_lag_frame.columns=['author','month_num','lag_1_feminism_median','lag_1_mensrights_median','lag_1_feminism_mean','lag_1_mensrights_mean','lag_1_feminism_total_score','lag_1_mensrights_total_score','lag_1_feminism_number_of_posts','lag_1_mensrights_number_of_posts']
    feminism_delta_median =x.feminism_median - first_lag_frame.lag_1_feminism_median
    mensrights_delta_median =x.mensrights_median - first_lag_frame.lag_1_mensrights_median
    feminism_delta_mean =x.feminism_mean - first_lag_frame.lag_1_feminism_mean
    mensrights_delta_mean =x.mensrights_mean - first_lag_frame.lag_1_mensrights_mean
    feminism_delta_total_score =x.feminism_total_score - first_lag_frame.lag_1_feminism_total_score
    mensrights_delta_total_score =x.mensrights_total_score - first_lag_frame.lag_1_mensrights_total_score
    feminism_delta_number_of_posts =x.feminism_number_of_posts - first_lag_frame.lag_1_feminism_number_of_posts
    mensrights_delta_number_of_posts =x.mensrights_number_of_posts - first_lag_frame.lag_1_mensrights_number_of_posts
   
    second_lag_frame=complete_frame[(complete_frame.author==x.author) & (complete_frame.month_num ==x.month_num-2)][['author','month_num','feminism_median','mensrights_median','feminism_mean','mensrights_mean','feminism_total_score','mensrights_total_score','feminism_number_of_posts','mensrights_number_of_posts']]
    second_lag_frame.columns=['author','month_num','lag_2_feminism_median','lag_2_mensrights_median','lag_2_feminism_mean','lag_2_mensrights_mean','lag_2_feminism_total_score','lag_2_mensrights_total_score','lag_2_feminism_number_of_posts','lag_2_mensrights_number_of_posts']
    third_lag_frame =complete_frame[(complete_frame.author==x.author) & (complete_frame.month_num ==x.month_num-3)][['author','month_num','feminism_median','mensrights_median','feminism_mean','mensrights_mean','feminism_total_score','mensrights_total_score','feminism_number_of_posts','mensrights_number_of_posts']]
    third_lag_frame.columns=['author','month_num','lag_3_feminism_median','lag_3_mensrights_median','lag_3_feminism_mean','lag_3_mensrights_mean','lag_3_feminism_total_score','lag_3_mensrights_total_score','lag_3_feminism_number_of_posts','lag_3_mensrights_number_of_posts']
    first_merge=pd.merge(first_lag_frame, second_lag_frame, how="outer", left_on="author", right_on ="author")
    second_merge=pd.merge(first_merge, third_lag_frame, how="outer", left_on="author", right_on ="author")
    return_frame= second_merge[['lag_1_feminism_median','lag_1_mensrights_median','lag_1_feminism_mean','lag_1_mensrights_mean','lag_1_feminism_total_score','lag_1_mensrights_total_score','lag_1_feminism_number_of_posts','lag_1_mensrights_number_of_posts','lag_2_feminism_median','lag_2_mensrights_median','lag_2_feminism_mean','lag_2_mensrights_mean','lag_2_feminism_total_score','lag_2_mensrights_total_score','lag_2_feminism_number_of_posts','lag_2_mensrights_number_of_posts','lag_3_feminism_median','lag_3_mensrights_median','lag_3_feminism_mean','lag_3_mensrights_mean','lag_3_feminism_total_score','lag_3_mensrights_total_score','lag_3_feminism_number_of_posts','lag_3_mensrights_number_of_posts']]
    return_frame[return_frame.isnull()]=0
    if return_frame.shape[0]>0:
        return_series=return_frame.iloc[0]
        if(feminism_delta_median.size >0):
            return_series = return_series.append( pd.Series( {"feminism_delta_median" : feminism_delta_median.get(0)}))
            return_series = return_series.append(pd.Series({"mensrights_delta_median": mensrights_delta_median.get(0)}))
            return_series = return_series.append(pd.Series({"feminism_delta_mean": feminism_delta_mean.get(0)}))
            return_series = return_series.append(pd.Series({"mensrights_delta_mean": mensrights_delta_mean.get(0)}))
            return_series = return_series.append(pd.Series({"feminism_delta_total_score": feminism_delta_total_score.get(0)}))
            return_series = return_series.append(pd.Series({"mensrights_delta_total_score": mensrights_delta_total_score.get(0)}))
            return_series = return_series.append(pd.Series({"feminism_delta_number_of_posts": feminism_delta_number_of_posts.get(0)}))
            return_series = return_series.append(pd.Series({"mensrights_delta_number_of_posts": mensrights_delta_number_of_posts.get(0)}))
        else:
            return_series = return_series.append(pd.Series({"feminism_delta_median": 0}))
            return_series = return_series.append(pd.Series({"mensrights_delta_median":0}))
            return_series = return_series.append(pd.Series({"feminism_delta_mean": 0}))
            return_series = return_series.append(pd.Series({"mensrights_delta_mean":0}))
            return_series = return_series.append(pd.Series({"feminism_delta_total_score":0}))
            return_series = return_series.append(pd.Series({"mensrights_delta_total_score":0}))
            return_series = return_series.append(pd.Series({"feminism_delta_number_of_posts":0}))
            return_series = return_series.append(pd.Series({"mensrights_delta_number_of_posts":0}))
        return return_series
    else:
        return  pd.Series({'lag_1_feminism_median':0, 'lag_1_mensrights_median':0, 'lag_1_feminism_mean':0, 'lag_1_mensrights_mean':0, 'lag_1_feminism_total_score':0, 'lag_1_mensrights_total_score':0, 'lag_1_feminism_number_of_posts':0, 'lag_1_mensrights_number_of_posts':0, 'lag_2_feminism_median':0, 'lag_2_mensrights_median':0, 'lag_2_feminism_mean':0, 'lag_2_mensrights_mean':0, 'lag_2_feminism_total_score':0, 'lag_2_mensrights_total_score':0, 'lag_2_feminism_number_of_posts':0, 'lag_2_mensrights_number_of_posts':0, 'lag_3_feminism_median':0, 'lag_3_mensrights_median':0, 'lag_3_feminism_mean':0, 'lag_3_mensrights_mean':0, 'lag_3_feminism_total_score':0, 'lag_3_mensrights_total_score':0, 'lag_3_feminism_number_of_posts':0, 'lag_3_mensrights_number_of_posts':0, "feminism_delta_median": 0, "mensrights_delta_median":0, "feminism_delta_mean": 0, "mensrights_delta_mean":0, "feminism_delta_total_score":0, "mensrights_delta_total_score":0, "feminism_delta_number_of_posts":0, "mensrights_delta_number_of_posts":0})


# In[271]:

get_prev_month(non_lagged_frame.iloc[4], non_lagged_frame)


# In[214]:

a=[0]*8
b=[1,2,3]
a+b


# In[136]:

type(non_lagged_frame.iloc[1:2])


# In[77]:

non_lagged_frame.head()


# In[278]:
temp_df=non_lagged_frame.head(10)
lagged_frame=non_lagged_frame.merge(logged_apply(non_lagged_frame, get_prev_month,axis=1, args=(non_lagged_frame,)), left_index=True, right_index=True)
lagged_frame.to_csv("time_seried_dataset_no_quantiles.csv", index=False)
