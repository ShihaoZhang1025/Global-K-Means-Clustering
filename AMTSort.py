
# coding: utf-8

# In[166]:



# In[1]:

from os import listdir
from os.path import isfile, join
import os
from datetime import datetime
import json
import pandas as pd
import glob
import sys
import copy

def SORTbyAMT(inputpath, outputpath):
  #outputpath: the path that you want put the result in
  #TOP 500, 1000, 2000
  if(inputpath == outputpath):
    sys.exit("Please use different paths")
 
  #import all the csv files in inputpath
  allfiles = glob.glob(inputpath +"/*.csv")
  frame = pd.DataFrame()
  list_ = []
  for file_ in allfiles:
    df = pd.read_csv(file_, index_col = None, header = 0).ix[:,(0,6)]
    df = df.rename(columns = {'Unnamed: 0': 'No_Stock'})
    list_.append(df)

  #set files name as '2011-w10' style
  files_name = [f for f in listdir(inputpath) if isfile(join(inputpath, f))]
  files_name = [s.replace('.csv', '') for s in files_name]
  for i, item in enumerate(files_name):
    Y = datetime.strptime(item, '%Y%m%d').isocalendar()[0]
    w = datetime.strptime(item, '%Y%m%d').isocalendar()[1]
    if len(str(w) == 1):
        files_name[i] = str(Y) + '0' + str(w)
    else:
        files_name[i] = str(Y) + str(w)

  #Merge all the files into one dataframe:
  A = list_[0].rename(columns = {'AMT': 'AMT_1'})
  for i, frame in enumerate(list_[1:], 2):
    A = A.merge(frame, on = 'No_Stock').rename( columns = {'AMT': 'AMT_%d' % i})
  
  
  Nostock = copy.copy(A['No_Stock'].tolist())
  A = A.drop('No_Stock', 1)
  A = A.T
  A.columns = Nostock
  
  week = pd.DataFrame(files_name, index = A.index.values)
  New = A.join(week, how = 'left')
  New = New.rename(columns = {0:'week'})
  D = New.groupby(["week"]).mean()
  D = D.fillna(0)
  Group = D.index.values
  
  for i in np.arange(len(Group)):
    AMT_500 = pd.DataFrame(D.rank(axis = 1, ascending = False).iloc[i,:] <= 500)
    AMT_1000 = pd.DataFrame(D.rank(axis = 1, ascending = False).iloc[i,:] <= 1000)
    AMT_2000 = pd.DataFrame(D.rank(axis = 1, ascending = False).iloc[i,:] <= 2000)
    AMT_500.to_csv(''.join([outputpath + '/TOP500' +Group[i] + '.csv']))
  
  print "Please see the files in the 'outpath' you set."


# In[167]:

SORTbyAMT('/Users/ShihaoZhang/Desktop/华勤投资/stockDayK', '/Users/ShihaoZhang/Desktop/华勤投资/example_1')

