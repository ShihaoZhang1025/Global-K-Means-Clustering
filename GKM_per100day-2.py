
# coding: utf-8

# In[2]:


"""
Spyder Editor

This is a temporary script file.
"""
import csv
import math
import copy
import glob
import numpy as np
import pandas as pd
from scipy import signal
import scipy.spatial.distance
from itertools import compress
from sklearn.preprocessing import scale
from sklearn.metrics.pairwise import euclidean_distances

inputpath = '/Users/ShihaoZhang/Desktop/华勤投资/stockDayK/new/'

#import all the csv files in inputpath
Allfiles = glob.glob(inputpath +"*.csv")
k = 100
a = np.arange(k, len(Allfiles), k)
b = np.split(Allfiles, a)

for i in np.arange(len(Allfiles)):
    
    allfiles = b[i].tolist

    temp = []
    for file_ in allfiles:
        df = pd.read_csv(file_, index_col = None, header = 0).ix[:,(0,4,10)]
        df = df[np.isfinite(df['CLOSE'])]
        df['Price'] = df.ix[:,(1,2)].prod(axis=1)
        df = df.ix[:,(0,3)]
        df = df.rename(columns = {'Unnamed: 0': 'No_Stock'})
        temp.append(df)
     
    A = reduce(lambda x,y: pd.merge(x,y, on='No_Stock'), temp)
    No_Stock = copy.copy(A.ix[:,0])

    A = A.drop('No_Stock', 1)

    smo = A.T.rolling(window = 10, center = False).mean().dropna()
    smo_detr = signal.detrend(smo, axis = 0, type='linear')
    smo_detr_norm = scale(smo_detr, axis=0, with_mean=True, with_std=True, copy=True)
    H = smo_detr_norm.T

    a = global_kmeans(H, 25, 0.0001, 20)
    d = {'No_Stock': No_Stock, 'Cluster': a}
    result = pd.DataFrame(data=d)
    cols = result.columns.tolist()
    cols = cols[-1:] + cols[:-1]
    result = result[cols]
    result.to_csv("Clusters_period%d.csv" %(i+1))


def global_kmeans(H, k, tau):
    
    D = H.shape[1] #D is the dimensions
    #H: the input data set, each row corresponding to an observation
    #k: the number of the clusters
    #tau is the threshold parameter
    ctr = np.mean(H, axis = 0) # when k=1, the centroid of the k-means is colMeans(x)
    
    for i in range(1, k):
        # try every stock point as initial to run k.means, get all the possible results for i-th center
        ctr_temp = [k_means(x, H, ctr, tau) for x in list(H)]
        ctr_temp = [x for x in ctr_temp if x is not None]#filter all the 'None' element in ctr_temp
        
        # get every possible "ctr" in i-th iteration:
        bb = [np.vstack((ctr, x)) for x in ctr_temp]
        label_temp = [euclidean_distances(H, x) for x in bb] 
        comb_lable_1toi = [np.argmin(x, axis = 0) for x in label_temp] # assign lables to every stock point
        
        # filtering all the unqualified label list
        yy = [table(x) for x in comb_lable_1toi]
        # "comb_lable_1toi" -- each list -- contains assigned label for H corresponding to each poiont
        filter_1 = [np.amax(x)==i for x in comb_lable_1toi]
        filter_2 = [np.amin(x)>20 for x in yy]
        label_filter = [a and b for a, b in zip(filter_1, filter_2)]
        bb = list(compress(bb, label_filter))
        comb_lable_1toi = list(compress(comb_lable_1toi, label_filter))
        
        # get the mean of each group
        comb_mean_groups = [GroupMean(x) for x in comb_lable_1toi]
        #"comb_mean_groups" -- each list -- contains the seperate means of each group
        mean_all = np.mean(H, axis = 0)
        
        dd = [np.square(euclidean_distances(x, mean_all)) for x in comb_mean_groups]
        
        # variance between groups
        label_count = [table(x) for x in comb_lable_1toi]
        var_btw = [np.sum(np.dot(x, y)) for x, y in zip(label_count, dd)]
        print var_btw
        ctr = bb[np.argmax(var_btw)]
        
    overall_d = euclidean_distances(H, ctr) # the final distence between all the centroid and the input data
    cluster = np.argmin(overall_d, axis = 0)
    
    return cluster  


def VecMatch(x, want):
    #'x' and 'want' are both array-like
    #return True if 'want' is contained in 'x'
    if(x.ndim == 1):
        return np.array_equal(x, want)
    else:
        return (x == want).all(1).any()

def ctr_vstack(x, y):
    return np.vstack((y, x))

def table(x):
    # 'x' is ndarray
    # this function is same as the 'table' in R
    unique, counts = np.unique(x, return_counts=True)
    return counts

def GroupMean(x):
    # 'x' is ndarray
    # return the means of each group in 'x'
    x_1 = pd.DataFrame(x, columns = ['label'])
    H_1 = pd.DataFrame(H)
    a = pd.concat([H_1, x_1], axis = 1)
    b = a.pivot_table(values= H_1.columns.values, index='label', aggfunc=np.mean)
    return b.as_matrix()


# In[3]:

def k_means(tk, H, K, tau):
    D = H.shape[1]
    if(K.ndim == 1):
        k = 1
    else:
        k = K.shape[0]
    
    if(VecMatch(K, tk)):
        tk = None
    
    else:
        DM = euclidean_distances(H, K.reshape(-1,D))
        delta = 1
        
        while(delta >= tau):
            dm = euclidean_distances(H, tk.reshape(-1,D))
            A = np.hstack((DM,dm))
            m = np.argmin(A, axis = 1)
            
            if(np.amax(m) < k):
                tk = None
                break
                
            else:
                tk_old = copy.copy(tk)
            
                tk = np.mean(H[m == k], axis = 0)
                delta = math.sqrt(sum(np.square(tk - tk_old)))
            
    return tk


# In[416]:

#print k_means(list(H)[1], H, np.mean(H, axis = 0), 0.0001)
#print [k_means(x, H, np.mean(H, axis = 0), 0.0001) for x in list(H)]


# In[ ]:




# In[ ]:




# In[ ]:




# In[44]:




# In[47]:

Allfiles = glob.glob(inputpath +"*.csv")
k = 10
a = np.arange(k, len(Allfiles), k)
b = np.split(Allfiles, a)

allfiles = b[1].tolist()
allfiles


# In[51]:

np.arange(10)


# In[50]:

for allfiles in b:
    print type(allfiles)

