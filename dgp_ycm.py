import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import math
from math import ceil, floor, exp, sqrt, log
import sys, traceback
import GPy
from GPy.kern import *
import random
import climin
import sys
import time
import os
import matplotlib.pyplot as plt
import logging
import csv
from memory_profiler import memory_usage
import statsmodels.api as sm
from datetime import date
from GPy.core import Mapping


# create logger for the application
logger = logging.getLogger('DGPM Logger')

ch = logging.StreamHandler()

# create formatter and add it to the handlers
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
ch.setFormatter(formatter)


logger.addHandler(ch)
logger.setLevel(logging.DEBUG)

def read_data():
    os.chdir('/home/admin123/Yield_Curve_Modelling/')
    fp = 'Src_2_2006_to_Present_Treasury_Data.csv'
    df = pd.read_csv(fp)
    ref_date = date(2012, 12, 31)
    df["Date"] = pd.to_datetime(df["Date"])
    return df

def do_random_gp_plot(k = 5):
    df = read_data()
    miy = 12.0
    mts = [1.0,3.0,6.0,12.0, 24.0,36.0,60.0, 84.0, 120.0, 240.0, 360.0]
    X = [t/miy for t in mts]
    X = np.array(X)
    X = np.reshape(X, (X.shape[0], 1))
    logger.info(X)
    req_cols = filter(lambda v : v not in ["Date", "key"], df.columns)
    Y = df.ix[k, req_cols]
    Y = np.reshape(Y, (Y.shape[0],1))
    k = RBF(input_dim = 1)
    m = GPy.models.GPRegression(X, Y, k)
    m.optimize()
    m.plot()
    
    
    return


def db_block():
    df = read_data()
    miy = 12.0
    mts = [1.0,3.0,6.0,12.0, 24.0,36.0,60.0, 84.0, 120.0, 240.0, 360.0]
    X = [t/miy for t in mts]
    X = np.array(X)
    X = np.reshape(X, (X.shape[0], 1))
    req_cols = filter(lambda v : v not in ["Date", "key"], df.columns)
    index_start = 0
    block_size = 60
    index_end = index_start + block_size
    df_block = df.ix[index_start: (index_end - 1),:]
    m = train_batch_mf(df_block)

    plt.scatter(X, priord[1], color = 'red')
    plt.scatter(X, priord[60], color = 'blue')
    plt.scatter(X, m.Y, color = 'green')

    return 
    
def seq_pred():
    global error_df, priord, perf_df, X, pred_mat, act_mat, rmse_df

    df = read_data()
    dates = df["Date"]
    miy = 12.0
    mts = [1.0,3.0,6.0,12.0, 24.0,36.0,60.0, 84.0, 120.0, 240.0, 360.0]
    X = [t/miy for t in mts]
    X = np.array(X)
    X = np.reshape(X, (X.shape[0], 1))
    req_cols = filter(lambda v : v not in ["Date", "key"], df.columns)
    N = df.shape[0]
    pred_rows = N - 2
    
    error_mat = np.ones((pred_rows,11), dtype = np.float64)
    pred_mat = np.ones((pred_rows,11), dtype = np.float64)
    act_mat = np.ones((pred_rows,11), dtype = np.float64)
    rmse_mat = np.ones((1,11), dtype = np.float64)
    date_list = list()
    
    logger.info("Dataset has " + str(N) + " rows")
    row_index = 0
    mf = Mapping(1,1)
    date_list = list()
    while row_index < (N-2): # (N-2) is because we don't need to process last record
        dfr = df.ix[row_index,:]
        Y = dfr[req_cols]
        Y = np.reshape(Y, (Y.shape[0],1))

        #log progress - update progress every 100 days
        if row_index % 100 == 0:
            logger.debug("Day %d processed..." %row_index)

        if row_index == 0:
            k = RBF(input_dim = 1) + Linear(input_dim = 1, ARD = True)
            m = GPy.models.GPRegression(X, Y, kernel = k)
            m.optimize()

        else:
            ## The following block corresponds to the update
            ## of Kalman filter
            act = df.ix[(row_index), req_cols]
            act = np.reshape(act,\
                                      (act.shape[0],1))
            # post_pred is the prediction for the previous day
            residual = act - post_pred
            # updated estimate
            X_star = X
            t1 = m.kern.K(X_star,X)
            t2 = np.linalg.inv(m.kern.K(X,X) +\
                               m['Gaussian_noise.variance']*\
                               np.eye(X.shape[0]))
            t3 = t1.dot(t2)
            t4 = t3.dot(residual)
            um = post_pred + t4
            um = um.astype('float64')


            def the_mf(X):
                    return um
            mf.f = the_mf
            mf.update_gradients = lambda a,b: None
            k = RBF( input_dim = 1) 
                
            lik = GPy.likelihoods.Gaussian()
            m = GPy.core.GP(X, Y, kernel = k,\
                            likelihood=lik, mean_function = mf)
        m.optimize()
        
        post_pred = m.predict(X)[0]
        row_index = row_index + 1
        date_list.append(dates[row_index])
        # prediction for the next day, the index in pred mat lags one behind
        # the index for dates
        estimates = post_pred.ravel()
        pred_mat[(row_index - 1), :] = post_pred.ravel()
        act = df.ix[row_index, req_cols]
        error_mat[(row_index - 1), :] = (act - estimates)*(act - estimates)
        

    error_df = pd.DataFrame(error_mat)
    error_df.columns = req_cols
    
    perf_df = pd.DataFrame()
    perf_df["Term"] = X.ravel()
    me = error_df.apply(np.mean, axis = 0)
    perf_df["Mean.Error"] = list(me)
    sd = error_df.apply(np.std, axis = 0)
    perf_df["SD"] = list(sd)

    # compute the rmse for each term
    N_preds = N - 2
    for c in range(error_df.shape[1]):
        se = error_df.ix[:, c] 
        rmse_mat[0, c] = sqrt(np.sum(se)/N_preds)

    rmse_df = pd.DataFrame(rmse_mat)
    rmse_df.columns = req_cols



    os.chdir('/home/admin123/Yield_Curve_Modelling/')
    fp = "GP_Results.csv"
    perf_df.to_csv(fp, index = False, header = True)
    fp = "rmse_results_DGP"+ ".csv"
    rmse_df.to_csv(fp, index = False, header = True)
    error_df["Date"] = np.array(date_list)
    fp = "10Y_GP_sq_term_errors.csv"
    error_df.to_csv(fp, index = False, header = True)

    
    with open("day_500_sample.csv", 'a') as d500sample:
        writer = csv.writer(d500sample)
        line_to_write = pred_mat[500,:].tolist()
        line_to_write.append("GP")
        writer.writerow(line_to_write)
          
        

    with open("day_1000_sample.csv", 'a') as d1000sample:
        writer = csv.writer(d1000sample)
        line_to_write = pred_mat[1000,:].tolist()
        line_to_write.append("GP")
        writer.writerow(line_to_write)
        

    with open("day_1500_sample.csv", 'a') as d1500sample:
        writer = csv.writer(d1500sample)
        line_to_write = pred_mat[1500,:].tolist()
        line_to_write.append("GP")
        writer.writerow(line_to_write)



    with open("day_2000_sample.csv", 'a') as d2000sample:
        writer = csv.writer(d2000sample)
        line_to_write = pred_mat[2000,:].tolist()
        line_to_write.append("GP")
        writer.writerow(line_to_write)  



    logger.info("Done!")   
    return  





    

