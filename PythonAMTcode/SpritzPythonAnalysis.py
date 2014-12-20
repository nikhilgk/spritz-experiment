# -*- coding: utf-8 -*-
"""
Created on Wed Dec 17 20:32:42 2014

@author: samuelkahn
"""
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import norm
def dataframe_to_csv():
    data=pd.DataFrame.from_csv('SpritzMain.csv')
    return data

def fun_histograms(SpritzData):
    fun_normal=SpritzData[((SpritzData.firstNormal == 1) & (SpritzData.firstFun==1))\
    | ((SpritzData.firstNormal == 0) & (SpritzData.firstFun==0) )]
    fun_spritz=SpritzData[((SpritzData.firstNormal == 0) & (SpritzData.firstFun==1))\
    | ((SpritzData.firstNormal == 1) & (SpritzData.firstFun==0) )]
    fig = plt.figure(figsize=(18, 6))
    ax1 = plt.subplot2grid((1, 4), (0, 0))
    ax1.hist(np.array(fun_normal['FUN']), bins=6, alpha=0.5,normed=1, label='FUN-Normal')
    ax1.set_xticks([0,1,2,3,4,5])    
    ax2 = plt.subplot2grid((1, 4), (0, 1))    
    ax2.hist(np.array(fun_spritz['FUN']), bins=6, color='g',alpha=0.5,normed=1, label='FUN-Spritz')

    plt.subplots_adjust(wspace=0.0)
    param = norm.fit(np.array(fun_normal['FUN']))
    x = np.linspace(0,6,100)
    pdf_fitted = norm.pdf(x,loc=param[0],scale=param[1])
#    
#    
    ax1.plot(x,pdf_fitted,'r-',label='Fun-Normal:Fit')
    ax1.legend(loc='upper left')
    param = norm.fit(np.array(fun_spritz['FUN']))
    x = np.linspace(0,6,100)
    pdf_fitted = norm.pdf(x,loc=param[0],scale=param[1])
    
    ax2.plot(x,pdf_fitted,'b-',label='Fun-Spritz:Fit')
    ax2.legend(loc='upper left')
    fig.suptitle('FUN Article: Normal vs Spritz Histogram and Fit',x=0.3,fontsize=14)
    ax1.set_xlabel('Score')
    ax1.set_ylabel('Density')
    
    ax2.set_xlabel('Score')
    
    
    ax1.text(np.mean(fun_normal['FUN'])-.3,0.41,'Mean:'+str(np.mean(fun_normal['FUN']))[:-9])
    ax2.text(np.mean(fun_spritz['FUN'])-.3,0.34,'Mean:'+str(np.mean(fun_spritz['FUN']))[:-9])
    
    plt.show()
def sad_histogram(SpritzData):
    sad_normal=SpritzData[((SpritzData.firstNormal == 1) & (SpritzData.firstFun==0))\
    | ((SpritzData.firstNormal == 0) & (SpritzData.firstFun==1) )]
    sad_spritz=SpritzData[((SpritzData.firstNormal == 0) & (SpritzData.firstFun==0))\
    | ((SpritzData.firstNormal == 1) & (SpritzData.firstFun==1) )]
    
    fig = plt.figure(figsize=(18, 6))
    ax1 = plt.subplot2grid((1, 4), (0, 0))
    ax1.hist(np.array(sad_normal['SAD']), bins=6, alpha=0.5,normed=1, label='SAD-Normal')

    ax2 = plt.subplot2grid((1, 4), (0, 1))    
    ax2.hist(np.array(sad_spritz['SAD']), bins=6, color='g',alpha=0.5,normed=1, label='SAD-Spritz')
    plt.subplots_adjust(wspace=0.0)
    
    
    param = norm.fit(np.array(sad_normal['SAD']))
    x = np.linspace(0,6,100)
    pdf_fitted = norm.pdf(x,loc=param[0],scale=param[1])
    ax1.plot(x,pdf_fitted,'r-',label='SAD-Normal:Fit')
    ax1.legend(loc='upper left')
    
    
    param = norm.fit(np.array(sad_spritz['SAD']))
    x = np.linspace(0,6,100)
    pdf_fitted = norm.pdf(x,loc=param[0],scale=param[1])
    ax2.plot(x,pdf_fitted,'b-',label='SAD-Spritz:Fit')
    ax2.legend(loc='upper left')
    
    
    fig.suptitle('SAD Article: Normal vs Spritz Histogram and Fit',x=0.3,fontsize=14)
    
    ax1.set_ylim([0,0.5])
    ax2.set_ylim([0,0.5])
    ax1.set_xticks([0,1,2,3,4,5])    
    ax1.set_xlabel('Score')
    ax1.set_ylabel('Density')
    ax2.set_xlabel('Score')
    ax1.text(np.mean(sad_normal['SAD'])-.52,0.35,'Mean:'+str(np.mean(sad_normal['SAD']))[:-9])
    ax2.text(np.mean(sad_spritz['SAD'])-.52,0.3,'Mean:'+str(np.mean(sad_spritz['SAD']))[:-9])
    
    plt.show()
def main():
    SpritzData=dataframe_to_csv()
    fun_histograms(SpritzData)
    sad_histogram(SpritzData)
    

    
if __name__=="__main__":
    main()