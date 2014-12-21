# -*- coding: utf-8 -*-
"""
Created on Wed Dec 17 20:32:42 2014

@author: samuelkahn
"""
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import norm
import statsmodels.formula.api as smf
def dataframe_to_csv():
    data=pd.DataFrame.from_csv('cleaned_data.csv')
    return data

def fun_histograms(SpritzData,x):
    fun_normal=SpritzData[((SpritzData.firstNormal == 1) & (SpritzData.firstFun==1))\
    | ((SpritzData.firstNormal == 0) & (SpritzData.firstFun==0) )]
    fun_spritz=SpritzData[((SpritzData.firstNormal == 0) & (SpritzData.firstFun==1))\
    | ((SpritzData.firstNormal == 1) & (SpritzData.firstFun==0) )]
    plt.hist(np.array(fun_normal['FUN']), bins=6, alpha=x,normed=1,color='purple',label='FUN-Normal')
    plt.hist(np.array(fun_spritz['FUN']), bins=6, alpha=0.5,normed=1,color='cyan', label='FUN-Spritz')
    
    param = norm.fit(np.array(fun_normal['FUN']))
    x = np.linspace(0,5,100)
    pdf_fitted = norm.pdf(x,loc=param[0],scale=param[1])
    
    
    plt.plot(x,pdf_fitted,'r-',label='Fun-Normal:Fit')
    
    param = norm.fit(np.array(fun_spritz['FUN']))
    x = np.linspace(0,5,100)
    pdf_fitted = norm.pdf(x,loc=param[0],scale=param[1])
    
    plt.plot(x,pdf_fitted,'b-',label='Fun-Spritz:Fit')
    plt.legend(loc='upper left')
    plt.xlim(0,5)
    plt.title('FUN Article: Normal vs Spritz Histogram and Fit')
    plt.xlabel('Score')
    plt.text(np.mean(fun_normal['FUN'])-.3,0.41,'Mean:'+str(np.mean(fun_normal['FUN']))[:-9])
    plt.text(np.mean(fun_spritz['FUN'])-.3,0.34,'Mean:'+str(np.mean(fun_spritz['FUN']))[:-9])
    plt.ylabel('Density')
    plt.show()
def sad_histogram(SpritzData):
    sad_normal=SpritzData[((SpritzData.firstNormal == 1) & (SpritzData.firstFun==0))\
    | ((SpritzData.firstNormal == 0) & (SpritzData.firstFun==1) )]
    sad_spritz=SpritzData[((SpritzData.firstNormal == 0) & (SpritzData.firstFun==0))\
    | ((SpritzData.firstNormal == 1) & (SpritzData.firstFun==1) )]
    plt.hist(np.array(sad_normal['SAD']), bins=6, alpha=0.5,normed=1,color='purple', label='SAD-Normal')
    plt.hist(np.array(sad_spritz['SAD']), bins=6, alpha=0.5,normed=1,color='yellow', label='SAD-Spritz')
    
    param = norm.fit(np.array(sad_normal['SAD']))
    x = np.linspace(0,5,100)
    pdf_fitted = norm.pdf(x,loc=param[0],scale=param[1])

    
    
    plt.plot(x,pdf_fitted,'r-',label='SAD-Normal:Fit')
    
    param = norm.fit(np.array(sad_spritz['SAD']))
    x = np.linspace(0,5,100)
    pdf_fitted = norm.pdf(x,loc=param[0],scale=param[1])
    
    plt.plot(x,pdf_fitted,'b-',label='SAD-Spritz:Fit')
    plt.legend(loc='upper left')
    plt.xlim(0,5)
    plt.text(np.mean(sad_normal['SAD'])-.3,0.35,'Mean:'+str(np.mean(sad_normal['SAD']))[:-9])
    plt.text(np.mean(sad_spritz['SAD'])-.3,0.32,'Mean:'+str(np.mean(sad_spritz['SAD']))[:-9])
    plt.title('SAD Article: Normal vs Spritz Histogram and Fit')
    plt.xlabel('Score')
    plt.ylabel('Density')
    plt.show()
def spritz_regression_analysis(data):
    data['isSAD']=np.where(data['FS'] == 'SAD', 1, 0)
    spritz_reg=smf.ols(formula='Y~trt+isSAD+firstNormal+Age+SpritzExp+UsesGlasses+DegreeCode+PrimEng'\
    +'+Female+RaceCode+readTM+readBook+readSci+readMag+readProf',data=data).fit()
    print spritz_reg.summary()
def main():
    SpritzData=dataframe_to_csv()
    print SpritzData.columns.values
    spritz_regression_analysis(SpritzData)

    

    
if __name__=="__main__":
    main()