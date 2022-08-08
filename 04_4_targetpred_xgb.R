library(xgboost)
library(tidyverse)
library(hydroGOF)
library(tictoc)
dirPath = 'XXX/2022_climQ'
setwd(dirPath)
dirmonth = c('01_janTS','02_febTS','03_marTS','04_aprTS','05_mayTS','06_junTS',
             '07_julTS','08_augTS','09_sepTS','10_octTS','11_novTS','12_decTS')


# built-in function
source('XXX/ml_support_functions.r')
source('XXX/qsim_support_functions.r')
source('XXX/plot_support_functions.r')

SNameSs = paste0('EX',1:7)
mons = c('01','02','03','04','05','06','07','08','09','10','11','12')

targetpred_xgb = list()
for(mm in 1:1){
  cat('--- experiment', mm,'-----','\n')
  SNameS = SNameSs[mm]
  nmodel = 100
  ninput = 100
  nn = nmodel*ninput
 
  SNameTs = c('EX0_1','EX0_2')
  scoresAll = list()
  for (ll in 1:2){
    tic()
    scores = mat.or.vec(nn,12)
    SNameT = SNameTs[ll]
    cat('---',SNameT,'---','\n')
    for(kk in 1:12){
      # load model
      sPath = file.path(dirPath,'res_xgb',SNameS)
      mon = mons[mm]
      load(file.path(sPath,paste0('xgb_mon',mon,'.rData')))
      
      # load tareget input
      
      proccessedPathS = paste(dirPath,'/','processing/',SNameT , '/', dirmonth[kk],sep = '')
      
      id = 1
      for(ip in 1:100){
        # read input datasets
        opFile = paste(proccessedPathS,'/' ,'traindatasets','/','data', formatC(ip, width = 4, flag = 0),'.rData', sep = '')
        load(opFile)
        
        train1dt = train1T[,-c(1,2,19, 20)] # remove first two columns - basin ID
        train2dt = train2T[,-c(1,2,19, 20)] # remove first two columns - basin ID
        testdt = testT[,-c(1,2,19, 20)]
        
        # re-scale predictor datasets, response variable is in the last column
        scaled_train1dt = scale_data_multi(train1dt[,-ncol(train1dt)], testdt[,-ncol(testdt)], 
                                           feature_range = c(0.1, 0.9))[[1]]
        scaled_testdt = scale_data_multi(train1dt[,-ncol(train1dt)], testdt[,-ncol(testdt)], 
                                         feature_range = c(0.1, 0.9))[[2]]
        scaled_train2dt = scale_data_multi(train1dt[,-ncol(train1dt)], train2dt[,-ncol(train2dt)], 
                                           feature_range = c(0.1, 0.9))[[2]]
        scalers = scale_data_multi(train1dt[,-ncol(train1dt)], testdt[,-ncol(testdt)], 
                                   feature_range = c(0.1, 0.9))[[3]]
        # use log Q as label
        scaled_train1dt = data.frame(scaled_train1dt, logQ = log(train1dt$Q))
        scaled_train2dt = data.frame(scaled_train2dt, logQ = log(train2dt$Q))
        scaled_testdt = data.frame(scaled_testdt, logQ = log(testdt$Q))
        
        # run and obtain simulation results from 500 models at sources
        for(im in 1:100){
          sourcemodel = listOutputrf[[im]]$model
          logsimTest = predict(sourcemodel, data.matrix(scaled_testdt[,-ncol(testdt)]))
          simTest = exp(logsimTest)
          obsTest  = testdt$Q %>% c()
          mtest = calc_KGE_metric_log(simTest, obsTest)
          
          # write out performance records
          scores[id,kk] = mtest[1]
          id = id +1
          if(id %% 1000 == 0) cat(id,'...','\n')
        }
      }
    }
    colnames(scores) = c('J','F','M','A','M','J','J','A','S','O','N','D')
    scoresAll[[ll]] = scores
    toc()
  }
  #targetpred_xgb[[mm]] = scoresAll
  save(scoresAll, file = file.path(dirPath,'processing',paste0('targetpred_xgb_sources_ex',mm,'.rData')))
}


#load(file.path(dirPath,'processing','xgbModelPerf_bm.rData'))
  
