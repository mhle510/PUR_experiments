library(e1071)
library(tidyverse)
library(hydroGOF)
# input data
dirPath = 'XXX/2022_climQ'

dirmonth = c('01_janTS','02_febTS','03_marTS','04_aprTS','05_mayTS','06_junTS',
             '07_julTS','08_augTS','09_sepTS','10_octTS','11_novTS','12_decTS')
# built-in function
source('XXX/ml_support_functions.r')
source('XXX/qsim_support_functions.r')
source('XXX/plot_support_functions.r')

# model information
SNames = paste0('EX',1:7)
mons = c('01','02','03','04','05','06','07','08','09','10','11','12')

for(im in 2:7){
  SName = SNames[im]
  for(kk in 1:12){
    cat('--------------','\n')
    cat('---',dirmonth[kk],'\n')
    
    mon = mons[kk]
    proccessedPathS = paste(dirPath,'/','processing/',SName , '/', dirmonth[kk],sep = '')
    
    # path create
    dir.create(file.path(dirPath,'res_svm'), showWarnings = F)
    dir.create(file.path(dirPath,'res_svm',SName),showWarnings = F)
    sPath = file.path(dirPath,'res_svm',SName)
    
    
    # step1 finding optimize of tree and split features -----------------------
    ctypes = c('eps-regression','nu-regression')
    kernels = c('linear','polynomial','radial','sigmoid')
    nn = length(ctypes)*length(kernels)
    
    # test with dataset 1 -------------------------------------------------------
    ii = 1
    
    opFile = paste(proccessedPathS,'/' ,'traindatasets','/','data', formatC(ii, width = 4, flag = 0),'.rData', sep = '')
    load(opFile)
    
    train1dt = train1S[,-c(1,2,19, 20)] # remove first two columns - basin ID
    train2dt = train2S[,-c(1,2,19, 20)] # remove first two columns - basin ID
    testdt = testS[,-c(1,2,19, 20)]
    
    # re-scale predictor datasets, response variable is in the last column
    scaled_train1dt = scale_data_multi(train1dt[,-ncol(train1dt)], testdt[,-ncol(testdt)], 
                                       feature_range = c(0.1, 0.9))[[1]]
    scaled_testdt = scale_data_multi(train1dt[,-ncol(train1dt)], testdt[,-ncol(testdt)], 
                                     feature_range = c(0.1, 0.9))[[2]]
    scaled_train2dt = scale_data_multi(train1dt[,-ncol(train1dt)], train2dt[,-ncol(testdt)], 
                                       feature_range = c(0.1, 0.9))[[2]]
    
    
    scalers_test = scale_data_multi(train1dt[,-ncol(train1dt)], testdt[,-ncol(testdt)], 
                                    feature_range = c(0.1, 0.9))[[3]]
    
    
    # use log Q as label
    scaled_train1dt = data.frame(scaled_train1dt, logQ = log(train1dt$Q))
    scaled_train2dt = data.frame(scaled_train2dt, logQ = log(train2dt$Q))
    scaled_testdt = data.frame(scaled_testdt, logQ = log(testdt$Q))
    
    
    metricsTest1 = data.frame(no = numeric(nn),
                              ctype = numeric(nn),
                              kerneltype = numeric(nn),
                              KGE_train1 = numeric(nn),
                              rho_train1 = numeric(nn),
                              alpha_train1 = numeric(nn),
                              beta_train1 = numeric(nn),
                              KGE_train2 = numeric(nn),
                              rho_train2 = numeric(nn),
                              alpha_train2 = numeric(nn),
                              beta_train2 = numeric(nn),
                              KGE_test = numeric(nn),
                              rho_test = numeric(nn),
                              alpha_test = numeric(nn),
                              beta_test = numeric(nn))
    
    id = 1
    for(it in 1:length(ctypes)){
      for(ik in 1:length(kernels)){
        #cat(id, '\n')
        #make this example reproducible
        set.seed(1)
        #fit the random forest model
        model = svm(formula = logQ ~.,
                    data = scaled_train1dt,
                    type = ctypes[it],
                    kernel = kernels[ik])
        
        
        logsimTrain1 = predict(model, newdata=scaled_train1dt[,-ncol(scaled_train1dt)])
        logsimTrain2 = predict(model, newdata=scaled_train2dt[,-ncol(scaled_train2dt)])
        logsimTest = predict(model, newdata=scaled_testdt[,-ncol(testdt)])
        
        simTrain1 = exp(logsimTrain1)
        simTrain2 = exp(logsimTrain2)
        simTest = exp(logsimTest)
        
        obsTrain1  = train1dt$Q %>% c()
        obsTrain2  = train2dt$Q %>% c()
        obsTest  = testdt$Q %>% c()
        
        
        mtrain1 = calc_KGE_metric_log(simTrain1, obsTrain1) # kge, rho, alpha, beta
        mtrain2 = calc_KGE_metric_log(simTrain2, obsTrain2)
        mtest = calc_KGE_metric_log(simTest, obsTest)
        
        #cat('train')
        #cat(mtrain1,'\n')
        #cat('test')
        #cat(mtest,'\n')
        
        metricsTest1$no[id] = id
        metricsTest1$ctype[id] = it
        metricsTest1$kerneltype[id] = ik
        metricsTest1$KGE_train1[id] = mtrain1[1]
        metricsTest1$rho_train1[id] = mtrain1[2]
        metricsTest1$alpha_train1[id] = mtrain1[3]
        metricsTest1$bias_train1[id] = mtrain1[4]
        metricsTest1$KGE_train2[id] = mtrain2[1]
        metricsTest1$rho_train2[id] = mtrain2[2]
        metricsTest1$alpha_train2[id] = mtrain2[3]
        metricsTest1$bias_train2[id] = mtrain2[4]
        metricsTest1$KGE_test[id] = mtest[1]
        metricsTest1$rho_test[id] = mtest[2]
        metricsTest1$alpha_test[id] = mtest[3]
        metricsTest1$beta[id] = mtest[4]
        id = id +1
      }
        
     
    }
    save(metricsTest1, file = file.path(sPath,paste0('turningparas_',mon,'_dt0001.rData')))
    
    load(file.path(sPath,paste0('turningparas_',mon,'_dt0001.rData')))
    
    # check best perform for train 1 and train 2
    kge12 = 0.4*metricsTest1$KGE_train1 + 0.6*metricsTest1$KGE_train2
    summary(kge12)
    plot(kge12)
    
    bestloc = which(kge12 == max(kge12))
    if (length(bestloc) > 1) bestloc = bestloc[1]
    
    kge12[bestloc]
    metricsTest1[bestloc,]
    
    cbest = ctypes[metricsTest1$ctype[bestloc]]
    kbest = kernels[metricsTest1$kerneltype[bestloc]]
    # test with best model after trial and error
    
    set.seed(1)
    bestModel = svm(formula = logQ ~.,
                    data = scaled_train1dt,
                    type = cbest,
                    kernel = kbest)
    
    logsimTrain1 = predict(bestModel, newdata=scaled_train1dt[,-ncol(scaled_train1dt)])
    logsimTrain2 = predict(bestModel, newdata=scaled_train2dt[,-ncol(scaled_train2dt)])
    logsimTest = predict(bestModel, newdata=scaled_testdt[,-ncol(testdt)])
    
    
    simTrain1 = exp(logsimTrain1)
    simTrain2 = exp(logsimTrain2)
    simTrain = c(simTrain1, simTrain2)
    simTest = exp(logsimTest)
    
    
    
    obsTrain1  = train1dt$Q %>% c()
    obsTrain2  = train2dt$Q %>% c()
    obsTrain = c(obsTrain1, obsTrain2)
    obsTest  = testdt$Q %>% c()
    
    
    scatterPlot_train_test(simTrain, obsTrain, simTest, obsTest)
    
    
    # step 2 running 1000 simulations ------------------------------------------
    nn = 500
    # turning parameters
    
    kbest = kbest
    cbest = cbest
    
    metricsTestFinal = data.frame(no = numeric(nn),
                                  ctype = numeric(nn),
                                  kerneltype = numeric(nn),
                                  KGE_train = numeric(nn),
                                  rho_train = numeric(nn),
                                  alpha_train = numeric(nn),
                                  beta_train = numeric(nn),
                                  KGE_test = numeric(nn),
                                  rho_test = numeric(nn),
                                  alpha_test = numeric(nn),
                                  beta_test = numeric(nn))
    listOutputrf = list()
    for(ll in 1 : nn){
      opFile = paste(proccessedPathS,'/' ,'traindatasets','/','data', formatC(ll, width = 4, flag = 0),'.rData', sep = '')
      load(opFile)
      
      train1dt = train1S[,-c(1,2,19, 20)] # remove first two columns - basin ID
      train2dt = train2S[,-c(1,2,19, 20)] # remove first two columns - basin ID
      testdt = testS[,-c(1,2,19, 20)]
      
      
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
      
      set.seed(1)
      
      bestModel = svm(formula = logQ ~.,
                      data = scaled_train1dt,
                      type = cbest,
                      kernel = kbest)
      
      logsimTrain1 = predict(bestModel, newdata=scaled_train1dt[,-ncol(scaled_train1dt)])
      logsimTrain2 = predict(bestModel, newdata=scaled_train2dt[,-ncol(scaled_train2dt)])
      logsimTest = predict(bestModel, newdata=scaled_testdt[,-ncol(testdt)])
      
      simTrain1 = exp(logsimTrain1)
      simTrain2 = exp(logsimTrain2)
      simTrain = c(simTrain1, simTrain2)
      simTest = exp(logsimTest)
      
      
      obsTrain1  = train1dt$Q %>% c()
      obsTrain2  = train2dt$Q %>% c()
      traindt = rbind.data.frame(train1dt, train2dt)
      obsTrain = traindt$Q %>% c()
      obsTest  = testdt$Q %>% c()
      
      
      mtrain = calc_KGE_metric_log(simTrain, obsTrain) # kge, rho, alpha, beta
      mtest = calc_KGE_metric_log(simTest, obsTest)
      
      metricsTestFinal$no[ll] = ll 
      metricsTestFinal$ctype[ll] = cbest
      metricsTestFinal$kerneltype[ll] = kbest
      metricsTestFinal$KGE_train[ll] = mtrain[1]
      metricsTestFinal$rho_train[ll] = mtrain[2]
      metricsTestFinal$alpha_train[ll] = mtrain[3]
      metricsTestFinal$beta_train[ll] = mtrain[4]
      metricsTestFinal$KGE_test[ll] = mtest[1]
      metricsTestFinal$rho_test[ll] = mtest[2]
      metricsTestFinal$alpha_test[ll] = mtest[3]
      metricsTestFinal$beta_test[ll] = mtest[4]
      
      
      # save data
      colNames = c('gsim.no','type', paste0('sim',formatC(ll, width = 4, flag = 0), sep = ''),
                   'obs')
      type = c(rep('train1',length(train1S$gsim.no)),
               rep('train2',length(train2S$gsim.no)),
               rep('test',length(testS$gsim.no)))
      simCombined = data.frame(c(train1S$gsim.no, train2S$gsim.no, testS$gsim.no),
                               type,
                               c(simTrain1, simTrain2, simTest),
                               c(obsTrain1, obsTrain2, obsTest))
      colnames(simCombined) = colNames
      
      listOutputrf[[ll]] = list(simCombined = simCombined, simTrain1 = simTrain1, 
                                simTrain2 = simTrain2, simTest = simTest, model = bestModel)
      #cat('------finish sim ', ll, '\n')
    }
    print(summary(metricsTestFinal))
    save(listOutputrf, metricsTestFinal, file = file.path(sPath,paste0('svm_mon',mon,'.rData')))
  }
}


  


