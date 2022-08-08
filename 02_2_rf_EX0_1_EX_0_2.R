library(randomForest)
library(tidyverse)
library(hydroGOF)
# input data
dirPath = 'XXX/2022_climQ'

dirmonth = c('01_janTS','02_febTS','03_marTS','04_aprTS','05_mayTS','06_junTS',
             '07_julTS','08_augTS','09_sepTS','10_octTS','11_novTS','12_decTS')
# built-in function
source('XXX/ml_support_functions.r')
source('XXXqsim_support_functions.r')
source('XXX/plot_support_functions.r')

# model information
SNames = c('EX0_1','EX0_2')
mons = c('01','02','03','04','05','06','07','08','09','10','11','12')

for(im in 2:2){
  SName = SNames[im]
  for(kk in 1:12){
    cat('--------------','\n')
    cat('---',dirmonth[kk],'\n')
    
    mon = mons[kk]
    proccessedPathS = paste(dirPath,'/','processing/',SName , '/', dirmonth[kk],sep = '')
    
    # path create
    dir.create(file.path(dirPath,'res_rf'), showWarnings = F)
    dir.create(file.path(dirPath,'res_rf',SName),showWarnings = F)
    sPath = file.path(dirPath,'res_rf',SName)
    
    
    
    # step1 finding optimize of tree and split features -----------------------
    treeNums = seq(2,50,2)
    splitFeatures = seq(1, 16, 1)
    nn = length(treeNums) * length(splitFeatures)
    
    # test with dataset 1 -------------------------------------------------------
    ii = 1
    
    opFile = paste(proccessedPathS,'/' ,'traindatasets','/','data', formatC(ii, width = 4, flag = 0),'.rData', sep = '')
    load(opFile)
    
    train1dt = train1T[,-c(1,2,19, 20)] # remove first two columns - basin ID
    train2dt = train2T[,-c(1,2,19, 20)] # remove first two columns - basin ID
    testdt = testT[,-c(1,2,19, 20)]
    
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
                              tree = numeric(nn),
                              split = numeric(nn),
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
    for(it in 1:length(treeNums)){
      for(is in 1: length(splitFeatures)){
        #cat(id, '\n')
        #make this example reproducible
        set.seed(1)
        #fit the random forest model
        model = randomForest(
          formula = logQ ~ .,
          x  = scaled_train1dt[,-ncol(train1dt)],
          y  = scaled_train1dt$logQ,
          xtest = scaled_train2dt[,-ncol(train2dt)],
          ytest = scaled_train2dt$logQ,
          ntree = treeNums[it],
          mtry = splitFeatures[is],
          keep.forest=TRUE
        )
        
        
        logsimTrain1 = predict(model, newdata=scaled_train1dt[,-ncol(scaled_train1dt)])
        logsimTrain2 = predict(model, newdata=scaled_train2dt[,-ncol(scaled_train2dt)])
        logsimTest = predict(model, newdata=scaled_testdt[,-ncol(testdt)])
        
        simTrain1 = exp(logsimTrain1)
        simTrain2 = exp(logsimTrain2)
        simTest = exp(logsimTest)
        
        obsTrain1  = train1dt$Q %>% c()
        obsTrain2  = train2dt$Q %>% c()
        obsTest  = testdt$Q %>% c()
        
        #par(mfrow = c(1,3))
        #boxplot(obsTrain1, ylim = c(0,500), main = 'train1')
        #boxplot(obsTrain2, ylim = c(0,500), main = 'train2')
        #boxplot(obsTest, ylim = c(0,500), main = 'tets')
        
        mtrain1 = calc_KGE_metric_log(simTrain1, obsTrain1) # kge, rho, alpha, beta
        mtrain2 = calc_KGE_metric_log(simTrain2, obsTrain2)
        mtest = calc_KGE_metric_log(simTest, obsTest)
        
        #cat('train')
        #cat(mtrain1,'\n')
        #cat('test')
        #cat(mtest,'\n')
        
        metricsTest1$no[id] = id
        metricsTest1$tree[id] = it
        metricsTest1$split[id] = is
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
    kge12 = 0.4*abs(metricsTest1$KGE_train1) + 0.6*abs(metricsTest1$KGE_train2)
    summary(kge12)
    plot(kge12)
    
    bestloc = which(kge12 == max(kge12))
    if (length(bestloc) > 1) bestloc = bestloc[1]
    
    kge12[bestloc]
    metricsTest1[bestloc,]
    
    #ntreebest = treeNums[2]
    #mtrybest = splitFeatures[16]
    ntreebest = treeNums[metricsTest1$tree[bestloc]]
    mtrybest = splitFeatures[metricsTest1$split[bestloc]]
    
    # test with best model after trial and error
    set.seed(1)
    bestModel = randomForest(
      formula = logQ ~ .,
      data = scaled_train1dt,
      ntree = ntreebest,
      mtry = mtrybest,
      keep.forest=TRUE
    )
    
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
    ntreebest = ntreebest
    mtrybest = mtrybest
    
    
    metricsTestFinal = data.frame(no = numeric(nn),
                                  tree = numeric(nn),
                                  split = numeric(nn),
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
      
      train1dt = train1T[,-c(1,2,19, 20)] # remove first two columns - basin ID
      train2dt = train2T[,-c(1,2,19, 20)] # remove first two columns - basin ID
      testdt = testT[,-c(1,2,19, 20)]
      
      # library(MASS)
      # lm = lm( Q ~. , data = train1dt)
      # boxcox(lm, plotit=T, lambda=seq(-2,2,by=0.5))
      # L <-boxcox(lm, plotit = F)$x[which.max(boxcox(lm, plotit = F)$y)] 
      # lmlog = lm(log(Q) ~., data = train1dt)
      # summary(lmlog)
      
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
      bestModel = randomForest(
        formula = logQ ~ .,
        data = scaled_train1dt,
        ntree = ntreebest,
        mtry = mtrybest,
        keep.forest=TRUE
      )
      
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
      
      #cat('train')
      #cat(mtrain,'\n')
      #cat('test')
      #cat(mtest,'\n')
      
      metricsTestFinal$no[ll] = ll 
      metricsTestFinal$tree[ll] = ntreebest
      metricsTestFinal$split[ll] = mtrybest
      metricsTestFinal$KGE_train[ll] = mtrain[1]
      metricsTestFinal$rho_train[ll] = mtrain[2]
      metricsTestFinal$alpha_train[ll] = mtrain[3]
      metricsTestFinal$beta_train[ll] = mtrain[4]
      metricsTestFinal$KGE_test[ll] = mtest[1]
      metricsTestFinal$rho_test[ll] = mtest[2]
      metricsTestFinal$alpha_test[ll] = mtest[3]
      metricsTestFinal$beta_test[ll] = mtest[4]
      
      # #### PLOT HISTOGRAM ##########
      # opFig1 = paste0(figPath,'/', paste0('sim',formatC(ll, width = 4, flag = 0), sep = ''),
      #                 '_qhist.pdf', sep = '')
      # pdf(opFig1,width=6.5,height=6.5)
      # par(mfrow=c(2,2),
      #     mar=c(3.25,3.25,1,1),mgp=c(2,0.75,0),
      #     tck=-0.015,cex.lab=0.9,cex.axis=0.8)
      # 
      # hist(traindt$Q, main = 'Q train', xlab = 'Q (cms)')
      # hist(testdt$Q, main = 'Q test', xlab = 'Q (cms)')
      # hist(log(traindt$Q), main = 'log Q train', xlab = 'log Q (cms)')
      # hist(log(testdt$Q), main = 'log Q test', xlab = 'log Q (cms)')
      # 
      # dev.off()
      # 
      
      # ## PLOT TRAIN-CROSS-VALIDATION-TEST LOCATIONS
      #catTrain1  = list.test[[ll]]$train1$gsim.no
      #catTrain2  = list.test[[ll]]$train2$gsim.no
      #catTest  = list.test[[ll]]$test$gsim.no
      # 
      # catTrain1 = data.frame(no = list.test[[ll]]$train1$no,
      #                        id  = list.test[[ll]]$train1$gsim.no,
      #                        lat = DfFinal$lat.new[list.test[[ll]]$train1$no],
      #                        lon = DfFinal$long.new[list.test[[ll]]$train1$no])
      # catTrain2 = data.frame(no = list.test[[ll]]$train2$no,
      #                        id  = list.test[[ll]]$train2$gsim.no,
      #                        lat = DfFinal$lat.new[list.test[[ll]]$train2$no],
      #                        lon = DfFinal$long.new[list.test[[ll]]$train2$no])
      # catTrain = rbind.data.frame(catTrain1, catTrain2)
      # catTest = data.frame(no = list.test[[ll]]$test$no,
      #                      id  = list.test[[ll]]$test$gsim.no,
      #                      lat = DfFinal$lat.new[list.test[[ll]]$test$no],
      #                      lon = DfFinal$long.new[list.test[[ll]]$test$no])
      # 
      # 
      # opFig2 = paste0(figPath,'/', paste0('sim',formatC(ll, width = 4, flag = 0), sep = ''),
      #                 '_qloc.pdf', sep = '')
      # pdf(opFig2,width=6.5,height=5)
      # 
      # par(mfrow=c(1,1),
      #     mar=c(3.25,3.25,1,1),mgp=c(2,0.75,0),
      #     tck=-0.015,cex.lab=0.9,cex.axis=0.8)
      # 
      # plot(wm, border = 'gray50', lwd = 0.2)
      # points(catTrain$lon, catTrain$lat, cex = 0.4, col = alpha('blue', 0.5), pch = 16 )
      # #points(catTrain2$lon, catTrain2$lat, cex = 0.4, col = alpha('green', 0.5), pch = 16 )
      # points(catTest$lon, catTest$lat, cex = 0.4, col = alpha('red', 0.5), pch = 16 )
      # 
      # legendText = c(paste("train"," (",nrow(catTrain),")",sep = ""),
      #                paste("test"," (",nrow(catTest),")",sep = ""))
      # legend("bottomleft", 
      #        legend = legendText,
      #        col = c('blue','red'), 
      #        pch = c(16,16), 
      #        bty = "n", 
      #        pt.cex = 0.9, 
      #        cex = 0.6, 
      #        text.col = "black", 
      #        horiz = F , 
      #        inset = c(0.1, 0.2))
      # dev.off()
      # ## PLOT SCATTER ##########
      # 
      # opFig3 = paste0(figPath,'/', paste0('sim',formatC(ll, width = 4, flag = 0), sep = ''),
      #                 '_qscatter.pdf', sep = '')
      # pdf(opFig3,width=6.5,height=3)
      # 
      # par(mfrow=c(1,2),
      #     mar=c(3.25,3.25,1,1),mgp=c(2,0.75,0),
      #     tck=-0.015,cex.lab=0.9,cex.axis=0.8)
      # 
      # 
      # #train
      # plot_colorByDensity(log10(obsTrain),log10(simTrain),
      #                     xlab="Log-10 of Observed Discharge",ylab="Log-10 of simulated discharge",
      #                     xlim=c(0,4),ylim=c(0,4))
      # abline(a=0,b=1,col="darkred",lwd=1.5)
      # metrics = calc_KGE_metric_log(simTrain, obsTrain)
      # metVals = c('KGE','rho','alpha','beta')
      # mtext = paste0(metrics,"=",metVals)
      # #print(mtext)
      # legend("topleft",bty="n",cex=1.25,text.font = 2,
      #        legend = paste('Train', sep = ''),
      #        text.col = "darkblue")
      # 
      # legend("bottomright",bty="n",cex=0.7,text.font = 2,ncol = 1,
      #        legend = mtext,
      #        text.col = "darkred")
      
      # #train2
      # plot_colorByDensity(log10(obsTrain2),log10(simTrain2),
      #                     xlab="Log-10 of Observed Discharge",ylab="Log-10 of simulated discharge",
      #                     xlim=c(0,4),ylim=c(0,4))
      # abline(a=0,b=1,col="darkred",lwd=1.5)
      # gofDat = gof(simTrain2,obsTrain2)
      # metrics = rownames(gofDat)[c(4,6,16)]
      # metVals = as.numeric(gofDat[,1])[c(4,6,16)]
      # mtext = paste0(metrics,"=",metVals)
      # #print(mtext)
      # legend("topleft",bty="n",cex=1.25,text.font = 2,
      #        legend = paste('Cross-validation', sep = ''),
      #        text.col = "darkblue")
      # 
      # legend("bottomright",bty="n",cex=0.6,text.font = 2,ncol = 1,
      #        legend = mtext,
      #        text.col = "darkred")
      # 
      
      # #test
      # plot_colorByDensity(log10(obsTest),log10(simTest),
      #                     xlab="Log-10 of Observed Discharge",ylab="Log-10 of simulated discharge",
      #                     xlim=c(0,4),ylim=c(0,4))
      # abline(a=0,b=1,col="darkred",lwd=1.5)
      # metrics = calc_KGE_metric_log(simTrain, obsTrain)
      # metVals = c('KGE','rho','alpha','beta')
      # mtext = paste0(metrics,"=",metVals)
      # #print(mtext)
      # legend("topleft",bty="n",cex=1.25,text.font = 2,
      #        legend = paste('Test', sep = ''),
      #        text.col = "darkblue")
      # 
      # legend("bottomright",bty="n",cex=0.7,text.font = 2,ncol = 1,
      #        legend = mtext,
      #        text.col = "darkred")
      # 
      # dev.off()
      # 
      
      # save data
      colNames = c('gsim.no','type', paste0('sim',formatC(ll, width = 4, flag = 0), sep = ''),
                   'obs')
      type = c(rep('train1',length(train1T$gsim.no)),
               rep('train2',length(train2T$gsim.no)),
               rep('test',length(testT$gsim.no)))
      simCombined = data.frame(c(train1T$gsim.no, train2T$gsim.no, testT$gsim.no),
                               type,
                               c(simTrain1, simTrain2, simTest),
                               c(obsTrain1, obsTrain2, obsTest))
      colnames(simCombined) = colNames
      
      listOutputrf[[ll]] = list(simCombined = simCombined, simTrain1 = simTrain1, 
                                simTrain2 = simTrain2, simTest = simTest, model = bestModel)
      #cat('------finish sim ', ll, '\n')
    }
    print(summary(metricsTestFinal))
    save(listOutputrf, metricsTestFinal, file = file.path(sPath,paste0('rf_mon',mon,'.rData')))
  }
}


  


