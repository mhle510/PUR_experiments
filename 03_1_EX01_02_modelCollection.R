# input data
dirPath = 'XXX/2022_climQ'
dir.create(file.path(dirPath, 'processing','sumRes'), showWarnings = F)
processedPath = file.path(dirPath, 'processing','sumRes')

# model information
SNames = c('EX0_1','EX0_2' )
mons = c('01','02','03','04','05','06','07','08','09','10','11','12')
nsim = 100

####KNN model ##############
knnModel7EXperf = list()
#locbest = rep(0,7)
for(m in 1:2){
  testMatrix = mat.or.vec(nsim,12)
  for(im in 1:12){
    SName = SNames[m]
    mon = mons[im]
    sPath = file.path(dirPath,'res_knn',SName)
    load(file.path(sPath,paste0('knn_mon',mon,'.rData')))
    testMatrix[,im] = metricsTestFinal$KGE_test[1:nsim]
  }
  scoreChar = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                         CI05 = apply(testMatrix, 2, function(x) quantile(x, 0.05)),
                         CI50 = apply(testMatrix, 2, function(x) quantile(x, 0.5)),
                         CI95 = apply(testMatrix, 2, function(x) quantile(x, 0.95)),
                         sd = apply(testMatrix, 2, function(x) sd(x)))
  
  knnModel7EXperf[[m]] = scoreChar
  csvFile = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                       mean = round(scoreChar$CI50,2),
                       sd = round(scoreChar$sd,2))
  cat('---knn---','EX',m,'-----','\n')
  print(scoreChar)
  
}
save(knnModel7EXperf,  file = file.path(dirPath,'processing','knnModelPerf_bm.rData'))


####SVM model ##############
svmModel7EXperf = list()
#locbest = rep(0,7)
for(m in 1:2){
  testMatrix = mat.or.vec(nsim,12)
  for(im in 1:12){
    SName = SNames[m]
    mon = mons[im]
    sPath = file.path(dirPath,'res_svm',SName)
    load(file.path(sPath,paste0('svm_mon',mon,'.rData')))
    testMatrix[,im] = metricsTestFinal$KGE_test[1:nsim]
  }
  scoreChar = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                         CI05 = apply(testMatrix, 2, function(x) quantile(x, 0.05)),
                         CI50 = apply(testMatrix, 2, function(x) quantile(x, 0.5)),
                         CI95 = apply(testMatrix, 2, function(x) quantile(x, 0.95)),
                         sd = apply(testMatrix, 2, function(x) sd(x)))
  
  svmModel7EXperf[[m]] = scoreChar
  csvFile = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                       mean = round(scoreChar$CI50,2),
                       sd = round(scoreChar$sd,2))
  opFile = paste0(processedPath,'/','Target2Target_Statistics_T',m,'_svm.csv')
  write.csv(csvFile, opFile, row.names = F)
  cat('---svm---','EX',m,'-----','\n')
  print(scoreChar)
  
}
save(svmModel7EXperf,  file = file.path(dirPath,'processing','svmModelPerf_bm.rData'))



####Random Forest model ##############
rfModel7EXperf = list()
#locbest = rep(0,7)
for(m in 1:2){
  testMatrix = mat.or.vec(nsim,12)
  for(im in 1:12){
    SName = SNames[m]
    mon = mons[im]
    sPath = file.path(dirPath,'res_rf',SName)
    load(file.path(sPath,paste0('rf_mon',mon,'.rData')))
    testMatrix[,im] = metricsTestFinal$KGE_test[1:nsim]
  }
  scoreChar = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                         CI05 = apply(testMatrix, 2, function(x) quantile(x, 0.05)),
                         CI50 = apply(testMatrix, 2, function(x) quantile(x, 0.5)),
                         CI95 = apply(testMatrix, 2, function(x) quantile(x, 0.95)),
                         sd = apply(testMatrix, 2, function(x) sd(x)))
 
  rfModel7EXperf[[m]] = scoreChar
  csvFile = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                       mean = round(scoreChar$CI50,2),
                       sd = round(scoreChar$sd,2))
  opFile = paste0(processedPath,'/','Target2Target_Statistics_T',m,'_rf.csv')
  write.csv(csvFile, opFile, row.names = F)

  cat('---rf---','EX',m,'-----','\n')
  print(scoreChar)
}

save(rfModel7EXperf,  file = file.path(dirPath,'processing','rfModelPerf_bm.rData'))

####XGBoost model ##############
xgbModel7EXperf = list()
#locbest = rep(0,7)
for(m in 1:2){
  testMatrix = mat.or.vec(nsim,12)
  for(im in 1:12){
    SName = SNames[m]
    mon = mons[im]
    sPath = file.path(dirPath,'res_xgb',SName)
    load(file.path(sPath,paste0('xgb_mon',mon,'.rData')))
    testMatrix[,im] = metricsTestFinal$KGE_test[1:nsim]
  }
  scoreChar = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                         CI05 = apply(testMatrix, 2, function(x) quantile(x, 0.05)),
                         CI50 = apply(testMatrix, 2, function(x) quantile(x, 0.5)),
                         CI95 = apply(testMatrix, 2, function(x) quantile(x, 0.95)),
                         sd = apply(testMatrix, 2, function(x) sd(x)))
  cat('---xgb---','EX',m,'-----','\n')
  print(scoreChar)
  xgbModel7EXperf[[m]] = scoreChar
  
  csvFile = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                       mean = round(scoreChar$CI50,2),
                       sd = round(scoreChar$sd,2))
  opFile = paste0(processedPath,'/','Target2Target_Statistics_T',m,'_xgb.csv')
  write.csv(csvFile, opFile, row.names = F)
}

save(xgbModel7EXperf,  file = file.path(dirPath,'processing','xgbModelPerf_bm.rData'))
