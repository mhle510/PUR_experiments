dirPath = 'XXX/2022_climQ'



# knn ---------------------------------------------------------------------
listex = list.files(file.path(dirPath,'processing'),
                    pattern = 'targetpred_knn_sources', full.names = T)

knnsourcesAll = list()
nsim = 100*100
#locbest = rep(0,7)
for(m in 1:2){
  knnsourcesubregions = list()
  for(iex in 1:7){
    load(listex[iex])
    testMatrix = scoresAll[[m]]
    scoreChar = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                           CI05 = apply(testMatrix, 2, function(x) quantile(x, 0.05)),
                           CI50 = apply(testMatrix, 2, function(x) quantile(x, 0.5)),
                           CI95 = apply(testMatrix, 2, function(x) quantile(x, 0.95)))
    cat('---knn---','region',m,'EX',iex,'-----','\n')
    print(scoreChar)
    knnsourcesubregions[[iex]] = scoreChar
  }
  knnsourcesAll[[m]] = knnsourcesubregions
}

save(knnsourcesAll, file = file.path(dirPath, 'processing','sum_knn_sources_at_targets.rData'))


# svm ---------------------------------------------------------------------
listex = list.files(file.path(dirPath,'processing'),
                    pattern = 'targetpred_svm_sources', full.names = T)

svmsourcesAll = list()
nsim = 100*100
#locbest = rep(0,7)
for(m in 1:2){
  svmsourcesubregions = list()
  for(iex in 1:7){
    load(listex[iex])
    testMatrix = scoresAll[[m]]
    scoreChar = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                           CI05 = apply(testMatrix, 2, function(x) quantile(x, 0.05)),
                           CI50 = apply(testMatrix, 2, function(x) quantile(x, 0.5)),
                           CI95 = apply(testMatrix, 2, function(x) quantile(x, 0.95)))
    cat('---svm---','region',m,'EX',iex,'-----','\n')
    print(scoreChar)
    svmsourcesubregions[[iex]] = scoreChar
  }
  svmsourcesAll[[m]] = svmsourcesubregions
}

save(svmsourcesAll, file = file.path(dirPath, 'processing','sum_svm_sources_at_targets.rData'))

# rf ---------------------------------------------------------------------
listex = list.files(file.path(dirPath,'processing'),
                    pattern = 'targetpred_rf_sources', full.names = T)

rfsourcesAll = list()
nsim = 100*100
#locbest = rep(0,7)
for(m in 1:2){
  rfsourcesubregions = list()
  for(iex in 1:7){
    load(listex[iex])
    testMatrix = scoresAll[[m]]
    scoreChar = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                           CI05 = apply(testMatrix, 2, function(x) quantile(x, 0.05)),
                           CI50 = apply(testMatrix, 2, function(x) quantile(x, 0.5)),
                           CI95 = apply(testMatrix, 2, function(x) quantile(x, 0.95)))
    cat('---rf---','region',m,'EX',iex,'-----','\n')
    print(scoreChar)
    rfsourcesubregions[[iex]] = scoreChar
  }
  rfsourcesAll[[m]] = rfsourcesubregions
}

save(rfsourcesAll, file = file.path(dirPath, 'processing','sum_rf_sources_at_targets.rData'))


# xgb ---------------------------------------------------------------------
listex = list.files(file.path(dirPath,'processing'),
                    pattern = 'targetpred_xgb_sources', full.names = T)

xgbsourcesAll = list()
nsim = 100*100
#locbest = rep(0,7)
for(m in 1:2){
  xgbsourcesubregions = list()
  for(iex in 1:7){
    load(listex[iex])
    testMatrix = scoresAll[[m]]
    scoreChar = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                           CI05 = apply(testMatrix, 2, function(x) quantile(x, 0.05)),
                           CI50 = apply(testMatrix, 2, function(x) quantile(x, 0.5)),
                           CI95 = apply(testMatrix, 2, function(x) quantile(x, 0.95)))
    cat('---xgb---','region',m,'EX',iex,'-----','\n')
    print(scoreChar)
    xgbsourcesubregions[[iex]] = scoreChar
  }
  xgbsourcesAll[[m]] = xgbsourcesubregions
}

save(xgbsourcesAll, file = file.path(dirPath, 'processing','sum_xgb_sources_at_targets.rData'))
