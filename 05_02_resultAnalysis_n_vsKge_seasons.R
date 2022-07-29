dirPath = '/media/hung/DATA/0.NCKH/0.MyPaper/2022_climQ'
resPath = file.path(dirPath, 'processing', 'sumRes')

source('/home/hung/Dropbox/RStudy/0.Code/built_in/plot_support_functions.r')

# take total samples
folderNames = c('EX1','EX2','EX3','EX4','EX5','EX6','EX7','EX0_1','EX0_2')
listfolderNames = file.path(dirPath, 'processing',folderNames,'01_janTS', 'traindatasetsInfo.csv')
infoDat = lapply(listfolderNames, read.csv)
sampleInfo = data.frame(exNames = c("EX1" ,"EX2" ,"EX3" ,"EX4" ,"EX5" ,"EX6", "EX7" ,"T1"  ,"T2"),
                        n_train1 = sapply(infoDat, '[',1,2),
                        n_train2 = sapply(infoDat, '[',1,8),
                        n_test = sapply(infoDat, '[',1,14)
)



##### SVM dataset
svmlistFiles = list.files(resPath, pattern = 'svm', full.names = T)
basename(svmlistFiles)

svmRes = lapply(svmlistFiles, read.csv)
exNames = substr(basename(svmlistFiles), 26, nchar(basename(svmlistFiles)) - 8)

#winter  DJF
listsvm = list()
listsvm[[1]] = data.frame(exNames = exNames,
                          n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                          n_test = sampleInfo$n_test,
                          kge = apply(sapply(svmRes, '[',c(12,1,2),2),2,mean))

#spring MAM
listsvm[[2]] = data.frame(exNames = exNames,
                          n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                          n_test = sampleInfo$n_test,
                          kge = apply(sapply(svmRes, '[',c(3,4,5),2),2,mean))
# summer JJA
listsvm[[3]]= data.frame(exNames = exNames,
                         n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                         n_test = sampleInfo$n_test,
                         kge = apply(sapply(svmRes, '[',c(6,7,8),2),2,mean))
# autumn SON
listsvm[[4]] = data.frame(exNames = exNames,
                          n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                          n_test = sampleInfo$n_test,
                          kge = apply(sapply(svmRes, '[',c(9,10,11),2),2,mean))

##### RF dataset
rflistFiles = list.files(resPath, pattern = 'rf', full.names = T)
basename(rflistFiles)

rfRes = lapply(rflistFiles, read.csv)
exNames = substr(basename(rflistFiles), 26, nchar(basename(rflistFiles)) - 7)

#winter  DJF
listrf = list()
listrf[[1]] = data.frame(exNames = exNames,
                          n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                          n_test = sampleInfo$n_test,
                          kge = apply(sapply(rfRes, '[',c(12,1,2),2),2,mean))

#spring MAM
listrf[[2]] = data.frame(exNames = exNames,
                          n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                          n_test = sampleInfo$n_test,
                          kge = apply(sapply(rfRes, '[',c(3,4,5),2),2,mean))
# summer JJA
listrf[[3]]= data.frame(exNames = exNames,
                         n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                         n_test = sampleInfo$n_test,
                         kge = apply(sapply(rfRes, '[',c(6,7,8),2),2,mean))
# autumn SON
listrf[[4]] = data.frame(exNames = exNames,
                          n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                          n_test = sampleInfo$n_test,
                          kge = apply(sapply(rfRes, '[',c(9,10,11),2),2,mean))

#####  XGB dataset
xgblistFiles = list.files(resPath, pattern = 'xgb', full.names = T)
basename(xgblistFiles)

xgbRes = lapply(xgblistFiles, read.csv)
exNames = substr(basename(xgblistFiles), 26, nchar(basename(xgblistFiles)) - 8)

#winter  DJF
listxgb = list()
listxgb[[1]] = data.frame(exNames = exNames,
                         n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                         n_test = sampleInfo$n_test,
                         kge = apply(sapply(xgbRes, '[',c(12,1,2),2),2,mean))

#spring MAM
listxgb[[2]] = data.frame(exNames = exNames,
                         n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                         n_test = sampleInfo$n_test,
                         kge = apply(sapply(xgbRes, '[',c(3,4,5),2),2,mean))
# summer JJA
listxgb[[3]]= data.frame(exNames = exNames,
                        n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                        n_test = sampleInfo$n_test,
                        kge = apply(sapply(xgbRes, '[',c(6,7,8),2),2,mean))
# autumn SON
listxgb[[4]] = data.frame(exNames = exNames,
                         n_train = sampleInfo$n_train1 + sampleInfo$n_train2,
                         n_test = sampleInfo$n_test,
                         kge = apply(sapply(xgbRes, '[',c(9,10,11),2),2,mean))

save(listsvm, listrf, listxgb, file = file.path(dirPath, 'processing', 'sumSeasonalres.rData'))
     