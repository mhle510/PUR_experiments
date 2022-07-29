dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
resPath = file.path(dirPath, 'processing', 'sumRes')

source('C:/Users/manhh/Dropbox/RStudy/0.Code/built_in/plot_support_functions.r')

load(file.path(dirPath, 'processing','sum_svm_sources_at_targets.rData'))
load(file.path(dirPath, 'processing','sum_rf_sources_at_targets.rData'))
load(file.path(dirPath, 'processing','sum_xgb_sources_at_targets.rData'))

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
svmlistFiles = list.files(resPath, pattern = 'svm', full.names = T)[1:7]
basename(svmlistFiles)

svmRes = lapply(svmlistFiles, read.csv)
exNames = substr(basename(svmlistFiles), 26, nchar(basename(svmlistFiles)) - 8)
exNames

#winter  DJF
listsvm_pairs = list()
svmS2T1 = svmsourcesAll[[1]]
listsvm_pairs[[1]] = data.frame(kge_t1 = apply(sapply(svmsourcesAll[[1]], '[',c(12,1,2),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                          kge_t2 = apply(sapply(svmsourcesAll[[2]], '[',c(12,1,2),3),2,mean),
                          kge_source = apply(sapply(svmRes, '[',c(12,1,2),2),2,mean))

#spring MAM
listsvm_pairs[[2]] = data.frame(kge_t1 = apply(sapply(svmsourcesAll[[1]], '[',c(3,4,5),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                                kge_t2 = apply(sapply(svmsourcesAll[[2]], '[',c(3,4,5),3),2,mean),
                                kge_source = apply(sapply(svmRes, '[',c(3,4,5),2),2,mean))

# summer JJA
listsvm_pairs[[3]] = data.frame(kge_t1 = apply(sapply(svmsourcesAll[[1]], '[',c(6,7,8),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                                kge_t2 = apply(sapply(svmsourcesAll[[2]], '[',c(6,7,8),3),2,mean),
                                kge_source = apply(sapply(svmRes, '[',c(6,7,8),2),2,mean))

# autumn SON
listsvm_pairs[[4]] = data.frame(kge_t1 = apply(sapply(svmsourcesAll[[1]], '[',c(9,10,11),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                                kge_t2 = apply(sapply(svmsourcesAll[[2]], '[',c(9,10,11),3),2,mean),
                                kge_source = apply(sapply(svmRes, '[',c(9,10,11),2),2,mean))

##### RF dataset
rflistFiles = list.files(resPath, pattern = 'rf', full.names = T)[1:7]
basename(rflistFiles)

rfRes = lapply(rflistFiles, read.csv)
exNames = substr(basename(rflistFiles), 26, nchar(basename(rflistFiles)) - 7)

#winter  DJF
listrf_pairs = list()
rfS2T1 = rfsourcesAll[[1]]
listrf_pairs[[1]] = data.frame(kge_t1 = apply(sapply(rfsourcesAll[[1]], '[',c(12,1,2),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                                kge_t2 = apply(sapply(rfsourcesAll[[2]], '[',c(12,1,2),3),2,mean),
                                kge_source = apply(sapply(rfRes, '[',c(12,1,2),2),2,mean))

#spring MAM
listrf_pairs[[2]] = data.frame(kge_t1 = apply(sapply(rfsourcesAll[[1]], '[',c(3,4,5),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                                kge_t2 = apply(sapply(rfsourcesAll[[2]], '[',c(3,4,5),3),2,mean),
                                kge_source = apply(sapply(rfRes, '[',c(3,4,5),2),2,mean))

# summer JJA
listrf_pairs[[3]] = data.frame(kge_t1 = apply(sapply(rfsourcesAll[[1]], '[',c(6,7,8),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                                kge_t2 = apply(sapply(rfsourcesAll[[2]], '[',c(6,7,8),3),2,mean),
                                kge_source = apply(sapply(rfRes, '[',c(6,7,8),2),2,mean))

# autumn SON
listrf_pairs[[4]] = data.frame(kge_t1 = apply(sapply(rfsourcesAll[[1]], '[',c(9,10,11),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                                kge_t2 = apply(sapply(rfsourcesAll[[2]], '[',c(9,10,11),3),2,mean),
                                kge_source = apply(sapply(rfRes, '[',c(9,10,11),2),2,mean))

#####  XGB dataset
xgblistFiles = list.files(resPath, pattern = 'xgb', full.names = T)[1:7]
basename(xgblistFiles)

xgbRes = lapply(xgblistFiles, read.csv)
exNames = substr(basename(xgblistFiles), 26, nchar(basename(xgblistFiles)) - 8)

#winter  DJF
listxgb_pairs = list()
xgbS2T1 = xgbsourcesAll[[1]]
listxgb_pairs[[1]] = data.frame(kge_t1 = apply(sapply(xgbsourcesAll[[1]], '[',c(12,1,2),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                               kge_t2 = apply(sapply(xgbsourcesAll[[2]], '[',c(12,1,2),3),2,mean),
                               kge_source = apply(sapply(xgbRes, '[',c(12,1,2),2),2,mean))

#spring MAM
listxgb_pairs[[2]] = data.frame(kge_t1 = apply(sapply(xgbsourcesAll[[1]], '[',c(3,4,5),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                               kge_t2 = apply(sapply(xgbsourcesAll[[2]], '[',c(3,4,5),3),2,mean),
                               kge_source = apply(sapply(xgbRes, '[',c(3,4,5),2),2,mean))

# summer JJA
listxgb_pairs[[3]] = data.frame(kge_t1 = apply(sapply(xgbsourcesAll[[1]], '[',c(6,7,8),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                               kge_t2 = apply(sapply(xgbsourcesAll[[2]], '[',c(6,7,8),3),2,mean),
                               kge_source = apply(sapply(xgbRes, '[',c(6,7,8),2),2,mean))

# autumn SON
listxgb_pairs[[4]] = data.frame(kge_t1 = apply(sapply(xgbsourcesAll[[1]], '[',c(9,10,11),3),2,mean), # take row 12,1,2 for each data frame, take column 3 ~ CI50
                               kge_t2 = apply(sapply(xgbsourcesAll[[2]], '[',c(9,10,11),3),2,mean),
                               kge_source = apply(sapply(xgbRes, '[',c(9,10,11),2),2,mean))

save(listsvm_pairs, listrf_pairs, listxgb_pairs, file = file.path(dirPath, 'processing', 'sumSeasonalres_S2T.rData'))
     