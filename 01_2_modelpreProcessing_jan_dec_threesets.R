
library(rgdal)
library(colorRamps)
require(fields)
# input data
dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'

dirmonth = c('01_janTS','02_febTS','03_marTS','04_aprTS','05_mayTS','06_junTS',
             '07_julTS','08_augTS','09_sepTS','10_octTS','11_novTS','12_decTS')

datanames = c('01Original.rdata','02Original.rdata', '03Original.rdata','04Original.rdata',
              '05Original.rdata','06Original.rdata','07Original.rdata','08Original.rdata',
              '09Original.rdata','10Original.rdata','11Original.rdata','12Original.rdata')


# model scenarinos
# M0_1:    T1
# M0_2:   T2
# M1:    S1
# M2:    S2
# M3:    S3
# M4:    S1 + S2
# M5:    S1 + S3
# M6:    S2 + S3
# M7:    S1 + S2 + S3

# M0_1 - T1 -----------------------------------------------------------------
opName = 'M0_1'
cat('-----------',opName,'\n')
for(idir in 1: 12){
  cat(dirmonth[idir],'\n')
  #formatC(idir,width = 2, flag = 0)
  
  
  load(file.path(dirPath,'processing', 'class',formatC(idir,width = 2, flag = 0), paste0(opName,'_datasets.rdata')))
  
  opFolder = file.path(dirPath,'processing',opName)
  
  dir.create(opFolder, showWarnings = F)
  dir.create(file.path(opFolder,dirmonth[idir]), showWarnings = F)
  proccessedPath = file.path(opFolder,dirmonth[idir])
  dir.create(file.path(proccessedPath, 'traindatasets'), showWarnings = F)
  load(file.path(dirPath,'processing', 'fullTS_F4',paste0(formatC(idir,width = 2, flag = 0),'Original.rdata')))
  
  datChar = data.frame(
    no = numeric(500),
    train1.n = numeric(500),
    train1.min = numeric(500),
    train1.mean = numeric(500),
    train1.median = numeric(500),
    train1.max = numeric(500),
    train1.sd = numeric(500),
    train2.n = numeric(500),
    train2.min = numeric(500),
    train2.mean = numeric(500),
    train2.median = numeric(500),
    train2.max = numeric(500),
    train2.sd = numeric(500),
    test.n = numeric(500),
    test.min = numeric(500),
    test.mean = numeric(500),
    test.median = numeric(500),
    test.sd = numeric(500),
    test.max = numeric(500))
  
  
  for(is in 1: 500){
    #cat(is,'\n')
    testid = listT[[is]]$testT$gsim.no
    trainid1 = listT[[is]]$train1T$gsim.no
    trainid2 = listT[[is]]$train2T$gsim.no
    
    testT = DfFinalF4[which(DfFinalF4$gsim.no %in% testid),]
    row.names(testT) = NULL
    
    train1T = DfFinalF4[which(DfFinalF4$gsim.no %in% trainid1),]
    row.names(train1T) = NULL
    
    train2T = DfFinalF4[which(DfFinalF4$gsim.no %in% trainid2),]
    row.names(train2T) = NULL
    
    stationInfo = data.frame(gsim.no = c(testT$gsim.no, train1T$gsim.no, train2T$gsim.no),
                             type = c(rep('test',nrow(testT)), rep('train1', nrow(train1T)), rep('train2', nrow(train2T))))
    
    datChar$no[is] = is
    datChar$train1.n[is] = nrow(train1T)
    datChar$train1.min[is] = min(train1T$Q)
    datChar$train1.mean[is] = mean(train1T$Q)
    datChar$train1.median[is] = median(train1T$Q)
    datChar$train1.max[is] = max(train1T$Q)
    datChar$train1.sd[is] = sd(train1T$Q)
    datChar$train2.n[is] = nrow(train2T)
    datChar$train2.min[is] = min(train2T$Q)
    datChar$train2.mean[is] = mean(train2T$Q)
    datChar$train2.median[is] = median(train2T$Q)
    datChar$train2.max[is] = max(train2T$Q)
    datChar$train2.sd[is] = sd(train2T$Q)
    datChar$test.n[is] = nrow(testT)
    datChar$test.min[is] = min(testT$Q)
    datChar$test.mean[is] = mean(testT$Q)
    datChar$test.median[is] = median(testT$Q)
    datChar$test.sd[is] = sd(testT$Q)
    datChar$test.max[is] = max(testT$Q)
    
    opFile = paste(proccessedPath,'/' ,'traindatasets','/','data', formatC(is, width = 4, flag = 0),'.rData', sep = '')
    save(testT, train1T, train2T, stationInfo,metaDataF4, DfFinalF4, file = opFile)
  }
  write.csv(datChar, paste(proccessedPath,'/', 'traindatasetsInfo.csv', sep = ''), row.names = F)
}

opName = 'M0_2'
cat('-----------',opName,'\n')
for(idir in 1: 12){
  cat(dirmonth[idir],'\n')
  #formatC(idir,width = 2, flag = 0)
  
  
  load(file.path(dirPath,'processing', 'class',formatC(idir,width = 2, flag = 0), paste0(opName,'_datasets.rdata')))
  
  opFolder = file.path(dirPath,'processing',opName)
  
  dir.create(opFolder, showWarnings = F)
  dir.create(file.path(opFolder,dirmonth[idir]), showWarnings = F)
  proccessedPath = file.path(opFolder,dirmonth[idir])
  dir.create(file.path(proccessedPath, 'traindatasets'), showWarnings = F)
  load(file.path(dirPath,'processing', 'fullTS_F4',paste0(formatC(idir,width = 2, flag = 0),'Original.rdata')))
  
  datChar = data.frame(
    no = numeric(500),
    train1.n = numeric(500),
    train1.min = numeric(500),
    train1.mean = numeric(500),
    train1.median = numeric(500),
    train1.max = numeric(500),
    train1.sd = numeric(500),
    train2.n = numeric(500),
    train2.min = numeric(500),
    train2.mean = numeric(500),
    train2.median = numeric(500),
    train2.max = numeric(500),
    train2.sd = numeric(500),
    test.n = numeric(500),
    test.min = numeric(500),
    test.mean = numeric(500),
    test.median = numeric(500),
    test.sd = numeric(500),
    test.max = numeric(500))
  
  
  for(is in 1: 500){
    #cat(is,'\n')
    testid = listT[[is]]$testT$gsim.no
    trainid1 = listT[[is]]$train1T$gsim.no
    trainid2 = listT[[is]]$train2T$gsim.no
    
    testT = DfFinalF4[which(DfFinalF4$gsim.no %in% testid),]
    row.names(testT) = NULL
    
    train1T = DfFinalF4[which(DfFinalF4$gsim.no %in% trainid1),]
    row.names(train1T) = NULL
    
    train2T = DfFinalF4[which(DfFinalF4$gsim.no %in% trainid2),]
    row.names(train2T) = NULL
    
    stationInfo = data.frame(gsim.no = c(testT$gsim.no, train1T$gsim.no, train2T$gsim.no),
                             type = c(rep('test',nrow(testT)), rep('train1', nrow(train1T)), rep('train2', nrow(train2T))))
    
    datChar$no[is] = is
    datChar$train1.n[is] = nrow(train1T)
    datChar$train1.min[is] = min(train1T$Q)
    datChar$train1.mean[is] = mean(train1T$Q)
    datChar$train1.median[is] = median(train1T$Q)
    datChar$train1.max[is] = max(train1T$Q)
    datChar$train1.sd[is] = sd(train1T$Q)
    datChar$train2.n[is] = nrow(train2T)
    datChar$train2.min[is] = min(train2T$Q)
    datChar$train2.mean[is] = mean(train2T$Q)
    datChar$train2.median[is] = median(train2T$Q)
    datChar$train2.max[is] = max(train2T$Q)
    datChar$train2.sd[is] = sd(train2T$Q)
    datChar$test.n[is] = nrow(testT)
    datChar$test.min[is] = min(testT$Q)
    datChar$test.mean[is] = mean(testT$Q)
    datChar$test.median[is] = median(testT$Q)
    datChar$test.sd[is] = sd(testT$Q)
    datChar$test.max[is] = max(testT$Q)
    
    opFile = paste(proccessedPath,'/' ,'traindatasets','/','data', formatC(is, width = 4, flag = 0),'.rData', sep = '')
    save(testT, train1T, train2T, stationInfo,metaDataF4, DfFinalF4, file = opFile)
  }
  write.csv(datChar, paste(proccessedPath,'/', 'traindatasetsInfo.csv', sep = ''), row.names = F)
}

# M1-M7-----------------------------------------------------------------
opNames = c('M1','M2','M3','M4','M5','M6','M7')
for(ii in 1:length(opNames)){
  opName = opNames[ii]
  cat('-----------',opName,'\n')
  for(idir in 1: 12){
    cat(dirmonth[idir],'\n')
    #formatC(idir,width = 2, flag = 0)
    
    load(file.path(dirPath,'processing', 'class',formatC(idir,width = 2, flag = 0), paste0(opName,'_datasets.rdata')))
    
    opFolder = file.path(dirPath,'processing',opName)
    
    dir.create(opFolder, showWarnings = F)
    dir.create(file.path(opFolder,dirmonth[idir]), showWarnings = F)
    proccessedPath = file.path(opFolder,dirmonth[idir])
    dir.create(file.path(proccessedPath, 'traindatasets'), showWarnings = F)
    load(file.path(dirPath,'processing', 'fullTS_F4',paste0(formatC(idir,width = 2, flag = 0),'Original.rdata')))
    
    datChar = data.frame(
      no = numeric(500),
      train1.n = numeric(500),
      train1.min = numeric(500),
      train1.mean = numeric(500),
      train1.median = numeric(500),
      train1.max = numeric(500),
      train1.sd = numeric(500),
      train2.n = numeric(500),
      train2.min = numeric(500),
      train2.mean = numeric(500),
      train2.median = numeric(500),
      train2.max = numeric(500),
      train2.sd = numeric(500),
      test.n = numeric(500),
      test.min = numeric(500),
      test.mean = numeric(500),
      test.median = numeric(500),
      test.sd = numeric(500),
      test.max = numeric(500))
    
    
    for(is in 1: 500){
      #cat(is,'\n')
      testid = listS[[is]]$testS$gsim.no
      trainid1 = listS[[is]]$train1S$gsim.no
      trainid2 = listS[[is]]$train2S$gsim.no
      
      testS = DfFinalF4[which(DfFinalF4$gsim.no %in% testid),]
      row.names(testS) = NULL
      
      train1S = DfFinalF4[which(DfFinalF4$gsim.no %in% trainid1),]
      row.names(train1S) = NULL
      
      train2S = DfFinalF4[which(DfFinalF4$gsim.no %in% trainid2),]
      row.names(train2S) = NULL
      
      stationInfo = data.frame(gsim.no = c(testS$gsim.no, train1S$gsim.no, train2S$gsim.no),
                               type = c(rep('test',nrow(testS)), rep('train1', nrow(train1S)), rep('train2', nrow(train2S))))
      
      datChar$no[is] = is
      datChar$train1.n[is] = nrow(train1S)
      datChar$train1.min[is] = min(train1S$Q)
      datChar$train1.mean[is] = mean(train1S$Q)
      datChar$train1.median[is] = median(train1S$Q)
      datChar$train1.max[is] = max(train1S$Q)
      datChar$train1.sd[is] = sd(train1S$Q)
      datChar$train2.n[is] = nrow(train2S)
      datChar$train2.min[is] = min(train2S$Q)
      datChar$train2.mean[is] = mean(train2S$Q)
      datChar$train2.median[is] = median(train2S$Q)
      datChar$train2.max[is] = max(train2S$Q)
      datChar$train2.sd[is] = sd(train2S$Q)
      datChar$test.n[is] = nrow(testS)
      datChar$test.min[is] = min(testS$Q)
      datChar$test.mean[is] = mean(testS$Q)
      datChar$test.median[is] = median(testS$Q)
      datChar$test.sd[is] = sd(testS$Q)
      datChar$test.max[is] = max(testS$Q)
      
      opFile = paste(proccessedPath,'/' ,'traindatasets','/','data', formatC(is, width = 4, flag = 0),'.rData', sep = '')
      save(testS, train1S, train2S, stationInfo,metaDataF4, DfFinalF4, file = opFile)
    }
    write.csv(datChar, paste(proccessedPath,'/', 'traindatasetsInfo.csv', sep = ''), row.names = F)
  }
  
}



