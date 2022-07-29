dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
mons = c('01','02','03','04','05','06','07','08','09','10','11','12')

for(ii in 1 : length(mons)){
  mon = mons[ii]
  load(file.path(dirPath,'processing', 'fullTS_F4',paste0(mon,'Original.rdata')))
  
  source('C:/Users/manhh/Dropbox/RStudy/0.Code/built_in/dataframe_support_functions.r')
  cat('----',ii,'\n')
  print(table(metaDataF4$group))
  
  # model scenarinos
  # M0_1:    T1
  # M0_2:    T2
  # M1:    S1
  # M2:    S2
  # M3:    S3
  # M4:    S1 + S2
  # M5:    S1 + S3
  # M6:    S2 + S3
  # M7:    S1 + S2 + S3
  
  
  
  # M01 - T1 -----------------------------------------------------------------
  subT = subset(metaDataF4, metaDataF4$group == 'T1')
  subT$group = 'T1'
  row.names(subT) = NULL
  
  listT = list()
  for(iloop in 1: 500){
    
    # for source
    i = sample(1:nrow(subT), round(0.5*nrow(subT)))
    j = setdiff(1:nrow(subT), i)
    
    i1 = sample(i, round(0.5*length(i)))
    i2 = setdiff(i, i1)
    
    
    idtrain1T = rownames(subT)[i1]
    idtrain2T = rownames(subT)[i2]
    idtestT = rownames(subT)[j]
    
    train1T = subT[as.numeric(idtrain1T),c('gsim.no','group')]
    train2T = subT[as.numeric(idtrain2T),c('gsim.no','group')]
    testT = subT[as.numeric(idtestT),c('gsim.no','group')]
    
    row.names(train1T) = NULL
    row.names(train2T) = NULL
    row.names(testT) = NULL
    
    listT[[iloop]] = list(train1T = train1T, train2T = train2T, testT = testT)
  }
  
  dir.create(file.path(dirPath, 'processing','class'), showWarnings = F)
  dir.create(file.path(dirPath, 'processing','class',mon), showWarnings = F)
  save(listT, file = file.path(dirPath, 'processing','class',mon ,'M0_1_datasets.rdata'))
  
  # M02 - T2 -----------------------------------------------------------------
  subT = subset(metaDataF4, metaDataF4$group == 'T2')
  subT$group = 'T2'
  row.names(subT) = NULL
  
  listT = list()
  for(iloop in 1: 500){
    
    # for source
    i = sample(1:nrow(subT), round(0.5*nrow(subT)))
    j = setdiff(1:nrow(subT), i)
    
    i1 = sample(i, round(0.5*length(i)))
    i2 = setdiff(i, i1)
    
    
    idtrain1T = rownames(subT)[i1]
    idtrain2T = rownames(subT)[i2]
    idtestT = rownames(subT)[j]
    
    train1T = subT[as.numeric(idtrain1T),c('gsim.no','group')]
    train2T = subT[as.numeric(idtrain2T),c('gsim.no','group')]
    testT = subT[as.numeric(idtestT),c('gsim.no','group')]
    
    row.names(train1T) = NULL
    row.names(train2T) = NULL
    row.names(testT) = NULL
    
    listT[[iloop]] = list(train1T = train1T, train2T = train2T, testT = testT)
  }
  
  dir.create(file.path(dirPath, 'processing','class'), showWarnings = F)
  dir.create(file.path(dirPath, 'processing','class',mon), showWarnings = F)
  save(listT, file = file.path(dirPath, 'processing','class',mon ,'M0_2_datasets.rdata'))
  
  # M1 - S1 -----------------------------------------------------------------
  subS = subset(metaDataF4, metaDataF4$group == 'S1')
  subS$group = 'S'
  row.names(subS) = NULL
  
  listS = list()
  for(iloop in 1: 500){
    
    # for source
    i = sample(1:nrow(subS), round(0.5*nrow(subS)))
    j = setdiff(1:nrow(subS), i)
    
    i1 = sample(i, round(0.5*length(i)))
    i2 = setdiff(i, i1)
    
    
    idtrain1S = rownames(subS)[i1]
    idtrain2S = rownames(subS)[i2]
    idtestS = rownames(subS)[j]
    
    train1S = subS[as.numeric(idtrain1S),c('gsim.no','group')]
    train2S = subS[as.numeric(idtrain2S),c('gsim.no','group')]
    testS = subS[as.numeric(idtestS),c('gsim.no','group')]
    
    row.names(train1S) = NULL
    row.names(train2S) = NULL
    row.names(testS) = NULL
    
    listS[[iloop]] = list(train1S = train1S, train2S = train2S, testS = testS)
  }
  
  dir.create(file.path(dirPath, 'processing','class'), showWarnings = F)
  dir.create(file.path(dirPath, 'processing','class',mon), showWarnings = F)
  save(listS, file = file.path(dirPath, 'processing','class',mon ,'M1_datasets.rdata'))
  
  # M2 - S2 -----------------------------------------------------------------
  subS = subset(metaDataF4, metaDataF4$group == 'S2')
  subS$group = 'S'
  row.names(subS) = NULL
  
  listS = list()
  for(iloop in 1: 500){
    
    # for source
    i = sample(1:nrow(subS), round(0.5*nrow(subS)))
    j = setdiff(1:nrow(subS), i)
    
    i1 = sample(i, round(0.5*length(i)))
    i2 = setdiff(i, i1)
    
    
    idtrain1S = rownames(subS)[i1]
    idtrain2S = rownames(subS)[i2]
    idtestS = rownames(subS)[j]
    
    train1S = subS[as.numeric(idtrain1S),c('gsim.no','group')]
    train2S = subS[as.numeric(idtrain2S),c('gsim.no','group')]
    testS = subS[as.numeric(idtestS),c('gsim.no','group')]
    
    row.names(train1S) = NULL
    row.names(train2S) = NULL
    row.names(testS) = NULL
    
    listS[[iloop]] = list(train1S = train1S, train2S = train2S, testS = testS)
  }
  
  dir.create(file.path(dirPath, 'processing','class'), showWarnings = F)
  dir.create(file.path(dirPath, 'processing','class',mon), showWarnings = F)
  save(listS, file = file.path(dirPath, 'processing','class',mon ,'M2_datasets.rdata'))
  
  
  # M3 - S3 -----------------------------------------------------------------
  subS = subset(metaDataF4, metaDataF4$group == 'S3')
  subS$group = 'S'
  row.names(subS) = NULL
  
  listS = list()
  for(iloop in 1: 500){
    
    # for source
    i = sample(1:nrow(subS), round(0.5*nrow(subS)))
    j = setdiff(1:nrow(subS), i)
    
    i1 = sample(i, round(0.5*length(i)))
    i2 = setdiff(i, i1)
    
    
    idtrain1S = rownames(subS)[i1]
    idtrain2S = rownames(subS)[i2]
    idtestS = rownames(subS)[j]
    
    train1S = subS[as.numeric(idtrain1S),c('gsim.no','group')]
    train2S = subS[as.numeric(idtrain2S),c('gsim.no','group')]
    testS = subS[as.numeric(idtestS),c('gsim.no','group')]
    
    row.names(train1S) = NULL
    row.names(train2S) = NULL
    row.names(testS) = NULL
    
    listS[[iloop]] = list(train1S = train1S, train2S = train2S, testS = testS)
  }
  
  dir.create(file.path(dirPath, 'processing','class'), showWarnings = F)
  dir.create(file.path(dirPath, 'processing','class',mon), showWarnings = F)
  save(listS, file = file.path(dirPath, 'processing','class',mon ,'M3_datasets.rdata'))
  
  
  
  # M4 - S1 + S2 ------------------------------------------------------------
  subS = subset(metaDataF4, metaDataF4$group == 'S1' | metaDataF4$group == 'S2')
  subS$group = 'S'
  row.names(subS) = NULL
  
  listS = list()
  for(iloop in 1: 500){
    
    # for source
    i = sample(1:nrow(subS), round(0.5*nrow(subS)))
    j = setdiff(1:nrow(subS), i)
    
    i1 = sample(i, round(0.5*length(i)))
    i2 = setdiff(i, i1)
    
    
    idtrain1S = rownames(subS)[i1]
    idtrain2S = rownames(subS)[i2]
    idtestS = rownames(subS)[j]
    
    train1S = subS[as.numeric(idtrain1S),c('gsim.no','group')]
    train2S = subS[as.numeric(idtrain2S),c('gsim.no','group')]
    testS = subS[as.numeric(idtestS),c('gsim.no','group')]
    
    row.names(train1S) = NULL
    row.names(train2S) = NULL
    row.names(testS) = NULL
    
    listS[[iloop]] = list(train1S = train1S, train2S = train2S, testS = testS)
  }
  
  dir.create(file.path(dirPath, 'processing','class'), showWarnings = F)
  dir.create(file.path(dirPath, 'processing','class',mon), showWarnings = F)
  save(listS, file = file.path(dirPath, 'processing','class',mon ,'M4_datasets.rdata'))
  
  
  # M5 - S1 + S3 ------------------------------------------------------------
  subS = subset(metaDataF4, metaDataF4$group == 'S1' | metaDataF4$group == 'S3')
  subS$group = 'S'
  row.names(subS) = NULL
  
  listS = list()
  for(iloop in 1: 500){
    
    # for source
    i = sample(1:nrow(subS), round(0.5*nrow(subS)))
    j = setdiff(1:nrow(subS), i)
    
    i1 = sample(i, round(0.5*length(i)))
    i2 = setdiff(i, i1)
    
    
    idtrain1S = rownames(subS)[i1]
    idtrain2S = rownames(subS)[i2]
    idtestS = rownames(subS)[j]
    
    train1S = subS[as.numeric(idtrain1S),c('gsim.no','group')]
    train2S = subS[as.numeric(idtrain2S),c('gsim.no','group')]
    testS = subS[as.numeric(idtestS),c('gsim.no','group')]
    
    row.names(train1S) = NULL
    row.names(train2S) = NULL
    row.names(testS) = NULL
    
    listS[[iloop]] = list(train1S = train1S, train2S = train2S, testS = testS)
  }
  
  dir.create(file.path(dirPath, 'processing','class'), showWarnings = F)
  dir.create(file.path(dirPath, 'processing','class',mon), showWarnings = F)
  save(listS, file = file.path(dirPath, 'processing','class',mon ,'M5_datasets.rdata'))
  
  
  
  # M6 - S2 + S3 ------------------------------------------------------------
  subS = subset(metaDataF4, metaDataF4$group == 'S2' | metaDataF4$group == 'S3')
  subS$group = 'S'
  row.names(subS) = NULL
  
  listS = list()
  for(iloop in 1: 500){
    
    # for source
    i = sample(1:nrow(subS), round(0.5*nrow(subS)))
    j = setdiff(1:nrow(subS), i)
    
    i1 = sample(i, round(0.5*length(i)))
    i2 = setdiff(i, i1)
    
    
    idtrain1S = rownames(subS)[i1]
    idtrain2S = rownames(subS)[i2]
    idtestS = rownames(subS)[j]
    
    train1S = subS[as.numeric(idtrain1S),c('gsim.no','group')]
    train2S = subS[as.numeric(idtrain2S),c('gsim.no','group')]
    testS = subS[as.numeric(idtestS),c('gsim.no','group')]
    
    row.names(train1S) = NULL
    row.names(train2S) = NULL
    row.names(testS) = NULL
    
    listS[[iloop]] = list(train1S = train1S, train2S = train2S, testS = testS)
  }
  
  dir.create(file.path(dirPath, 'processing','class'), showWarnings = F)
  dir.create(file.path(dirPath, 'processing','class',mon), showWarnings = F)
  save(listS, file = file.path(dirPath, 'processing','class',mon ,'M6_datasets.rdata'))
  
  
  
  # M7 - S1 + S2 + S3 -------------------------------------------------------
  
  subS = subset(metaDataF4, metaDataF4$group == 'S1' | metaDataF4$group == 'S2' | metaDataF4$group == 'S3')
  subS$group = 'S'
  row.names(subS) = NULL
  
  listS = list()
  for(iloop in 1: 500){
    
    # for source
    i = sample(1:nrow(subS), round(0.5*nrow(subS)))
    j = setdiff(1:nrow(subS), i)
    
    i1 = sample(i, round(0.5*length(i)))
    i2 = setdiff(i, i1)
    
    
    idtrain1S = rownames(subS)[i1]
    idtrain2S = rownames(subS)[i2]
    idtestS = rownames(subS)[j]
    
    train1S = subS[as.numeric(idtrain1S),c('gsim.no','group')]
    train2S = subS[as.numeric(idtrain2S),c('gsim.no','group')]
    testS = subS[as.numeric(idtestS),c('gsim.no','group')]
    
    row.names(train1S) = NULL
    row.names(train2S) = NULL
    row.names(testS) = NULL
    
    listS[[iloop]] = list(train1S = train1S, train2S = train2S, testS = testS)
  }
  
  dir.create(file.path(dirPath, 'processing','class'), showWarnings = F)
  dir.create(file.path(dirPath, 'processing','class',mon), showWarnings = F)
  save(listS, file = file.path(dirPath, 'processing','class',mon ,'M7_datasets.rdata'))
}



