
# output: create FullTS_F41_TSv1 (Filter fouth time and assigning labels (target, sources) for catchments)
library(tidyverse)

dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
load(file = file.path(dirPath,'processing','qsim_metaData_4rdScreening_S_T_catch_update.rdata'))
load(file = file.path(dirPath,'processing','obsDat.rdata'))


# calculate and plot climatology Q
calc_climQ = function(submeta, obsDat){
  rownames(submeta) = NULL
  rownames(obsDat) = NULL
  id = which(obsDat$gsim.no %in% submeta$gsim.no)
  subQ = obsDat[id,]
  climsubQ = colMeans(subQ[,3:14])
  return(climsubQ)
}

# estimation of monthly climatology at S1
S1ClimQ = calc_climQ(S1p, obsDat)
S2ClimQ = calc_climQ(S2p, obsDat)
S3ClimQ = calc_climQ(S3p, obsDat)
T1ClimQ = calc_climQ(T1p, obsDat)
T2ClimQ = calc_climQ(T2p, obsDat)

yrange = range(c(S1ClimQ, S2ClimQ, S3ClimQ, T1ClimQ, T2ClimQ))

plot(log10(S1ClimQ), ylim = log10(yrange), type = 'l', col = 'red')
points(log10(S2ClimQ), type = 'l', col = 'dark red')
points(log10(S3ClimQ), type = 'l', col = 'dark red')
points(log10(T1ClimQ), type = 'l', col = 'blue')
points(log10(T2ClimQ), type = 'l', col = 'dark blue')


# -------------------------------------------------------------------------
# assign labels for each region for input datasets
dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
load(file = file.path(dirPath,'processing','qsim_metaData_4rdScreening_S_T_catch_update.rdata'))
processedPath = file.path(dirPath,'processing','fullTS_F4')
dir.create(processedPath, showWarnings = F)

group = data.frame(gsim.no = c(S1p$gsim.no, S2p$gsim.no, S3p$gsim.no, T1p$gsim.no, T2p$gsim.no),
                   group = c(rep('S1', nrow(S1p)),rep('S2', nrow(S2p)),rep('S3', nrow(S3p)),
                             rep('T1', nrow(T1p)),rep('T2', nrow(T2p))))

for(im in 1 : 12){
  loadFile = paste(dirPath,'/', 'processing','/','fullTS', '/',formatC(im, width = 2, flag = 0), 'Original.rdata', sep = '')
  load(loadFile)
  colnames(DfFinal)[1] = 'gsim.no'
  
  # Df 
  DfFinalF4 = right_join(group, DfFinal)
  DfFinalF4 = DfFinalF4[complete.cases(DfFinalF4$group),]
  
  # meta
  metaDataF4 = right_join(group, metaDataF3)
  metaDataF4 = metaDataF41[complete.cases(metaDataF4$group),]
  
  opFile =  paste(dirPath,'/', 'processing','/','fullTS_F4', '/',formatC(im, width = 2, flag = 0), 'Original.rdata', sep = '')
  
  save(DfFinalF4, metaDataF4, file = opFile)
}

