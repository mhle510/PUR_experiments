dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
ii=1
load(file.path(dirPath,'processing','fullTS_F4',
               paste0(formatC(ii, width = 2, flag = 0), 'Original.rdata')))

####### copy
# for gsim data 
dir.create(file.path(dirPath,'0_input','rawGsim'), showWarnings = F)
dir.create(file.path(dirPath,'0_input','rawGsim','fourthScreen'), showWarnings = F)
dir.create(file.path(dirPath,'0_input','rawGsim','fourthScreen','monts'), showWarnings = F)
dir.create(file.path(dirPath,'0_input','rawGsim','fourthScreen','catchments'), showWarnings = F)

rawPath = file.path(dirPath,'0_input','rawGsim','thirdScreen','monts')
targetPatch = file.path(dirPath,'0_input','rawGsim','fourthScreen','monts')
listFiles = list.files(rawPath, pattern = '.mon', recursive = T)
copyid  = which( substr(listFiles,1, 10) %in% metaDataF4$gsim.no)
summary(copyid)
for(ic in 1 :length(copyid)){
  cat(ic, '\n')
  checkfile = file.path(rawPath, listFiles[copyid[ic]])
  file.copy(checkfile ,targetPatch, overwrite = T)
}

rawPath = file.path(dirPath,'0_input','rawGsim','thirdScreen','catchments')
targetPatch = file.path(dirPath,'0_input','rawGsim','fourthScreen','catchments')
for(ic in 1 :nrow(metaDataF4)){
  sname = tolower(metaDataF4$gsim.no[ic])
  cat(ic,'--',sname,'\n')
  checkfiles = list.files(path = rawPath,
                          pattern = sname, recursive = T, full.names = T)
  file.copy(checkfiles ,targetPatch)
}


####### Export meta data for publication

dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
dir.create(file.path(dirPath, 'processing','publisheddata'), showWarnings = F)
processedPath = (file.path(dirPath, 'processing','publisheddata'))

# raw GSIM
raw.gsim.meta = read.csv(file.path('H:/GlobalRunoff/GSIM','GSIM_metadata/GSIM_catalog/GSIM_metadata.csv'))

#after the first step selection
load(file.path(dirPath,'processing','qsim_metaData_qts_3rdScreening.rData'))
firststep.gsim.meta = metaDataF3
write.csv(firststep.gsim.meta, file.path(processedPath,'meta.gsim.afterfirststep.csv'), row.names = F)

#after the second step selection
load(file.path(dirPath,'processing','fullTS_F4','01Original.rdata'))
secondstep.gsim.meta = DfFinalF4
rmid = which(colnames(secondstep.gsim.meta) %in% c('P','T','Q','climate.type','landcover.type'))
write.csv(secondstep.gsim.meta[,-rmid], file.path(processedPath,'meta.gsim.aftersecondstep.csv'), row.names = F)

#dataset for different months
for(im in 1 : 12){
  load.file =  paste(dirPath,'/', 'processing','/','fullTS_F4', '/',formatC(im, width = 2, flag = 0), 'Original.rdata', sep = '')
  load(load.file)
  meta.file = DfFinalF4
  rmid = which(colnames(secondstep.gsim.meta) %in% c('climate.type','landcover.type'))
  write.csv(meta.file[,-rmid], file.path(processedPath,
                                         paste0(formatC(im, width = 2, flag = 0),'dataset.csv')), row.names = F)
}
