####################################################################################
# Calc Streamflow from QSIM
# Manh-Hung Le - 2021 Apr 27
# objectives:
# - select stations belong major land cover and climatic condition
# - remove NA from 18 predictors
# - calculate annual streamflow, remove extreme cases
# - copying catchments and areal noah data to specific folders


# gldas datasets have been extracted beforehand with a total of 4894 stations
####################################################################################
library(lubridate)
library(tidyverse)
library(rgdal)
library(psych)

# input data
dirPath = 'XXX/2022_climQ'
qPath = 'XXX/rawGsim/TIMESERIES_SUB2'
pPath = 'XXX/2022_climQ/0_input/gldas/csv/precip'
tPath = 'XXX/2022_climQ/0_input/gldas/csv/airt'
#etPath = 'D:/0.NCKH/0.MyPaper/2021_GlobalQ_AI/noah/csv/et'
#smPath = 'D:/0.NCKH/0.MyPaper/2021_GlobalQ_AI/noah/csv/sm'
gsimOrig = read.csv('XXX/2022_climQ/0_input/gsimInfoSubNewRule_4894stations.csv')
# load gsim data
load(file.path(dirPath,'processing','qsim_metaData_qts_1st2ndScreening.rdata'))
metaData = gsimInfoSub22

proccessedPath = file.path(dirPath,'processing')
#dir.create(file.path(dirPath, 'figs'), showWarnings = F)
#dir.create(proccessedPath, showWarnings = F)
dir.create(file.path(proccessedPath,'fullTS'), showWarnings = F)

# own-built function
source(file.path(dirPath,'rCode','gsim_support_functions.r'))

# reading noah data
selectedLoc = which(gsimOrig$gsim.no %in% metaData$gsim.no ) #(find location of small set in big set)
pList = list.files(pPath, pattern = ".csv", 
                        full.names = T, recursive = T)
tList = list.files(tPath, pattern = ".csv", 
                    full.names = T, recursive = T)
pList = pList[selectedLoc]
tList = tList[selectedLoc]

pName = str_remove(basename(pList),'.csv')

pDat = lapply(pList, function(x) read.csv(x, header = TRUE))
tDat = lapply(tList, function(x) read.csv(x, header = TRUE))
#smDat = lapply(smList, function(x) read.csv(x, header = TRUE))


n = length(qMonValF22)
# annual calculation
annualDf = data.frame(Name = character(n),
                      Q = numeric(n),
                      P = numeric(n),
                      T = numeric(n))
for(ii in 1: n){
        print(ii)
        # Q process
        qts = qMonValF22[[ii]]
        pts = pDat[[ii]]
        tts = tDat[[ii]]

        gsimdate = make_date(year = qts$year, month = qts$month, 1)
        noahdate = make_date(year = as.numeric(substr(pts$date,1,4)),
                             month = as.numeric(substr(pts$date,5,6)),
                             1)
        # update noah data to match with streamflow data
        noahloc = which(noahdate %in% gsimdate)
        pts = pts[noahloc,]
        tts = tts[noahloc,]
        
        # annual data
        annualDf$Name[ii] = metaData$gsim.no[ii]
        annualDf$Q[ii] = round(mean(qts$MEAN, na.rm = T),2)
        annualDf$P[ii] = round(mean(pts[,2], na.rm = T),2)
        annualDf$T[ii] = round(mean(tts[,2], na.rm = T),2)
}

summary(annualDf)

# other features

# area.meta:  :catchment size information from metadata in km2 (if available)
# altitude.meta :height of gauge above sea level (m) reported from metadata
# climate.type : catchment climate (major groups of Koppen-Geiger system) if one Climate type present over more than 50% catchment area, otherwise 'No dominant
 #                                class' value was assigned
# dr.mean   :average catchment drainage density 
# ele.mean  :average values of catchment elevation
# ir.mean :average of catchment irrigation area
# landcover.type :catchment land-cover (UN Classification System for 2015) if one single land-cover type present over more than 50% catchment area, otherwise 
#                 'No dominant class' value was assigned
# lithology.type  :catchment lithology (16 classes by Dürr et al. [2005] (*)) if one single lithology type present over more than 50% catchment area, otherwise 'No 
#                                dominant class' value was assigned
# slp.mean :average of slope within catchment boundary
# pd.mean : average values of population density within catchment boundary (GPWv4 - 2010)
# sb.mean: average  values of bulk density in kg/cubic-meter within catchment boundary
# scl.mean: average values of clay content in soil profile within catchment boundary
# snd.mean: average values of sand content in soil profile within catchment boundary
# slt.mean: average values of silt content in soil profile within catchment boundary
# soil.type: catchment soil class (WRB) if one single soil class present over more than
#            50% catchment area, otherwise 'No dominant class' value was assigned
# tp.mean: average  values of topographic index within catchment boundary


atrNamesNumeric = c('long.new','lat.new','area.meta','altitude.dem', 'dr.mean','ele.mean','ir.mean',
                   'slp.mean','pd.mean','sb.mean','scl.mean','snd.mean','slt.mean',  'tp.mean')

atrNamesCategory = c('climate.type','landcover.type')

# remove NA values for attributes
annualDfAtr = data.frame(annualDf, metaData[,atrNamesNumeric], metaData[,atrNamesCategory])
annualDfAtr = annualDfAtr[complete.cases(annualDfAtr[,4:18]),]
adjLoc = which(metaData$gsim.no %in% annualDfAtr$Name)
metaDataAdj = metaData[adjLoc,]


# remove extrem case
boxplot(annualDfAtr$Q)
which(annualDfAtr$Q > 600)

# remove extreme case

annualDfAtr2 = annualDfAtr[-704,]
metaDataAdj2 = metaDataAdj[-704,]

boxplot(annualDfAtr2$Q)

# determine climate type and land cover type
table(annualDfAtr2$climate.type)
table(annualDfAtr2$landcover.type)

#filter for climate type
annualDfAtr3 = annualDfAtr2[annualDfAtr2$climate.type %in% c('Arid','Equatorial','Snow','Warm Temperate'),]
metaDataAdj3 = metaDataAdj2[metaDataAdj2$gsim.no %in% annualDfAtr3$Name,]

table(annualDfAtr3$climate.type)

#filter for land cover type
annualDfAtr4 = annualDfAtr3[annualDfAtr3$landcover.type %in% c('Agriculture','Forest','Grassland','No dominant class','Shrubland'),]
metaDataAdj4 = metaDataAdj3[metaDataAdj3$gsim.no %in% annualDfAtr4$Name,]

table(annualDfAtr4$climate.type)
table(annualDfAtr4$landcover.type)

monthCount2 = data.frame(gsim.no = metaData$gsim.no,
                         monthCount1)
monthCount2  = monthCount2[monthCount2$gsim.no %in% annualDfAtr4$Name,]

annualDfFinal = data.frame(gsimNo = annualDfAtr4$Name,
                           annualDfAtr4[,-c(1,2)],
                           Q = annualDfAtr4$Q)
metaDataFinal = metaDataAdj4

# plot spatial
wm = readOGR('E:/GlobalRunoff/GSIM/spatialDat/world_borders.shp')
plot(wm, border = 'grey')
points(annualDfFinal$long.new,annualDfFinal $lat.new)



# plot box-plot Q
boxplot(annualDfFinal$Q)
summary(annualDfFinal[,-c(18,19,20)])


# export final filter data
fileOp = file.path(proccessedPath,'fullTS','annualOriginal.rdata')
save(monthCount2, annualDfFinal, metaDataFinal, file = fileOp)

# export metaData and streamflow time series
names(qMonValF22)

qMonValF3 = qMonValF22
id = which(names(qMonValF3) %in% metaDataFinal$gsim.no )
summary(id)
qMonValF3 = qMonValF3[id]

metaDataF3 = metaDataFinal
fileOp = file.path(proccessedPath,'qsim_metaData_qts_3rdScreening.rData')
save(qMonValF3, metaDataF3, file = fileOp )



# copying files -----------------------------------------------------------
library(tidyverse)

# noah files
# for air temp
rawPath = file.path(dirPath,'0_input','gldas','csv','airt')
dir.create(file.path(dirPath,'0_input','gldas','csvthirdScreen'), showWarnings = F)
dir.create(file.path(dirPath,'0_input','gldas','csvthirdScreen','airT'), showWarnings = F)
targetPatch = file.path(dirPath,'0_input','gldas','csvthirdScreen','airT')
listFiles = list.files(rawPath, pattern = '.csv', recursive = T)
copyid  = which( substr(listFiles,1, 10) %in% metaDataFinal$gsim.no)
summary(copyid)

for(ic in 1 :length(copyid)){
        cat(ic, '\n')
        checkfile = file.path(rawPath, listFiles[copyid[ic]])
        file.copy(checkfile ,targetPatch, overwrite = T)
        
}

# for precip
rawPath = file.path(dirPath,'0_input','gldas','csv','precip')
dir.create(file.path(dirPath,'0_input','gldas','csvthirdScreen'), showWarnings = F)
dir.create(file.path(dirPath,'0_input','gldas','csvthirdScreen','precip'), showWarnings = F)
targetPatch = file.path(dirPath,'0_input','gldas','csvthirdScreen','precip')
listFiles = list.files(rawPath, pattern = '.csv', recursive = T)
copyid  = which( substr(listFiles,1, 10) %in% metaDataFinal$gsim.no)
summary(copyid)

for(ic in 1 :length(copyid)){
        cat(ic, '\n')
        checkfile = file.path(rawPath, listFiles[copyid[ic]])
        file.copy(checkfile ,targetPatch, overwrite = T)
        
}

# for gsim data 
dir.create(file.path(dirPath,'0_input','rawGsim'), showWarnings = F)
dir.create(file.path(dirPath,'0_input','rawGsim','thirdScreen'), showWarnings = F)
dir.create(file.path(dirPath,'0_input','rawGsim','thirdScreen','monts'), showWarnings = F)
dir.create(file.path(dirPath,'0_input','rawGsim','thirdScreen','catchments'), showWarnings = F)

rawPath = 'E:/GlobalRunoff/GSIM/GSIM_indices/TIMESERIES/monthly'
targetPatch = file.path(dirPath,'0_input','rawGsim','thirdScreen','monts')
listFiles = list.files(rawPath, pattern = '.mon', recursive = T)
copyid  = which( substr(listFiles,1, 10) %in% metaDataFinal$gsim.no)

for(ic in 1 :length(copyid)){
        cat(ic, '\n')
        checkfile = file.path(rawPath, listFiles[copyid[ic]])
        file.copy(checkfile ,targetPatch, overwrite = T)
}

rawPath = 'XXX/GSIM/GSIM_metadata/GSIM_catchments'
targetPatch = file.path(dirPath,'0_input','rawGsim','thirdScreen','catchments')
for(ic in 1 :nrow(metaDataFinal)){
        sname = tolower(metaDataFinal$gsim.no[ic])
        cat(ic,'--',sname,'\n')
        checkfiles = list.files(path = rawPath,
                                pattern = sname, recursive = T, full.names = T)
        file.copy(checkfiles ,targetPatch)
}
