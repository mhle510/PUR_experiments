####################################################################################
# Find which
# Manh-Hung Le - 2021 June 24
# objectives:
# - screening datasets
# - filter metadata with specific rules
# - ouput: qMonVal (three cols: date, val, no.good.value); gsimInfo (corresponding metadata)

####################################################################################
require(rgdal)
require(tidyverse)
require(quanteda)
require(lubridate)
library(raster)
require(tidyverse)
require(quanteda)
require(lubridate)


# path setup
projPath = 'XXX/GSIM'
dir.create(file.path(projPath,'flowsignalPrediction'),showWarnings = F)
processPath = file.path(projPath,'flowsignalPrediction')

dir.create(file.path('XXX/2022_climQ','processing'), showWarnings = F)
outPath = file.path('XXX/2022_climQ','processing')

# meta data
gsimCatch = read.csv(file.path(projPath,'GSIM_metadata/GSIM_catalog/GSIM_catchment_characteristics.csv'))
gsimMetadata = read.csv(file.path(projPath,'GSIM_metadata/GSIM_catalog/GSIM_metadata.csv'))
gsimHomogeneity = read.csv(file.path(projPath, 'GSIM_indices/HOMOGENEITY/monthly_homogeneity.csv'))
gsimInfo = full_join(gsimCatch, gsimMetadata, by ='gsim.no')
gsimInfo = full_join(gsimInfo, gsimHomogeneity, by ='gsim.no')

# check NA for gsimno
naloc = which(is.na(gsimInfo$gsim.no))

# load shapfile
wm = readOGR('XXX/GSIM/spatialDat/world_borders.shp')

# own-built function
source(file.path(projPath, 'Rcode','gsim_support_functions.r'))
source('XXX/dataframe_support_functions.r')

# first screening: inital selection-------------------------------------------------
# Criteria:
# select data with more than 10 years and good data greater than 120
# select basin with good delineation (flagged with high and medium)
# select basin within 625-10000 km2
# select basin with likely less human impact (no. of dams  = 0)
gsimInfoSub = filter(gsimInfo, year.no >= 10) %>% # more than 10 year
  filter((year.end - 1948) >= 10) %>% # more than 10 year after 1981
  filter((-year.start + 2014) >= 10) %>% # no later than 2014 - fit with NOAH V20
  filter(number.good.time.steps >= 120) %>%
  filter(quality == 'High' | quality == 'Medium'  ) %>% # good delineation
  filter(area.meta >= 625) %>%  #area greater than 625
  filter(area.meta <= 10000) %>% # area smaller than 10000
  filter(no.dams == 0) # unregulated basin
# check order of row names
plot(rownames(gsimInfoSub))
# check conditions
hist(gsimInfoSub$year.end - 1948)
hist(2014 - gsimInfoSub$year.start)
#hist(gsimInfoSub$area)


# plot for first screening
plot(wm, border = 'grey')
points(gsimInfoSub$long.new, gsimInfoSub$lat.new)

# how many stations after the first screening 
nrow(gsimInfoSub)

# Second screening: clean time series - part 1 --------------------------------------------------------
# input: qMonVal and gsimInforSub
# Remove timeservers with unrealistic runoff rate 
orimonPath = 'E:/GlobalRunoff/GSIM/GSIM_indices/TIMESERIES/monthly'
orimonFiles = list.files(orimonPath , pattern = '.mon', full.names = T, recursive = T)
origsimNo = basename(orimonFiles) %>% str_remove_all('.mon') #4894 files 
head(origsimNo)

# location of station name in the first screening (find location of small set in big set)
locgsimF1 = which(origsimNo %in% gsimInfoSub$gsim.no) # big set - small set

# check
origsimNo[locgsimF1[[12]]]
gsimInfoSub[12,1:10]

# second screening with unrealistic time series
monFilesF1 = orimonFiles[locgsimF1]
sname = origsimNo[locgsimF1]
n = length(monFilesF1)
qDatF1 = lapply(monFilesF1, function(x) read.gsim.ts(x))

# take date and mean value and navailable
qMonValF1 = lapply(qDatF1, '[',,c(1,2,11))
qMonValMaxF1 = sapply(qMonValF1, function(x) max(x[,2], na.rm = T))
qMonValMinF1 = sapply(qMonValF1, function(x) min(x[,2], na.rm = T))

summary(qMonValMaxF1)
summary(qMonValMinF1)

# assign names for monthly streamflow timeseries
names(qMonValF1) = sname 
#names(qMonValF1)

# time series with monthly rate greater than 2000
loc2000 = which(qMonValMaxF1 >= 2000)
# time series with negative values
locNeg = which(qMonValMinF1 < 0)
loc2000
locNeg
# manually screening
t1 = qDatF1[[62]]
plot(t1$MEAN)
basename(monFilesF1[62])
gsimInfoSub[62,1:10]

t2 = qDatF1[[1608]]
plot(t2$MEAN)
basename(monFilesF1[1608])
gsimInfoSub[1608,1:10]

t2 = qDatF1[[2800]]
plot(t2$MEAN)
basename(monFilesF1[2800])
gsimInfoSub[2800,1:10]
# treat unrealistic data
nF1 = length(qMonValF1)

qMonValF2 = qMonValF1
for(ii in 1:nF1){
  datsub = qMonValF1[[ii]]
  datval = datsub[,2]
  datval[datsub[,2] == Inf] = NA
  datval[datsub[,2] == -Inf] = NA
  datval[datsub[,2] > 2000 ] = NA # assign NA for value greater than 2000 per month
  datval[datsub[,2] <= 0.1 ] = NA # assign NA for  negative value
  datval[datsub[,3] < 15] = NA # assign NA for month with smaller than 15 values per month
  # assign back value for qMonVal
  datsub[,2] = datval
  qMonValF2[[ii]] = datsub
}

# recheck again
qMonValMax = sapply(qMonValF2, function(x) max(x[,2], na.rm = T))
qMonValMin = sapply(qMonValF2, function(x) min(x[,2], na.rm = T))

summary(qMonValMax)
summary(qMonValMin)

which(qMonValMax == -Inf)
which(qMonValMin == Inf)

gsimInfoSub[3253, 1: 10]
dat3253 = qMonValF2[[3253]]

# remove station 3253
qMonValF2 = qMonValF2[-3253]
gsimInfoSub2 = gsimInfoSub[-3253,]

# recheck
qMonValMax = sapply(qMonValF2, function(x) max(x[,2], na.rm = T))
qMonValMin = sapply(qMonValF2, function(x) min(x[,2], na.rm = T))

summary(qMonValMax)
summary(qMonValMin)

names(qMonValF2)

# Second screening - part 2---------------------------------------------------------
# input: qMonVal1 and gsimInforSub1
# check whether each month has at least 5 values (for average)
nFilter2 = length(qMonValF2)

flagged = rep(0, nFilter2)
# flag is equal to 1 as there is a month with less than 5 or NA and within 1948-2012
qMonValF22 = qMonValF2
monthCount = mat.or.vec(nFilter2,12)
for(ii in 1: nFilter2){
  datsub = qMonValF22[[ii]]
  datsub = datsub[complete.cases(datsub$MEAN),]
  datsub$date = lubridate::ymd(datsub$date)
  datsub$year = year(datsub$date)
  datsub$month = month(datsub$date)
  
  locrange = which(datsub$year >= 1948 & datsub$year <=2012)
  datsub = datsub[locrange,]
  mc = table(datsub$month) %>% as.data.frame()
  
  if(nrow(mc) == 12){ 
    cc = mc[,2]
  } else {
    colnames(mc) = c('mm','val')
    missM = which(!seq(1,12,1) %in% mc$mm)
    missM = data.frame(mm = missM, val = NA)
    mcAdj = rbind.data.frame(missM, mc)
    mcAdj = mcAdj[order(mcAdj$mm),]
    cc = mcAdj[,2]
    flagged[ii] = 1
  }
  
  monthCount[ii,] = cc
  loc = which(monthCount[ii,] < 5)
  if(length(loc) > 0 ) {flagged[ii] = 1}
  qMonValF22[[ii]] = datsub
}

locFlagged = which(flagged > 0)

monthCount[34,]
t1 = qMonValF22[[34]]
plot(t1$MEAN)
gsimInfoSub2[34,1:10]

# remove not qualify datasets
monthCount1 = monthCount[-locFlagged,]
qMonValF22 = qMonValF22[-locFlagged]
gsimInfoSub22 = gsimInfoSub2[-locFlagged,]

# min count minimum of five
summary(monthCount1)

# total basin after the second screening
nrow(gsimInfoSub22)

# check names in final
head(names(qMonValF22))
head(gsimInfoSub22[,1:6])
head(monthCount1)

save(gsimInfoSub22,  monthCount1, qMonValF22,
     file = file.path(outPath,'qsim_metaData_qts_1st2ndScreening.rdata'))
