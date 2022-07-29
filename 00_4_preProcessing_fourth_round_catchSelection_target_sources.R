library(rgdal)
library(tidyverse)
#library(splitstackshape)

dirPath = '/media/hung/DATA/0.NCKH/0.MyPaper/2022_climQ'
load(file.path(dirPath,'processing','qsim_metaData_qts_3rdScreening.rData'))
#load(file.path(dirPath,'processedDat','climate.land.meta.rdata'))
#load(file.path(dirPath,'processedDat','ccluccatClass.rdata'))
load(file.path(dirPath, 'processing','gsimCountry.rData'))

source('/home/hung/Dropbox/RStudy/0.Code/built_in/dataframe_support_functions.r')
source('/home/hung/Dropbox/RStudy/0.Code/built_in/spatial_support_functions.r')

wm = readOGR(file.path(dirPath,'spatialDat','world_borders.shp'))

# source(s) and target pre-defined
S1 = create_polygon_box(-135,26,-48,61)
S2 = create_polygon_box(-60,-33,-34,-5)
S3 = create_polygon_box(-12,35,30,60)
T1 = create_polygon_box(22,-35,42,-3)
T2 = create_polygon_box(59.8,-11,121,40)

colnames(gsimCountry)[1] = 'gsim.no'
str(gsimCountry)

metaDataF3$flag = 1
metaDataF3New = right_join(metaDataF3, gsimCountry, by = 'gsim.no')
metaDataF3New = metaDataF3New[which(metaDataF3New$flag==1),]

# initial plot
subnewClass2 = subset(metaDataF3New, metaDataF3New$developNation == 1)
plot(0,0, col = 'white', xlim = c(-180,180), ylim = c(-90,90), xlab = '', ylab = '')
plot(wm, border = 'grey80', add = T)
points(metaDataF3New$long.new, metaDataF3New$lat.new, pch = 16, cex = 0.9, col = 'red')
points(subnewClass2$long.new, subnewClass2$lat.new, pch = 16, cex = 0.9, col = 'blue')
plot(S1, add = T)
plot(S2, add = T)
plot(S3, add = T)
plot(T1, add = T)
plot(T2, add = T)

rawPoints = metaDataF3New[,c('lat.new','long.new','gsim.no','name','developNation','region')]

S1p = pointsData_in_boxShape(rawPoints, S1)
S2p = pointsData_in_boxShape(rawPoints, S2)
S3p = pointsData_in_boxShape(rawPoints, S3)
T1p = pointsData_in_boxShape(rawPoints, T1)
T2p = pointsData_in_boxShape(rawPoints, T2)

# minor adjust for each point sets
S1p  = subset(S1p) 
nrow(S1p)

S2p = S2p #keep as is
nrow(S2p)

S3p = subset(S3p) 
nrow(S3p)

T1p = T1p #keep as is
nrow(T1p)

T2p = T2p
nrow(T2p)

plot(0,0, col = 'white', xlim = c(-180,180), ylim = c(-90,90), xlab = '', ylab = '')
plot(wm, border = 'grey80', add = T)
points(S1p$long.new, S1p$lat.new)
points(S2p$long.new, S2p$lat.new)
points(S3p$long.new, S3p$lat.new)
points(T1p$long.new, T1p$lat.new)
points(T2p$long.new, T2p$lat.new)

save(S1p, S2p, S3p, T1p, T2p, S1, S2, S3, T1, T2, file = file.path(dirPath,'processing','qsim_metaData_4rdScreening_S_T_catch_update.rdata'))
