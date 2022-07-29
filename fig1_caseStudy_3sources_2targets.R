library(rgdal)
library(tidyverse)
library(colorRamps)
library(fields)
#source('/home/hung/Dropbox/RStudy/0.Code/built_in/plot_support_functions.r')
source('C:/Users/manhh/Dropbox/RStudy/0.Code/built_in/plot_support_functions.r')

#dirPath = '/media/hung/DATA/0.NCKH/0.MyPaper/2022_climQ'
dirPath = '/media/hung/DATA/0.NCKH/0.MyPaper/2022_climQ'
dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'

dir.create(file.path(dirPath,'1_figs'), showWarnings = F)
figPath = file.path(dirPath,'1_figs')



# load meta files
load(file.path(dirPath,'processing','qsim_metaData_4rdScreening_S_T_catch_update.rdata'))
load(file.path(dirPath,'processing','fullTS_F4',
               paste0(formatC(1, width = 2, flag = 0), 'Original.rdata')))

# load world map
# wm = readOGR('/media/hung/Seagate Backup Plus Drive/GlobalRunoff/GSIM/spatialDat/world_borders.shp')
wm = readOGR('H:/GlobalRunoff/GSIM/spatialDat/world_borders.shp')

# S1 = create_polygon_box(-135,26,-48,61)
# S2 = create_polygon_box(-60,-33,-34,-5)
# S3 = create_polygon_box(-12,35,30,60)
# T1 = create_polygon_box(22,-35,42,-3)
# T2 = create_polygon_box(59.8,-11,121,40)
coordinateTextS = data.frame(x = c(-134,-60, -11), y = c(22,-37,31))
textNameS = c('S1(n=987)', 'S2(n=813)', 'S3(n=457)')


coordinateTextT = data.frame(x = c(20, 61), y = c(-39,-15))
textNameT= c('T1(n=81)','T2(n=133)')

catchT = subset(metaDataF4, metaDataF4$group == 'T2' | metaDataF4$group == 'T1')
catchS = metaDataF4[which(! metaDataF4$group %in% catchT$group),]
  
pdf(file.path(figPath,'fig01_caseStudy.pdf'), width = 6.5, height = 3)
par(mfrow = c(1,1), mar=c(2,2,0.5,0.5), mgp=c(1.25,0.25,0), tck=-0.01 ,cex = 0.8)
plot(0,0, col = 'white', ylim = c(-60,60), xlim = c(-150, 150), xlab = '', ylab = '')
plot(wm, border = 'grey80', add = T)
abline(h = seq(-60, 60, 20), lwd = 0.2, lty = 2, col = 'grey80')
abline(v = seq(-140, 140, 20), lwd = 0.2, lty = 2, col = 'grey80')
plot(S1, border = 'blue', add = T)
plot(S2, border = 'blue', add = T)
plot(S3, border = 'blue', add = T)
plot(T1, border = 'red', add = T)
plot(T2, border = 'red', add = T)
points(catchS$long.new, catchS$lat.new, col = t_col('blue'), cex = 0.85, pch = 16)
points(catchT$long.new, catchT$lat.new, col = t_col('red'), cex = 0.85, pch = 16)
text(coordinateTextS,  labels = textNameS, adj = 0, col = 'blue', cex = 0.8)
text(coordinateTextT,  labels = textNameT, adj = 0, col = 'red', cex = 0.8)

dev.off()
