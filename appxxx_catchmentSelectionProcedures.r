library(rgdal)
source('C:/Users/manhh/Dropbox/RStudy/0.Code/built_in/plot_support_functions.r')

dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
figPath = file.path(dirPath,'1_figs')

# raw GSIM
raw.gsim.meta = read.csv(file.path('H:/GlobalRunoff/GSIM','GSIM_metadata/GSIM_catalog/GSIM_metadata.csv'))

#after the first step selection
firststep.gsim.meta = read.csv(file.path(dirPath, 'processing','publisheddata','meta.gsim.afterfirststep.csv'))

#after the second step selection
secondstep.gsim.meta = read.csv(file.path(dirPath, 'processing','publisheddata','meta.gsim.aftersecondstep.csv'))

# load world map
wm = readOGR('H:/GlobalRunoff/GSIM/spatialDat/world_borders.shp')

# load boundary for the second step
load(file.path(dirPath,'processing','qsim_metaData_4rdScreening_S_T_catch_update.rdata'))

pdf(file.path(figPath,'app_catchmentselection.pdf'), width = 5.8, height = 8)
par(mfrow = c(3,1), mar=c(2,2,1.5,0.5), mgp=c(1.25,0.25,0), tck=-0.01 ,cex = 0.8)

# raw GSIM
plot(0,0, col = 'white', ylim = c(-60,60), xlim = c(-150, 150), xlab = '', ylab = '')
plot(wm, border = 'grey80', add = T)
abline(h = seq(-60, 60, 20), lwd = 0.2, lty = 2, col = 'grey80')
abline(v = seq(-140, 140, 20), lwd = 0.2, lty = 2, col = 'grey80')
points(raw.gsim.meta$longitude, raw.gsim.meta$latitude, col = t_col('black'), cex = 0.6, pch = 16)
mtext(paste0('(a) raw GSIM dataset (n = ', nrow(raw.gsim.meta),')'), adj = 0, col = 'darkred', font = 2, cex = 0.9)

# after the first step
plot(0,0, col = 'white', ylim = c(-60,60), xlim = c(-150, 150), xlab = '', ylab = '')
plot(wm, border = 'grey80', add = T)
abline(h = seq(-60, 60, 20), lwd = 0.2, lty = 2, col = 'grey80')
abline(v = seq(-140, 140, 20), lwd = 0.2, lty = 2, col = 'grey80')
points(firststep.gsim.meta$long.new, firststep.gsim.meta$lat.new, col = t_col('black'), cex = 0.6, pch = 16)
mtext(paste0('(b) after the first step (n = ', nrow(firststep.gsim.meta),')'), adj = 0, col = 'darkred', font = 2, cex = 0.9)

# after the second step
plot(0,0, col = 'white', ylim = c(-60,60), xlim = c(-150, 150), xlab = '', ylab = '')
plot(wm, border = 'grey80', add = T)
abline(h = seq(-60, 60, 20), lwd = 0.2, lty = 2, col = 'grey80')
abline(v = seq(-140, 140, 20), lwd = 0.2, lty = 2, col = 'grey80')
points(secondstep.gsim.meta$long.new,secondstep.gsim.meta$lat.new, col = t_col('black'), cex = 0.6, pch = 16)
plot(S1, border = 'black', add = T)
plot(S2, border = 'black', add = T)
plot(S3, border = 'black', add = T)
plot(T1, border = 'black', add = T)
plot(T2, border = 'black', add = T)
mtext(paste0('(c) after the second step (n = ', nrow(secondstep.gsim.meta),')'), adj = 0, col = 'darkred', font = 2, cex = 0.9)

dev.off()