dirPath = '/media/hung/DATA/0.NCKH/0.MyPaper/2022_climQ'
figPath = file.path(dirPath,'1_figs')

source('/home/hung/Dropbox/RStudy/0.Code/built_in/plot_support_functions.r')
### benchmark models at target
load(file.path(dirPath, 'processing','svmModelPerf_bm.rData'))
load(file.path(dirPath, 'processing','rfModelPerf_bm.rData'))
load(file.path(dirPath, 'processing','xgbModelPerf_bm.rData'))


# sources to targets 
load(file.path(dirPath, 'processing','sum_svm_sources_at_targets.rData'))
load(file.path(dirPath, 'processing','sum_rf_sources_at_targets.rData'))
load(file.path(dirPath, 'processing','sum_xgb_sources_at_targets.rData'))
# performances at target regions using source models (different experiments)
reg = 2 # 1 is Africa, 2 is Asia
exs = seq(1,7,1) # experiment 1 to 7

# finding maximum and minimum values for each region


miny = 10; maxy = -10
for(ie in 1:length(exs)){
  exp = exs[ie]
  localSimbm = svmModel7EXperf[[reg]]$CI05
  sourceSim = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                         svm = svmsourcesAll[[reg]][[exp]]$CI05,
                         rf = rfsourcesAll[[reg]][[exp]]$CI05,
                         xgb = xgbsourcesAll[[reg]][[exp]]$CI05)
  yrange = range(c(sourceSim[,-1], localSimbm))
  if(yrange[1] < miny) {miny = yrange[1]}
  if(yrange[2] > maxy) {maxy = yrange[2]}
}
cat(miny, maxy,'\n')

miny = 10; maxy = -10
for(ie in 1:length(exs)){
  exp = exs[ie]
  localSimbm = svmModel7EXperf[[reg]]$CI95
  sourceSim = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                         svm = svmsourcesAll[[reg]][[exp]]$CI95,
                         rf = rfsourcesAll[[reg]][[exp]]$CI95,
                         xgb = xgbsourcesAll[[reg]][[exp]]$CI95)
  yrange = range(c(sourceSim[,-1], localSimbm))
  if(yrange[1] < miny) {miny = yrange[1]}
  if(yrange[2] > maxy) {maxy = yrange[2]}
}
cat(miny, maxy,'\n')

# final range -1.94, 0.83

yrange = c(-0.78,0.82)

exNames = paste0('EX',1:7)
modNames = c('SVM','RF','XGB')
order1 = c('(a','(b','(c','(d','(e','(f','(g')
order2 = c('-1) ','-2) ','-3) ')

panelNames = NULL
for(iex in 1:7){
  panelNames = c(panelNames, paste0(order1[iex], order2, exNames[iex],'-',modNames) )
}
panelNames
# panelNames = c('(a-1) Africa - EX1','(a-2) Asia - EX1',
#                '(b-1) Africa - EX2','(b-2) Asia - EX2',
#                '(c-1) Africa - EX3','(c-2) Asia - EX3',
#                '(d-1) Africa - EX4','(d-2) Asia - EX4',
#                '(e-1) Africa - EX5','(e-2) Asia - EX5',
#                '(f-1) Africa - EX6','(f-2) Asia - EX6',
#                '(g-1) Africa - EX7','(g-2) Asia - EX7')

pdf(file.path(figPath,'figxxx_performances_at_target_T2_sources_models_update.pdf'), width = 6, height = 8)
#lo = matrix(c(seq(1,21,1),22,22,22), ncol = 3, byrow = T)
#layout(lo, heights = c(rep(0.8,7),0.4))

par(mfrow = c(7,3), mar=c(2.2,2,1,1),mgp=c(1.8,0.25,0),oma = c(0,0,1,1), tck=-0.05, 
    cex  = 0.7, font.lab = 2)
id = 1
for(ie in 1:length(exs)){
  exp = exs[ie]
  # benchmark model
  localSimbm_lb = svmModel7EXperf[[reg]]$CI05
  localSimbm_mi = svmModel7EXperf[[reg]]$CI50
  localSimbm_ub = svmModel7EXperf[[reg]]$CI95
  
  # sources with confidence interval
  sourceSim_lb = data.frame(svm = svmsourcesAll[[reg]][[exp]]$CI05,
                            rf = rfsourcesAll[[reg]][[exp]]$CI05,
                            xgb = xgbsourcesAll[[reg]][[exp]]$CI05)
  
  sourceSim_mi = data.frame(svm = svmsourcesAll[[reg]][[exp]]$CI50,
                            rf = rfsourcesAll[[reg]][[exp]]$CI50,
                            xgb = xgbsourcesAll[[reg]][[exp]]$CI50)
  
  sourceSim_ub = data.frame(svm = svmsourcesAll[[reg]][[exp]]$CI95,
                            rf = rfsourcesAll[[reg]][[exp]]$CI95,
                            xgb = xgbsourcesAll[[reg]][[exp]]$CI95)
  
  diffChar = c(svm = median(svmsourcesAll[[reg]][[exp]]$CI50 - svmModel7EXperf[[reg]]$CI50),
                        rf = median(rfsourcesAll[[reg]][[exp]]$CI50 - svmModel7EXperf[[reg]]$CI50),
                        xgb = median(xgbsourcesAll[[reg]][[exp]]$CI50 - svmModel7EXperf[[reg]]$CI50))
  diffChar = round(diffChar,2)
  # panel 1'
  polLocal = data.frame(x = c(1, seq(1,12,1),seq(12,2,-1)),
                        y = c(localSimbm_ub[1], localSimbm_lb, localSimbm_ub[seq(12,2,-1)]))
  plot(localSimbm_mi, type = 'l', ylim = yrange, col = 'gray', lwd = 2,xlab="",ylab="", xaxt = 'n')
  polygon(polLocal, col = t_col('gray', percent = 60), border = NA)
  
  polSVM = data.frame(x = c(1, seq(1,12,1),seq(12,2,-1)),
                        y = c(sourceSim_ub$svm[1], sourceSim_lb$svm, sourceSim_ub$svm[seq(12,2,-1)]))
  points(1:12, sourceSim_mi$svm, type = 'l',col = 'red', lwd = 2)
  polygon(polSVM, col = t_col('red', percent = 60), border = NA)
  axis(1, at = seq(1, 12, 1), labels = c('J','F','M','A','M','J','J','A','S','O','N','D'))
  mtext(panelNames[id], col = 'darkred', cex = 0.9, adj = 0)
  mtext('KGE', side = 2, line = 1, cex = 0.7)
  legend('bottomleft', legend = diffChar[1], col = 'black', cex = 0.9, bty = 'n')
  id = id +1
  
  # panel 2'
  polLocal = data.frame(x = c(1, seq(1,12,1),seq(12,2,-1)),
                        y = c(localSimbm_ub[1], localSimbm_lb, localSimbm_ub[seq(12,2,-1)]))
  plot(localSimbm_mi, type = 'l', ylim = yrange, col = 'gray', lwd = 2,xlab="",ylab="", xaxt = 'n')
  polygon(polLocal, col = t_col('gray', percent = 60), border = NA)
  
  polRF = data.frame(x = c(1, seq(1,12,1),seq(12,2,-1)),
                      y = c(sourceSim_ub$rf[1], sourceSim_lb$rf, sourceSim_ub$rf[seq(12,2,-1)]))
  points(1:12, sourceSim_mi$rf, type = 'l',col = 'blue', lwd = 2)
  polygon(polRF, col = t_col('blue', percent = 60), border = NA)
  axis(1, at = seq(1, 12, 1), labels = c('J','F','M','A','M','J','J','A','S','O','N','D'))
  mtext(panelNames[id], col = 'darkred', cex = 0.9, adj = 0)
  mtext('', side = 2, line = 1, cex = 0.7)
  legend('bottomleft', legend = diffChar[2], col = 'black', cex = 0.9, bty = 'n')
  id = id +1
  
  # panel 3'
  polLocal = data.frame(x = c(1, seq(1,12,1),seq(12,2,-1)),
                        y = c(localSimbm_ub[1], localSimbm_lb, localSimbm_ub[seq(12,2,-1)]))
  plot(localSimbm_mi, type = 'l', ylim = yrange, col = 'gray', lwd = 2,xlab="",ylab="", xaxt = 'n')
  polygon(polLocal, col = t_col('gray', percent = 60), border = NA)
  
  polXGB = data.frame(x = c(1, seq(1,12,1),seq(12,2,-1)),
                      y = c(sourceSim_ub$xgb[1], sourceSim_lb$xgb, sourceSim_ub$xgb[seq(12,2,-1)]))
  points(1:12, sourceSim_mi$xgb, type = 'l',col = 'green', lwd = 2)
  polygon(polXGB, col = t_col('green', percent = 60), border = NA)
  axis(1, at = seq(1, 12, 1), labels = c('J','F','M','A','M','J','J','A','S','O','N','D'))
  mtext(panelNames[id], col = 'darkred', cex = 0.9, adj = 0)
  mtext('', side = 2, line = 1, cex = 0.7)
  legend('bottomleft', legend = diffChar[3], col = 'black', cex = 0.9, bty = 'n')
  id = id +1
}

#plot.new()
#plot(0,0,col="white",xlim=c(0,1),ylim=c(0,1),xlab="",ylab="", axes  =F)
#legend('top', lwd = rep(1.2,5), col = c('gray','black','red','blue','green'), lty = rep(1,4), cex = 1,
#       legend = c('svm_local','knn_source','svm_source','rf_souce','xgb_source'), ncol = 5, bty = 'n')


dev.off()

