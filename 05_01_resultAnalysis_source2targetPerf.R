dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
figPath = file.path(dirPath,'1_figs')
### benchmark models at target
load(file.path(dirPath, 'processing','knnModelPerf_bm.rData'))
load(file.path(dirPath, 'processing','svmModelPerf_bm.rData'))
load(file.path(dirPath, 'processing','rfModelPerf_bm.rData'))
load(file.path(dirPath, 'processing','xgbModelPerf_bm.rData'))

# sources to targets 
load(file.path(dirPath, 'processing','sum_knn_sources_at_targets.rData'))
load(file.path(dirPath, 'processing','sum_svm_sources_at_targets.rData'))
load(file.path(dirPath, 'processing','sum_rf_sources_at_targets.rData'))
load(file.path(dirPath, 'processing','sum_xgb_sources_at_targets.rData'))


# performances at target regions using local models
regs = c(1,2)
panelNames = c('(a) Africa','(b) Asia')
pdf(file.path(figPath,'figxxx_performances_at_target_regions_local_models.pdf'), width = 6.5, height = 3)
par(mfrow = c(1,2), mar=c(2.2,2,1,1),oma= c(1,1,1,1),mgp=c(1.2,0.25,0),tck=-0.01, 
    cex  = 0.7, font.lab = 2)
for(ip in 1:length(regs)){
  ireg = regs[ip]
  localSim = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                        knn = knnModel7EXperf[[ireg]]$CI50,
                        svm = svmModel7EXperf[[ireg]]$CI50,
                        rf = rfModel7EXperf[[ireg]]$CI50,
                        xgb = xgbModel7EXperf[[ireg]]$CI50)
  yrange = range(localSim[,-1])
  yrange[2] = yrange[2] + 0.1*yrange[2]
  plot(localSim$knn, type = 'l', ylim = yrange, col = 'black', lwd = 1.2,xlab="",ylab="", xaxt = 'n')
  points(localSim$svm, type = 'l',col = 'red', lwd = 1.2)
  points(localSim$rf, type = 'l',col = 'blue', lwd = 1.2)
  points(localSim$xgb, type = 'l',col = 'green', lwd = 1.2)
  axis(1, at = seq(1, 12, 1), labels = localSim$mon)
  mtext(panelNames[ip], col = 'darkred', cex = 0.9, adj = 0)
  
  if(ip == 1) {
    legend('top', lwd = rep(1.2,4), col = c('black','red','blue','green'), lty = rep(1,4),
           legend = c('knn','svm','rf','xgb'), ncol = 4, bty = 'n')
  }
}
dev.off()



# performances at target regions using source models (different experiments)
regs = c(1,2) # 1 is Africa, 2 is Asia
exs = seq(1,7,1) # experiment 1 to 7

# finding maximum and minimum values for each region

for(ir in 1:length(regs)){
  miny = 10; maxy = -10
  for(ie in 1:length(exs)){
    exp = exs[ie]
    reg = regs[ir]
    localSimbm = svmModel7EXperf[[reg]]$CI50
    sourceSim = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                           knn = knnsourcesAll[[reg]][[exp]]$CI50,
                           svm = svmsourcesAll[[reg]][[exp]]$CI50,
                           rf = rfsourcesAll[[reg]][[exp]]$CI50,
                           xgb = xgbsourcesAll[[reg]][[exp]]$CI50)
    yrange = range(c(sourceSim[,-1], localSimbm))
    if(yrange[1] < miny) {miny = yrange[1]}
    if(yrange[2] > maxy) {maxy = yrange[2]}
  }
  cat('---region---',ir,'\n')
  cat(miny, maxy,'\n')
    
}

yranges = matrix(c(-1.08, 0.65,
                  -0.25, 0.68), ncol = 2, byrow = T)
panelNames = c('(a-1) Africa - EX1','(a-2) Asian - EX1',
               '(b-1) Africa - EX2','(b-2) Asian - EX2',
               '(c-1) Africa - EX3','(c-2) Asian - EX3',
               '(d-1) Africa - EX4','(d-2) Asian - EX4',
               '(e-1) Africa - EX5','(e-2) Asian - EX5',
               '(f-1) Africa - EX6','(f-2) Asian - EX6',
               '(g-1) Africa - EX7','(g-2) Asian - EX7')

pdf(file.path(figPath,'figxxx_performances_at_target_regions_sources_models.pdf'), width = 6.5, height = 8)
par(mfrow = c(7,2), mar=c(2.2,2,1,1),oma= c(1,1,1,1),mgp=c(1.2,0.25,0),tck=-0.01, 
    cex  = 0.7, font.lab = 2)
id = 1
for(ie in 1:length(exs)){
  exp = exs[ie]
  for(ir in 1:length(regs)){
    reg = regs[ir]
    localSimbm = svmModel7EXperf[[reg]]$CI50
    sourceSim = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                           knn = knnsourcesAll[[reg]][[exp]]$CI50,
                           svm = svmsourcesAll[[reg]][[exp]]$CI50,
                           rf = rfsourcesAll[[reg]][[exp]]$CI50,
                           xgb = xgbsourcesAll[[reg]][[exp]]$CI50)
    yrange = yranges[ir,]
    yrange[2] = yrange[2]
    plot(localSimbm, type = 'l', ylim = yrange, col = 'gray', lwd = 3,xlab="",ylab="", xaxt = 'n')
    points(sourceSim$knn, type = 'l',col = 'black', lwd = 1.2)
    points(sourceSim$svm, type = 'l',col = 'red', lwd = 1.2)
    points(sourceSim$rf, type = 'l',col = 'blue', lwd = 1.2)
    points(sourceSim$xgb, type = 'l',col = 'green', lwd = 1.2)
    axis(1, at = seq(1, 12, 1), labels = localSim$mon)
    mtext(panelNames[id], col = 'darkred', cex = 0.9, adj = 0)
    if(id == 1) {
      legend('top', lwd = rep(1.2,5), col = c('gray','black','red','blue','green'), lty = rep(1,4),
             legend = c('local_model','knn_source','svm_source','rf_souce','xgb_source'), ncol = 3, bty = 'n')
    }
    id = id+1
  }
}
dev.off()

