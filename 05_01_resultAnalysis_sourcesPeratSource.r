dirPath = '/media/hung/DATA/0.NCKH/0.MyPaper/2022_climQ'



### benchmark models at target
load(file.path(dirPath, 'processing','knnModelPerf.rData'))
load(file.path(dirPath, 'processing','svmModelPerf.rData'))
load(file.path(dirPath, 'processing','rfModelPerf.rData'))
load(file.path(dirPath, 'processing','xgbModelPerf.rData'))


# performances at target regions using source models (different experiments)
exs = seq(1,7,1) # experiment 1 to 7

# finding maximum and minimum values for each region

miny = 10; maxy = -10
for(ie in 1:length(exs)){
  exp = exs[ie]
  sourceSim = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                         knn = knnModel7EXperf[[exp]]$CI50,
                         svm = svmModel7EXperf[[exp]]$CI50,
                         rf = rfModel7EXperf[[exp]]$CI50,
                         xgb = xgbModel7EXperf[[exp]]$CI50)
  yrange = range(sourceSim[,-1])
  if(yrange[1] < miny) {miny = yrange[1]}
  if(yrange[2] > maxy) {maxy = yrange[2]}
}
cat(miny, maxy,'\n')  


yrange = c(miny,maxy)
panelNames = c('(a) EX1',
               '(b) EX2',
               '(c) EX3',
               '(d) EX4',
               '(e) EX5',
               '(f) EX6',
               '(g) EX7')

pdf(file.path(figPath,'figxxx_performances_at_sources_models.pdf'), width = 6, height = 6)
par(mfrow = c(4,2), mar=c(2.2,2,1,1),mgp=c(1.8,0.25,0),oma = c(0,0,1,1), tck=-0.05, 
    cex  = 0.7, font.lab = 2)
for(ie in 1:length(exs)){
  exp = exs[ie]
  sourceSim = data.frame(mon = c('J','F','M','A','M','J','J','A','S','O','N','D'),
                         knn = knnModel7EXperf[[exp]]$CI50,
                         svm = svmModel7EXperf[[exp]]$CI50,
                         rf = rfModel7EXperf[[exp]]$CI50,
                         xgb = xgbModel7EXperf[[exp]]$CI50)
  
  plot(sourceSim$knn, ylim = yrange, xlab="",ylab="", xaxt = 'n', 
       type = 'l',col = 'black', lwd = 1.2)
  points(sourceSim$svm, type = 'l',col = 'red', lwd = 1.2)
  points(sourceSim$rf, type = 'l',col = 'blue', lwd = 1.2)
  points(sourceSim$xgb, type = 'l',col = 'green', lwd = 1.2)
  axis(1, at = seq(1, 12, 1), labels = c('J','F','M','A','M','J','J','A','S','O','N','D'))
  mtext(panelNames[ie], col = 'darkred', cex = 0.9, adj = 0)
  mtext('KGE', side = 2, line = 1, cex = 0.7)
}

plot.new()
legend('top', lwd = rep(1.2,5), col = c('black','red','blue','green'), lty = rep(1,4), cex = 1,
       legend = c('knn','svm','rf','xgb'), ncol = 1, bty = 'n')


dev.off()

