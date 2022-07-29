dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
figPath = file.path(dirPath,'1_figs')

source('C:/Users/manhh/Dropbox/RStudy/0.Code/built_in/plot_support_functions.r')
library(viridis)
library(vioplot)


load(file.path(dirPath,'processing','svmEX7Collection.rData'))
svmMatrix = testMatrix

load(file.path(dirPath,'processing','rfEX7Collection.rData'))
rfMatrix = testMatrix

load(file.path(dirPath,'processing','xgbEX7Collection.rData'))
xgbMatrix = testMatrix

yrange = range(c(svmMatrix, rfMatrix, xgbMatrix))
yrange
recMat = data.frame(lb = seq(0.65,0.875,0.025),
                    ub = seq(0.675,0.90,0.025),
                    cat = seq(1,10,1))
recMat
svmMatrixC = apply(svmMatrix, 2, function(x)  convert_realval_2_categories(x, recMat))  
rfMatrixC = apply(rfMatrix, 2, function(x)  convert_realval_2_categories(x, recMat))  
xgbMatrixC = apply(xgbMatrix, 2, function(x)  convert_realval_2_categories(x, recMat))  


min = min(c(svmMatrix, rfMatrix, xgbMatrix), na.rm=T)
max = max(c(svmMatrix, rfMatrix, xgbMatrix), na.rm=T)

svmMatrix.l = data.frame( val = c(svmMatrix),
                          class = rep(1:12,each = 100))
rfMatrix.l = data.frame( val = c(rfMatrix),
                          class = rep(1:12,each = 100))
xgbMatrix.l = data.frame( val = c(xgbMatrix),
                          class = rep(1:12,each = 100))


xLabel = seq(1, 100,1)
monLabel = c('Jan','Feb','Mar','Apr','May','Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')




pdf(file.path(figPath,'figxxx_KGEscores_EX7_updated_vioplot.pdf'), width = 5.5, height = 5)
m = matrix(c(1,2,3))
layout(mat = m)

par(mar = c(2.5,3,2,1), mgp=c(1.25,0.25,0),tck=-0.01, cex = 0.8)

vioplot(svmMatrix.l$val ~ svmMatrix.l$class, ylim = c(min,max), xaxt = 'n',xlab = 'Month', ylab = 'KGE')
axis(1, at = seq(1,12,1), labels = c('J','F','M','A','M','J','J','A','S','O','N','D'), cex = 0.8)
mtext('(a) Support Vector Machine', cex = 1, col = 'darkred', adj = 0)

vioplot(rfMatrix.l$val ~ svmMatrix.l$class, ylim = c(min,max), xaxt = 'n',xlab = 'Month', ylab = 'KGE')
axis(1, at = seq(1,12,1), labels = c('J','F','M','A','M','J','J','A','S','O','N','D'), cex = 0.8)
mtext('(b) Random Forest', cex = 1, col = 'darkred', adj = 0)

vioplot(xgbMatrix.l$val ~ svmMatrix.l$class, ylim = c(min,max),  xaxt = 'n',xlab = 'Month', ylab = 'KGE')
axis(1, at = seq(1,12,1), labels = c('J','F','M','A','M','J','J','A','S','O','N','D'), cex = 0.8)
mtext('(c) XGBoost', cex = 1, col = 'darkred', adj = 0)
dev.off()