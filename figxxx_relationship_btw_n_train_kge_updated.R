dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
figPath = file.path(dirPath,'1_figs')
resPath = file.path(dirPath, 'processing', 'sumRes')

source('C:/Users/manhh/Dropbox/RStudy/0.Code/built_in/plot_support_functions.r')

load(file.path(dirPath, 'processing', 'sumSeasonalres.rData'))
yrange = c(0.25,0.9)

modNames = c('SVM', 'RF', 'XGB')
seaNames = c('DJF', 'MAM','JJA','SON')
order1 = c('(a','(b','(c')
order2 = c('-1) ','-2) ','-3) ', '-4) ')
panelNames = NULL
for(iex in 1:3){
  panelNames = c(panelNames, paste0(order1[iex], order2, modNames[iex],'-',seaNames) )
}
panelNames


pchType = c(0,1,2,3,4,6,7,8,10)
pdf(file.path(figPath,'figxxx_relationship_btw_models_sample_sizes_updated.pdf'), width = 6.5, height = 5)
m = matrix(c(seq(1,12,1),13,13,13,13), ncol=4, byrow = T)
layout(m, heights = c(0.8,0.8,0.8,0.3))
par(mar=c(2.5,2.5,1,1),mgp=c(1.25,0.25,0),oma = c(0,0,1,1), tck=-0.05, 
    cex  = 0.7, font.lab = 2)
id = 1
for(ip in 1:4){
  dat = listsvm[[ip]]
  
  if(ip == 1){
    plot(log10(dat$n_train), dat$kge, ylim = yrange, type = 'p', pch  = pchType, cex = 1.5, 
         xlab = '', ylab = 'KGE')
    legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 1, bty = 'n')
  } else {
    plot(log10(dat$n_train), dat$kge, ylim = yrange, type = 'p', pch  = pchType, cex = 1.5, 
         xlab = '', ylab = '')
    legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 1, bty = 'n')
  }
  mtext(panelNames[id], col = 'darkred', cex = 0.8, adj = 0)
  id = id +1
}

for(ip in 1:4){
  dat = listrf[[ip]]
  if(ip == 1){
    plot(log10(dat$n_train), dat$kge, ylim = yrange, type = 'p', pch  = pchType, cex = 1.5, 
         xlab = '', ylab = 'KGE')
    legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 1, bty = 'n')
  } else {
    plot(log10(dat$n_train), dat$kge, ylim = yrange, type = 'p', pch  = pchType, cex = 1.5, 
         xlab = '', ylab = '')
    legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 1, bty = 'n')
  }
  mtext(panelNames[id], col = 'darkred', cex = 0.8, adj = 0)
  id = id +1
}

#xgb
for(ip in 1:4){
  dat = listxgb[[ip]]
  if(ip == 1){
    plot(log10(dat$n_train), dat$kge, ylim = yrange, type = 'p', pch  = pchType,cex = 1.5, 
         xlab = 'Sample size (10^n)', ylab = 'KGE')
    legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 1, bty = 'n')
  } else {
    plot(log10(dat$n_train), dat$kge, ylim = yrange, type = 'p',pch  = pchType, cex = 1.5, 
         xlab = 'Sample size (10^n)', ylab = '')
    legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 1, bty = 'n')
  }
  mtext(panelNames[id], col = 'darkred', cex = 0.8, adj = 0)
  id = id +1
}

par(mar=c(1,5,1,5))
plot(0,0, col = 'white',   xlab = '', ylab = '', xaxt ='n', yaxt = 'n')
legend('center', pch = pchType, legend = dat$exNames, cex = 1.2, bty = 'n', ncol = 9)

dev.off()


