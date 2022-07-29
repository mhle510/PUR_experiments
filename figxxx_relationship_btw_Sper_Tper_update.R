dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
figPath = file.path(dirPath,'1_figs')
source('C:/Users/manhh/Dropbox/RStudy/0.Code/built_in/plot_support_functions.r')

load(file.path(dirPath, 'processing', 'sumSeasonalres_S2T.rData'))
yrange = c(-0.5,0.7)
xrange = c(0.55,0.9)

modNames = c('SVM', 'RF', 'XGB')
seaNames = c('DJF', 'MAM','JJA','SON')
order1 = c('(a','(b','(c')
order2 = c('-1) ','-2) ','-3) ', '-4) ')
panelNames = NULL
for(iex in 1:3){
  panelNames = c(panelNames, paste0(order1[iex], order2, modNames[iex],'-',seaNames) )
}
panelNames


pdf(file.path(figPath,'figxxx_relationship_btw_sourcePer_vs_targetPer_updated.pdf'), width = 6.5, height = 5)
pchType = c(0,1,2,3,4,6,7,8,10)
m = matrix(c(seq(1,12,1),13,13,13,13), ncol=4, byrow = T)
layout(m, heights = c(0.8,0.8,0.8,0.3))
par( mar=c(2.5,2.5,1,1),mgp=c(1.25,0.25,0),oma = c(0,0,1,1), tck=-0.05, 
    cex  = 0.7, font.lab = 2)
id = 1
for(ip in 1:4){
  dat = listsvm_pairs[[ip]]
  no = paste0('EX',(rownames(dat)))
  if(ip == 1){
    plot(dat$kge_source, dat$kge_t1, ylim = yrange, xlim = xrange,  type = 'p', pch  = pchType, cex = 1.5, col = t_col('red'),
         xlab = '', ylab = 'KGE_target')
    points(dat$kge_source, dat$kge_t2, type = 'p', pch = pchType, cex = 1.5, col = t_col('blue'))
    #text(dat$kge_source, dat$kge_t2, labels = no)
    abline(h = -0.41, lwd = 1, lty = 2)
    #legend('topleft', legend = c('Africa','Asia'), pch = c(16,16), col = c('red', 'blue'), cex = 0.8)
  } else {
    plot(dat$kge_source, dat$kge_t1, ylim = yrange, xlim = xrange,  type = 'p', pch  =  pchType, cex = 1.5, col = t_col('red'),
         xlab = '', ylab = '')
    points(dat$kge_source, dat$kge_t2, type = 'p', pch =  pchType, cex = 1.5, col = t_col('blue'))
    abline(h = -0.41, lwd = 1, lty = 2)
    #legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 0.8, bty = 'n')
  }
  mtext(panelNames[id], col = 'darkred', cex = 0.8, adj = 0)
  id = id +1
}

for(ip in 1:4){
  dat = listrf_pairs[[ip]]
  if(ip == 1){
    plot(dat$kge_source, dat$kge_t1, ylim = yrange, xlim = xrange,  type = 'p', pch  =pchType, cex = 1.5, col = t_col('red'),
         xlab = '', ylab = 'KGE_target')
    points(dat$kge_source, dat$kge_t2, type = 'p', pch = pchType, cex = 1.5, col = t_col('blue'))
    abline(h = -0.41, lwd = 1, lty = 2)
    #legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 0.8, bty = 'n')
  } else {
    plot(dat$kge_source, dat$kge_t1, ylim = yrange, xlim = xrange,  type = 'p', pch  =pchType, cex = 1.5, col = t_col('red'),
         xlab = '', ylab = '')
    points(dat$kge_source, dat$kge_t2, type = 'p', pch = pchType, cex = 1.5, col = t_col('blue'))
    abline(h = -0.41, lwd = 1, lty = 2)
    #legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 0.8, bty = 'n')
  }
  mtext(panelNames[id], col = 'darkred', cex = 0.8, adj = 0)
  id = id +1
}

#xgb
for(ip in 1:4){
  dat = listxgb_pairs[[ip]]
  if(ip == 1){
    plot(dat$kge_source, dat$kge_t1, ylim = yrange, xlim = xrange,  type = 'p', pch  =pchType, cex = 1.5, col = t_col('red'),
         xlab = 'KGE_source', ylab = 'KGE_target')
    points(dat$kge_source, dat$kge_t2, type = 'p', pch = pchType, cex = 1.5, col = t_col('blue'))
    abline(h = -0.41, lwd = 1, lty = 2)
    #legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 0.8, bty = 'n')
  } else {
    plot(dat$kge_source, dat$kge_t1, ylim = yrange, xlim = xrange,  type = 'p', pch  =pchType, cex = 1.5, col = t_col('red'),
         xlab = 'KGE_source', ylab = '')
    points(dat$kge_source, dat$kge_t2, type = 'p', pch = pchType, cex = 1.5, col = t_col('blue'))
    abline(h = -0.41, lwd = 1, lty = 2)
    #legend('bottomright', legend = round(cor(dat$n_train, dat$kge),3), cex = 0.8, bty = 'n')
  }
  mtext(panelNames[id], col = 'darkred', cex = 0.8, adj = 0)
  id = id +1
}

par(mar=c(1,5,1,5))
plot(0,0, col = 'white',   xlab = '', ylab = '', xaxt ='n', yaxt = 'n')
legend('center', pch = pchType, legend = c('EX01','EX02','EX03','EX04','EX05','EX06','EX07'), cex = 1.2, bty = 'n', ncol = 7)

dev.off()


