####################################################################################
# Calc Streamflow from QSIM
# Manh-Hung Le - 2021 Apr 27
# objectives:
# - calculate lmonthly climatic
# - filter based on climatic and land cover 

# output:
# part 1- climatological timeseries for each month
# Part 2- obsDat.rData

####################################################################################
library(lubridate)
library(tidyverse)
library(rgdal)
library(psych)


############ PART1- Calculate streamflow in each month ###########
# input data
dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
pPath = file.path(dirPath, '0_input','gldas','csvthirdScreen', 'precip')
tPath = file.path(dirPath, '0_input','gldas','csvthirdScreen', 'airT')

processedPath = file.path(dirPath,'processing','fullTS')

# load gsim metadata
load(file.path(dirPath,'processing','qsim_metaData_qts_3rdScreening.rdata'))

# own-built function
source(file.path(dirPath,'rCode','gsim_support_functions.r'))

# reading noah data
pList = list.files(pPath, pattern = ".csv", 
                        full.names = T, recursive = T)
tList = list.files(tPath, pattern = ".csv", 
                    full.names = T, recursive = T)
pName = str_remove(basename(pList),'.csv')
pDat = lapply(pList, function(x) read.csv(x, header = TRUE))
tDat = lapply(tList, function(x) read.csv(x, header = TRUE))


# check name corrct
head(pName)
names(qMonValF3)[1]

tail(pName)
names(qMonValF3)[2722]

n = length(qMonValF3)
# annual calculation
for(im in 1:12){
        Df = data.frame(Name = character(n),
                        Q = numeric(n),
                        P = numeric(n),
                        T = numeric(n))
        
        for(ii in 1: n){
                #print(ii)
                # Q process
                qts = qMonValF3[[ii]]
                pts = pDat[[ii]]
                tts = tDat[[ii]]

                gsimdateN = paste(formatC(qts$year, width = 4, flag= 0),
                                  formatC(qts$month, width = 2, flag= 0), sep = '')
                #gsimdate = make_date(year = qts$year, month = qts$month, 1)
                noahdateN = paste(formatC(substr(pts$date,1,4), width = 4, flag = 0),
                                 formatC(substr(pts$date,5,6), width = 2, flag = 0), sep = '')
                # update noah data to match with streamflow data
                noahloc = which(noahdateN %in% gsimdateN)
                pts = pts[noahloc,]
                tts = tts[noahloc,]
                
                qloc = which(gsimdateN %in% noahdateN)
                qts = qts[qloc,]
                
                imloc = which(qts$month == im)
                        
                qts = qts[imloc,]
                pts = pts[imloc,]
                tts = tts[imloc,]
                
                
                # monthly data
                Df$Name[ii] = metaDataF3$gsim.no[ii]
                Df$Q[ii] = round(mean(qts$MEAN, na.rm = T),2)
                Df$P[ii] = round(mean(pts[,2], na.rm = T),2)
                Df$T[ii] = round(mean(tts[,2], na.rm = T),2)
        }
        
        summary(Df)
        # other features
        
        # area.meta:  :catchment size information from metadata in km2 (if available)
        # altitude.meta :height of gauge above sea level (m) reported from metadata
        # climate.type : catchment climate (major groups of Koppen-Geiger system) if one Climate type present over more than 50% catchment area, otherwise 'No dominant
        #                                class' value was assigned
        # dr.mean   :average catchment drainage density 
        # ele.mean  :average values of catchment elevation
        # ir.mean :average of catchment irrigation area
        # landcover.type :catchment land-cover (UN Classification System for 2015) if one single land-cover type present over more than 50% catchment area, otherwise 
        #                 'No dominant class' value was assigned
        # lithology.type  :catchment lithology (16 classes by Dürr et al. [2005] (*)) if one single lithology type present over more than 50% catchment area, otherwise 'No 
        #                                dominant class' value was assigned
        # slp.mean :average of slope within catchment boundary
        # pd.mean : average values of population density within catchment boundary (GPWv4 - 2010)
        # sb.mean: average  values of bulk density in kg/cubic-meter within catchment boundary
        # scl.mean: average values of clay content in soil profile within catchment boundary
        # snd.mean: average values of sand content in soil profile within catchment boundary
        # slt.mean: average values of silt content in soil profile within catchment boundary
        # soil.type: catchment soil class (WRB) if one single soil class present over more than
        #            50% catchment area, otherwise 'No dominant class' value was assigned
        # tp.mean: average  values of topographic index within catchment boundary
        atrNamesNumeric = c('long.new','lat.new','area.meta','altitude.dem', 'dr.mean','ele.mean','ir.mean',
                            'slp.mean','pd.mean','sb.mean','scl.mean','snd.mean','slt.mean',  'tp.mean')
        atrNamesCategory = c('climate.type','landcover.type')
        
        # remove NA values for attributes
        DfAtr = data.frame(Df, metaDataF3[,atrNamesNumeric], metaDataF3[,atrNamesCategory])
        
        
        cat('------------', im,'--------','\n')
        cat(summary(DfAtr$Q), '\n')
        
        DfFinal = data.frame(gsimNo = DfAtr$Name,
                             DfAtr[,-c(1,2)],
                             Q = DfAtr$Q)
        
        rownames(DfFinal) = NULL
   
        # plot cross-correlation
        #jpeg(filename = paste(dirPath,'/','figs', '/','crosscorrelation_',
        #                          paste0(formatC(im, width = 2,flag = 0)), 'df.jpg', sep = ''), 
        #     res = 300, width = 500, height = 500, units=  'mm')
       # pairs.panels(DfFinal[,-1])
        #dev.off()
        
        # export final filter data
        fileOp = file.path(processedPath
                           ,paste(paste0(formatC(im, width = 2,flag = 0)), 'Original.rdata', sep = ''))
        save(DfFinal, metaDataF3, file = fileOp)
}


############ PART2- Collect streamflow in a data frame ###########
# input data
dirPath = 'D:/0.NCKH/0.MyPaper/2022_climQ'
obsDat = mat.or.vec(2722, 12)
for(im in 1 : 12){
  loadFile = paste(dirPath,'/', 'processing','/','fullTS', '/',formatC(im, width = 2, flag = 0), 'Original.rdata', sep = '')
  load(loadFile)
  obsDat[,im] = DfFinal$Q
}

colnames(obsDat) = paste('q',formatC(1:12, width = 2, flag = 0), sep = '')
obsDat = data.frame(no = seq(1,2722,1),
                    gsim.no = metaDataF3$gsim.no,
                    obsDat)
save(obsDat, file = file.path(dirPath,'processing','obsDat.rData'))





