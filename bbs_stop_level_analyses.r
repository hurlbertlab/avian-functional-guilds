#All 50-stop data through 2010 from BBS ftp site
fifty = read.csv('//bioark.bio.unc.edu/hurlbertlab/databases/bbs/fiftystopdata/2010_fiftystopdata.csv',header=T)

#Remove poorly sampled species (waterbirds, nocturnal, raptors, etc)
fifty2 = fifty[fifty$AOU >2880 & ! fifty$AOU %in% c(3900,3901,3910) & ! (fifty$AOU >= 3240 & fifty$AOU <=3640) ,]

#Remove sites with RunType==0
weather = read.csv('//bioark.bio.unc.edu/hurlbertlab/databases/bbs/2010data/weather.csv',header=T)
fifty3 = merge(fifty2,weather[,c('RouteDataId','RunType')],by.x='RouteDataID',by.y='RouteDataId',all.x=T)
fifty4 = fifty3[fifty3$RunType==1,]
fifty5 = data.frame(stateroute = 1000*fifty4$statenum+fifty4$Route, fifty4[,6:57])
write.csv(fifty5,'//bioark.bio.unc.edu/hurlbertlab/databases/bbs/fiftystopdata/fiftystop_thru2010_goodspp_goodrtes.csv',row.names=F)

#Number of years each route was surveyed
rte.yrs = data.frame(table(unique(fifty5[,c('stateroute','year')])$stateroute))
rte.yrs$Var1 = as.character(rte.yrs$Var1)


#function for calculating max richness of any point count stop along a route
# -reads in dataframe with stateroute, year, AOU, and 50 stop columns

maxstopS = function(widedata) {
  long = melt(widedata, id=names(widedata)[1:3])
  long = long[long$value!=0,]

  stop.s = data.frame(table(long[,c('stateroute','variable')]))
  stop.s = stop.s[order(long$stateroute,long$variable),]
  max.stop.s = aggregate(stop.s$Freq,by=list(stop.s$stateroute),max)
}


yrs = 1997:2010
maxstop.output = data.frame(Group.1=NA,x=NA,year=NA)

for (y in yrs) {
  fifty.y = fifty5[fifty5$year==y,]
  foo = data.frame(maxstopS(fifty.y),year=y)
  maxstop.output = rbind(maxstop.output, foo)
}

mean.maxstopS = aggregate(maxstop.output$x,by=list(maxstop.output$Group.1),mean)

routes = read.csv('\\\\Bioark.bio.unc.edu\\hurlbertlab\\Databases\\BBS\\2010Data\\routes.csv',header=T)
routes$stateroute = 1000*routes$statenum + routes$Route
meanmax_stopS = merge(mean.maxstopS,routes[,c('stateroute','Longi','Lati')],by.x='Group.1',by.y='stateroute',all.x=T)

col.threshold = 30
cols = colorRampPalette(c("Blue","Light Blue","Yellow","Orange","Red"))(col.threshold)

meanmax_stopS$x2 = meanmax_stopS$x
meanmax_stopS$x2[meanmax_stopS$x2>col.threshold] = col.threshold

#Weed out routes with fewer than 5 yrs of surveys
meanmax_stopS = meanmax_stopS[meanmax_stopS$Group.1 %in% rte.yrs[rte.yrs$Freq>=5,'Var1'],]

#Mapping
library(sp)
library(rgdal)

prj.string<-"+proj=laea +lat_0=45 +lon_0=-100 +units=km"

NAM = readOGR('//bioark.bio.unc.edu/hurlbertallen/GIS/BirdRangeMaps/BaseMaps/NABaseMap','na_base_Lambert_Azimuthal')
NAM.laea = spTransform(NAM,CRS(prj.string))

coordinates(meanmax_stopS) = c('Longi','Lati')
proj4string(meanmax_stopS) = CRS("+proj=longlat +ellps=WGS84")
meanmax_stopS.laea = spTransform(meanmax_stopS, CRS(prj.string))

pdf('//bioark.bio.unc.edu/hurlbertallen/proposals/macrosystems_birds/BBS_max_stopS.pdf',height=6,width=8)
plot(NAM.laea,xlim=c(-2700,2500),ylim=c(-2000,2100))
points(meanmax_stopS.laea, pch=16, col = cols[meanmax_stopS$x2],cex=.5)
mtext('Maximum number of species observed at one stop',3,line=1)
dev.off()

