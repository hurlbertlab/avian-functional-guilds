#Define 4 foraging guilds of birds

guild = read.csv('\\\\Bioark.bio.unc.edu\\hurlbertallen\\Databases\\Bird Taxonomy\\NAbirds_Trophic_Table.csv',header=T,quote='\"')

#merge 'guild' to this taxonomic table in order to bring in common names
#aou = read.csv('\\\\Bioark.bio.unc.edu\\hurlbertallen\\Databases\\Bird Taxonomy\\Tax_AOU_Alpha.csv',header=T)
#aou2 = merge(aou,guild,by.x="AOU_OUT",by.y="AOU",all.x=T)

#foliage gleaners
fgs = guild[guild$Foraging %in% c('foliage glean','hover/glean') & guild$Trophic.Group %in% c('insectivore','insct/om','omnivore'),'AOU']

#ground gleaners
ggs = guild[guild$Foraging=='ground glean' & guild$Trophic.Group %in% c('insectivore','insct/om','omnivore'),'AOU']

#aerial insectivores
ais = guild[guild$Foraging %in% c('aerial foraging','hawks') & guild$Trophic.Group %in% c('insectivore','insct/om','omnivore'),'AOU']

#bark gleaners
bgs = guild[guild$Foraging=='bark glean','AOU']


#Load BBS data (sites surveyed in every year from 1996-2010) and route data
data_dir = '//bioark.bio.unc.edu/hurlbertallen/manuscripts/_completed/coreoccasional/data'
load(paste(data_dir,'/species by route abundances 1996-2010 consecutive.rdata',sep=''))
routes = read.csv(paste(data_dir,'/routes 1996-2010 consecutive.csv',sep=''),header=T)
env = read.csv(paste(data_dir,'/All Env Data 1km.csv',sep=''), header=T)

abund = as.data.frame(abundance)

fg.abund = abund[,names(abund) %in% fgs]
gg.abund = abund[,names(abund) %in% ggs]
bg.abund = abund[,names(abund) %in% bgs]
ai.abund = abund[,names(abund) %in% ais]

#Calculating richness (replaces 0's with NA)
fg.S = apply(fg.abund,1,function(x) if(sum(x)>0) {sum(x>0)} else {NA})
gg.S = apply(gg.abund,1,function(x) if(sum(x)>0) {sum(x>0)} else {NA})
bg.S = apply(bg.abund,1,function(x) if(sum(x)>0) {sum(x>0)} else {NA})
ai.S = apply(ai.abund,1,function(x) if(sum(x)>0) {sum(x>0)} else {NA})

tot.S = apply(abund,1,function(x) sum(x>0))
fg.pct = fg.S/tot.S
gg.pct = gg.S/tot.S
bg.pct = bg.S/tot.S
ai.pct = ai.S/tot.S

#Calculating abundance
fg.N = apply(fg.abund,1,function(x) if(sum(x)>0) {sum(x)} else {NA})
gg.N = apply(gg.abund,1,function(x) if(sum(x)>0) {sum(x)} else {NA})
bg.N = apply(bg.abund,1,function(x) if(sum(x)>0) {sum(x)} else {NA})
ai.N = apply(ai.abund,1,function(x) if(sum(x)>0) {sum(x)} else {NA})
tot.N = rowSums(abund)

fg.pctN = fg.N/tot.N
gg.pctN = gg.N/tot.N
bg.pctN = bg.N/tot.N
ai.pctN = ai.N/tot.N

guilddata1 = data.frame(stateroute = as.numeric(names(fg.S)),fg.S, gg.S, bg.S, ai.S, tot.S, fg.pct, gg.pct, bg.pct, ai.pct,
                      fg.N, gg.N, bg.N, ai.N, tot.N, fg.pctN, gg.pctN, bg.pctN, ai.pctN)

guilddata = merge(guilddata, env[,c('stateroute','sum.NDVI')], by='stateroute',all.x=T)

######################################################
# Figures illustrating proportional representation
########################################################

pdf('//bioark.bio.unc.edu/hurlbertallen/proposals/macrosystems_birds/guild_proportion_boxplots.pdf',height=8,width=6)
par(mfrow=c(2,1),mar=c(4,4,1,1))
boxplot(cbind(gg.pct,fg.pct,ai.pct,bg.pct),xaxt="n",las=1,tck=-.03)
axis(1,c('ground\ngleaners','foliage\ngleaners','aerial\ninsectivores','bark\ngleaners'),at=1:4,tck=-.03)
axis(4,at=seq(0,.7,by=.1),tck=.03,labels=F)
mtext('Proportion of species',3,line=.5)

boxplot(cbind(gg.pctN,fg.pctN,ai.pctN,bg.pctN),xaxt="n",las=1,tck=-.03)
axis(1,c('ground\ngleaners','foliage\ngleaners','aerial\ninsectivores','bark\ngleaners'),at=1:4,tck=-.03)
axis(4,at=seq(0,.7,by=.1),tck=.03,labels=F)
mtext('Proportion of individuals',3,line=.5)
dev.off()

pdf('//bioark.bio.unc.edu/hurlbertallen/proposals/macrosystems_birds/guild_propN_vs_propS.pdf',height=6,width=6)
plot(gg.pctN,gg.pct,pch=16,col='blue',xlim=c(0,1),ylim=c(0,1),xlab='Proportion of individuals',ylab='Proportion of species')
points(fg.pctN,fg.pct,pch=16,col='green')
points(bg.pctN,bg.pct,pch=16,col='red')
points(ai.pctN,ai.pct,pch=16,col='gold2',cex=.5)
abline(a=0,b=1,lty='dashed',lwd=2)
legend('topleft',c('ground gleaners','foliage gleaners','aerial insectivores','bark gleaners'),pch=16,
       col=c('blue','green','gold2','red'))
dev.off()

pdf('//bioark.bio.unc.edu/hurlbertallen/projects/avianfunctionalguilds/guild_propS_vs_NDVI.pdf',height=6,width=6)
plot(guilddata$sum.NDVI, guilddata$gg.pct,pch=16,col='blue',xlim=c(0,1),ylim=c(0,.7),xlab='Summer NDVI',
     ylab='Proportion of species')
points(guilddata$sum.NDVI,guilddata$fg.pct,pch=16,col='green')
points(guilddata$sum.NDVI,guilddata$bg.pct,pch=16,col='red')
points(guilddata$sum.NDVI,guilddata$ai.pct,pch=16,col='gold2')
abline(lm(guilddata$fg.pct~guilddata$sum.NDVI),col='green',lwd=2)
abline(lm(guilddata$gg.pct~guilddata$sum.NDVI),col='blue',lwd=2)
abline(lm(guilddata$bg.pct~guilddata$sum.NDVI),col='red',lwd=2)
abline(lm(guilddata$ai.pct~guilddata$sum.NDVI),col='gold2',lwd=2)

legend('topright',c('ground gleaners','foliage gleaners','aerial insectivores','bark gleaners'),pch=16,
       col=c('blue','green','gold2','red'))
dev.off()

pdf('//bioark.bio.unc.edu/hurlbertallen/projects/avianfunctionalguilds/guild_propN_vs_NDVI.pdf',height=6,width=6)
plot(guilddata$sum.NDVI, guilddata$gg.pctN,pch=16,col='blue',xlim=c(0,1),ylim=c(0,1),xlab='Summer NDVI',
     ylab='Proportion of individuals')
points(guilddata$sum.NDVI,guilddata$fg.pctN,pch=16,col='green')
points(guilddata$sum.NDVI,guilddata$bg.pctN,pch=16,col='red')
points(guilddata$sum.NDVI,guilddata$ai.pctN,pch=16,col='gold2')
abline(lm(guilddata$fg.pctN~guilddata$sum.NDVI),col='green',lwd=2)
abline(lm(guilddata$gg.pctN~guilddata$sum.NDVI),col='blue',lwd=2)
abline(lm(guilddata$bg.pctN~guilddata$sum.NDVI),col='red',lwd=2)
abline(lm(guilddata$ai.pctN~guilddata$sum.NDVI),col='gold2',lwd=2)

legend('topleft',c('ground gleaners','foliage gleaners','aerial insectivores','bark gleaners'),pch=16,
       col=c('blue','green','gold2','red'))
dev.off()


########################################################
# Figures illustrating S~N relationships
#######################################################
gg.SN = lm(log10(gg.S)~log10(gg.N))
fg.SN = lm(log10(fg.S)~log10(fg.N))
ai.SN = lm(log10(ai.S)~log10(ai.N))
bg.SN = lm(log10(bg.S)~log10(bg.N))
all.SN = lm(log10(c(gg.S,fg.S,ai.S,bg.S))~log10(c(gg.N,fg.N,ai.N,bg.N)))

pdf('//bioark.bio.unc.edu/hurlbertallen/proposals/macrosystems_birds/guild_SN_relationships.pdf',height=6,width=8)
plot(log10(gg.S)~log10(gg.N),pch=16,col='blue',xlim=c(0,3.5),ylim=c(0,1.7),
      xlab='log10 Number of individuals',ylab='log10 Number of species')
points(log10(fg.N),log10(fg.S),pch=16,col='green')
points(log10(bg.N),log10(bg.S),pch=16,col='red')
points(log10(ai.N),log10(ai.S),pch=16,col='gold2')
segments(min(log10(gg.N),na.rm=T),min(predict(gg.SN)),max(log10(gg.N),na.rm=T),max(predict(gg.SN)),col='blue',lwd=3)
segments(min(log10(fg.N),na.rm=T),min(predict(fg.SN)),max(log10(fg.N),na.rm=T),max(predict(fg.SN)),col='green',lwd=3)
segments(min(log10(bg.N),na.rm=T),min(predict(bg.SN)),max(log10(bg.N),na.rm=T),max(predict(bg.SN)),col='red',lwd=3)
segments(min(log10(ai.N),na.rm=T),min(predict(ai.SN)),max(log10(ai.N),na.rm=T),max(predict(ai.SN)),col='gold2',lwd=3)
#segments(min(log10(bg.N),na.rm=T),min(predict(all.SN)),max(log10(gg.N),na.rm=T),max(predict(all.SN)),lwd=2, lty='dashed')
legend('bottomright',c('ground gleaners','foliage gleaners','aerial insectivores','bark gleaners'),pch=16,
       col=c('blue','green','gold2','red'))
text(1.9,1.7,paste('y =',round(gg.SN$coef[2],2),'x +',round(gg.SN$coef[1],2),', R^2 =',round(summary(gg.SN)$r.squared,2)),col='blue')
text(0.9,1.3,paste('y =',round(fg.SN$coef[2],2),'x +',round(fg.SN$coef[1],2),', R^2 =',round(summary(fg.SN)$r.squared,2)),col='green')
text(2.7,.7,paste('y =',round(ai.SN$coef[2],2),'x +',round(ai.SN$coef[1],2),', R^2 =',round(summary(ai.SN)$r.squared,2)),col='gold2')
text(1.5,.2,paste('y =',round(bg.SN$coef[2],2),'x +',round(bg.SN$coef[1],2),', R^2 =',round(summary(bg.SN)$r.squared,2)),col='red')
dev.off()


##############################################################
# Maps!
##############################################################
require(maps)
cols = colorRampPalette(c("Blue","Light Blue","Dark Green","Yellow","Red"))(50)

pdf('//bioark.bio.unc.edu/hurlbertallen/proposals/macrosystems_birds/guild_richness_maps.pdf', height = 8, width = 12)
par(mfrow=c(2,2))
map('state')
mtext('Foliage gleaner richness',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*fg.S/max(fg.S),0)])
legend(-75,32,c(max(fg.S),round(max(fg.S)/2,0),1),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Ground gleaner richness',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*gg.S/max(gg.S),0)])
legend(-75,32,c(max(gg.S),round(max(gg.S)/2,0),1),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Bark gleaner richness',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*bg.S/max(bg.S),0)])
legend(-75,32,c(max(bg.S),round(max(bg.S)/2,0),1),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Aerial insectivore richness',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*ai.S/max(ai.S),0)])
legend(-75,32,c(max(ai.S),round(max(ai.S)/2,0),1),col=cols[c(50,25,1)],pch=16)
dev.off()

#Map of proportional richness
pdf('//bioark.bio.unc.edu/hurlbertallen/proposals/macrosystems_birds/guild_richness_maps_proportions.pdf', height = 8, width = 12)
par(mfrow=c(2,2))
map('state')
mtext('Proportion of foliage gleaning species',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*fg.pct/max(fg.pct),0)])
legend(-75,32,c(round(max(fg.pct),2),round(mean(range(fg.pct)),2),round(min(fg.pct),1)),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Proportion of ground gleaning species',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*gg.pct/max(gg.pct),0)])
legend(-75,32,c(round(max(gg.pct),2),round(mean(range(gg.pct)),2),round(min(gg.pct),1)),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Proportion of bark gleaning species',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*bg.pct/max(bg.pct),0)])
legend(-75,32,c(round(max(bg.pct),2),round(mean(range(bg.pct)),2),round(min(bg.pct),1)),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Proportion of aerial insectivore species',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*ai.pct/max(ai.pct),0)])
legend(-75,32,c(round(max(ai.pct),2),round(mean(range(ai.pct)),2),round(min(ai.pct),1)),col=cols[c(50,25,1)],pch=16)
dev.off()

#Abundance maps
pdf('//bioark.bio.unc.edu/hurlbertallen/proposals/macrosystems_birds/guild_abundance_maps.pdf', height = 8, width = 12)
par(mfrow=c(2,2))
map('state')
mtext('Foliage gleaner abundance',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*fg.N/max(fg.N),0)])
legend(-75,32,c(max(fg.N),round(max(fg.N)/2,0),1),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Ground gleaner richness',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*gg.N/max(gg.N),0)])
legend(-75,32,c(max(gg.N),round(max(gg.N)/2,0),1),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Bark gleaner richness',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*bg.N/max(bg.N),0)])
legend(-75,32,c(max(bg.N),round(max(bg.N)/2,0),1),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Aerial insectivore richness',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*ai.N/max(ai.N),0)])
legend(-75,32,c(max(ai.N),round(max(ai.N)/2,0),1),col=cols[c(50,25,1)],pch=16)
dev.off()

#Map of proportional abundance
pdf('//bioark.bio.unc.edu/hurlbertallen/proposals/macrosystems_birds/guild_prop_abund_maps.pdf', height = 8, width = 12)
par(mfrow=c(2,2))
map('state')
mtext('Proportion of foliage gleaning individuals',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*fg.pctN/max(fg.pctN),0)])
legend(-75,32,c(round(max(fg.pctN),2),round(mean(range(fg.pctN)),2),round(min(fg.pctN),1)),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Proportion of ground gleaning individuals',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*gg.pctN/max(gg.pctN),0)])
legend(-75,32,c(round(max(gg.pctN),2),round(mean(range(gg.pctN)),2),round(min(gg.pctN),1)),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Proportion of bark gleaning individuals',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*bg.pctN/max(bg.pctN),0)])
legend(-75,32,c(round(max(bg.pctN),2),round(mean(range(bg.pctN)),2),round(min(bg.pctN),1)),col=cols[c(50,25,1)],pch=16)

map('state')
mtext('Proportion of aerial insectivore individuals',3,line=1)
points(routes$Longi,routes$Lati,pch=16,col=cols[round(50*ai.pctN/max(ai.pctN),0)])
legend(-75,32,c(round(max(ai.pctN),2),round(mean(range(ai.pctN)),2),round(min(ai.pctN),1)),col=cols[c(50,25,1)],pch=16)
dev.off()
