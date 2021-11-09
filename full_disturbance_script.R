### Created 7/29/19
### Updated 11/09/21
library(tidyverse)
library(ggpubr)
library(vegan)
library(codyn)

##### SETUP AND READ-IN DATA #####
theme_set(theme_classic())

plotkey<-read_csv("plotkey.csv", skip=8)
spkey<-read_csv("spkey.csv", skip=6)
mixkey<-read_csv("mixkey.csv", skip=5)
vegplot<-read_csv("vegplot.csv", skip=14)%>%
  mutate(sub=tolower(sub))%>%
  mutate(year=2019)
vegplot2020<-read_csv("vegplot2020.csv", skip=13)%>%
  mutate(sub=tolower(sub))%>%
  mutate(year=2020)

vegplot_j<-vegplot%>%
  dplyr::select(1:5, 9:13, 41)%>%
  mutate(perhyp=NA)
vegplot2020_j<-vegplot2020%>%
  dplyr::select(1:11, 34)%>%
  mutate(talll=lheight)%>%
  dplyr::select(-lheight)
timeseries<-rbind(vegplot_j, vegplot2020_j) # data for timeseries

##### DATA CLEANUP #####
##FN for Calculating SE
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

#vegtog: (2019 only) join of data sheets with plotkey
vegtog<-left_join(vegplot, plotkey)%>%
  dplyr::select(-(14:17), -(46:50))%>%
  mutate(perpg=as.numeric(perpg))%>%
  mutate(burntrt=ifelse(burntrt=="b", "Burned", "Not Burned"))%>%
  mutate(bopsclim=ifelse(bopsclim=="w", "Warmed", "Not Warmed"))%>%
  mutate(site=ifelse(site=="DC", "Southern", 
                     ifelse(site=="WC", "Central", "Northern")))%>%
  mutate(plot=as.factor(plot))%>%
  mutate(sub=as.factor(sub))%>%
  mutate(micro=as.factor(micro))%>%
  mutate(seedmix=as.factor(ifelse(seedmix=="f", "Forbs", 
                                  ifelse(seedmix=="e", "Restoration mix", 
                                         ifelse(seedmix=="fr", "Festuca roemeri", 
                                                ifelse(seedmix=="dc", "Danthonia californica (OR)", 
                                                       ifelse(seedmix=="km", "Koeleria macrantha",
                                                              ifelse(seedmix=="p","Pasture grasses", 
                                                                     ifelse(seedmix=="c", "California grasses", 
                                                                            ifelse(seedmix=="x", "control", "?"))))))))))%>%
  filter(perf!="x")

vegtog$site<-factor(vegtog$site, levels=c("Northern", 
                                          "Central", 
                                          "Southern"))
vegtog$burntrt<-factor(vegtog$burntrt, levels=c("Not Burned", "Burned"))
vegtog$bopsclim<-factor(vegtog$bopsclim, levels=c("Not Warmed", "Warmed"))
vegtog$seedmix<-factor(vegtog$seedmix, levels=c("Festuca roemeri", "Danthonia californica (OR)", "Koeleria macrantha", 
                                                "Pasture grasses", "California grasses", "Forbs", "Restoration mix", 
                                                "control", "?"))


### Vegtog 1: Make data set long, gathering percent covers
vegtog1<-vegtog%>%
  gather(func, cover, 9:13)%>%
  mutate(func=as.factor(ifelse(func=="perag", "Annual grasses", ifelse(func=="perpg", "Perennial grasses", ifelse(func=="perl", "Litter", ifelse(func=="perf", "Forbs", "Bare ground"))))))%>%
  filter(!is.na(cover), cover!="x")%>%
  mutate(cover=as.numeric(cover))%>%
  filter(cover<100)
vegtog1$func<-factor(vegtog1$func, levels=c("Perennial grasses", "Annual grasses", "Forbs", "Litter", "Bare ground"))


### SEEDLING DYNAMICS #####

vegtog_counts<-vegtog%>%
  dplyr::select(1:5, 9:12, 14:41)%>%
  gather(species, count, 11:32)%>%
  mutate(count=ifelse(species=="koeleria"&
                        is.na(count)&seedmix=="Koeleria macrantha", 0, count))%>%
  mutate(count=ifelse(species=="dancalor"&
                        is.na(count)&seedmix=="Danthonia californica (OR)", 0, count))%>%
  mutate(count=ifelse(species=="fesroe"&
                        is.na(count)&seedmix=="Festuca roemeri", 0, count))%>%
  mutate(count=ifelse(species=="pasture"&
                        is.na(count)&seedmix=="Pasture grasses", 0, count))%>%
  mutate(count=ifelse((species=="dancalcal"|species=="fesid"|species=="stipa")&
                        (is.na(count)&seedmix=="California grasses"), 0, count))%>%
  mutate(count=ifelse((species=="achillea"|species=="agoseris"|species=="cammasia"|
                         species=="clarkia"|species=="collinsia"|species=="drymocallis"|
                         species=="lomatiumn"|species=="lomatiumu"|species=="lotus"|
                         species=="lupinus"|species=="microseris"|species=="plectritis"|
                         species=="prunella"|species=="tritilea"|species=="zigadenus")&
                        (is.na(count)&seedmix=="Forbs"), 0, count))%>%
  mutate(count=ifelse((species=="dancalor"|species=="fesroe"|species=="achillea"|
                         species=="cammasia"|species=="lotus"|species=="lomatiumn"|
                         species=="prunella"|species=="microseris")&
                        (is.na(count)&seedmix=="Restoration mix"), 0, count))%>%
  filter(!is.na(count))%>%
  mutate(count=as.numeric(count))%>%
  mutate(count=count*frac)

vegtog_survival<-left_join(vegtog_counts, spkey, by=c("species"="code"))%>%
  filter(seedmix!="?", seedmix!="Restoration mix")%>%
  mutate(grams=ifelse(seedmix=="Forbs", .053, ifelse(seedmix=="California grasses", .266, .8)))%>%
  mutate(seeded=grams/seed_g100*100)%>%
  mutate(survival=count/seeded)%>%
  filter(species!="agoseris", species!="lupinus", species!="microseris", species!="tritilea", species!="zigadenus") %>%
  group_by(species)%>%
  mutate(zsurvival=scale(survival))%>%
  mutate(func=ifelse(species=="koeleria"|species=="dancalor"|species=="fesroe", "Native Perennial Grasses", ifelse(species=="pasture", "Pasture Grasses", species)))
#vegtog_survival_spread<-vegtog_survival%>%
#  slice(-924, -1045, -1166, -1286, -1408, -1529, -1650, -1771, -1892, -2013)%>%
#  dplyr::select(plot, site, seedmix, sciname, native, longevity, type, burntrt, survival)%>%
#  mutate(burntrt=ifelse(burntrt=="Not Burned", "notburned", "burned"))%>%
#  spread(burntrt, survival)




#### SURVIVAL BY SITE 2019   ####
f5a<-ggplot(subset(vegtog_survival, func=="Native Perennial Grasses"|func=="Pasture Grasses"), aes(x=burntrt, y=survival, fill=burntrt))+
  geom_jitter(size=.5, color="grey")+
  geom_boxplot() + facet_grid(func~site, scales="free") +
  scale_fill_manual(labels=c("Unburned", "Burned", "Unburned, Warmed", "Burned, Warmed"), 
                    values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+ geom_hline(yintercept=0)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("Proportion of Seedling Survival")+ggtitle("Native Perennial Grasses")


## SETUP timeseries central site survival ####
survival2020<-vegplot2020%>%
  dplyr::select(1:4, 12:34)%>%
  gather(species, count, 5:26)%>%
  separate(count, into=c("count2", "percent", "tillers"))%>% 
  mutate(count=as.numeric(count2))%>%
  mutate(count=ifelse(species=="koeleria"&
                        is.na(count)&seedmix=="km", 0, count))%>%
  mutate(count=ifelse(species=="dancalor"&
                        is.na(count)&seedmix=="dc", 0, count))%>%
  mutate(count=ifelse(species=="fesroe"&
                        is.na(count)&seedmix=="fr", 0, count))%>%
  mutate(count=ifelse(species=="pasture"&
                        is.na(count)&seedmix=="p", 0, count))%>%
  mutate(count=ifelse((species=="dancalcal"|species=="fesid"|species=="stipa")&
                        (is.na(count)&seedmix=="c"), 0, count))%>%
  mutate(count=ifelse((species=="achillea"|species=="agoseris"|species=="cammasia"|
                         species=="clarkia"|species=="collinsia"|species=="drymocallis"|
                         species=="lomatiumn"|species=="lomatiumu"|species=="lotus"|
                         species=="lupinus"|species=="microseris"|species=="plectritis"|
                         species=="prunella"|species=="tritilea"|species=="zigadenus")&
                        (is.na(count)&seedmix=="f"), 0, count))%>%
  mutate(count=ifelse((species=="dancalor"|species=="fesroe"|species=="achillea"|
                         species=="cammasia"|species=="lotus"|species=="lomatiumn"|
                         species=="prunella"|species=="microseris")&
                        (is.na(count)&seedmix=="e"), 0, count))%>%
  filter(!is.na(count))%>%
  mutate(count2=as.numeric(count))%>%
  mutate(tillers=as.numeric(ifelse(tillers=="14T", 14, 
                                   ifelse(tillers=="2T", 2, 
                                          ifelse(tillers=="1T", 1,
                                                 ifelse(tillers=="65T",65, 
                                                        ifelse(tillers=="7T",7,
                                                               ifelse(tillers=="3T",3,
                                                                      ifelse(tillers=="12T", 12, 
                                                                             ifelse(tillers=="4T", 4, tillers))))))))))%>%
  mutate(tillers=ifelse(is.na(tillers), 0, tillers))%>%
  dplyr::select(1,2,3,6,7,9)%>%
  mutate(plot=as.factor(plot), micro=as.factor(micro))

survivalTS<-subset(vegtog_survival, site=="Central")%>%
  dplyr::select(plot, sub, micro, seedmix, burntrt, species, count, survival)

#### STATS SURVIVAL ####
library(nlme)
library(multcomp)

#By Fn. Group:
#Northern/Central/Southern Bunchgrass
VStrt<-vegtog_survival%>%
  mutate(trt=as.factor(paste(site, burntrt, sep="_")))%>%
  ungroup()%>%
  mutate(trt=as.factor(trt))

BunchgrassMM<-lme(survival~trt, random = ~1|micro/sub, data = subset(VStrt, func=="Native Perennial Grasses"))
summary(glht(BunchgrassMM, linfct=mcp(trt="Tukey")))

#NCS Pasture Grasses
PastureMM<-lme(survival~trt, random = ~1|micro/sub, data = subset(VStrt, func=="Pasture Grasses"))
summary(glht(PastureMM, linfct=mcp(trt="Tukey")))

#Aggregate
survivalTS1<-left_join(survivalTS, survival2020)%>%
  mutate(survival2=count2/(count/survival), survival2=ifelse(is.na(survival2), 0, survival2))%>%
  gather(year, total, count, count2)%>%
  mutate(year=as.numeric(ifelse(year=="count", 2019, 2020)))%>%
  mutate(survival3=ifelse(year==2019, survival, survival2))%>%
  filter(!is.na(total), !is.na(survival3))%>%
  mutate(survival3=ifelse(is.infinite(survival3), 0, survival3))

survivalTS_agg<-survivalTS1%>%
  group_by(year, species, burntrt)%>%
  summarize(meancount=mean(total), secount=calcSE(total), meansurvival=mean(survival3), sesurvival=calcSE(survival3))

survivalTS1b<-survivalTS1%>%
  mutate(trt=as.factor(paste(burntrt, species)))

#Establishment
establishment<-lme(survival3~burntrt, random = ~1|micro/sub, data = subset(survivalTS1, year==2019&species=="prunella"))
summary(glht(establishment, linfct=mcp(burntrt="Tukey")))

survivalTS2<-survivalTS1%>%
  mutate(trt=as.factor(paste(year, burntrt, sep="_")))

#By Species
#Fescue survival MM TS
survival_fesroe<-lme(survival3~trt, random = ~1|micro/sub, data = subset(survivalTS2, species=="fesroe"))
summary(glht(survival_fesroe, linfct=mcp(trt="Tukey")))
#Danthonia survival MM TS
survival_danthonia<-lme(survival3~trt, random = ~1|micro/sub, data = subset(survivalTS2, species=="dancalor"))
summary(glht(survival_danthonia, linfct=mcp(trt="Tukey")))
#Koeleria survival MM TS
survival_koeleria<-lme(survival3~trt, random = ~1|micro/sub, data = subset(survivalTS2, species=="koeleria"))
summary(glht(survival_koeleria, linfct=mcp(trt="Tukey")))
#Pasture survival MM TS
survival_pasture<-lme(survival3~trt, random = ~1|micro/sub, data = subset(survivalTS2, species=="pasture"))
summary(glht(survival_pasture, linfct=mcp(trt="Tukey")))


#data for yearly survival
survivalTS_agg1<-subset(survivalTS_agg,
                        species=="dancalor"|
                          species=='fesroe'|species=="koeleria"|
                          species=="pasture")%>%
  ungroup()%>% 
  mutate(species=as.factor(species))
levels(survivalTS_agg1$species) <- c("Danthonia californica", "Festuca roemeri", "Koeleria macrantha", "pasture grasses")

#####  VISUALIZE SURVIVAL BY YEAR/GRASS SPECIES #####
ggplot(subset(survivalTS_agg1), aes(x=as.factor(year), meansurvival)) + 
  geom_line(aes(group=burntrt, color=burntrt), size=1)+
  geom_point(aes(group=burntrt, color=burntrt))+
  geom_errorbar(aes(ymin=meansurvival-sesurvival, ymax=meansurvival+sesurvival,group=burntrt, color=burntrt), width=.2)+
  xlab("")+ylab("Central site net survival")+scale_color_manual(values=c("dodgerblue", "brown3"))+
  facet_wrap(~species)+theme(text=element_text(size=15), legend.title= element_blank())


#### VISUALIZE SURVIVAL FORBS ####
survivalTS_agg$species<-factor(survivalTS_agg$species, levels=c("clarkia", "collinsia",  "plectritis", "lotus", "achillea", "cammasia", "drymocallis", "lomatiumn", "lomatiumu", "prunella","fesid", "fesroe", "koeleria", "pasture", "stipa", "dancalcal", "dancalor"))

levels(survivalTS_agg$species) <- c("clarkia", "collinsia",  "plectritis", "lotus","achillea", "cammasia", "drymocallis", "lomatiumn", "lomatiumu",  "prunella","fesid", "fesroe", "koeleria", "pasture", "stipa", "dancalcal", "dancalor")
#ggarrange(cnt, srv, common.legend = T, legend="right")
ggplot(subset(survivalTS_agg, !species%in%c("fesid", "fesroe", "koeleria", "pasture", "stipa", "dancalcal", "dancalor")&year==2019), aes(x=as.factor(species), meansurvival)) + 
  #geom_line(aes(group=burntrt, color=burntrt), size=1)+
  geom_point(aes(group=burntrt, color=burntrt))+
  geom_errorbar(aes(ymin=meansurvival-sesurvival, ymax=meansurvival+sesurvival,group=burntrt, color=burntrt), width=.2)+
  xlab("")+ylab("Percent survival")+scale_color_manual(values=c("dodgerblue", "brown3"))+
  theme(text=element_text(size=15), legend.title= element_blank())+theme_classic()


#Calculate shannon diversity of forbs
library(codyn)
eug.shannon<-left_join(vegtog_counts, spkey, by=c("species"="code"))%>%
  filter(seedmix=="Forbs")%>%
  group_by(plot, sub, micro, site, burntrt)%>%
  mutate(present=count/count)%>%
  mutate(rep=paste(plot, sub, micro, site))
eug.shannon[is.na(eug.shannon)] <- 0

eug.shannon<-left_join(eug.shannon, community_diversity(eug.shannon, abundance.var="count", replicate.var="rep", metric = c("Shannon")))


eug.shannon<-eug.shannon%>%
  group_by(plot, sub, micro, site, burntrt, Shannon)%>%
  summarize(richness=sum(present), abundance=sum(count))%>%
  gather(metric, value, richness, abundance, Shannon)

######### VISUALIZE FORB DIVERSITY ####
f5b<-ggplot(subset(eug.shannon, metric!="richness"), aes(x=burntrt, y=value))+
  geom_jitter(size=.5, color="grey")+
  geom_boxplot(aes(fill=burntrt))+  facet_grid(metric~site, scales="free")+
  scale_fill_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+theme(text=element_text(size=15))+ylab("space")


#### FULL NPG AND FORBS BURNING EFFECT FIGURE   #####

figure5<-vegtog_survival%>%
  ungroup()%>%
  filter(func=="Native Perennial Grasses"|func=="Pasture Grasses" )%>%
  dplyr::select(plot, sub, micro, site, burntrt, func, species, count, seeded)%>%
  group_by(plot, sub, site, burntrt, func)%>%
  summarize(count=sum(count), seeded=sum(seeded))%>%
  mutate(survival=count/seeded)%>%
  mutate(metric=func, value=survival)%>%
  dplyr::select(-func, -survival)

figure5<-rbind(figure5, ungroup(eug.shannon))%>%
  filter(metric!="richness")%>%
  mutate(value2=ifelse(metric=="abundance", value/952, value))
figure5$metric<-factor(figure5$metric, levels=c("Native Perennial Grasses", "Pasture Grasses", "abundance", "Shannon"))

figure5_stat<-figure5%>%
  mutate(trt=as.factor(paste(site, burntrt, sep="_")))%>%
  ungroup()%>%
  mutate(trt=as.factor(trt))

IPGMM<-lme(value~trt, random = ~1|sub, data = subset(figure5_stat, metric=="Pasture Grasses"))
summary(glht(IPGMM, linfct=mcp(trt="Tukey")))


ggplot(subset(figure5, metric!="richness"), aes(x=burntrt, y=value2))+
  
  geom_boxplot(aes(group=burntrt))+  facet_grid(metric~site, scales="free")+
  geom_jitter(size=.5, width=.2, color="grey")+
  scale_fill_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+theme(text=element_text(size=15))+ylab("")+ geom_hline(yintercept=0)+theme_classic()

#test for normality
ggplot(subset(figure5, metric!="richness"), aes(x=value2))+
  facet_grid(metric~interaction(site, burntrt), scales="free")+
  geom_histogram()+
  theme(text=element_text(size=15))+ylab("")+
  geom_hline(yintercept=0)+theme_classic()


###STAT FORBS MIX ####

eug_shan2<-eug.shannon%>%
  mutate(trt=as.factor(paste(site, burntrt, sep="_")))%>%
  ungroup()%>%
  mutate(trt=as.factor(trt))

abundanceMM<-lme(value~trt, random = ~1|sub/micro, data = subset(eug_shan2, metric=="abundance"))
summary(glht(abundanceMM, linfct=mcp(trt="Tukey")))

ShannonMM<-lme(value~trt, random = ~1|sub/micro, data = subset(eug_shan2, metric=="Shannon"))
summary(glht(ShannonMM, linfct=mcp(trt="Tukey")))

### SET UP COMMUNITY DISSIMILARITY MATRICES ####
vegtog2p<-vegtog1%>%
  group_by(plot, func, burntrt, site)%>%
  filter(!is.na(cover))%>%
  summarize(plotcover=mean(cover))%>%
  mutate(plotcover=plotcover+1)%>%
  filter(!is.na(plotcover))%>%
  ungroup()%>%
  mutate(burntrt=ifelse(burntrt=="Not Burned", "NotBurned", "Burned"))%>%
  spread(burntrt, plotcover)%>%
  mutate(burnLRR=log(Burned/NotBurned))%>%
  mutate(burndiff=Burned-NotBurned)
wide_composition<-vegtog2p%>%
  gather(burntrt, cover, Burned, NotBurned)%>%
  dplyr::select(-4, -5)%>%
  spread(func, cover, fill=0)%>%
  mutate(id=paste(plot, burntrt, sep="_"))%>%
  dplyr::select(9, 4, 5, 6)%>%
  dplyr::select(1:4)%>%
  filter(id!="51_NotBurned")
colnames(wide_composition)<-c("id", "pg", "ag", "f")
wide_composition_rel<-wide_composition%>%
  group_by(id)%>%
  mutate(totcov=sum(pg, ag, f))%>%
  mutate(pg=pg*100/totcov, ag=ag*100/totcov, f=f*100/totcov)%>%
  mutate(newtot=ag+pg+f)%>%
  dplyr::select(-totcov, -newtot)%>%
  ungroup()

wcrel2<-wide_composition%>%
  mutate(totcov=sum(pg, ag, f))%>%
  mutate(pg=pg*100/totcov, ag=ag*100/totcov, f=f*100/totcov)%>%
  separate(id, into=c("plot", "burntrt"), sep="_")%>%
  mutate(plot=as.numeric(plot))%>%
  mutate(site=ifelse(plot<21, "southern", ifelse(plot>40, "northern", "central")))
wcrel2.5<-unique(dplyr::select
                 (left_join(
                   wcrel2, dplyr::select(mutate(plotkey, 
                                                site=ifelse(site=="DC", "southern", 
                                                            ifelse(site=="WC", "central", "northern"))), 
                                         1:4)), -sub))
wcrel2.5.5<-unique(dplyr::select(data.scores, burntrt, plot, groups))
newplotkey<-left_join(wcrel2.5, wcrel2.5.5)%>%
  mutate(warmtrt=ifelse(fullclim=="warm"|fullclim=="warmppt", "warm", "ambient"))




wcrS<-wide_composition_rel[1:40,]
wcrC<-wide_composition_rel[41:80,]
wcrNa<-wide_composition_rel[81:100,]
wcrNb<-wide_composition_rel[102:119,]
wcrN<-rbind(wcrNa, wcrNb)

wcrUB<-vegtog2p%>%
  gather(burntrt, cover, Burned, NotBurned)%>%
  dplyr::select(-4, -5)%>%
  spread(func, cover, fill=0)%>%
  filter(burntrt=="NotBurned")%>%
  mutate(id=paste(plot, burntrt, sep="_"))%>%
  dplyr::select(9, 4, 5, 6)%>%
  dplyr::select(1:4)%>%
  filter(id!="51_NotBurned")
colnames(wcrUB)<-c("id", "pg", "ag", "f")
wcrUB1<-wcrUB%>%
  group_by(id)%>%
  mutate(totcov=sum(pg, ag, f))%>%
  mutate(pg=pg*100/totcov, ag=ag*100/totcov, f=f*100/totcov)%>%
  mutate(newtot=ag+pg+f)%>%
  dplyr::select(-totcov, -newtot)%>%
  ungroup()

wcrNUB<-vegtog2p%>%
  filter(site=="Northern")%>%  
  dplyr::select(-NotBurned, -burnLRR, -burndiff)%>%
  spread(func, Burned, fill=0)%>%
  dplyr::select(1, 5, 6, 7)
colnames(wcrNUB)<-c("id", "pg", "ag", "f")
wcrNUB<-wcrNUB%>%
  group_by(id)%>%
  mutate(totcov=sum(pg, ag, f))%>%
  mutate(pg=pg*100/totcov, ag=ag*100/totcov, f=f*100/totcov)%>%
  mutate(newtot=ag+pg+f)%>%
  dplyr::select(-totcov, -newtot)%>%
  ungroup()

wcrNUBdrought_control<-wcrNUB%>%
  filter(id%in% unique(filter(newplotkey, site=='nortnern'&(fullclim=="control"|fullclim=="drought"))$plot))
wcrNUBwarm_control<-wcrNUB%>%
  filter(id%in% unique(filter(newplotkey, site=='nortnern'&(fullclim=="control"|fullclim=="warm"))$plot))
wcrNUBwarmppt_control<-wcrNUB%>%
  filter(id%in% unique(filter(newplotkey, site=='nortnern'&(fullclim=="control"|fullclim=="warmppt"))$plot))



wcrCUB<-vegtog2p%>%
  filter(site=="Central")%>%  
  dplyr::select(-NotBurned, -burnLRR, -burndiff)%>%
  spread(func, Burned, fill=0)%>%
  dplyr::select(1, 5, 6, 7)
colnames(wcrCUB)<-c("id", "pg", "ag", "f")
wcrCUB<-wcrCUB%>%
  group_by(id)%>%
  mutate(totcov=sum(pg, ag, f))%>%
  mutate(pg=pg*100/totcov, ag=ag*100/totcov, f=f*100/totcov)%>%
  mutate(newtot=ag+pg+f)%>%
  dplyr::select(-totcov, -newtot)%>%
  ungroup()
wcrCUBdrought_control<-wcrCUB%>%
  filter(id%in% unique(filter(newplotkey, site=='central'&(fullclim=="control"|fullclim=="drought"))$plot))
wcrCUBwarm_control<-wcrCUB%>%
  filter(id%in% unique(filter(newplotkey, site=='central'&(fullclim=="control"|fullclim=="warm"))$plot))
wcrCUBwarmppt_control<-wcrCUB%>%
  filter(id%in% unique(filter(newplotkey, site=='central'&(fullclim=="control"|fullclim=="warmppt"))$plot))


wcrSUB<-vegtog2p%>%
  filter(site=="Southern")%>%  
  dplyr::select(-NotBurned, -burnLRR, -burndiff)%>%
  spread(func, Burned, fill=0)%>%
  dplyr::select(1, 5, 6, 7)
colnames(wcrSUB)<-c("id", "pg", "ag", "f")
wcrSUB<-wcrSUB%>%
  group_by(id)%>%
  mutate(totcov=sum(pg, ag, f))%>%
  mutate(pg=pg*100/totcov, ag=ag*100/totcov, f=f*100/totcov)%>%
  mutate(newtot=ag+pg+f)%>%
  dplyr::select(-totcov, -newtot)%>%
  ungroup()
wcrSUBdrought_control<-wcrSUB%>%
  filter(id%in% unique(filter(newplotkey, site=='southern'&(fullclim=="control"|fullclim=="drought"))$plot))
wcrSUBwarm_control<-wcrSUB%>%
  filter(id%in% unique(filter(newplotkey, site=='southern'&(fullclim=="control"|fullclim=="warm"))$plot))
wcrSUBwarmppt_control<-wcrSUB%>%
  filter(id%in% unique(filter(newplotkey, site=='southern'&(fullclim=="control"|fullclim=="warmppt"))$plot))


# add rownames
rownames(wide_composition_rel) <-wide_composition$id
wide_composition_rel <- dplyr::select(wide_composition_rel, -id)

rownames(wcrUB1) <-wcrUB1$id
wcrUB1 <- dplyr::select(wcrUB1, -id)

#### CREATE CLUSTERS ####
library(vegan)
library(MASS)
library(cluster)
library(factoextra)
#make bray-curtis dissimilarity matrix
wide_dist <- vegdist(wide_composition_rel)


x<-hclust(wide_dist, method="complete")
y<-agnes(wide_dist, method="complete")
plot(x)
pltree(y, cex = 0.6, hang = -1, main = "Dendrogram of agnes")

fviz_nbclust(as.matrix(wide_dist), FUN = hcut, method="wss")

groups<-cutree(x, k=4) #change K to change number of clusters

wide_clusters<-cbind(wide_composition, groups)
#wide_clusters$id <- row.names(wide_clusters)#%>%
wide_clusters1<-wide_clusters%>%
  separate(id, c("plot", "burntrt"))%>%
  #dplyr::select(plot, burntrt, 2, 3, 4, 5)%>%
  mutate(plot=as.numeric(plot))%>%
  mutate(site=ifelse(plot<21, "southern", "central"))%>%
  mutate(site=ifelse(plot>40, "northern", site))%>%
  mutate(groups=as.factor(groups))%>%
  mutate(groups=ifelse(groups==1, "AG + Forbs", ifelse(groups==2, "Annuals Dominant", ifelse(groups==4, "PG + Forbs", "Forbs Dominant"))))
#mutate(groups=ifelse(groups==3, "Forbs and Perennials", ifelse(groups==1, "Annuals", "Forbs")))
wide_clusters1$site<-factor(wide_clusters1$site, levels=c("southern", 
                                                          "central", 
                                                          "northern"))
wide_clusters1$groups<-factor(wide_clusters1$groups, levels=c("Annuals Dominant", "AG + Forbs","PG + Forbs","Forbs Dominant"))
#wide_clusters1$groups<-factor(wide_clusters1$groups, levels=c("Forbs", 

wide_clusters1$burntrt<-factor(wide_clusters1$burntrt, levels=c("NotBurned", "Burned"))

### characterize clusters
cluster_char<-wide_clusters1%>%
  gather(fg, cover, ag, pg, f)
cluster_char$fg<-factor(cluster_char$fg, levels=c("f", "pg", "ag"))

means<-cluster_char%>%
  group_by(groups, fg)%>%
  summarize(mean=mean(cover), se=calcSE(cover))

### FIGURE : CHARACTERIZATION OF CLUSTERS ####
ggplot(cluster_char, aes(x=groups, y=cover))+geom_boxplot(aes(fill=fg))  +
  scale_fill_manual(labels=c("forbs", "perennial grasses", "annual grasses"), 
                    values=c("royalblue3","chartreuse4",  "tan"))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()), text=element_text(size=13)) +ylab("Mean sublot relative cover") + theme_classic()

### ASSOCIATED STATS ####
cluster_char1<-mutate(cluster_char, trt=as.factor(paste(groups, fg, sep="_")))

pgMM<-lme(cover~trt, random = ~1|site/plot/burntrt, data = subset(cluster_char1))
summary(glht(pgMM, linfct=mcp(trt="Tukey")))

#######  ESTABLISHMETN BY CLUSTER BY CLUSTER , NEEDS TO MOVE AFTER CLUSTERING RESULTS #########

figure5.cluster<-figure5%>%
  group_by(plot, sub, site, burntrt, metric)%>%
  mutate(value=ifelse(metric=="abundance", value/953.5711, value))%>%
  summarize(mvalue=mean(value))%>%
  ungroup()%>%
  mutate(plot=as.numeric(plot))%>%
  mutate(burntrt=ifelse(burntrt=="Not Burned", "NotBurned", "Burned"))
figure5.cluster1<-left_join(figure5.cluster, dplyr::select(wide_clusters1, groups, (plot), burntrt))%>%
  dplyr::select(-sub)%>%
  spread(burntrt, groups)%>%
  dplyr::select(-Burned)%>%
  spread(`NotBurned`, mvalue)%>%
  mutate(b=(`<NA>`))%>%
  mutate(ub=ifelse(!is.na(`Annuals Dominant`), `Annuals Dominant`, 
                   ifelse(!is.na(`AG + Forbs`), `AG + Forbs`, 
                          ifelse(!is.na(`PG + Forbs`), `PG + Forbs`, `Forbs Dominant`))))%>%
  mutate(b=ifelse(is.na(b), 0, b))%>%
  mutate(func=ifelse(!is.na(`Annuals Dominant`), 'AG Dominant', 
                     ifelse(!is.na(`AG + Forbs`), 'AG + Forbs', 
                            ifelse(!is.na(`PG + Forbs`), 'PG + Forbs', 'Forbs Dominant'))))%>%
  dplyr::select(-c(4:8))%>%
  mutate(difference=b-ub)%>%
  mutate(lrr=log((b+.0001)/(ub+.0001)))
figure5.cluster1$func<-factor(figure5.cluster1$func, levels=c("AG Dominant", "AG + Forbs", "PG + Forbs", "Forbs Dominant"))

#### FIGURE : ESTABLISHMENT BY STARTING COMPOSITION ####

#whole unburned
ggplot(subset(figure5.cluster1, metric!="richness"), aes(x=func, y=ub))+
  geom_jitter(size=.5, color="grey")+
  geom_boxplot(aes(fill=func))+  facet_wrap(~metric, scales="free")+
  scale_fill_manual(values=c("tan", "goldenrod2", "chartreuse4","royalblue3"))+
  
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+theme_classic()

#whole lrr
ggplot(subset(figure5.cluster1, metric!="richness"), aes(x=func, y=lrr))+
  geom_jitter(size=.5, color="grey")+
  geom_boxplot(aes(fill=func))+  facet_wrap(~metric, scales="free")+
  scale_fill_manual(values=c("tan", "goldenrod2", "chartreuse4","royalblue3"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank())+theme_classic()

#central ub
ggplot(subset(figure5.cluster1, metric!="richness"&site=="Central"), aes(x=func, y=ub))+
  geom_jitter(size=.5, color="grey")+
  geom_boxplot(aes(fill=func))+  facet_wrap(~metric, scales="free")+
  scale_fill_manual(values=c("tan", "goldenrod2", "chartreuse4","royalblue3"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +theme_classic()

#central lrr
ggplot(subset(figure5.cluster1, metric!="richness"&site=="Central"), aes(x=func, y=lrr))+
  geom_jitter(size=.5, color="grey")+
  geom_boxplot(aes(fill=func))+  facet_wrap(~metric, scales="free")+
  scale_fill_manual(values=c("tan", "goldenrod2", "chartreuse4","royalblue3"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank()) +theme_classic()

inter<-mutate(figure5.cluster1, trt=as.factor(paste(func, site, sep="_")))  

#allsite
MM<-lme(lrr~func, random = ~1|site, data = subset(inter, metric=="Shannon"&!is.na(difference)))
summary(glht(MM, linfct=mcp(func="Tukey")))
cld(glht(MM, linfct=mcp(func="Tukey")))

#by site
MM<-aov(lrr~func, data = subset(inter, metric=="Shannon"&!is.na(difference)&site=="Central"))
summary(glht(MM, linfct=mcp(func="Tukey")))
cld(glht(MM, linfct=mcp(func="Tukey")))


summary(glht(npgMM, linfct=mcp(func="Tukey")))
cld(glht(npgMM, linfct=mcp(func="Tukey")))

abMM<-lme(difference~func, random = ~1|site/plot, data = subset(figure5.cluster1, metric=="abundance"&!is.na(difference)))
summary(glht(abMM, linfct=mcp(func="Tukey")))
cld(glht(abMM, linfct=mcp(func="Tukey")))

shMM<-lme(difference~func, random = ~1|plot, data = subset(figure5.cluster1, metric=="Shannon"&!is.na(difference)))
summary(glht(shMM, linfct=mcp(func="Tukey")))
cld(glht(shMM, linfct=mcp(func="Tukey")))

### RUN NMDS 2019 DATA #### 

# run the NMDS
plotspecNMDS <- metaMDS(wide_composition_rel, scale=T)
#base r plot
plot(plotspecNMDS)

# Extract and format axis scores
data.scores <- as.data.frame(scores(plotspecNMDS, display=c("sites")))
data.scores$ID <- (wide_composition$id)

data.scores <- as_tibble(data.scores) %>%
  separate(ID, c("plot", "burntrt"), sep="_") %>%
  mutate(plot=as.numeric(plot)) %>%
  mutate(site=ifelse(plot<21, "southern", "central"))%>%
  mutate(site=ifelse(plot>40, "northern", site))
data.scores<-left_join(data.scores, dplyr::select(wide_clusters1, plot, burntrt, groups))
plotkey<-plotkey%>%
  mutate(burntrt=ifelse(burntrt=="u", "NotBurned", "Burned"))
data.scores<-left_join(data.scores, dplyr::select(plotkey, plot, fullclim))


# Extract and format species scores
species.scores <- as.data.frame(scores(plotspecNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)%>%
  mutate(species=ifelse(species=="ag", "annual grasses", ifelse(species=="pg", "perennial grasses", "forbs")))

site.burn.mean=aggregate(data.scores[,c("NMDS1", "NMDS2")], 
                         list(group = interaction(data.scores$site, data.scores$burntrt)), mean)

site.trt.mean=aggregate(data.scores[,c("NMDS1", "NMDS2")], 
                        list(group = interaction(data.scores$site, data.scores$fullclim)), mean)



### FIGURE : NMDS SITE BY LEGAGY TREATMENT ####
ggplot() + 
  #geom_segment(data=species.scores,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
  #                       arrow = arrow(length = unit(0.25, "cm")),colour="grey70")+
  geom_point(data=subset(data.scores, burntrt=="NotBurned"),aes(x=NMDS1,y=NMDS2,shape=site, color=fullclim), size=2) + # add the point markers
  coord_equal() +
  scale_colour_manual(values=c("dodgerblue","darkorange","red","purple")) +
  scale_shape_manual(values=c(16, 17, 15))+
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=4) +
  stat_ellipse(aes(x=data.scores$NMDS1, y=data.scores$NMDS2, group=data.scores$site), type='t',size =.5, linetype=2)+
  theme_classic()+xlab("NMDS 1")+ylab("NMDS 2")+theme_classic()#+

#### FIGURE : NMDS SITE BY CLUSTER MEMBERHSIP ####
ggplot() +
  # geom_segment(data=species.scores,aes(x=0,xend=NMDS1,y=0,yend=NMDS2),
  #              arrow = arrow(length = unit(0.25, "cm")),colour="grey80")+
  geom_point(data=subset(data.scores, burntrt=="NotBurned"),aes(x=NMDS1,y=NMDS2,color=groups, shape=site), size=2) +
  scale_colour_manual(values=c("tan", "goldenrod2", "chartreuse4", "royalblue3")) +
  scale_shape_manual(values=c(16, 17, 15))+
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=4) +
  stat_ellipse(aes(x=data.scores$NMDS1, y=data.scores$NMDS2, group=data.scores$site), type='t',size =.5, linetype=2)+
  theme_classic()+xlab("NMDS 1")+ylab("NMDS 2")+theme_classic()  

## perMANOVA NMDS STATISTICS ####
# main effects of site and climate and groups
adonis(wcrUB1 ~ site*fullclim, data=subset(wcrel2.5, burntrt=="NotBurned"), perm=1e3)
adonis(wcrUB1 ~ groups, data=subset(newplotkey, burntrt=="NotBurned"), perm=1e3)



#main effects of climate within each group. none, so dont go into pairs.   
adonis(dplyr::select(filter(wcrNUB, id!=51), -1) ~ fullclim, data=subset(wcrel2.5, site=="nortnern"&burntrt=="NotBurned"), perm=1e3) # nothing significant
adonis(dplyr::select(wcrCUB, -1) ~ fullclim, data=subset(wcrel2.5, site=="central"&burntrt=="NotBurned"), perm=1e3) # nothing significant
adonis(dplyr::select(wcrSUB, -1) ~ fullclim, data=subset(wcrel2.5, site=="southern"&burntrt=="NotBurned"), perm=1e3) # nothing significant

#with warming combined
adonis(wcrUB1 ~ site*warmtrt, data=subset(newplotkey, burntrt=="NotBurned"), perm=1e3)

adonis(dplyr::select(filter(wcrNUB, id!=51), -1) ~ warmtrt, data=subset(newplotkey, site=="nortnern"&burntrt=="NotBurned"), perm=1e3) # nothing significant
adonis(dplyr::select(wcrCUB, -1) ~ warmtrt, data=subset(newplotkey, site=="central"&burntrt=="NotBurned"), perm=1e3) # nothing significant
adonis(dplyr::select(wcrSUB, -1) ~ warmtrt, data=subset(newplotkey, site=="southern"&burntrt=="NotBurned"), perm=1e3) # nothing significant

#just drought
adonis(dplyr::select(filter(wcrNUBdrought_control, id!=51), -1) ~ fullclim, data=subset(wcrel2.5, site=="nortnern"&burntrt=="NotBurned"&(fullclim=="control"|fullclim=="drought")), perm=1e3) # nothing significant
adonis(dplyr::select(wcrCUBdrought_control, -1) ~ fullclim, data=subset(wcrel2.5, site=="central"&burntrt=="NotBurned"&(fullclim=="control"|fullclim=="drought")), perm=1e3) # nothing significant
adonis(dplyr::select(wcrSUBdrought_control, -1) ~ fullclim, data=subset(wcrel2.5, site=="southern"&burntrt=="NotBurned"&(fullclim=="control"|fullclim=="drought")), perm=1e3) # nothing significant

#just warm
adonis(dplyr::select(filter(wcrNUBwarm_control, id!=51), -1) ~ fullclim, data=subset(wcrel2.5, site=="nortnern"&burntrt=="NotBurned"&(fullclim=="control"|fullclim=="warm")), perm=1e3) # nothing significant
adonis(dplyr::select(wcrCUBwarm_control, -1) ~ fullclim, data=subset(wcrel2.5, site=="central"&burntrt=="NotBurned"&(fullclim=="control"|fullclim=="warm")), perm=1e3) # nothing significant
adonis(dplyr::select(wcrSUBwarm_control, -1) ~ fullclim, data=subset(wcrel2.5, site=="southern"&burntrt=="NotBurned"&(fullclim=="control"|fullclim=="warm")), perm=1e3) # nothing significant

#just warmppt
adonis(dplyr::select(filter(wcrNUBdrought_control, id!=51), -1) ~ fullclim, data=subset(wcrel2.5, site=="nortnern"&burntrt=="NotBurned"&(fullclim=="control"|fullclim=="warmppt")), perm=1e3) # nothing significant
adonis(dplyr::select(wcrCUBdrought_control, -1) ~ fullclim, data=subset(wcrel2.5, site=="central"&burntrt=="NotBurned"&(fullclim=="control"|fullclim=="warmppt")), perm=1e3) # nothing significant
adonis(dplyr::select(wcrSUBdrought_control, -1) ~ fullclim, data=subset(wcrel2.5, site=="southern"&burntrt=="NotBurned"&(fullclim=="control"|fullclim=="warmppt")), perm=1e3) # nothing significant

#main effects of burn and site interaction 2019
adonis(wide_composition_rel ~ site*burntrt, data=wcrel2.5, perm=1e3)

#burns by site 2019
adonis(dplyr::select(filter(wcrN), -1) ~ burntrt, data=subset(wcrel2.5, site=="nortnern"&plot!=51), perm=1e3) 
adonis(dplyr::select(wcrC, -1) ~ burntrt, data=subset(wcrel2.5, site=="central"), perm=1e3) 
adonis(dplyr::select(wcrS, -1) ~ burntrt, data=subset(wcrel2.5, site=="southern"), perm=1e3) 


### FIGURE : RIVER PLOTS, TRANSITIONS ANALYSIS ####
library(ggalluvial)


riverN<-ggplot(subset(wide_clusters1, site=="northern"), aes(x=burntrt, stratum=groups, alluvium=plot, 
                                                             fill=groups, label=groups))+
  geom_flow(width=.5)+
  scale_x_discrete(expand = c(.1, .1))+
  geom_stratum(alpha=.5, width=.5)+
  geom_text(stat="stratum", size=0, min.y=3)+
  scale_fill_manual(values=c("royalblue3","chartreuse4", "tan", "goldenrod2"))+
  theme(legend.position = "none")+xlab("")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()), text=element_text(size=15))

riverC<-ggplot(subset(wide_clusters1, site=="central"), aes(x=burntrt, stratum=groups, alluvium=plot, 
                                                            fill=groups, label=groups))+
  geom_flow(width=.5)+
  scale_x_discrete(expand = c(.1, .1))+
  geom_stratum(alpha=.5, width=.5)+
  geom_text(stat="stratum", size=0, min.y=3)+
  scale_fill_manual(values=c("royalblue3","chartreuse4", "goldenrod2", "tan"))+
  theme(legend.position = "none")+xlab("")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()), text=element_text(size=15))

riverS<-ggplot(subset(wide_clusters1, site=="southern"), aes(x=burntrt, stratum=groups, alluvium=plot, 
                                                             fill=groups, label=groups))+
  geom_flow(width=.5)+
  scale_x_discrete(expand = c(.1, .1))+
  geom_stratum(alpha=.5, width=.5)+
  geom_text(stat="stratum", size=0, min.y=3)+
  scale_fill_manual(values=c("royalblue3","goldenrod2", "tan"))+
  theme(legend.position = "none", text=element_text(size=15))+xlab("")
ggarrange(riverN, riverC, riverS, ncol=1, nrow=3)


