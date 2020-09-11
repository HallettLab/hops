### Created 7/29/19
### Analysis of first year burning and seeding in sites
library(tidyverse)
library(ggpubr)
##FN for Calculating SE
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

##### SETUP AND READ-IN #####
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
timeseries<-rbind(vegplot_j, vegplot2020_j) # dat for timeseries

##### Cleanup #####

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

## Burning on AG/LITTER/BG as % change ####

# Each veg as a proportion of all veg
ggplot(subset(vegtog1, func=="Litter"|func=="Bare ground"), aes(x=burntrt, y=cover))+geom_jitter(color="grey", size=.25)+geom_boxplot(aes(fill=burntrt))+facet_grid(site~func, scales="free")+
  scale_fill_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank())) +ylab("% Cover")
#summarized+errorbars
                     
### 1b. Annual:perennial LRR response to burning 
#vegtog.rat<-vegtog%>%
#  mutate(perag=as.numeric(perag), perpg=as.numeric(perpg))%>%
#  filter(!is.na(perag), !is.na(perpg), perag<100, perpg<100)%>%
#  mutate(perag=perag+1, perpg=perpg+1)%>%
#  mutate(anntoper=perag/perpg)%>%
#  mutate(anntoper=ifelse(plot>39&anntoper>10, 0, anntoper))%>%  ## TAKING OUT A WILD OUTLIER FOR VIZ PURPOSES - GO BACK TO THIS TO SEE WHY ITS CRAZY
#  mutate(anntoper=log(anntoper)) # log it so that everything between 0 and 1 (more perennials) is visible as negative
#visualize
#ggplot(vegtog.rat, aes(x=burntrt, y=anntoper))+geom_jitter(color="grey", size=.25)+geom_boxplot(aes(fill=burntrt))+facet_wrap(~site, scales="free")+
#   scale_fill_manual(labels=c("Burned","Unburned"), 
#                      values=c("brown2", "dodgerblue"))+
#  theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank(), legend.title=(element_blank())) +ylab("log(Annual/Perennial cover)")


### 1c. Vegtog 2 p: LRR of burned:not burned by functional group at the plot level
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

vegtog2s<-vegtog1%>% #vegtog 2s: LRR of burned: not burned by fn group at the microplot level (just lots more replicates)
  group_by(plot, seedmix, func, burntrt, site)%>%
  filter(!is.na(cover))%>%
  summarize(cover=mean(cover))%>%
  ungroup()%>%
  mutate(cover=cover+1)%>%
  mutate(burntrt=ifelse(burntrt=="Not Burned", "NotBurned", "Burned"))%>%
  spread(burntrt, cover)%>%
  mutate(burnLRR=log(Burned/NotBurned))%>%
  mutate(burndiff=Burned-NotBurned)


## Setup Burn LRR by FG ####

#LRR
#ggplot(subset(vegtog2, func=="Annual grasses"|func=="Litter"|func=="Bare ground"), aes(x=site, y=burnLRR))+geom_jitter(color="grey", size=.25)+geom_boxplot(aes(fill=site))+geom_hline(yintercept=0)+facet_grid(~func, scales="free")+
#  scale_fill_manual(labels=c("Northern", "Central", "Southern"), 
#                    values=c("green3", "yellowgreen", "burlywood1"))+
#  theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(Burned/Unburned % cover)")

##oldfig
#ggplot(subset(vegtog2p, func=="Litter"|func=="Bare ground"), aes(x=site, y=burnLRR))+geom_jitter(color="grey", size=.25)+geom_boxplot(aes(fill=site))+geom_hline(yintercept=0)+facet_grid(burntrt~func, scales="free")+
#  scale_fill_manual(labels=c("Northern", "Central", "Southern"), 
#                    values=c("green3", "yellowgreen", "burlywood1"))+
#  theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("Change in % cover from burning")

vegtog2bub<-vegtog2p%>%
  gather(burntrt, cover, Burned, NotBurned)
vegtog2bub$burntrt<-factor(vegtog2bub$burntrt, levels=c("NotBurned", "Burned"))

# old vis: Burn LRR Litter/BG###########
ggplot(subset(vegtog2bub, func=="Litter"|func=="Bare ground"), aes(x=site, y=cover))+geom_jitter(color="grey", size=.25)+geom_boxplot(aes(fill=site))+facet_grid(func~burntrt, scales="free")+
  scale_fill_manual(labels=c("Northern", "Central", "Southern"), 
                    values=c("green3", "yellowgreen", "burlywood1"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("Subplot Percent Cover")

vegtog2bubs<-vegtog2s%>%
  gather(burntrt, cover, Burned, NotBurned)
vegtog2bubs$burntrt<-factor(vegtog2bubs$burntrt, levels=c("NotBurned", "Burned")) 

ggplot(subset(vegtog2bubs, func=="Litter"|func=="Bare ground"), aes(x=site, y=cover))+geom_jitter(color="grey", size=.25)+geom_boxplot(aes(fill=site))+facet_grid(func~burntrt, scales="free")+
  scale_fill_manual(labels=c("Northern", "Central", "Southern"), 
                    values=c("green3", "yellowgreen", "burlywood1"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("Micro Percent Cover")

### 1d. Incorporate heating effect at the central site. Interaction of prior heating + burning to percent covers.
#vegtog.rat$burntrt<-factor(vegtog.rat$burntrt, levels=c("Not Burned", "Burned"))
#vegtog.rat$bopsclim<-factor(vegtog.rat$bopsclim, levels=c("Not Warmed", "Warmed"))
#ggplot(subset(vegtog.rat), aes(x=interaction(bopsclim, burntrt), y=anntoper))+geom_jitter(color="grey", size=.25)+geom_boxplot(aes(fill=interaction(bopsclim,burntrt)))+facet_wrap(~site, scales="free")+
#  scale_fill_manual(labels=c("Unburned, Ambient", "Unburned, Warmed","Burned, Ambient",  "Burned, Warmed"), 
#                    values=c("dodgerblue", "chartreuse3","brown2",  "orange"))+
#  theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank(), legend.title=(element_blank())) +ylab("log(Annual/Perennial cover)")

# old vis: Burn LRR AG/PG/F ####

vegtog3<-vegtog2p%>%
  filter(func!="Bare ground"&func!="Litter")%>%
  group_by(plot, site)%>%
  summarize(burntot=sum(Burned), unburntot=sum(NotBurned))

vegtog4<-left_join(vegtog2p, vegtog3)%>%
  mutate(burned=Burned/burntot, unburned=NotBurned/unburntot)%>% #the lower case ones are ONLY as a proportion of vegetative cover (excluding BG and litter)
  mutate(difdif=burned-unburned)%>%
  gather(trt, prop, burned, unburned)%>%
  mutate(trt=as.factor(trt))
vegtog4$trt<-factor(vegtog4$trt, levels=c("unburned", "burned"))

ggplot(subset(vegtog4, func=="Annual grasses"|func=="Perennial grasses"|func=="Forbs"), aes(x=site, y=(difdif)))+geom_jitter(color="grey", size=.25)+geom_boxplot(aes(fill=site))+geom_hline(yintercept=0)+facet_grid(~func, scales="free")+
  scale_fill_manual(labels=c("Northern", "Central", "Southern"), 
                    values=c("green3", "yellowgreen", "burlywood1"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("Relative Cover LRR")

ggplot(subset(vegtog4, func=="Annual grasses"|func=="Perennial grasses"|func=="Forbs"), aes(x=trt, y=prop))+geom_point(color="grey", size=.25)+facet_grid(~func) + geom_line(aes(group=plot, color=site))+
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("Relative cover")+
    scale_color_manual(labels=c("Northern", "Central", "Southern"), 
                      values=c("green3", "yellowgreen", "burlywood1"))



### Question 3: Species Seedling Dynamics #####

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
vegtog_survival_spread<-vegtog_survival%>%
  slice(-924, -1045, -1166, -1286, -1408, -1529, -1650, -1771, -1892, -2013)%>%
  dplyr::select(plot, site, seedmix, sciname, native, longevity, type, burntrt, survival)%>%
  mutate(burntrt=ifelse(burntrt=="Not Burned", "notburned", "burned"))%>%
  spread(burntrt, survival)


  
# visualize totals seedlings by site/treatment
#ggplot(subset(vegtog_counts), 
#       aes(x=interaction(bopsclim, burntrt), y=log(count), fill=interaction(burntrt,bopsclim)))+ 
#  geom_boxplot() +facet_grid(seedmix~site, scales="free") +
#  theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank())

# visualize % survival by species, site, burn, warm

#### FIGURE 4a: SURVIVAL BY SITE 2019   ####
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

vegtog_survival<-left_join(vegtog_counts, spkey, by=c("species"="code"))%>%
  filter(seedmix!="?", seedmix!="Restoration mix")%>%
  mutate(grams=ifelse(seedmix=="Forbs", .053, ifelse(seedmix=="California grasses", .266, .8)))%>%
  mutate(seeded=grams/seed_g100*100)%>%
  mutate(survival=count/seeded)%>%
  filter(species!="agoseris", species!="lupinus", species!="microseris", species!="tritilea", species!="zigadenus") %>%
  group_by(species)%>%
  mutate(zsurvival=scale(survival))%>%
  mutate(func=ifelse(species=="koeleria"|species=="dancalor"|species=="fesroe", "Native Perennial Grasses", ifelse(species=="pasture", "Pasture Grasses", species)))
vegtog_survival_spread<-vegtog_survival%>%
  slice(-924, -1045, -1166, -1286, -1408, -1529, -1650, -1771, -1892, -2013)%>%
  dplyr::select(plot, site, seedmix, sciname, native, longevity, type, burntrt, survival)%>%
  mutate(burntrt=ifelse(burntrt=="Not Burned", "notburned", "burned"))%>%
  spread(burntrt, survival)

survivalTS<-subset(vegtog_survival, site=="Central")%>%
  dplyr::select(plot, sub, micro, seedmix, burntrt, species, count, survival)

#### STAT ####
library(nlme)
library(multcomp)
survivalTS2<-survivalTS1%>%
  mutate(trt=as.factor(paste(year, burntrt, sep="_")))

#Fescue survival MM TS
survival_fesroe<-lme(survival3~trt, random = ~1|micro/sub, data = subset(survivalTS2, species=="fesroe"))
summary(glht(survival_fesroe, linfct=mcp(trt="Tukey")))
#Pasture survival MM TS
survival_pasture<-lme(survival3~trt, random = ~1|micro/sub, data = subset(survivalTS2, species=="pasture"))
summary(glht(survival_pasture, linfct=mcp(trt="Tukey")))

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

#cnt<-ggplot(subset(survivalTS_agg, species=="dancalor"|
#                species=='fesroe'|species=="koeleria"|
#                species=="pasture"), 
#       aes(x=as.factor(year), meancount)) + 
#  geom_line(aes(group=species, color=species))+
#  geom_point(aes(group=species, color=species))+
#  geom_errorbar(aes(ymin=meancount-secount, ymax=meancount+secount,group=species, color=species), width=.2)#+
#  facet_wrap(~species, scales="free")+ylab=("# Seedlings")


#data for yearly survival
survivalTS_agg1<-subset(survivalTS_agg,
                        species=="dancalor"|
                          species=='fesroe'|species=="koeleria"|
                          species=="pasture")%>%
  ungroup()%>% 
  mutate(species=as.factor(species))
levels(survivalTS_agg1$species) <- c("Danthonia californica", "Festuca roemeri", "Koeleria macrantha", "pasture grasses")

#####  SUPPLEMENT 4: SPP. SURVIVAL;YEAR #####
ggplot(subset(survivalTS_agg1), aes(x=as.factor(year), meansurvival)) + 
  geom_line(aes(group=burntrt, color=burntrt), size=1)+
  geom_point(aes(group=burntrt, color=burntrt))+
  geom_errorbar(aes(ymin=meansurvival-sesurvival, ymax=meansurvival+sesurvival,group=burntrt, color=burntrt), width=.2)+
  xlab("")+ylab("Central site net survival")+scale_color_manual(values=c("dodgerblue", "brown3"))+
  facet_wrap(~species, scales="free")+theme(text=element_text(size=15), legend.title= element_blank())

#ggarrange(cnt, srv, common.legend = T, legend="right")



#ggplot(subset(vegtog_survival_spread, longevity=="p"&type=="g"&(native=="n"|native=="i")), aes(x=sciname, y=survival, fill=site))+
#  geom_boxplot()  +
#  scale_fill_manual(labels=c("Northern", "Central", "Southern"), 
#                    values=c("green3", "yellowgreen", "burlywood1"))+
#  theme(axis.title.x=element_blank(),
#        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("Survival LRR Burned/Not Burned")+ggtitle("Native Perennial Grasses")

# CALIFORNIA PERENNIAL GRASSES
#ggplot(subset(vegtog_survival, longevity=="p"&type=="g"&native=="c"&species!="stipa"), aes(x=interaction(bopsclim, burntrt), y=log(survival+1), fill=interaction(burntrt, bopsclim)))+
#  geom_boxplot() + facet_grid(sciname~site, scales="free") +
#  scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed"), 
#                    values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+
#  theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(% Survival+1)")+ggtitle("California Perennial Grasses")

# PASTURE GRASSES
#ggplot(subset(vegtog_survival, longevity=="p"&type=="g"&native=="i"), aes(x=interaction(bopsclim, burntrt), y=log(survival+1), fill=interaction(burntrt, bopsclim)))+
#  geom_boxplot() + facet_grid(sciname~site, scales="free") +
#  scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
#                    values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+
#  theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(% Survival+1)")+ggtitle("Pasture Grasses")

# FORBS
# ggplot(subset(vegtog_survival, longevity=="a"&type=="f"&(species!="drymocallis"&species!="lomatiumu")), aes(x=interaction(bopsclim, burntrt), y=log(survival+1), fill=interaction(burntrt, bopsclim)))+
#  geom_boxplot() + facet_grid(sciname~site, scales="free") +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(% Survival+1)")+
#   scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
#                     values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+ggtitle("Forbs")

#DANCALCAL vs DANCALOR (about the same)
# ggplot(subset(vegtog_survival, longevity=="p"&type=="g"&(species=="dancalcal"|species=="dancalor")), aes(x=interaction(bopsclim, burntrt), y=log(survival+1), fill=interaction(burntrt, bopsclim)))+
#   geom_boxplot() + facet_grid(sciname~site, scales="free") +
#   scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
#                     values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(% Survival+1)")+ggtitle("Native vs. California Danthonia")
 
#FESROE vs FESIDA (about the same, but FESIDA better in SOR)
# ggplot(subset(vegtog_survival, longevity=="p"&type=="g"&(species=="fesroe"|species=="fesid")), aes(x=interaction(bopsclim, burntrt), y=log(survival+1), fill=interaction(burntrt, bopsclim)))+
#   geom_boxplot() + facet_grid(sciname~site, scales="free") +
#   scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
#                     values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(% Survival+1)")+ggtitle("Native Roemer's vs. California Idaho Fescue")
 
 #eugene mix shannon diversity
 library(codyn)
 eug.shannon<-left_join(vegtog_counts, spkey, by=c("species"="code"))%>%
   filter(seedmix=="Restoration mix")%>%
   group_by(plot, sub, micro, site, burntrt)%>%
   mutate(present=count/count)%>%
   mutate(rep=paste(plot, sub, micro, site))
 eug.shannon[is.na(eug.shannon)] <- 0

eug.shannon<-left_join(eug.shannon, community_diversity(eug.shannon, abundance.var="count", replicate.var="rep", metric = c("Shannon")))
 
  eug.shannon<-eug.shannon%>%
    group_by(plot, sub, micro, site, burntrt, Shannon)%>%
    summarize(richness=sum(present), abundance=sum(count))%>%
    gather(metric, value, richness, abundance, Shannon)
   
######### FIGURE 4B: RESTO seedling diversity ####
  f5b<-ggplot(subset(eug.shannon, metric!="richness"), aes(x=burntrt, y=value))+
  geom_jitter(size=.5, color="grey")+
  geom_boxplot(aes(fill=burntrt))+  facet_grid(metric~site, scales="free")+
  scale_fill_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+theme(text=element_text(size=15))+ylab("space")
  
  ggarrange(f5a, f5b, nrow=2, ncol=1, common.legend = T)
#combine figures 4 a and b
figure5<-vegtog_survival%>%
 ungroup()%>%
  select(plot, sub, micro, site, burntrt, func, survival)%>%
  filter(func=="Native Perennial Grasses"|func=="Pasture Grasses" )%>%
  mutate(metric=func, value=survival)%>%
  dplyr::select(-func, -survival)

figure5<-rbind(figure5, ungroup(eug.shannon))%>%
  filter(metric!="richness")
figure5$metric<-factor(figure5$metric, levels=c("Native Perennial Grasses", "Pasture Grasses", "abundance", "Shannon"))

ggplot(subset(figure5, metric!="richness"), aes(x=burntrt, y=value))+
  geom_jitter(size=.5, color="grey")+
  geom_boxplot(aes(fill=burntrt))+  facet_grid(metric~site, scales="free")+
  scale_fill_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+theme(text=element_text(size=15))+ylab("")+ geom_hline(yintercept=0)

figure5.cluster<-figure5%>%
  group_by(plot, sub, site, burntrt, metric)%>%
  summarize(mvalue=mean(value))%>%
  ungroup()%>%
  mutate(plot=as.numeric(plot))%>%
  mutate(burntrt=ifelse(burntrt=="Not Burned", "NotBurned", "Burned"))
figure5.cluster1<-left_join(figure5.cluster, select(wide_clusters1, groups, (plot), burntrt))

ggplot(subset(figure5.cluster1, metric!="richness"), aes(x=burntrt, y=mvalue))+
  geom_jitter(size=.5, color="grey")+
  geom_boxplot(aes(fill=burntrt))+  facet_grid(metric~groups*site, scales="free")+
  scale_fill_manual( 
                    values=c("dodgerblue", "brown2"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+theme(text=element_text(size=15))+ylab("")+ geom_hline(yintercept=0)


###STAT RESTO MIX ####
eug_shan2<-eug.shannon%>%
  mutate(trt=as.factor(paste(site, burntrt, sep="_")))%>%
  ungroup()%>%
  mutate(trt=as.factor(trt))
  
abundanceMM<-lme(value~trt, random = ~1|sub/micro, data = subset(eug_shan2, metric=="abundance"))
  summary(glht(abundanceMM, linfct=mcp(trt="Tukey")))
  
  ShannonMM<-lme(value~trt, random = ~1|sub/micro, data = subset(eug_shan2, metric=="Shannon"))
  summary(glht(ShannonMM, linfct=mcp(trt="Tukey")))
  
### Question 3: LITTER AS DRIVER #####  
biomass.test<-read_csv("early_bops_biomass.csv")%>%
  dplyr::select(3:9)%>%
  mutate(sub=tolower(sub))
vegtog_survival_bm<-left_join(vegtog_survival, biomass.test)%>%
  filter(!is.na(ag_g))%>%
  mutate(total.biomass=ag_g+pg_g+forb_g+litter_g)%>%
  mutate(perl.biomass=litter_g/total.biomass)%>%
  mutate(litter.vol=talll*perl)
vegtog_survival_bm2<-vegtog_survival_bm%>%
  mutate(litter_height=talll, litter_vol=litter.vol, litter_cover=perl, litter_weight=litter_g, litter_percent.bm=perl.biomass)

vegtog_survival_bm$burntrt<-factor(vegtog_survival_bm$burntrt, levels=c("Not Burned", "Burned"))
vegtog_survival_bm$bopsclim<-factor(vegtog_survival_bm$bopsclim, levels=c("Not Warmed", "Warmed"))

### 3.i Correlations between forms of litter: cover, depth, volume(cover*depth), total biomass, % of biomass. 
cm<-cor(as.matrix(select(vegtog_survival_bm2, litter_height, litter_vol, litter_cover, litter_weight, litter_percent.bm)))

corrplot(cm, type="upper", order="hclust", method="number")


#litter variability (cover vs. biomass)
lv1<-ggplot(vegtog_survival_bm, aes(x=as.factor(plot), y=litter_g)) + geom_boxplot(aes(fill=burntrt)) + geom_point() +facet_wrap(~burntrt)+labs(x="plot", y="litter (g)")+
  scale_fill_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2"))+xlab("")
lv2<-ggplot(vegtog_survival_bm, aes(x=as.factor(plot), y=perl)) + geom_boxplot(aes(fill=burntrt)) + geom_point() +facet_wrap(~burntrt)+labs(x="plot", y="percent litter")+
  scale_fill_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2")) +  theme( legend.title=(element_blank()))
lv3<-ggplot(vegtog_survival_bm, aes(x=as.factor(plot), y=perl*talll)) + geom_boxplot(aes(fill=burntrt)) + geom_point() +facet_wrap(~burntrt)+labs(x="plot", y="percent x depth litter")+
  scale_fill_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2"))+xlab("")
ggarrange(lv1, lv2, lv3, ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")

#do controls parallel general litter?
vegsub<-dplyr::select(vegplot, plot, sub, micro, seedmix)
controlstest<-left_join(biomass.test, vegsub)%>%
  mutate(subplot=paste(plot, sub, sep=""))

ggplot(controlstest, aes(x=as.factor(subplot), y=litter_g)) + geom_point(size=.4)  +
  labs(x="plot", y="litter (g)")+ 
    xlab("") +geom_point(data=subset(controlstest, seedmix=="x"), color="green")

ggplot(controlstest, aes(x=as.factor(subplot), y=ag_g)) + geom_point(size=.4)  +
  labs(x="plot", y="annual grasses (g)")+ 
  xlab("") +geom_point(data=subset(controlstest, seedmix=="x"), color="green")


ggplot(controlstest, aes(x=as.factor(subplot), y=pg_g)) + geom_point(size=.4)  +
  labs(x="plot", y="perennial grasses (g)")+ 
  xlab("") +geom_point(data=subset(controlstest, seedmix=="x"), color="green")


ggplot(controlstest, aes(x=as.factor(subplot), y=forb_g)) + geom_point(size=.4)  +
  labs(x="plot", y="forb (g)")+ 
  xlab("") +geom_point(data=subset(controlstest, seedmix=="x"), color="green")


### 3.1 How did burning (and warming) affect current litter cover and mass?
bl1<-ggplot(subset(vegtog_survival, site!="Northern"), aes(x=interaction(bopsclim, burntrt), y=(perl))) +geom_jitter(color="grey", size=.5)+
  geom_boxplot(aes(fill=interaction(bopsclim, burntrt))) + labs(x="", y="percent litter") +facet_wrap(~site)+
  scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
                    values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+theme(axis.title.x=element_blank(),
                                                                                     axis.text.x=element_blank(),
                                                                                     axis.ticks.x=element_blank(), legend.title=(element_blank()))
bl3<-ggplot(subset(vegtog_survival, site!="Northern"), aes(x=interaction(bopsclim, burntrt), y=(perl*talll))) +geom_jitter(color="grey", size=.5)+
  geom_boxplot(aes(fill=interaction(bopsclim, burntrt))) + labs(x="", y="percent x depth litter") +facet_wrap(~site)+
  scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
                    values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+theme(axis.title.x=element_blank(),
                                                                                     axis.text.x=element_blank(),
                                                                                     axis.ticks.x=element_blank(), legend.title=(element_blank()))
bl2<-ggplot(vegtog_survival_bm, aes(x=interaction(bopsclim, burntrt), y=litter_g)) +geom_jitter(color="grey", size=.5)+
  geom_boxplot(aes(fill=interaction(bopsclim, burntrt)))+ labs(x="", y="litter (g)") +facet_wrap(~site)+
  scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
                    values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+theme(axis.title.x=element_blank(),
                                                                                     axis.text.x=element_blank(),
                                                                                     axis.ticks.x=element_blank(), legend.title=(element_blank()))
ggarrange(bl1, bl3, bl2, nrow=3, common.legend = TRUE, legend="right")

### AG vs Litter drive survival####


#aggregate native grasses, pasture grasses, ca grasses
vegtog_survival2<-vegtog_survival%>%
  mutate(success=count)%>%
  mutate(failure=seeded-count)

vegtog_survival_agg<-vegtog_survival2%>%
  filter(longevity=="p"&type=="g")%>%
  group_by(plot, sub, native, longevity, type, bopsclim, burntrt, site, perag, talll, perl, success, failure)%>%
  ungroup()%>%
  mutate(failure=as.integer(failure))%>%
  mutate(failure=ifelse(failure<0, 0, failure))%>%
  mutate(success=as.integer(success))%>%
  mutate(native=ifelse(native=="n", "Native Perennial", ifelse(native=="i", "Pasture Grasses", "California Perennials")))
vegtog_survival_agg[is.na(vegtog_survival_agg)] <- 0 

#agPLOT<-ggplot(subset(vegtog_survival_agg, longevity=="p"&type=="g"&native!="California Perennials"), aes(x=perag, y=log(survival+1)))+ 
#  geom_jitter(aes(color=burntrt)) +facet_grid(native~site, scales="free")  +
#  geom_smooth(method="lm", color="black", se=F) + xlab("Percent annual grass")+
#  scale_color_manual(labels=c("Unburned", "Burned"), 
#                     values=c("dodgerblue", "brown2"))+theme(axis.text.x=element_blank(),
#                                                             axis.ticks.x=element_blank(), 
#                                                             legend.title=(element_blank()))
### FIGURE 6: SURVIVAL AS FN OF BIOMASS ####
ggplot(subset(vegtog_survival_agg, longevity=="p"&type=="g"&native!="California Perennials"), aes(x=perl*talll, y=log(survival+1), succ=success, fail=failure))+ 
  geom_jitter(aes(color=burntrt)) +facet_grid(native~site, scales="free")  +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), formula = cbind(succ, fail) ~ x, color="black", se=T) + xlab("Litter volume (cover*depth)")+
  scale_color_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2"))+theme(text=element_text(size=17),axis.text.x=element_blank(),
                                                           axis.ticks.x=element_blank(), 
                                                           legend.title=(element_blank()))+labs(y="Seedling survival")
glmdata<-vegtog_survival_agg%>%
  mutate(lvol=perl(talll))
mod <- glm(cbind(success, failure) ~ (perl*talll),family='binomial',data=subset(vegtog_survival_agg, native=="Pasture Grasses"&site=="Northern"), na.action=na.exclude)
summary(mod)
1-4460/8219

#lbPLOT<-ggplot(vegtog_survival_bm, aes(x=litter_g, y=log(survival+1)))+ 
#  geom_jitter(aes(color=burntrt)) +facet_grid(site~sciname, scales="free")  +
#  geom_smooth(method = "glm", 
#              method.args = list(family = "binomial") + xlab("grams litter") +
#  scale_color_manual(labels=c("Unburned", "Burned"), 
#                     values=c("dodgerblue", "brown2"))+
#  scale_color_manual(labels=c("Unburned", "Burned"), 
#                     values=c("dodgerblue", "brown2"))+theme(
#                                                             axis.ticks.x=element_blank(), 
#                                                             legend.title=(element_blank()))+  labs(x="Grams litter", y="log(% Survival+1)")
#  
#ggarrange(agPLOT, lvPLOT, common.legend = T, legend = "bottom")

 
### 3.xx Does litter cover affect community composition (percents of other groups?) - what happens when you burn away litter to background community? compare within plots difference between burned/not burned. 

### Question 1: Setup clusters (2019) ####
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

# add rownames
rownames(wide_composition_rel) <-wide_composition$id
wide_composition_rel <- dplyr::select(wide_composition_rel, -id)

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

groups<-cutree(x, k=4)

wide_clusters<-cbind(wide_composition, groups)
wide_clusters$ID <- row.names(wide_clusters)#%>%
wide_clusters1<-wide_clusters%>%
  separate(ID, c("plot", "burntrt"))%>%
  #dplyr::select(plot, burntrt, 2, 3, 4, 5)%>%
  mutate(plot=as.numeric(plot))%>%
  mutate(site=ifelse(plot<21, "southern", "central"))%>%
  mutate(site=ifelse(plot>40, "northern", site))%>%
  mutate(groups=as.factor(groups))%>%
  mutate(groups=ifelse(groups==1, "More Annuals", ifelse(groups==2, "Annuals Dominant", ifelse(groups==4, "Forbs and Perennials", "Forbs"))))

wide_clusters1$site<-factor(wide_clusters1$site, levels=c("southern", 
                                          "central", 
                                          "northern"))
wide_clusters1$groups<-factor(wide_clusters1$groups, levels=c("Forbs", 
                                                          "Forbs and Perennials", 
                                                          "More Annuals", "Annuals Dominant"))
wide_clusters1$burntrt<-factor(wide_clusters1$burntrt, levels=c("NotBurned", "Burned"))

### Question 1: Figure 1: Cluster Characterization (2019 Only) ####
### characterize clusters
cluster_char<-wide_clusters1%>%
  gather(fg, cover, ag, pg, f)
cluster_char$fg<-factor(cluster_char$fg, levels=c("f", "pg", "ag"))

means<-cluster_char%>%
  group_by(groups, fg)%>%
  summarize(mean=mean(cover), se=calcSE(cover))

### PUBLICATION FIGURE 1
ggplot(cluster_char, aes(x=groups, y=cover))+geom_boxplot(aes(fill=fg))  +
  scale_fill_manual(labels=c("forbs", "perennial grasses", "annual grasses"), 
                         values=c("gray25", "gray55", "gray90"))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()), text=element_text(size=20)) +ylab("Mean Sublot % Cover")


## Backup figure: Clusters by site/burntrt
#ggplot(wide_clusters1, aes(x=burntrt, y=site)) +geom_jitter(aes(color=groups), width=.15)+
#  scale_color_manual( 
#                    values=c("royalblue3","chartreuse4", "tan", "goldenrod2"))+
#  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
#        axis.ticks.x=element_blank(), legend.title=(element_blank()))



### Question 1: Figure 3: NMDS (2019 Only) #### 

# run the NMDS
plotspecNMDS <- metaMDS(wide_composition_rel, scale=T)
#base r plot
plot(plotspecNMDS)

# Extract and format axis scores
data.scores <- as.data.frame(scores(plotspecNMDS, display=c("sites")))
data.scores$ID <- row.names(wide_composition_rel)

data.scores <- as_tibble(data.scores) %>%
  separate(ID, c("plot", "burntrt"), sep="_") %>%
  mutate(plot=as.numeric(plot)) %>%
  mutate(site=ifelse(plot<21, "southern", "central"))%>%
  mutate(site=ifelse(plot>40, "northern", site))
data.scores<-left_join(data.scores, dplyr::select(wide_clusters1, plot, burntrt, groups))
plotkey<-plotkey%>%
  mutate(burntrt=ifelse(burntrt=="u", "NotBurned", "Burned"))
data.scores<-left_join(data.scores, dplyr::select(plotkey, plot, burntrt, fullclim))
data.scores<-data.scores%>%
  mutate(fullclim=ifelse(fullclim=="drought"|fullclim=="control", "control", "warmed"))

# Extract and format species scores
species.scores <- as.data.frame(scores(plotspecNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)%>%
  mutate(species=ifelse(species=="ag", "annual grasses", ifelse(species=="pg", "perennial grasses", "forbs")))

site.burn.mean=aggregate(data.scores[,c("NMDS1", "NMDS2")], 
                    list(group = interaction(data.scores$site, data.scores$burntrt)), mean)
arrows<-site.burn.mean%>%
  separate(group, into = c("site", "burntrt"))%>%
  gather("id", "val", NMDS1, NMDS2)%>%
  mutate(id=ifelse(id=="NMDS1", "x", "y"))%>%
  mutate(id=paste(id, burntrt, sep="_"))%>%
  dplyr::select(-burntrt)%>%
  spread(id, val)

site.warm.mean=aggregate(data.scores[,c("NMDS1", "NMDS2")], 
                            list(group = interaction(data.scores$site, data.scores$fullclim)), mean)
arrows.warm<-site.warm.mean%>%
  separate(group, into = c("site", "fullclim"))%>%
  gather("id", "val", NMDS1, NMDS2)%>%
  mutate(id=ifelse(id=="NMDS1", "x", "y"))%>%
  mutate(id=paste(id, fullclim, sep="_"))%>%
  dplyr::select(-fullclim)%>%
  spread(id, val)

### Pub Figure 3: NMDS/site by burntrt (2019)

#ggplot() +
  # add the species labels
 # geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=burntrt, color=site,), size=2) + # add the point markers
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
#  scale_colour_manual(values=c("gray50", "gray80", "black")) +
#  scale_shape_manual(values=c(1, 16))+
#  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=4) +
#  coord_equal() +
#  theme_bw()+
  #annotate("text",x = site.burn.mean$NMDS1,y = site.burn.mean$NMDS2,label=site.burn.mean$group)+
 # geom_segment(aes(x=arrows$x_NotBurned, y=arrows$y_NotBurned, 
   #                xend=arrows$x_Burned, yend=arrows$y_Burned), 
  #             arrow=arrow(length=unit(.3, "cm")), color="red3", size=2)


#Figure 1a. Site/Cluster/Warming NMDS
ggplot() +
  # add the species labels
  stat_ellipse(aes(x=data.scores$NMDS1, y=data.scores$NMDS2, color=data.scores$site), type='t',size =.75, linetype=2)+
  geom_point(data=subset(data.scores, burntrt=="NotBurned"),aes(x=NMDS1,y=NMDS2,shape=fullclim, color=groups,), size=2) + # add the point markers
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
 # scale_colour_manual(values=c("gray50", "gray80", "black")) +
  scale_shape_manual(values=c(1, 16))+
  coord_equal() +
  scale_color_manual(values=c("tan","grey50" , "royalblue3","chartreuse4","goldenrod2", "gray80",  "black"))+
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=4) +
  theme_bw()+xlab("NMDS 1")+ylab("NMDS 2")+
  geom_segment(aes(x=arrows.warm$x_control, y=arrows.warm$y_control, 
                   xend=arrows.warm$x_warmed, yend=arrows.warm$y_warmed), 
               arrow=arrow(length=unit(.3, "cm")), size=1.5)
 

library(vegan)

wcrel2<-wide_composition%>%
  mutate(totcov=sum(pg, ag, f))%>%
  mutate(pg=pg*100/totcov, ag=ag*100/totcov, f=f*100/totcov)%>%
  separate(id, into=c("plot", "burntrt"), sep="_")%>%
  mutate(plot=as.numeric(plot))%>%
  mutate(site=ifelse(plot<21, "southern", ifelse(plot>40, "nortnern", "central")))
                  
adonis(wide_composition_rel ~ site+burntrt, data=wcrel2, perm=1e3) # nothing significant

### Question 1: Figure 2: RIVER PLOTS (2019 Only)####
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
  
### Question 2: Figure 4: Timeseries Riverplots
### Question 2: Setup timeseries dataset ####


timeseries1<-left_join(timeseries, plotkey)%>%
  gather(func, cover, 6:10)%>%
  mutate(func=as.factor(ifelse(func=="perag", "Annual grasses", ifelse(func=="perpg", "Perennial grasses", ifelse(func=="perl", "Litter", ifelse(func=="perf", "Forbs", "Bare ground"))))))%>%
  filter(!is.na(cover), cover!="x")%>%
  mutate(cover=as.numeric(cover))%>%
  filter(cover<100)%>%
  filter(plot<41&plot>20)%>%
  filter(!is.na(cover))
timeseries1$func<-factor(timeseries1$func, levels=c("Perennial grasses", "Annual grasses", "Forbs", "Litter", "Bare ground"))


timeseries2s<-timeseries1%>% #vegtog 2s: LRR of burned: not burned by fn group at the microplot level (just lots more replicates)
  group_by(plot, seedmix, func, burntrt, year)%>%
  filter(!is.na(cover))%>%
  summarize(cover=mean(cover))%>%
  ungroup()%>%
  mutate(burntrt=ifelse(burntrt=="u", "NotBurned", "Burned"))%>%
  mutate(id=paste(year, plot, burntrt, seedmix, sep="_"))%>%
  spread(func, cover)

colnames(timeseries2s)<-c("plot", "seedmix", "burntrt", "year", "id", "pg", "ag", "f", "del", "del2")
timeseries_rel<-timeseries2s%>%
  group_by(id)%>%
  mutate(totcov=sum(pg, ag, f))%>%
  mutate(pg=pg*100/totcov, ag=ag*100/totcov, f=f*100/totcov)%>%
  mutate(newtot=ag+pg+f)%>%
  dplyr::select(-totcov, -newtot)%>%
  ungroup()%>%
  dplyr::select(id, ag, pg, f)%>%
  filter(!is.na(ag))
rownames(timeseries_rel)<-timeseries_rel$id
timeseries_rel <- dplyr::select(timeseries_rel, -id)

#make bray-curtis dissimilarity matrix
wide_dist <- vegdist(timeseries_rel, na.rm=T)


x<-hclust(wide_dist, method="complete")

plot(x)

groups<-cutree(x, k=4)

timeseries3<-timeseries2s%>%
  filter(id%in%rownames(timeseries_rel))

wide_clusters<-cbind(timeseries3, groups)
wide_clusters$ID <- row.names(wide_clusters)#%>%
wide_clusters1<-wide_clusters%>%
  mutate(plot=as.numeric(plot))%>%
  mutate(id=paste(plot, seedmix))%>%
  mutate(groups=as.factor(groups))%>%
  mutate(groups=ifelse(groups==1, "even", ifelse(groups==2, "f", ifelse(groups==3, "ag", ifelse(groups==4, "pg", groups)))))%>%
  mutate(ID2=paste(plot, seedmix, burntrt))


#wide_clusters1$groups<-factor(wide_clusters1$groups, levels=c("more forbs", 
                                                              "forbs and perennials", 
                                                              "more annuals", "annuals dominant"))
wide_clusters1$burntrt<-factor(wide_clusters1$burntrt, levels=c("NotBurned", "Burned"))

cluster_char<-wide_clusters1%>%
  gather(fg, cover, ag, pg, f)

#transitions
transitions<-wide_clusters1%>%
  dplyr::select(plot, seedmix, burntrt, year, groups)%>%
  mutate(year=ifelse(year==2019, 'y2019', "y2020"))%>%
  spread(year, groups)%>%
  mutate(transitions=ifelse(y2019==y2020, 1, 0))%>%
  group_by(burntrt)%>%
  filter(!is.na(transitions))%>%
  summarize(transitions=sum(as.numeric(transitions)), total=n())

### recharacterize
ggplot(cluster_char, aes(x=as.factor(groups), y=cover))+geom_boxplot(aes(fill=fg))  +
  scale_fill_manual(labels=c("annual grasses", "forbs", "perennial grasses"), 
                    values=c("goldenrod2", "royalblue3", "chartreuse4"  ))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank())) +ylab("Central site seedplot % cover")

  
### Question 2: Timeseries riverplots ####
b <- ggplot(subset(wide_clusters1, burntrt=="Burned"), aes(x=as.factor(year), stratum=as.factor(groups), alluvium=id, 
                             fill=as.factor(groups), label=as.factor(groups)))+
    geom_flow()+
    scale_x_discrete(expand = c(.1, .1))+
    geom_stratum(alpha=.5)+
    geom_text(stat="stratum", size=3)+
    scale_fill_manual(values=c("royalblue3","chartreuse4", "tan", "goldenrod2"))+
    theme(legend.position = "none")+ggtitle("burned")
 nb<- ggplot(subset(wide_clusters1, burntrt=="NotBurned"), aes(x=as.factor(year), stratum=as.factor(groups), alluvium=id, 
                                                        fill=as.factor(groups), label=as.factor(groups)))+
    geom_flow()+
    scale_x_discrete(expand = c(.1, .1))+
    geom_stratum(alpha=.5)+
    geom_text(stat="stratum", size=3)+
    scale_fill_manual(values=c("royalblue3","chartreuse4", "tan", "goldenrod2"))+
    theme(legend.position = "none")+ggtitle("not burned")
  
ggarrange(b, nb)
 
control<- ggplot(subset(wide_clusters1, seedmix=="x"&burntrt=="Burned"), aes(x=as.factor(year), stratum=as.factor(groups), alluvium=ID2, 
                                                                   fill=as.factor(groups), label=as.factor(groups)))+
  geom_flow()+
  scale_x_discrete(expand = c(.1, .1))+
  geom_stratum(alpha=.5)+
  geom_text(stat="stratum", size=3)+
  scale_fill_manual(values=c("royalblue3","chartreuse4", "tan", "goldenrod2"))+
  theme(legend.position = "none")+ggtitle("not seeded")
pas<-ggplot(subset(wide_clusters1, seedmix=="p"&burntrt=="Burned"), aes(x=as.factor(year), stratum=as.factor(groups), alluvium=ID2, 
                                                      fill=as.factor(groups), label=as.factor(groups)))+
  geom_flow()+
  scale_x_discrete(expand = c(.1, .1))+
  geom_stratum(alpha=.5)+
  geom_text(stat="stratum", size=3)+
  scale_fill_manual(values=c("royalblue3","chartreuse4", "tan", "goldenrod2"))+
  theme(legend.position = "none")+ggtitle("pasture")
cali<-ggplot(subset(wide_clusters1, seedmix=="c"&burntrt=="Burned"), aes(x=as.factor(year), stratum=as.factor(groups),alluvium=ID2, 
                                                       fill=as.factor(groups), label=as.factor(groups)))+
  geom_flow()+
  scale_x_discrete(expand = c(.1, .1))+
  geom_stratum(alpha=.5)+
  geom_text(stat="stratum", size=3)+
  scale_fill_manual(values=c("royalblue3","chartreuse4", "tan", "goldenrod2"))+
  theme(legend.position = "none")+ggtitle("cali")
km<-ggplot(subset(wide_clusters1, seedmix=="km"&burntrt=="Burned"), aes(x=as.factor(year), stratum=as.factor(groups), alluvium=ID2, 
                                                     fill=as.factor(groups), label=as.factor(groups)))+
  geom_flow()+
  scale_x_discrete(expand = c(.1, .1))+
  geom_stratum(alpha=.5)+
  geom_text(stat="stratum", size=3)+
  scale_fill_manual(values=c("royalblue3","chartreuse4", "tan", "goldenrod2"))+
  theme(legend.position = "none")+ggtitle("koeleria")
dc<-ggplot(subset(wide_clusters1, seedmix=="dc"&burntrt=="Burned"), aes(x=as.factor(year), stratum=as.factor(groups), alluvium=ID2, 
                                                     fill=as.factor(groups), label=as.factor(groups)))+
  geom_flow()+
  scale_x_discrete(expand = c(.1, .1))+
  geom_stratum(alpha=.5)+
  geom_text(stat="stratum", size=3)+
  scale_fill_manual(values=c("royalblue3","chartreuse4", "tan", "goldenrod2"))+
  theme(legend.position = "none")+ggtitle("danthonia")
fr<-ggplot(subset(wide_clusters1, seedmix=="fr"&burntrt=="Burned"), aes(x=as.factor(year), stratum=as.factor(groups), alluvium=ID2, 
                                                     fill=as.factor(groups), label=as.factor(groups)))+
  geom_flow()+
  scale_x_discrete(expand = c(.1, .1))+
  geom_stratum(alpha=.5)+
  geom_text(stat="stratum", size=3)+
  scale_fill_manual(values=c("royalblue3","chartreuse4", "tan", "goldenrod2"))+
  theme(legend.position = "none")+ggtitle("roemers")
ggarrange(control, pas, cali, km, dc, fr)


# do a plot with panels for burned and unburned.  y is number of plots in each category x is seedmix
ggplot(subset(wide_clusters1, seedmix!="P"&seedmix!="p/c"&year==2020), aes(x=seedmix)) +geom_bar(position="stack", aes(fill=groups))+facet_grid(~burntrt)

#plot with just perennial component (LRR seedmix vs control)
wide_clusters2<-wide_clusters1%>%
  group_by(seedmix, burntrt, year)%>%
  filter(!is.na(pg))%>%
  summarize(pgse=calcSE(pg), meanpg=mean(pg))

ggplot(subset(wide_clusters2, seedmix!="P"&seedmix!="p/c"&seedmix!="f"&seedmix!="e"), aes(x=burntrt, y=meanpg)) +
  geom_line(aes(group=seedmix, color=seedmix), size=2)+
  geom_errorbar(aes(ymax=meanpg+pgse, ymin=meanpg-pgse, group=seedmix, color=seedmix), width=.1)+
  facet_wrap(~year)



### Q2: Contingency tables analysis####
library(vcd)
library(DescTools)
obs.table1<-matrix(c(6, 14, 10, 10, 9, 11), nrow=3, ncol=2, byrow=T)
colnames(obs.table1)<-c('transition', 'stable')
rownames(obs.table1)<-c('northern',"central", "southern")
chisq.test(obs.table1)
GTest(obs.table1)
mosaic(obs.table1, gp = shading_Friendly, gp_args = list(interpolate = function(x) pmin(x/4, 1)), split_vertical = TRUE, type="observed")
mosaic(obs.table2, gp = shading_hcl(obs.table3), split_vertical = TRUE, type="observed")

sub1a<-obs.table1[1:2,]
GTest(sub1a)

sub1b<-obs.table1[2:3,]
GTest(sub1b)

sub1c<-obs.table1[-2,]
GTest(sub1c)

pairwise.G.test(obs.table1, p.method="none")


obs.table2<-matrix(c(0, 18, 2, 3, 14, 3, 1, 14, 5), nrow=3, ncol=3, byrow=T)
colnames(obs.table2)<-c('annual', 'stable', 'perennial')
rownames(obs.table2)<-c('northern',"central", "southern")
chisq.test(obs.table2)
GTest(obs.table2)
f4b<-mosaic(obs.table2, gp = shading_Friendly, labeling=labeling_residuals, gp_args = list(interpolate = function(x) pmin(x/4, 1)), split_vertical = TRUE, main="Arthritis: [Treatment] [Improved]")

shading_Friendly2()ot2<-t(obs.table2)
pairwise.G.test(ot2, p.method="none")

pairwise.G.test(obs.table2, p.method="none")


obs.table3<-matrix(c(69, 87, 72, 85), nrow=2, ncol=2, byrow=T)
colnames(obs.table3)<-c('transition', 'stable')
rownames(obs.table3)<-c('burned', 'unburned')
chisq.test(obs.table3)
GTest(obs.table3)
f4c<-mosaic(obs.table3, gp = shading_Friendly, gp_args = list(interpolate = function(x) pmin(x/4, 1)), split_vertical = TRUE)

ggarrange(f4a, f4b, f4c)
 ### Question 2: Timeseries NMDS ####

vegtog3p<-vegtog2p%>%
  mutate(year=2019)%>%
  mutate(id=paste(year, plot, sep="_"))%>%
  dplyr::select(id, func, Burned, NotBurned)

timeseries2p<-timeseries1%>% #vegtog 2s: LRR of burned: not burned by fn group at the microplot level (just lots more replicates)
  group_by(plot, func, burntrt, year)%>%
  filter(!is.na(cover))%>%
  summarize(cover=mean(cover))%>%
  ungroup()%>%
  mutate(burntrt=ifelse(burntrt=="u", "NotBurned", "Burned"))%>%
  mutate(id=paste(year, plot,  sep="_"))%>%
  spread(burntrt, cover)%>%
  filter(year==2020)%>%
  dplyr::select(id,  func, Burned, NotBurned)

timeseries3p<-rbind(timeseries2p, vegtog3p)
  

wide_compositionTS<-timeseries3p%>%
  gather(burntrt, cover, Burned, NotBurned)%>%
  spread(func, cover, fill=0)%>%
  mutate(id=paste(as.character(id), burntrt, sep="_"))%>%
  dplyr::select(1, 3, 4, 5)%>%
  filter(id!="2019_51_NotBurned")
colnames(wide_compositionTS)<-c("id", "pg", "ag", "f")
wide_composition_relTS<-wide_compositionTS%>%
  group_by(id)%>%
  mutate(totcov=sum(pg, ag, f))%>%
  mutate(pg=pg*100/totcov, ag=ag*100/totcov, f=f*100/totcov)%>%
  mutate(newtot=ag+pg+f)%>%
  dplyr::select(-totcov, -newtot)%>%
  ungroup()
rownames(wide_composition_relTS)<-wide_composition_relTS$id
wide_composition_relTS<-dplyr::select(wide_composition_relTS, -1)


# run the NMDS
plotspecNMDS <- metaMDS(wide_composition_relTS, scale=T)
#base r plot
plot(plotspecNMDS)

# Extract and format axis scores
data.scores <- as.data.frame(scores(plotspecNMDS, display=c("sites")))
data.scores$ID <- row.names(wide_composition_relTS)

data.scores <- as_tibble(data.scores) %>%
  separate(ID, c("year", "plot", "burntrt"), sep="_") %>%
  mutate(plot=as.numeric(plot)) %>%
  mutate(site=ifelse(plot<21, "southern", "central"))%>%
  mutate(site=ifelse(plot>40, "northern", site))
#data.scores<-left_join(data.scores, dplyr::select(wide_clusters1, plot, groups))

#data.scores <- right_join(plotkey, data.scores)

# Extract and format species scores
species.scores <- as.data.frame(scores(plotspecNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)%>%
  mutate(species=ifelse(species=="ag", "annual grasses", ifelse(species=="pg", "perennial grasses", "forbs")))

site.year.burn.mean=aggregate(data.scores[,c("NMDS1", "NMDS2")], 
                         list(group = interaction(data.scores$site, data.scores$burntrt, data.scores$year)), mean)

arrows<-site.year.burn.mean%>%
  separate(group, into = c("site", "burntrt", "year"))%>%
  gather("id", "val", NMDS1, NMDS2)%>%
  mutate(id=ifelse(id=="NMDS1", "x", "y"))%>%
  mutate(id=paste(id, burntrt, sep="_"))%>%
  dplyr::select(-burntrt)%>%
  spread(id, val)
arrows2<-arrows%>%
  filter(site=="central")%>%
  gather(id, val, )
### Pub Figure 3: NMDS/site by burntrt (2019)

ggplot() +
  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=interaction(burntrt,year), color=site), size=2) + # add the point markers
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("gray50", "gray80", "black")) +
  scale_shape_manual(values=c(1, 16, 2, 17))+
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=4) +
  coord_equal() +
  theme_bw()+
  #annotate("text",x = site.burn.mean$NMDS1,y = site.burn.mean$NMDS2,label=site.burn.mean$group)+
  geom_segment(aes(x=arrows$x_NotBurned, y=arrows$y_NotBurned, 
                   xend=arrows$x_Burned, yend=arrows$y_Burned), 
               arrow=arrow(length=unit(.3, "cm")), color="red3", size=2)+
  geom_segment(aes(x=0.05216261, y=-0.08951118, 
                   xend=0.14580061, yend=-0.03681127), 
               arrow=arrow(length=unit(.3, "cm")), color="blue3", size=2)#+
  geom_segment(aes(x=0.01681522, y=0.06173924, 
                   xend=0.14580061, yend=-0.03681127), 
               arrow=arrow(length=unit(.3, "cm")), color="blue3", size=2)+
  geom_segment(aes(x=-0.08152399, y=-0.01860995, 
                   xend=0.05216261, yend=-0.08951118), 
               arrow=arrow(length=unit(.3, "cm")), color="blue3", size=2)

######################################
#zoom in on central site only
arrows3<-subset(arrows, site=="central")
ggplot() +
  # add the species labels
  geom_point(data=subset(data.scores, site=="central"),aes(x=NMDS1,y=NMDS2,shape=burntrt,color=year), size=2) + # add the point markers
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("gray60", "black")) +
  scale_shape_manual(values=c(16, 1))+
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=5) +
  coord_equal() +
  theme_bw()+
  #annotate("text",x = site.burn.mean$NMDS1,y = site.burn.mean$NMDS2,label=site.burn.mean$group)+
  geom_segment(aes(x=arrows3$x_NotBurned, y=arrows3$y_NotBurned, 
                   xend=arrows3$x_Burned, yend=arrows3$y_Burned), 
               arrow=arrow(length=unit(.3, "cm")), color="red3", size=2)+
  geom_segment(aes(x=0.01681522, y=0.06173924, 
                   xend=0.14580061, yend=-0.03681127), 
               arrow=arrow(length=unit(.3, "cm")), color="blue3", size=1)+
  geom_segment(aes(x=-0.08152399, y=-0.01860995, 
                   xend=0.05216261, yend=-0.08951118), 
               arrow=arrow(length=unit(.3, "cm")), color="blue3", size=1)+xlim(c(-.65, .5))+theme(text=element_text(size=15), legend.title = element_blank())
