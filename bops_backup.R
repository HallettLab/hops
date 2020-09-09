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
  select(1:5, 9:13, 41)%>%
  mutate(perhyp=NA)
vegplot2020_j<-vegplot2020%>%
  select(1:11, 34)%>%
  mutate(talll=lheight)%>%
  select(-lheight)
timeseries<-rbind(vegplot_j, vegplot2020_j) # dat for timeseries
rm(vegplot_j, vegplot2020)

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
  dplyr::select(1:5, 9:12, 14:40)%>%
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
  spread(burntrt, survival)%>%
  mutate(LRR=log(burned+1/notburned+1))


  
# visualize totals seedlings by site/treatment
#ggplot(subset(vegtog_counts), 
#       aes(x=interaction(bopsclim, burntrt), y=log(count), fill=interaction(burntrt,bopsclim)))+ 
#  geom_boxplot() +facet_grid(seedmix~site, scales="free") +
#  theme(axis.title.x=element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank())

# visualize % survival by species, site, burn, warm

#NATIVE PERENNIAL GRASSES
ggplot(subset(vegtog_survival, func=="Native Perennial Grasses"|func=="Pasture Grasses"), aes(x=burntrt, y=zsurvival, fill=burntrt))+
  geom_boxplot() + facet_grid(func~site, scales="free") +
  scale_fill_manual(labels=c("Unburned", "Burned", "Unburned, Warmed", "Burned, Warmed"), 
                     values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+ geom_hline(yintercept=0)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("Species Scaled Survival")+ggtitle("Native Perennial Grasses")

ggplot(subset(vegtog_survival_spread, longevity=="p"&type=="g"&(native=="n"|native=="i")), aes(x=sciname, y=LRR, fill=site))+
  geom_boxplot()  +
  scale_fill_manual(labels=c("Northern", "Central", "Southern"), 
                    values=c("green3", "yellowgreen", "burlywood1"))+
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("Survival LRR Burned/Not Burned")+ggtitle("Native Perennial Grasses")

# CALIFORNIA PERENNIAL GRASSES
ggplot(subset(vegtog_survival, longevity=="p"&type=="g"&native=="c"&species!="stipa"), aes(x=interaction(bopsclim, burntrt), y=log(survival+1), fill=interaction(burntrt, bopsclim)))+
  geom_boxplot() + facet_grid(sciname~site, scales="free") +
  scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed"), 
                    values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(% Survival+1)")+ggtitle("California Perennial Grasses")

# PASTURE GRASSES
ggplot(subset(vegtog_survival, longevity=="p"&type=="g"&native=="i"), aes(x=interaction(bopsclim, burntrt), y=log(survival+1), fill=interaction(burntrt, bopsclim)))+
  geom_boxplot() + facet_grid(sciname~site, scales="free") +
  scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
                    values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(% Survival+1)")+ggtitle("Pasture Grasses")

# FORBS
 ggplot(subset(vegtog_survival, longevity=="a"&type=="f"&(species!="drymocallis"&species!="lomatiumu")), aes(x=interaction(bopsclim, burntrt), y=log(survival+1), fill=interaction(burntrt, bopsclim)))+
  geom_boxplot() + facet_grid(sciname~site, scales="free") +
   theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(% Survival+1)")+
   scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
                     values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+ggtitle("Forbs")

#DANCALCAL vs DANCALOR (about the same)
 ggplot(subset(vegtog_survival, longevity=="p"&type=="g"&(species=="dancalcal"|species=="dancalor")), aes(x=interaction(bopsclim, burntrt), y=log(survival+1), fill=interaction(burntrt, bopsclim)))+
   geom_boxplot() + facet_grid(sciname~site, scales="free") +
   scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
                     values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+
   theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(% Survival+1)")+ggtitle("Native vs. California Danthonia")
 
#FESROE vs FESIDA (about the same, but FESIDA better in SOR)
 ggplot(subset(vegtog_survival, longevity=="p"&type=="g"&(species=="fesroe"|species=="fesid")), aes(x=interaction(bopsclim, burntrt), y=log(survival+1), fill=interaction(burntrt, bopsclim)))+
   geom_boxplot() + facet_grid(sciname~site, scales="free") +
   scale_fill_manual(labels=c("Unburned, Ambient", "Burned, Ambient", "Unburned, Warmed", "Burned, Warmed"), 
                     values=c("dodgerblue", "brown2", "chartreuse3", "orange"))+
   theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(), legend.title=(element_blank()))+ylab("log(% Survival+1)")+ggtitle("Native Roemer's vs. California Idaho Fescue")
 
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
   
ggplot(subset(eug.shannon, metric!="richness"), aes(x=burntrt, y=value))+
  geom_jitter(size=.5, color="grey")+
  geom_boxplot(aes(fill=burntrt))+  facet_grid(metric~site, scales="free")+
  scale_fill_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2"))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.title=(element_blank()))
 
### PART 3: LITTER AS DRIVER #####  
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
vegtog_survival_agg<-vegtog_survival%>%
  filter(longevity=="p"&type=="g")%>%
  group_by(plot, sub, native, longevity, type, bopsclim, burntrt, site, perag, talll, perl)%>%
  summarize(survival=mean(survival)) %>%
  ungroup()%>%
  mutate(native=ifelse(native=="n", "Native Perennial", ifelse(native=="i", "Pasture Grasses", "California Perennials")))


agPLOT<-ggplot(subset(vegtog_survival_agg, longevity=="p"&type=="g"&native!="California Perennials"), aes(x=perag, y=log(survival+1)))+ 
  geom_jitter(aes(color=burntrt)) +facet_grid(native~site, scales="free")  +
  geom_smooth(method="lm", color="black", se=F) + xlab("Percent annual grass")+
  scale_color_manual(labels=c("Unburned", "Burned"), 
                     values=c("dodgerblue", "brown2"))+theme(axis.text.x=element_blank(),
                                                             axis.ticks.x=element_blank(), 
                                                             legend.title=(element_blank()))

lvPLOT<-ggplot(subset(vegtog_survival_agg, longevity=="p"&type=="g"&native!="California Perennials"), aes(x=perl*talll, y=log(survival+1)))+ 
  geom_jitter(aes(color=burntrt)) +facet_grid(native~site, scales="free")  +
  geom_smooth(method="lm", formula=(y)~(x), color="black", se=F) + xlab("Litter volume (cover*depth)")+
  scale_color_manual(labels=c("Unburned", "Burned"), 
                    values=c("dodgerblue", "brown2"))+theme(axis.text.x=element_blank(),
                                                           axis.ticks.x=element_blank(), 
                                                           legend.title=(element_blank()))+labs(y="")

lbPLOT<-ggplot(vegtog_survival_bm, aes(x=litter_g, y=log(survival+1)))+ 
  geom_jitter(aes(color=burntrt)) +facet_grid(site~sciname, scales="free")  +
  geom_smooth(method="lm", color="black", se=F) + xlab("grams litter") +
  scale_color_manual(labels=c("Unburned", "Burned"), 
                     values=c("dodgerblue", "brown2"))+
  scale_color_manual(labels=c("Unburned", "Burned"), 
                     values=c("dodgerblue", "brown2"))+theme(
                                                             axis.ticks.x=element_blank(), 
                                                             legend.title=(element_blank()))+  labs(x="Grams litter", y="log(% Survival+1)")
  
ggarrange(agPLOT, lvPLOT, common.legend = T, legend = "bottom")

 
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
#make bray-curtis dissimilarity matrix
wide_dist <- vegdist(wide_composition_rel)


x<-hclust(wide_dist, method="complete")

plot(x)

groups<-cutree(x, k=4)

wide_clusters<-cbind(wide_composition, groups)
wide_clusters$ID <- row.names(wide_clusters)#%>%
wide_clusters1<-wide_clusters%>%
  separate(ID, c("plot", "burntrt"))%>%
  #dplyr::select(plot, burntrt, 2, 3, 4, 5)%>%
  mutate(plot=as.numeric(plot))%>%
  mutate(site=ifelse(plot<21, "southern", "central"))%>%
  mutate(site=ifelse(plot>40, "northern", site))%>%
  mutate(groups=as.factor(groups))

wide_clusters1$site<-factor(wide_clusters1$site, levels=c("southern", 
                                          "central", 
                                          "northern"))

#clusters by site
ggplot(wide_clusters1, aes(x=burntrt, y=site)) +geom_jitter(aes(color=groups), width=.15)


  


### Question 1: Figure 1: Cluster Characterization (2019 Only) ####

### characterize clusters
cluster_char<-wide_clusters1%>%
  gather(fg, cover, ag, pg, f)%>%
  group_by(groups, fg)

ggplot(cluster_char, aes(x=groups, y=cover))+geom_boxplot(aes(fill=fg))


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
data.scores<-left_join(data.scores, dplyr::select(wide_clusters1, plot, groups))

#data.scores <- right_join(plotkey, data.scores)

# Extract and format species scores
species.scores <- as.data.frame(scores(plotspecNMDS, display=c("species")))
species.scores$species <- row.names(species.scores)
species.scores <- as_tibble(species.scores)

site.burn.mean=aggregate(data.scores[,c("NMDS1", "NMDS2")], 
                    list(group = interaction(data.scores$site, data.scores$burntrt)), mean)
cluster.burn.mean
NMDS.mean=aggregate(dune.NMDS.data[,c("NMDS1", "NMDS2")], 
                    list(group = dune.NMDS.data$Management), mean)

# NMDS of sites/burntrt
ggplot() +
  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=interaction(burntrt), color=interaction(site)),size=3) + # add the point markers
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  #  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=9) +
  coord_equal() +
  theme_bw()+
  annotate("text",x = site.burn.mean$NMDS1,y = site.burn.mean$NMDS2,label=site.burn.mean$group)



# NMDS of clusters/burntrt
ggplot() +
  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=burntrt, color=groups)) + # add the point markers
  #  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  #  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species), size=9) +
  coord_equal() +
  theme_bw()

### Question 1: Figure 2: RIVER PLOTS (2019 Only)####
library(riverplot)

rivplot<-wide_clusters1%>%
  dplyr::select(5:8)%>%
  mutate(groups=ifelse(groups=="1", "more_ag", ifelse(groups=="2", "ag", ifelse(groups=="3", "more_f", "f_pg"))))%>%
  spread(burntrt, groups)

  

# you'll have your own state labels
states <- c("more_ag", "ag", "more_f", "f_pg")


# create a node object
nodes_mine<-data.frame(ID=paste0(rep(states, each=2), rep(1:2, 4)), x=rep(1:2, 4))

#tablulate edge weights (transition counts) somehow
trans1_mine<-table(paste(rivplot$NotBurned, rivplot$Burned))

# starting and ending nodes for each edge (flow)
N1 <- paste0(rep(states,each=4),1)
N2 <- paste0(rep(states,4),2)
value=as.vector(trans1_mine)
value<-c(2, 1, 4, 0, 7, 10, 3, 1, 1, 1, 9, 3, 1, 0, 7, 10)
# create edge object
edges <- data.frame(N1 = N1, N2 = N2, Value=value)

# this is key, you want a riverplot object, this makes it for you given
# an edges and nodes data.frame.
#?makeRiver


Dat2 <- makeRiver(nodes_mine, edges)
plot(Dat2)



### Question 2: Figure 4: Timeseries Riverplots