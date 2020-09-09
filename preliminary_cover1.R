library(tidyverse)

##FN for Calculating SE
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

### READ IN AND CLEAN THE DATA ###
test<-read_csv("/Users/alejandrobrambila/Documents/Repositories/HOPS/hops/PI_DATA_2018-04-06.csv")

funcdat0 <- read_csv("/Users/alejandrobrambila/Documents/Repositories/HOPS/hops/PI_DATA_2018-04-06.csv")[1:3,] %>%
  select(-c(X1:X4)) 




funcdat <- as_tibble(t(funcdat0)) 
names(funcdat) = c("growth", "func", "species")

sppdat0 <- read_csv("/Users/alejandrobrambila/Documents/Repositories/HOPS/hops/PI_DATA_2018-04-06.csv", skip = 3) %>%
  mutate(Site = factor(Site, levels = c("Southern", "Central", "Northern"))) %>%
  gather("species", "cover", 5:107) %>%
  mutate(cover = parse_number(cover)) %>%
  mutate(treatment = `Climate Treatment`) %>%
  filter(!species%in% c("Total Cover", "Unknown sample 12", "Unknown sample 6 (forb)")) %>%
  group_by(species) %>%
  mutate(maxcover = max(cover))%>%
#  mutate(treatment=ifelse(treatment=="Drought", "Control", ifelse(treatment=="Warming&ppt", "Warming", treatment)))%>%
  mutate(year=as.factor("2017"))

sppdat <- left_join(sppdat0, funcdat)

#add 2018 data
sppdat2018east <- read_csv("/Users/alejandrobrambila/Documents/Repositories/HOPS/hops/2018_east.csv")%>%
  mutate(Site = factor(site, levels = c("Southern", "Central", "Northern"))) %>%
  gather("species", "cover", 8:120)

sppdat2018east[is.na(sppdat2018east)]<-0

sppdat2018east <- sppdat2018east%>%
  mutate(cover=as.numeric(cover))%>%
  mutate(cover=cover/hits)%>%
  mutate(year=as.factor(Year))

sppdatE<-left_join(sppdat2018east, funcdat)%>%
  group_by(species) %>%
  mutate(maxcover = max(cover))%>%
  mutate(treatment=ifelse(treatment=="Drought", "Control", ifelse(treatment=="Warming&ppt", "Warming", treatment)))%>%
  filter(!is.na(growth))%>%
  select(-Site, -side, -hits)%>%
  mutate(year=as.factor(year))%>%
  select(-Year)%>%
  mutate(cover=cover*100)

names(sppdat) <- c("plot", "experiment", "site", "climate treatment", "species", "cover", "treatment", "maxcover", "year", "growth", "func")
names(sppdatE) <- c("plot", "experiment", "site", "treatment", "species", "cover","year", "growth", "func", "maxcover")

sppdat<-rbind(sppdat, sppdatE)

annualgrasses<-sppdat%>%
  filter(growth=="Annual", func=="Grass")
perennialgrasses<-sppdat%>%
  filter(growth=="Perennial", func=="Grass")



### AGGREGATE COVER ###

tog <- left_join(sppdat, funcdat) %>%
  group_by(plot, year, site, func, growth) %>%
  summarize(totcover = sum(cover)) %>% 
  tbl_df() %>%
  mutate(site = factor(site, levels = c("Northern", "Central", "Southern"))) %>%
  filter(!is.na(growth))
write_csv(tog, "prelimcov.csv")

togspp <- left_join(sppdat, funcdat)%>%
  mutate(site = factor(site, levels = c("Northern", "Central", "Southern"))) %>%
  filter(!is.na(treatment), !is.na(growth))%>%
  filter(cover!=0)

pa <-togspp%>%
  mutate(cover=ifelse(cover==0, 0, 1))%>%
  filter(cover!=0)%>%
  group_by(site, experiment, treatment, species, year)%>%
  summarize(numplots=sum(cover))

### ANOVA ###
library(lme4)
library(emmeans)
library(multcomp)
centralGrasses<-tog%>%
  filter(experiment=="HOPS"&site=="Central"&func=="Grass")%>%
  mutate(plot=as.factor(plot), year=as.factor(year)) %>%
  mutate(growth=as.character(growth), treatment=as.character(treatment))%>%
  mutate(trt=as.factor(paste(growth, treatment, sep="")))
model=lmer(totcover~trt + (1|year) + (1|plot), data=centralGrasses)
summary(model)
summary(glht(model, mcp(trt="Tukey")))



ggplot(tog2, aes(x=interaction(func, growth), y = totcover)) + 
  geom_boxplot() + 
  facet_grid(interaction(site, experiment) ~  interaction(treatment, year))

ggplot(subset(tog, experiment=="HOPS"), aes(x=interaction(treatment, year), y = totcover)) + 
  geom_boxplot() + 
  facet_grid(site ~ interaction(func, growth)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("functional group response")

ggplot(subset(sppdat, species=="Achillea millefolium"|
                species=="Eriophyllum lanatum"|
                species=="Plectritis congesta"|
                species=="Prunella vulgaris"), aes(x=species, y = log(cover))) + 
  geom_boxplot() + 
  facet_grid(interaction(site, experiment) ~  interaction(treatment, year), scales="free")

pa2<-pa%>%
  group_by(site, experiment, treatment, species)%>%
  summarize(numplots=sum(numplots))%>%
  group_by(species)%>%
  mutate(maxplots=max(numplots))%>%
  filter(maxplots>6)%>%
  group_by(treatment)%>%
  desc(maxplots)
pa2<-left_join(pa2, funcdat)

ggplot(subset(pa2, (species=="Achillea millefolium"|
                species=="Eriophyllum lanatum"|
                species=="Plectritis congesta"|
                species=="Prunella vulgaris")&(treatment=="Control"|treatment=="Drought")), aes(x=species, y = numplots)) + 
  geom_bar(stat="identity") + 
  facet_grid(interaction(site, experiment) ~  interaction(treatment))

ggplot(subset(pa2, treatment=="Control"|treatment=="Drought"), aes(x=reorder(species, -numplots), y = numplots)) + 
  geom_bar(stat="identity", aes(fill=interaction(growth, func))) + 
  facet_grid(interaction(site, experiment) ~  interaction(treatment))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(subset(pa2, species=="Poa compressa"|
                species=="Schedonorus arundinaceus"|species=="Vulpia spp."|
                species=="Agrostis capillaris"), aes(x=reorder(species, -numplots), y = numplots)) + 
  geom_bar(stat="identity") + 
  facet_grid(interaction(site, experiment) ~  interaction(treatment))


### TREND ###
plottog<-tog%>%
group_by(year, plot, site, experiment, treatment, func, growth) %>%
  summarize(cover=sum(totcover), secover=calcSE(totcover))%>%
  ungroup()%>%
  mutate(group=paste(treatment, experiment, sep="_"))%>%
  mutate(year=as.numeric(year))

meantog <- tog%>%
  group_by(year, site, experiment, treatment, func, growth) %>%
  summarize(meancover=mean(totcover), secover=calcSE(totcover))%>%
  ungroup()%>%
  mutate(group=paste(treatment, experiment, sep="_"))%>%
  mutate(year=as.numeric(year))
  
###### FIGS PROPOSAL #####  no pasture


proptog<-tog%>%
  filter(func=="Grass")%>%
  select(-func)%>%
  spread(growth, totcover)%>%
  mutate(paRatio=(Perennial+1)/(Annual+1))
ggplot(subset(tog, func=="Grass"&experiment=="HOPS"&treatment=="Control"), aes(x=growth, y = totcover)) + 
  geom_boxplot(aes(fill=growth)) + 
  xlab("")+ylab("% Cover")+
  theme_bw(base_size=22)+
  scale_fill_manual(values=c("pink", "lightblue"))+
  facet_wrap(~site)+
  theme(legend.position="none",axis.text.x=element_blank(), axis.ticks.x=element_blank())+ggtitle("a) Control plots by site")

centralTreatPlot<-ggplot(subset(tog, func=="Grass"&experiment=="HOPS"&site=="Central"), aes(x=(treatment), y = totcover)) + 
  geom_boxplot(aes(fill=growth)) +
  xlab("")+ylab("")+
  facet_wrap(~site)+
  theme_bw(base_size=22)+
  theme(legend.title=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  scale_fill_manual(values=c("pink", "lightblue"))+
  ggtitle("b) Central site heat effect")


library(gridExtra)
grid.arrange(plotNCS, centralTreatPlot, nrow=1)

ggplot(subset(proptog, experiment=="HOPS"&site=="Central"), aes(x=treatment, y = log(paRatio))) + 
  geom_boxplot(aes(fill=treatment)) + 
  # geom_errorbar(aes(ymin=meancover-secover, ymax=meancover+secover, color=treatment), width=.05)  +
  xlab("Heat Legacy Treatment")+ylab("P:A Ratio")

#with pasture
ggplot(subset(meantog, func=="Grass"), aes(x=(year), y = meancover)) + 
  geom_line(aes(color=group)) + 
  geom_errorbar(aes(ymin=meancover-secover, ymax=meancover+secover, color=group), width=.05) +
  facet_grid(interaction(func, growth)~site) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("functional group trend")

#just central
ggplot(subset(meantog, func=="Grass"&experiment=="HOPS"&site=="Central"), aes(x=(year), y = meancover)) + 
  geom_line(aes(color=treatment)) + 
  geom_errorbar(aes(ymin=meancover-secover, ymax=meancover+secover, color=treatment), width=.05) +
  facet_grid(~interaction(func, growth)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("functional group trend")


### SPECIES LEVEL RESPONSES ###

## Perennial grasses ##
ggplot(subset(sppdat, func == "Grass" & growth == "Perennial" & maxcover > 15), aes(x=Site, y=cover)) +geom_boxplot() +
  facet_wrap(~species) + ggtitle("Common perennial grasses")


## Focal species driving the patterns ##
# Southern pattern driven 

ggplot(subset(sppdat,func == "Grass" & growth == "Perennial" & maxcover > 20), aes(x=treatment, y=cover)) +geom_boxplot() +
  facet_grid(Site~species, scales = "free")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ggplot(subset(sppdat, species == "Schedonorus arundinaceus"), aes(x=treatment, y=cover)) +geom_boxplot() +
#   facet_wrap(~Site)
# 
# ggplot(subset(sppdat, species == "Agrostis capillaris"), aes(x=treatment, y=cover)) +geom_boxplot() +
#   facet_wrap(~Site)
# 
# ggplot(subset(sppdat, species == "Alopecurus pratensis"), aes(x=treatment, y=cover)) +geom_boxplot() +
#   facet_wrap(~Site)



## Elymus doesn't care 
# ggplot(subset(sppdat, species == "Elymus repens"), aes(x=treatment, y=cover)) +geom_boxplot() +
#   facet_wrap(~Site)


## Perennial forbs ##

ggplot(subset(sppdat, func == "Forb" & growth == "Perennial" & maxcover > 15), aes(x=Site, y=cover)) +geom_boxplot() +
  facet_wrap(~species)

ggplot(subset(sppdat,func == "Forb" & growth == "Perennial" & maxcover > 5), aes(x=treatment, y=cover)) +geom_boxplot() +
  facet_grid(Site~species, scales = "free")



## Annual forbs ##
# not much doing, galium doesn't love drought 

ggplot(subset(sppdat,func == "Forb" & growth == "Annual" & maxcover > 20), aes(x=treatment, y=cover)) +geom_boxplot() +
  facet_grid(Site~species, scales = "free")


## Annual grasses ##

ggplot(subset(sppdat,func == "Grass" & growth == "Annual" & maxcover > 20), aes(x=treatment, y=cover)) +geom_boxplot() +
  facet_grid(Site~species, scales = "free")

ggplot(subset(sppdat, species == "Bromus tectorum"), aes(x=treatment, y=cover)) +geom_boxplot() +
  facet_grid(~Site, scales = "free")



