### The purpose of this script is to apply a trait framework to the HOPS 
### project.  Traits measured include height, seed mass, and SLA.  Analysis 
### consists of:
### 1. Intra-species trait response to treatment 
###    a. Relative variability of response traits
###    b. Variability predicted by maximum northern range?
###    c. Relationship of intra-inter species variability
### 2. Community level response to treatments.
###    a. Species means shift in a predictable direction?
###    b. Species means predict shifts in abundance?
###    c. Community-level trait responses.


##Load Libraries
library(tidyverse)
library(ggplot2)
library(vegan)
library(ade4)
library(scatterplot3d)

##FN for Calculating SE
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}


#################
### LOAD DATA ###
#################

# Plot level data
plot_details <- read_csv("plot_details.csv") %>%
  select(-plot_comments, -experiment)
plot_details$plot<-as.character(plot_details$plot)

# Leaf trait data (by individual)
leaf_traits <- read_csv("leaf_traits.csv") %>%
  select(species, plot, leafid, wet, dry, bladearea)
# Parse dry weight as numeric (reading as character)
leaf_traits$dry<-as.numeric(leaf_traits$dry)
# Calculate SLA
leaf_traits <- leaf_traits %>%
  mutate(sla=bladearea/dry) %>%
  filter(sla!=0, sla!=Inf, !is.na(sla))
# Join leaf traits to plot data
leaf_traits <- left_join(leaf_traits, plot_details) %>%
  filter(!is.na(dry), !is.na(bladearea)) %>%
  select(-leafid)
# Calculate SLA means/error by species
leafbyspec <- leaf_traits %>%
  filter(!is.na(treatment)) %>%
  mutate(site = factor(site, levels = c("Southern", "Central", "Northern"))) %>%
  group_by(species, site, treatment) %>%
  summarize(meansla=mean(sla), errorsla=calcSE(sla))

# Plant height data
#by individual
plant_heights <- read_csv("plant_heights.csv") %>%
  select(-Date_Measured) %>%
  mutate(Species=ifelse(Species=="Achyrachaena mollis", "ACHMOL", ifelse(Species=="Alopecurus pratensis", "ALOPRA", ifelse(Species=="Bromus hordaceus", "BROHOR", 
                                                                                                                           ifelse(Species=="Collinsia grandiflora", "COLGRA", ifelse(Species=="Plagiobothrys nothofulvus", "PLANOT", ifelse(Species=="Plectritis congesta", "PLECON", 
                                                                                                                                                                                                                                            ifelse(Species=="Poa compressa", "POACOM", ifelse(Species=="Schedonorus arundinaceus", "SCHARU", ifelse(Species=="Sidalcea malviflora", "SIDMAL", 
                                                                                                                                                                                                                                                                                                                                                    ifelse(Species=="Trifolium subterraneum", "TRISUB", ifelse(Species=="Vulpia spp.", "VULPIA", ifelse(Species=="Sidalcea malviflora", "SIDMAL", Species)))))))))))))
names(plant_heights)<-c("species", "site", "plot", "Height_mm")
# treatment avgs
plant_heights <- left_join(plant_heights, plot_details)
heightbyspec <- plant_heights%>%
  filter(!is.na(treatment)) %>%
  mutate(site = factor(site, levels = c("Southern", "Central", "Northern"))) %>%
  group_by(species, site, treatment) %>%
  summarize(meanheight=mean(Height_mm), errorheight=calcSE(Height_mm))

# Seed mass data by species
seedbyspec <- read_csv("seed_mass.csv")
names(seedbyspec)<-c("fullname", "species", "seedperpound")

#Read in functional group data
funcdat0 <- read_csv("PI_DATA_2018-04-06.csv")[1:3,] %>%
  select(-c(X1:X4)) 
funcdat <- as_tibble(t(funcdat0)) 
names(funcdat) = c("growth", "func", "species")
rm(funcdat0)

# Percent cover 
cover <- read_csv("PI_DATA_2018-04-06.csv", skip = 3) %>%
  mutate(Site = factor(Site, levels = c("Southern", "Central", "Northern"))) %>%
  gather("species", "cover", 5:107) %>%
  mutate(cover = parse_number(cover)) %>%
  mutate(treatment = `Climate Treatment`) %>%
  filter(!species%in% c("Total Cover", "Unknown sample 12", "Unknown sample 6 (forb)")) %>%
  group_by(species) %>%
  mutate(maxcover = max(cover))

###################################################
### 1a. Relative variability of response traits ###
###################################################

### STEP 1: calculate the SE of each trait for each species  (across all treatments)
aggregated_height <- plant_heights %>%
  group_by(species) %>%
  summarize(errorheight=calcSE(Height_mm)) %>%
  mutate(trait="Height")
names(aggregated_height) <- c("species", "Height", "trait")
aggregated_leaves <-leaf_traits %>%
  group_by(species) %>%
  filter(!is.na(treatment)) %>%
  filter(sla !=0, species!="PLECON") %>%
  summarize(SLA=calcSE(sla)) %>%
  mutate(trait="SLA")
seedbyspec<-seedbyspec %>%
  mutate(trait="Seed/Lb")


trait_var <- full_join(aggregated_height, aggregated_leaves)
trait_var <- full_join(trait_var, seedbyspec)
trait_var <- trait_var %>%
  mutate("Seed/Lb"=0) %>%
  select(Height, SLA, "Seed/Lb")
trait_var<-gather(trait_var, "trait", "traitval") %>%
  filter(!is.na(traitval)) %>%
  mutate(trait = factor(trait, levels = c("Height", "SLA", "Seed/Lb")))
seedbyspec<-seedbyspec %>%
  select(species, "Seed/Lb")
rm(aggregated_height, aggregated_leaves)

### STEP 2: boxplot (x axis=trait, y axis = SE of trait)
ggplot(trait_var, aes(trait, traitval)) +geom_boxplot()+
  xlab("Trait") + ylab("Standard Error") +ggtitle("Trait variability (SEs of species means)")

############################################################
### 1b. Variability predicted by maximum northern range? ###
############################################################

### COLGRA (HNL), SIDMAL (INL), ACHMOL (LNL)
### For each category of species, look at how mean/SE of height/SLA 
### changes by treatment (site and location)


## HNL (COLGRA as proxy)
# SLA Variation
ggplot(subset(leafbyspec, species=="COLGRA"), aes(treatment, meansla)) + geom_bar(stat="identity") +facet_wrap(~site) +
  geom_errorbar(aes(ymin=(meansla-errorsla), ymax=(meansla+errorsla)))
#Height Variation
ggplot(subset(heightbyspec, species=="COLGRA"), aes(treatment, meanheight)) + geom_bar(stat="identity") +facet_wrap(~site) +
  geom_errorbar(aes(ymin=(meanheight-errorheight), ymax=(meanheight+errorheight)))

##INL (SIDMAL as proxy)
# SLA Variation
ggplot(subset(leafbyspec, species=="SIDMAL"), aes(treatment, meansla)) + geom_bar(stat="identity") +facet_wrap(~site) +
  geom_errorbar(aes(ymin=(meansla-errorsla), ymax=(meansla+errorsla)))
#Height Variation
ggplot(subset(heightbyspec, species=="SIDMAL"), aes(treatment, meanheight)) + geom_bar(stat="identity") +facet_wrap(~site) +
  geom_errorbar(aes(ymin=(meanheight-errorheight), ymax=(meanheight+errorheight)))

##LNL (ACHMOL as proxy) 
# SLA Variation -- NO DATA !!!
ggplot(subset(leafbyspec, species=="ACHMOL"), aes(treatment, meansla)) + geom_bar(stat="identity") +facet_wrap(~site) +
  geom_errorbar(aes(ymin=(meansla-errorsla), ymax=(meansla+errorsla)))
#Height Variation
ggplot(subset(heightbyspec, species=="ACHMOL"), aes(treatment, meanheight)) + geom_bar(stat="identity") +facet_wrap(~site) +
  geom_errorbar(aes(ymin=(meanheight-errorheight), ymax=(meanheight+errorheight)))

#########################################################
### 1c. Relationship of intra-inter species variation ###
#########################################################
### Method 1: Aggregate mean/SE of each trait by species.  


# Calculate and visualize mean and SE of SLA
sla_variation <- leaf_traits %>%
  filter(!is.na(treatment))  %>%
  group_by(species) %>%
  summarize(meanarea=mean(bladearea), meansla=mean(sla), errorsla=calcSE(sla))

ggplot(sla_variation, aes(species, meansla)) + geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=(meansla-errorsla), ymax=(meansla+errorsla)))

ggplot(leaf_traits, aes(species, sla)) +geom_boxplot()

#Intraspecies SLA Variation
ggplot(subset(leaf_traits, species!="PLECON"), aes(species, sla)) +geom_boxplot() +
  xlab("Species Code") + ylab("SLA (cm2/g)") +ggtitle("Inter+intraspecies SLA variation")

#tukey
library(agricolae)
SLA.lm <- lm(sla ~ species, data = leaf_traits)
SLA.av <- aov(SLA.lm)
summary(SLA.av)
tukey.test <- TukeyHSD(SLA.av)
tukey.test
plot(tukey.test)

tukey.test2 <- HSD.test(SLA.av, trt = 'species')
tukey.test2
# Calculate and visualize mean and SE of height
height_variation <- plant_heights %>%
  group_by(species) %>%
  summarize(meanheight=mean(Height_mm), errorheight=calcSE(Height_mm))

ggplot(height_variation, aes(species, meanheight)) + geom_bar(stat="identity") + 
  geom_errorbar(aes(ymin=(meanheight-errorheight), ymax=(meanheight+errorheight)))

#tukey
library(agricolae)
height.lm <- lm(Height_mm ~ species, data = plant_heights)
heights.av <- aov(height.lm)
summary(SLA.av)
tukey.test <- TukeyHSD(SLA.av)
tukey.test
plot(tukey.test)

tukey.test2 <- HSD.test(heights.av, trt = 'species')
tukey.test2

#Height Variation
ggplot(plant_heights, aes(species, Height_mm)) +geom_boxplot() +
  xlab("Species Code") + ylab("Height (mm)") +ggtitle("Inter+intraspecies height variation")

#Seed mass means
ggplot(seedbyspec, aes(species, seedperpound)) +geom_bar(stat="identity") +
  xlab("Species Code") + ylab("Seeds/Lb") +ggtitle("Interspecies seed mass variation")


### Method 2: PCA
tog0 <- leafbyspec #%>%
#  select(-errorsla)
tog1 <- heightbyspec# %>%
 # select(-errorheight)
tog2 <-seedbyspec %>%
  select(-fullname) %>%
  mutate(CON="Control", PREC="Warming+Precip", WARM="Warming", DRO="Drought") %>%
  gather(tog2, "treatment", 3:6) %>%
  select(-tog2)
tog <- full_join(tog0, tog1)
tog <- full_join(tog, tog2)
tognona<-tog %>%
  filter(!is.na(meansla), !is.na(meanheight), !is.na(seedperpound))
tog <- tog %>%
  filter(!is.na(site)) %>%
  mutate(meansla = parse_number(meansla)) %>%
  mutate(meanheight = parse_number(meanheight)) %>%
  mutate(seedperpound = parse_number(seedperpound))
tog$rowid  <- 1:nrow(tog)

rm(tog0, tog1, tog2)

#function from guy who made ade4 package. should fill NAs with missing values
f1 <- function(vec) { 
  m <- mean(vec, na.rm = TRUE) 
  vec[is.na(vec)] <- m 
  return(vec) 
} 
Y = apply(tog[4:6],2,f1) 
Y<-as.tibble(Y)
Y$rowid  <- 1:nrow(Y)

tog<-left_join(tog, Y, by="rowid") %>%
  select(-rowid, -meansla.x, -meanheight.x, -seedperpound.x, -DRO)
names(tog) <- c("species", "site", "treatment", "sla", "height", "seedmass")
rm(Y)

trait.rda <- rda(tog[4:6])
biplot(trait.rda)
ordihull(trait.rda,
         group = tog$species)

nona.rda <- rda(tognona[4:6])
biplot(nona.rda)
ordihull(nona.rda,
         group = tognona$species)

colors <- c("red", "blue", "green", "purple", "brown", "orange", "magenta")
colors <- colors[as.factor(tognona$species)]

colors2<- c("red", "blue", "green", "purple", "brown", "orange", "magenta", "yellow", "grey", "black")
colors2 <- colors2[as.factor(tog$species)]
scatterplot3d(tog[4:6], color=colors2, pch=20, label.tick.marks=FALSE, box=FALSE)

ggplot(tog, aes(meansla, meanheight)) +geom_point(aes(color=species)) + theme_classic() + ylab("Mean Height (mm)") +xlab("Mean SLA (cm2/g)")

ggplot(tog) +geom_point(aes(meansla, meanheight, color=species, shape=treatment)) + facet_wrap(~site)
  theme_classic() + ylab("Mean Height (mm)") +xlab("Mean SLA (cm2/g)")
  
##########################################################
### 2a. Species means shift in a predictable direction? ###
##########################################################

#####################################################
### 2b. Species means predict shift in abundance? ###
#####################################################
sppdat0 <- read_csv("PI_DATA_2018-04-06.csv", skip = 3) %>%
  mutate(Site = factor(Site, levels = c("Southern", "Central", "Northern"))) %>%
  gather("species", "cover", 5:107) %>%
  mutate(cover = parse_number(cover)) %>%
  mutate(treatment = `Climate Treatment`) %>%
  filter(!species%in% c("Total Cover", "Unknown sample 12", "Unknown sample 6 (forb)")) %>%
  group_by(species) %>%
  mutate(maxcover = max(cover))
sppdat <- left_join(sppdat0, funcdat)

### AGGREGATE COVER ###

tog <- left_join(sppdat, funcdat) %>%
  group_by(Plot, Site, treatment, func, growth) %>%
  summarize(totcover = sum(cover)) %>% 
  tbl_df() %>%
  mutate(Site = factor(Site, levels = c("Southern", "Central", "Northern"))) %>%
  filter(!is.na(treatment), !is.na(growth))

sppdatavg <- sppdat0 %>%
  group_by(Site, species, treatment, maxcover) %>%
  summarize(meancover=mean(cover), secover=calcSE(cover))


### SIDALCEA COVER BY TREATMENT ###
ggplot(subset(sppdatavg, species == "Sidalcea malviflora"), aes(x=treatment, y=meancover)) +geom_bar(stat="identity")+
  facet_wrap(~Site)

##########################################
### 2c. Community level trait response ###
##########################################

#Bring traits by species into the same table
tog <- left_join(leafbyspec, heightbyspec, seedbyspec)

ggplot(subset(tog, species=="SIDMAL"), aes(treatment, avgheight)) + geom_bar(stat="identity") +facet_wrap(~site) +
  geom_errorbar(aes(ymin=(avgheight-SEheight), ymax=(avgheight+SEheight)))

ggplot(subset(tog, species=="PLECON"), aes(treatment, avgheight)) + geom_bar(stat="identity") +facet_wrap(~site) +
  geom_errorbar(aes(ymin=(avgheight-SEheight), ymax=(avgheight+SEheight)))


