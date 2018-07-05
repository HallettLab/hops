library(tidyverse)
library(here)

# set ggplot2 theme
theme_set(theme_bw(base_size = 16) + theme(text = element_text(size = 20)) +
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.background = element_blank(),
                  panel.border = element_rect(colour = "black")))

### READ IN AND CLEAN THE DATA ###
funcdat0 <- read_csv("Data/PI_DATA_2018-04-06.csv")[1:3,] %>%
  select(-c(X1:X4)) 

funcdat <- as_tibble(t(funcdat0)) 
names(funcdat) = c("growth", "func", "species")

sppdat0 <- read_csv("Data/PI_DATA_2018-04-06.csv", skip = 3) %>%
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

ggplot(tog, aes(x=interaction(func, growth), y = totcover)) + 
  geom_boxplot() + 
  facet_grid(Site ~ treatment)


ggplot(tog, aes(x=treatment, y = totcover)) + 
  geom_boxplot() + 
  facet_grid(Site ~ interaction(func, growth))


### SPECIES LEVEL RESPONSES ###

## Perennial grasses ##
ggplot(subset(sppdat, func == "Grass" & growth == "Perennial" & maxcover > 15), aes(x=Site, y=cover)) +geom_boxplot() +
  facet_wrap(~species)


## Focal species driving the patterns ##
# Southern pattern driven 

ggplot(subset(sppdat,func == "Grass" & growth == "Perennial" & maxcover > 5), aes(x=treatment, y=cover)) +geom_boxplot() +
  facet_grid(Site~species, scales = "free")

# ggplot(subset(sppdat, species == "Schedonorus arundinaceus"), aes(x=treatment, y=cover)) +geom_boxplot() +
#   facet_wrap(~Site)
# 
# ggplot(subset(sppdat, species == "Agrostis capillaris"), aes(x=treatment, y=cover)) +geom_boxplot() +
#   facet_wrap(~Site)
# 
# ggplot(subset(sppdat, species == "Alopecurus pratensis"), aes(x=treatment, y=cover)) +geom_boxplot() +
#   facet_wrap(~Site)

## Exploratory grant figure

grassdat <- sppdat %>%
  filter(func == "Grass", !is.na(growth)) %>%
  mutate(WarmTrt = "Warm",
         WarmTrt = ifelse(treatment == "Drought" | treatment == "Control", "Ambient", WarmTrt)) %>%
  group_by(Plot, Site, WarmTrt, growth) %>%
  summarize(cover = sum(cover)) %>%
  tbl_df() %>%
  mutate(Site = factor(Site, levels = c("Southern", "Central", "Northern")),
         growth = factor(growth, levels = c("Perennial", "Annual"))) 
  

ggplot(grassdat, aes(x=WarmTrt, y=cover)) + geom_boxplot() + facet_grid(growth~Site) + labs(x= "Treatment", y = "Percent Cover")

ggplot(subset(grassdat, growth == "Perennial"), aes(x=WarmTrt, y=cover)) + geom_boxplot(fill = "grey80") + facet_wrap(~Site) + labs(x= "Treatment", y = "Perennial Grass Cover")
ggsave(here("Figs", "fig1-perennial-with-warming.pdf"), width = 8, height = 4)
ggsave(here("Figs", "fig1-perennial-with-warming.jpg"), width = 8, height = 4)


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

ggplot(subset(sppdat,func == "Forb" & growth == "Annual" & maxcover > 5), aes(x=treatment, y=cover)) +geom_boxplot() +
  facet_grid(Site~species, scales = "free")


## Annual grasses ##

ggplot(subset(sppdat,func == "Grass" & growth == "Annual"), aes(x=treatment, y=cover)) +geom_boxplot() +
  facet_grid(Site~species, scales = "free")

ggplot(subset(sppdat, species == "Bromus tectorum"), aes(x=treatment, y=cover)) +geom_boxplot() +
  facet_grid(~Site, scales = "free")



