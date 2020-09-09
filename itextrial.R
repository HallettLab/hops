### Created 8/19/19
### Analysis of ITEX warming chamber trial
### Treatments include standard ITEX (itx), thermal mass ITEX (itm), and ambient (c). 
library(tidyverse)
library(ggpubr)
##FN for Calculating SE
calcSE<-function(x){
  x <- x[!is.na(x)]
  sd(x)/sqrt(length(x))
}

theme_set(theme_classic())

### Read in the data
clo<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/clo.csv", skip=14)%>%
  mutate(trt="control",  medium="air", position="lo", loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
chi<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/chi.csv", skip=14)%>%
  mutate(trt="control",  medium="air", position="hi", loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c5<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c5.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=5, loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c5e<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c5e.csv", skip=14)%>%
  mutate(trt="control", medium="soil", position=5, loc="e")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c5w<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c5w.csv", skip=14)%>%
  mutate(trt="control", medium="soil", position=5, loc="w")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c5s<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c5s.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=5, loc="s")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c5n<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c5n.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=5, loc="n")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c5ee<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c5ee.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=5, loc="ee")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c5ww<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c5ww.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=5, loc="ww")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c5ss<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c5ss.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=5, loc="ss")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c5nn<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c5nn.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=5, loc="nn")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)

c10<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c10.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=10, loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c10e<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c10e.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=10, loc="e")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c10w<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c10w.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=10, loc="w")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c10s<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c10s.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=10, loc="s")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c10n<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c10n.csv", skip=14)%>%
  mutate(trt="control", medium="soil", position=10, loc="n")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c10ee<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c10ee.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=10, loc="ee")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c10ww<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c10ww.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=10, loc="ww")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c10ss<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c10ss.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=10, loc="ss")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
c10nn<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/c10nn.csv", skip=14)%>%
  mutate(trt="control",  medium="soil", position=10, loc="nn")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)

itxlo<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itxlo.csv", skip=14)%>%
  mutate(trt="itx",  medium="air", position="lo", loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itxhi<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itxhi.csv", skip=14)%>%
  mutate(trt="itx",  medium="air", position="hi", loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx5<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx5.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=5, loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx5e<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx5e.csv", skip=14)%>%
  mutate(trt="itx", medium="soil", position=5, loc="e")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx5w<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx5w.csv", skip=14)%>%
  mutate(trt="itx", medium="soil", position=5, loc="w")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx5s<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx5s.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=5, loc="s")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx5n<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx5n.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=5, loc="n")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx5ee<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx5ee.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=5, loc="ee")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx5ww<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx5ww.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=5, loc="ww")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx5ss<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx5ss.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=5, loc="ss")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx5nn<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx5nn.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=5, loc="nn")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)

itx10<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx10.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=10, loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx10e<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx10e.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=10, loc="e")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx10w<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx10w.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=10, loc="w")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx10s<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx10s.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=10, loc="s")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx10n<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx10n.csv", skip=14)%>%
  mutate(trt="itx", medium="soil", position=10, loc="n")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx10ee<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx10ee.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=10, loc="ee")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx10ww<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx10ww.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=10, loc="ww")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx10ss<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx10ss.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=10, loc="ss")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itx10nn<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itx10nn.csv", skip=14)%>%
  mutate(trt="itx",  medium="soil", position=10, loc="nn")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)

itmlo<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itmlo.csv", skip=14)%>%
  mutate(trt="itm",  medium="air", position="lo", loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itmhi<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itmhi.csv", skip=14)%>%
  mutate(trt="itm",  medium="air", position="hi", loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm5<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm5.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=5, loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
#itm5e<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm5e.csv", skip=14)%>%
#  mutate(trt="itm", medium="soil", position=5, loc="e")%>%
#  rename(time="Date/Time", temp=Value)%>%
#  select(-Unit)
itm5w<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm5w.csv", skip=14)%>%
  mutate(trt="itm", medium="soil", position=5, loc="w")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm5s<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm5s.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=5, loc="s")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm5n<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm5n.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=5, loc="n")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm5ee<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm5ee.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=5, loc="ee")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm5ww<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm5ww.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=5, loc="ww")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm5ss<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm5ss.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=5, loc="ss")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm5nn<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm5nn.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=5, loc="nn")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)

itm10<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm10.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=10, loc="m")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm10e<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm10e.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=10, loc="e")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm10w<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm10w.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=10, loc="w")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm10s<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm10s.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=10, loc="s")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm10n<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm10n.csv", skip=14)%>%
  mutate(trt="itm", medium="soil", position=10, loc="n")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm10ee<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm10ee.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=10, loc="ee")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm10ww<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm10ww.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=10, loc="ww")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm10ss<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm10ss.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=10, loc="ss")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)
itm10nn<-read_csv("/Users/alejandrobrambila/Google Drive/thermochron/itm10nn.csv", skip=14)%>%
  mutate(trt="itm",  medium="soil", position=10, loc="nn")%>%
  rename(time="Date/Time", temp=Value)%>%
  select(-Unit)

# standardization tests
depthtest<-rbind(c5[100:200,],c5e[100:200,],c5w[100:200,],c5s[100:200,],
                 c5n[100:200,],c5ee[100:200,],c5ww[100:200,],c5ss[100:200,],
                 c5nn[100:200,],c10[100:200,],c10e[100:200,],c10w[100:200,],c10s[100:200,],
                 c10n[100:200,],c10ee[100:200,],c10ww[100:200,],c10ss[100:200,],
                 c10nn[100:200,],clo[100:200,],
                 chi[100:200,],itx5[100:200,],itx5e[100:200,],itx5w[100:200,],itx5s[100:200,],
                 itx5n[100:200,],itx5ee[100:200,],itx5ww[100:200,],itx5ss[100:200,],
                 itx5nn[100:200,],itx10[100:200,],itx10e[100:200,],itx10w[100:200,],itx10s[100:200,],
                 itx10n[100:200,],itx10ee[100:200,],itx10ww[100:200,],itx10ss[100:200,],
                 itx10nn[100:200,],itxlo[100:200,],
                 itxhi[100:200,],itm5[100:200,],itm5w[100:200,],itm5s[100:200,],
                 itm5n[100:200,],itm5ee[100:200,],itm5ww[100:200,],itm5ss[100:200,],
                 itm5nn[100:200,],itm10[100:200,],itm10e[100:200,],itm10w[100:200,],itm10s[100:200,],
                 itm10n[100:200,],itm10ee[100:200,],itm10ww[100:200,],itm10ss[100:200,],
                 itm10nn[100:200,],itmlo[100:200,],
                 itmhi[100:200,])%>%
  mutate(ztemp0=scale(temp))%>%
  group_by(time)%>%
  mutate(ztemp=scale(temp))%>%
  mutate(daynight=ifelse(ztemp0>0, "day", "night"))%>%
  ungroup()%>%
  mutate(timparse=parse_datetime(time, "%m/%d/%y %I:%M:%S %p"))%>%
  mutate(mdy=format(depthtest$timparse, "%m%d%y"))
  

ggplot(subset(depthtest), aes(time, temp)) +geom_jitter(aes(color=as.factor(trt)))+facet_wrap(~position)

fullset<-rbind(c5,c5e,c5w,c5s,
                 c5n,c5ee,c5ww,c5ss,
                 c5nn,c10,c10e,c10w,c10s,
                 c10n,c10ee,c10ww,c10ss,
                 c10nn,clo,
                 chi,itx5,itx5e,itx5w,itx5s,
                 itx5n,itx5ee,itx5ww,itx5ss,
                 itx5nn,itx10,itx10e,itx10w,itx10s,
                 itx10n,itx10ee,itx10ww,itx10ss,
                 itx10nn,itxlo,
                 itxhi,itm5,itm5w,itm5s,
                 itm5n,itm5ee,itm5ww,itm5ss,
                 itm5nn,itm10,itm10e,itm10w,itm10s,
                 itm10n,itm10ee,itm10ww,itm10ss,
                 itm10nn,itmlo,
                 itmhi)%>%
  mutate(ztemp0=scale(temp))%>%
  group_by(time)%>%
  mutate(ztemp=scale(temp))%>%
  mutate(daynight=ifelse(ztemp0>0, "day", "night"))%>%
  ungroup()%>%
  mutate(timparse=parse_datetime(time, "%m/%d/%y %I:%M:%S %p"))%>%
  mutate(timparse2=as.Date(timparse))

fs<-ggplot(subset(fullset), aes(timparse2, temp)) +
  geom_jitter(aes(color=as.factor(trt)))+
  facet_wrap(~position)+
  scale_x_date(date_breaks="months", date_labels="%b")


fullset0<-fullset%>%
  mutate(mdy=as.factor(format(fullset$timparse, "%m%d")))%>%
  group_by(mdy, trt, position, loc)%>%
  mutate(maxtemp=as.numeric(max(temp)), mintemp=min(temp))%>%
  filter(temp==maxtemp|temp==mintemp)%>%
  gather("maxmin", "value", maxtemp, mintemp)%>%
  filter(temp==value)
fullset2<-fullset0%>%
  ungroup()%>%
  group_by(temp, trt, medium, position, loc, mdy, maxmin)%>%
  summarize()

extras<-fullset2%>%
  group_by(mdy, trt, position, loc)%>%
  summarize(count=n())
hist(extras$count)  

early.months<-fullset%>%
  filter(timparse2>"2019-04-19")%>%
  filter(timparse2<"2019-06-01")

em<-ggplot(early.months, aes(timparse2, temp)) +
  geom_jitter(aes(color=as.factor(trt)))+
  facet_wrap(~position)+
  scale_x_date(date_breaks="weeks", date_labels="%d-%b")

early.months0<-early.months%>%
  mutate(mdy=as.factor(format(early.months$timparse, "%m%d")))%>%
  group_by(mdy, trt, position, loc)%>%
  mutate(maxtemp=as.numeric(max(temp)), mintemp=min(temp))%>%
  filter(temp==maxtemp|temp==mintemp)%>%
  gather("maxmin", "value", maxtemp, mintemp)%>%
  filter(temp==value)
early.months2<-early.months0%>%
  ungroup()%>%
  group_by(temp, trt, medium, position, loc, mdy, maxmin)%>%
  summarize()

#actual max min temps over the time period, but lots of var becase seasonality matters
ggplot(fullset2, aes(x=maxmin, y=temp)) +geom_boxplot(aes(fill=trt)) +facet_wrap(~position, scales="free")
ggplot(early.months2, aes(x=maxmin, y=temp)) +geom_boxplot(aes(fill=trt)) +facet_wrap(~position, scales="free")

#do differences within each day
fullset3<-fullset2%>%
  spread(trt, temp)%>%
  mutate(itm_effect=itm-control, itx_effect=itx-control)%>%
  select(-control, -itm, -itx)%>%
  gather("trt", "tempdiff", itm_effect, itx_effect)%>%
  ungroup()%>%
  mutate(position=ifelse(position=="10", "10cm soil", ifelse(position=="5", "5cm soil", ifelse(position=="hi", "60cm air", "30cm air"))))%>%
  mutate(mdy2=as.character(mdy))%>%
  mutate(mdy3=as.numeric(mdy2))%>%
  filter(mdy3>417, mdy3<815)%>%
  filter(!is.na(tempdiff))%>%
  group_by(mdy, position, maxmin, trt)%>%
  summarize(tempdiff=mean(tempdiff))

early.months3<-early.months2%>%
  spread(trt, temp)%>%
  mutate(itm_effect=itm-control, itx_effect=itx-control)%>%
  select(-control, -itm, -itx)%>%
  gather("trt", "tempdiff", itm_effect, itx_effect)%>%
  ungroup()%>%
  mutate(position=ifelse(position=="10", "10cm soil", ifelse(position=="5", "5cm soil", ifelse(position=="hi", "60cm air", "30cm air"))))%>%
  mutate(mdy2=as.character(mdy))%>%
  mutate(mdy3=as.numeric(mdy2))%>%
  filter(mdy3>417, mdy3<815)%>%
  filter(!is.na(tempdiff))%>%
  group_by(mdy, position, maxmin, trt)%>%
  summarize(tempdiff=mean(tempdiff))

## air and soil temp differences in chamber vs control
fs3<-ggplot(fullset3, aes(x=maxmin, y=tempdiff)) +
  geom_boxplot(aes(fill=trt)) +
  facet_wrap(~position, scales="free")+
  ylab("Deviation in Temp > Control (Celsius)") +
  xlab("")+
  scale_fill_discrete(labels=c("Thermal Mass Itex", "Standard Itex"))+
  geom_hline(yintercept=0)

## air and soil temp differences in chamber vs control (thru end of may)
em3<-ggplot(early.months3, aes(x=maxmin, y=tempdiff)) +
  geom_boxplot(aes(fill=trt)) +
  facet_wrap(~position, scales="free")+
  ylab("Deviation in Temp > Control (Celsius)") +
  xlab("")+
  scale_fill_discrete(labels=c("Thermal Mass Itex", "Standard Itex"))+
  geom_hline(yintercept=0)


ggarrange(fs, fs3, em, em3)

