library(foreign)
library(corrplot)
library(plm)
library(het.test)
library(ggplot2)
library(reshape2)
library(regclass)
library(stargazer)
library(dplyr)
library(ggthemes)

dir <- "C:/Users/HP/Documents/UT Dallas/Fall 2019/BUAN 6312 - Applied Econometrics and Time Series Analysis/Project"
setwd(dir)
gunsdata <- read.dta(paste0(dir,"/guns.dta"))
gunsdata$shall <- as.factor(gunsdata$shall)
gd_panel <- pdata.frame(gunsdata, index=c("stateid", "year"))
form <- log(vio) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pw1064 +
  pb1064

# Density Plots
melt.gunsdata <- melt(gunsdata)
ggplot(data = melt.gunsdata, aes(x = value)) + 
  stat_density(fill="slateblue") + 
  facet_wrap(~variable, scales = "free") +
  ggtitle("Distribution Plots")

# Shall Law vs No Shall Law
vmrsub <- gunsdata %>%
  select(year, vio, mur, rob, shall)
moltenvmr <- vmrsub %>%
  group_by(year, shall) %>%
  summarize(AvgVio = mean(vio),
            AvgMur = mean(mur),
            AvgRob = mean(rob))
moltenvmr <- melt(moltenvmr, id = c("year","shall"))

ggplot(moltenvmr, aes(x=year,y=value,color=shall))+
  geom_line()+
  facet_grid(variable~shall, scales="free_y") + 
  theme(legend.position = "none") + theme_stata()


VIOLENCE RATE

ols <- plm(form, data = gd_panel, model="pooling")
summary(ols)

ols <- plm(form, data = gd_panel, model="pooling")
summary(ols, vcov = vcovHC(ols, method="arellano"))

form <- log(vio) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pw1064 +
  pb1064
fe <- plm(form, data=gd_panel, model="within")
ra <- plm(form, data = gd_panel, model = "random")
summary(fe, vcov = vcovHC(fe, method="arellano"))

form <- log(vio) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pw1064 +
  pb1064 + factor(year)-1
fet <- plm(form, data=gd_panel, model="within", effect = "twoways")
summary(fet, vcov = vcovHC(fet, method="arellano"))

pFtest(fet,fe)

form <- log(vio) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pw1064 +
  pb1064 + factor(year)-1
fet <- plm(form, data=gd_panel, model="within")
summary(fet, vcov = vcovHC(fet, method="arellano"))

pFtest(fet,fe)
phtest(fe, ra)
phtest(fet, ra)

MURDER RATE

form <- log(mur) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pw1064 +
  pb1064
ols <- plm(form, data = gd_panel, model="pooling")
summary(ols)

ols <- plm(form, data = gd_panel, model="pooling")
summary(ols, vcov = vcovHC(ols, method="arellano"))

fe <- plm(form, data=gd_panel, model="within")
ra <- plm(form, data = gd_panel, model = "random")
summary(fe, vcov = vcovHC(fe, method="arellano"))

form <- log(mur) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pw1064 +
  pb1064 + factor(year)-1
fet <- plm(form, data=gd_panel, model="within")
summary(fet, vcov = vcovHC(fet, method="arellano", effect = "twoways"))

pFtest(fet,fe)

form <- log(mur) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pw1064 +
  pb1064 + factor(year)-1
fet <- plm(form, data=gd_panel, model="within")
summary(fet, vcov = vcovHC(fet, method="arellano"))

pFtest(fet,fe)

phtest(fe, ra)
phtest(fet, ra)

ROBBERY RATE

form <- log(rob) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pw1064 +
  pb1064
ols <- plm(form, data = gd_panel, model="pooling")
summary(ols)

ols <- plm(form, data = gd_panel, model="pooling")
summary(ols, vcov = vcovHC(ols, method="arellano"))

fe <- plm(form, data=gd_panel, model="within")
ra <- plm(form, data = gd_panel, model = "random")
summary(fe, vcov = vcovHC(fe, method="arellano"))

form <- log(rob) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pw1064 +
  pb1064 + factor(year)-1
fet <- plm(form, data=gd_panel, model="within", effect = "twoways")
summary(fet, vcov = vcovHC(fet, method="arellano"))

pFtest(fet,fe)

form <- log(rob) ~ shall + incarc_rate + density + avginc + pop + pm1029 + pw1064 +
  pb1064 + factor(year)-1
fet <- plm(form, data=gd_panel, model="within")
summary(fet, vcov = vcovHC(fet, method="arellano"))

pFtest(fet,fe)
phtest(fe, ra)
phtest(fet, ra)
