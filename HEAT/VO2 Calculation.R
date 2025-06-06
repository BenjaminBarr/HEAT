library(here) # here makes a project transportable
library(janitor) # clean_names
library(readxl) # read excel, duh!
library(data.table) # magical data frames
library(magrittr) # pipes
library(stringr) # string functions
library(forcats) # factor functions

# analysis packages
library(emmeans) # the workhorse for inference
library(nlme) # gls and some lmm
library(lme4) # linear mixed models
library(lmerTest) # linear mixed model inference
library(afex) # ANOVA linear models
library(glmmTMB) # generalized linear models
library(MASS) # negative binomial and some other functions
library(car) # model checking and ANOVA
library(DHARMa) # model checking

# graphing packages
library(ggsci) # color palettes
library(ggpubr) # publication quality plots
library(ggforce) # better jitter
library(cowplot) # combine plots
library(knitr) # kable tables
library(kableExtra) # kable_styling tables

library(insight)
library(lazyWeave)
library(dplyr)
library(mgcv)
getwd()

df <- read_excel("./Terrell.VO2.peak.xlsx")
names(df)

names(df)<- paste(names(df), df[1,])
names(df)

df$`VO2 mL/min` <- as.numeric(df$`VO2 mL/min`)
df$`HR bpm` <- as.numeric(df$`HR bpm`)

names(df) <- tolower(names(df))
names(df)[15] <- "vo2"
names(df)[24] <- "hr"

df1 <- df[c(15,24)]
df1 <- df1[-c(1:2),]

is.na(df$vo2)
is.na(df$hr)

df1 <- df1 %>%
  mutate(smooth_y = predict(gam(vo2 ~ s(hr, bs = "cs"), data = df1)))

max(df1$smooth_y)

ggplot(df1, aes(x = hr, y = vo2)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = max(df1$smooth_y)) +
  geom_point(data = df1, aes(x = hr, y = smooth_y), color = "red")
