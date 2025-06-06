install.packages("gghighlight")

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
library(gghighlight)

getwd()

df <- read_excel("./Terrell.VO2.peak.xlsx")
names(df)

names(df)<- paste(names(df), df[1,])
names(df)

df$`VO2/kg mL/min/Kg` <- as.numeric(df$`VO2/kg mL/min/Kg`)
df$`HR bpm` <- as.numeric(df$`HR bpm`)
df$`Power Watt` <- as.numeric(df$`Power Watt`)

names(df) <- tolower(names(df))
names(df)[22] <- "vo2"
names(df)[24] <- "hr"
names(df)[37] <- "power"

df1 <- df[c(22,24,37)]
df1 <- df1[-c(1:2),]
df1[1,3] <- 0
max(df1$power)

is.na(df1$vo2)
is.na(df1$hr)
df1
df1 <- df1 %>%
  mutate(smooth_y = predict(gam(hr ~ s(vo2, bs = "cs"), data = df1)))

max(df1$smooth_y)
max(df1$smooth_y)*.7
nrow(df1)

which(df1$smooth_y %in% max(df1$smooth_y)*.7 | max(df1$smooth_y)*.75)

ggplot(df1, aes(x = vo2, y = hr)) +
  geom_point(data = df1, aes(color = power)) +
  geom_point(data = df1, aes(x = vo2, y = smooth_y), color = "blue") +
  scale_color_gradient(low = "yellow", high = "red")+
  geom_smooth(color = "black") +
  geom_hline(yintercept = max(df1$smooth_y)) +
  gghighlight(max(smooth_y) > 115.993)
