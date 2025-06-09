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

df <- read_excel("./HEAT/Terrell.VO2.peak.xlsx")
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
  mutate(smooth_y = predict(gam(hr ~ s(vo2, bs = "cs"), data = df))) %>%
  mutate(lm_y = predict(lm(hr ~ vo2), data = df))

quantile(df1$vo2, probs = .99)


vo2.range <- data.frame(per = c(.7, .75, .8, .85, .9, .95))

vo2.range$vo2 <- vo2.range$per*25.496

# Fit a LOESS model
loess_model <- loess(hr ~ vo2, data = df1)
lm_model <- lm(hr~vo2, data = df1)

# Predict the y-value at the 70th percentile
vo2.range$hr <- predict(loess_model, newdata = vo2.range)
vo2.range$hrlm <- predict(lm_model, newdata = vo2.range)

ggplot(df1, aes(x = vo2, y = hr)) +
  geom_point(data = df1, aes(color = power)) + 
  scale_color_gradient(low = "yellow", high = "red")+
  geom_smooth(color = "black") +
  geom_point(data = vo2.range, aes(x = vo2, y = hr), color = "black", size = 3) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = vo2.range[1,3], ymax = vo2.range[2,3]), fill = "lightblue", alpha = 0.02) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = vo2.range[3,3], ymax = vo2.range[4,3]), fill = "purple", alpha = 0.02) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = vo2.range[5,3], ymax = vo2.range[6,3]), fill = "green", alpha = 0.01) +
  annotate("text", x = 5, y = (vo2.range[1,3] + vo2.range[2,3])/2, 
           label = paste("70-75% VO2 Max (", round(vo2.range[1,3]), "-", round(vo2.range[2,3]), " bpm)", sep = "")) +
  #annotate("text", x = 5, y = vo2.range[2,2], label = round(vo2.range[2,2])) +
  annotate("text", x = 5, y = (vo2.range[3,3] + vo2.range[4,3])/2, 
           label = paste("80-85% VO2 Max (", round(vo2.range[3,3]), "-", round(vo2.range[4,3]), " bpm)", sep = "")) +
  #annotate("text", x = 7.5, y = vo2.range[4,2], label = round(vo2.range[4,2])) +
  annotate("text", x = 5, y = (vo2.range[5,3] + vo2.range[6,3])/2, 
           label = paste("90-95% VO2 Max (", round(vo2.range[5,3]), "-", round(vo2.range[6,3]), " bpm)", sep = "")) +
  #annotate("text", x = 10, y = vo2.range[6,2], label = round(vo2.range[6,2])) +
  geom_hline(yintercept = vo2.range$hr) +
  labs(title = "Terrell's VO2 Max") +
  theme_pubr()
  


