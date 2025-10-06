install.packages("XML")

library(xml2)
library(XML)
library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)

getwd()
xml.doc <- read_xml("~/HEAT/10.3.2025/apple_health_export/export.xml")
unique(xml_name(xml_find_all(xml.doc, ".//*")))
records <- xml_find_all(xml.doc, ".//Record")
df <- data.frame(
  type = xml_attr(records, "type"),
  sourceName = xml_attr(records, "sourceName"),
  unit = xml_attr(records, "unit"),
  value = xml_attr(records, "value"),
  startDate = xml_attr(records, "startDate"),
  endDate = xml_attr(records, "endDate"),
  stringsAsFactors = FALSE
)
df

df <- df %>%
  mutate(
    startDate = ymd_hms(startDate),
    endDate = ymd_hms(endDate),
    start_date = as.Date(startDate),
    start_time = format(startDate, "%H:%M:%S"),
    end_date = as.Date(endDate),
    end_time = format(endDate, "%H:%M:%S")
  )
df

df <- df %>%
  select(type, sourceName, unit, value,
         startDate, start_date, start_time,
         endDate, end_date, end_time)
df

#Set correct timezone for datetime

df$startDate <- as.POSIXct(df$startDate, tz = "UTC")
df$startDate <- format(df$startDate, tz = "America/Chicago", usetz = TRUE)
df$start_time <- format(as.POSIXct(df$startDate), "%I:%M:%S %p")
df$endDate <- as.POSIXct(df$endDate, tz = "UTC")
df$endDate <- format(df$endDate, tz = "America/Chicago", usetz = TRUE)
df$end_time <- format(as.POSIXct(df$endDate), "%I:%M:%S %p")

#Select date of participant
hea005 <- df[which(df$start_date == "2025-09-12"),]
hea005$value <- as.numeric(hea005$value)

#Identify appropriate start_time from metabolic cart data -> *05:35:56 PM
##Closest to the actual start time 
hea005[which(grepl("\\b05:35:\\d{2} PM\\b", hea005$start_time)),]
which(grepl("\\b05:35:57 PM\\b", hea005$start_time))

hea005 <- hea005[1159:nrow(hea005), ]
hea005$startDate <- as.POSIXct(hea005$startDate)
hea005$start_diff <- hea005$startDate - hea005$startDate[1]

total_seconds <- as.numeric(hea005$start_diff, units = "secs")

# Calculate minutes and seconds
minutes <- floor(total_seconds / 60)
seconds <- round(total_seconds %% 60)

# Format as mm:ss
hea005$start_diff_mmss <- sprintf("%02d:%02d", minutes, seconds)
head(hea005)[c(4,12)]
hea005$value <- as.numeric(hea005$value)
hea005$start_diff <- as.numeric(hea005$start_diff)
is.numeric(hea005$value)

#Identify peak HR time
hea005[which(hea005$value == 163),]

#Identify end met test time from met cart data - *19:06
hea005[which(hea005$start_diff_mmss == "19:06"),]
hea005 <- hea005[1:1147,]

ggplot(hea005, aes(start_diff, value)) +
  geom_point()

#Match time on cart
cart <- read_xlsx("~/HEAT/HEA005 VO2.xlsx", sheet = "Data (2)") # Move units in excel to title and delete units row along with other preceding info
names(cart)
c.time <- cart[c(1,13)]

# Extract mm:ss
c.time[1] <- as.POSIXct(c.time$`t (s)`, tz = "UCT")
c.time[1] <- format(c.time$`t (s)`, "%M:%S")
c.time

hea005.final <- hea005[which(hea005$start_diff_mmss %in% c.time$`t (s)`),]
hea005.final
max(hea005.final$`heart rate (bpm)`)

names(hea005.final)
hea005.final <- hea005.final[c(4, 12)]
names(hea005.final)[2] <- "t (s)"
hea005.final <- merge(hea005.final, c.time, by = "t (s)")
hea005.final$`VO2/kg` <- as.numeric(hea005.final$`VO2/kg`)
names(hea005.final)[2] <- "heart rate (bpm)"
ggplot(hea005.final, aes(`t (s)`, `heart rate (bpm)`)) +
  geom_point()


write.csv(hea005.final, "./hea005.vo2.target.csv")

