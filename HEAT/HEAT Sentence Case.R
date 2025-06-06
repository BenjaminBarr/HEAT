library(readxl)
library(stringr)
instrument <- read.csv("HeatAndExerciseInAgingAsTherap_DataDictionary_2025-05-20.csv")
caps <- instrument$Field.Label
caps[which(str_detect(caps, "[1-9].") == FALSE)]
caps <- str_to_sentence(caps, locale = "en")
instrument$Field.Label <- caps
write.csv(instrument,"HEAT.Data.Dictionary.csv")
