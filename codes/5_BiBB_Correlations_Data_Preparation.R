# Copyright (c) 2024 Honorata Bogusz

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

{
rm(list=ls())
gc()
options(timeout = 600)
options(scipen=999)
options(digits=2)

requiredPackages = c("splitstackshape", "tidyverse", "dplyr", "reshape2", "haven", 
                     "stringr", "viridisLite", "gridExtra", "lmreg", "mice","Hmisc",
                     "ggrepel", "viridis", "foreign", "bnstruct", "outliers","forecast",
                     "labelVector", "readxl", "survival", "ggfortify", "ranger",
                     "zoo", "imputeTS", "extrafont", "remotes", "gplots", 
                     "knitr", "corrplot", "ggplot2", "stats", "ggcorrplot", 
                     "showtext", "plyr")
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }
rm(list=ls())
gc()
}

############################## 2006 ##############################   

data <- read_dta("../generated_data/bibb2006_har.dta")

data <- data[,c("idnum",
                "s1", # sex
                "zpalter", # age
                "isced", # education
                "EB1_92o", # occupation
                "bula", # Bundesland
                "f310", # organizing !
                "f313", # investigating !
                "f311", # researching !
                "f322_01", # programming !
                "f312", # teaching !
                "f314", # consulting !
                "f307", # buying !
                "f309", # promoting !
                "f228", # wfh
                "f201", # work overtime
                "f411_01", # work under pressure
                "f518")] # monthly brutto wage 

    for (i in 2:ncol(data)){
      print(unique(data[[i]]))
    }

for (i in 3:ncol(data)){
  data[[i]][data[[i]]==9] <- NA
  data[[i]][data[[i]]==-4] <- NA
}

    for (i in 2:ncol(data)){
      print(unique(data[[i]]))
    }

for (i in c(7:9, 11:14)){
  data[[i]][data[[i]]==2] <- 1
  data[[i]][data[[i]]==3] <- 0
}

    for (i in 2:ncol(data)){
      print(unique(data[[i]]))
    }

unique(data$f322_01)
data$f322_01[data$f322_01==2] <- 0
unique(data$f322_01)

unique(data$f411_01)
data$f411_01 <- mapvalues(data$f411_01, from = c(1, 2, 3, 4), to = c(4, 3, 2, 1))
unique(data$f411_01)

unique(data$f228)
data$f228[data$f228==2] <- 0
unique(data$f228)

unique(data$f201)
data$f201[data$f201==2] <- 0
unique(data$f201)

colnames(data) <- c("id",
                    "sex",
                    "age",
                    "isced",
                    "occ",
                    "bula",
                    "organizing", # organizing !
                    "investigating", # investigating !
                    "researching", # researching !
                    "programming", # programming !
                    "teaching", # teaching !
                    "consulting", # consulting !
                    "buying", # buying !
                    "promoting", # promoting !
                    "wfh", # wfh
                    "overtime", # work overtime
                    "pressure", # work under pressure
                    "monthly_wage")

unique(data$isced)

data$edu <- case_when(
  data$isced==1 | data$isced==2 ~ 1,
  data$isced==3 | data$isced==4 | data$isced==5 | data$isced==6 ~ 2,
  data$isced==7 | data$isced==8 | data$isced==9 ~ 3
)
unique(data$edu)
data$isced <- NULL

data$occ <- substr(data$occ, start = 1, stop = 2)
unique(data$occ)

for(i in 2:ncol(data)){
  data[[i]] <- as.numeric(data[[i]])
}

data$programming <- ifelse(is.na(data$programming), 0, data$programming)

data$analytic <- ((data$organizing + data$researching + data$investigating + data$programming)/4)*100
data$interactive <- ((data$teaching + data$consulting + data$buying + data$promoting)/4)*100

unique(data$analytic)
unique(data$interactive)

data$age2 <- data$age * data$age

data_2006 <- data[,c("id", "sex", "age", "age2", "edu", "occ", "bula", 
                     "analytic", "interactive", "wfh", "overtime", "pressure", "monthly_wage")]

############################## 2012 ##############################   

data <- read_dta("../generated_data/bibb2012_har.dta")

data <- data[,c("Intnr",
                "S1", # sex
                "Zpalter", # age
                "Isced", # education
                "EB92o", # occupation
                "Bula", # Bundesland
                "F310", # organizing !
                "F313", # investigating !
                "F311", # researching !
                "F325_01", # programming !
                "F312", # teaching !
                "F314", # consulting !
                "F307", # buying !
                "F309", # promoting !
                "F204", # overtime
                "F411_01", # pressure
                "F518_SUF")] # wage


    for (i in 2:ncol(data)){
      print(unique(data[[i]]))
    }

for (i in 3:ncol(data)){
  data[[i]][data[[i]]==9] <- NA
  data[[i]][data[[i]]==-4] <- NA
}

    for (i in 2:ncol(data)){
      print(unique(data[[i]]))
    }

for (i in c(7:9, 11:14)){
  data[[i]][data[[i]]==2] <- 1
  data[[i]][data[[i]]==3] <- 0
}

    for (i in 2:ncol(data)){
      print(unique(data[[i]]))
    }

unique(data$F325_01)
data$F325_01[data$F325_01==2] <- 0
unique(data$F325_01)

unique(data$F411_01)
data$F411_01 <- mapvalues(data$F411_01, from = c(1, 2, 3, 4), to = c(4, 3, 2, 1))
unique(data$F411_01)

unique(data$F204)
data$F204 <- ifelse(!is.na(data$F204), 1, 0)

colnames(data) <- c("id",
                    "sex",
                    "age",
                    "isced",
                    "occ",
                    "bula",
                    "organizing", # organizing !
                    "investigating", # investigating !
                    "researching", # researching !
                    "programming", # programming !
                    "teaching", # teaching !
                    "consulting", # consulting !
                    "buying", # buying !
                    "promoting", # promoting !
                    "overtime", # work overtime
                    "pressure", # work under pressure
                    "monthly_wage")

unique(data$isced)
data$edu <- case_when(
  data$isced==1 | data$isced==2 ~ 1,
  data$isced==3 | data$isced==4 | data$isced==5 | data$isced==6 ~ 2,
  data$isced==7 | data$isced==8 | data$isced==9 ~ 3
)
unique(data$edu)
data$isced <- NULL

data$occ <- substr(data$occ, start = 1, stop = 2)
unique(data$occ)

for(i in 2:ncol(data)){
  data[[i]] <- as.numeric(data[[i]])
}

data$programming <- ifelse(is.na(data$programming), 0, data$programming)

data$analytic <- ((data$organizing + data$researching + data$investigating + data$programming)/4)*100
data$interactive <- ((data$teaching + data$consulting + data$buying + data$promoting)/4)*100

unique(data$analytic)
unique(data$interactive)

data$age2 <- data$age * data$age

data$wfh <- NA

data_2012 <- data[,c("id", "sex", "age", "age2", "edu", "occ", "bula", 
                     "analytic", "interactive", "wfh", "overtime", "pressure", "monthly_wage")]

############################## 2018 ##############################   

data <- read_dta("../generated_data/bibb2018_har.dta")

data <- data[,c("intnr",
                "S1", # sex
                "zpalter", # age
                "Casmin", # education
                "F100_kldb92_3d", # occupation
                "Bula", # Bundesland
                "F310", # organizing !
                "F313", # investigating !
                "F311", # researching !
                "F325_01", # programming !
                "F312", # teaching !
                "F314", # consulting !
                "F307", # buying !
                "F309", # promoting !
                "F228", # wfh
                "F204", # overtime
                "F411_01", # pressure
                "F518_SUF")] # wage

unique(data$Casmin)
data$edu <- case_when(
  data$Casmin==1 | data$Casmin==2 | data$Casmin==3 ~ 1,
  data$Casmin==4 | data$Casmin==5 | data$Casmin==6 | data$Casmin==7 ~ 2,
  data$Casmin==8 | data$Casmin==9 ~ 3
)
unique(data$edu)

    for (i in 2:ncol(data)){
      print(unique(data[[i]]))
    }

for (i in 3:ncol(data)){
  data[[i]][data[[i]]==9] <- NA
  data[[i]][data[[i]]==-4] <- NA
}

for (i in 2:ncol(data)){
  print(unique(data[[i]]))
}

for (i in c(7:9, 11:14)){
  data[[i]][data[[i]]==2] <- 1
  data[[i]][data[[i]]==3] <- 0
}

for (i in 2:ncol(data)){
  print(unique(data[[i]]))
}

data$Casmin <- NULL

unique(data$F325_01)
data$F325_01[data$F325_01==2] <- 0
unique(data$F325_01)

unique(data$F411_01)
data$F411_01 <- mapvalues(data$F411_01, from = c(1, 2, 3, 4), to = c(4, 3, 2, 1))
unique(data$F411_01)

unique(data$F204)
data$F204 <- ifelse(!is.na(data$F204), 1, 0)

unique(data$F228)
data$F228[data$F228==2] <- 0
unique(data$F228)

colnames(data) <- c("id",
                    "sex",
                    "age",
                    "occ",
                    "bula",
                    "organizing", # organizing !
                    "investigating", # investigating !
                    "researching", # researching !
                    "programming", # programming !
                    "teaching", # teaching !
                    "consulting", # consulting !
                    "buying", # buying !
                    "promoting", # promoting !
                    "wfh", # work from home
                    "overtime", # work overtime
                    "pressure", # work under pressure
                    "monthly_wage",
                    "edu")

data$occ <- substr(data$occ, start = 1, stop = 2)
unique(data$occ)

for(i in 2:ncol(data)){
  data[[i]] <- as.numeric(data[[i]])
}

data$programming <- ifelse(is.na(data$programming), 0, data$programming)

data$analytic <- ((data$organizing + data$researching + data$investigating + data$programming)/4)*100
data$interactive <- ((data$teaching + data$consulting + data$buying + data$promoting)/4)*100

unique(data$analytic)
unique(data$interactive)

data$age2 <- data$age * data$age

data_2018 <- data[,c("id", "sex", "age", "age2", "edu", "occ", "bula", 
                     "analytic", "interactive", "wfh", "overtime", "pressure", "monthly_wage")]

# MERGE AND SAVE DATA

data_2006$year <- 2006
data_2012$year <- 2012
data_2018$year <- 2018
data <- rbind(data_2006, data_2012, data_2018)
rm(data_2006, data_2012, data_2018)

write_dta(data, "../generated_data/data_for_correlations.dta")