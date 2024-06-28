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
                     "labelVector", "readxl", "zoo", "imputeTS")
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }

rm(list=ls())
gc()
}

######################## Wave 1979 ######################## 

  data <- read_dta("../generated_data/bibb1979_har.dta")
  
  data <- data[,c(2, 44, 130:218)]
  
  for(i in 3:ncol(data)){
    data[[i]] <- as.numeric(data[[i]])
  }
  
  data <- data[complete.cases(data),]
  
  data$organizing <- ifelse(data$V214==1,1,0)
  
  data$researching <- ifelse(data$V169==1,1,
                             ifelse(data$V170==1,1,
                                    ifelse(data$V171==1,1,
                                           ifelse(data$V172==1,1,
                                                  ifelse(data$V173==1,1,0)))))
  
  data$constructing <- ifelse(data$V172==1,1,
                              ifelse(data$V173==1,1,0))
    
  data$programming <- ifelse(data$V197==1,1,0)
  
  data$applying_law <- ifelse(data$V201==1,1,
                              ifelse(data$V202==1,1,0))
  
  data$teaching <- ifelse(data$V206==1,1,0)
  
  data$consulting <- ifelse(data$V180==1, 1,
                            ifelse(data$V181==1, 1,
                                   ifelse(data$V183==1, 1,
                                          ifelse(data$V185==1, 1,
                                                 ifelse(data$V186==1, 1,
                                                        ifelse(data$V203==1, 1,0))))))

  data$buying <- ifelse(data$V181==1, 1,
                            ifelse(data$V182==1, 1,
                                   ifelse(data$V183==1, 1,
                                          ifelse(data$V184==1, 1,
                                                 ifelse(data$V185==1, 1,0)))))
  
  data$promoting <- ifelse(data$V212==1,1,0)
  
  data$negotiating <- ifelse(data$V213==1,1,0)
  
  data$managing <- ifelse(data$V214==1,1,
                          ifelse(data$V215==1,1,
                                 ifelse(data$V216==1,1,
                                        ifelse(data$V217==1,1,0))))
  
  data$repairing <- ifelse(data$V155==1,1,0)
  
  data$caring <- ifelse(data$V207==1, 1,
                        ifelse(data$V208==1, 1,0))
  
  data$accomodating <- ifelse(data$V156==1,1,
                              ifelse(data$V157==1,1,
                                     ifelse(data$V207==1,1,0)))
  
  data$protecting <- ifelse(data$V198==1, 1,
                            ifelse(data$V199==1, 1,0))
  
  data$cleaning <- ifelse(data$V199==1,1,
                          ifelse(data$V200==1,1,
                                 ifelse(data$V158==1,1,
                                        ifelse(data$V159==1,1,0))))
  
  data$measuring <- ifelse(data$V165==1,1,
                           ifelse(data$V166==1,1,
                                  ifelse(data$V167==1,1,
                                         ifelse(data$V168==1,1,0))))
  
  data$writing <- ifelse(data$V190==1,1,
                         ifelse(data$V191==1,1,
                                ifelse(data$V192==1,1,0)))
  
  data$calculating <- ifelse(data$V193==1,1,
                             ifelse(data$V194==1,1,
                                    ifelse(data$V195==1,1,0)))
  
  data$operating <- ifelse(data$V218==1,1,0)
  
  data$manufacturing <- ifelse(data$V130==1, 1,
                            ifelse(data$V131==1, 1,
                                   ifelse(data$V132==1, 1,
                                          ifelse(data$V133==1, 1,
                                                 ifelse(data$V134==1, 1,
                                                        ifelse(data$V135==1, 1,
                                                               ifelse(data$V136==1, 1,
                                                                      ifelse(data$V137==1, 1,
                                                                             ifelse(data$V138==1, 1,
                                                                                    ifelse(data$V139==1, 1,
                                                                                           ifelse(data$V140==1, 1,
                                                                                                  ifelse(data$V141==1, 1,
                                                                                                         ifelse(data$V142==1, 1,
                                                                                                                ifelse(data$V143==1, 1,
                                                                                                                       ifelse(data$V144==1, 1,
                                                                                                                              ifelse(data$V145==1, 1,
                                                                                                                                     ifelse(data$V146==1, 1,
                                                                                                                                            ifelse(data$V147==1, 1,
                                                                                                                                                   ifelse(data$V148==1, 1,
                                                                                                                                                          ifelse(data$V149==1, 1,
                                                                                                                                                                 ifelse(data$V150==1, 1,0)))))))))))))))))))))
  data$storing <- ifelse(data$V160==1,1,
                         ifelse(data$V161==1,1,
                                ifelse(data$V162==1,1,
                                       ifelse(data$V163==1,1,
                                              ifelse(data$V164==1,1,0)))))
  
  colnames(data)[1:2] <- c("id", "occupation")
  
  data <- data[,c("id", "occupation",
                  "organizing", "researching", "constructing", "programming", "applying_law", #analytic
                  "teaching", "consulting", "buying", "promoting", "negotiating", "managing", #interactive
                  "repairing", "caring", "accomodating",  "protecting",  "cleaning", # non-routine manual
                  "measuring", "writing", "calculating", # routine-cognitive  
                  "operating", "manufacturing", "storing")] # non-routine manual
  
  row_sub = apply(data, 1, function(row) all(row !=8 ))
  data <- data[row_sub,]
  row_sub = apply(data, 1, function(row) all(row !=9 ))
  data <- data[row_sub,]
  
  na <- is.na(data)
  sum(na)/sum(1-na)
  
  data <- data[complete.cases(data),]
  
  class(data$occupation)

# repair occupation codes (curr. coded as long and without 0 at the beginning)

  data$occupation <- as.character(data$occupation)
  data_1 <- data[(nchar(as.character(data$occupation)) == 5),]
  data_2 <- data[(nchar(as.character(data$occupation)) == 6),]
  
  data_1$occupation <- paste0("0", data_1$occupation)
  
  data <- rbind(data_1, data_2)
  rm(data_1, data_2)
  
  data$occupation <- gsub('.{3}$', '', data$occupation)
  
# create task measures

  data$analytic <- ((data$organizing + data$researching + data$constructing + data$programming + data$applying_law)/5)*100
  
  data$interactive <- ((data$teaching + data$consulting + data$buying + data$promoting + data$negotiating + data$managing)/6)*100
  
  data$nonroutine_cognitive <- ((data$organizing + data$researching + data$constructing + data$programming + data$applying_law + data$teaching + data$consulting + data$buying + data$promoting + data$negotiating + data$managing)/11)*100
  
  data$nonroutine_manual <- ((data$repairing + data$caring + data$accomodating + data$protecting + data$cleaning)/5)*100
  
  data$routine_cognitive <- ((data$measuring + data$writing + data$calculating)/3)*100
  
  data$routine_manual <- ((data$operating + data$manufacturing + data$storing)/3)*100
  
  data <- data[,c("id", "occupation", "analytic", "interactive", "nonroutine_cognitive", "nonroutine_manual", "routine_cognitive", "routine_manual")]
  
  analytic <- aggregate(data$analytic, by=list(occupation=data$occupation), FUN=mean)
  colnames(analytic)[2] <- "analytic"

  interactive <- aggregate(data$interactive, by=list(occupation=data$occupation), FUN=mean)
  colnames(interactive)[2] <- "interactive"
  
  nonroutine_cognitive <- aggregate(data$nonroutine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_cognitive)[2] <- "nonroutine_cognitive"
  
  nonroutine_manual <- aggregate(data$nonroutine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_manual)[2] <- "nonroutine_manual"
  
  routine_cognitive <- aggregate(data$routine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_cognitive)[2] <- "routine_cognitive"
  
  routine_manual <- aggregate(data$routine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_manual)[2] <- "routine_manual"
  
  task_measures_1979 <- merge(analytic, interactive, by="occupation")
  task_measures_1979 <- merge(task_measures_1979, nonroutine_cognitive, by="occupation")
  task_measures_1979 <- merge(task_measures_1979, nonroutine_manual, by="occupation")
  task_measures_1979 <- merge(task_measures_1979, routine_cognitive, by="occupation")
  task_measures_1979 <- merge(task_measures_1979, routine_manual, by="occupation")
  
  rm(analytic, interactive, nonroutine_manual, nonroutine_cognitive, routine_cognitive, routine_manual, data)

######################## Wave 1986 ######################## 
  
  data <- read_dta("../generated_data/bibb1986_har.dta")
  
  data <- data[,c(2,5,11:33)]
  
  row_sub = apply(data, 1, function(row) all(row !=8 ))
  data <- data[row_sub,]
  row_sub = apply(data, 1, function(row) all(row !=9 ))
  data <- data[row_sub,]
  
  data[data==2] <- 1
  
  for(i in 3:ncol(data)){
    data[[i]] <- as.numeric(data[[i]])
  }
  
  data <- data[complete.cases(data),]
  
  data$organizing <- ifelse(data$v33==1,1,0)
  
  data$researching <- ifelse(data$v21==1,1,
                             ifelse(data$v22==1,1,0))
  
  data$constructing <- ifelse(data$v22==1,1,0)
  
  data$programming <- ifelse(data$v26==1,1,0)
  
  data$applying_law <- ifelse(data$v28==1,1,0)
  
  data$teaching <- ifelse(data$v29==1,1,0)
  
  data$consulting <- ifelse(data$v29==1, 1,
                            ifelse(data$v30==1, 1,
                                   ifelse(data$v31==1, 1,0)))
  
  data$buying <- ifelse(data$v23==1,1,0)
  
  data$managing <- ifelse(data$v32==1,1,0)
  
  data$repairing <- ifelse(data$v13==1,1,
                           ifelse(data$v14==1,1,0))
  
  data$caring <- ifelse(data$v30==1,1,0)
  
  data$accomodating <- ifelse(data$v18==1,1,0)
  
  data$protecting <- ifelse(data$v27==1,1,0)
  
  data$cleaning <- ifelse(data$v19==1,1,0)
  
  data$writing <- ifelse(data$v24==1,1,0)
  
  data$calculating <- ifelse(data$v25==1,1,0)
  
  data$operating <- ifelse(data$v11==1, 1,
                           ifelse(data$v12==1, 1,0))
  
  data$manufacturing <- ifelse(data$v15==1, 1,
                            ifelse(data$v16==1, 1,
                                   ifelse(data$v17==1, 1,0)))
  
  data$storing <- ifelse(data$v20==1,1,0)
  
  colnames(data)[1:2] <- c("id", "occupation")
  
  data <- data[,c("id", "occupation",
                  "organizing", "researching", "constructing", "programming", "applying_law", # analytic 
                  "teaching", "consulting", "buying", "managing", # interactive
                  "repairing", "caring", "accomodating", "protecting", "cleaning", # non-routine manual
                  "writing", "calculating", # routine cognitive
                  "operating", "manufacturing", "storing")] # routine manual
  
  # remove NA
  
  na <- is.na(data)
  sum(na)/sum(1-na)
  
  data <- data[complete.cases(data),]
  
  # repair occupation codes (curr. coded as long and without 0 at the beginning)
  
  data$occupation <- as.character(data$occupation)
  data_1 <- data[(nchar(as.character(data$occupation)) == 2),]
  data_2 <- data[(nchar(as.character(data$occupation)) == 3),]
  
  data_1$occupation <- paste0("0", data_1$occupation)
  
  data <- rbind(data_1, data_2)
  rm(data_1, data_2)
  
  # create task measures
  
  data$analytic <- ((data$organizing + data$researching + data$constructing + data$programming + data$applying_law)/5)*100
  
  data$interactive <- ((data$teaching + data$consulting + data$buying + data$managing)/4)*100
  
  data$nonroutine_cognitive <- ((data$organizing + data$researching + data$constructing + data$programming + data$applying_law + data$teaching + data$consulting + data$buying + data$managing)/9)*100
  
  data$nonroutine_manual <- ((data$repairing + data$caring + data$accomodating + data$protecting + data$cleaning)/5)*100
  
  data$routine_cognitive <- ((data$writing + data$calculating)/2)*100
  
  data$routine_manual <- ((data$operating + data$manufacturing + data$storing)/3)*100
  
  data <- data[,c("id", "occupation", "analytic", "interactive", "nonroutine_cognitive", "nonroutine_manual", "routine_cognitive", "routine_manual")]
  
  analytic <- aggregate(data$analytic, by=list(occupation=data$occupation), FUN=mean)
  colnames(analytic)[2] <- "analytic"

  interactive <- aggregate(data$interactive, by=list(occupation=data$occupation), FUN=mean)
  colnames(interactive)[2] <- "interactive"
  
  nonroutine_cognitive <- aggregate(data$nonroutine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_cognitive)[2] <- "nonroutine_cognitive"
  
  nonroutine_manual <- aggregate(data$nonroutine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_manual)[2] <- "nonroutine_manual"
  
  routine_cognitive <- aggregate(data$routine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_cognitive)[2] <- "routine_cognitive"
  
  routine_manual <- aggregate(data$routine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_manual)[2] <- "routine_manual"
  
  task_measures_1986 <- merge(analytic, interactive, by="occupation")
  task_measures_1986 <- merge(task_measures_1986, nonroutine_manual, by="occupation")
  task_measures_1986 <- merge(task_measures_1986, nonroutine_cognitive, by="occupation")
  task_measures_1986 <- merge(task_measures_1986, routine_cognitive, by="occupation")
  task_measures_1986 <- merge(task_measures_1986, routine_manual, by="occupation")
  
  rm(analytic, interactive, nonroutine_cognitive, nonroutine_manual, routine_cognitive, routine_manual, data)

######################## Wave 1992 ######################## 
  
  data <- read_dta("../generated_data/bibb1992_har.dta")
  
  data <- data[,c(2,17,38:85)]
  
  for(i in 3:ncol(data)){
    data[[i]] <- as.numeric(data[[i]])
  }
  
  data$organizing <- ifelse(data$v64==1,1,0)
  
  data$researching <- ifelse(data$v52==1,1,
                             ifelse(data$v53==1,1,0))
  
  data$constructing <- ifelse(data$v53==1,1,0)
  
  data$programming <- ifelse(data$v57==1,1,0)
  
  data$applying_law <- ifelse(data$v59==1,1,0)
  
  data$teaching <- ifelse(data$v60==1,1,0)
  
  data$consulting <- ifelse(data$v60==1, 1,
                            ifelse(data$v61==1, 1,
                                   ifelse(data$v62==1, 1,0)))
  
  data$buying <- ifelse(data$v54==1,1,0)
  
  data$promoting <- ifelse(data$v85==1,1,0)
  
  data$managing <- ifelse(data$v63==1,1,0)
  
  data$repairing <- ifelse(data$v40==1,1,
                           ifelse(data$v41==1,1,
                                  ifelse(data$v42==1,1,0)))
  
  data$caring <- ifelse(data$v61==1,1,0)
  
  data$accomodating <- ifelse(data$v47==1,1,0)
  
  data$protecting <- ifelse(data$v58==1,1,0)
  
  data$cleaning <- ifelse(data$v48==1,1,
                          ifelse(data$v49==1,1,0))
  
  data$writing <- ifelse(data$v55==1,1,0)
  
  data$calculating <- ifelse(data$v56==1,1,0)
  
  data$operating <- ifelse(data$v38==1, 1,
                           ifelse(data$v39==1, 1,0))
  
  data$manufacturing <- ifelse(data$v43==1, 1,
                               ifelse(data$v44==1, 1,
                                      ifelse(data$v45==1, 1,
                                             ifelse(data$v46==1, 1,0))))
  
  data$storing <- ifelse(data$v50==1,1,
                         ifelse(data$v51==1,1,
                                ifelse(data$v41==1,1,0)))
  
  colnames(data)[1:2] <- c("id", "occupation")
  
  data <- data[,c("id", "occupation",
                  "organizing", "researching", "constructing", "programming", "applying_law", # analytic  
                  "teaching", "consulting", "promoting", "buying", "managing", # interactive      
                  "repairing", "caring", "accomodating", "protecting", "cleaning", # non-routine manual   
                  "calculating", "writing", # routine cognitive
                  "operating", "manufacturing", "storing")] # routine manual
  # remove NA
  
  na <- is.na(data)
  sum(na)/sum(1-na)
  
  data <- data[complete.cases(data),]
  
  # repair occupation codes (curr. coded as long and without 0 at the beginning)
  
  data$occupation <- as.character(data$occupation)
  data_1 <- data[(nchar(as.character(data$occupation)) == 2),]
  data_2 <- data[(nchar(as.character(data$occupation)) == 3),]
  
  data_1$occupation <- paste0("0", data_1$occupation)
  
  data <- rbind(data_1, data_2)
  rm(data_1, data_2)
  
  # create task measures
  
  data$analytic <- ((data$organizing + data$researching + data$constructing + data$programming + data$applying_law)/5)*100
  
  data$interactive <- ((data$teaching + data$consulting + data$promoting + data$buying + data$managing)/5)*100
  
  data$nonroutine_cognitive <- ((data$organizing + data$researching + data$constructing + data$programming + data$applying_law + data$teaching + data$consulting + data$promoting + data$buying + data$managing)/11)*100
  
  data$nonroutine_manual <- ((data$repairing + data$caring + data$accomodating + data$protecting + data$cleaning)/5)*100
  
  data$routine_cognitive <- ((data$writing + data$calculating)/2)*100
  
  data$routine_manual <- ((data$operating + data$manufacturing + data$storing)/3)*100
  
  data <- data[,c("id", "occupation", "analytic", "nonroutine_cognitive", "interactive", "nonroutine_manual", "routine_cognitive", "routine_manual")]
  
  analytic <- aggregate(data$analytic, by=list(occupation=data$occupation), FUN=mean)
  colnames(analytic)[2] <- "analytic"

  interactive <- aggregate(data$interactive, by=list(occupation=data$occupation), FUN=mean)
  colnames(interactive)[2] <- "interactive"
  
  nonroutine_cognitive <- aggregate(data$nonroutine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_cognitive)[2] <- "nonroutine_cognitive"
  
  nonroutine_manual <- aggregate(data$nonroutine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_manual)[2] <- "nonroutine_manual"
  
  routine_cognitive <- aggregate(data$routine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_cognitive)[2] <- "routine_cognitive"
  
  routine_manual <- aggregate(data$routine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_manual)[2] <- "routine_manual"
  
  task_measures_1992 <- merge(analytic, interactive, by="occupation")
  task_measures_1992 <- merge(task_measures_1992, nonroutine_cognitive, by="occupation")
  task_measures_1992 <- merge(task_measures_1992, nonroutine_manual, by="occupation")
  task_measures_1992 <- merge(task_measures_1992, routine_cognitive, by="occupation")
  task_measures_1992 <- merge(task_measures_1992, routine_manual, by="occupation")
  
  rm(analytic, interactive, nonroutine_cognitive, nonroutine_manual, routine_cognitive, routine_manual, data)

######################## Wave 1999 ######################## 
  
  data <- read_dta("../generated_data/bibb1999_har.dta")
  
  data <- data[,c(2,13,189:229)]
  
  for(i in 3:ncol(data)){
    data[[i]] <- as.numeric(data[[i]])
  }
  
  data[data==2] <- 1
  data[data==3] <- 0
  
  data$organizing <- ifelse(data$v195==1,1,0)
  
  data$investigating <- ifelse(data$v197==1,1,0)
  
  data$researching <- ifelse(data$v199==1,1,0)
  
  data$teaching <- ifelse(data$v189==1,1,0)
  
  data$consulting <- ifelse(data$v190==1,1,0)
  
  data$buying <- ifelse(data$v194==1,1,0)
  
  data$promoting <- ifelse(data$v196==1,1,0)
  
  data$negotiating <- ifelse(data$v198==1,1,0)
  
  data$repairing <- ifelse(data$v193==1,1,0)
  
  data$caring <- ifelse(data$v201==1,1,
                        ifelse(data$v229==1,1,0))
  
  data$measuring <- ifelse(data$v191==1,1,0)
  
  data$operating <- ifelse(data$v192==1,1,0)
  
  data$manufacturing <- ifelse(data$v200==1,1,0)
  
  colnames(data)[1:2] <- c("id", "occupation")
  
  data <- data[,c("id", "occupation",
                  "organizing", "investigating", "researching", # analytic
                  "teaching", "consulting", "buying", "promoting", "negotiating", # interactive
                  "repairing", "caring" , # nonroutine manual
                  "measuring", # routine cognitive
                  "operating", "manufacturing")] # routine manual
  
  # remove NA
  
  na <- is.na(data)
  sum(na)/sum(1-na)
  
  data <- data[complete.cases(data),]
  
  class(data$occupation)
  
  # repair occupation codes (curr. coded as long and without 0 at the beginning)
  
  data$occupation <- as.character(data$occupation)
  data_1 <- data[(nchar(as.character(data$occupation)) == 2),]
  data_2 <- data[(nchar(as.character(data$occupation)) == 3),]
  
  data_1$occupation <- paste0("0", data_1$occupation)
  
  data <- rbind(data_1, data_2)
  rm(data_1, data_2)
  
  # create task measures
  
  data$analytic <- ((data$organizing + data$researching + data$investigating)/3)*100
                     
  data$interactive <- ((data$teaching + data$consulting + data$buying + data$promoting + data$negotiating)/5)*100
  
  data$nonroutine_cognitive <- ((data$organizing + data$researching + data$investigating + data$teaching + data$consulting + data$buying + data$promoting + data$negotiating)/8)*100
  
  data$nonroutine_manual <- ((data$repairing + data$caring)/2)*100
  
  data$routine_cognitive <- ((data$measuring)/1)*100
  
  data$routine_manual <- ((data$operating + data$manufacturing)/2)*100
  
  data <- data[,c("id", "occupation", "analytic", "interactive", "nonroutine_cognitive", "nonroutine_manual", "routine_cognitive", "routine_manual")]
  
  analytic <- aggregate(data$analytic, by=list(occupation=data$occupation), FUN=mean)
  colnames(analytic)[2] <- "analytic"

  interactive <- aggregate(data$interactive, by=list(occupation=data$occupation), FUN=mean)
  colnames(interactive)[2] <- "interactive"
  
  nonroutine_cognitive <- aggregate(data$nonroutine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_cognitive)[2] <- "nonroutine_cognitive"
  
  nonroutine_manual <- aggregate(data$nonroutine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_manual)[2] <- "nonroutine_manual"
  
  routine_cognitive <- aggregate(data$routine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_cognitive)[2] <- "routine_cognitive"
  
  routine_manual <- aggregate(data$routine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_manual)[2] <- "routine_manual"
  
  task_measures_1999 <- merge(analytic, interactive, by="occupation")
  task_measures_1999 <- merge(task_measures_1999, nonroutine_cognitive, by="occupation")
  task_measures_1999 <- merge(task_measures_1999, nonroutine_manual, by="occupation")
  task_measures_1999 <- merge(task_measures_1999, routine_cognitive, by="occupation")
  task_measures_1999 <- merge(task_measures_1999, routine_manual, by="occupation")
  
  rm(analytic, interactive, nonroutine_manual, nonroutine_cognitive, routine_cognitive, routine_manual, data)

######################## Wave 2006 ######################## 
  
  data <- read_dta("../generated_data/bibb2006_har.dta")

  data <- data[,c("idnum", "EB1_92o",
                  "f310", # organizing
                  "f313", # investigating
                  "f311", # researching
                  "f322_01", # programming
                  "f312", # teaching
                  "f314", # consulting
                  "f307", # buying
                  "f309", # promoting
                  "f306", # repairing
                  "f316", # caring
                  "f315", # accomodating
                  "f317", # protecting
                  "f304", # measuring
                  "f305", # operating
                  "f303", # manufacturing
                  "f308")] # storing
  
  for (i in 3:ncol(data)){
    data[[i]][data[[i]]==9] <- NA
  }
  unique(data$f310)
  
  unique(data$f322_01)
  data$f322_01[data$f322_01==2] <- 0
  unique(data$f322_01)
  
  data$f322_01 <- ifelse(is.na(data$f322_01), 0, data$f322_01)
  unique(data$f322_01)
  
  data[data==2] <- 1
  data[data==3] <- 0
  
  for(i in 3:ncol(data)){
    data[[i]] <- as.numeric(data[[i]])
  }
  
  colnames(data) <- c("id", "occupation",
                      "organizing",
                      "investigating",
                      "researching",
                      "programming",
                      "teaching",
                      "consulting",
                      "buying",
                      "promoting",
                      "repairing",
                      "caring",
                      "accomodating",
                      "protecting",
                      "measuring",
                      "operating",
                      "manufacturing",
                      "storing")
  
  # remove NA
  
  na <- is.na(data)
  sum(na)/sum(1-na)
  
  data <- data[complete.cases(data),]
  
  na <- is.na(data)
  sum(na)/sum(1-na)
  
  # repair occupation codes (curr. coded as long and without 0 at the beginning)
  
  data$occupation <- as.character(data$occupation)
  data_1 <- data[(nchar(as.character(data$occupation)) == 2),]
  data_2 <- data[(nchar(as.character(data$occupation)) == 3),]
  
  data_1 <- subset(data_1, occupation!="-1")
  data_1$occupation <- paste0("0", data_1$occupation)
  
  data <- rbind(data_1, data_2)
  rm(data_1, data_2)
  
  # create task measures
  
  data$analytic <- ((data$organizing + data$researching + data$investigating + data$programming)/4)*100
  
  data$interactive <- ((data$teaching + data$consulting + data$buying + data$promoting)/4)*100
 
  data$nonroutine_cognitive <- ((data$organizing + data$researching + data$investigating + data$programming + data$teaching + data$consulting + data$buying + data$promoting)/8)*100
  
  data$nonroutine_manual <- ((data$repairing + data$caring + data$accomodating + data$protecting)/4)*100
  
  data$routine_cognitive <- ((data$measuring)/1)*100
  
  data$routine_manual <- ((data$operating + data$manufacturing + data$storing)/3)*100
  
  data <- data[,c("id", "occupation", "analytic", "interactive", "nonroutine_cognitive", "nonroutine_manual", "routine_cognitive", "routine_manual")]
  
  analytic <- aggregate(data$analytic, by=list(occupation=data$occupation), FUN=mean)
  colnames(analytic)[2] <- "analytic"

  interactive <- aggregate(data$interactive, by=list(occupation=data$occupation), FUN=mean)
  colnames(interactive)[2] <- "interactive"
  
  nonroutine_cognitive <- aggregate(data$nonroutine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_cognitive)[2] <- "nonroutine_cognitive"
  
  nonroutine_manual <- aggregate(data$nonroutine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_manual)[2] <- "nonroutine_manual"
  
  routine_cognitive <- aggregate(data$routine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_cognitive)[2] <- "routine_cognitive"
  
  routine_manual <- aggregate(data$routine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_manual)[2] <- "routine_manual"
  
  task_measures_2006 <- merge(analytic, interactive, by="occupation")
  task_measures_2006 <- merge(task_measures_2006, nonroutine_cognitive, by="occupation")
  task_measures_2006 <- merge(task_measures_2006, nonroutine_manual, by="occupation")
  task_measures_2006 <- merge(task_measures_2006, routine_cognitive, by="occupation")
  task_measures_2006 <- merge(task_measures_2006, routine_manual, by="occupation")
  
  rm(analytic, interactive, nonroutine_cognitive, nonroutine_manual, routine_cognitive, routine_manual, data)

######################## Wave 2012 ######################## 
  
  data <- read_dta("../generated_data/bibb2012_har.dta")
  
  data <- data[,c("Intnr", "EB92o",
                  "F310", # organizing
                  "F313", # investigating
                  "F311", # researching
                  "F325_01", # programming
                  "F312", # teaching
                  "F314", # consulting
                  "F307", # buying
                  "F309", # promoting
                  "F306", # repairing
                  "F316", # caring
                  "F315", # accomodating
                  "F317", # protecting
                  "F304", # measuring
                  "F305", # operating
                  "F303", # manufacturing
                  "F308")] # storing
  
  for (i in 3:ncol(data)){
    data[[i]][data[[i]]==9] <- NA
  }
  
  unique(data$F310)
  
  unique(data$F325_01)
  data$F325_01[data$F325_01==2] <- 0
  unique(data$F325_01)
  
  data$F325_01 <- ifelse(is.na(data$F325_01), 0, data$F325_01)
  unique(data$F325_01)

  data[data==2] <- 1
  data[data==3] <- 0
  
  for(i in 3:ncol(data)){
    data[[i]] <- as.numeric(data[[i]])
  }
  
  colnames(data) <- c("id", "occupation",
                      "organizing",
                      "investigating",
                      "researching",
                      "programming",
                      "teaching",
                      "consulting",
                      "buying",
                      "promoting",
                      "repairing",
                      "caring",
                      "accomodating",
                      "protecting",
                      "measuring",
                      "operating",
                      "manufacturing",
                      "storing")
  
  # remove NA
  
  na <- is.na(data)
  sum(na)/sum(1-na)
  
  data <- data[complete.cases(data),]
  
  na <- is.na(data)
  sum(na)/sum(1-na)
  
  # repair occupation codes (curr. coded as long and without 0 at the beginning)
  
  data$occupation <- as.character(data$occupation)
  data_1 <- data[(nchar(as.character(data$occupation)) == 2),]
  data_2 <- data[(nchar(as.character(data$occupation)) == 3),]
  
  data_1 <- subset(data_1, occupation!="-1")
  data_1$occupation <- paste0("0", data_1$occupation)
  
  data <- rbind(data_1, data_2)
  rm(data_1, data_2)
  
  # create task measures
  
  data$analytic <- ((data$organizing + data$researching + data$investigating + data$programming)/4)*100
  
  data$interactive <- ((data$teaching + data$consulting + data$buying + data$promoting)/4)*100
  
  data$nonroutine_cognitive <- ((data$organizing + data$researching + data$investigating + data$programming + data$teaching + data$consulting + data$buying + data$promoting)/8)*100
  
  data$nonroutine_manual <- ((data$repairing + data$caring + data$accomodating + data$protecting)/4)*100
  
  data$routine_cognitive <- ((data$measuring)/1)*100
  
  data$routine_manual <- ((data$operating + data$manufacturing + data$storing)/3)*100
  
  data <- data[,c("id", "occupation", "analytic", "interactive", "nonroutine_cognitive", "nonroutine_manual", "routine_cognitive", "routine_manual")]
  
  analytic <- aggregate(data$analytic, by=list(occupation=data$occupation), FUN=mean)
  colnames(analytic)[2] <- "analytic"

  interactive <- aggregate(data$interactive, by=list(occupation=data$occupation), FUN=mean)
  colnames(interactive)[2] <- "interactive"
  
  nonroutine_cognitive <- aggregate(data$nonroutine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_cognitive)[2] <- "nonroutine_cognitive"
  
  nonroutine_manual <- aggregate(data$nonroutine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_manual)[2] <- "nonroutine_manual"
  
  routine_cognitive <- aggregate(data$routine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_cognitive)[2] <- "routine_cognitive"
  
  routine_manual <- aggregate(data$routine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_manual)[2] <- "routine_manual"
  
  task_measures_2012 <- merge(analytic, interactive, by="occupation")
  task_measures_2012 <- merge(task_measures_2012, nonroutine_cognitive, by="occupation")
  task_measures_2012 <- merge(task_measures_2012, nonroutine_manual, by="occupation")
  task_measures_2012 <- merge(task_measures_2012, routine_cognitive, by="occupation")
  task_measures_2012 <- merge(task_measures_2012, routine_manual, by="occupation")
  
  rm(analytic, interactive, nonroutine_cognitive, nonroutine_manual, routine_cognitive, routine_manual, data)

######################## Wave 2018 ######################## 
  
  data <- read_dta("../generated_data/bibb2018_har.dta")
  
  data <- data[,c("intnr", "F100_kldb92_3d",
                "F310", # organizing
                "F313", # investigating
                "F311", # researching
                "F325_01", # programming
                "F312", # teaching
                "F314", # consulting
                "F307", # buying
                "F309", # promoting
                "F306", # repairing
                "F316", # caring
                "F315", # accomodating
                "F317", # protecting
                "F304", # measuring
                "F305", # operating
                "F303", # manufacturing
                "F308")] # storing

  for (i in 3:ncol(data)){
    data[[i]][data[[i]]==9] <- NA
  }
  
  unique(data$F310)
  
  unique(data$F325_01)
  data$F325_01[data$F325_01==2] <- 0
  unique(data$F325_01)
  
  data$F325_01 <- ifelse(is.na(data$F325_01), 0, data$F325_01)
  unique(data$F325_01)

  data[data==2] <- 1
  data[data==3] <- 0

  for(i in 3:ncol(data)){
  data[[i]] <- as.numeric(data[[i]])
}

  colnames(data) <- c("id", "occupation",
                    "organizing",
                    "investigating",
                    "researching",
                    "programming",
                    "teaching",
                    "consulting",
                    "buying",
                    "promoting",
                    "repairing",
                    "caring",
                    "accomodating",
                    "protecting",
                    "measuring",
                    "operating",
                    "manufacturing",
                    "storing")
  
  # remove NA
  
  na <- is.na(data)
  sum(na)/sum(1-na)
  
  data <- data[complete.cases(data),]
  
  na <- is.na(data)
  sum(na)/sum(1-na)
  
  # repair occupation codes (curr. coded as long and without 0 at the beginning)
  
  data$occupation <- as.character(data$occupation)
  data_1 <- data[(nchar(as.character(data$occupation)) == 2),]
  data_2 <- data[(nchar(as.character(data$occupation)) == 3),]
  
  data_1 <- subset(data_1, occupation!="-1")
  data_1$occupation <- paste0("0", data_1$occupation)
  
  data <- rbind(data_1, data_2)
  rm(data_1, data_2)
  
  # create task measures
  
  data$analytic <- ((data$organizing + data$researching + data$investigating + data$programming)/4)*100
  
  data$interactive <- ((data$teaching + data$consulting + data$buying + data$promoting)/4)*100
  
  data$nonroutine_cognitive <- ((data$organizing + data$researching + data$investigating + data$programming + data$teaching + data$consulting + data$buying + data$promoting)/8)*100
  
  data$nonroutine_manual <- ((data$repairing + data$caring + data$accomodating + data$protecting)/4)*100
  
  data$routine_cognitive <- ((data$measuring)/1)*100
  
  data$routine_manual <- ((data$operating + data$manufacturing + data$storing)/3)*100
  
  data <- data[,c("id", "occupation", "analytic", "interactive", "nonroutine_cognitive", "nonroutine_manual", "routine_cognitive", "routine_manual")]
  
  analytic <- aggregate(data$analytic, by=list(occupation=data$occupation), FUN=mean)
  colnames(analytic)[2] <- "analytic"

  interactive <- aggregate(data$interactive, by=list(occupation=data$occupation), FUN=mean)
  colnames(interactive)[2] <- "interactive"
  
  nonroutine_cognitive <- aggregate(data$nonroutine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_cognitive)[2] <- "nonroutine_cognitive"
  
  nonroutine_manual <- aggregate(data$nonroutine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(nonroutine_manual)[2] <- "nonroutine_manual"
  
  routine_cognitive <- aggregate(data$routine_cognitive, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_cognitive)[2] <- "routine_cognitive"
  
  routine_manual <- aggregate(data$routine_manual, by=list(occupation=data$occupation), FUN=mean)
  colnames(routine_manual)[2] <- "routine_manual"
  
  task_measures_2018 <- merge(analytic, interactive, by="occupation")
  task_measures_2018 <- merge(task_measures_2018, nonroutine_cognitive, by="occupation")
  task_measures_2018 <- merge(task_measures_2018, nonroutine_manual, by="occupation")
  task_measures_2018 <- merge(task_measures_2018, routine_cognitive, by="occupation")
  task_measures_2018 <- merge(task_measures_2018, routine_manual, by="occupation")
  
  rm(analytic, interactive, nonroutine_cognitive, nonroutine_manual, routine_cognitive, routine_manual, data)

######################## Prepare for merging with the SOEP ########################

rm(list=setdiff(ls(), c("task_measures_1979", 
                        "task_measures_1986",
                        "task_measures_1992",
                        "task_measures_1999",
                        "task_measures_2006",
                        "task_measures_2012",
                        "task_measures_2018")))
  
task_measures_2012$occupation <- str_replace_all(task_measures_2012$occupation, "-", "")
task_measures_2012 <- subset(task_measures_2012, nchar(occupation)==3)

task_measures_2018$occupation <- str_replace_all(task_measures_2018$occupation, "-", "")
task_measures_2018 <- subset(task_measures_2018, nchar(occupation)==3)

colnames(task_measures_1979)[1] <- "kldb_1988_3d"
colnames(task_measures_1986)[1] <- "kldb_1988_3d"
colnames(task_measures_1992)[1] <- "kldb_1988_3d"
colnames(task_measures_1999)[1] <- "kldb_1992_3d"
colnames(task_measures_2006)[1] <- "kldb_1992_3d"
colnames(task_measures_2012)[1] <- "kldb_1992_3d"
colnames(task_measures_2018)[1] <- "kldb_1992_3d"

crosswalk <- read_excel("../original_data/kldb_1988_1992.xls")
crosswalk$kldb_1988 <- gsub('.{1}$', '', crosswalk$kldb_1988)
crosswalk$kldb_1992 <- gsub('.{1}$', '', crosswalk$kldb_1992)
crosswalk <- unique(crosswalk)
crosswalk <- crosswalk[with(crosswalk, order(kldb_1988, kldb_1992)),]

length(unique(crosswalk$kldb_1988))
length(unique(crosswalk$kldb_1992))

temp_1 <- crosswalk %>% count(kldb_1988)
temp_2 <- crosswalk %>% count(kldb_1992)

colnames(crosswalk) <- c("kldb_1988_3d", "kldb_1992_3d")

# recode the file from 1979 from kldb 1988 to kldb 1992

task_measures_1979 <- merge(task_measures_1979, crosswalk, by="kldb_1988_3d")

analytic <- task_measures_1979[,c("kldb_1992_3d", "kldb_1988_3d", "analytic")]
analytic <- aggregate(analytic$analytic, by=list(kldb_1992_3d=analytic$kldb_1992_3d), FUN=mean)
colnames(analytic)[2] <- "analytic"

interactive <- task_measures_1979[,c("kldb_1992_3d", "kldb_1988_3d", "interactive")]
interactive <- aggregate(interactive$interactive, by=list(kldb_1992_3d=interactive$kldb_1992_3d), FUN=mean)
colnames(interactive)[2] <- "interactive"

nonroutine_cognitive <- task_measures_1979[,c("kldb_1992_3d", "kldb_1988_3d", "nonroutine_cognitive")]
nonroutine_cognitive <- aggregate(nonroutine_cognitive$nonroutine_cognitive, by=list(kldb_1992_3d=nonroutine_cognitive$kldb_1992_3d), FUN=mean)
colnames(nonroutine_cognitive)[2] <- "nonroutine_cognitive"

nonroutine_manual <- task_measures_1979[,c("kldb_1992_3d", "kldb_1988_3d", "nonroutine_manual")]
nonroutine_manual <- aggregate(nonroutine_manual$nonroutine_manual, by=list(kldb_1992_3d=nonroutine_manual$kldb_1992_3d), FUN=mean)
colnames(nonroutine_manual)[2] <- "nonroutine_manual"

routine_cognitive <- task_measures_1979[,c("kldb_1992_3d", "kldb_1988_3d", "routine_cognitive")]
routine_cognitive <- aggregate(routine_cognitive$routine_cognitive, by=list(kldb_1992_3d=routine_cognitive$kldb_1992_3d), FUN=mean)
colnames(routine_cognitive)[2] <- "routine_cognitive"

routine_manual <- task_measures_1979[,c("kldb_1992_3d", "kldb_1988_3d", "routine_manual")]
routine_manual <- aggregate(routine_manual$routine_manual, by=list(kldb_1992_3d=routine_manual$kldb_1992_3d), FUN=mean)
colnames(routine_manual)[2] <- "routine_manual"

task_measures_1979 <- merge(analytic, interactive, by="kldb_1992_3d")
task_measures_1979 <- merge(task_measures_1979, nonroutine_cognitive, by="kldb_1992_3d")
task_measures_1979 <- merge(task_measures_1979, nonroutine_manual, by="kldb_1992_3d")
task_measures_1979 <- merge(task_measures_1979, routine_cognitive, by="kldb_1992_3d")
task_measures_1979 <- merge(task_measures_1979, routine_manual, by="kldb_1992_3d")

# recode the file from 1986 from kldb 1988 to kldb 1992

task_measures_1986 <- merge(task_measures_1986, crosswalk, by="kldb_1988_3d")

analytic <- task_measures_1986[,c("kldb_1992_3d", "kldb_1988_3d", "analytic")]
analytic <- aggregate(analytic$analytic, by=list(kldb_1992_3d=analytic$kldb_1992_3d), FUN=mean)
colnames(analytic)[2] <- "analytic"

interactive <- task_measures_1986[,c("kldb_1992_3d", "kldb_1988_3d", "interactive")]
interactive <- aggregate(interactive$interactive, by=list(kldb_1992_3d=interactive$kldb_1992_3d), FUN=mean)
colnames(interactive)[2] <- "interactive"

nonroutine_cognitive <- task_measures_1986[,c("kldb_1992_3d", "kldb_1988_3d", "nonroutine_cognitive")]
nonroutine_cognitive <- aggregate(nonroutine_cognitive$nonroutine_cognitive, by=list(kldb_1992_3d=nonroutine_cognitive$kldb_1992_3d), FUN=mean)
colnames(nonroutine_cognitive)[2] <- "nonroutine_cognitive"

nonroutine_manual <- task_measures_1986[,c("kldb_1992_3d", "kldb_1988_3d", "nonroutine_manual")]
nonroutine_manual <- aggregate(nonroutine_manual$nonroutine_manual, by=list(kldb_1992_3d=nonroutine_manual$kldb_1992_3d), FUN=mean)
colnames(nonroutine_manual)[2] <- "nonroutine_manual"

routine_cognitive <- task_measures_1986[,c("kldb_1992_3d", "kldb_1988_3d", "routine_cognitive")]
routine_cognitive <- aggregate(routine_cognitive$routine_cognitive, by=list(kldb_1992_3d=routine_cognitive$kldb_1992_3d), FUN=mean)
colnames(routine_cognitive)[2] <- "routine_cognitive"

routine_manual <- task_measures_1986[,c("kldb_1992_3d", "kldb_1988_3d", "routine_manual")]
routine_manual <- aggregate(routine_manual$routine_manual, by=list(kldb_1992_3d=routine_manual$kldb_1992_3d), FUN=mean)
colnames(routine_manual)[2] <- "routine_manual"

task_measures_1986 <- merge(analytic, interactive, by="kldb_1992_3d")
task_measures_1986 <- merge(task_measures_1986, nonroutine_cognitive, by="kldb_1992_3d")
task_measures_1986 <- merge(task_measures_1986, nonroutine_manual, by="kldb_1992_3d")
task_measures_1986 <- merge(task_measures_1986, routine_cognitive, by="kldb_1992_3d")
task_measures_1986 <- merge(task_measures_1986, routine_manual, by="kldb_1992_3d")

# recode the file from 1992 from kldb 1988 to kldb 1992

task_measures_1992 <- merge(task_measures_1992, crosswalk, by="kldb_1988_3d")

analytic <- task_measures_1992[,c("kldb_1992_3d", "kldb_1988_3d", "analytic")]
analytic <- aggregate(analytic$analytic, by=list(kldb_1992_3d=analytic$kldb_1992_3d), FUN=mean)
colnames(analytic)[2] <- "analytic"

interactive <- task_measures_1992[,c("kldb_1992_3d", "kldb_1988_3d", "interactive")]
interactive <- aggregate(interactive$interactive, by=list(kldb_1992_3d=interactive$kldb_1992_3d), FUN=mean)
colnames(interactive)[2] <- "interactive"

nonroutine_cognitive <- task_measures_1992[,c("kldb_1992_3d", "kldb_1988_3d", "nonroutine_cognitive")]
nonroutine_cognitive <- aggregate(nonroutine_cognitive$nonroutine_cognitive, by=list(kldb_1992_3d=nonroutine_cognitive$kldb_1992_3d), FUN=mean)
colnames(nonroutine_cognitive)[2] <- "nonroutine_cognitive"

nonroutine_manual <- task_measures_1992[,c("kldb_1992_3d", "kldb_1988_3d", "nonroutine_manual")]
nonroutine_manual <- aggregate(nonroutine_manual$nonroutine_manual, by=list(kldb_1992_3d=nonroutine_manual$kldb_1992_3d), FUN=mean)
colnames(nonroutine_manual)[2] <- "nonroutine_manual"

routine_cognitive <- task_measures_1992[,c("kldb_1992_3d", "kldb_1988_3d", "routine_cognitive")]
routine_cognitive <- aggregate(routine_cognitive$routine_cognitive, by=list(kldb_1992_3d=routine_cognitive$kldb_1992_3d), FUN=mean)
colnames(routine_cognitive)[2] <- "routine_cognitive"

routine_manual <- task_measures_1992[,c("kldb_1992_3d", "kldb_1988_3d", "routine_manual")]
routine_manual <- aggregate(routine_manual$routine_manual, by=list(kldb_1992_3d=routine_manual$kldb_1992_3d), FUN=mean)
colnames(routine_manual)[2] <- "routine_manual"

task_measures_1992 <- merge(analytic, interactive, by="kldb_1992_3d")
task_measures_1992 <- merge(task_measures_1992, nonroutine_cognitive, by="kldb_1992_3d")
task_measures_1992 <- merge(task_measures_1992, nonroutine_manual, by="kldb_1992_3d")
task_measures_1992 <- merge(task_measures_1992, routine_cognitive, by="kldb_1992_3d")
task_measures_1992 <- merge(task_measures_1992, routine_manual, by="kldb_1992_3d")

rm(analytic, interactive, nonroutine_manual, nonroutine_cognitive, routine_cognitive, routine_manual, temp_1, temp_2, crosswalk)

rm(list=setdiff(ls(), c("task_measures_1979",
                        "task_measures_1986",
                        "task_measures_1992",
                        "task_measures_1999",
                        "task_measures_2006",
                        "task_measures_2012",
                        "task_measures_2018")))

######################## Complete the time series ########################

# keep only the analytic and interactive categories
task_measures_1979 <- task_measures_1979[,c(1:3)]
task_measures_1986 <- task_measures_1986[,c(1:3)]
task_measures_1992 <- task_measures_1992[,c(1:3)]
task_measures_1999 <- task_measures_1999[,c(1:3)]
task_measures_2006 <- task_measures_2006[,c(1:3)]
task_measures_2012 <- task_measures_2012[,c(1:3)]
task_measures_2018 <- task_measures_2018[,c(1:3)]

task_measures_1979$year <- 1979
task_measures_1986$year <- 1986
task_measures_1992$year <- 1992
task_measures_1999$year <- 1999
task_measures_2006$year <- 2006
task_measures_2012$year <- 2012
task_measures_2018$year <- 2018

task_measures <- rbind(task_measures_1979,
                       task_measures_1986,
                       task_measures_1992,
                       task_measures_1999,
                       task_measures_2006,
                       task_measures_2012,
                       task_measures_2018)

rm(list=setdiff(ls(), c("task_measures")))

task_measures <- task_measures[with(task_measures, order(kldb_1992_3d, year)),]
row.names(task_measures) <- NULL

# expand to include all combinations of occupation-year
occupations <- unique(task_measures$kldb_1992_3d)
kldb_1992_3d <- data.frame(kldb_1992_3d=rep(occupations, 40))
kldb_1992_3d <- kldb_1992_3d[with(kldb_1992_3d, order(kldb_1992_3d)),]

year <- seq(1979,2018,1)
combinations <- data.frame(kldb_1992_3d,year)
task_measures <- merge(combinations, task_measures, by=c("kldb_1992_3d", "year"), all.x=T)
rm(combinations)
task_measures <- task_measures[with(task_measures, order(kldb_1992_3d, year)),]

rm(list=setdiff(ls(), c("task_measures", "year")))
colnames(task_measures)[1] <- "occupation"

df_1 <- data.frame()
df_2 <- data.frame()

for (occupation_ in unique(task_measures$occupation)){
  temp <- task_measures %>% filter(occupation==occupation_)
  if ((nrow(temp) < 2) | (colSums(is.na(temp))[3]>38 | colSums(is.na(temp))[4]>38)) {
    df_1 <- rbind(df_1, temp)
  }else{

    temp$analytic_cleaned <- na_interpolation(temp$analytic, "linear")
    temp$interactive_cleaned <- na_interpolation(temp$interactive, "linear")
    temp <- temp[,c("occupation", "year", "analytic_cleaned", "interactive_cleaned")]
    df_2 <- rbind(df_2, temp)

  }
}

task_measures <- df_2
colnames(task_measures) <- c("kldb_1992_3d", "year", "analytic", "interactive")
sum(is.na(task_measures))/(1-sum(is.na(task_measures)))
rm(list=setdiff(ls(), c("task_measures")))

# create 2nd lags

df <- data.frame()

for (o in unique(task_measures$kldb_1992_3d)){
  temp <- task_measures %>% filter(kldb_1992_3d==o)
  temp$l1_analytic <- Lag(temp$analytic, 1)
  temp$l2_analytic <- Lag(temp$analytic, 2)
  temp$l1_interactive <- Lag(temp$interactive, 1)
  temp$l2_interactive <- Lag(temp$interactive, 2)
  df <- rbind(df, temp)
}

task_measures <- df

save(task_measures, file="../generated_data/task_measures.RData")

rm(list=ls())
gc()