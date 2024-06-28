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
                     "survminer", "zoo", "imputeTS", "extrafont", "remotes")
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }

rm(list=ls())
gc()
}

# length(unique(ppathl$pid))
# length(unique(biobirth$pid))
# length(unique(pgen$pid))
# length(unique(biol$pid))

# 1. Load and merge SOEP

## load the biobirth dataset (bibliographical information about child birth)
biobirth <- read_dta("../original_data/SOEP-CORE.v37eu_STATA/Stata/biobirth.dta")
biobirth <- biobirth[,c("pid", "gebjahr", "sex", "kidgeb01", "kidgeb02","kidgeb03")]

## read the pgen dataset (general info about individuals)
pgen <- read_dta("../original_data/SOEP-CORE.v37eu_STATA/Stata/pgen.dta")
pgen <- pgen[,c("pid", "syear", "pgnation", "pgisced97", "pglfs", "pgkldb92", "pgpartnr")]

## read the bioparen dataset (info on siblings)
bioparen <- read_dta("../original_data/SOEP-CORE.v37eu_STATA/Stata/bioparen.dta")
bioparen <- bioparen[,c("pid", "nums", "numb")]

## read the residence dataset (west/east)
region <- read_dta("../original_data/residence.dta")
colnames(region) <- c("pid", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000",
                      "2001", "2003", "2004", "2002", "2006", "2005", "2007", "2008", "2009", "2010", "2011", "2012",
                      "2013", "2014", "2015", "2016", "2017", "2018", "2019")

region <- region %>% gather(syear, residence, -c(pid))
unique(region$residence)
region$residence <- ifelse(region$residence==-2, NA, region$residence)

unique(region$syear)
region$syear <- as.numeric(region$syear)

# read the tracking ppathl dataset (SOEP-CORE.v37eu_STATA/Stata sample info)

ppathl <- read_dta("../original_data/SOEP-CORE.v37eu_STATA/Stata/ppathl.dta")
ppathl <- ppathl[,c("pid", "psample")]
ppathl <- unique(ppathl)

# merge datasets

length(unique(biobirth$pid))
length(unique(pgen$pid))
length(unique(bioparen$pid))
length(unique(region$pid))
length(unique(ppathl$pid))

data <- merge(biobirth, pgen, by = "pid")
length(unique(data$pid))
data <- merge(data, bioparen, by = "pid", all.x = T)
length(unique(data$pid))
data <- merge(data, region, by = c("pid", "syear"), all.x = T)
length(unique(data$pid))
data <- merge(data, ppathl, by = c("pid"), all.x = T)
length(unique(data$pid))

rm(list=setdiff(ls(), c("data")))

# drop people from the following samples: high income, refugee, shareholder, lgbt+

data <- subset(data, psample!=7)
data <- subset(data, psample!=17)
data <- subset(data, psample!=18)
data <- subset(data, psample!=19)
data <- subset(data, psample!=22)
data <- subset(data, psample!=23)
data <- subset(data, psample!=24)

# order

data <- data[with(data, order(pid, syear)),]
rownames(data) <- NULL

# 2. Clean and recode basic individual variables

colnames(data)

# birth year

unique(data$gebjahr)
data %>% count(gebjahr)
data <- subset(data, gebjahr!=-1)

# gender

unique(data$sex)
data %>% count(sex)
data <- subset(data, sex!=-1)

# child 1 birth year

unique(data$kidgeb01)
data %>% count(kidgeb01)
data <- subset(data, kidgeb01!=-1)
data <- subset(data, kidgeb01!=-3)

length(unique(data$pid))

# nationality

unique(data$pgnation)
data %>% count(pgnation)

data <- subset(data, pgnation>0)

data$migrant <- ifelse(data$pgnation==1, 0, 1)
data %>% count(migrant)

# education

data %>% count(pgisced97)
unique(data$pgisced97)
data$education <- case_when(
  (data$pgisced97==0 | data$pgisced97==1 | data$pgisced97==2) ~ 1,
  (data$pgisced97==3 | data$pgisced97==4 | data$pgisced97==5) ~ 2,
  (data$pgisced97==6) ~ 3
)
data %>% count(education)
unique(data$education)

# labour force status

unique(data$pglfs)
data %>% count(pglfs)
data$lfs <- case_when(
  (data$pglfs > 10) ~ 1, # employed
  (data$pglfs==3) ~ 2, # in education
  (data$pglfs==6) ~ 3, # unemployed
  (data$pglfs==1 | data$pglfs==2 | data$pglfs==4 | data$pglfs==5 | data$pglfs==8 | data$pglfs==9 | data$pglfs==10) ~ 4 # inactive
  
)
data %>% count(lfs)
unique(data$lfs)

# occupation

# unique(data$pgkldb92)
# data %>% count(pgkldb92)
# data$occupation_kldb92 <- ifelse((data$pgkldb92==-8 | data$pgkldb92==-7 | data$pgkldb92==-6 | data$pgkldb92==-5 | data$pgkldb92==-4 | data$pgkldb92==-3 | data$pgkldb92==-1), NA, data$pgkldb92)
# sum(is.na(data$occupation_kldb92))

unique(data$pgkldb92)
data %>% count(pgkldb92)
data$occupation_kldb92 <- ifelse((data$pgkldb92 < 0), NA, data$pgkldb92)
sum(is.na(data$occupation_kldb92))

# number of siblings

unique(data$numb)
unique(data$nums)

data$numb_2 <- ifelse(data$numb==-2, 0,
                          ifelse(data$numb==-1 | data$numb==-5, NA, data$numb))
data$nums_2 <- ifelse(data$nums==-2, 0,
                          ifelse(data$nums==-1 | data$nums==-5, NA, data$nums))
data$num_sib <- ifelse(data$numb_2=='NA' | data$nums_2=='NA', NA, data$numb_2+data$nums_2)
data %>% count(num_sib)

# west/east
unique(data$residence)
data$residence <- ifelse(data$syear < 1990, 1, data$residence)
data %>% count(residence)

length(unique(data$pid))

# leave only relevant columns

data <- data[,c("pid", "syear", "gebjahr", "sex", "migrant", "kidgeb01", "kidgeb02", "kidgeb03", 
                "residence", "num_sib", "education", "lfs", "occupation_kldb92")]

sum(is.na(data$pid))
sum(is.na(data$syear))
sum(is.na(data$gebjahr))
sum(is.na(data$sex))
sum(is.na(data$migrant))
sum(is.na(data$kidgeb01))
sum(is.na(data$kidgeb02))
sum(is.na(data$kidgeb03))
sum(is.na(data$residence))
sum(is.na(data$num_sib))
sum(is.na(data$education))
sum(is.na(data$lfs))
sum(is.na(data$occupation_kldb92))

# 3. Interpolate education, lfs, occupation, and residence

# Education

unique(data$education)

ids <- unique(data$pid)
df_1 <- data.frame()
df_2 <- data.frame()

for (id in ids){
  temp <- data %>% filter(pid==id)
  if (colSums(is.na(temp))["education"]==nrow(temp)){
    df_1 <- rbind(df_1, temp)
  }else{
    temp$education <- na_locf(temp$education)
    df_2 <- rbind(df_2, temp)
  }
}

data <- rbind(df_1, df_2)
data <- data[with(data, order(pid, syear)),]
row.names(data) <- NULL

# LFS

unique(data$lfs)

ids <- unique(data$pid)
df_1 <- data.frame()
df_2 <- data.frame()

for (id in ids){
  temp <- data %>% filter(pid==id)
  if (colSums(is.na(temp))["lfs"]==nrow(temp)){
    df_1 <- rbind(df_1, temp)
  }else{
    temp$lfs <- na_locf(temp$lfs)
    df_2 <- rbind(df_2, temp)
  }
}

data <- rbind(df_1, df_2)
data <- data[with(data, order(pid, syear)),]
row.names(data) <- NULL

# Occupation

# # set occupation to -2 for people who are not working
# 
# data$occupation_kldb92 <- ifelse(data$lfs!=1, -2, data$occupation_kldb92)
# data$occupation_kldb92 <- ifelse(data$lfs==1 & data$occupation_kldb92==-2, NA, data$occupation_kldb92)
# 
# unique(data$occupation_kldb92)

ids <- unique(data$pid)
df_1 <- data.frame()
df_2 <- data.frame()

for (id in ids){
  temp <- data %>% filter(pid==id)
  if (colSums(is.na(temp))["occupation_kldb92"]==nrow(temp)){
    df_1 <- rbind(df_1, temp)
  }else{
    temp$occupation_kldb92 <- na_locf(temp$occupation_kldb92)
    df_2 <- rbind(df_2, temp)
  }
}

data <- rbind(df_1, df_2)
data <- data[with(data, order(pid, syear)),]
row.names(data) <- NULL

# Residence

unique(data$residence)

ids <- unique(data$pid)
df_1 <- data.frame()
df_2 <- data.frame()

for (id in ids){
  temp <- data %>% filter(pid==id)
  if (colSums(is.na(temp))["residence"]==nrow(temp)){
    df_1 <- rbind(df_1, temp)
  }else{
    temp$residence <- na_locf(temp$residence)
    df_2 <- rbind(df_2, temp)
  }
}

data <- rbind(df_1, df_2)
data <- data[with(data, order(pid, syear)),]
row.names(data) <- NULL

# Create first and second lags

data <- data %>% group_by(pid) %>%
  mutate(l1_education = lag(education, 1),
         l1_lfs = lag(lfs, 1),
         l1_occupation_kldb92 = lag(occupation_kldb92, 1),
         l1_residence = lag(residence, 1),
         l2_education = lag(education, 2),
         l2_lfs = lag(lfs, 2),
         l2_occupation_kldb92 = lag(occupation_kldb92, 2),
         l2_residence = lag(residence, 2))

rm(list=setdiff(ls(), c("data")))

# save intermediately data

save(data, file = "../generated_data/data_int_1_rob.RData")

# 4. Structure for birth analysis

rm(list = ls())

load("../generated_data/data_int_1_rob.RData")

data$pid <- as.numeric(data$pid)

# delete the last year each person is observed

data <- data %>% group_by(pid) %>%
  mutate(max_syear = max(syear))

data <- subset(data, syear!=max_syear)

# drop observations for 2020

unique(data$syear)
data <- subset(data, syear!=2020)

# calculate age

data$age <- data$syear - data$gebjahr

# restrict by age

data <- subset(data, age>19 & age<50)
sum(is.na(data$age))

unique(data$gebjahr)
data %>% count(gebjahr)
data$cohort <- case_when(
  data$gebjahr>=1930 & data$gebjahr<1950 ~ 1,
  data$gebjahr>=1950 & data$gebjahr<1960 ~ 2,
  data$gebjahr>=1960 & data$gebjahr<1970 ~ 3,
  data$gebjahr>=1970 & data$gebjahr<1980 ~ 4,
  data$gebjahr>=1980 & data$gebjahr<1990 ~ 5,
  data$gebjahr>=1990 ~ 6
)
unique(data$cohort)

data$child_1 <- ifelse(data$syear==data$kidgeb01, 1, 0)
data$child_2 <- ifelse(data$syear==data$kidgeb02, 1, 0)
data$child_3 <- ifelse(data$syear==data$kidgeb03, 1, 0)

rm(list=setdiff(ls(), c("data")))
data <- data[with(data, order(pid, syear)),]
row.names(data) <- NULL

## FIRST BIRTH

first_birth <- data[,c("pid", "syear", "gebjahr", "age", "cohort", "sex", "migrant", "num_sib",
                       "child_1", "kidgeb01",
                       "residence", "l1_residence", "l2_residence",
                       "education", "l1_education", "l2_education",
                       "lfs", "l1_lfs", "l2_lfs",
                       "occupation_kldb92", "l1_occupation_kldb92", "l2_occupation_kldb92")]

# delete people who had the first birth before entering the panel

first_birth <- first_birth %>% group_by(pid) %>%
  mutate(min_year = min(syear))

unique(first_birth$kidgeb01)
sum(is.na(first_birth$kidgeb01))

first_birth <- subset(first_birth, (kidgeb01 > min_year) | kidgeb01 == -2)

# stop observing parents after 1st birth
ids <- unique(first_birth$pid)
parents_a <- data.frame()
parents_b <- data.frame()
non_parents <- data.frame()

for (id in ids){
  temp <- first_birth %>% filter(id==pid)
  y0 <- min(temp$syear)
  if (sum(temp$child_1)==1){
    temp_1a <- slice(temp, 1:which(child_1==1, arr.ind = T))
    temp_1a$for_analysis <- 1
    parents_a <- rbind(parents_a, temp_1a)
    temp_1b <- slice(temp, (which(child_1==1, arr.ind = T)+1):n())
    temp_1b$for_analysis <- 0
    parents_b <- rbind(parents_b, temp_1b)
  }else{
    temp$for_analysis <- 1
    non_parents <- rbind(non_parents, temp)
  }
}

first_birth <- rbind(parents_a, parents_b, non_parents)
first_birth <- first_birth[with(first_birth, order(pid, syear)),]
first_birth <- unique(first_birth)
row.names(first_birth) <- NULL

parents_1 <- subset(first_birth, child_1==1)
parents_1 <- parents_1[,c("pid", "syear")]
colnames(parents_1)[2] <- "child_1_y"

save(first_birth, file="../generated_data/data_int_2_rob_fb.RData")
save(parents_1, file="../generated_data/data_int_2_rob_p1.RData")

rm(list=setdiff(ls(), c("data", "parents_1", "first_birth")))

## SECOND BIRTH

second_birth <- data[,c("pid", "syear", "gebjahr", "age", "cohort", "sex", "migrant", "num_sib",
                       "child_2", "kidgeb02",
                       "residence", "l1_residence", "l2_residence",
                       "education", "l1_education", "l2_education",
                       "lfs", "l1_lfs", "l2_lfs",
                       "occupation_kldb92", "l1_occupation_kldb92", "l2_occupation_kldb92")]

second_birth <- merge(second_birth, parents_1, by = "pid")
length(unique(second_birth$pid))

# start observing at 1st birth
second_birth <- second_birth[with(second_birth, order(pid, syear)),]
second_birth$check <- second_birth$syear - second_birth$child_1_y
second_birth <- subset(second_birth, check>-1)
second_birth$check <- NULL

# stop observing parents after 2st birth
ids <- unique(second_birth$pid)
parents_a <- data.frame()
parents_b <- data.frame()
non_parents <- data.frame()

for (id in ids){
  temp <- second_birth %>% filter(id==pid)
  y0 <- min(temp$syear)
  temp$year_no <- temp$syear-y0+1
  temp <- unique(temp)
  if (sum(temp$child_2)==1){
    temp_1a <- slice(temp, 1:which(child_2==1, arr.ind = T))
    temp_1a$for_analysis <- 1
    parents_a <- rbind(parents_a, temp_1a)
    temp_1b <- slice(temp, (which(child_2==1, arr.ind = T)+1):n())
    temp_1b$for_analysis <- 0
    parents_b <- rbind(parents_b, temp_1b)
  }else{
    temp$for_analysis <- 1
    non_parents <- rbind(non_parents, temp)
  }
}

second_birth <- rbind(parents_a, parents_b, non_parents)
second_birth <- second_birth[with(second_birth, order(pid, syear)),]
second_birth <- unique(second_birth)
row.names(second_birth) <- NULL

parents_2 <- subset(second_birth, child_2==1)
parents_2 <- parents_2[,c("pid", "syear")]
colnames(parents_2)[2] <- "child_2_y"

save(second_birth, file="../generated_data/data_int_2_rob_sb.RData")
save(parents_2, file="../generated_data/data_int_2_rob_p2.RData")

rm(list=setdiff(ls(), c("data",
                        "parents_1", "first_birth",
                        "parents_2", "second_birth")))


## THIRD BIRTH

third_birth <- data[,c("pid", "syear", "gebjahr", "age", "cohort", "sex", "migrant", "num_sib",
                        "child_3", "kidgeb03",
                        "residence", "l1_residence", "l2_residence",
                        "education", "l1_education", "l2_education",
                        "lfs", "l1_lfs", "l2_lfs",
                        "occupation_kldb92", "l1_occupation_kldb92", "l2_occupation_kldb92")]

third_birth <- merge(third_birth, parents_2, by = "pid")
length(unique(third_birth$pid))

# start observing at 1st birth
third_birth <- third_birth[with(third_birth, order(pid, syear)),]
third_birth$check <- third_birth$syear - third_birth$child_2_y
third_birth <- subset(third_birth, check>-1)
third_birth$check <- NULL

# stop observing parents after 3rd birth
ids <- unique(third_birth$pid)
parents_a <- data.frame()
parents_b <- data.frame()
non_parents <- data.frame()

for (id in ids){
  temp <- third_birth %>% filter(id==pid)
  y0 <- min(temp$syear)
  temp$year_no <- temp$syear-y0+1
  temp <- unique(temp)
  if (sum(temp$child_3)==1){
    temp_1a <- slice(temp, 1:which(child_3==1, arr.ind = T))
    temp_1a$for_analysis <- 1
    parents_a <- rbind(parents_a, temp_1a)
    temp_1b <- slice(temp, (which(child_3==1, arr.ind = T)+1):n())
    temp_1b$for_analysis <- 0
    parents_b <- rbind(parents_b, temp_1b)
  }else{
    temp$for_analysis <- 1
    non_parents <- rbind(non_parents, temp)
  }
}

third_birth <- rbind(parents_a, parents_b, non_parents)
third_birth <- third_birth[with(third_birth, order(pid, syear)),]
third_birth <- unique(third_birth)
row.names(third_birth) <- NULL

save(third_birth, file="../generated_data/data_int_2_rob_tb.RData")

rm(list=ls())

# 5. Add task measures

load("../generated_data/data_int_2_rob_fb.RData")
load("../generated_data/data_int_2_rob_sb.RData")
load("../generated_data/data_int_2_rob_tb.RData")

# check the fraction of missing occupations

sum(is.na(first_birth$occupation_kldb92))/sum(1-is.na(first_birth$occupation_kldb92))
sum(is.na(second_birth$occupation_kldb92))/sum(1-is.na(second_birth$occupation_kldb92))
sum(is.na(third_birth$occupation_kldb92))/sum(1-is.na(third_birth$occupation_kldb92))

sum(is.na(first_birth$l1_occupation_kldb92))/sum(1-is.na(first_birth$l1_occupation_kldb92))
sum(is.na(second_birth$l1_occupation_kldb92))/sum(1-is.na(second_birth$l1_occupation_kldb92))
sum(is.na(third_birth$l1_occupation_kldb92))/sum(1-is.na(third_birth$l1_occupation_kldb92))

sum(is.na(first_birth$l2_occupation_kldb92))/sum(1-is.na(first_birth$l2_occupation_kldb92))
sum(is.na(second_birth$l2_occupation_kldb92))/sum(1-is.na(second_birth$l2_occupation_kldb92))
sum(is.na(third_birth$l2_occupation_kldb92))/sum(1-is.na(third_birth$l2_occupation_kldb92))

# append task measures

load("../generated_data/task_measures.RData")
task_measures_l1 <- task_measures
task_measures_l2 <- task_measures

task_measures <- task_measures[,c("kldb_1992_3d", "year", "analytic", "interactive")]
colnames(task_measures)[1:2] <- c("occupation_kldb92", "syear")

task_measures_l1 <- task_measures_l1[,c("kldb_1992_3d", "year", "l1_analytic", "l1_interactive")]
colnames(task_measures_l1)[1:2] <- c("l1_occupation_kldb92", "syear")

task_measures_l2 <- task_measures_l2[,c("kldb_1992_3d", "year", "l2_analytic", "l2_interactive")]
colnames(task_measures_l2)[1:2] <- c("l2_occupation_kldb92", "syear")

# FIRST BIRTH

# task measures

temp_1 <- first_birth[(nchar(as.character(first_birth$occupation_kldb92)) == 4 & is.na(first_birth$occupation_kldb92)==0),]
temp_2 <- first_birth[(nchar(as.character(first_birth$occupation_kldb92)) == 3 & is.na(first_birth$occupation_kldb92)==0),]
temp_3 <- first_birth[((first_birth$occupation_kldb92 == -2) & is.na(first_birth$occupation_kldb92)==0),]
temp_4 <- first_birth[is.na(first_birth$occupation_kldb92),]
temp_1$occupation_kldb92 <- sub('.$','',temp_1$occupation_kldb92)
temp_1 <- merge(temp_1, task_measures, by=c("occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures, by=c("occupation_kldb92", "syear"), all.x = T)
temp_3$analytic <- ifelse(temp_3$lfs==1, NA, -2)
temp_3$interactive <- ifelse(temp_3$lfs==1, NA, -2)
temp_4$analytic <- ifelse(temp_4$lfs==1, NA, -2)
temp_4$interactive <- ifelse(temp_4$lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
first_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
first_birth <- first_birth[with(first_birth, order(pid, syear)),]
row.names(first_birth) <- NULL

first_birth <- unique(first_birth)

# l1 task measures

temp_1 <- first_birth[(nchar(as.character(first_birth$l1_occupation_kldb92)) == 4 & is.na(first_birth$l1_occupation_kldb92)==0),]
temp_2 <- first_birth[(nchar(as.character(first_birth$l1_occupation_kldb92)) == 3 & is.na(first_birth$l1_occupation_kldb92)==0),]
temp_3 <- first_birth[((first_birth$l1_occupation_kldb92 == -2) & is.na(first_birth$l1_occupation_kldb92)==0),]
temp_4 <- first_birth[is.na(first_birth$l1_occupation_kldb92),]
temp_1$l1_occupation_kldb92 <- sub('.$','',temp_1$l1_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l1, by=c("l1_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l1, by=c("l1_occupation_kldb92", "syear"), all.x = T)
temp_3$l1_analytic <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_3$l1_interactive <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_4$l1_analytic <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_4$l1_interactive <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
first_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
first_birth <- first_birth[with(first_birth, order(pid, syear)),]
row.names(first_birth) <- NULL

first_birth <- unique(first_birth)

# l2 task measures

temp_1 <- first_birth[(nchar(as.character(first_birth$l2_occupation_kldb92)) == 4 & is.na(first_birth$l2_occupation_kldb92)==0),]
temp_2 <- first_birth[(nchar(as.character(first_birth$l2_occupation_kldb92)) == 3 & is.na(first_birth$l2_occupation_kldb92)==0),]
temp_3 <- first_birth[((first_birth$l2_occupation_kldb92 == -2) & is.na(first_birth$l2_occupation_kldb92)==0),]
temp_4 <- first_birth[is.na(first_birth$l2_occupation_kldb92),]
temp_1$l2_occupation_kldb92 <- sub('.$','',temp_1$l2_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l2, by=c("l2_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l2, by=c("l2_occupation_kldb92", "syear"), all.x = T)
temp_3$l2_analytic <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_3$l2_interactive <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_4$l2_analytic <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_4$l2_interactive <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
first_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
first_birth <- first_birth[with(first_birth, order(pid, syear)),]
row.names(first_birth) <- NULL

first_birth <- unique(first_birth)

# SECOND BIRTH

# task measures

temp_1 <- second_birth[(nchar(as.character(second_birth$occupation_kldb92)) == 4 & is.na(second_birth$occupation_kldb92)==0),]
temp_2 <- second_birth[(nchar(as.character(second_birth$occupation_kldb92)) == 3 & is.na(second_birth$occupation_kldb92)==0),]
temp_3 <- second_birth[((second_birth$occupation_kldb92 == -2) & is.na(second_birth$occupation_kldb92)==0),]
temp_4 <- second_birth[is.na(second_birth$occupation_kldb92),]
temp_1$occupation_kldb92 <- sub('.$','',temp_1$occupation_kldb92)
temp_1 <- merge(temp_1, task_measures, by=c("occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures, by=c("occupation_kldb92", "syear"), all.x = T)
temp_3$analytic <- ifelse(temp_3$lfs==1, NA, -2)
temp_3$interactive <- ifelse(temp_3$lfs==1, NA, -2)
temp_4$analytic <- ifelse(temp_4$lfs==1, NA, -2)
temp_4$interactive <- ifelse(temp_4$lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
second_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
second_birth <- second_birth[with(second_birth, order(pid, syear)),]
row.names(second_birth) <- NULL

second_birth <- unique(second_birth)

# l1 task measures

temp_1 <- second_birth[(nchar(as.character(second_birth$l1_occupation_kldb92)) == 4 & is.na(second_birth$l1_occupation_kldb92)==0),]
temp_2 <- second_birth[(nchar(as.character(second_birth$l1_occupation_kldb92)) == 3 & is.na(second_birth$l1_occupation_kldb92)==0),]
temp_3 <- second_birth[((second_birth$l1_occupation_kldb92 == -2) & is.na(second_birth$l1_occupation_kldb92)==0),]
temp_4 <- second_birth[is.na(second_birth$l1_occupation_kldb92),]
temp_1$l1_occupation_kldb92 <- sub('.$','',temp_1$l1_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l1, by=c("l1_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l1, by=c("l1_occupation_kldb92", "syear"), all.x = T)
temp_3$l1_analytic <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_3$l1_interactive <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_4$l1_analytic <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_4$l1_interactive <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
second_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
second_birth <- second_birth[with(second_birth, order(pid, syear)),]
row.names(second_birth) <- NULL

second_birth <- unique(second_birth)

# l2 task measures

temp_1 <- second_birth[(nchar(as.character(second_birth$l2_occupation_kldb92)) == 4 & is.na(second_birth$l2_occupation_kldb92)==0),]
temp_2 <- second_birth[(nchar(as.character(second_birth$l2_occupation_kldb92)) == 3 & is.na(second_birth$l2_occupation_kldb92)==0),]
temp_3 <- second_birth[((second_birth$l2_occupation_kldb92 == -2) & is.na(second_birth$l2_occupation_kldb92)==0),]
temp_4 <- second_birth[is.na(second_birth$l2_occupation_kldb92),]
temp_1$l2_occupation_kldb92 <- sub('.$','',temp_1$l2_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l2, by=c("l2_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l2, by=c("l2_occupation_kldb92", "syear"), all.x = T)
temp_3$l2_analytic <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_3$l2_interactive <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_4$l2_analytic <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_4$l2_interactive <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
second_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
second_birth <- second_birth[with(second_birth, order(pid, syear)),]
row.names(second_birth) <- NULL

second_birth <- unique(second_birth)

# THIRD BIRTH

# task measures

temp_1 <- third_birth[(nchar(as.character(third_birth$occupation_kldb92)) == 4 & is.na(third_birth$occupation_kldb92)==0),]
temp_2 <- third_birth[(nchar(as.character(third_birth$occupation_kldb92)) == 3 & is.na(third_birth$occupation_kldb92)==0),]
temp_3 <- third_birth[((third_birth$occupation_kldb92 == -2) & is.na(third_birth$occupation_kldb92)==0),]
temp_4 <- third_birth[is.na(third_birth$occupation_kldb92),]
temp_1$occupation_kldb92 <- sub('.$','',temp_1$occupation_kldb92)
temp_1 <- merge(temp_1, task_measures, by=c("occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures, by=c("occupation_kldb92", "syear"), all.x = T)
temp_3$analytic <- ifelse(temp_3$lfs==1, NA, -2)
temp_3$interactive <- ifelse(temp_3$lfs==1, NA, -2)
temp_4$analytic <- ifelse(temp_4$lfs==1, NA, -2)
temp_4$interactive <- ifelse(temp_4$lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
third_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
third_birth <- third_birth[with(third_birth, order(pid, syear)),]
row.names(third_birth) <- NULL

third_birth <- unique(third_birth)

# l1 task measures

temp_1 <- third_birth[(nchar(as.character(third_birth$l1_occupation_kldb92)) == 4 & is.na(third_birth$l1_occupation_kldb92)==0),]
temp_2 <- third_birth[(nchar(as.character(third_birth$l1_occupation_kldb92)) == 3 & is.na(third_birth$l1_occupation_kldb92)==0),]
temp_3 <- third_birth[((third_birth$l1_occupation_kldb92 == -2) & is.na(third_birth$l1_occupation_kldb92)==0),]
temp_4 <- third_birth[is.na(third_birth$l1_occupation_kldb92),]
temp_1$l1_occupation_kldb92 <- sub('.$','',temp_1$l1_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l1, by=c("l1_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l1, by=c("l1_occupation_kldb92", "syear"), all.x = T)
temp_3$l1_analytic <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_3$l1_interactive <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_4$l1_analytic <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_4$l1_interactive <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
third_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
third_birth <- third_birth[with(third_birth, order(pid, syear)),]
row.names(third_birth) <- NULL

third_birth <- unique(third_birth)

# l2 task measures

temp_1 <- third_birth[(nchar(as.character(third_birth$l2_occupation_kldb92)) == 4 & is.na(third_birth$l2_occupation_kldb92)==0),]
temp_2 <- third_birth[(nchar(as.character(third_birth$l2_occupation_kldb92)) == 3 & is.na(third_birth$l2_occupation_kldb92)==0),]
temp_3 <- third_birth[((third_birth$l2_occupation_kldb92 == -2) & is.na(third_birth$l2_occupation_kldb92)==0),]
temp_4 <- third_birth[is.na(third_birth$l2_occupation_kldb92),]
temp_1$l2_occupation_kldb92 <- sub('.$','',temp_1$l2_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l2, by=c("l2_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l2, by=c("l2_occupation_kldb92", "syear"), all.x = T)
temp_3$l2_analytic <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_3$l2_interactive <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_4$l2_analytic <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_4$l2_interactive <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
third_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
third_birth <- third_birth[with(third_birth, order(pid, syear)),]
row.names(third_birth) <- NULL

third_birth <- unique(third_birth)

rm(list=setdiff(ls(), c("first_birth", "second_birth", "third_birth")))

# Add task measures fixed at the year of the birth of 1st child to the second_birth and third_birth datasets.

sb_fixed_value <- second_birth[,c("pid", "syear", "child_1_y", "analytic", "interactive")]
sb_fixed_value <- subset(sb_fixed_value, syear==child_1_y)
sb_fixed_value <- sb_fixed_value[,c("pid", "analytic", "interactive")]
colnames(sb_fixed_value)[2:3] <- c("analytic_child_1", "interactive_child_1")

second_birth <- merge(second_birth, sb_fixed_value, by = "pid")

tb_fixed_value <- third_birth[,c("pid", "syear", "child_2_y", "analytic", "interactive")]
tb_fixed_value <- subset(tb_fixed_value, syear==child_2_y)
tb_fixed_value <- tb_fixed_value[,c("pid", "analytic", "interactive")]
colnames(tb_fixed_value)[2:3] <- c("analytic_child_2", "interactive_child_2")

third_birth <- merge(third_birth, sb_fixed_value, by = "pid")
third_birth <- merge(third_birth, tb_fixed_value, by = "pid")

rm(list=setdiff(ls(), c("first_birth", "second_birth", "third_birth")))

# 5. Add partnership status and partner's lfs and occupation

partnr <- read_dta("../original_data/SOEP-CORE.v37eu_STATA/Stata/pgen.dta")
partnr <- partnr[,c("pid", "syear", "pgpartnr")]
unique(partnr$pgpartnr)
partnr$par_pid <- ifelse(partnr$pgpartnr==-7 | partnr$pgpartnr==-6 | partnr$pgpartnr==-5 | partnr$pgpartnr==-4 | partnr$pgpartnr==-3 | partnr$pgpartnr==-1, NA, partnr$pgpartnr)
partnr$pgpartnr <- NULL

# add sex of the partner

ppathl <- read_dta("../original_data/SOEP-CORE.v37eu_STATA/Stata/ppathl.dta")
ppathl <- ppathl[,c("pid", "sex")]
ppathl <- unique(ppathl)
unique(ppathl$sex)
ppathl <- subset(ppathl, sex==1 | sex==2)
colnames(ppathl)[1] <- "par_pid"

partnr <- merge(partnr, ppathl, by = "par_pid", all.x = T)
partnr <- unique(partnr)
rm(ppathl)

partnr <- partnr[,c("pid", "syear", "par_pid", "sex")]
colnames(partnr)[4] <- "par_sex"

partnr <- partnr[with(partnr, order(pid, syear)),]
rownames(partnr) <- NULL
partnr$par_sex <- ifelse(partnr$par_pid==-2, -2, partnr$par_sex)
unique(partnr$par_sex)

partnr %>% count(par_pid)
partnr %>% count(par_sex)

sum(is.na(partnr$par_pid))
partnr <- subset(partnr, !is.na(par_sex))

# Create binary union status

partnr$union_status <- ifelse(partnr$par_pid!=-2, 1, 0)
sum(is.na(partnr$union_status))

# Create lagged union status

partnr <- partnr %>% group_by(pid) %>%
  mutate(l1_union_status = lag(union_status, 1),
         l2_union_status = lag(union_status, 2))

# Merge with birth files

first_birth <- merge(first_birth, partnr, by = c("pid", "syear"), all.x = T)
second_birth <- merge(second_birth, partnr, by = c("pid", "syear"), all.x = T)
third_birth <- merge(third_birth, partnr, by = c("pid", "syear"), all.x = T)

rm(partnr)

first_birth <- unique(first_birth)
second_birth <- unique(second_birth)
third_birth <- unique(third_birth)

first_birth$mean_sex <- (first_birth$sex + first_birth$par_sex)/2
unique(first_birth$mean_sex)

second_birth$mean_sex <- (second_birth$sex + second_birth$par_sex)/2
unique(second_birth$mean_sex)

third_birth$mean_sex <- (third_birth$sex + third_birth$par_sex)/2
unique(third_birth$mean_sex)

first_birth$couple_type <- case_when(
  (first_birth$mean_sex==-0.5 | first_birth$mean_sex==0) ~ -2,
  (first_birth$mean_sex==1.5) ~ 1,
  (first_birth$mean_sex==1) ~ 2,
  (first_birth$mean_sex==2) ~ 3
)

unique(first_birth$couple_type)

second_birth$couple_type <- case_when(
  (second_birth$mean_sex==-0.5 | second_birth$mean_sex==0) ~ -2,
  (second_birth$mean_sex==1.5) ~ 1,
  (second_birth$mean_sex==1) ~ 2,
  (second_birth$mean_sex==2) ~ 3
)

unique(second_birth$couple_type)

third_birth$couple_type <- case_when(
  (third_birth$mean_sex==-0.5 | third_birth$mean_sex==0) ~ -2,
  (third_birth$mean_sex==1.5) ~ 1,
  (third_birth$mean_sex==1) ~ 2,
  (third_birth$mean_sex==2) ~ 3
)

unique(first_birth$couple_type)

first_birth %>% count(couple_type)
second_birth %>% count(couple_type)
third_birth %>% count(couple_type)

first_birth$mean_sex <- NULL
second_birth$mean_sex <- NULL
third_birth$mean_sex <- NULL

gc()

# add partner occupation & labour force status

load("../generated_data/data_int_1.RData")

data <- data[,c("pid", "syear", "lfs", "l1_lfs", "l2_lfs", "occupation_kldb92", "l1_occupation_kldb92", "l2_occupation_kldb92")]
colnames(data) <- c("par_pid", "syear", "par_lfs", "l1_par_lfs", "l2_par_lfs", "par_occupation_kldb92", "l1_par_occupation_kldb92", "l2_par_occupation_kldb92")

first_birth <- merge(first_birth, data, by = c("par_pid", "syear"), all.x = T)
first_birth <- first_birth[with(first_birth, order(pid, syear)),]
row.names(first_birth) <- NULL

second_birth <- merge(second_birth, data, by = c("par_pid", "syear"), all.x = T)
second_birth <- second_birth[with(second_birth, order(pid, syear)),]
row.names(second_birth) <- NULL

third_birth <- merge(third_birth, data, by = c("par_pid", "syear"), all.x = T)
third_birth <- third_birth[with(third_birth, order(pid, syear)),]
row.names(third_birth) <- NULL

for(i in grep("^par_lfs$", colnames(first_birth)):grep("^l2_par_occupation_kldb92$", colnames(first_birth))){
  first_birth[[i]] <- ifelse(first_birth$par_pid==-2, -2, first_birth[[i]])
}

for(i in grep("^par_lfs$", colnames(second_birth)):grep("^l2_par_occupation_kldb92$", colnames(second_birth))){
  second_birth[[i]] <- ifelse(second_birth$par_pid==-2, -2, second_birth[[i]])
}

for(i in grep("^par_lfs$", colnames(third_birth)):grep("^l2_par_occupation_kldb92$", colnames(third_birth))){
  third_birth[[i]] <- ifelse(third_birth$par_pid==-2, -2, third_birth[[i]])
}

rm(list=setdiff(ls(), c("first_birth", "second_birth", "third_birth")))

save(first_birth, file = "../generated_data/data_int_3_rob_fb.RData")
save(second_birth, file = "../generated_data/data_int_3_rob_sb.RData")
save(third_birth, file = "../generated_data/data_int_3_rob_tb.RData")

rm(list = ls())

# 6. Partner's task measures

load("../generated_data/data_int_3_rob_fb.RData")
load("../generated_data/data_int_3_rob_sb.RData")
load("../generated_data/data_int_3_rob_tb.RData")

load("../generated_data/task_measures.RData")
task_measures_l1 <- task_measures
task_measures_l2 <- task_measures

task_measures <- task_measures[,c("kldb_1992_3d", "year", "analytic", "interactive")]
colnames(task_measures) <- c("par_occupation_kldb92", "syear", "par_analytic", "par_interactive")

task_measures_l1 <- task_measures_l1[,c("kldb_1992_3d", "year", "l1_analytic", "l1_interactive")]
colnames(task_measures_l1) <- c("l1_par_occupation_kldb92", "syear", "l1_par_analytic", "l1_par_interactive")

task_measures_l2 <- task_measures_l2[,c("kldb_1992_3d", "year", "l2_analytic", "l2_interactive")]
colnames(task_measures_l2) <- c("l2_par_occupation_kldb92", "syear", "l2_par_analytic", "l2_par_interactive")

# FIRST BIRTH

# task measures

temp_1 <- first_birth[(nchar(as.character(first_birth$par_occupation_kldb92)) == 4 & is.na(first_birth$par_occupation_kldb92)==0),]
temp_2 <- first_birth[(nchar(as.character(first_birth$par_occupation_kldb92)) == 3 & is.na(first_birth$par_occupation_kldb92)==0),]
temp_3 <- first_birth[((first_birth$par_occupation_kldb92 == -2) & is.na(first_birth$par_occupation_kldb92)==0),]
temp_4 <- first_birth[is.na(first_birth$par_occupation_kldb92),]
temp_1$par_occupation_kldb92 <- sub('.$','',temp_1$par_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures, by=c("par_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures, by=c("par_occupation_kldb92", "syear"), all.x = T)
temp_3$par_analytic <- ifelse(temp_3$lfs==1, NA, -2)
temp_3$par_interactive <- ifelse(temp_3$lfs==1, NA, -2)
temp_4$par_analytic <- ifelse(temp_4$lfs==1, NA, -2)
temp_4$par_interactive <- ifelse(temp_4$lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
first_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
first_birth <- first_birth[with(first_birth, order(pid, syear)),]
row.names(first_birth) <- NULL

first_birth <- unique(first_birth)

# l1 task measures

temp_1 <- first_birth[(nchar(as.character(first_birth$l1_par_occupation_kldb92)) == 4 & is.na(first_birth$l1_par_occupation_kldb92)==0),]
temp_2 <- first_birth[(nchar(as.character(first_birth$l1_par_occupation_kldb92)) == 3 & is.na(first_birth$l1_par_occupation_kldb92)==0),]
temp_3 <- first_birth[((first_birth$l1_par_occupation_kldb92 == -2) & is.na(first_birth$l1_par_occupation_kldb92)==0),]
temp_4 <- first_birth[is.na(first_birth$l1_par_occupation_kldb92),]
temp_1$l1_par_occupation_kldb92 <- sub('.$','',temp_1$l1_par_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l1, by=c("l1_par_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l1, by=c("l1_par_occupation_kldb92", "syear"), all.x = T)
temp_3$l1_par_analytic <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_3$l1_par_interactive <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_4$l1_par_analytic <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_4$l1_par_interactive <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
first_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
first_birth <- first_birth[with(first_birth, order(pid, syear)),]
row.names(first_birth) <- NULL

first_birth <- unique(first_birth)

# l2 task measures

temp_1 <- first_birth[(nchar(as.character(first_birth$l2_par_occupation_kldb92)) == 4 & is.na(first_birth$l2_par_occupation_kldb92)==0),]
temp_2 <- first_birth[(nchar(as.character(first_birth$l2_par_occupation_kldb92)) == 3 & is.na(first_birth$l2_par_occupation_kldb92)==0),]
temp_3 <- first_birth[((first_birth$l2_par_occupation_kldb92 == -2) & is.na(first_birth$l2_par_occupation_kldb92)==0),]
temp_4 <- first_birth[is.na(first_birth$l2_par_occupation_kldb92),]
temp_1$l2_par_occupation_kldb92 <- sub('.$','',temp_1$l2_par_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l2, by=c("l2_par_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l2, by=c("l2_par_occupation_kldb92", "syear"), all.x = T)
temp_3$l2_par_analytic <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_3$l2_par_interactive <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_4$l2_par_analytic <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_4$l2_par_interactive <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
first_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
first_birth <- first_birth[with(first_birth, order(pid, syear)),]
row.names(first_birth) <- NULL

first_birth <- unique(first_birth)

# SECOND BIRTH

# task measures

temp_1 <- second_birth[(nchar(as.character(second_birth$par_occupation_kldb92)) == 4 & is.na(second_birth$par_occupation_kldb92)==0),]
temp_2 <- second_birth[(nchar(as.character(second_birth$par_occupation_kldb92)) == 3 & is.na(second_birth$par_occupation_kldb92)==0),]
temp_3 <- second_birth[((second_birth$par_occupation_kldb92 == -2) & is.na(second_birth$par_occupation_kldb92)==0),]
temp_4 <- second_birth[is.na(second_birth$par_occupation_kldb92),]
temp_1$par_occupation_kldb92 <- sub('.$','',temp_1$par_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures, by=c("par_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures, by=c("par_occupation_kldb92", "syear"), all.x = T)
temp_3$par_analytic <- ifelse(temp_3$lfs==1, NA, -2)
temp_3$par_interactive <- ifelse(temp_3$lfs==1, NA, -2)
temp_4$par_analytic <- ifelse(temp_4$lfs==1, NA, -2)
temp_4$par_interactive <- ifelse(temp_4$lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
second_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
second_birth <- second_birth[with(second_birth, order(pid, syear)),]
row.names(second_birth) <- NULL

second_birth <- unique(second_birth)

# l1 task measures

temp_1 <- second_birth[(nchar(as.character(second_birth$l1_par_occupation_kldb92)) == 4 & is.na(second_birth$l1_par_occupation_kldb92)==0),]
temp_2 <- second_birth[(nchar(as.character(second_birth$l1_par_occupation_kldb92)) == 3 & is.na(second_birth$l1_par_occupation_kldb92)==0),]
temp_3 <- second_birth[((second_birth$l1_par_occupation_kldb92 == -2) & is.na(second_birth$l1_par_occupation_kldb92)==0),]
temp_4 <- second_birth[is.na(second_birth$l1_par_occupation_kldb92),]
temp_1$l1_par_occupation_kldb92 <- sub('.$','',temp_1$l1_par_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l1, by=c("l1_par_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l1, by=c("l1_par_occupation_kldb92", "syear"), all.x = T)
temp_3$l1_par_analytic <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_3$l1_par_interactive <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_4$l1_par_analytic <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_4$l1_par_interactive <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
second_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
second_birth <- second_birth[with(second_birth, order(pid, syear)),]
row.names(second_birth) <- NULL

second_birth <- unique(second_birth)

# l2 task measures

temp_1 <- second_birth[(nchar(as.character(second_birth$l2_par_occupation_kldb92)) == 4 & is.na(second_birth$l2_par_occupation_kldb92)==0),]
temp_2 <- second_birth[(nchar(as.character(second_birth$l2_par_occupation_kldb92)) == 3 & is.na(second_birth$l2_par_occupation_kldb92)==0),]
temp_3 <- second_birth[((second_birth$l2_par_occupation_kldb92 == -2) & is.na(second_birth$l2_par_occupation_kldb92)==0),]
temp_4 <- second_birth[is.na(second_birth$l2_par_occupation_kldb92),]
temp_1$l2_par_occupation_kldb92 <- sub('.$','',temp_1$l2_par_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l2, by=c("l2_par_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l2, by=c("l2_par_occupation_kldb92", "syear"), all.x = T)
temp_3$l2_par_analytic <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_3$l2_par_interactive <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_4$l2_par_analytic <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_4$l2_par_interactive <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
second_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
second_birth <- second_birth[with(second_birth, order(pid, syear)),]
row.names(second_birth) <- NULL

second_birth <- unique(second_birth)

# THIRD BIRTH

# task measures

temp_1 <- third_birth[(nchar(as.character(third_birth$par_occupation_kldb92)) == 4 & is.na(third_birth$par_occupation_kldb92)==0),]
temp_2 <- third_birth[(nchar(as.character(third_birth$par_occupation_kldb92)) == 3 & is.na(third_birth$par_occupation_kldb92)==0),]
temp_3 <- third_birth[((third_birth$par_occupation_kldb92 == -2) & is.na(third_birth$par_occupation_kldb92)==0),]
temp_4 <- third_birth[is.na(third_birth$par_occupation_kldb92),]
temp_1$par_occupation_kldb92 <- sub('.$','',temp_1$par_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures, by=c("par_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures, by=c("par_occupation_kldb92", "syear"), all.x = T)
temp_3$par_analytic <- ifelse(temp_3$lfs==1, NA, -2)
temp_3$par_interactive <- ifelse(temp_3$lfs==1, NA, -2)
temp_4$par_analytic <- ifelse(temp_4$lfs==1, NA, -2)
temp_4$par_interactive <- ifelse(temp_4$lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
third_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
third_birth <- third_birth[with(third_birth, order(pid, syear)),]
row.names(third_birth) <- NULL

third_birth <- unique(third_birth)

# l1 task measures

temp_1 <- third_birth[(nchar(as.character(third_birth$l1_par_occupation_kldb92)) == 4 & is.na(third_birth$l1_par_occupation_kldb92)==0),]
temp_2 <- third_birth[(nchar(as.character(third_birth$l1_par_occupation_kldb92)) == 3 & is.na(third_birth$l1_par_occupation_kldb92)==0),]
temp_3 <- third_birth[((third_birth$l1_par_occupation_kldb92 == -2) & is.na(third_birth$l1_par_occupation_kldb92)==0),]
temp_4 <- third_birth[is.na(third_birth$l1_par_occupation_kldb92),]
temp_1$l1_par_occupation_kldb92 <- sub('.$','',temp_1$l1_par_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l1, by=c("l1_par_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l1, by=c("l1_par_occupation_kldb92", "syear"), all.x = T)
temp_3$l1_par_analytic <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_3$l1_par_interactive <- ifelse(temp_3$l1_lfs==1, NA, -2)
temp_4$l1_par_analytic <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_4$l1_par_interactive <- ifelse(temp_4$l1_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
third_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
third_birth <- third_birth[with(third_birth, order(pid, syear)),]
row.names(third_birth) <- NULL

third_birth <- unique(third_birth)

# l2 task measures

temp_1 <- third_birth[(nchar(as.character(third_birth$l2_par_occupation_kldb92)) == 4 & is.na(third_birth$l2_par_occupation_kldb92)==0),]
temp_2 <- third_birth[(nchar(as.character(third_birth$l2_par_occupation_kldb92)) == 3 & is.na(third_birth$l2_par_occupation_kldb92)==0),]
temp_3 <- third_birth[((third_birth$l2_par_occupation_kldb92 == -2) & is.na(third_birth$l2_par_occupation_kldb92)==0),]
temp_4 <- third_birth[is.na(third_birth$l2_par_occupation_kldb92),]
temp_1$l2_par_occupation_kldb92 <- sub('.$','',temp_1$l2_par_occupation_kldb92)
temp_1 <- merge(temp_1, task_measures_l2, by=c("l2_par_occupation_kldb92", "syear"), all.x = T)
temp_2 <- merge(temp_2, task_measures_l2, by=c("l2_par_occupation_kldb92", "syear"), all.x = T)
temp_3$l2_par_analytic <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_3$l2_par_interactive <- ifelse(temp_3$l2_lfs==1, NA, -2)
temp_4$l2_par_analytic <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_4$l2_par_interactive <- ifelse(temp_4$l2_lfs==1, NA, -2)
temp_1 <- temp_1[,colnames(temp_3)]
temp_2 <- temp_2[,colnames(temp_3)]
third_birth <- rbind(temp_1, temp_2, temp_3, temp_4)
third_birth <- third_birth[with(third_birth, order(pid, syear)),]
row.names(third_birth) <- NULL

third_birth <- unique(third_birth)

for(i in grep("^par_analytic$", colnames(first_birth)):grep("^l2_par_interactive$", colnames(first_birth))){
  first_birth[[i]] <- ifelse(first_birth$par_pid==-2, -2, first_birth[[i]])
}

for(i in grep("^par_analytic$", colnames(second_birth)):grep("^l2_par_interactive$", colnames(second_birth))){
  second_birth[[i]] <- ifelse(second_birth$par_pid==-2, -2, second_birth[[i]])
}

for(i in grep("^par_analytic$", colnames(third_birth)):grep("^l2_par_interactive$", colnames(third_birth))){
  third_birth[[i]] <- ifelse(third_birth$par_pid==-2, -2, third_birth[[i]])
}

rm(list=setdiff(ls(), c("first_birth", "second_birth", "third_birth")))

# Categorize variables

# FIRST BIRTH

  unique(first_birth$age)
  first_birth$baseline <- ifelse(first_birth$age>=20 & first_birth$age<25, 1,
                                 ifelse(first_birth$age>=25 & first_birth$age<30, 2,
                                        ifelse(first_birth$age>=30 & first_birth$age<34, 3,
                                               ifelse(first_birth$age>=34 & first_birth$age<40, 4, 5))))
  unique(first_birth$baseline)
  
  unique(first_birth$syear)
  first_birth$period <- ifelse(first_birth$syear>=1984 & first_birth$syear<2000, 1,
                                 ifelse(first_birth$syear>=2000 & first_birth$syear<2008, 2,3))
  unique(first_birth$period)

  # ### analytic ###
  # 
  # # cat_analytic
  # 
  # unique(first_birth$analytic)
  # first_birth$cat_analytic <- ifelse(is.na(first_birth$analytic), 0,
  #                                    ifelse(first_birth$lfs!=1, 1,
  #                                           ifelse(first_birth$analytic>=0 & first_birth$analytic<33, 2,
  #                                                  ifelse(first_birth$analytic>=33 & first_birth$analytic<66, 3,4))))
  # unique(first_birth$cat_analytic)
  # 
  # # cat_l1_analytic
  #   
  # unique(first_birth$l1_analytic)
  # first_birth$cat_l1_analytic <- ifelse(is.na(first_birth$l1_analytic), 0,
  #                                       ifelse(first_birth$l1_lfs!=1, 1,
  #                                              ifelse(first_birth$l1_analytic>=0 & first_birth$l1_analytic<33, 2,
  #                                                     ifelse(first_birth$l1_analytic>=33 & first_birth$l1_analytic<66, 3,4))))
  # unique(first_birth$cat_l1_analytic)
  # 
  # # cat_l2_analytic
  # 
  # unique(first_birth$l2_analytic)
  # first_birth$cat_l2_analytic <- ifelse(is.na(first_birth$l2_analytic), 0,
  #                                    ifelse(first_birth$l2_lfs!=1, 1,
  #                                           ifelse(first_birth$l2_analytic>=0 & first_birth$l2_analytic<33, 2,
  #                                                  ifelse(first_birth$l2_analytic>=33 & first_birth$l2_analytic<66, 3,4))))
  # unique(first_birth$cat_l2_analytic)
  # 
  # ### interactive ###
  # 
  # # cat_interactive
  # 
  # unique(first_birth$interactive)
  # first_birth$cat_interactive <- ifelse(is.na(first_birth$interactive), 0,
  #                                    ifelse(first_birth$lfs!=1, 1,
  #                                           ifelse(first_birth$interactive>=0 & first_birth$interactive<33, 2,
  #                                                  ifelse(first_birth$interactive>=33 & first_birth$interactive<66, 3,4))))
  # unique(first_birth$cat_interactive)
  # 
  # # cat_l1_interactive
  # 
  # unique(first_birth$l1_interactive)
  # first_birth$cat_l1_interactive <- ifelse(is.na(first_birth$l1_interactive), 0,
  #                                          ifelse(first_birth$l1_lfs!=1, 1,
  #                                                 ifelse(first_birth$l1_interactive>=0 & first_birth$l1_interactive<33, 2,
  #                                                        ifelse(first_birth$l1_interactive>=33 & first_birth$l1_interactive<66, 3,4))))
  # unique(first_birth$cat_l1_interactive)
  # 
  # # cat_l2_interactive
  # 
  # unique(first_birth$l2_interactive)
  # first_birth$cat_l2_interactive <- ifelse(is.na(first_birth$l2_interactive), 0,
  #                                       ifelse(first_birth$l2_lfs!=1, 1,
  #                                              ifelse(first_birth$l2_interactive>=0 & first_birth$l2_interactive<33, 2,
  #                                                     ifelse(first_birth$l2_interactive>=33 & first_birth$l2_interactive<66, 3,4))))
  # unique(first_birth$cat_l2_interactive)
  # 
  # ### par_analytic ###
  # 
  # # cat_par_analytic
  # 
  # unique(first_birth$par_analytic)
  # first_birth$cat_par_analytic <- ifelse(is.na(first_birth$par_analytic), 0,
  #                                    ifelse(first_birth$par_lfs!=1, 1,
  #                                           ifelse(first_birth$par_analytic>=0 & first_birth$par_analytic<33, 2,
  #                                                  ifelse(first_birth$par_analytic>=33 & first_birth$par_analytic<66, 3,4))))
  # unique(first_birth$cat_par_analytic)
  # 
  # # cat_l1_par_analytic
  # 
  # unique(first_birth$l1_par_analytic)
  # first_birth$cat_l1_par_analytic <- ifelse(is.na(first_birth$l1_par_analytic), 0,
  #                                           ifelse(first_birth$l1_par_lfs!=1, 1,
  #                                                  ifelse(first_birth$l1_par_analytic>=0 & first_birth$l1_par_analytic<33, 2,
  #                                                         ifelse(first_birth$l1_par_analytic>=33 & first_birth$l1_par_analytic<66, 3,4))))
  # unique(first_birth$cat_l1_par_analytic)
  # 
  # # cat_l2_par_analytic
  # 
  # unique(first_birth$l2_par_analytic)
  # first_birth$cat_l2_par_analytic <- ifelse(is.na(first_birth$l2_par_analytic), 0,
  #                                       ifelse(first_birth$l2_par_lfs!=1, 1,
  #                                              ifelse(first_birth$l2_par_analytic>=0 & first_birth$l2_par_analytic<33, 2,
  #                                                     ifelse(first_birth$l2_par_analytic>=33 & first_birth$l2_par_analytic<66, 3,4))))
  # unique(first_birth$cat_l2_par_analytic)
  # 
  # ### par_interactive ###
  # 
  # # cat_par_interactive
  # 
  # unique(first_birth$par_interactive)
  # first_birth$cat_par_interactive <- ifelse(is.na(first_birth$par_interactive), 0,
  #                                       ifelse(first_birth$par_lfs!=1, 1,
  #                                              ifelse(first_birth$par_interactive>=0 & first_birth$par_interactive<33, 2,
  #                                                     ifelse(first_birth$par_interactive>=33 & first_birth$par_interactive<66, 3,4))))
  # unique(first_birth$cat_par_interactive)
  # 
  # # cat_l1_par_interactive
  # 
  # unique(first_birth$l1_par_interactive)
  # first_birth$cat_l1_par_interactive <- ifelse(is.na(first_birth$l1_par_interactive), 0,
  #                                              ifelse(first_birth$l1_par_lfs!=1, 1,
  #                                                     ifelse(first_birth$l1_par_interactive>=0 & first_birth$l1_par_interactive<33, 2,
  #                                                            ifelse(first_birth$l1_par_interactive>=33 & first_birth$l1_par_interactive<66, 3,4))))
  # unique(first_birth$cat_l1_par_interactive)
  # 
  # # cat_l2_par_interactive
  # 
  # unique(first_birth$l2_par_interactive)
  # first_birth$cat_l2_par_interactive <- ifelse(is.na(first_birth$l2_par_interactive), 0,
  #                                          ifelse(first_birth$l2_par_lfs!=1, 1,
  #                                                 ifelse(first_birth$l2_par_interactive>=0 & first_birth$l2_par_interactive<33, 2,
  #                                                        ifelse(first_birth$l2_par_interactive>=33 & first_birth$l2_par_interactive<66, 3,4))))
  # unique(first_birth$cat_l2_par_interactive)

# SECOND BIRTH

  unique(second_birth$age)
  second_birth$baseline <- ifelse(second_birth$age>=20 & second_birth$age<25, 1,
                                 ifelse(second_birth$age>=25 & second_birth$age<30, 2,
                                        ifelse(second_birth$age>=30 & second_birth$age<34, 3,
                                               ifelse(second_birth$age>=34 & second_birth$age<40, 4, 5))))
  unique(second_birth$baseline)
  
  unique(second_birth$syear)
  second_birth$period <- ifelse(second_birth$syear>=1984 & second_birth$syear<2000, 1,
                               ifelse(second_birth$syear>=2000 & second_birth$syear<2008, 2,3))
  unique(second_birth$period)
  
  # ### analytic ###
  # 
  # # cat_analytic
  # 
  # unique(second_birth$analytic)
  # second_birth$cat_analytic <- ifelse(is.na(second_birth$analytic), 0,
  #                                    ifelse(second_birth$lfs!=1, 1,
  #                                           ifelse(second_birth$analytic>=0 & second_birth$analytic<33, 2,
  #                                                  ifelse(second_birth$analytic>=33 & second_birth$analytic<66, 3,4))))
  # unique(second_birth$cat_analytic)
  # 
  # # cat_l1_analytic
  # 
  # unique(second_birth$l1_analytic)
  # second_birth$cat_l1_analytic <- ifelse(is.na(second_birth$l1_analytic), 0,
  #                                        ifelse(second_birth$l1_lfs!=1, 1,
  #                                               ifelse(second_birth$l1_analytic>=0 & second_birth$l1_analytic<33, 2,
  #                                                      ifelse(second_birth$l1_analytic>=33 & second_birth$l1_analytic<66, 3,4))))
  # unique(second_birth$cat_l1_analytic)
  # 
  # # cat_l2_analytic
  # 
  # unique(second_birth$l2_analytic)
  # second_birth$cat_l2_analytic <- ifelse(is.na(second_birth$l2_analytic), 0,
  #                                       ifelse(second_birth$l2_lfs!=1, 1,
  #                                              ifelse(second_birth$l2_analytic>=0 & second_birth$l2_analytic<33, 2,
  #                                                     ifelse(second_birth$l2_analytic>=33 & second_birth$l2_analytic<66, 3,4))))
  # unique(second_birth$cat_l2_analytic)
  # 
  # # cat_analytic_child_1
  # 
  # unique(second_birth$analytic_child_1)
  # second_birth$cat_analytic_child_1 <- ifelse(is.na(second_birth$analytic_child_1), 0,
  #                                        ifelse(second_birth$analytic_child_1==-2, 1,
  #                                               ifelse(second_birth$analytic_child_1>=0 & second_birth$analytic_child_1<33, 2,
  #                                                      ifelse(second_birth$analytic_child_1>=33 & second_birth$analytic_child_1<66, 3,4))))
  # unique(second_birth$cat_analytic_child_1)
  # 
  # ### interactive ###
  # 
  # # cat_interactive
  # 
  # unique(second_birth$interactive)
  # second_birth$cat_interactive <- ifelse(is.na(second_birth$interactive), 0,
  #                                       ifelse(second_birth$lfs!=1, 1,
  #                                              ifelse(second_birth$interactive>=0 & second_birth$interactive<33, 2,
  #                                                     ifelse(second_birth$interactive>=33 & second_birth$interactive<66, 3,4))))
  # unique(second_birth$cat_interactive)
  # 
  # # cat_l1_interactive
  # 
  # unique(second_birth$l1_interactive)
  # second_birth$cat_l1_interactive <- ifelse(is.na(second_birth$l1_interactive), 0,
  #                                           ifelse(second_birth$l1_lfs!=1, 1,
  #                                                  ifelse(second_birth$l1_interactive>=0 & second_birth$l1_interactive<33, 2,
  #                                                         ifelse(second_birth$l1_interactive>=33 & second_birth$l1_interactive<66, 3,4))))
  # unique(second_birth$cat_l1_interactive)
  # 
  # # cat_l2_interactive
  # 
  # unique(second_birth$l2_interactive)
  # second_birth$cat_l2_interactive <- ifelse(is.na(second_birth$l2_interactive), 0,
  #                                          ifelse(second_birth$l2_lfs!=1, 1,
  #                                                 ifelse(second_birth$l2_interactive>=0 & second_birth$l2_interactive<33, 2,
  #                                                        ifelse(second_birth$l2_interactive>=33 & second_birth$l2_interactive<66, 3,4))))
  # unique(second_birth$cat_l2_interactive)
  # 
  # # cat_interactive_child_1
  # 
  # unique(second_birth$interactive_child_1)
  # second_birth$cat_interactive_child_1 <- ifelse(is.na(second_birth$interactive_child_1), 0,
  #                                             ifelse(second_birth$interactive_child_1==-2, 1,
  #                                                    ifelse(second_birth$interactive_child_1>=0 & second_birth$interactive_child_1<33, 2,
  #                                                           ifelse(second_birth$interactive_child_1>=33 & second_birth$interactive_child_1<66, 3,4))))
  # unique(second_birth$cat_interactive_child_1)
  # 
  # 
  # ### par_analytic ###
  # 
  # # cat_par_analytic
  # 
  # unique(second_birth$par_analytic)
  # second_birth$cat_par_analytic <- ifelse(is.na(second_birth$par_analytic), 0,
  #                                        ifelse(second_birth$par_lfs!=1, 1,
  #                                               ifelse(second_birth$par_analytic>=0 & second_birth$par_analytic<33, 2,
  #                                                      ifelse(second_birth$par_analytic>=33 & second_birth$par_analytic<66, 3,4))))
  # unique(second_birth$cat_par_analytic)
  # 
  # # cat_l1_par_analytic
  # 
  # unique(second_birth$l1_par_analytic)
  # second_birth$cat_l1_par_analytic <- ifelse(is.na(second_birth$l1_par_analytic), 0,
  #                                            ifelse(second_birth$l1_par_lfs!=1, 1,
  #                                                   ifelse(second_birth$l1_par_analytic>=0 & second_birth$l1_par_analytic<33, 2,
  #                                                          ifelse(second_birth$l1_par_analytic>=33 & second_birth$l1_par_analytic<66, 3,4))))
  # unique(second_birth$cat_l1_par_analytic)
  # 
  # # cat_l2_par_analytic
  # 
  # unique(second_birth$l2_par_analytic)
  # second_birth$cat_l2_par_analytic <- ifelse(is.na(second_birth$l2_par_analytic), 0,
  #                                           ifelse(second_birth$l2_par_lfs!=1, 1,
  #                                                  ifelse(second_birth$l2_par_analytic>=0 & second_birth$l2_par_analytic<33, 2,
  #                                                         ifelse(second_birth$l2_par_analytic>=33 & second_birth$l2_par_analytic<66, 3,4))))
  # unique(second_birth$cat_l2_par_analytic)
  # 
  # ### par_interactive ###
  # 
  # # cat_par_interactive
  # 
  # unique(second_birth$par_interactive)
  # second_birth$cat_par_interactive <- ifelse(is.na(second_birth$par_interactive), 0,
  #                                           ifelse(second_birth$par_lfs!=1, 1,
  #                                                  ifelse(second_birth$par_interactive>=0 & second_birth$par_interactive<33, 2,
  #                                                         ifelse(second_birth$par_interactive>=33 & second_birth$par_interactive<66, 3,4))))
  # unique(second_birth$cat_par_interactive)
  # 
  # # cat_l1_par_interactive
  # 
  # unique(second_birth$l1_par_interactive)
  # second_birth$cat_l1_par_interactive <- ifelse(is.na(second_birth$l1_par_interactive), 0,
  #                                               ifelse(second_birth$l1_par_lfs!=1, 1,
  #                                                      ifelse(second_birth$l1_par_interactive>=0 & second_birth$l1_par_interactive<33, 2,
  #                                                             ifelse(second_birth$l1_par_interactive>=33 & second_birth$l1_par_interactive<66, 3,4))))
  # unique(second_birth$cat_l1_par_interactive)
  # 
  # # cat_l2_par_interactive
  # 
  # unique(second_birth$l2_par_interactive)
  # second_birth$cat_l2_par_interactive <- ifelse(is.na(second_birth$l2_par_interactive), 0,
  #                                              ifelse(second_birth$l2_par_lfs!=1, 1,
  #                                                     ifelse(second_birth$l2_par_interactive>=0 & second_birth$l2_par_interactive<33, 2,
  #                                                            ifelse(second_birth$l2_par_interactive>=33 & second_birth$l2_par_interactive<66, 3,4))))
  # unique(second_birth$cat_l2_par_interactive)

# THIRD BIRTH

  unique(third_birth$age)
  third_birth$baseline <- ifelse(third_birth$age>=20 & third_birth$age<25, 1,
                                  ifelse(third_birth$age>=25 & third_birth$age<30, 2,
                                         ifelse(third_birth$age>=30 & third_birth$age<34, 3,
                                                ifelse(third_birth$age>=34 & third_birth$age<40, 4, 5))))
  unique(third_birth$baseline)
  
  unique(third_birth$syear)
  third_birth$period <- ifelse(third_birth$syear>=1984 & third_birth$syear<2000, 1,
                                ifelse(third_birth$syear>=2000 & third_birth$syear<2008, 2,3))
  unique(third_birth$period)
  
  # ### analytic ###
  # 
  # # cat_analytic
  # 
  # unique(third_birth$analytic)
  # third_birth$cat_analytic <- ifelse(is.na(third_birth$analytic), 0,
  #                                    ifelse(third_birth$lfs!=1, 1,
  #                                           ifelse(third_birth$analytic>=0 & third_birth$analytic<33, 2,
  #                                                  ifelse(third_birth$analytic>=33 & third_birth$analytic<66, 3,4))))
  # unique(third_birth$cat_analytic)
  # 
  # # cat_l1_analytic
  # 
  # unique(third_birth$l1_analytic)
  # third_birth$cat_l1_analytic <- ifelse(is.na(third_birth$l1_analytic), 0,
  #                                       ifelse(third_birth$l1_lfs!=1, 1,
  #                                              ifelse(third_birth$l1_analytic>=0 & third_birth$l1_analytic<33, 2,
  #                                                     ifelse(third_birth$l1_analytic>=33 & third_birth$l1_analytic<66, 3,4))))
  # unique(third_birth$cat_l1_analytic)
  # 
  # # cat_l2_analytic
  # 
  # unique(third_birth$l2_analytic)
  # third_birth$cat_l2_analytic <- ifelse(is.na(third_birth$l2_analytic), 0,
  #                                       ifelse(third_birth$l2_lfs!=1, 1,
  #                                              ifelse(third_birth$l2_analytic>=0 & third_birth$l2_analytic<33, 2,
  #                                                     ifelse(third_birth$l2_analytic>=33 & third_birth$l2_analytic<66, 3,4))))
  # unique(third_birth$cat_l2_analytic)
  # 
  # # cat_analytic_child_1
  # 
  # unique(third_birth$analytic_child_1)
  # third_birth$cat_analytic_child_1 <- ifelse(is.na(third_birth$analytic_child_1), 0,
  #                                             ifelse(third_birth$analytic_child_1==-2, 1,
  #                                                    ifelse(third_birth$analytic_child_1>=0 & third_birth$analytic_child_1<33, 2,
  #                                                           ifelse(third_birth$analytic_child_1>=33 & third_birth$analytic_child_1<66, 3,4))))
  # unique(third_birth$cat_analytic_child_1)
  # 
  # # cat_analytic_child_2
  # 
  # unique(third_birth$analytic_child_2)
  # third_birth$cat_analytic_child_2 <- ifelse(is.na(third_birth$analytic_child_2), 0,
  #                                            ifelse(third_birth$analytic_child_2==-2, 1,
  #                                                   ifelse(third_birth$analytic_child_2>=0 & third_birth$analytic_child_2<33, 2,
  #                                                          ifelse(third_birth$analytic_child_2>=33 & third_birth$analytic_child_2<66, 3,4))))
  # unique(third_birth$cat_analytic_child_2)
  # 
  # ### interactive ###
  # 
  # # cat_interactive
  # 
  # unique(third_birth$interactive)
  # third_birth$cat_interactive <- ifelse(is.na(third_birth$interactive), 0,
  #                                       ifelse(third_birth$lfs!=1, 1,
  #                                              ifelse(third_birth$interactive>=0 & third_birth$interactive<33, 2,
  #                                                     ifelse(third_birth$interactive>=33 & third_birth$interactive<66, 3,4))))
  # unique(third_birth$cat_interactive)
  # 
  # # cat_l1_interactive
  # 
  # unique(third_birth$l1_interactive)
  # third_birth$cat_l1_interactive <- ifelse(is.na(third_birth$l1_interactive), 0,
  #                                          ifelse(third_birth$l1_lfs!=1, 1,
  #                                                 ifelse(third_birth$l1_interactive>=0 & third_birth$l1_interactive<33, 2,
  #                                                        ifelse(third_birth$l1_interactive>=33 & third_birth$l1_interactive<66, 3,4))))
  # unique(third_birth$cat_l1_interactive)
  # 
  # # cat_l2_interactive
  # 
  # unique(third_birth$l2_interactive)
  # third_birth$cat_l2_interactive <- ifelse(is.na(third_birth$l2_interactive), 0,
  #                                          ifelse(third_birth$l2_lfs!=1, 1,
  #                                                 ifelse(third_birth$l2_interactive>=0 & third_birth$l2_interactive<33, 2,
  #                                                        ifelse(third_birth$l2_interactive>=33 & third_birth$l2_interactive<66, 3,4))))
  # unique(third_birth$cat_l2_interactive)
  # 
  # # cat_interactive_child_1
  # 
  # unique(third_birth$interactive_child_1)
  # third_birth$cat_interactive_child_1 <- ifelse(is.na(third_birth$interactive_child_1), 0,
  #                                            ifelse(third_birth$interactive_child_1==-2, 1,
  #                                                   ifelse(third_birth$interactive_child_1>=0 & third_birth$interactive_child_1<33, 2,
  #                                                          ifelse(third_birth$interactive_child_1>=33 & third_birth$interactive_child_1<66, 3,4))))
  # unique(third_birth$cat_interactive_child_1)
  # 
  # # cat_interactive_child_2
  # 
  # unique(third_birth$interactive_child_2)
  # third_birth$cat_interactive_child_2 <- ifelse(is.na(third_birth$interactive_child_2), 0,
  #                                            ifelse(third_birth$interactive_child_2==-2, 1,
  #                                                   ifelse(third_birth$interactive_child_2>=0 & third_birth$interactive_child_2<33, 2,
  #                                                          ifelse(third_birth$interactive_child_2>=33 & third_birth$interactive_child_2<66, 3,4))))
  # unique(third_birth$cat_interactive_child_2)
  # 
  # ### par_analytic ###
  # 
  # # cat_par_analytic
  # 
  # unique(third_birth$par_analytic)
  # third_birth$cat_par_analytic <- ifelse(is.na(third_birth$par_analytic), 0,
  #                                        ifelse(third_birth$par_lfs!=1, 1,
  #                                               ifelse(third_birth$par_analytic>=0 & third_birth$par_analytic<33, 2,
  #                                                      ifelse(third_birth$par_analytic>=33 & third_birth$par_analytic<66, 3,4))))
  # unique(third_birth$cat_par_analytic)
  # 
  # # cat_l1_par_analytic
  # 
  # unique(third_birth$l1_par_analytic)
  # third_birth$cat_l1_par_analytic <- ifelse(is.na(third_birth$l1_par_analytic), 0,
  #                                           ifelse(third_birth$l1_par_lfs!=1, 1,
  #                                                  ifelse(third_birth$l1_par_analytic>=0 & third_birth$l1_par_analytic<33, 2,
  #                                                         ifelse(third_birth$l1_par_analytic>=33 & third_birth$l1_par_analytic<66, 3,4))))
  # unique(third_birth$cat_l1_par_analytic)
  # 
  # # cat_l2_par_analytic
  # 
  # unique(third_birth$l2_par_analytic)
  # third_birth$cat_l2_par_analytic <- ifelse(is.na(third_birth$l2_par_analytic), 0,
  #                                           ifelse(third_birth$l2_par_lfs!=1, 1,
  #                                                  ifelse(third_birth$l2_par_analytic>=0 & third_birth$l2_par_analytic<33, 2,
  #                                                         ifelse(third_birth$l2_par_analytic>=33 & third_birth$l2_par_analytic<66, 3,4))))
  # unique(third_birth$cat_l2_par_analytic)
  # 
  # ### par_interactive ###
  # 
  # # cat_par_interactive
  # 
  # unique(third_birth$par_interactive)
  # third_birth$cat_par_interactive <- ifelse(is.na(third_birth$par_interactive), 0,
  #                                           ifelse(third_birth$par_lfs!=1, 1,
  #                                                  ifelse(third_birth$par_interactive>=0 & third_birth$par_interactive<33, 2,
  #                                                         ifelse(third_birth$par_interactive>=33 & third_birth$par_interactive<66, 3,4))))
  # unique(third_birth$cat_par_interactive)
  #  
  # # cat_l1_par_interactive
  # 
  # unique(third_birth$l1_par_interactive)
  # third_birth$cat_l1_par_interactive <- ifelse(is.na(third_birth$l1_par_interactive), 0,
  #                                              ifelse(third_birth$l1_par_lfs!=1, 1,
  #                                                     ifelse(third_birth$l1_par_interactive>=0 & third_birth$l1_par_interactive<33, 2,
  #                                                            ifelse(third_birth$l1_par_interactive>=33 & third_birth$l1_par_interactive<66, 3,4))))
  # unique(third_birth$cat_l1_par_interactive)
  # 
  # # cat_l2_par_interactive
  # 
  # unique(third_birth$l2_par_interactive)
  # third_birth$cat_l2_par_interactive <- ifelse(is.na(third_birth$l2_par_interactive), 0,
  #                                              ifelse(third_birth$l2_par_lfs!=1, 1,
  #                                                     ifelse(third_birth$l2_par_interactive>=0 & third_birth$l2_par_interactive<33, 2,
  #                                                            ifelse(third_birth$l2_par_interactive>=33 & third_birth$l2_par_interactive<66, 3,4))))
  # unique(third_birth$cat_l2_par_interactive)
  
# RECODE NUMBER OF SIBLINGS
  
  unique(first_birth$num_sib)
  first_birth %>% count(num_sib)
  first_birth$num_sib2 <- case_when(
    first_birth$num_sib == 0 ~ 1,
    first_birth$num_sib == 1 ~ 2,
    first_birth$num_sib > 1 ~ 3
  )
  first_birth %>% count(num_sib2)
  first_birth$num_sib <- NULL
  colnames(first_birth)[ncol(first_birth)] <- "num_sib"
  
  unique(second_birth$num_sib)
  second_birth %>% count(num_sib)
  second_birth$num_sib2 <- case_when(
    second_birth$num_sib == 0 ~ 1,
    second_birth$num_sib == 1 ~ 2,
    second_birth$num_sib > 1 ~ 3
  )
  second_birth %>% count(num_sib2)
  second_birth$num_sib <- NULL
  colnames(second_birth)[ncol(second_birth)] <- "num_sib"
  
  unique(third_birth$num_sib)
  third_birth %>% count(num_sib)
  third_birth$num_sib2 <- case_when(
    third_birth$num_sib == 0 ~ 1,
    third_birth$num_sib == 1 ~ 2,
    third_birth$num_sib > 1 ~ 3
  )
  third_birth %>% count(num_sib2)
  third_birth$num_sib <- NULL
  colnames(third_birth)[ncol(third_birth)] <- "num_sib"
  

# REARRANGE DATA
  
colnames(first_birth)

first_birth <- first_birth[,c("pid", "syear", "period", "gebjahr", "age", "cohort", "baseline", "sex", "migrant", 
                              "num_sib", "residence", "l1_residence", "l2_residence",
                              "child_1", 
                              "occupation_kldb92", "l1_occupation_kldb92", "l2_occupation_kldb92",
                              "education", "l1_education", "l2_education",
                              "lfs", "l1_lfs", "l2_lfs",
                              "analytic", "l1_analytic", "l2_analytic", 
                              # "cat_analytic", "cat_l1_analytic", "cat_l2_analytic",
                              "interactive", "l1_interactive", "l2_interactive", 
                              # "cat_interactive", "cat_l1_interactive", "cat_l2_interactive",
                              "couple_type", "union_status", "l1_union_status", "l2_union_status",
                              "par_pid", "par_sex",
                              "par_lfs", "l1_par_lfs", "l2_par_lfs",
                              "par_occupation_kldb92", "l1_par_occupation_kldb92", "l2_par_occupation_kldb92",
                              "par_analytic", "l1_par_analytic", "l2_par_analytic", 
                              # "cat_par_analytic", "cat_l1_par_analytic", "cat_l2_par_analytic",
                              "par_interactive", "l1_par_interactive", "l2_par_interactive", 
                              # "cat_par_interactive", "cat_l1_par_interactive", "cat_l2_par_interactive",
                              "for_analysis")]

colnames(second_birth)

second_birth <- second_birth[,c("pid", "syear", "period", "gebjahr", "age", "cohort", "baseline", "sex", "migrant",
                              "num_sib", "residence", "l1_residence", "l2_residence",
                              "year_no", "child_1_y", "child_2", 
                              "occupation_kldb92", "l1_occupation_kldb92", "l2_occupation_kldb92",
                              "education", "l1_education", "l2_education",
                              "lfs", "l1_lfs", "l2_lfs",
                              "analytic", "l1_analytic", "l2_analytic", 
                              # "cat_analytic", "cat_l1_analytic", "cat_l2_analytic",
                              "analytic_child_1", 
                              # "cat_analytic_child_1",
                              "interactive", "l1_interactive", "l2_interactive", 
                              # "cat_interactive", "cat_l1_interactive", "cat_l2_interactive",
                              "interactive_child_1", 
                              # "cat_interactive_child_1",
                              "couple_type", "union_status", "l1_union_status", "l2_union_status",
                              "par_pid", "par_sex",
                              "par_lfs", "l1_par_lfs", "l2_par_lfs",
                              "par_occupation_kldb92", "l1_par_occupation_kldb92", "l2_par_occupation_kldb92",
                              "par_analytic", "l1_par_analytic", "l2_par_analytic", 
                              # "cat_par_analytic", "cat_l1_par_analytic", "cat_l2_par_analytic",
                              "par_interactive", "l1_par_interactive", "l2_par_interactive", 
                              # "cat_par_interactive", "cat_l1_par_interactive", "cat_l2_par_interactive",
                              "for_analysis")]

colnames(third_birth)

third_birth <- third_birth[,c("pid", "syear", "period", "gebjahr", "age", "cohort", "baseline", "sex", "migrant", 
                                "num_sib", "residence", "l1_residence", "l2_residence",
                                "year_no", "child_2_y", "child_3", 
                                "occupation_kldb92", "l1_occupation_kldb92", "l2_occupation_kldb92",
                                "education", "l1_education", "l2_education",
                                "lfs", "l1_lfs", "l2_lfs",
                                "analytic", "l1_analytic", "l2_analytic", 
                                # "cat_analytic", "cat_l1_analytic", "cat_l2_analytic",
                                "analytic_child_1", 
                                # "cat_analytic_child_1", 
                                "analytic_child_2", 
                                # "cat_analytic_child_2",
                                "interactive", "l1_interactive", "l2_interactive", 
                                # "cat_interactive", "cat_l1_interactive", "cat_l2_interactive",
                                "interactive_child_1", 
                                # "cat_interactive_child_1", 
                                "interactive_child_2", 
                                # "cat_interactive_child_2",
                                "couple_type", "union_status", "l1_union_status", "l2_union_status",
                                "par_pid", "par_sex",
                                "par_lfs", "l1_par_lfs", "l2_par_lfs",
                                "par_occupation_kldb92", "l1_par_occupation_kldb92", "l2_par_occupation_kldb92",
                                "par_analytic", "l1_par_analytic", "l2_par_analytic", 
                                # "cat_par_analytic", "cat_l1_par_analytic", "cat_l2_par_analytic",
                                "par_interactive", "l1_par_interactive", "l2_par_interactive", 
                                # "cat_par_interactive", "cat_l1_par_interactive", "cat_l2_par_interactive",
                                "for_analysis")]

# Calculate the age of first/second child

second_birth$age_child_1 <- second_birth$syear - second_birth$child_1_y
third_birth$age_child_2 <- third_birth$syear - third_birth$child_2_y

save(first_birth, file = "../generated_data/first_birth_for_analysis_rob.RData")
save(second_birth, file = "../generated_data/second_birth_for_analysis_rob.RData")
save(third_birth, file = "../generated_data/third_birth_for_analysis_rob.RData")

write_dta(first_birth, "../generated_data/first_birth_for_analysis_rob.dta")
write_dta(second_birth, "../generated_data/second_birth_for_analysis_rob.dta")
write_dta(third_birth, "../generated_data/third_birth_for_analysis_rob.dta")

rm(list=ls())
