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
  
  requiredPackages = c("haven", "viridis", "tidyverse", "ggplot2", "stats", "survival", "Rcpp", "foreach", "doParallel")
  for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)}
  for(i in requiredPackages){if(!require(i,character.only = TRUE)) library(i,character.only = TRUE) }
  
  rm(list=ls())
  gc()
}

################## ALL PERIODS ##################

# 0. Initial preparation

{
data <- read_dta("../final_data/first_birth_for_analysis_labeled.dta")
data <- subset(data, migrant == 0 & for_analysis == 1)
data <- data[, c("child_1", "age", "sex", "cat_l2_analytic_new", "cat_l2_interactive_new", "period", "baseline", "num_sib", "l2_residence", "l2_union_status")]

eq <- child_1 ~ factor(baseline) + factor(cat_l2_analytic_new) + factor(baseline*cat_l2_analytic_new) + factor(num_sib) + factor(l2_residence) + factor(period)
eq_refit <- y_new ~ factor(baseline) + factor(cat_l2_analytic_new) + factor(baseline*cat_l2_analytic_new) + factor(num_sib) + factor(l2_residence) + factor(period)

data <- na.omit(data)

sim_pi <- function(model, pdata, n, p) {
  odata <- model$data
  yhat <- predict(model, type = "response")
  lp <- (1 - p) / 2
  up <- 1 - lp
  set.seed(123)
  seeds <- round(runif(n, 1, 1000), 0)
  sim_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    set.seed(seeds[i])
    sim_y <- rbinom(length(yhat), size = 1, prob = yhat)
    sdata <- data.frame(y_new = sim_y, odata[,2:ncol(odata)])
    refit <- glm(eq_refit, 
                 data = sdata, family = binomial(link = cloglog), na.action = na.omit)
    bpred <- predict(refit, type = "response", newdata = pdata)
    bpred[1] <- bpred[1]*5
    bpred[2] <- bpred[2]*5
    bpred[3] <- bpred[3]*4
    bpred[4] <- bpred[4]*6
    bpred[5] <- bpred[5]*10
    1 - exp(-cumsum(bpred))
  }
  sim_ci <- t(apply(sim_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = apply(sim_y, 2, mean), lower = sim_ci[, 1], upper = sim_ci[, 2]))
}

dummy_case_1 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = c(3, 3, 4, 4, 4),
                           period = 3,
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_2 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = c(2, 3, 3, 3, 3),
                           period = 3,
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_3 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = 2,
                           period = 3,
                           num_sib = 1,
                           l2_residence = 1)

cols <- c("#440154FF", "#FDE725FF", "#21908CFF")

reps <- 1000
}

# 1. WOMEN ANALYTIC

cloglog <- glm(eq, 
               data = subset(data, sex == 2),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

# 3 cases

case_1 <- predict(cloglog, type = "response", newdata =  dummy_case_1)
case_1[1] <- case_1[1]*5
case_1[2] <- case_1[2]*5
case_1[3] <- case_1[3]*4
case_1[4] <- case_1[4]*6
case_1[5] <- case_1[5]*10
case_1 <- 1 - exp(-cumsum(case_1))
case_1_ci = sim_pi(cloglog, dummy_case_1, reps, 0.83)

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))
case_2_ci = sim_pi(cloglog, dummy_case_2, reps, 0.83)

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))
case_3_ci = sim_pi(cloglog, dummy_case_3, reps, 0.83)

all=data.frame(h=c(case_1, case_2, case_3), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))
alllow=data.frame(h=c(case_1_ci$lower, case_2_ci$lower, case_3_ci$lower), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))
allup=data.frame(h=c(case_1_ci$upper, case_2_ci$upper, case_3_ci$upper), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))

# 2. MEN ANALYTIC

cloglog <- glm(eq, 
               data = subset(data, sex == 1),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

# 3 cases

case_1 <- predict(cloglog, type = "response", newdata =  dummy_case_1)
case_1[1] <- case_1[1]*5
case_1[2] <- case_1[2]*5
case_1[3] <- case_1[3]*4
case_1[4] <- case_1[4]*6
case_1[5] <- case_1[5]*10
case_1 <- 1 - exp(-cumsum(case_1))
case_1_ci = sim_pi(cloglog, dummy_case_1, reps, 0.83)

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))
case_2_ci = sim_pi(cloglog, dummy_case_2, reps, 0.83)

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))
case_3_ci = sim_pi(cloglog, dummy_case_3, reps, 0.83)

all=data.frame(h=c(case_1, case_2, case_3), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))
alllow=data.frame(h=c(case_1_ci$lower, case_2_ci$lower, case_3_ci$lower), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))
allup=data.frame(h=c(case_1_ci$upper, case_2_ci$upper, case_3_ci$upper), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))

# 3. WOMEN INTERACTIVE

eq <- child_1 ~ factor(baseline) + factor(cat_l2_interactive_new) + factor(baseline*cat_l2_interactive_new) + factor(num_sib) + factor(l2_residence) + factor(period)
eq_refit <- y_new ~ factor(baseline) + factor(cat_l2_interactive_new) + factor(baseline*cat_l2_interactive_new) + factor(num_sib) + factor(l2_residence) + factor(period)

dummy_case_1 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = c(3, 3, 4, 4, 4),
                           period = 3,
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_2 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = c(2, 3, 3, 3, 3),
                           period = 3,
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_3 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = 2,
                           period = 3,
                           num_sib = 1,
                           l2_residence = 1)

cloglog <- glm(eq, 
               data = subset(data, sex == 2),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

# 3 cases

case_1 <- predict(cloglog, type = "response", newdata =  dummy_case_1)
case_1[1] <- case_1[1]*5
case_1[2] <- case_1[2]*5
case_1[3] <- case_1[3]*4
case_1[4] <- case_1[4]*6
case_1[5] <- case_1[5]*10
case_1 <- 1 - exp(-cumsum(case_1))
case_1_ci = sim_pi(cloglog, dummy_case_1, reps, 0.83)

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))
case_2_ci = sim_pi(cloglog, dummy_case_2, reps, 0.83)

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))
case_3_ci = sim_pi(cloglog, dummy_case_3, reps, 0.83)

all=data.frame(h=c(case_1, case_2, case_3), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))
alllow=data.frame(h=c(case_1_ci$lower, case_2_ci$lower, case_3_ci$lower), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))
allup=data.frame(h=c(case_1_ci$upper, case_2_ci$upper, case_3_ci$upper), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))

# 4. MEN INTERACTIVE

cloglog <- glm(eq, 
               data = subset(data, sex == 1),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

# 3 cases

case_1 <- predict(cloglog, type = "response", newdata =  dummy_case_1)
case_1[1] <- case_1[1]*5
case_1[2] <- case_1[2]*5
case_1[3] <- case_1[3]*4
case_1[4] <- case_1[4]*6
case_1[5] <- case_1[5]*10
case_1 <- 1 - exp(-cumsum(case_1))
case_1_ci = sim_pi(cloglog, dummy_case_1, reps, 0.83)

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))
case_2_ci = sim_pi(cloglog, dummy_case_2, reps, 0.83)

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))
case_3_ci = sim_pi(cloglog, dummy_case_3, reps, 0.83)

all=data.frame(h=c(case_1, case_2, case_3), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))
alllow=data.frame(h=c(case_1_ci$lower, case_2_ci$lower, case_3_ci$lower), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))
allup=data.frame(h=c(case_1_ci$upper, case_2_ci$upper, case_3_ci$upper), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))

################## PERIOD 1984 -- 1999 ##################

# 1. WOMEN ANALYTIC

rm(list=ls())
gc()
total_ci <- data.frame()

data <- read_dta("../final_data/first_birth_for_analysis_labeled.dta")
data <- subset(data, migrant == 0 & for_analysis == 1)
data <- data[, c("child_1", "age", "sex", "cat_l2_analytic_new", "cat_l2_interactive_new", "period", "baseline", "num_sib", "l2_residence", "l2_union_status")]
data <- subset(data, period == 1)
data <- subset(data, cat_l2_analytic_new != 4)
data <- na.omit(data)

eq <- child_1 ~ factor(baseline) + factor(cat_l2_analytic_new) + factor(baseline*cat_l2_analytic_new) + factor(num_sib) + factor(l2_residence)

cloglog <- glm(eq, 
               data = subset(data, sex == 2),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

dummy_case_2 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = c(2, 3, 3, 3, 3),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_3 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = 2,
                           num_sib = 1,
                           l2_residence = 1)

# 2 cases

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))

all=data.frame(h=c(case_2, case_3), baseline=c(1:5), case=c(rep("2", 5), rep("3", 5)))

all$period <- "1984-1999"
all$tm <- "analytic"
all$sex <- 2

total_ci <- rbind(total_ci, all)

# 2. MEN ANALYTIC

cloglog <- glm(eq, 
               data = subset(data, sex == 1),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

dummy_case_2 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = c(2, 3, 3, 3, 3),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_3 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = 2,
                           num_sib = 1,
                           l2_residence = 1)

# 2 cases

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))

all=data.frame(h=c(case_2, case_3), baseline=c(1:5), case=c(rep("2", 5), rep("3", 5)))

all$period <- "1984-1999"
all$tm <- "analytic"
all$sex <- 1

total_ci <- rbind(total_ci, all)

# 3. WOMEN INTERACTIVE

data <- read_dta("../final_data/first_birth_for_analysis_labeled.dta")
data <- subset(data, migrant == 0 & for_analysis == 1)
data <- data[, c("child_1", "age", "sex", "cat_l2_analytic_new", "cat_l2_interactive_new", "period", "baseline", "num_sib", "l2_residence", "l2_union_status")]
data <- subset(data, period == 1)
data <- subset(data, cat_l2_interactive_new != 4)
data <- na.omit(data)

eq <- child_1 ~ factor(baseline) + factor(cat_l2_interactive_new) + factor(baseline*cat_l2_interactive_new) + factor(num_sib) + factor(l2_residence)

cloglog <- glm(eq, 
               data = subset(data, sex == 2),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

dummy_case_2 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = c(2, 3, 3, 3, 3),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_3 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = 2,
                           num_sib = 1,
                           l2_residence = 1)

# 2 cases

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))

all=data.frame(h=c(case_2, case_3), baseline=c(1:5), case=c(rep("2", 5), rep("3", 5)))

all$period <- "1984-1999"
all$tm <- "interactive"
all$sex <- 2

total_ci <- rbind(total_ci, all)

# 4. MEN ANALYTIC

cloglog <- glm(eq, 
               data = subset(data, sex == 1),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

dummy_case_2 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = c(2, 3, 3, 3, 3),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_3 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = 2,
                           num_sib = 1,
                           l2_residence = 1)

# 2 cases

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))

all=data.frame(h=c(case_2, case_3), baseline=c(1:5), case=c(rep("2", 5), rep("3", 5)))

all$period <- "1984-1999"
all$tm <- "interactive"
all$sex <- 1

total_ci <- rbind(total_ci, all)

################## PERIOD 2000 -- 2018 ##################

# 1. WOMEN ANALYTIC

rm(list=setdiff(ls(), "total_ci"))
gc()

data <- read_dta("../final_data/first_birth_for_analysis_labeled.dta")
data <- subset(data, migrant == 0 & for_analysis == 1)
data <- data[, c("child_1", "age", "sex", "cat_l2_analytic_new", "cat_l2_interactive_new", "period", "baseline", "num_sib", "l2_residence", "l2_union_status")]
data <- subset(data, period != 1)
data <- na.omit(data)

eq <- child_1 ~ factor(baseline) + factor(cat_l2_analytic_new) + factor(baseline*cat_l2_analytic_new) + factor(num_sib) + factor(l2_residence)

cloglog <- glm(eq, 
               data = subset(data, sex == 2),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

dummy_case_1 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = c(3, 3, 4, 4, 4),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_2 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = c(2, 3, 3, 3, 3),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_3 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = 2,
                           num_sib = 1,
                           l2_residence = 1)

# 3 cases

case_1 <- predict(cloglog, type = "response", newdata =  dummy_case_1)
case_1[1] <- case_1[1]*5
case_1[2] <- case_1[2]*5
case_1[3] <- case_1[3]*4
case_1[4] <- case_1[4]*6
case_1[5] <- case_1[5]*10
case_1 <- 1 - exp(-cumsum(case_1))

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))

all=data.frame(h=c(case_1, case_2, case_3), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))

all$period <- "2000-2018"
all$tm <- "analytic"
all$sex <- 2

total_ci <- rbind(total_ci, all)

# 2. MEN ANALYTIC

cloglog <- glm(eq, 
               data = subset(data, sex == 1),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

dummy_case_1 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = c(3, 3, 4, 4, 4),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_2 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = c(2, 3, 3, 3, 3),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_3 <- data.frame(baseline = c(1:5),
                           cat_l2_analytic_new = 2,
                           num_sib = 1,
                           l2_residence = 1)

# 3 cases

case_1 <- predict(cloglog, type = "response", newdata =  dummy_case_1)
case_1[1] <- case_1[1]*5
case_1[2] <- case_1[2]*5
case_1[3] <- case_1[3]*4
case_1[4] <- case_1[4]*6
case_1[5] <- case_1[5]*10
case_1 <- 1 - exp(-cumsum(case_1))

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))

all=data.frame(h=c(case_1, case_2, case_3), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))

all$period <- "2000-2018"
all$tm <- "analytic"
all$sex <- 1

total_ci <- rbind(total_ci, all)

# 3. WOMEN INTERACTIVE

data <- read_dta("../final_data/first_birth_for_analysis_labeled.dta")
data <- subset(data, migrant == 0 & for_analysis == 1)
data <- data[, c("child_1", "age", "sex", "cat_l2_analytic_new", "cat_l2_interactive_new", "period", "baseline", "num_sib", "l2_residence", "l2_union_status")]
data <- subset(data, period != 1)
data <- na.omit(data)

eq <- child_1 ~ factor(baseline) + factor(cat_l2_interactive_new) + factor(baseline*cat_l2_interactive_new) + factor(num_sib) + factor(l2_residence)

cloglog <- glm(eq, 
               data = subset(data, sex == 2),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

dummy_case_1 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = c(3, 3, 4, 4, 4),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_2 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = c(2, 3, 3, 3, 3),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_3 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = 2,
                           num_sib = 1,
                           l2_residence = 1)

# 3 cases

case_1 <- predict(cloglog, type = "response", newdata =  dummy_case_1)
case_1[1] <- case_1[1]*5
case_1[2] <- case_1[2]*5
case_1[3] <- case_1[3]*4
case_1[4] <- case_1[4]*6
case_1[5] <- case_1[5]*10
case_1 <- 1 - exp(-cumsum(case_1))

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))

all=data.frame(h=c(case_1, case_2, case_3), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))

all$period <- "2000-2018"
all$tm <- "interactive"
all$sex <- 2

total_ci <- rbind(total_ci, all)

# 4. MEN INTERACTIVE

cloglog <- glm(eq, 
               data = subset(data, sex == 1),  family = binomial(link = cloglog), na.action = na.omit)
summary(cloglog)

dummy_case_1 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = c(3, 3, 4, 4, 4),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_2 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = c(2, 3, 3, 3, 3),
                           num_sib = 1,
                           l2_residence = 1)

dummy_case_3 <- data.frame(baseline = c(1:5),
                           cat_l2_interactive_new = 2,
                           num_sib = 1,
                           l2_residence = 1)

# 3 cases

case_1 <- predict(cloglog, type = "response", newdata =  dummy_case_1)
case_1[1] <- case_1[1]*5
case_1[2] <- case_1[2]*5
case_1[3] <- case_1[3]*4
case_1[4] <- case_1[4]*6
case_1[5] <- case_1[5]*10
case_1 <- 1 - exp(-cumsum(case_1))

case_2 <- predict(cloglog, type = "response", newdata =  dummy_case_2)
case_2[1] <- case_2[1]*5
case_2[2] <- case_2[2]*5
case_2[3] <- case_2[3]*4
case_2[4] <- case_2[4]*6
case_2[5] <- case_2[5]*10
case_2 <- 1 - exp(-cumsum(case_2))

case_3 <- predict(cloglog, type = "response", newdata =  dummy_case_3)
case_3[1] <- case_3[1]*5
case_3[2] <- case_3[2]*5
case_3[3] <- case_3[3]*4
case_3[4] <- case_3[4]*6
case_3[5] <- case_3[5]*10
case_3 <- 1 - exp(-cumsum(case_3))

all=data.frame(h=c(case_1, case_2, case_3), baseline=c(1:5), case=c(rep("1", 5), rep("2", 5), rep("3", 5)))

all$period <- "2000-2018"
all$tm <- "interactive"
all$sex <- 1

total_ci <- rbind(total_ci, all)
write_dta(total_ci, "../generated_data/cum_inc.dta")


