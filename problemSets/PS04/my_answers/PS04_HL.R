#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################
# load essential libraries
library(stargazer)
library(survival)
install.packages("eha")
library(eha)

# load data on child mortality by mother's background and child gender
data("child")

# run Cox Proportional Hazard model
cox_ml <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)

# view  outcomes
summary(cox_ml )

# output outcomes
stargazer(cox_ml, 
          apply.coef = exp,       
          t.auto = FALSE,         
          p.auto = FALSE, 
          title = "Cox Proportional Hazard Model of Child Mortality(Hazard Ratio)", 
          style = "default", 
          out = "q1_table.tex")

# run Likelihood Ratio Test for model quality assessment
drop1(cox_ml , test = "Chisq")

#####################
# Problem 2
#####################
# import data
disaster_data <- read.csv("C:/Users/janel/Documents/GitHub/StatsII_2026/datasets/disaster_response.csv", stringsAsFactors = FALSE)

# load essential libraries
install.packages("sampleSelection")
library(sampleSelection)

# run heck selection model
heck_ml <- heckit(selection = binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged, 
                  outcome = originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged, 
                  data = disaster_data)

# view outcomes
summary(heck_ml)

stargazer(heck_ml, 
          title = "Heckman Selection Model of Disaster Relief Actions", 
          style = "default", 
          out = "q2_table.tex")