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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))
str(climateSupport)

# Data cleaning
climateSupport$countries_n <- as.numeric(as.character(climateSupport$countries))
library(dplyr) 
climateSupport <- climateSupport  |>
  mutate(
    countries_n = case_when(
      grepl("20", countries) ~ 20,
      grepl("80", countries) ~ 80,
      grepl("160", countries) ~ 160
    ),
    sanctions_n = case_when(
      grepl("None", sanctions) ~ 0,
      grepl("5%", sanctions) ~ 5,
      grepl("15%", sanctions) ~ 15,
      grepl("20%", sanctions) ~ 20
    ))
str(climateSupport) 

# run the logit regression model
add_model <- glm(choice ~ countries_n + sanctions_n, 
               data = climateSupport, 
               family = binomial(link = "logit"))
summary(add_model)

library(stargazer)
stargazer(add_model, 
          type = "latex",               
          title = "Logistic Regression Results",
          dep.var.labels = "Support for Policy", 
          covariate.labels = c("Countries (N)", "Sanctions (Percentage)"), 
          header = FALSE)               

# run the reduced model
red_model <- glm(choice ~ 1, data = climateSupport, 
                 family = binomial(link = "logit"))
anova(red_model, add_model, test ="LRT")


#####################
# Problem 3
#####################
# run the interaction model
int_model <- glm(choice ~ countries_n * sanctions_n, 
                         data = climateSupport, 
                         family = binomial(link = "logit"))
summary(int_model)

# run the LRT
anova(add_model,int_model, test = "LRT")

stargazer(add_model, int_model, type = "latex",
          title = "Comparison of Additive and Interaction Models",
          dep.var.labels = "Support for Policy",
          column.labels = c("Additive", "Interaction"),
          covariate.labels = c("Countries (N)", "Sanctions (Percentage)", 
                               "Countries x Sanctions"),
          header = FALSE)