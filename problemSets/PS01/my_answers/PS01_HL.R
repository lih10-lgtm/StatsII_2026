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

set.seed(123)
# define the Kolmogorov-Smirnov test function
KST <- function(data) {
# create empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
# define theoretical distribution
  theoretical_dis <- pnorm(data, 0, 1)
# generate test statistic
  D <- max(abs(empiricalCDF -theoretical_dis))
# generate p value
  sum_item <- 0
  for ( k in 1:100) {
    num <- -((2*k-1) ^ 2 * pi^2)
    den <- 8 * D^2
    item <- exp(num / den)
    sum_item <- sum_item + item
  }
  p_value <- (sqrt(pi * 2)/D) * sum_item
  cat("Test statistic: ", D, "\n")
  cat("P-value: ", p_value, "\n")
  }

# generate test statistic
set.seed(123)
test_data <- rcauchy(1000, location = 0, scale = 1)
# run the test
test <- KST(test_data)

#####################
# Problem 2
#####################
set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# define log-likelihood function
linear.lik <- function(theta, y, x) {
  
  n <- length(x)
  inter <- theta[1]
  beta <- theta[2]
  sigma <- abs(theta[3]) 
  y_hat <- inter + beta*x
  error <- y - y_hat
  sigma_2 <- sigma^2
  
  log_lik <- -0.5*n*log(2*pi)-0.5*n*log(sigma_2)-(sum(error^2)/(2*sigma_2))
  return(-log_lik)
}

# give a random guess for parameters
set.seed(123)
init_theta <- runif(3,0,5)

# uses the Newton-Raphson algorithm
lik_result <- optim(par = init_theta, 
                    fn = linear.lik, 
                    y = data$y, 
                    x = data$x, 
                    method = "BFGS",
                    hessian = TRUE)

cat("final intercept:", lik_result$par[1], "\n",
    "final_beta:     ", lik_result$par[2], "\n",
    "final_sigma:    ", abs(lik_result$par[3]), "\n")

# compare with OLD model result
OLS_ml <- lm(y ~ x, data = data)
summary(OLS_ml)

if (!require(stargazer)) install.packages("stargazer")
library(stargazer)
stargazer(OLS_ml, type = "latex", title = "OLS Regression Results", header = FALSE)

