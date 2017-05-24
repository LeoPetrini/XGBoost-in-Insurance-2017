library(data.table)
library(tidyverse)
library(forcats)
library(dtplyr)
library(mgcv)
require(Rcpp)
library(parallel)
library(ModelMetrics)
library(xgboost)
library(readr)
library(readxl)
library(Matrix)
library(rBayesianOptimization)

rm(list=ls())
set.seed(123)
options(contrasts=c("contr.treatment","contr.treatment"))
workDir<-  "/home/rstudio/Actuarial Pricing Game"
setwd(workDir)

load(file="./data/datasets2.RData")
sourceCpp(file="./code/poissonLogLoss.cpp")
source('./code/00_utility.R')


