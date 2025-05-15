## Install packages
library(tidyverse)
library(ggplot2)
library(corrr)
library(dplyr, warn.conflicts = FALSE)
results <- read.csv("results.csv")

## Variables
YOB <- results$YearOfBirth
Gender <- results$Gender
YSW <- results$YearStartedYT
PGA <- results$GoldenAgeYT

## Functions
## Creates function for Pearson correlation
pcorr <- function(x, y){
## Establish means for x and y
  meanx <- mean(x)
  meany <- mean(y)
  
## Intialize the numerator and squared values
  numerator <- 0
  diffxsq <- 0
  diffysq <- 0
  
## For loop to iterated through each value of each vector at position i
  for (i in 1:length(x)){
    numerator <- numerator + (x[i] - meanx)*(y[i] - meany)
    
    ## Calculate the squared differences for x and y
    diffxsq <- diffxsq + (x[i] - meanx)^2
    diffysq <- diffysq + (y[i] - meany)^2
    diffsums <- diffxsq * diffysq
  }
  ## Squares the denominator
  denominator <- sqrt(diffsums)
  ## Divides the numerator by the denominator
  r <- numerator/  denominator
  return(r)
}