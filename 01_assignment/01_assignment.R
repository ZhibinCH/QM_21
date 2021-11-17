# Assigment 01
# Clean R environment
rm(list = ls())

# Ex. 1 
scores <- c (4, 8, 7, 9, 10, 8, 2, 8)
average_score <- mean(scores)
var_scores <- var(scores)
scores_modified <- c (4, 8, 7, 9, 15, 8, -3, 8)
average_scores_modified <- mean(scores_modified)
var_scores_modified <- var(scores_modified)

# Ex.2 share of GDPlibrary(readxl)
rm(list = ls())
library(tidyverse)
getwd()
setwd("...") # customize the path
# Data source:
# https://www.sipri.org/databases/milex#
# https://sipri.org/sites/default/files/SIPRI-Milex-data-1949-2020_0.xlsx
meGDP <- read_excel("SIPRI-Milex-data-1949-2020_0.xlsx", sheet = "Share of GDP", range = "A6:BV197")
me <- data.frame(meGDP)
library(data.table)
me.long <- melt(setDT(me), id.vars = c("Country"), variable.name = "year")
me_2020 <- me.long[me.long$year == "X2020"]
me_2020$year <- substring(me_2020$year,2)
names(me_2020)[names(me_2020) == "value"] <- "share"
names(me_2020)[names(me_2020) == "Country"] <- "country"
me_2020$year <- as.numeric(me_2020$year)
me_2020$share <- as.numeric(me_2020$share)

me_2020_noNA <-me_2020[!is.na(me_2020$share)]
me_2020_noNA$share <- me_2020_noNA$share *100 # change to % representation
me_2020_positive <- me_2020_noNA[me_2020_noNA$share>0]
summary(me_2020_noNA$share)
summary(me_2020_positive$share)
var(me_2020_positive$share)

hist(me_2020_positive$share, main = "Histogram of Global Military Expenditure 2020",
     xlab = "Percentage of gross domestic product",
     ylab = "Number of countries",
     xlim = c(0,11),
     ylim = c(0,80),
     breaks = 10,
     axes = FALSE
     )

axis(side=1, at=seq(0,11,1), labels=seq(0,11,1))
axis(side=2, at=seq(0,80,10), labels=seq(0,80,10))

countries <- c("USA", "Germany", "Russia")
for (i in countries){
  cat(paste(i,":"))
  cat(me_2020_positive$share[me_2020_positive$country == i] )
  cat("\n")
} # % in GDP

characteristics <- c(min(me_2020_positive$share), max(me_2020_positive$share), median(me_2020_positive$share))
for (i in characteristics){
  
  cat(me_2020_positive$country[me_2020_positive$share == i] )
  cat("\n")
}


