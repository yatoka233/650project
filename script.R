######################################################################### 
#-----------------------------------------------------------------------#
# Created on:    2022/11/8                                              #
# Updated on:    2022/11/20                                             #
# EMAIL:         dengfy@umich.edu                                       #
#                Umich  Feiyang Deng.                                   #
#########################################################################

#### Data Analysis Script ####

rm(list=ls())
library('readxl')
library('ggplot2')
library('tidyverse')
library('mice')
library('VIM')


### import data
setwd("E:/Biostat Study/BIOSTAT 650/Group Project")
set.seed(123)

inputFile <- "Depression Data.xls"
raw_data <- read_excel(inputFile, sheet=1, na='NA')
raw_data <- as.data.frame(raw_data)
summary(raw_data)

## Visualize missing values
md.pattern(raw_data)
aggr(raw_data, prop=F, numbers=T)



## add indicator to identify complete rows
na_rows <- which(rowSums(is.na(raw_data)) > 0)
raw_data$complete <- 1
raw_data$complete[na_rows] <- 0
raw_data$complete <- as.factor(raw_data$complete)



### Descriptive analysis of CESDTOT4, TOTIADL4, EE46
## Boxplot
ggplot(raw_data, aes(x=complete, y=CESDTOT4, fill=complete)) +
  geom_boxplot()
ggplot(raw_data, aes(x=complete, y=TOTIADL4, fill=complete)) +
  geom_boxplot()
ggplot(raw_data, aes(x=complete, y=EE46, fill=complete)) +
  geom_boxplot()

## Density
ggplot(raw_data, aes(x=CESDTOT4, fill=complete)) +
  geom_density(alpha=.25)
ggplot(raw_data, aes(x=TOTIADL4, fill=complete)) +
  geom_density(alpha=.25)
## Histogram
ggplot() +
  geom_histogram(data=raw_data[-na_rows,], 
                 binwidth = 0.5,
                 aes(x=EE46, y=..count../(1682-length(na_rows))),
                 fill = 13, alpha = 0.7) +
  geom_histogram(data=raw_data[na_rows,], 
                 binwidth = 0.5,
                 aes(x=EE46, y=-..count../sum(1-is.na(raw_data$EE46[na_rows]))),
                 fill = 10, alpha = 0.7) +
  ylab('Incomplete data VS Complete data') +
  ggtitle('Histogram of EE46') +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "label", 
           x = 4, y = 0.4, 
           label = "EE46 of Complete data",
           fill = 12, 
           color = "white", hjust = 1) +
  annotate(geom = "label", 
           x = 4, y = -0.3, 
           label = "EE46 of Inomplete data",
           fill = 14, 
           color = "white", hjust = 1)


## test if same distribution
ks.test(raw_data$CESDTOT4[-na_rows], raw_data$CESDTOT4[na_rows], na.omit=TRUE)
ks.test(raw_data$TOTIADL4[-na_rows], raw_data$TOTIADL4[na_rows], na.omit=TRUE)
chisq.test(raw_data$EE46, raw_data$complete)



#################
#################

### Continuous data imputation
contin_data <- raw_data[,c(4,12,14,15)]
summary(contin_data)

## KNN imputation (BUG)
library("DMwR2")
outcome <- raw_data[13]
# knnImputation(contin_data) ### BUG

## Mice predictive mean matching
library("mice")
tmp <- mice(contin_data,m=5,maxit=50,meth='pmm',seed=500)
contin_data_mice <- complete(tmp,1)

## Mean and Median imputation
library("Hmisc")
contin_data_mean <- contin_data
for(i in c(1:4)){
  contin_data_mean[,i] <- impute(contin_data[,i], mean)  # 均值替代
}
contin_data_median <- contin_data
for(i in c(1:4)){
  contin_data_median[,i] <- impute(contin_data[,i], median)  # 中位数替代
}







getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
cate_idx <- c(2,3,5,6,7,8,9,10,11,16,17,18,19,20)
for( i in cate_idx){
  print(getmode(raw_data[,i]))
}






