######################################################################### 
#-----------------------------------------------------------------------#
# Created on:    2022/11/8                                              #
# Updated on:    2022/11/20                                             #
# EMAIL:         dengfy@umich.edu                                       #
#                Umich  Feiyang Deng.                                   #
#########################################################################

##### Data Analysis Script #####

rm(list=ls())
library('readxl')
library('ggplot2')
library('ggpubr')
library('tidyverse')
library('mice')
library('VIM')
library('MASS')
library('car')
library("DMwR2")
library("Hmisc")

library('utils')


#### import data ####
setwd("E:/Biostat Study/BIOSTAT 650/Group Project")
set.seed(123)

inputFile <- "Depression Data.xls"
raw_data <- read_excel(inputFile, sheet=1, na='NA')
raw_data <- as.data.frame(raw_data)

summary(raw_data)


#### Visualize missing values
md.pattern(raw_data)
aggr(raw_data, prop=F, numbers=T)


###############################################################
#### Descriptive analysis for complete and incomplete data ####
###############################################################


## add indicator to identify complete rows
na_rows <- which(rowSums(is.na(raw_data)) > 0)
raw_data$complete <- 1
raw_data$complete[na_rows] <- 0
raw_data$complete <- as.factor(raw_data$complete)


### Descriptive analysis of AGE4, TOTMMSE4, TOTIADL4, TOTADL4, EE46
## All continuous variables (except outcome) and one categorical predictor of interest
#### Boxplot  ####
ggplot(raw_data, aes(x=complete, y=AGE4, fill=complete)) +
  geom_boxplot()
ggplot(raw_data, aes(x=complete, y=TOTMMSE4, fill=complete)) +
  geom_boxplot()
ggplot(raw_data, aes(x=complete, y=TOTIADL4, fill=complete)) +
  geom_boxplot()
ggplot(raw_data, aes(x=complete, y=TOTADL4, fill=complete)) +
  geom_boxplot()
ggplot(raw_data, aes(x=complete, y=EE46, fill=complete)) +
  geom_boxplot()

#### Density ####
ggplot(raw_data, aes(x=AGE4, fill=complete)) +
  geom_density(alpha=.25)
ggplot(raw_data, aes(x=TOTMMSE4, fill=complete)) +
  geom_density(alpha=.25)
ggplot(raw_data, aes(x=TOTIADL4, fill=complete)) +
  geom_density(alpha=.25)
ggplot(raw_data, aes(x=TOTADL4, fill=complete)) +
  geom_density(alpha=.25)

#### Histogram ####
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


## test if same distribution AGE4, TOTMMSE4, TOTIADL4, TOTADL4, EE46
ks.test(raw_data$AGE4[-na_rows], raw_data$AGE4[na_rows], na.omit=TRUE)
ks.test(raw_data$TOTMMSE4[-na_rows], raw_data$TOTMMSE4[na_rows], na.omit=TRUE)
ks.test(raw_data$TOTIADL4[-na_rows], raw_data$TOTIADL4[na_rows], na.omit=TRUE)
ks.test(raw_data$TOTADL4[-na_rows], raw_data$TOTADL4[na_rows], na.omit=TRUE)
chisq.test(raw_data$EE46, raw_data$complete)

############################################



####################
#### Imputation ####
####################
# column index
# Categorical variables : GRADE, USBORN, marstat, nkids4, health4, khyper41, mdiab41, nfrac41, u43s, cc43, ee46, hha4, oo49lang, Male
# indeces : GRADE(2), USBORN(3), MARSTAT4(5), NKIDS4(6), HEALTH4(7), KHYPER41(8),
contin_idx <- c(4,12,14,15)
outcome_idx <- c(13)
cate_na_idx <- c(2,6,8,9,10,11,16,17,18)# index of categorical with missing values
cate_full_idx <- c(3,5,7,19,20)
other_idx <- c(1,21)
colnames(raw_data[,cate_full_idx])

summary(raw_data)

################## Imputation 1 #########################
impute_result <- Imputation(raw_data, contin_idx, outcome_idx, cate_na_idx, cate_full_idx)

summary(impute_result$cate_na)
summary(impute_result$cate_na_knn)
summary(impute_result$contin_data_knn)
#### combine data after imputing
new_data <- cbind(impute_result$outcome, 
                  impute_result$cate_full, 
                  impute_result$cate_na_knn, 
                  impute_result$contin_data_knn)[-impute_result$outcome_na_idx,]

new_data$GRADE <- as.numeric(as.character(new_data$GRADE))
new_data$NKIDS4 <- as.numeric(as.character(new_data$NKIDS4))
summary(new_data)
#############################################################


################## Imputation 2 #########################
# raw_data$GRADE <- as.factor(raw_data$GRADE)
raw_data$USBORN <- as.factor(raw_data$USBORN)
# raw_data$AGE4 <- as.factor(raw_data$AGE4)
raw_data$MARSTAT4 <- as.factor(raw_data$MARSTAT4)
# raw_data$NKIDS4 <- as.factor(raw_data$NKIDS4)
raw_data$HEALTH4 <- ordered(raw_data$HEALTH4) ## order
raw_data$KHYPER41 <- as.factor(raw_data$KHYPER41)
raw_data$MDIAB41 <- as.factor(raw_data$MDIAB41)
raw_data$NFRAC41 <- as.factor(raw_data$NFRAC41)
raw_data$U43S <- as.factor(raw_data$U43S)

raw_data$CC43 <- ordered(raw_data$CC43) ## order
raw_data$EE46 <- ordered(raw_data$EE46) ## order
raw_data$HHA4 <- as.factor(raw_data$HHA4)
raw_data$OO49LANG <- as.factor(raw_data$OO49LANG)
raw_data$MALE <- as.factor(raw_data$MALE)



outcome_na_idx <- which(is.na(raw_data$CESDTOT4))

colnames(raw_data[-c(1,21)])
mice_tmp <- mice(raw_data[-c(1,21)],m=5,maxit=10,seed=500) ## 4 best result so far
mice_data <- complete(mice_tmp,4)[-outcome_na_idx,]
# mice_data <- complete(mice_tmp,5)
summary(mice_data)

mice_data$HEALTH4 <- factor(mice_data$HEALTH4, ordered = FALSE) ## order
mice_data$CC43 <- factor(mice_data$CC43, ordered = FALSE) ## order
mice_data$EE46 <- factor(mice_data$EE46, ordered = FALSE) ## order

new_data <- mice_data


table(mice_data$GRADE)
table(mice_data$NKIDS4)
t <- lm(CESDTOT4~.,data=mice_data)
summary(t)
par(mfrow=c(2,2)) 
plot(t)  ## double bow  transform arcsin(sqrt(y))
par(mfrow=c(1,1))

# mice_data$CESDTOT4 <- sqrt(mice_data$CESDTOT4)
# t <- lm(CESDTOT4~.,data=mice_data)
# summary(t)
# par(mfrow=c(2,2)) 
# plot(t)  ## double bow  transform arcsin(sqrt(y))
# par(mfrow=c(1,1))
######################################################




########################################


############################################
#### Descriptive analysis for new data ####
###########################################
# select(CESDTOT4,USBORN,MARSTAT4,HEALTH4,OO49LANG,MALE,GRADE,
#        NKIDS4,KHYPER41,MDIAB41,NFRAC41,U43S,CC43,EE46,HHA4,AGE4,
#        TOTMMSE4,TOTIADL4,TOTADL4) %>%

colnames(new_data)
library("gtsummary")
new_data %>%
  tbl_summary(missing = "no",
              type = list(CESDTOT4 ~ 'continuous',
                          USBORN ~ 'categorical',
                          MARSTAT4 ~ 'categorical',
                          HEALTH4 ~ 'categorical',
                          OO49LANG ~ 'categorical',
                          MALE ~ 'categorical',
                          GRADE ~ 'continuous',
                          NKIDS4 ~ 'continuous',
                          KHYPER41 ~ 'categorical',
                          MDIAB41 ~ 'categorical',
                          NFRAC41 ~ 'categorical',
                          U43S ~ 'categorical',
                          CC43 ~ 'categorical',
                          EE46 ~ 'categorical',
                          HHA4 ~ 'categorical',
                          AGE4 ~ 'continuous',
                          TOTMMSE4 ~ 'continuous',
                          TOTIADL4 ~ 'continuous',
                          TOTADL4 ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd}) min:{min} max:{max} IQR:[{p25}, {p75}]",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list(all_continuous() ~ c(2,2),
                            all_categorical() ~ c(0,2))
  )


#### boxplot ####
p1 <- ggplot(new_data, aes(y=CESDTOT4)) +
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
p2 <- ggplot(new_data, aes(y=TOTIADL4)) +
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
p3 <- ggplot(new_data, aes(y=TOTADL4)) +
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
p4 <- ggplot(new_data, aes(y=AGE4)) +
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
p5 <- ggplot(new_data, aes(y=TOTMMSE4)) +
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
p6 <- ggplot(new_data, aes(y=GRADE)) +
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")

ggarrange(p1,p2,p3,p4,p5,p6,
          ncol = 3, nrow = 2)


##### density of outcome and continuous variables ####
ggplot(new_data, aes(x=CESDTOT4)) +
  geom_density(alpha=.5, fill="lightblue", color="blue")

ggplot(new_data, aes(x=TOTIADL4)) +
  geom_density(alpha=.5, fill="lightblue", color="blue")
ggplot(new_data, aes(x=TOTADL4)) +
  geom_density(alpha=.5, fill="lightblue", color="blue")
ggplot(new_data, aes(x=AGE4)) +
  geom_density(alpha=.5, fill="lightblue", color="blue")
ggplot(new_data, aes(x=TOTMMSE4)) +
  geom_density(alpha=.5, fill="lightblue", color="blue")


##### histogram of ordical variables ####
ggplot(new_data, aes(x=as.factor(GRADE))) +
  geom_histogram(color="lightblue",stat="count", fill="#69b3a2", alpha=0.7)
ggplot(new_data, aes(x=as.factor(HEALTH4))) +
  geom_histogram(color="lightblue",stat="count", fill="#69b3a2", alpha=0.7)
ggplot(new_data, aes(x=as.factor(NKIDS4))) +
  geom_histogram(color="lightblue",stat="count", fill="#69b3a2", alpha=0.7)
ggplot(new_data, aes(x=as.factor(EE46))) +
  geom_histogram(color="lightblue",stat="count", fill="#69b3a2", alpha=0.7)


##### piechart of categorical variables ####
p <- ggplot(data=new_data,mapping = aes(x='Content',y=as.factor(EE46),fill=as.factor(EE46)))+ 
  geom_bar(stat = 'identity', position = 'stack', width = 1)
p + coord_polar(theta = 'y') + labs(x = 'Category', y = 'number', title = 'pie') + 
  theme(axis.text = element_blank())

p <- ggplot(data=new_data,mapping = aes(x='Content',y=as.factor(MARSTAT4),fill=as.factor(MARSTAT4)))+ 
  geom_bar(stat = 'identity', position = 'stack', width = 1)
p + coord_polar(theta = 'y') + labs(x = 'Category', y = 'number', title = 'pie') + 
  theme(axis.text = element_blank())


# psych::pairs.panels(new_data)

##### outcome vs Xi ####
ggplot(new_data, aes(x = TOTIADL4, y = CESDTOT4)) + 
  geom_point(color = "blue")+
  geom_smooth(method = lm, color = "red", fill="#69b3a2", se = TRUE)
ggplot(new_data, aes(x = TOTADL4, y = CESDTOT4)) + 
  geom_point(color = "blue")+
  geom_smooth(method = lm, color = "red", fill="#69b3a2", se = TRUE)
ggplot(new_data, aes(x = AGE4, y = CESDTOT4)) + 
  geom_point(color = "blue")+
  geom_smooth(method = lm, color = "red", fill="#69b3a2", se = TRUE)
ggplot(new_data, aes(x = TOTMMSE4, y = CESDTOT4)) + 
  geom_point(color = "blue")+
  geom_smooth(method = lm, color = "red", fill="#69b3a2", se = TRUE)


##### outcome vs categorical ####
ggplot(new_data, aes(x = EE46,y=CESDTOT4)) +
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
ggplot(new_data, aes(x = as.factor(GRADE), y = CESDTOT4)) + 
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
ggplot(new_data, aes(x = as.factor(NKIDS4), y = CESDTOT4)) + 
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
ggplot(new_data, aes(x = HEALTH4, y = CESDTOT4)) + 
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
ggplot(new_data, aes(x = as.factor(AGE4), y = CESDTOT4)) + 
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
ggplot(new_data, aes(x = MARSTAT4, y = CESDTOT4)) + 
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")

ggplot(new_data, aes(x = as.factor(TOTADL4), y = CESDTOT4)) + 
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
ggplot(new_data, aes(x = as.factor(TOTIADL4), y = CESDTOT4)) + 
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")
ggplot(new_data, aes(x = as.factor(TOTMMSE4), y = CESDTOT4)) + 
  geom_boxplot(fill="lightgreen", alpha=0.7, color="DarkSlateGray")


#### correlation of covariates ####
## just for reference
library("corrplot")
summary(new_data)
contin_idx_cor <- c(1,3,5,11,13,14)
contin_data <- new_data[,contin_idx_cor]
cate_data <- new_data[,-contin_idx_cor]
for(i in 1:length(cate_data)){
  cate_data[,i] <- as.numeric(as.character(cate_data[,i]))
}
summary(cate_data)

cormat = cor(contin_data, method = "pearson")
pres <- cor.mtest(contin_data, conf.level = .95)
corrplot.mixed(cormat, lower.col = "black", number.cex = 1,p.mat = pres$p, sig.level = .05)

cormat = cor(cate_data, method = "spearman")
pres <- cor.mtest(cate_data, conf.level = .95)
corrplot.mixed(cormat, lower.col = "black", number.cex = 1,p.mat = pres$p, sig.level = .05)

######################################


########################################################
#### Bivariate analysis (outcome vs. Xi regression) ####
########################################################
# Questions: 1. how to deal with categorical variables with categories > 5 ??  eg. GRADE KIDS
#            2. do we need to make group for continuous variables ??  eg. AGE IDAL
#            3. in data there are to major groups (physical health variables and mental health variables)
#               is there any relationship inside or between physical health variables and mental health variables ??
head(new_data)
#2 grade not significant
M2=lm(CESDTOT4~GRADE,data=new_data)
summary(M2)
# not significant
BM2=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+GRADE,data=new_data)
summary(BM2)

## after transform grade could be significant
table(new_data$GRADE)
new_data$GRADE_Cat <- 0
new_data$GRADE_Cat[new_data$GRADE>=7 & new_data$GRADE<=10] <- 1
table(new_data$GRADE_Cat)

M2=lm(CESDTOT4~GRADE_Cat,data=new_data)
anova(M2)
BM2=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+as.factor(GRADE_Cat),data=new_data)
anova(BM2)


#3 USBORN  significant
M3=lm(CESDTOT4~USBORN,data=new_data)
summary(M3)
# significant
BM3=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+USBORN,data=new_data)
summary(BM3)

#4 AGE4  significant
M4=lm(CESDTOT4~AGE4,data=new_data)
summary(M4)
#not significant
BM4=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+AGE4,data=new_data)
summary(BM4)

table(new_data$AGE4)
new_data$AGE4_Cat <- 0
new_data$AGE4_Cat[new_data$AGE4>=79 & new_data$AGE4<=84] <- 1
new_data$AGE4_Cat[new_data$AGE4>=85 & new_data$AGE4<=88] <- 2
new_data$AGE4_Cat[new_data$AGE4>=89] <- 3

new_data$AGE4_Cat <- as.factor(new_data$AGE4_Cat)

# new_data$AGE4_Cat <- split_IQR(new_data$AGE4)
table(new_data$AGE4_Cat)

M4=lm(CESDTOT4~AGE4_Cat,data=new_data)
summary(M4)
BM4=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+AGE4_Cat,data=new_data)
summary(BM4)

#5 MARSTAT4 significant
new_data$MARSTAT4 <- as.factor(new_data$MARSTAT4)
M5=lm(CESDTOT4~MARSTAT4,data=new_data)
summary(M5)
# significant
BM5=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+MARSTAT4,data=new_data)
anova(BM5)

table(new_data$MARSTAT4)
new_data$MARSTAT4_Cat <- 0
new_data$MARSTAT4_Cat[new_data$MARSTAT4==3] <- 1
table(new_data$MARSTAT4_Cat)
new_data$MARSTAT4_Cat <- as.factor(new_data$MARSTAT4_Cat)

M5=lm(CESDTOT4~MARSTAT4_Cat,data=new_data)
summary(M5)

BM5=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+MARSTAT4_Cat,data=new_data)
anova(BM5)

#6 NKIDS4 not significant
M6=lm(CESDTOT4~as.factor(NKIDS4),data=new_data)
summary(M6)
# not significant
BM6=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+NKIDS4,data=new_data)
summary(BM6)

new_data$NKIDS4_Cat <- 0
new_data$NKIDS4_Cat[new_data$NKIDS4>=14] <- 1
table(new_data$NKIDS4_Cat)

M6=lm(CESDTOT4~as.factor(NKIDS4_Cat),data=new_data)
summary(M6)
BM6=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+as.factor(NKIDS4_Cat),data=new_data)
summary(BM6)

#7 HEALTH4 significant
M7=lm(CESDTOT4~factor(HEALTH4),data=new_data)
summary(M7)
# significant
BM7=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+factor(HEALTH4),data=new_data)
anova(BM7)

table(new_data$HEALTH4)
new_data$HEALTH4[new_data$HEALTH4==2] <- 1
table(new_data$HEALTH4)
M7=lm(CESDTOT4~factor(HEALTH4),data=new_data)
summary(M7)
BM7=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+factor(HEALTH4),data=new_data)
anova(BM7)

#8 KHYPER41 significant
M8=lm(CESDTOT4~KHYPER41,data=new_data)
summary(M8)
# significant
BM8=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+KHYPER41,data=new_data)
summary(BM8)

#9 MDIAB41 significant
M9=lm(CESDTOT4~MDIAB41,data=new_data)
summary(M9)
# not significant
BM9=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+MDIAB41,data=new_data)
summary(BM9)

#10 NFRAC41 not significant
M10=lm(CESDTOT4~NFRAC41,data=new_data)
summary(M10)

# not significant
BM10=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+NFRAC41,data=new_data)
summary(BM10)

#11 U43S significant
M11=lm(CESDTOT4~U43S,data=new_data)
summary(M11)
# significant
BM11=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+U43S,data=new_data)
summary(BM11)


#12 TOTMMSE4 significant
M12=lm(CESDTOT4~TOTMMSE4,data=new_data)
summary(M12)
# significant
BM12=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+TOTMMSE4,data=new_data)
summary(BM12)

## transform is not significant after stepwise
table(new_data$TOTMMSE4)
new_data$TOTMMSE4_Cat <- 0
new_data$TOTMMSE4_Cat[new_data$TOTMMSE4>=17 & new_data$TOTMMSE4<=24] <- 1
new_data$TOTMMSE4_Cat[new_data$TOTMMSE4>=25] <- 2
new_data$TOTMMSE4_Cat <- as.factor(new_data$TOTMMSE4_Cat)
table(new_data$TOTMMSE4_Cat)

M12=lm(CESDTOT4~TOTMMSE4_Cat,data=new_data)
summary(M12)
BM12=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+TOTMMSE4_Cat,data=new_data)
summary(BM12)


#14 TOTIADL4 significant
M14=lm(CESDTOT4~TOTIADL4,data=new_data)
summary(M14)

table(new_data$TOTIADL4)
new_data$TOTIADL4_Cat <- 0
new_data$TOTIADL4_Cat[new_data$TOTIADL4>=5 & new_data$TOTIADL4<=8] <- 1
new_data$TOTIADL4_Cat[new_data$TOTIADL4>=9] <- 2
new_data$TOTIADL4_Cat <- as.factor(new_data$TOTIADL4_Cat)
table(new_data$TOTIADL4_Cat)
M15=lm(CESDTOT4~TOTIADL4_Cat,data=new_data)
summary(M15)


#15 totadl4 significant
M15=lm(CESDTOT4~TOTADL4,data=new_data)
summary(M15)
# significant
BM15=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+TOTADL4,data=new_data)
summary(BM15)

table(new_data$TOTADL4)
new_data$TOTADL4_Cat <- 0
new_data$TOTADL4_Cat[new_data$TOTADL4!=0] <- 1
new_data$TOTADL4_Cat <- as.factor(new_data$TOTADL4_Cat)

M15=lm(CESDTOT4~TOTADL4,data=new_data)
summary(M15)
BM15=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+TOTADL4_Cat,data=new_data)
summary(BM15)

#16 CC43 significant
M16=lm(CESDTOT4~CC43,data=new_data)
summary(M16)
# significant
BM16=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+CC43,data=new_data)
summary(BM16)

#17 EE46 significant
M17=lm(CESDTOT4~EE46,data=new_data)
summary(M17)

table(new_data$EE46)
new_data$EE46[new_data$EE46==4] <- 1
M17=lm(CESDTOT4~EE46,data=new_data)
summary(M17)



#18 hha4 -significant
M18=lm(CESDTOT4~HHA4,data=new_data)
summary(M18)
#not significant 
BM18=lm(CESDTOT4~TOTIADL4+as.factor(EE46)+HHA4,data=new_data)
summary(BM18)

#19 oo49lang -significant
M19=lm(CESDTOT4~OO49LANG,data=new_data)
summary(M19)
# significant
BM19<-lm(CESDTOT4~TOTIADL4+as.factor(EE46)+OO49LANG,data=new_data)
summary(BM19)

#20 Male -significant
M20=lm(CESDTOT4~MALE,data=new_data)
summary(M20)
# siginificant
BM20<-lm(CESDTOT4~TOTIADL4+as.factor(EE46)+MALE,data=new_data)
summary(BM20)



###############################





##########################
##### Building Models ####
##########################
colnames(new_data)
select_data <- new_data[,-c(1,4,5,23,24,25)]
# select_data <- new_data
colnames(select_data)

select_data$GRADE_Cat <- as.factor(select_data$GRADE_Cat)
select_data$AGE4_Cat <- as.factor(select_data$AGE4_Cat)
summary(select_data)


#### Full model ####
full.model <- lm(CESDTOT4~., data = select_data)
summary(full.model)

### plots
par(mfrow=c(2,2)) 
plot(full.model)  ## sqrt(y)
par(mfrow=c(1,1))
### partial regression plot
avPlots(full.model)
### Residual plots
residualPlots(full.model,type="response")


##### Transform ####
select_data$CESDTOT4 <- sqrt(select_data$CESDTOT4)
full.model.t <- lm(CESDTOT4~., data = select_data)
summary(full.model.t)

### plots
par(mfrow=c(2,2)) 
plot(full.model.t)
par(mfrow=c(1,1))
### partial regression plot
avPlots(full.model.t)
### Residual plots
residualPlots(full.model.t,type="response")


### full model doesn't have multicollinearity
vif_values <- vif(full.model.t)[,1]
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "Orchid")
abline(v = 5, lwd = 3, lty = 2)


#### Outliers ####
full_yhat = full.model.t$fitted.values
full_res = full.model.t$residuals #m1.yhat-fev
full_h = hatvalues(full.model.t) #leverage
full_r = rstandard(full.model.t) #internally studentized residuals
full_rr = rstudent(full.model.t) #externally studentized residuals

n = nrow(select_data)
p = full.model.t$rank # dimensions
full_sigma = sqrt( sum(full_res^2)/(n-p) )
full_z = full_res/full_sigma #standardized residual

outlier <- rep(0, length(full_z))
outlier[which(abs(full_rr) >= 3)] <- 1
sum(outlier)
outlier <- as.factor(outlier)

qplot(x=full_yhat, y=full_rr, col=outlier)
qplot(x=new_data$TOTIADL4, y=new_data$CESDTOT4, col=outlier)

tmp <- new_data[which(abs(full_rr) >= 3),]

full.model.t2 <- lm(CESDTOT4~., data = select_data[-which(abs(full_rr) >= 3),])
summary(full.model.t2)

### plots
par(mfrow=c(2,2)) 
plot(full.model.t2)
par(mfrow=c(1,1))


#### difference after taking out outliers
(coefficients(full.model.t) - coefficients(full.model.t2))/coefficients(full.model.t)*100

####################################################





#############################
#### Stepwise regression ####
#############################
##### step
lr_step <- step(full.model.t, direction="both")
summary(lr_step)

lr_step1 <- step(full.model.t2, direction="both")
summary(lr_step1)

lr_step$anova
par(mfrow=c(2,2)) 
plot(lr_step)
par(mfrow=c(1,1))

##### stepAIC
step.model <- stepAIC(full.model.t, direction = "both", trace = FALSE)
summary(step.model)
step.model$anova

##### stepwise
result <- stepwise(full.model.t, lm(CESDTOT4~1, data = select_data), 
                  alpha.to.enter = 0.05, alpha.to.leave = 0.1)
summary(result)

##### back ward after stepwise (same result)
lr_back <- step(lr_step, direction="backward")
summary(lr_back)



###################
#### Diagnosis ####
###################
main.model <- lr_back

### multicollinearity
vif_values <- vif(main.model)[,1]
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "Orchid")
abline(v = 10, lwd = 3, lty = 2)
ggplot(select_data, aes(x = TOTIADL4, y = CESDTOT4)) + 
  geom_point(color = "blue") +
  geom_smooth(method = lm, color = "red", fill="#69b3a2", se = TRUE)



main_yhat = main.model$fitted.values
main_res = main.model$residuals #m1.yhat-fev
main_h = hatvalues(main.model) #leverage
main_r = rstandard(main.model) #internally studentized residuals
main_rr = rstudent(main.model) #externally studentized residuals

n = nrow(select_data)
p = main.model$rank # dimensions
main_sigma = sqrt( sum(main_res^2)/(n-p) )
main_z = main_res/main_sigma #standardized residual

## no outlier
outlier <- rep(0, length(main_z))
outlier[which(abs(main_rr) >= 3)] <- 1
sum(outlier)
outlier <- as.factor(outlier)

qplot(x=main_yhat, y=main_rr, col=outlier)

tmp <- new_data[which(abs(main_rr) >= 3),]

main.model2 <- lm(CESDTOT4 ~ USBORN + HEALTH4 + KHYPER41 + TOTMMSE4 + 
                    TOTIADL4 + CC43 + EE46 + OO49LANG + MALE + GRADE_Cat + MARSTAT4_Cat + 
                    TOTADL4_Cat, data = select_data[-which(abs(full_rr) >= 3),])
summary(main.model)
summary(main.model2)
#### difference after taking out outliers
(coefficients(main.model2) - coefficients(main.model))/coefficients(main.model2)*100

### partial regression plot
avPlots(main.model)
### Residual plots
residualPlots(main.model,type="response")
table(select_data$TOTADL4_Cat)

### normal
qqPlot(main_z)
hist(main_z)

shapiro.test(main_z) ## reject but only for small sample size (<50)
ks.test(main_z, "pnorm")
library('nortest')
lillie.test(main_z)


### empirical cdf of main_z and N(0,1)
sample_plot <- data.frame(z=main_z, rn=rnorm(length(main_z)))
ggplot(sample_plot) +
  stat_ecdf(aes(main_z),color="red", size=1, alpha=0.7) +
  stat_ecdf(aes(rn),color="blue", size=1, alpha=0.7) +
  ylab('') + xlab('') +
  ggtitle('Empirical cdf of standardized residual and N(0,1)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate(geom = "label", 
           x = 4, y = 0.6, 
           label = "cdf of standardized residual",
           fill = "red", 
           color = "white", hjust = 1) +
  annotate(geom = "label", 
           x = 3, y = 0.4, 
           label = "cdf of N(0, 1)",
           fill = "blue", 
           color = "white", hjust = 1)

### independence
main_res1 <- main_res[1:length(main_res)-1]
main_res2 <- main_res[2:length(main_res)]
qplot(x=main_res1, y=main_res2)




#####################
#### Interaction ####
#####################
summary(main.model)
write.csv(select_data, "E:/Biostat Study/BIOSTAT 650/Group Project/code/select_data.csv")
summary(select_data)

inter.model <- lm(formula = CESDTOT4 ~ USBORN + HEALTH4 + KHYPER41 + TOTMMSE4 + 
                    TOTIADL4 + CC43 + EE46 + OO49LANG + MALE + GRADE_Cat + MARSTAT4_Cat + 
                    TOTADL4_Cat + TOTMMSE4*TOTIADL4, data = select_data)
summary(inter.model)












