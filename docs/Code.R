library(dplyr)
library(tidyr) 
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(knitr)
library(kableExtra)
library(tableone)


## Reading Data & Cleaning
library(NHANES)

data<-data.frame(NHANES$ID, NHANES$Gender,NHANES$Age, NHANES$Education,
                 NHANES$Depressed, NHANES$Race1,NHANES$Poverty, NHANES$MaritalStatus,
                 NHANES$Smoke100n,NHANES$HHIncomeMid,NHANES$BMI, NHANES$SurveyYr,
                 NHANES$PhysActive, NHANES$BPSysAve, NHANES$HealthGen, NHANES$Diabetes, NHANES$Alcohol12PlusYr )

data$sex <- ifelse(NHANES$Gender=='female', 1, 0)
data$educationLevel <- ifelse(NHANES$Education=='8th Grade', 8, 
                              ifelse(NHANES$Education=='9 - 11th Grade', 10, 
                                     ifelse(NHANES$Education=='High School', 12, 
                                            ifelse(NHANES$Education=='Some College', 14, 16))))
data$depressionLevel <- ifelse(NHANES$Depressed=='None', 0, 1)
data$married <- ifelse(NHANES$MaritalStatus=='Married ', 1,0)
data$smoker <- ifelse(NHANES$Smoke100n=='Smoker', 1, 0)
data$cycle <- ifelse(NHANES$SurveyYr=='2009_10', 1, 2)
data$active <- ifelse(NHANES$PhysActive=='Yes', 1, 0)
data$health <- ifelse(NHANES$HealthGen=='Excellent', 3, 
                      ifelse(NHANES$HealthGen=='Vgood', 3, 
                             ifelse(NHANES$HealthGen=='Good', 2, 
                                    ifelse(NHANES$HealthGen=='Fair', 1,
                                           ifelse(NHANES$HealthGen=='Poor', 1, 0 )))))
data$diabetLevel <- ifelse(NHANES$Diabetes=='Yes', 1, 0)
data$alcoholLevel <- ifelse(NHANES$Alcohol12PlusYr=='Yes', 1, 0)
data$white <- ifelse(NHANES$Race1=='White', 1, 0)
data$cat <- ifelse(NHANES$BMI<18.6, 'Under weight', 
                   ifelse(NHANES$BMI<25, 'Normal weight',
                          ifelse(NHANES$BMI<30, 'Over weight', 'Obesity')))
colnames(data)<-c('id','gender','age', 'education',
                  'depressed', 'race','poverty', 'marital',
                  'smoking','income', 'bmi', 'year',
                  'physactive', 'bp', 'health general', 'diabet', 'alcohol',
                  'sex', 'education_level', 'depression_level', 'marital_level',
                  'smoker', 'cycle', 'active', 'health', 'diabet_level', 
                  'alcohol_level', 'white', 'cat')
data <- data[complete.cases(data), ]

vars1 <- c("cycle", "active", "bmi", "education",
           "poverty", "age", "diabet_level")
cc.Table2 <- CreateTableOne(vars = vars1, strata = c("cycle","active"), data = data, test = FALSE)
q <- print(cc.Table2 , smd = FALSE)

data <- data[data$age>=20,]
data <- data[data$bmi>=18.5,]



## Scatter Plot of bmi & wealth

gg <- ggplot(data) + 
  geom_point(aes(x=poverty, y=bmi, color=physactive)) +
  labs(title="Scatterplot", x="Wealth Status", y="BMI") + 
  theme(plot.title=element_text(size=15, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12)) + 
  scale_color_discrete(name="Physically Active") +
  facet_grid( ~factor(education, levels=c('8th Grade', '9 - 11th Grade', 'High School', 'Some College', 'College Grad')))


## Jitter Plot of education & wealth

ggplot(data) + 
  geom_jitter(aes(x=poverty, y=education, color=physactive)) +
  labs(title="Jitterplot", x="Wealth Status", y="Education Level") + 
  theme(plot.title=element_text(size=15, face="bold"), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12)) + 
  scale_color_discrete(name="Physically Active") +
  scale_y_discrete(limits = c('8th Grade', '9 - 11th Grade', 'High School', 'Some College', 'College Grad')) 


## Regression

C <- data$education_level
X <- data$poverty
Y <- data$bmi
A <- data$active
diabet <- data$diabet_level
age <- data$age

treat <- A~X+C+age+diabet
alpha <- glm(treat,binomial)
A <- model.response(model.frame(treat,data))
w <- ifelse(A<1, 1/(1-fitted(alpha)), 1/fitted(alpha))
mod <- lm(-Y ~ age+diabet+X+C+A+A:X, data = data, weights=w) 


