install.packages("performance")
install.packages("see")
install.packages("visreg")
library(tidyverse)
library(performance)
ibrary(readr)
Complete <- read_csv("CaseCompetition/Data/Complete.csv")

summary(Complete)
total_lm <- lm(`TOTAL_Avg_Mean BMI children` ~ cannot_afford_diet + urban_percent + 
              educ_exp + healt_exp + Population + GDP_capita + GINI + total_sugar, data = Complete)
five_to_nine <- lm(`5_9_Avg_Mean BMI children` ~ cannot_afford_diet + urban_percent + 
                 educ_exp + healt_exp + Population + GDP_capita + GINI + total_sugar, data = Complete)
ten_to_nineteen <- lm(`10_19_Avg_Mean BMI children` ~ cannot_afford_diet + urban_percent + 
                 educ_exp + healt_exp + Population + GDP_capita + GINI + total_sugar, data = Complete)

from_2017 = Complete[Complete$Year == 2017 | Complete$Year == 2018 | Complete$Year == 2019,]

total_lm_2017 = lm(`5_9_Avg_Mean BMI children` ~ cannot_afford_diet + urban_percent + 
                     educ_exp + healt_exp + Population + GDP_capita + GINI, data = from_2017)

check_model(total_lm_2017)
summary(total_lm_2017)
visreg::visreg(total_lm_2017)
check_model(total_lm)
check_model(five_to_nine)
check_model(ten_to_nineteen)

summary(total_lm)
summary(five_to_nine)
summary(ten_to_nineteen)

predict(total_lm_2017, Complete)