install.packages("performance")
install.packages("see")
install.packages("visreg")
library(tidyverse)
library(performance)
ibrary(readr)
Complete <- read_csv("CaseCompetition/Data/Complete.csv")

df2 = Overweight_and_Obesity_Data %>% group_by(Country, Year) %>% summarise(mean(`Obesity (Prevalence of BMI>2SD)`))

Complete = merge(Complete, df2, by=c("Country","Year"))
names(Complete)[14] <- "Obesity_perc"

summary(Complete)
total_lm <- lm(`TOTAL_Avg_Mean BMI children` ~ cannot_afford_diet + urban_percent + 
              educ_exp + healt_exp + Population + GDP_capita + GINI + total_sugar, data = Complete)
five_to_nine <- lm(`5_9_Avg_Mean BMI children` ~ cannot_afford_diet + urban_percent + 
                 educ_exp + healt_exp + Population + GDP_capita + GINI + total_sugar, data = Complete)
ten_to_nineteen <- lm(`10_19_Avg_Mean BMI children` ~ cannot_afford_diet + urban_percent + 
                 educ_exp + healt_exp + Population + GDP_capita + GINI + total_sugar, data = Complete)
obesity <- lm(Obesity_perc ~ urban_percent + 
                educ_exp + healt_exp + Population + GDP_capita + GINI, data = till_2017)

till_2017 = Complete[Complete$Year != 2017 & Complete$Year != 2018 & Complete$Year != 2019,]

total_lm_2017 = lm(`5_9_Avg_Mean BMI children` ~ urban_percent + 
                     educ_exp + healt_exp + Population + GDP_capita, data = from_2017)

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

write_csv(Complete, "Complete_data.csv")

