
# ---- Load the Libraries ----
rm(list = ls())
library(survey)
library(survey)
library(ggeffects)
library(ggplot2)
library(sandwich)
library(patchwork)
library(stargazer)
library(contrast)
library(car)
library(dplyr)
# ---- Load dataset ----
setwd("/Users/yingyuejiang/Desktop/Brown\ University/master\ thesis/code_1214/")
NSCG_0321_10<-read.csv("NSCG_0321_10_1214.csv")
temporary<-read.csv("temporary.csv")
male_per<-NSCG_0321_10%>%filter(gender=="M")
#male, permanent resident 
male_per<-male_per%>%mutate(
  current_job_new_new=case_when(
    current_job_new%in%c("282710")~"282730",
    current_job_new%in%c("321950")~"341980",
    current_job_new%in%c("570950")~"570930",
    current_job_new%in%c("732510")~"732520",
    current_job_new%in%c("772330")~"770100",
    TRUE~as.character(current_job_new)
  )
)%>%
  mutate(fit=case_when(stem_major==1&work_stem==1~1,
                       stem_major==0&work_stem==0~1,
                       stem_major==1&work_stem==0~0,
                       stem_major==0&work_stem==1~0))
#male, those who are employed
male_work<-male_per%>%filter(unemploy==0)%>%
  mutate(size = if_else(employ_size %in% c(7, 8), "7", employ_size))

# ---- Male ----
## ----Descriptive statistics----
# Define survey design
design <- svydesign(id = ~1, data = male_per, weights = ~weight)
design_work <- svydesign(id = ~1, data = male_work,weights = ~weight)
# Weighted unemployment 
svytable(~degree_place + unemploy, design)%>%prop.table(margin = 1)
# Calculate the average of satis_sec for each degree_place
svyby(~satis_sec, ~degree_place, design_work, svymean, na.rm = TRUE)
# Weighted race
svytable(~degree_place + race_new, design)%>%prop.table(margin = 1)
# Weighted region
svytable(~degree_place + current_loc_new, design)%>%prop.table(margin = 1)*100
# Calculate the average of age for each degree_place
svyby(~age, ~degree_place, design, svymean, na.rm = TRUE)
# Weighted type of degree
svytable(~degree_place + highest_degree, design)%>%prop.table(margin = 1)*100
# Weighted year
svytable(~degree_place + year, design)%>%prop.table(margin = 1)*100
# Weighted stem work
svytable(~degree_place +work_stem , design)%>%prop.table(margin = 1)*100
# Weighted mother's education
svytable(~degree_place +mother_edu_new , design)%>%prop.table(margin = 1)*100
# Weighted father's education
svytable(~degree_place +father_edu_new , design)%>%prop.table(margin = 1)*100
# Weighted marital status
svytable(~degree_place +married_new , design)%>%prop.table(margin = 1)*100
# Weighted number of children
svytable(~degree_place +child_total , design)%>%prop.table(margin = 1)*100
# Weighted sector
svytable(~degree_place +sector , design_work,na.rm = TRUE)%>%prop.table(margin = 1)*100
# Weighted full time 
svytable(~degree_place +full_time , design_work,na.rm = TRUE)%>%prop.table(margin = 1)*100
# Weighted education occupation fit
svytable(~ degree_place+fit ,design_work ,na.rm = TRUE)%>%prop.table(margin = 1)*100
# Weighted firm size
svytable(~ degree_place+size ,design_work ,na.rm = TRUE)%>%prop.table(margin = 1)*100
## ----Unemployment----
### ---- Logit model----

male_per$degree_place <- factor(male_per$degree_place)
male_per$degree_place <- relevel(male_per$degree_place, ref = "0")

design <- svydesign(id = ~1, data = male_per, weights = ~weight)
design_work <- svydesign(id = ~1, data = male_work,weights = ~weight)
#logit model, male
glm_male1<- svyglm(unemploy ~as.factor(degree_place)+
                     as.factor(work_stem)+
                     as.factor(race_new)+
                     age + 
                     age2 + 
                     as.factor(married_new) +
                     as.factor(child_total) + 
                     as.factor(mother_edu_new) + 
                     as.factor(father_edu_new) +
                     as.factor(highest_degree) +
                     as.factor(year) + 
                     as.factor(current_loc_new) ,
                   design = design,  family = quasibinomial())
#logit model, male, have interaction 
glm_male2<- svyglm(unemploy ~ as.factor(degree_place)*as.factor(work_stem)+
                     as.factor(race_new)+
                     age + 
                     age2 + 
                     as.factor(married_new) +
                     as.factor(child_total) + 
                     as.factor(mother_edu_new) + 
                     as.factor(father_edu_new) +
                     as.factor(highest_degree) +
                     as.factor(year)+
                     as.factor(current_loc_new) ,
                   design = design,  family = quasibinomial())


library(stargazer)
stargazer(glm_male1,glm_male2,type="text",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))
### ---- Adjusted R2----

# Fit the null model
null_model <- svyglm(unemploy ~ 1, design = design, family = quasibinomial())

# Extract null and residual deviance
null_deviance <- null_model$deviance
residual_deviance <- glm_male1$deviance

# Extract total number of observations (N) and number of predictors (k)
N <- nrow(design$variables)  # Total observations in survey design
k <- length(coef(glm_male1)) - 1  # Number of predictors (excluding intercept)

# Calculate adjusted pseudo R-squared
pseudo_r2_adj <- 1 - (residual_deviance / null_deviance) * ((N - 1) / (N - k - 1))
pseudo_r2_adj


# Fit the null model
null_model <- svyglm(unemploy ~ 1, design = design, family = quasibinomial())

# Extract null and residual deviance
null_deviance <- null_model$deviance
residual_deviance <- glm_male2$deviance

# Extract total number of observations (N) and number of predictors (k)
N <- nrow(design$variables)  # Total observations in survey design
k <- length(coef(glm_male2)) - 1  # Number of predictors (excluding intercept)

# Calculate adjusted pseudo R-squared
pseudo_r2_adj <- 1 - (residual_deviance / null_deviance) * ((N - 1) / (N - k - 1))
pseudo_r2_adj

### ----Fixed effect model----

library(fixest)
#two categories
fixed1 <- feglm(
  unemploy ~ as.factor(degree_place) +
    as.factor(race_new) +
    age + 
    age2 + 
    as.factor(married_new) +
    as.factor(child_total) + 
    as.factor(mother_edu_new) + 
    as.factor(father_edu_new) +
    as.factor(highest_degree) +
    as.factor(year) +
    as.factor(current_loc_new) |
    as.factor(work_stem),   # Fixed effects here
  data =male_per,
  family = binomial("logit"),
  weights = ~weight   # Add weights here
)

#Five categories

fixed2<- feglm(
  unemploy ~ as.factor(degree_place) +
    as.factor(race_new) +
    age + 
    age2 + 
    as.factor(married_new) +
    as.factor(child_total) + 
    as.factor(mother_edu_new) + 
    as.factor(father_edu_new) +
    as.factor(highest_degree) +
    as.factor(year) +
    as.factor(current_loc_new) |
    as.factor(current_job_broad),   # Fixed effects here
  data = male_per,
  family = binomial("logit"),
  weights = ~weight   # Add weights here
)

#126 detailed occupations

fixed3 <- feglm(
  unemploy ~ as.factor(degree_place) +
    as.factor(race_new) +
    age + 
    age2 + 
    as.factor(married_new) +
    as.factor(child_total) + 
    as.factor(mother_edu_new) + 
    as.factor(father_edu_new) +
    as.factor(highest_degree) +
    as.factor(year) +
    as.factor(current_loc_new) |
    as.factor(current_job_new_new),   # Fixed effects here
  data = male_per,
  family = binomial("logit"),
  weights = ~weight   # Add weights here
)


library(modelsummary)
summary(fixed1, 
        type = "text", 
        stars = TRUE, 
        star.cutoffs = c(0.05, 0.01, 0.001))
modelsummary(fixed1, 
             type = "text", 
             stars = TRUE, 
             star.cutoffs = c(0.05, 0.01, 0.001))

summary(fixed2, 
        type = "text", 
        stars = TRUE, 
        star.cutoffs = c(0.05, 0.01, 0.001))
modelsummary(fixed2, 
             type = "text", 
             stars = TRUE, 
             star.cutoffs = c(0.05, 0.01, 0.001))

summary(fixed3, 
        type = "text", 
        stars = TRUE, 
        star.cutoffs = c(0.05, 0.01, 0.001))
modelsummary(fixed3, 
             type = "text", 
             stars = TRUE, 
             star.cutoffs = c(0.05, 0.01, 0.001))

### ----subgroup----
# Subset the data for degree_place == 0
design <- svydesign(id = ~1, data = male_per, weights = ~weight)
design_place0 <- subset(design, degree_place == 0)
glm_place0 <- svyglm(unemploy ~ as.factor(work_stem)+as.factor(race_new)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year)+
                       
                       as.factor(current_loc_new), design = design_place0, family = quasibinomial())

# Subset the data for degree_place == 1
design_place1 <- subset(design, degree_place == 1)
glm_place1 <- svyglm(unemploy ~ as.factor(work_stem)+as.factor(race_new)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year)+
                       
                       as.factor(current_loc_new), design = design_place1, family = quasibinomial())
# Subset the data for degree_place == 2
design_place2 <- subset(design, degree_place == 2)
glm_place2 <- svyglm(unemploy ~ as.factor(work_stem)+as.factor(race_new)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year)+
                       
                       as.factor(current_loc_new), design = design_place2, family = quasibinomial())

stargazer(glm_place0,glm_place1,glm_place2,type="text",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))
### ----plot----
hypothetical_data <- expand.grid(
  work_stem = c(0,1),
  degree_place = c(0,1,2),
  age = 43,
  age2 = 43^2,
  married_new = 1,
  child_total = 0,
  mother_edu_new = 0,
  father_edu_new = 0,
  highest_degree = 1,
  year = 2013,
  current_loc_new = 4,
  race_new = 0
)
# Predict probabilities of unemployment based on the model
predicted_probs <- predict(glm_male1, newdata = hypothetical_data, type = "response", se.fit = TRUE)


response <- c(0.018609, 0.015770, 0.022723, 0.019268, 0.027042, 0.022946 )
SE <- c(0.0024, 0.0020, 0.0037, 0.0031, 0.0044, 0.0037)

# Create a data frame
predicted_df <- data.frame(
  Response = response,
  SE = SE
)
# Calculate the 95% confidence interval
z_value <- 1.96  # Z-score for 95% confidence
predicted_df$Lower_CI <- predicted_df$Response - (z_value * predicted_df$SE)
predicted_df$Upper_CI <- predicted_df$Response + (z_value * predicted_df$SE)
# View the final data frame
print(predicted_df)


predicted_df$work_stem <- NA  # or you can initialize with 0s

# Assign 0 to rows 1, 3, 5
predicted_df$work_stem[c(1, 3, 5)] <- 0

# Assign 1 to rows 2, 4, 6
predicted_df$work_stem[c(2, 4, 6)] <- 1

# Initialize work_stem with NA or any default value
predicted_df$degree_place<- NA  # or you can initialize with 0s


predicted_df$degree_place[c(1,2)] <- 0

# Assign 1 to rows 2, 4, 6
predicted_df$degree_place[c(3,4)] <- 1
predicted_df$degree_place[c(5,6)] <- 2

# View the updated data frame
print(predicted_df)



# Plot the predicted probabilities for different degree_place values
ggplot(predicted_df, aes(x = as.factor(degree_place), y = Response, group = as.factor(work_stem))) +
  geom_point(aes(shape = as.factor(work_stem)), size = 3, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = Lower_CI,ymax = Upper_CI), width = 0.2, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(16, 17),  # Different point shapes for STEM and Non-STEM
                     labels = c("Non-STEM occupation", "STEM occupation")) +
  labs(title = "Predicted probabilities of unemployment by occupation for male natives and immigrants",
       x = "",
       y = "Predicted probability of unemployment",
       shape = "Occupation type") +
  scale_x_discrete(labels = c("Native-born", "Immigrant, US degree", "Immigrant, foreign degree")) +  # Custom labels for degree place
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA),   # Set plot background to white
    legend.background = element_rect(fill = "white", color = NA)  # Set legend background to white
  )
## ----Satisfaction----
male_work<-male_work%>%mutate(
  log_income=if_else(income>0,log(income),0)
) %>%
  mutate(size = if_else(employ_size %in% c(7, 8), "7", employ_size),
         logincome=if_else(income>0,log(income),0),
         log_100=income/1000,
         income_new=case_when(year==2010~0.85*income,
                              year==2013~0.79*income,
                              year==2015~0.78*income,
                              year==2017~0.75*income,
                              year==2019~0.72*income,
                              year==2021~0.67*income,
                              TRUE~1*income),
         logincome_new=if_else(income_new>0,log(income_new),0),
         age2_new=age2/1000)

### ----linear regression----
male_work$degree_place <- factor(male_work$degree_place)
male_work$degree_place <- relevel(male_work$degree_place, ref = "0")
design_work <- svydesign(id = ~1, data = male_work, weights = ~weight)
lm_male1<- svyglm( satis_sec ~ as.factor(degree_place)+
                       as.factor(work_stem)+
                       as.factor(race_new)+
                       fit+
                       full_time+
                       as.factor(size)+
                       as.factor(sector)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year) + 
                       as.factor(current_loc_new) ,
                     family=gaussian(),
                     design = design_work)
lm_male2<- svyglm(satis_sec ~ as.factor(degree_place)*as.factor(work_stem)+
                      as.factor(race_new)+
                      fit+
                      full_time+
                      as.factor(sector)+
                      age + 
                      age2 + 
                      as.factor(married_new) +
                      as.factor(child_total) + 
                      as.factor(mother_edu_new) + 
                      as.factor(father_edu_new) +
                      as.factor(highest_degree) +
                      as.factor(year) + 
                      as.factor(size)+
                      as.factor(current_loc_new) ,
                    family=gaussian(),
                    design = design_work)
stargazer(lm_male1,lm_male2,type="text",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

### ----tempory worker----
male_tem_work<-temporary%>%filter(unemploy==0,
                                  citizen%in%c(0,3)|tem%in%c(1,2,3,4),
                                  gender=="M")%>%mutate(
                                    work_visa=case_when(degree_place == 0~0,#US born
                                                        degree_place==1&citizen %in% c(0, 3)~1,#foreign born, US degree, non-temporary
                                                        degree_place==2&citizen %in% c(0, 3)~2,#foreign born, foreign degree, temporary
                                                        degree_place%in%c(1)&citizen %in% c(4)~3,#foreign born, US degree, temporary
                                                        degree_place%in%c(2)&citizen %in% c(4)~3),
                                    fit=case_when(stem_major==1&work_stem==1~1,
                                                  stem_major==0&work_stem==0~1,
                                                  stem_major==1&work_stem==0~0,
                                                  stem_major==0&work_stem==1~0)
                                    
                                  )%>%
  mutate(size = if_else(employ_size %in% c(7, 8), "7", employ_size))
male_tem_work$work_visa <- factor(male_tem_work$work_visa)
male_tem_work$work_visa <- relevel(male_tem_work$work_visa, ref = "0")
design_tem_work <- svydesign(id = ~1, data = male_tem_work,weights = ~weight)
tem_satis1<- svyglm(satis_sec ~ as.factor(work_visa)+
                      as.factor(work_stem)+
                      as.factor(race_new)+
                      fit+
                      full_time+
                      as.factor(sector)+
                      age + 
                      age2 + 
                      as.factor(married_new) +
                      as.factor(child_total) + 
                      as.factor(mother_edu_new) + 
                      as.factor(father_edu_new) +
                      as.factor(highest_degree) +
                      as.factor(year) + 
                      as.factor(size)+
                      as.factor(current_loc_new) ,
                    family=gaussian(),
                    design = design_tem_work)
stargazer(tem_satis1,type="text",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

### ----R2----
summary.lm(lm_male1)
summary.lm(lm_male2)
summary.lm(tem_satis1)


### ----subgroup----
design_work0 <- subset(design_work, degree_place == 0)
lm_place0 <- svyglm(satis_sec ~ as.factor(work_stem)+
                      as.factor(race_new)+
                      age + 
                      age2 + 
                      as.factor(married_new) +
                      as.factor(child_total) + 
                      as.factor(mother_edu_new) + 
                      as.factor(father_edu_new) +
                      as.factor(highest_degree) +
                      as.factor(year)+
                      as.factor(sector)+
                      as.factor(full_time)+
                      fit+
                      as.factor(size)+
                      as.factor(current_loc_new), design = design_work0, family=gaussian())

# Subset the data for degree_place == 1
design_work1 <- subset(design_work, degree_place == 1)
lm_place1 <- svyglm(satis_sec ~ as.factor(work_stem)+as.factor(race_new)+
                      age + 
                      age2 + 
                      as.factor(married_new) +
                      as.factor(child_total) + 
                      as.factor(mother_edu_new) + 
                      as.factor(father_edu_new) +
                      as.factor(highest_degree) +
                      as.factor(year)+
                    as.factor(sector)+
                      as.factor(full_time)+
                      fit+
                      as.factor(size)+
                      as.factor(current_loc_new), design = design_work1, family=gaussian())
# Subset the data for degree_place == 2
design_work2 <- subset(design_work, degree_place == 2)
lm_place2 <- svyglm(satis_sec ~ as.factor(work_stem)+
                      as.factor(race_new)+
                      age + 
                      age2 + 
                      as.factor(married_new) +
                      as.factor(child_total) + 
                      as.factor(mother_edu_new) + 
                      as.factor(father_edu_new) +
                      as.factor(highest_degree) +
                      as.factor(year)+
                      +as.factor(sector)+
                      as.factor(full_time)+
                      fit+as.factor(size)+
                      as.factor(current_loc_new), design = design_work2, family=gaussian())

stargazer(lm_place0,lm_place1,lm_place2,type="text",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))
# ----Female----
## ----Load data----
female_per<-NSCG_0321_10%>%filter(gender=="F")
female_per<-female_per%>%mutate(
  current_job_new_new=case_when(
    current_job_new%in%c("282710")~"282730",
    # current_job_new%in%c("321950")~"341980",
    current_job_new%in%c("570950")~"570930",
    # current_job_new%in%c("732510")~"732520",
    # current_job_new%in%c("772330")~"770100",
    current_job_new%in%c("121720")~"121730",
    current_job_new%in%c("331910")~"331960",
    current_job_new%in%c("570920")~"570990",
    current_job_new%in%c("742880")~"742990",
    
    TRUE~as.character(current_job_new)
  )
)%>%
  mutate(fit=case_when(stem_major==1&work_stem==1~1,
                       stem_major==0&work_stem==0~1,
                       stem_major==1&work_stem==0~0,
                       stem_major==0&work_stem==1~0))
female_work<-female_per%>%filter(unemploy==0)%>%
  mutate(size = if_else(employ_size %in% c(7, 8), "7", employ_size))


## ----Descriptive statistics----

# Define survey design

female_design <- svydesign(id = ~1, data = female_per, weights = ~weight)

female_work <- svydesign(id = ~1, data = female_work,weights = ~weight)
# Weighted unemployment 
svytable(~degree_place+unemploy,female_design)%>%prop.table(margin = 1)
# Calculate the average of satis_sec for each degree_place
svyby(~satis_sec, ~degree_place,female_work,svymean, na.rm = TRUE)
# Weighted race
svytable(~degree_place+race_new, female_design)%>%prop.table(margin = 1)
# Weighted region
svytable(~degree_place + current_loc_new,female_design)%>%prop.table(margin = 1)*100
# Calculate the average of age for each degree_place
svyby(~age, ~degree_place, female_design, svymean, na.rm = TRUE)
# Weighted type of degree
svytable(~degree_place + highest_degree, female_design)%>%prop.table(margin = 1)*100
# Weighted year
svytable(~degree_place + year, female_design)%>%prop.table(margin = 1)*100
# Weighted stem work
svytable(~degree_place +work_stem , female_design)%>%prop.table(margin = 1)*100
# Weighted mother's education
svytable(~degree_place +mother_edu_new , female_design)%>%prop.table(margin = 1)*100
# Weighted father's education
svytable(~degree_place +father_edu_new , female_design)%>%prop.table(margin = 1)*100
# Weighted father's education
svytable(~degree_place +married_new , female_design)%>%prop.table(margin = 1)*100
# Weighted number of children
svytable(~degree_place +child_total , female_design)%>%prop.table(margin = 1)*100
# Weighted sector
svytable(~degree_place +sector , female_work,na.rm = TRUE)%>%prop.table(margin = 1)*100
# Weighted full time 
svytable(~degree_place +full_time , female_work,na.rm = TRUE)%>%prop.table(margin = 1)*100
# Weighted education occupation fit
svytable(~ degree_place+fit ,female_work ,na.rm = TRUE)%>%prop.table(margin = 1)*100
# Weighted firm size
svytable(~ degree_place+size ,female_work ,na.rm = TRUE)%>%prop.table(margin = 1)*100
## ----Unemployment----
### ----Logistic regression----

female_per$degree_place <- factor(female_per$degree_place)
female_per$degree_place <- relevel(female_per$degree_place, ref = "0")
female_design <- svydesign(id = ~1, data = female_per, weights = ~weight)

glm_female1<- svyglm(unemploy ~ as.factor(degree_place)+
                       as.factor(work_stem)+
                       as.factor(race_new)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year) + 
                       as.factor(current_loc_new),
                     design = female_design,  family = quasibinomial())

glm_female2<- svyglm(unemploy ~ as.factor(degree_place)*as.factor(work_stem)+
                       as.factor(race_new)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year)+
                       as.factor(current_loc_new) ,
                     design = female_design,  family = quasibinomial())

stargazer(glm_female1,glm_female2,type="text",star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))


### ----R2----
# Fit the null model
null_model <- svyglm(unemploy ~ 1, design = female_design, family = quasibinomial())

# Extract null and residual deviance
null_deviance <- null_model$deviance
residual_deviance <- glm_female1$deviance

# Extract total number of observations (N) and number of predictors (k)
N <- nrow(female_design$variables)  # Total observations in survey design
k <- length(coef(glm_female1)) - 1  # Number of predictors (excluding intercept)

# Calculate adjusted pseudo R-squared
pseudo_r2_adj_female <- 1 - (residual_deviance / null_deviance) * ((N - 1) / (N - k - 1))
pseudo_r2_adj_female


# Extract null deviance from the null model
null_deviance <- null_model$deviance
residual_deviance <- glm_female2$deviance
N <- nrow(female_design$variables)  # Total observations in survey design
k <- length(coef(glm_female2)) - 1  # Number of predictors (excluding intercept)

# Calculate pseudo R²
pseudo_r2_female <- 1 - (residual_deviance / null_deviance)* ((N - 1) / (N - k - 1))
pseudo_r2_female

### ----subgroup----
design_place0 <- subset(female_design, degree_place == 0)
glm_place0 <- svyglm(unemploy ~ as.factor(work_stem)+
                       as.factor(race_new)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year)+
                       as.factor(current_loc_new), design = design_place0, family = quasibinomial())

# Subset the data for degree_place == 1
design_place1 <- subset(female_design, degree_place == 1)
glm_place1 <- svyglm(unemploy ~ as.factor(work_stem)+
                       as.factor(race_new)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year)+
                       as.factor(current_loc_new), design = design_place1, family = quasibinomial())
# Subset the data for degree_place == 2
design_place2 <- subset(female_design, degree_place == 2)
glm_place2 <- svyglm(unemploy ~ as.factor(work_stem)+
                       as.factor(race_new)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year)+
                       as.factor(current_loc_new), design = design_place2, family = quasibinomial())
stargazer(glm_place0,glm_place1,glm_place2,type="text",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

### ----fixed effect----
library(fixest)
#two categories
fixed4 <- feglm(
  unemploy ~ as.factor(degree_place) +
    as.factor(race_new) +
    age + 
    age2 + 
    as.factor(married_new) +
    as.factor(child_total) + 
    as.factor(mother_edu_new) + 
    as.factor(father_edu_new) +
    as.factor(highest_degree) +
    as.factor(year) +
    as.factor(current_loc_new) |
    as.factor(work_stem),   # Fixed effects here
  data =female_per,
  family = binomial("logit"),
  weights = ~weight   # Add weights here
)

#Five categories

fixed5<- feglm(
  unemploy ~ as.factor(degree_place) +
    as.factor(race_new) +
    age + 
    age2 + 
    as.factor(married_new) +
    as.factor(child_total) + 
    as.factor(mother_edu_new) + 
    as.factor(father_edu_new) +
    as.factor(highest_degree) +
    as.factor(year) +
    as.factor(current_loc_new) |
    as.factor(current_job_broad),   # Fixed effects here
  data = female_per,
  family = binomial("logit"),
  weights = ~weight   # Add weights here
)

#126 detailed occupations

fixed6 <- feglm(
  unemploy ~ as.factor(degree_place) +
    as.factor(race_new) +
    age + 
    age2 + 
    as.factor(married_new) +
    as.factor(child_total) + 
    as.factor(mother_edu_new) + 
    as.factor(father_edu_new) +
    as.factor(highest_degree) +
    as.factor(year) +
    as.factor(current_loc_new) |
    as.factor(current_job_new_new),   # Fixed effects here
  data = female_per,
  family = binomial("logit"),
  weights = ~weight   # Add weights here
)

summary(fixed4, 
        type = "text", 
        stars = TRUE, 
        star.cutoffs = c(0.05, 0.01, 0.001))
modelsummary(fixed4, 
             type = "text", 
             stars = TRUE, 
             star.cutoffs = c(0.05, 0.01, 0.001))
summary(fixed5, 
        type = "text", 
        stars = TRUE, 
        star.cutoffs = c(0.05, 0.01, 0.001))
modelsummary(fixed5, 
             type = "text", 
             stars = TRUE, 
             star.cutoffs = c(0.05, 0.01, 0.001))
summary(fixed6, 
        type = "text", 
        stars = TRUE, 
        star.cutoffs = c(0.05, 0.01, 0.001))
modelsummary(fixed6, 
             type = "text", 
             stars = TRUE, 
             star.cutoffs = c(0.05, 0.01, 0.001))
### ----plot----

hypothetical_data <- expand.grid(
  work_stem = c(0,1),
  degree_place = c(0,1,2),
  age = 43,
  age2 = 43^2,
  married_new = 1,
  child_total = 0,
  mother_edu_new = 0,
  father_edu_new = 0,
  highest_degree = 1,
  year = 2013,
  current_loc_new = 4,
  race_new = 0
)
# Predict probabilities of unemployment based on the model
predicted_probs1 <- predict(glm_female1, newdata = hypothetical_data, type = "response", se.fit = TRUE)
response1 <- c(0.026046,0.017831,0.033774, 0.023180,0.054752 ,  0.037836)
SE1 <- c(0.0033, 0.0024, 0.0057, 0.0041, 0.0085, 0.0062)

# Create a data frame
predicted_df1 <- data.frame(
  Response = response1,
  SE = SE1
)
# Calculate the 95% confidence interval
z_value <- 1.96  # Z-score for 95% confidence
predicted_df1$Lower_CI <- predicted_df1$Response - (z_value * predicted_df1$SE)
predicted_df1$Upper_CI <- predicted_df1$Response + (z_value * predicted_df1$SE)
# View the final data frame
print(predicted_df1)


# Initialize work_stem with NA or any default value
predicted_df1$work_stem <- NA  # or you can initialize with 0s

# Assign 0 to rows 1, 3, 5
predicted_df1$work_stem[c(1, 3, 5)] <- 0

# Assign 1 to rows 2, 4, 6
predicted_df1$work_stem[c(2, 4, 6)] <- 1

# Initialize work_stem with NA or any default value
predicted_df1$degree_place<- NA  # or you can initialize with 0s


predicted_df1$degree_place[c(1,2)] <- 0

# Assign 1 to rows 2, 4, 6
predicted_df1$degree_place[c(3,4)] <- 1
predicted_df1$degree_place[c(5,6)] <- 2

# View the updated data frame
print(predicted_df1)


ggplot(predicted_df1, aes(x = as.factor(degree_place), y = Response, group = as.factor(work_stem))) +
  geom_point(aes(shape = as.factor(work_stem)), size = 3, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(16, 17),  # Different point shapes for STEM and Non-STEM
                     labels = c("Non-STEM occupation", "STEM occupation")) +
  labs(title = "Predicted probabilities of unemployment by occupation for female natives and immigrants",
       x = "",
       y = "Predicted probability of unemployment",
       shape = "Occupation type") +
  scale_x_discrete(labels = c("Native-born", "Immigrant, US degree", "Immigrant, foreign degree")) +  # Custom labels for degree place
  scale_y_continuous(breaks = seq(0.0, 0.08, by = 0.01), limits = c(0.01, 0.08)) +  # Set y-axis ticks and limits
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA),   # Set plot background to white
    legend.background = element_rect(fill = "white", color = NA)  # Set legend background to white
  )

### ----plot: both----
library(patchwork)

# Define shared y-axis scale
y_scale <- scale_y_continuous(
  breaks = seq(0.0, 0.08, by = 0.01),  # Tick marks
  limits = c(0.01, 0.08)              # Consistent range for y-axis
)

# Plot for males
plot_male <- ggplot(predicted_df, aes(x = as.factor(degree_place), y = Response, group = as.factor(work_stem))) +
  geom_point(aes(shape = as.factor(work_stem)), size = 3, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(width = 0.5)) +
  scale_shape_manual(
    values = c(16, 17), 
    labels = c("Non-STEM occupation", "STEM occupation")
  ) +
  labs(
    title = "Male Natives and Immigrants",
    x = "",
    y = "Predicted Probability of Unemployment",
    shape = "Occupation Type"
  ) +
  scale_x_discrete(labels = c("Native-born", "Immigrant, US degree", "Immigrant, foreign degree")) +
  y_scale +  # Apply shared y-axis scale
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Plot for females
plot_female <- ggplot(predicted_df1, aes(x = as.factor(degree_place), y = Response, group = as.factor(work_stem))) +
  geom_point(aes(shape = as.factor(work_stem)), size = 3, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(width = 0.5)) +
  scale_shape_manual(
    values = c(16, 17), 
    labels = c("Non-STEM occupation", "STEM occupation")
  ) +
  labs(
    title = "Female Natives and Immigrants",
    x = "",
    y = "",
    shape = "Occupation Type"
  ) +
  scale_x_discrete(labels = c("Native-born", "Immigrant, US degree", "Immigrant, foreign degree")) +
  y_scale +  # Apply shared y-axis scale
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Combine the plots with a shared legend
combined_plot <- plot_male + plot_female +
  plot_layout(ncol = 2, guides = "collect") &  # Arrange side-by-side and share the legend
  theme(legend.position = "bottom")           # Move legend to the bottom
combined_plot

## ----Satisfaction----
female_work$degree_place <- factor(female_work$degree_place)
female_work$degree_place <- relevel(female_work$degree_place, ref = "0")
design_work_female <- svydesign(id = ~1, data = female_work, weights = ~weight)
lm_female1<- svyglm( satis_sec ~ as.factor(degree_place)+as.factor(work_stem)+
                       as.factor(race_new)+
                       fit+
                       full_time+
                       as.factor(sector)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year) + 
                       as.factor(current_loc_new)+as.factor(size) ,
                     family=gaussian(),
                     design = design_work_female)
lm_female2<- svyglm(satis_sec ~ as.factor(degree_place)*as.factor(work_stem)+
                      as.factor(race_new)+
                      fit+
                      full_time+
                      as.factor(sector)+
                      age + 
                      age2 + 
                      as.factor(married_new) +
                      as.factor(child_total) + 
                      as.factor(mother_edu_new) + 
                      as.factor(father_edu_new) +
                      as.factor(highest_degree) +
                      as.factor(year) + 
                      as.factor(current_loc_new)+
                      as.factor(size),
                    family=gaussian(),
                    design = design_work_female)
stargazer(lm_female1,lm_female2,type="text",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))
### ----R2----
summary.lm(lm_female1)
summary.lm(lm_female2)
summary.lm(tem_satis1_female)

### ----fixed effect----
lm_female2_3 <- plm(satis_sec ~ as.factor(degree_place)+
                      as.factor(race_new) + 
                      age+
                      age2+ 
                      as.factor(married_new)+
                      as.factor(child_total)+ 
                      as.factor(mother_edu_new)+
                      as.factor(father_edu_new) +
                      as.factor(highest_degree) + 
                      as.factor(year) + 
                      as.factor(current_loc_new)+
                      as.factor(sector)+
                      as.factor(full_time)+
                      fit+
                      as.factor(size),
                    data = female_work,
                    index = c("work_stem"),
                    model = "within",
                    weights = weight )
lm_female2_4 <- plm(satis_sec ~ as.factor(degree_place)+
                      as.factor(race_new)+
                      age+
                      age2+ 
                      as.factor(married_new)+
                    as.factor(child_total)+ 
                      as.factor(mother_edu_new)+
                      as.factor(father_edu_new) +
                      as.factor(highest_degree)+
                      as.factor(year) + 
                      as.factor(current_loc_new)+
                      as.factor(sector)+
                      as.factor(full_time)+
                      fit+
                      as.factor(size),
                    data = female_work,
                    index = c("current_job_broad"),
                    model = "within",
                    weights = weight )
lm_female2_5 <- plm(satis_sec ~ as.factor(degree_place)+
                      as.factor(race_new)+
                      age+
                      age2+ 
                      as.factor(married_new)+
                      as.factor(child_total)+ 
                      as.factor(mother_edu_new)+
                      as.factor(father_edu_new)+
                      as.factor(highest_degree)+
                      as.factor(year) + 
                      as.factor(current_loc_new)+
                      as.factor(sector)+
                      as.factor(full_time)+
                      fit+
                      as.factor(size),
                    data = female_work,
                    index = c("current_job_new_new"),
                    model = "within",
                    weights = weight )
stargazer(lm_female2_3,lm_female2_4,lm_female2_5,type="text",
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))
### ----temporary----
female_temporary<-temporary%>%filter(gender=="F")
female_tem_work<-female_temporary%>%filter(unemploy==0,
                                           citizen%in%c(0,3)|tem%in%c(1,2,3,4))%>%mutate(
                                             work_visa=case_when(degree_place == 0~0,#US born
                                                                 degree_place==1&citizen %in% c(0, 3)~1,#foreign born, US degree, non-temporary
                                                                 degree_place==2&citizen %in% c(0, 3)~2,#foreign born, foreign degree, temporary
                                                                 degree_place%in%c(1)&citizen %in% c(4)~3,#foreign born, US degree, temporary
                                                                 degree_place%in%c(2)&citizen %in% c(4)~3),
                                             fit=case_when(stem_major==1&work_stem==1~1,
                                                           stem_major==0&work_stem==0~1,
                                                           stem_major==1&work_stem==0~0,
                                                           stem_major==0&work_stem==1~0)
                                             
                                           )%>%
  mutate(size = if_else(employ_size %in% c(7, 8), "7", employ_size))

female_tem_work$work_visa <- factor(female_tem_work$work_visa)
female_tem_work$work_visa <- relevel(female_tem_work$work_visa, ref = "1")
design_tem_work_female <- svydesign(id = ~1, data = female_tem_work,weights = ~weight)

tem_satis1_female<- svyglm( satis_sec ~ as.factor(work_visa)+as.factor(work_stem)+
                       as.factor(race_new)+
                       fit+
                       full_time+
                       as.factor(sector)+
                       age + 
                       age2 + 
                       as.factor(married_new) +
                       as.factor(child_total) + 
                       as.factor(mother_edu_new) + 
                       as.factor(father_edu_new) +
                       as.factor(highest_degree) +
                       as.factor(year) + 
                       as.factor(size)+
                       as.factor(current_loc_new) ,
                     family=gaussian(),
                     design = design_tem_work_female)


design_work5 <- subset(design_work_female, degree_place == 0)
lm_place5 <- svyglm(satis_sec ~ as.factor(work_stem)+as.factor(race_new)+
                      age + 
                      age2 + 
                      as.factor(married_new) +
                      as.factor(child_total) + 
                      as.factor(mother_edu_new) + 
                      as.factor(father_edu_new) +
                      as.factor(highest_degree) +
                      as.factor(year)+as.factor(sector)+as.factor(full_time)+fit+as.factor(size)+
                      
                      as.factor(current_loc_new), design = design_work5, family=gaussian())
### ----subgroup----
# Subset the data for degree_place == 1
design_work6 <- subset(design_work_female, degree_place == 1)
lm_place6 <- svyglm(satis_sec ~ as.factor(work_stem)+
                      as.factor(race_new)+
                      age + 
                      age2 + 
                      as.factor(married_new)+
                      as.factor(child_total)+ 
                      as.factor(mother_edu_new)+ 
                      as.factor(father_edu_new)+
                      as.factor(highest_degree)+
                      as.factor(year)+
                  as.factor(sector)+
                    as.factor(full_time)+
                    fit+
                    as.factor(size)+
                      as.factor(current_loc_new), design = design_work6, family=gaussian())
# Subset the data for degree_place == 2
design_work7 <- subset(design_work_female, degree_place == 2)
lm_place7 <- svyglm(satis_sec ~ as.factor(work_stem)+as.factor(race_new)+
                      age + 
                      age2 + 
                      as.factor(married_new) +
                      as.factor(child_total) + 
                      as.factor(mother_edu_new) + 
                      as.factor(father_edu_new) +
                      as.factor(highest_degree) +
                      as.factor(year)+
                      +as.factor(sector)+as.factor(full_time)+fit+as.factor(size)+
                      as.factor(current_loc_new), design = design_work7, family=gaussian())
### ----plot----
hypothetical_data <- expand.grid(
  degree_place = c(0, 1, 2),  # Assume 0: native, 1: US degree, 2: foreign degree
  work_stem= c(0, 1),    # 0: Non-STEM, 1: STEM
  race_new = 0,               # Assuming race_new = 0 (example)
  sector = 3,                 # Example sector
  age = 43,                   # Example age
  age2 = 43^2,                # Age squared
  married_new = 1,            # Example marital status
  child_total = 0,            # Example number of children
  mother_edu_new = 0,         # Example mother education
  father_edu_new = 0,         # Example father education
  highest_degree = 1,         # Example highest degree
  year = 2013,                # Example year
  current_loc_new = 4,         # Example current location
  full_time=1,
  fit=1,
  size=7
)
# Predict probabilities of unemployment based on the model
predicted_probs2 <- predict(lm_male1, newdata = hypothetical_data, type = "response", se.fit = TRUE)
predicted_probs3 <- predict(lm_female1, newdata = hypothetical_data, type = "response", se.fit = TRUE)


response2 <- c( 3.1578,  3.1822, 3.0902,3.1146, 3.0411, 3.0655)
SE2 <- c(0.0196, 0.0188, 0.0248,  0.024, 0.0259, 0.0251)

# Create a data frame
predicted_df2 <- data.frame(
  Response = response2,
  SE = SE2
)
# Calculate the 95% confidence interval
z_value <- 1.96  # Z-score for 95% confidence
predicted_df2$Lower_CI <- predicted_df2$Response - (z_value * predicted_df2$SE)
predicted_df2$Upper_CI <- predicted_df2$Response + (z_value * predicted_df2$SE)
# View the final data frame

# Initialize work_stem with NA or any default value
predicted_df2$work_stem <- NA  # or you can initialize with 0s

# Assign 0 to rows 1, 3, 5
predicted_df2$work_stem[c(1, 3, 5)] <- 0

# Assign 1 to rows 2, 4, 6
predicted_df2$work_stem[c(2, 4, 6)] <- 1

# Initialize work_stem with NA or any default value
predicted_df2$degree_place<- NA  # or you can initialize with 0s


predicted_df2$degree_place[c(1,2)] <- 0

# Assign 1 to rows 2, 4, 6
predicted_df2$degree_place[c(3,4)] <- 1
predicted_df2$degree_place[c(5,6)] <- 2


response3 <- c( 3.1827,  3.2699, 3.1023, 3.1895, 3.0605, 3.1477 )
SE3<- c(0.0203,0.0199,  0.0272,  0.0265, 0.03,0.0293)

# Create a data frame
predicted_df3 <- data.frame(
  Response = response3,
  SE = SE3
)
# Calculate the 95% confidence interval
z_value <- 1.96  # Z-score for 95% confidence
predicted_df3$Lower_CI <- predicted_df3$Response - (z_value * predicted_df3$SE)
predicted_df3$Upper_CI <- predicted_df3$Response + (z_value * predicted_df3$SE)
# Initialize work_stem with NA or any default value
predicted_df3$work_stem <- NA  # or you can initialize with 0s

# Assign 0 to rows 1, 3, 5
predicted_df3$work_stem[c(1, 3, 5)] <- 0

# Assign 1 to rows 2, 4, 6
predicted_df3$work_stem[c(2, 4, 6)] <- 1

# Initialize work_stem with NA or any default value
predicted_df3$degree_place<- NA  # or you can initialize with 0s


predicted_df3$degree_place[c(1,2)] <- 0

# Assign 1 to rows 2, 4, 6
predicted_df3$degree_place[c(3,4)] <- 1
predicted_df3$degree_place[c(5,6)] <- 2

y_scale <- scale_y_continuous(
  breaks = seq(2.5, 3.5, by = 0.25),  # Tick marks
  limits = c(2.75, 3.5)              # Consistent range for y-axis
)

# Plot for males
plot_male <- ggplot(predicted_df2, aes(x = as.factor(degree_place), y = Response, group = as.factor(work_stem))) +
  geom_point(aes(shape = as.factor(work_stem)), size = 3, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(width = 0.5)) +
  scale_shape_manual(
    values = c(16, 17), 
    labels = c("Non-STEM occupation", "STEM occupation")
  ) +
  labs(
    title = "Male Natives and Immigrants",
    x = "",
    y = "Predicted Job Security Satisfaction",
    shape = "Occupation Type"
  ) +
  scale_x_discrete(labels = c("Native-born", "Immigrant, US degree", "Immigrant, foreign degree")) +
  y_scale +  # Apply shared y-axis scale
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

# Plot for females
plot_female <- ggplot(predicted_df3, aes(x = as.factor(degree_place), y = Response, group = as.factor(work_stem))) +
  geom_point(aes(shape = as.factor(work_stem)), size = 3, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, position = position_dodge(width = 0.5)) +
  scale_shape_manual(
    values = c(16, 17), 
    labels = c("Non-STEM occupation", "STEM occupation")
  ) +
  labs(
    title = "Female Natives and Immigrants",
    x = "",
    y = "",
    shape = "Occupation Type"
  ) +
  scale_x_discrete(labels = c("Native-born", "Immigrant, US degree", "Immigrant, foreign degree")) +
  y_scale +  # Apply shared y-axis scale
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )
library(patchwork)
# Combine the plots with a shared legend
combined_plot <- plot_male + plot_female +
  plot_layout(ncol = 2, guides = "collect") &  # Arrange side-by-side and share the legend
  theme(legend.position = "bottom")           # Move legend to the bottom
combined_plot

## ----t-test----
# unemployment
difference<--0.168-(-0.387)
se_difference<-sqrt(0.056^2+0.062^2)
t_stat <- difference / se_difference
df_small <- min(df.residual(glm_female1), df.residual(glm_male1))
p_value <- 2 * pt(-abs(t_stat), df = df_small)
rounded_x <- round(p_value, 2)
cat("P-value:", p_value, "\n")

#job security satisfaction

difference1<--0.025-(0.087)
se_difference1<-sqrt(0.007^2+0.008^2)
t_stat1 <- difference1 / se_difference1
df_small1 <- min(df.residual(lm_female1), df.residual(lm_male1))


p_value1 <- 2 * pt(-abs(t_stat1), df = df_small1)
rounded_x <- round(p_value1, 2)
cat("P-value:", p_value1, "\n")
