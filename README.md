# Past-Projects
Relevant projects to demonstrate statistical competence across graduate work.

---
title: "SOL_Analysis_Diet_Exercise"
output:
  pdf_document: default
  html_document: default
date: "2026-07-18"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(openxlsx)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(broom)
library(tidyverse)
library(geepack)
library(emmeans)
library(ggplot2)
library(gtsummary)
library(broom.mixed)
library(knitr)
#install.packages("ggsignif")
library(ggsignif)
#install.packages("glmtoolbox")
library(glmtoolbox)
library(see)
library(gtsummary)
library(gt)
```

```{r}
library(geepack)   # For fitting the GEE model
library(ggeffects) # For extracting marginal effects
library(ggplot2)   # For plotting

library(dplyr)
library(pwrss)
```

```{r}
library(tinytex)
```

```{r}
tinytex::tlmgr_repo("https://ctan.org")
```

```{r}
#write.csv(SOL_Dataset, "~/Desktop/SOL Analysis/SOL_Dataset.csv")

SOL_Dataset <- read.csv("SOL_Dataset.csv") %>% 
  mutate(PID = as.factor(PID))

Final_Data_with_weights_4 <- SOL_Dataset %>% 
   mutate(Tobacco_Use = if_else(TBEA3 == "S", "3", TBEA3)) %>% 
  mutate(Tobacco_Use_1 = case_when(Tobacco_Use == 1 ~ "Daily",
                                   Tobacco_Use == 2 ~ "Some Days",
                                   Tobacco_Use == 3 ~ "Not At All")) %>%
  mutate(Tobacco_Use_1 = if_else(Tobacco_Use_1 == "Daily" | Tobacco_Use_1 == "Some Days", "Yes", "No")) %>% 
  select(PID, Birth_Complications_Cat,Average_sleep_Real_num_hours, Depression_Baseline, AGE, BMI, Num_preg_prior_V1, Tobacco_Use_T2, Receipt_of_Prenatal_Care, income, AGE_diff, Average_sleep_Real, Average_sleep_Real2, AHEI2010, PCE4_Delivery_Type, WHIIRS_Real, PCE7C_G, PCE2A_WEEKS, GPAQ_PAG2008, MHEA1_H, MHEA16_D, Tobacco_Use_1, LiveBirth_orNot2, PCE7B, AGE_V2, Depression_V2, PCE4) %>% 
   mutate(income = as.factor(income)) %>% 
  mutate(Tobacco_Use_T2 = as.factor(Tobacco_Use_T2)) %>% 
  mutate(Average_sleep_Real = as.factor(Average_sleep_Real)) %>% 
   mutate(Average_sleep_Real_2 = as.factor(Average_sleep_Real2)) %>% 
   mutate(Average_sleep_Real_2 = factor(Average_sleep_Real2, levels = c("2", "1", "3"))) %>% 
  mutate(GPAQ_PAG2008 = as.factor(GPAQ_PAG2008)) %>% 
  mutate(GPAQ_PAG20082 = factor(GPAQ_PAG2008, levels = c("2", "1", "3", "4"))) %>% 
    mutate(GPAQ_PAG20083 = factor(GPAQ_PAG2008, levels = c("3", "1", "2", "4"))) %>% 
    mutate(GPAQ_PAG20084 = factor(GPAQ_PAG2008, levels = c("4", "1", "2", "3"))) %>% 
  mutate(Receipt_of_Prenatal_Care = as.factor(Receipt_of_Prenatal_Care)) %>% 
  rename(Hypertension_Visit_1 = MHEA1_H) %>% 
  rename(Diabetes_Visit_1 = MHEA16_D) %>% 
  rename(Infant_Gender = PCE7B) %>% 
  mutate(New_PA = case_when(GPAQ_PAG2008 == "2" | GPAQ_PAG2008 == "1" ~ "No",
                            GPAQ_PAG2008 == "4" | GPAQ_PAG2008 == "3" ~ "Yes")) %>% 
  mutate(PID = as.factor(PID))

Physical_Activity <- read.csv("pa_derv_lad1.csv") %>% 
  select(PID, PAG2008, PAG2008YN, ADHERENTYN, MV_WEEK) %>% 
  mutate(PID = as.factor(PID))

Final_Data_with_weights_4 <- Physical_Activity %>% 
  right_join(Final_Data_with_weights_4, by = c("PID" = "PID"), relationship = "many-to-many") %>% 
  mutate(PAG2008YN = if_else(PAG2008YN == 0, "0", "1")) %>% 
  mutate(PAG2008 = case_when(PAG2008 == 1 | PAG2008 == 2 ~ "1&2",
                                  PAG2008 == 3 ~ "3",
                                  PAG2008 == 4 ~ "4"))

Final_Data_with_weights_4 <- Final_Data_with_weights_4 %>%
  mutate(PAG2008_2 = factor(PAG2008, levels = c("3", "1&2", "4")))
```

```{r}
Final_Data_with_Weights_Deduplicated <- Final_Data_with_weights_4[!duplicated(Final_Data_with_weights_4$PID), ]
```

```{r}
Final_Data_with_weights_4 <- Final_Data_with_weights_4 %>% 
  mutate(low_birthweight = if_else(PCE7C_G < 2500, "Low", "Average/High")) %>% 
  mutate(low_GA = if_else(PCE2A_WEEKS < 37, "Low", "Average/High"))
```

```{r}
Final_Data_with_weights_4 %>%
  select(AGE, AGE_V2, income, BMI, Tobacco_Use_T2, Num_preg_prior_V1, 
         Depression_Baseline, Receipt_of_Prenatal_Care, PCE4_Delivery_Type, PCE4, PCE2A_WEEKS, PCE7C_G, 
         Birth_Complications_Cat, Hypertension_Visit_1, Diabetes_Visit_1, Tobacco_Use_1, LiveBirth_orNot2, Infant_Gender, AHEI2010, Average_sleep_Real, Average_sleep_Real_num_hours, WHIIRS_Real, PAG2008YN, Depression_V2, ADHERENTYN, GPAQ_PAG2008, PAG2008, low_birthweight, low_GA, New_PA) %>%
  tbl_summary(label = list(AGE ~ "Maternal age V1",
      AGE_V2 ~ "Maternal age V2",
      income ~ "Household Income",
      BMI ~ "BMI V1",
      Num_preg_prior_V1 ~ "Pregnancies prior to Visit One",
      Depression_Baseline ~ "V1 Maternal Depression",
      PCE2A_WEEKS ~ "Gestational Age",
      PCE7C_G ~ "Infant Weight at Birth (grams)",
      Receipt_of_Prenatal_Care ~ "Receipt of Prenatal Care",
      Hypertension_Visit_1 ~ "Pre-pregnancy Hypertension",
      Diabetes_Visit_1 ~ "Pre-pregnancy Diabetes",
      Tobacco_Use_1 ~ "Smoking V1",
      Birth_Complications_Cat ~ "Pregnancy Complications",
      Tobacco_Use_T2 ~ "Smoking V2",
      Infant_Gender ~ "Infant Gender",
      PCE4_Delivery_Type ~ "Delivery Type",
      LiveBirth_orNot2 ~ "Fetal Demise (2)",
      AHEI2010 ~ "Dietary Quality (Alternative Healthy Eating Index (AHE2010))",
      PAG2008YN ~ "Accelerometer (Actical)",
      GPAQ_PAG2008 ~ "GPAQ",
      PAG2008 ~ "Actical 4 cat",
      Average_sleep_Real_num_hours ~ "Average Sleep (Weekdays and Weekends)",
      WHIIRS_Real ~ "Insomnia Symptoms (Self Reported)",
      Average_sleep_Real ~ "Sleep Categories",
      ADHERENTYN ~ "Adherance",
      PCE4 ~ "Original Delivery Type",
      low_birthweight ~ "Low Birth Weight",
      low_GA ~ "Low Gestational Age",
      New_PA ~ "Met Guidelines"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = list(
    all_continuous() ~ 2,
    all_categorical() ~ c(0, 0)# 0 for count, 2 for percentage
  ),
    type = list(
      AGE ~ "continuous",
      AGE_V2 ~ "continuous",
      income ~ "categorical",
      BMI ~ "continuous",
      Num_preg_prior_V1 ~ "continuous",
      Depression_Baseline ~ "continuous",
      Depression_V2 ~ "continuous",
      Receipt_of_Prenatal_Care ~ "categorical",
      PCE4_Delivery_Type ~ "categorical",
      PCE2A_WEEKS ~ "continuous",
      PCE7C_G ~ "continuous",
      Hypertension_Visit_1 ~ "categorical",
      Diabetes_Visit_1 ~ "categorical",
      Tobacco_Use_1 ~ "categorical",
      Birth_Complications_Cat ~ "categorical",
      Tobacco_Use_T2 ~ "categorical",
      Infant_Gender ~ "categorical",
      LiveBirth_orNot2 ~ "categorical",
      AHEI2010 ~ "continuous",
      WHIIRS_Real ~ "continuous",
      PAG2008YN ~ "categorical",
      Average_sleep_Real_num_hours ~ "continuous",
      Average_sleep_Real ~ "categorical",
      ADHERENTYN ~ "categorical",
      PAG2008 ~ "categorical",
      GPAQ_PAG2008 ~ "categorical",
      PCE4 ~ "categorical",
      low_birthweight ~ "categorical",
      low_GA ~ "categorical",
      New_PA ~ "categorical"
    )) 
```

```{r}
Final_Data_with_Weights_Deduplicated %>%
  select(AGE, AGE_V2, income, BMI, Tobacco_Use_T2, Num_preg_prior_V1, 
         Depression_Baseline, Receipt_of_Prenatal_Care, PCE4_Delivery_Type, PCE2A_WEEKS, PCE7C_G, 
         Birth_Complications_Cat, Hypertension_Visit_1, Diabetes_Visit_1, Tobacco_Use_1, LiveBirth_orNot2, Infant_Gender, AHEI2010, Average_sleep_Real, Average_sleep_Real_num_hours, WHIIRS_Real, PAG2008YN, Depression_V2, ADHERENTYN, GPAQ_PAG2008, PAG2008) %>%
  tbl_summary(label = list(AGE ~ "Maternal age V1",
      AGE_V2 ~ "Maternal age V2",
      income ~ "Household Income",
      BMI ~ "BMI V1",
      Num_preg_prior_V1 ~ "Pregnancies prior to Visit One",
      Depression_Baseline ~ "V1 Maternal Depression",
      PCE2A_WEEKS ~ "Gestational Age",
      PCE7C_G ~ "Infant Weight at Birth (grams)",
      Receipt_of_Prenatal_Care ~ "Receipt of Prenatal Care",
      Hypertension_Visit_1 ~ "Pre-pregnancy Hypertension",
      Diabetes_Visit_1 ~ "Pre-pregnancy Diabetes",
      Tobacco_Use_1 ~ "Smoking V1",
      Birth_Complications_Cat ~ "Pregnancy Complications",
      Tobacco_Use_T2 ~ "Smoking V2",
      Infant_Gender ~ "Infant Gender",
      PCE4_Delivery_Type ~ "Delivery Type",
      LiveBirth_orNot2 ~ "Fetal Demise (2)",
      AHEI2010 ~ "Dietary Quality (Alternative Healthy Eating Index (AHE2010))",
      PAG2008YN ~ "Accelerometer (Actical)",
      GPAQ_PAG2008 ~ "GPAQ",
      PAG2008 ~ "Actical 4 cat",
      Average_sleep_Real_num_hours ~ "Average Sleep (Weekdays and Weekends)",
      WHIIRS_Real ~ "Insomnia Symptoms (Self Reported)",
      Average_sleep_Real ~ "Sleep Categories",
      ADHERENTYN ~ "Adherance"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = list(
    all_continuous() ~ 2,
    all_categorical() ~ c(0, 0)# 0 for count, 2 for percentage
  ),
    type = list(
      AGE ~ "continuous",
      AGE_V2 ~ "continuous",
      income ~ "categorical",
      BMI ~ "continuous",
      Num_preg_prior_V1 ~ "continuous",
      Depression_Baseline ~ "continuous",
      Depression_V2 ~ "continuous",
      Receipt_of_Prenatal_Care ~ "categorical",
      PCE4_Delivery_Type ~ "categorical",
      PCE2A_WEEKS ~ "continuous",
      PCE7C_G ~ "continuous",
      Hypertension_Visit_1 ~ "categorical",
      Diabetes_Visit_1 ~ "categorical",
      Tobacco_Use_1 ~ "categorical",
      Birth_Complications_Cat ~ "categorical",
      Tobacco_Use_T2 ~ "categorical",
      Infant_Gender ~ "categorical",
      LiveBirth_orNot2 ~ "categorical",
      AHEI2010 ~ "continuous",
      WHIIRS_Real ~ "continuous",
      PAG2008YN ~ "categorical",
      Average_sleep_Real_num_hours ~ "continuous",
      Average_sleep_Real ~ "categorical",
      ADHERENTYN ~ "categorical",
      PAG2008 ~ "categorical",
      GPAQ_PAG2008 ~ "categorical"
    )) %>% 
   as_gt() %>%
   tab_header(
   title = md("**Descriptives: Participant Health, Pregnancy, and Behavioral Characteristics**"),
    subtitle = md("*(n = 419) No Clustering*"))
```

```{r}
Final_Data_with_weights_5 <- Final_Data_with_weights_4 %>% 
  rename(Average_sleep_cat = Average_sleep_Real) %>% 
  mutate(Average_sleep_cat = case_when(Average_sleep_cat == 1 ~ "<6 hours",
                                  Average_sleep_cat == 2 ~ "6-9 hours",
                                   Average_sleep_cat == 3 ~ ">9 hours")) %>% 
  mutate(GPAQ_PAG2008 = case_when(GPAQ_PAG2008 == 1 ~ "Sedentary",
                                  GPAQ_PAG2008 == 2 ~ "Low",
                                  GPAQ_PAG2008 == 3 ~ "Medium",
                                  GPAQ_PAG2008 == 4 ~ "High")) %>% 
   mutate(GPAQ_PAG2008 = factor(GPAQ_PAG2008, levels = c("Sedentary", "Low", "Medium", "High"))) %>% 
  mutate(PAG2008 = case_when(PAG2008 == 1 ~ "Sedentary",
                                  PAG2008 == 2 ~ "Low",
                                  PAG2008 == 3 ~ "Medium",
                                  PAG2008 == 4 ~ "High")) %>% 
   mutate(PAG2008 = factor(PAG2008, levels = c("Sedentary", "Low", "Medium", "High"))) %>% 
  mutate(ADHERENTYN = if_else(ADHERENTYN == 0, "No", "Yes")) %>% 
  mutate(PAG2008YN = if_else(PAG2008YN == 0, "No (sedentary and low)", "Yes (medium and high)")) %>% 
  mutate(PCE4_Delivery_Type = if_else(PCE4_Delivery_Type == 0, "Vaginal Delivery", "C-Section")) %>% 
  mutate(Birth_Complications_Cat = if_else(Birth_Complications_Cat == 0, "No", "Yes"))
```

```{r}
Final_Data_with_weights_5 %>%
  select(Receipt_of_Prenatal_Care, PCE4_Delivery_Type, PCE2A_WEEKS, PCE7C_G, 
         Birth_Complications_Cat, LiveBirth_orNot2, Infant_Gender, low_birthweight, low_GA) %>%
  tbl_summary(label = list(
      PCE2A_WEEKS ~ "Gestational Age",
      PCE7C_G ~ "Infant Weight at Birth (grams)",
      Receipt_of_Prenatal_Care ~ "Receipt of Prenatal Care",
      Birth_Complications_Cat ~ "Pregnancy Complications",
      Infant_Gender ~ "Infant Gender",
      PCE4_Delivery_Type ~ "Delivery Type",
      LiveBirth_orNot2 ~ "Fetal Demise (2)",
      low_GA ~ "Low Gestational Age (Categorical)",
      low_birthweight ~ "Low Birth Weight (Categorical)"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = list(
    all_continuous() ~ 2,
    all_categorical() ~ c(0, 0)# 0 for count, 2 for percentage
  ),
    type = list(
      Receipt_of_Prenatal_Care ~ "categorical",
      PCE4_Delivery_Type ~ "categorical",
      PCE2A_WEEKS ~ "continuous",
      PCE7C_G ~ "continuous",
      Birth_Complications_Cat ~ "categorical",
      Infant_Gender ~ "categorical",
      LiveBirth_orNot2 ~ "categorical",
      low_GA ~ "categorical",
      low_birthweight ~ "categorical"
    )) %>% 
   as_gt() %>%
   tab_header(
   title = md("**Prevalence of Pregnancy and Infant Outcomes**"),
    subtitle = md("*Live Births Only (508 moms)*"))
```

```{r}
wb <- createWorkbook()
addWorksheet(wb,"Aim1_gee")

#Aim 1: Longitudinal Association between Sleep Quality and Birth Complications

#Aim1.1.1: Longitudinal Association between Sleep Quality (As measured by Average Hours of Sleep (weekends and weekdays) on Mothers' likelihood of Developing Birth Complications (0 = No Birth Complications, 1 = Has Birth Complications)

vars <- c("Birth_Complications_Cat","Average_sleep_Real_num_hours",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim1.1.1 <- geeglm(Birth_Complications_Cat~ Average_sleep_Real_num_hours + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
SummaryAim1.1.1 <-tidy(Aim1.1.1)
kable(SummaryAim1.1.1)

writeFormula(wb, "Aim1_gee", "Aim1.1.1 Birth_Complications_Cat", xy = c(1,3))
writeData(wb,"Aim1_gee", as.data.frame(SummaryAim1.1.1), rowNames=TRUE, colNames=TRUE, xy = c(1,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

##Aim1.1.2: Longitudinal Association between Sleep Quality (Sleep Quality (As measured by Average Hours of Sleep (weekends and weekdays) on mothers' likelihood of getting a C-section (1) in comparison to Vaginal Delivery (0))

vars <- c("PCE4_Delivery_Type","Average_sleep_Real_num_hours", 
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_sleep_PCE <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim1.1.2 <- geeglm(PCE4_Delivery_Type ~ Average_sleep_Real_num_hours +  AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_sleep_PCE, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
SummaryAim1.1.2 <- tidy(Aim1.1.2)
kable(SummaryAim1.1.2)

writeFormula(wb, "Aim1_gee", "Aim1.1.2 Delivery_Type", xy = c(10,3))
writeData(wb,"Aim1_gee", as.data.frame(SummaryAim1.1.2), rowNames=TRUE, colNames=TRUE, xy = c(10,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

##Aim1.1.3: Longitudinal Association between Sleep Quality (Sleep Quality (As measured by Average Hours of Sleep (weekends and weekdays) on infant's Birthweight

vars <- c("PCE7C_G","Average_sleep_Real_num_hours",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_PCE_Average_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m1.1.3 = geeglm(PCE7C_G ~ Average_sleep_Real_num_hours + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_PCE_Average_sleep, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m1.1.3)

writeFormula(wb, "Aim1_gee", "m1.1.3 Birthweight_grams", xy = c(20,3))
writeData(wb,"Aim1_gee", as.data.frame(summary(m1.1.3)$coefficients), rowNames=TRUE, colNames=TRUE, xy = c(20,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

##Aim1.1.4: Longitudinal Association between Sleep Quality (Sleep Quality (As measured by Average Hours of Sleep (weekends and weekdays) on gestational age

vars <- c("PCE2A_WEEKS","Average_sleep_Real_num_hours",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_Ges_Age_Average_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m1.1.4 = geeglm(PCE2A_WEEKS ~ Average_sleep_Real_num_hours + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Ges_Age_Average_sleep, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m1.1.4)

writeFormula(wb, "Aim1_gee", "m1.1.4 infant_age", xy = c(30,3))
writeData(wb,"Aim1_gee", as.data.frame(summary(m1.1.4)$coefficients), rowNames=TRUE, colNames=TRUE, xy = c(30,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Aim1.2.1: Longitudinal Association between Sleep Quality (As measured by Average Hours of Sleep (weekends and weekdays) as a categorical variable, group 1: <6 hours of sleep, group 2: 6-9 hours of sleep, group 3: >9 hours of sleep on mothers' likelihood of developing birth complications (0 = No Birth Complications, 1 = Has Birth Complications)

vars <- c("Birth_Complications_Cat","Average_sleep_Real", "Average_sleep_Real2",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_sleep_Real <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim1.2.1 <- geeglm(Birth_Complications_Cat ~ Average_sleep_Real_2 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff,  data = Final_Data_with_weights_no_Missing_sleep_Real, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(Aim1.2.1)
SummaryAim1.2.1 <-tidy(Aim1.2.1, conf.int = TRUE)
kable(SummaryAim1.2.1)

writeFormula(wb, "Aim1_gee", "Aim1.2.1 Birth_Complications_Cat", xy = c(1,16))
writeData(wb,"Aim1_gee", as.data.frame(SummaryAim1.2.1), rowNames=TRUE, colNames=TRUE, xy = c(1,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Aim1.2.2: Longitudinal Association between Sleep Quality (As measured by Average Hours of Sleep (weekends and weekdays) as a categorical variable, group 1: <6 hours of sleep, group 2: 6-9 hours of sleep, group 3: >9 hours of sleep on mothers' likelihood of getting a C-section (1) in comparison to Vaginal Delivery (0))

vars <- c("PCE4_Delivery_Type","Average_sleep_Real", "Average_sleep_Real2",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_sleepcat_PCE <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim1.2.2 <- geeglm(PCE4_Delivery_Type ~ Average_sleep_Real_2 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_sleepcat_PCE, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(Aim1.2.2)
SummaryAim1.2.2 <- tidy(Aim1.2.2, conf.int = TRUE)
kable(SummaryAim1.2.2)

writeFormula(wb, "Aim1_gee", "m1.2.2 Delivery_Type", xy = c(10,16))
writeData(wb,"Aim1_gee", as.data.frame(SummaryAim1.2.2), rowNames=TRUE, colNames=TRUE, xy = c(10,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Aim1.2.3: Longitudinal Association between Sleep Quality (As measured by Average Hours of Sleep (weekends and weekdays) as a categorical variable, group 1: <6 hours of sleep, group 2: 6-9 hours of sleep, group 3: >9 hours of sleep on infant's birth weight

vars <- c("PCE7C_G","Average_sleep_Real",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_PCE_Average_sleep_cat <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m1.2.3 = geeglm(PCE7C_G ~ Average_sleep_Real_2 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_PCE_Average_sleep_cat, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m1.2.3)
A <- tidy(m1.2.3, conf.int = TRUE)
kable(A)

writeFormula(wb, "Aim1_gee", "m1.2.3 Birthweight_grams", xy = c(20,16))
writeData(wb,"Aim1_gee", as.data.frame(A), rowNames=TRUE, colNames=TRUE, xy = c(20,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Aim1.2.4: Longitudinal Association between Sleep Quality (As measured by Average Hours of Sleep (weekends and weekdays) as a categorical variable, group 1: <6 hours of sleep, group 2: 6-9 hours of sleep, group 3: >9 hours of sleep on infant's birth weight

vars <- c("PCE2A_WEEKS","Average_sleep_Real",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_Ges_Age_Average_sleep_cat <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m1.2.4 = geeglm(PCE2A_WEEKS ~ Average_sleep_Real_2 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Ges_Age_Average_sleep_cat, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m1.2.4)
B <- tidy(m1.2.4, conf.int = TRUE)
kable(B)

writeFormula(wb, "Aim1_gee", "m1.2.4 Gestational Age", xy = c(30,16))
writeData(wb,"Aim1_gee", as.data.frame(B), rowNames=TRUE, colNames=TRUE, xy = c(30,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Aim1.3.1: Longitudinal Association between Sleep Quality (As measured by the Women's Health Initiative Insomnia Rating Scale) and mothers' likelihood developing Birth Complications

vars <- c("Birth_Complications_Cat","WHIIRS_Real",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_WHIIRS <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim1.3.1 <- geeglm(Birth_Complications_Cat ~ WHIIRS_Real + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_WHIIRS, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(Aim1.3.1)
SummaryAim1.3.1 <- tidy(Aim1.3.1)
kable(SummaryAim1.3.1)

writeFormula(wb, "Aim1_gee", "m1.3.1 Birth_Complications_Cat", xy = c(1,30))
writeData(wb,"Aim1_gee", as.data.frame(SummaryAim1.3.1), rowNames=TRUE, colNames=TRUE, xy = c(1,30), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Aim1.3.2: Longitudinal Association between Sleep Quality (As measured by the Women's Health Initiative Insomnia Rating Scale) and mothers' likelihood getting a C-section

vars <- c("PCE4_Delivery_Type","WHIIRS_Real",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "Depression_Baseline")
Final_Data_with_weights_no_Missing_PCE_WHIIRS <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]), ]

Aim1.3.2 <- geeglm(PCE4_Delivery_Type ~ WHIIRS_Real + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_PCE_WHIIRS, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(Aim1.3.2)
SummaryAim1.3.2 <- tidy(Aim1.3.2)
kable(SummaryAim1.3.2)

writeFormula(wb, "Aim1_gee", "m1.3.2 Delivery_Type", xy = c(10,30))
writeData(wb,"Aim1_gee", as.data.frame(SummaryAim1.3.2), rowNames=TRUE, colNames=TRUE, xy = c(10,31), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Aim1.3.3: Longitudinal Association between Sleep Quality (As measured by the Women's Health Initiative Insomnia Rating Scale) and infant's birth weight

vars <- c("PCE7C_G","WHIIRS_Real",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_PCE_WHIIRS <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m1.3.3 = geeglm(PCE7C_G ~ WHIIRS_Real + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_PCE_WHIIRS, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m1.3.3)

writeFormula(wb, "Aim1_gee", "m1.3.3 Birthweight_grams", xy = c(20,30))
writeData(wb,"Aim1_gee", as.data.frame(summary(m1.3.3)$coefficients), rowNames=TRUE, colNames=TRUE, xy = c(20,31), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Aim1.3.4: Longitudinal Association between Sleep Quality (As measured by the Women's Health Initiative Insomnia Rating Scale) and gestational age

vars <- c("PCE2A_WEEKS","WHIIRS_Real", 
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_PCE2A_WHIIRS <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m1.3.4 = geeglm(PCE2A_WEEKS ~ WHIIRS_Real +  AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_PCE2A_WHIIRS, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m1.3.4)

writeFormula(wb, "Aim1_gee", "m1.3.4 infant_age", xy = c(30,30))
writeData(wb,"Aim1_gee", as.data.frame(summary(m1.3.4)$coefficients), rowNames=TRUE, colNames=TRUE, xy = c(30,31), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

saveWorkbook(wb,"/Users/lijeannecheam/Desktop/Aim2_Sleep.xlsx", overwrite = TRUE)
```

```{r}
#Longitudinal Association between depression at visit 1 and birth complications

wb <- createWorkbook()
addWorksheet(wb,"Aim2")

vars <- c("Birth_Complications_Cat", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.1 <- glmgee (Birth_Complications_Cat~ Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(Aim_2.1.1)
SummaryAim2.1.1 <-tidy(Aim_2.1.1)
kable(SummaryAim2.1.1)

writeFormula(wb, "Aim2", "m2.1.1 Birth Complications", xy = c(1,3))
writeData(wb,"Aim2", SummaryAim2.1.1, rowNames=TRUE, colNames=TRUE, xy = c(1,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Longitudinal Association between depression at visit 1 and delivery type
vars <- c("PCE4_Delivery_Type", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.2 <- glmgee (PCE4_Delivery_Type~ Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
SummaryAim2.1.2 <-tidy(Aim_2.1.2)
summary(Aim_2.1.2)

writeFormula(wb, "Aim2", "m2.1.2 Delivery Type", xy = c(10,3))
writeData(wb,"Aim2", SummaryAim2.1.2, rowNames=TRUE, colNames=TRUE, xy = c(10,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Longitudinal Association between depression at visit 1 and Gestational age

vars <- c("PCE7C_G", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.3 <- glmgee (PCE7C_G ~ Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, id = PID, corstr = "exchangeable")
summary(Aim_2.1.3)
SummaryAim2.1.3 <-tidy(Aim_2.1.3)

writeFormula(wb, "Aim2", "m2.1.3 Birth Weight", xy = c(20,3))
writeData(wb,"Aim2", SummaryAim2.1.3, rowNames=TRUE, colNames=TRUE, xy = c(20,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Longitudinal Association between depression at visit 1 and birth weight

vars <- c("PCE2A_WEEKS", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.4 <- glmgee (PCE2A_WEEKS ~ Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, id = PID, corstr = "exchangeable")
summary(Aim_2.1.4)
SummaryAim2.1.4 <-tidy(Aim_2.1.4)

writeFormula(wb, "Aim2", "m2.1.4 Gestational Age", xy = c(30,3))
writeData(wb,"Aim2", SummaryAim2.1.4, rowNames=TRUE, colNames=TRUE, xy = c(30,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

saveWorkbook(wb,"/Users/lijeannecheam/Desktop/Aim2_Depression.xlsx", overwrite = TRUE)
```

```{r}
##Aim3.1.1: Longitudinal Association between Diet (AHEI) on birth complications

vars <- c("Birth_Complications_Cat","AHEI2010",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_AHEI <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim3.1.1 <- glmgee (Birth_Complications_Cat~ AHEI2010 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_AHEI, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(Aim3.1.1)
SummaryAim3.1.1 <-tidy(Aim3.1.1)
kable(SummaryAim3.1.1)

##Aim3.1.2: Longitudinal Association between Diet (AHEI) on delivery type

vars <- c("PCE4_Delivery_Type","AHEI2010",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_sleep_PCE4_AHEI <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim3.1.2 <- glmgee (PCE4_Delivery_Type ~ AHEI2010 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_sleep_PCE4_AHEI, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
SummaryAim3.1.2 <- tidy(Aim3.1.2)
kable(SummaryAim3.1.2)
summary(Aim3.1.2)


##Aim3.1.3: Longitudinal Association between Diet (AHEI) on infant's Birth weight

vars <- c("PCE7C_G","AHEI2010",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_PCE7G_AHEI <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m3.1.3 = glmgee (PCE7C_G ~ AHEI2010 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_PCE7G_AHEI, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m3.1.3)

##Aim3.1.4: Longitudinal Association between Diet (AHEI) on gestational age

vars <- c("PCE2A_WEEKS","AHEI2010",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_Ges_Age_AHEI <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m3.1.4 = glmgee (PCE2A_WEEKS ~ AHEI2010 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Ges_Age_AHEI, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m3.1.4)
```

```{r}
##Aim4.1.1: Longitudinal Association between Physical Activity (GPAQ_PAG2008) on birth complications

wb <- createWorkbook()
addWorksheet(wb,"Aim4")

vars <- c("Birth_Complications_Cat", "GPAQ_PAG2008", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "income")
Final_Data_with_weights_no_Missing_Indp_Birth <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m4.1.1 <- glmgee(Birth_Complications_Cat ~ GPAQ_PAG20083 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m4.1.1)
SummaryAim4.1.1 <-tidy(m4.1.1)
kable(SummaryAim4.1.1)

writeFormula(wb, "Aim4", "Aim4.1.1 Birth_Complications_Cat", xy = c(1,3))
writeData(wb,"Aim4", as.data.frame(SummaryAim4.1.1), rowNames=TRUE, colNames=TRUE, xy = c(1,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

##Aim4.1.2: Longitudinal Association between Physical Activity (GPAQ_PAG2008) on Delivery Type

vars <- c("PCE4_Delivery_Type", "GPAQ_PAG2008", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_Indp_Deliv <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m4.1.2 <- glmgee(PCE4_Delivery_Type ~ GPAQ_PAG20083 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Deliv,  family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m4.1.2)
SummaryAim4.1.2 <-tidy(m4.1.2, conf.int = TRUE)
kable(SummaryAim4.1.2)

writeFormula(wb, "Aim4", "Aim4.1.2 Delivery Type", xy = c(10,3))
writeData(wb,"Aim4", as.data.frame(SummaryAim4.1.2), rowNames=TRUE, colNames=TRUE, xy = c(10,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

##Aim4.1.3: Longitudinal Association between Physical Activity (GPAQ_PAG2008) on Birth Weight

vars <- c("PCE7C_G","GPAQ_PAG20083", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_bw <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m4.1.3 <- glmgee(PCE7C_G ~ GPAQ_PAG20083 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_bw, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m4.1.3)
SummaryAim4.1.3 <-tidy(m4.1.3, conf.int = TRUE)
kable(SummaryAim4.1.3)

writeFormula(wb, "Aim4", "Aim4.1.3 Birthweight", xy = c(20,3))
writeData(wb,"Aim4", as.data.frame(SummaryAim4.1.3), rowNames=TRUE, colNames=TRUE, xy = c(20,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#3>4 (3 is ref)

#marginal_predictions1 <- ggemmeans(m4.1.3, terms = c("GPAQ_PAG20083", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2","Receipt_of_Prenatal_Care","income","AGE_diff"))

#plot(marginal_predictions1) + 
  #labs(title = "Population-Average Marginal Effects Over Time",
       #x = "Physical Activity", 
       #y = "Predicted birth weight") +
  #theme_minimal()

vars <- c("PCE2A_WEEKS","GPAQ_PAG2008", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_ges_age <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]
##Aim4.1.4: Longitudinal Association between Physical Activity (GPAQ_PAG2008) on Gestational Age

m4.1.4 <- glmgee(PCE2A_WEEKS ~ GPAQ_PAG20083 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_ges_age, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m4.1.4)
SummaryAim4.1.4 <-tidy(m4.1.4)
kable(SummaryAim4.1.4)
writeFormula(wb, "Aim4", "Aim4.1.4 Gestational Age", xy = c(30,3))
writeData(wb,"Aim4", as.data.frame(SummaryAim4.1.4), rowNames=TRUE, colNames=TRUE, xy = c(30,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

saveWorkbook(wb,"/Users/lijeannecheam/Desktop/Aim4_DietRefgroup1.xlsx", overwrite = TRUE)
```

```{r}
#marginal_predictions <- ggemmeans(m4.1.4, terms = c("GPAQ_PAG20083", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2","Receipt_of_Prenatal_Care","income","AGE_diff"))

#plot(marginal_predictions) + 
  #labs(title = "Population-Average Marginal Effects Over Time",
       #x = "Physical Activity", 
       #y = "Predicted Gestational Age") +
  #theme_minimal()

#3>1 (1 is the reference group)
#3>2 (2 is the reference group)
#3>4 (3 is the reference group)

# mutate(Tobacco_Use = if_else(TBEA3 == "S", "3", TBEA3)) %>% 
```

```{r}
##Aim4.1.1: Longitudinal Association between Physical Activity (GPAQ_PAG2008) on birth complications

wb <- createWorkbook()
addWorksheet(wb,"Aim4")

vars <- c("Birth_Complications_Cat", "PAG2008", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "income")
Final_Data_with_weights_no_Missing_Indp_Birth <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m4.1.1 <- geeglm(Birth_Complications_Cat ~ PAG2008_2 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m4.1.1)
SummaryAim4.1.1 <-tidy(m4.1.1)
kable(SummaryAim4.1.1)

writeFormula(wb, "Aim4", "Aim4.1.1 Birth_Complications_Cat", xy = c(1,3))
writeData(wb,"Aim4", as.data.frame(SummaryAim4.1.1), rowNames=TRUE, colNames=TRUE, xy = c(1,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

##Aim4.1.2: Longitudinal Association between Physical Activity (GPAQ_PAG2008) on Delivery Type

vars <- c("PCE4_Delivery_Type", "PAG2008_2", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m4.1.2 <- geeglm(PCE4_Delivery_Type ~ PAG2008_2 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, family = binomial(link = "logit"), data = Final_Data_with_weights_no_Missing, id = PID, corstr = "exchangeable")
summary(m4.1.2)
SummaryAim4.1.2 <-tidy(m4.1.2, conf.int = TRUE)
kable(SummaryAim4.1.2)

writeFormula(wb, "Aim4", "Aim4.1.2 Delivery Type", xy = c(10,3))
writeData(wb,"Aim4", as.data.frame(SummaryAim4.1.2), rowNames=TRUE, colNames=TRUE, xy = c(10,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

##Aim4.1.3: Longitudinal Association between Physical Activity (GPAQ_PAG2008) on Birth Weight

vars <- c("PCE7C_G","PAG2008", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_bw <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m4.1.3 <- geeglm(PCE7C_G ~ PAG2008 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_bw, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m4.1.3)
SummaryAim4.1.3 <-tidy(m4.1.3, conf.int = TRUE)
kable(SummaryAim4.1.3)

writeFormula(wb, "Aim4", "Aim4.1.3 Birthweight", xy = c(20,3))
writeData(wb,"Aim4", as.data.frame(SummaryAim4.1.3), rowNames=TRUE, colNames=TRUE, xy = c(20,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#3>4 (3 is ref)

#marginal_predictions1 <- ggemmeans(m4.1.3, terms = c("GPAQ_PAG20083", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2","Receipt_of_Prenatal_Care","income","AGE_diff"))

#plot(marginal_predictions1) + 
  #labs(title = "Population-Average Marginal Effects Over Time",
       #x = "Physical Activity", 
       #y = "Predicted birth weight") +
  #theme_minimal()

vars <- c("PCE2A_WEEKS","PAG2008", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff")
Final_Data_with_weights_no_Missing_ges_age <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]
##Aim4.1.4: Longitudinal Association between Physical Activity (GPAQ_PAG2008) on Gestational Age

m4.1.4 <- glmgee(PCE2A_WEEKS ~ PAG2008 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_ges_age, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m4.1.4)
SummaryAim4.1.4 <-tidy(m4.1.4)
kable(SummaryAim4.1.4)
writeFormula(wb, "Aim4", "Aim4.1.4 Gestational Age", xy = c(30,3))
writeData(wb,"Aim4", as.data.frame(SummaryAim4.1.4), rowNames=TRUE, colNames=TRUE, xy = c(30,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

saveWorkbook(wb,"/Users/lijeannecheam/Desktop/Aim4_DietRefgroup1.xlsx", overwrite = TRUE)
```

```{r}
#marginal_predictions <- ggemmeans(m4.1.4, terms = c("GPAQ_PAG20083", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2","Receipt_of_Prenatal_Care","income","AGE_diff"))

#plot(marginal_predictions) + 
  #labs(title = "Population-Average Marginal Effects Over Time",
       #x = "Physical Activity", 
       #y = "Predicted Gestational Age") +
  #theme_minimal()

#3>1 (1 is the reference group)
#3>2 (2 is the reference group)
#3>4 (3 is the reference group)

# mutate(Tobacco_Use = if_else(TBEA3 == "S", "3", TBEA3)) %>% 
```

```{r}
wb <- createWorkbook()
addWorksheet(wb,"Aim5")

vars <- c("Birth_Complications_Cat","AHEI2010","Average_sleep_Real", "Depression_Baseline", "WHIIRS_Real", "Average_sleep_Real_num_hours", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2","Receipt_of_Prenatal_Care","income","AGE_diff", "income", "GPAQ_PAG20083")
Final_Data_with_weights_no_Missing_Indp_Birth <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.1 = glmgee(Birth_Complications_Cat ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real_num_hours + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.1)
tidy(m5.1.1, exponentiate = TRUE)

m5.1.1.a = glmgee(Birth_Complications_Cat ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real_2 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.1.a)

Step_Result1 <- stepCriterion(m5.1.1, direction="backward", criterion="p-value", test="score", scope=list(lower=~1, upper=~AHEI2010 + WHIIRS_Real + Average_sleep_Real_num_hours + GPAQ_PAG20083 + Depression_Baseline + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

Step_Result1.a <- stepCriterion(m5.1.1.a, direction="backward", criterion="p-value", test="score", scope=list(lower=~1, upper=~AHEI2010 + WHIIRS_Real + GPAQ_PAG20083 +  Depression_Baseline + Average_sleep_Real_2 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_2)

response_var <- deparse(formula(m5.1.1)[[2]])

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result1$final))

# 3. Refit model using the winning formula
final_modelA <- update(m5.1.1, formula = final_formula)

# 4. Extract coefficient summary table
coef_table1 <- as.data.frame(coef(summary(final_modelA)))

sum_final <- summary(final_modelA)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table1 <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table1 <- parsimonious_table1[parsimonious_table1$Predictor != "(Intercept)", ]
kable(parsimonious_table1)

writeFormula(wb, "Aim5", "Aim5.1.1 Birth Complications", xy = c(1,3))
writeData(wb,"Aim5", parsimonious_table1, rowNames=TRUE, colNames=TRUE, xy = c(1,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

response_var.a <- deparse(formula(m5.1.1.a)[[2]])

# 2. Combine response variable with final selected predictors
final_formula.a <- as.formula(paste(response_var.a, Step_Result1.a$final))

# 3. Refit model using the winning formula
final_model.a <- update(m5.1.1.a, formula = final_formula.a)

# 4. Extract coefficient summary table
coef_table1.a <- as.data.frame(coef(summary(final_model.a)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model.a)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table1a <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table1a <- parsimonious_table1a[parsimonious_table1a$Predictor != "(Intercept)", ]
kable(parsimonious_table1a)

writeFormula(wb, "Aim5", "Aim5.1.1 Birth Complications", xy = c(1,16))
writeData(wb,"Aim5", parsimonious_table1a, rowNames=TRUE, colNames=TRUE, xy = c(1,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE4_Delivery_Type","AHEI2010",
          "Average_sleep_Real","GPAQ_PAG20083", "Depression_Baseline", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "WHIIRS_Real", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_Indp_Deliv <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.2 = glmgee(PCE4_Delivery_Type ~ AHEI2010 + Average_sleep_Real_2 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv,  family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.2)

m5.1.2.a = glmgee(PCE4_Delivery_Type ~ AHEI2010 + Average_sleep_Real_num_hours + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv,  family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.2.a)
B <- tidy(m5.1.2.a)
kable(B)

Step_Result2 <- stepCriterion(m5.1.2, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + Average_sleep_Real_2 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_2)

Step_Result2.a <- stepCriterion(m5.1.2.a, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + Average_sleep_Real_num_hours + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

response_var <- deparse(formula(m5.1.2)[[2]])

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result2$final))

# 3. Refit model using the winning formula
final_model <- update(m5.1.2, formula = final_formula)

# 4. Extract coefficient summary table
coef_table2 <- as.data.frame(coef(summary(final_model)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table2a <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table2a <- parsimonious_table2a[parsimonious_table2a$Predictor != "(Intercept)", ]
kable(parsimonious_table2a)

writeFormula(wb, "Aim5", "Aim5.1.2 Delivery Type", xy = c(10,3))
writeData(wb,"Aim5", parsimonious_table2a, rowNames=TRUE, colNames=TRUE, xy = c(10,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

response_var2 <- deparse(formula(m5.1.2.a)[[2]])

# 2. Combine response variable with final selected predictors
final_formula2 <- as.formula(paste(response_var2, Step_Result2.a$final))

# 3. Refit model using the winning formula
final_model2 <- update(m5.1.2.a, formula = final_formula2)

# 4. Extract coefficient summary table
coef_table2.a <- as.data.frame(coef(summary(final_model2)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model2)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table2 <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se,3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table2 <- parsimonious_table2[parsimonious_table2$Predictor != "(Intercept)", ]
kable(parsimonious_table2)

writeFormula(wb, "Aim5", "Aim5.1.2 Delivery Type", xy = c(10,16))
writeData(wb,"Aim5", parsimonious_table2, rowNames=TRUE, colNames=TRUE, xy = c(10,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE7C_G","AHEI2010",
          "Average_sleep_Real","GPAQ_PAG20083", "Depression_Baseline", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "WHIIRS_Real", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_bw <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.3.a = glmgee(PCE7C_G ~ AHEI2010 + Average_sleep_Real_2 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_bw, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.3.a)

Step_Result3.a <- stepCriterion(m5.1.3.a, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + Average_sleep_Real_2 + GPAQ_PAG20083 + as.numeric(Depression_Baseline) + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real), levels=c(0.10,0.10), force.in = ~Average_sleep_Real_2)

gvif(geeglm(PCE7C_G ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_bw, family = gaussian(link = "identity"), id = PID, corstr = "exchangeable"), type = "predictor")

response_var3a <- deparse(formula(m5.1.3.a)[[2]])

# 2. Combine response variable with final selected predictors
final_formula3a <- as.formula(paste(response_var3a, Step_Result3.a$final))

# 3. Refit model using the winning formula
final_model3a <- update(m5.1.3.a, formula = final_formula3a)

# 4. Extract coefficient summary table
coef_table3.a <- as.data.frame(coef(summary(final_model3a)))
#####

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model3a)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table3a <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table3a <- parsimonious_table3a[parsimonious_table3a$Predictor != "(Intercept)", ]

# View your beautifully formatted table
print(parsimonious_table3a)
######

writeFormula(wb, "Aim5", "Aim5.1.3 Birth Weight", xy = c(20,3))
writeData(wb,"Aim5", parsimonious_table3a, rowNames=TRUE, colNames=TRUE, xy = c(20,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

m5.1.3.b = glmgee(PCE7C_G ~ AHEI2010 + Average_sleep_Real_num_hours + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_bw, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.3.b)

Step_Result3.b <- stepCriterion(m5.1.3.b, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + GPAQ_PAG20083 + as.numeric(Depression_Baseline) + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real + Average_sleep_Real_num_hours), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

response_var3b <- deparse(formula(m5.1.3.b)[[2]])

# 2. Combine response variable with final selected predictors
final_formula3b <- as.formula(paste(response_var3b, Step_Result3.b$final))

# 3. Refit model using the winning formula
final_model3b <- update(m5.1.3.b, formula = final_formula3b)

# 4. Extract coefficient summary table
coef_table3b <- as.data.frame(coef(summary(final_model3b)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model3b)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table3b <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table3b <- parsimonious_table3b[parsimonious_table3b$Predictor != "(Intercept)", ]
kable(parsimonious_table3b)

writeFormula(wb, "Aim5", "Aim5.1.3 Birth Weight", xy = c(20,16))
writeData(wb,"Aim5", parsimonious_table3b, rowNames=TRUE, colNames=TRUE, xy = c(20,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE2A_WEEKS",
          "Average_sleep_Real","GPAQ_PAG20083","Depression_Baseline", "BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "AGE", "AHEI2010", "WHIIRS_Real", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_ges_age <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.4 <- glmgee(PCE2A_WEEKS ~ AHEI2010 + Average_sleep_Real_2 + GPAQ_PAG20083 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_ges_age, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.4)

Step_Result4 <- stepCriterion(m5.1.4, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~ Average_sleep_Real_2 + GPAQ_PAG20083 + AHEI2010 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real), levels=c(0.10,0.05))

response_var <- deparse(formula(m5.1.4)[[2]])

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result4$final))

# 3. Refit model using the winning formula
final_model <- update(m5.1.4, formula = final_formula)

# 4. Extract coefficient summary table
coef_table4 <- as.data.frame(coef(summary(final_model)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table4 <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table4 <- parsimonious_table4[parsimonious_table4$Predictor != "(Intercept)", ]
kable(parsimonious_table4)

writeFormula(wb, "Aim5", "Aim5.1.4 Gestational Age", xy = c(30,3))
writeData(wb,"Aim5", parsimonious_table4, rowNames=TRUE, colNames=TRUE, xy = c(30,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

m5.1.4.b <- glmgee(PCE2A_WEEKS ~ AHEI2010 + Average_sleep_Real_num_hours + GPAQ_PAG20083 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_ges_age, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.4.b)

Step_Result4.b <- stepCriterion(m5.1.4.b, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~ GPAQ_PAG20083 + AHEI2010 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real + Average_sleep_Real_num_hours), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

response_var <- deparse(formula(m5.1.4.b)[[2]])

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result4.b$final))

# 3. Refit model using the winning formula
final_model <- update(m5.1.4.b, formula = final_formula)

# 4. Extract coefficient summary table
coef_table4.b <- as.data.frame(coef(summary(final_model)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table4.b <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table4.b <- parsimonious_table4.b[parsimonious_table4.b$Predictor != "(Intercept)", ]
kable(parsimonious_table4.b)

writeFormula(wb, "Aim5", "Aim5.1.4 Gestational Age", xy = c(30,16))
writeData(wb,"Aim5", parsimonious_table4.b, rowNames=TRUE, colNames=TRUE, xy = c(30,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

saveWorkbook(wb,"/Users/lijeannecheam/Desktop/Aim5(Ref6-9hours).xlsx", overwrite = TRUE)
```

```{r}
wb <- createWorkbook()
addWorksheet(wb,"Aim5")

vars <- c("Birth_Complications_Cat","AHEI2010","Average_sleep_Real", "Depression_Baseline", "WHIIRS_Real", "Average_sleep_Real_num_hours", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2","Receipt_of_Prenatal_Care","income","AGE_diff", "income", "PAG2008")
Final_Data_with_weights_no_Missing_Indp_Birth <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.1 = glmgee(Birth_Complications_Cat ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real_num_hours + PAG2008 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.1)
tidy(m5.1.1, exponentiate = TRUE)

m5.1.1.a = glmgee(Birth_Complications_Cat ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real_2 + PAG2008_2 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.1.a)

gvif(geeglm(Birth_Complications_Cat ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real + PAG2008 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth, family = binomial(link = "logit"), id = PID, corstr = "exchangeable"),type="predictor")

Step_Result1 <- stepCriterion(m5.1.1, direction="backward", criterion="p-value", test="score", scope=list(lower=~1, upper=~AHEI2010 + WHIIRS_Real + Average_sleep_Real_num_hours + PAG2008_2 + Depression_Baseline + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

Step_Result1.a <- stepCriterion(m5.1.1.a, direction="backward", criterion="p-value", test="score", scope=list(lower=~1, upper=~AHEI2010 + WHIIRS_Real + PAG2008_2 +  Depression_Baseline + Average_sleep_Real_2 + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_2)

response_var <- deparse(formula(m5.1.1)[[2]])

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result1$final))

# 3. Refit model using the winning formula
final_modelA <- update(m5.1.1, formula = final_formula)

# 4. Extract coefficient summary table
coef_table1 <- as.data.frame(coef(summary(final_modelA)))

sum_final <- summary(final_modelA)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table1 <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 5), ", ", round(upper_ci, 5), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table1 <- parsimonious_table1[parsimonious_table1$Predictor != "(Intercept)", ]
kable(parsimonious_table1)

writeFormula(wb, "Aim5", "Aim5.1.1 Birth Complications", xy = c(1,3))
writeData(wb,"Aim5", parsimonious_table1, rowNames=TRUE, colNames=TRUE, xy = c(1,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

response_var.a <- deparse(formula(m5.1.1.a)[[2]])

# 2. Combine response variable with final selected predictors
final_formula.a <- as.formula(paste(response_var.a, Step_Result1.a$final))

# 3. Refit model using the winning formula
final_model.a <- update(m5.1.1.a, formula = final_formula.a)

# 4. Extract coefficient summary table
coef_table1.a <- as.data.frame(coef(summary(final_model.a)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model.a)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table1a <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 5), ", ", round(upper_ci, 5), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table1a <- parsimonious_table1a[parsimonious_table1a$Predictor != "(Intercept)", ]
kable(parsimonious_table1a)

writeFormula(wb, "Aim5", "Aim5.1.1 Birth Complications", xy = c(1,16))
writeData(wb,"Aim5", parsimonious_table1a, rowNames=TRUE, colNames=TRUE, xy = c(1,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE4_Delivery_Type","AHEI2010",
          "Average_sleep_Real","PAG2008", "Depression_Baseline", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "WHIIRS_Real", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_Indp_Deliv <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.2 = glmgee(PCE4_Delivery_Type ~ AHEI2010 + Average_sleep_Real_2 + PAG2008 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv,  family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.2)

m5.1.2.a = glmgee(PCE4_Delivery_Type ~ AHEI2010 + Average_sleep_Real_num_hours + PAG2008_2 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv,  family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.2.a)
B <- tidy(m5.1.2.a)
kable(B)

Step_Result2 <- stepCriterion(m5.1.2, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + Average_sleep_Real + PAG2008 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv), levels=c(0.10,0.05), force.in = ~Average_sleep_Real)

Step_Result2.a <- stepCriterion(m5.1.2.a, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + Average_sleep_Real_num_hours + PAG2008 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

response_var <- deparse(formula(m5.1.2)[[2]])

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result2$final))

# 3. Refit model using the winning formula
final_model <- update(m5.1.2, formula = final_formula)

# 4. Extract coefficient summary table
coef_table2 <- as.data.frame(coef(summary(final_model)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table2a <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 5), ", ", round(upper_ci, 5), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table2a <- parsimonious_table2a[parsimonious_table2a$Predictor != "(Intercept)", ]
kable(parsimonious_table2a)

writeFormula(wb, "Aim5", "Aim5.1.2 Delivery Type", xy = c(10,3))
writeData(wb,"Aim5", parsimonious_table2a, rowNames=TRUE, colNames=TRUE, xy = c(10,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

response_var2 <- deparse(formula(m5.1.2.a)[[2]])

# 2. Combine response variable with final selected predictors
final_formula2 <- as.formula(paste(response_var2, Step_Result2.a$final))

# 3. Refit model using the winning formula
final_model2 <- update(m5.1.2.a, formula = final_formula2)

# 4. Extract coefficient summary table
coef_table2.a <- as.data.frame(coef(summary(final_model2)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model2)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table2 <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se,3),
  CI_95      = paste0("[", round(lower_ci, 5), ", ", round(upper_ci, 5), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table2 <- parsimonious_table2[parsimonious_table2$Predictor != "(Intercept)", ]
kable(parsimonious_table2)

writeFormula(wb, "Aim5", "Aim5.1.2 Delivery Type", xy = c(10,16))
writeData(wb,"Aim5", parsimonious_table2, rowNames=TRUE, colNames=TRUE, xy = c(10,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE7C_G","AHEI2010",
          "Average_sleep_Real","PAG2008", "Depression_Baseline", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "WHIIRS_Real", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_bw <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.3.a = glmgee(PCE7C_G ~ AHEI2010 + Average_sleep_Real_2 + PAG2008_2 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_bw, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.3.a)

Step_Result3.a <- stepCriterion(m5.1.3.a, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + Average_sleep_Real_2 + PAG2008_2 + as.numeric(Depression_Baseline) + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real), levels=c(0.10,0.10), force.in = ~Average_sleep_Real_2)

gvif(geeglm(PCE7C_G ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real + PAG2008 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_bw, family = gaussian(link = "identity"), id = PID, corstr = "exchangeable"), type = "predictor")

response_var3a <- deparse(formula(m5.1.3.a)[[2]])

# 2. Combine response variable with final selected predictors
final_formula3a <- as.formula(paste(response_var3a, Step_Result3.a$final))

# 3. Refit model using the winning formula
final_model3a <- update(m5.1.3.a, formula = final_formula3a)

# 4. Extract coefficient summary table
coef_table3.a <- as.data.frame(coef(summary(final_model3a)))
#####

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model3a)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table3a <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 5), ", ", round(upper_ci, 5), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table3a <- parsimonious_table3a[parsimonious_table3a$Predictor != "(Intercept)", ]

# View your beautifully formatted table
print(parsimonious_table3a)
######

writeFormula(wb, "Aim5", "Aim5.1.3 Birth Weight", xy = c(20,3))
writeData(wb,"Aim5", parsimonious_table3a, rowNames=TRUE, colNames=TRUE, xy = c(20,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

m5.1.3.b = glmgee(PCE7C_G ~ AHEI2010 + Average_sleep_Real_num_hours + PAG2008_2 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_bw, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.3.b)

Step_Result3.b <- stepCriterion(m5.1.3.b, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + PAG2008_2 + as.numeric(Depression_Baseline) + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real + Average_sleep_Real_num_hours), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

response_var3b <- deparse(formula(m5.1.3.b)[[2]])

# 2. Combine response variable with final selected predictors
final_formula3b <- as.formula(paste(response_var3b, Step_Result3.b$final))

# 3. Refit model using the winning formula
final_model3b <- update(m5.1.3.b, formula = final_formula3b)

# 4. Extract coefficient summary table
coef_table3b <- as.data.frame(coef(summary(final_model3b)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model3b)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table3b <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 5), ", ", round(upper_ci, 5), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table3b <- parsimonious_table3b[parsimonious_table3b$Predictor != "(Intercept)", ]
kable(parsimonious_table3b)

writeFormula(wb, "Aim5", "Aim5.1.3 Birth Weight", xy = c(20,16))
writeData(wb,"Aim5", parsimonious_table3b, rowNames=TRUE, colNames=TRUE, xy = c(20,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE2A_WEEKS",
          "Average_sleep_Real","PAG2008","Depression_Baseline", "BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "AGE", "AHEI2010", "WHIIRS_Real", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_ges_age <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.4 <- glmgee(PCE2A_WEEKS ~ AHEI2010 + Average_sleep_Real_2 + PAG2008_2 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_ges_age, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.4)

Step_Result4 <- stepCriterion(m5.1.4, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~ PAG2008_2 + AHEI2010 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_2)

response_var <- deparse(formula(m5.1.4)[[2]])

Step_Result3.a <- stepCriterion(m5.1.3.a, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + Average_sleep_Real_2 + PAG2008_2 + as.numeric(Depression_Baseline) + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real), levels=c(0.10,0.10), force.in = ~Average_sleep_Real_2)


# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result4$final))

# 3. Refit model using the winning formula
final_model <- update(m5.1.4, formula = final_formula)

# 4. Extract coefficient summary table
coef_table4 <- as.data.frame(coef(summary(final_model)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table4 <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 5), ", ", round(upper_ci, 5), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table4 <- parsimonious_table4[parsimonious_table4$Predictor != "(Intercept)", ]
kable(parsimonious_table4)

writeFormula(wb, "Aim5", "Aim5.1.4 Gestational Age", xy = c(30,3))
writeData(wb,"Aim5", parsimonious_table4, rowNames=TRUE, colNames=TRUE, xy = c(30,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

m5.1.4.b <- glmgee(PCE2A_WEEKS ~ AHEI2010 + Average_sleep_Real_num_hours + PAG2008_2 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_ges_age, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.4.b)

Step_Result4.b <- stepCriterion(m5.1.4.b, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~ PAG2008_2 + AHEI2010 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real + Average_sleep_Real_num_hours), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)
summary(Step_Result4.b)
response_var <- deparse(formula(m5.1.4.b)[[2]])

Step_Result4 <- stepCriterion(m5.1.4, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~ Average_sleep_Real_2 + GPAQ_PAG20083 + PAG2008_2 + AHEI2010 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real), levels=c(0.10,0.05), force.in = ~ Average_sleep_Real_2)
summary(Step_Result4)

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result4.b$final))

# 3. Refit model using the winning formula
final_model <- update(m5.1.4.b, formula = final_formula)

# 4. Extract coefficient summary table
coef_table4.b <- as.data.frame(coef(summary(final_model)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table4.b <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.Error  = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 5), ", ", round(upper_ci, 5), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table4.b <- parsimonious_table4.b[parsimonious_table4.b$Predictor != "(Intercept)", ]
kable(parsimonious_table4.b)

writeFormula(wb, "Aim5", "Aim5.1.4 Gestational Age", xy = c(30,16))
writeData(wb,"Aim5", parsimonious_table4.b, rowNames=TRUE, colNames=TRUE, xy = c(30,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

saveWorkbook(wb,"/Users/lijeannecheam/Desktop/Aim5(Ref6-9hours).xlsx", overwrite = TRUE)
```

```{r}
#install.packages("car")
library(car)
###Stepwise regression for IVs + Covariates (Sleep + Diet + Depression + Exercise (Actical) + all other control variables), Group 1 for sleep <6 hours as reference group

wb <- createWorkbook()
addWorksheet(wb,"Aim5")

vars <- c("Birth_Complications_Cat","AHEI2010","Average_sleep_Real", "Depression_Baseline", "WHIIRS_Real", "Average_sleep_Real_num_hours", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2","Receipt_of_Prenatal_Care","income","AGE_diff", "income", "PAG2008", "GPAQ_PAG20083")
Final_Data_with_weights_no_Missing_Indp_Birth <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.1 = glmgee(Birth_Complications_Cat ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real_num_hours + PAG2008 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + GPAQ_PAG20083 + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.1)
tidy(m5.1.1, exponentiate = TRUE)

m5.1.1.a = glmgee(Birth_Complications_Cat ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real_2 + PAG2008_2 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.1.a)

gvif(geeglm(Birth_Complications_Cat ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real + PAG2008 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth, family = binomial(link = "logit"), id = PID, corstr = "exchangeable"),type="predictor")

Step_Result1 <- stepCriterion(m5.1.1, direction="backward", criterion="p-value", test="score", scope=list(lower=~1, upper=~AHEI2010 + WHIIRS_Real + Average_sleep_Real_num_hours + PAG2008_2 + GPAQ_PAG20083 + Depression_Baseline + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

Step_Result1.a <- stepCriterion(m5.1.1.a, direction="backward", criterion="p-value", test="score", scope=list(lower=~1, upper=~AHEI2010 + WHIIRS_Real + PAG2008_2 +  Depression_Baseline + Average_sleep_Real_2 + AGE + BMI + Num_preg_prior_V1 + GPAQ_PAG20083 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_Indp_Birth), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_2)

response_var <- deparse(formula(m5.1.1)[[2]])

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result1$final))

# 3. Refit model using the winning formula
final_modelA <- update(m5.1.1, formula = final_formula)

# 4. Extract coefficient summary table
coef_table1 <- as.data.frame(coef(summary(final_model)))

sum_final <- summary(final_modelA)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table1 <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.E      = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table1 <- parsimonious_table1[parsimonious_table1$Predictor != "(Intercept)", ]
kable(parsimonious_table1)

writeFormula(wb, "Aim5", "Aim5.1.1 Birth Complications", xy = c(1,3))
writeData(wb,"Aim5", parsimonious_table1, rowNames=TRUE, colNames=TRUE, xy = c(1,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

response_var.a <- deparse(formula(m5.1.1.a)[[2]])

# 2. Combine response variable with final selected predictors
final_formula.a <- as.formula(paste(response_var.a, Step_Result1.a$final))

# 3. Refit model using the winning formula
final_model.a <- update(m5.1.1.a, formula = final_formula.a)

# 4. Extract coefficient summary table
coef_table1.a <- as.data.frame(coef(summary(final_model.a)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model.a)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table1a <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.E      = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table1a <- parsimonious_table1a[parsimonious_table1a$Predictor != "(Intercept)", ]
kable(parsimonious_table1a)

writeFormula(wb, "Aim5", "Aim5.1.1 Birth Complications", xy = c(1,16))
writeData(wb,"Aim5", parsimonious_table1a, rowNames=TRUE, colNames=TRUE, xy = c(1,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE4_Delivery_Type","AHEI2010",
          "Average_sleep_Real","PAG2008","GPAQ_PAG20083", "Depression_Baseline", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "WHIIRS_Real", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_Indp_Deliv <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.2 = glmgee(PCE4_Delivery_Type ~ AHEI2010 + Average_sleep_Real_2 + PAG2008_2 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv,  family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.2)

m5.1.2.a = glmgee(PCE4_Delivery_Type ~ AHEI2010 + Average_sleep_Real_num_hours + PAG2008_2 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv,  family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(m5.1.2.a)
B <- tidy(m5.1.2.a)
kable(B)

Step_Result2 <- stepCriterion(m5.1.2, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + Average_sleep_Real_2 + PAG2008_2 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_2)

Step_Result2.a <- stepCriterion(m5.1.2.a, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + Average_sleep_Real_num_hours + GPAQ_PAG20083 + PAG2008_2 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_Indp_Deliv), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

response_var <- deparse(formula(m5.1.2)[[2]])

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result2$final))

# 3. Refit model using the winning formula
final_model <- update(m5.1.2, formula = final_formula)

# 4. Extract coefficient summary table
coef_table2 <- as.data.frame(coef(summary(final_model)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table2a <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.E      = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table2a <- parsimonious_table2a[parsimonious_table2a$Predictor != "(Intercept)", ]
kable(parsimonious_table2a)

writeFormula(wb, "Aim5", "Aim5.1.2 Delivery Type", xy = c(10,3))
writeData(wb,"Aim5", parsimonious_table2a, rowNames=TRUE, colNames=TRUE, xy = c(10,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

response_var2 <- deparse(formula(m5.1.2.a)[[2]])

# 2. Combine response variable with final selected predictors
final_formula2 <- as.formula(paste(response_var2, Step_Result2.a$final))

# 3. Refit model using the winning formula
final_model2 <- update(m5.1.2.a, formula = final_formula2)

# 4. Extract coefficient summary table
coef_table2.a <- as.data.frame(coef(summary(final_model2)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model2)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table2 <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.E      = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table2 <- parsimonious_table2[parsimonious_table2$Predictor != "(Intercept)", ]
kable(parsimonious_table2)

writeFormula(wb, "Aim5", "Aim5.1.2 Delivery Type", xy = c(10,16))
writeData(wb,"Aim5", parsimonious_table2, rowNames=TRUE, colNames=TRUE, xy = c(10,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE7C_G","AHEI2010",
          "Average_sleep_Real","PAG2008", "GPAQ_PAG20083", "Depression_Baseline", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "WHIIRS_Real", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_bw <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.3.a = glmgee(PCE7C_G ~ AHEI2010 + Average_sleep_Real_2 + PAG2008_2 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_bw, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.3.a)

Step_Result3.a <- stepCriterion(m5.1.3.a, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + Average_sleep_Real_2 + PAG2008_2 + GPAQ_PAG20083 + as.numeric(Depression_Baseline) + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_2)

gvif(geeglm(PCE7C_G ~ AHEI2010 + WHIIRS_Real + Average_sleep_Real + PAG2008 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data = Final_Data_with_weights_no_Missing_bw, family = gaussian(link = "identity"), id = PID, corstr = "exchangeable"), type = "predictor")

response_var3a <- deparse(formula(m5.1.3.a)[[2]])

# 2. Combine response variable with final selected predictors
final_formula3a <- as.formula(paste(response_var3a, Step_Result3.a$final))

# 3. Refit model using the winning formula
final_model3a <- update(m5.1.3.a, formula = final_formula3a)

# 4. Extract coefficient summary table
coef_table3.a <- as.data.frame(coef(summary(final_model3a)))
#####

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model3a)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table3a <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.E      = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table3a <- parsimonious_table3a[parsimonious_table3a$Predictor != "(Intercept)", ]

# View your beautifully formatted table
print(parsimonious_table3a)
######

writeFormula(wb, "Aim5", "Aim5.1.3 Birth Weight", xy = c(20,3))
writeData(wb,"Aim5", parsimonious_table3a, rowNames=TRUE, colNames=TRUE, xy = c(20,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

m5.1.3.b = glmgee(PCE7C_G ~ AHEI2010 + Average_sleep_Real_num_hours + PAG2008_2 + GPAQ_PAG20083 + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_bw, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.3.b)

Step_Result3.b <- stepCriterion(m5.1.3.b, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~AHEI2010 + PAG2008_2 + GPAQ_PAG20083 + as.numeric(Depression_Baseline) + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + WHIIRS_Real + Average_sleep_Real_num_hours), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

response_var3b <- deparse(formula(m5.1.3.b)[[2]])

# 2. Combine response variable with final selected predictors
final_formula3b <- as.formula(paste(response_var3b, Step_Result3.b$final))

# 3. Refit model using the winning formula
final_model3b <- update(m5.1.3.b, formula = final_formula3b)

# 4. Extract coefficient summary table
coef_table3b <- as.data.frame(coef(summary(final_model3b)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model3b)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table3b <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.E      = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table3b <- parsimonious_table3b[parsimonious_table3b$Predictor != "(Intercept)", ]
kable(parsimonious_table3b)

writeFormula(wb, "Aim5", "Aim5.1.3 Birth Weight", xy = c(20,16))
writeData(wb,"Aim5", parsimonious_table3b, rowNames=TRUE, colNames=TRUE, xy = c(20,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE2A_WEEKS",
          "Average_sleep_Real","PAG2008", "GPAQ_PAG20083", "Depression_Baseline", "BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "AGE", "AHEI2010", "WHIIRS_Real", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_ges_age <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

m5.1.4 <- glmgee(PCE2A_WEEKS ~ AHEI2010 + Average_sleep_Real_2 + PAG2008_2 + GPAQ_PAG20083 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_ges_age, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.4)

Step_Result4 <- stepCriterion(m5.1.4, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~ Average_sleep_Real_2 + GPAQ_PAG20083 + PAG2008_2 + AHEI2010 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real), levels=c(0.10,0.05), force.in = ~ Average_sleep_Real_2)

response_var <- deparse(formula(m5.1.4)[[2]])

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result4$final))

# 3. Refit model using the winning formula
final_model <- update(m5.1.4, formula = final_formula)

# 4. Extract coefficient summary table
coef_table4 <- as.data.frame(coef(summary(final_model)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table4 <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.E      = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table4 <- parsimonious_table4[parsimonious_table4$Predictor != "(Intercept)", ]
kable(parsimonious_table4)

writeFormula(wb, "Aim5", "Aim5.1.4 Gestational Age", xy = c(30,3))
writeData(wb,"Aim5", parsimonious_table4, rowNames=TRUE, colNames=TRUE, xy = c(30,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

m5.1.4.b <- glmgee(PCE2A_WEEKS ~ AHEI2010 + Average_sleep_Real_num_hours + PAG2008_2 + GPAQ_PAG20083 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real, data = Final_Data_with_weights_no_Missing_ges_age, id = PID, family = gaussian(link = "identity"), corstr = "exchangeable")
summary(m5.1.4.b)

Step_Result4.b <- stepCriterion(m5.1.4.b, direction="backward", criterion="p-value", test="wald", scope=list(lower=~1, upper=~ PAG2008_2 + GPAQ_PAG20083 + AHEI2010 + Depression_Baseline + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff + AGE + WHIIRS_Real + Average_sleep_Real_num_hours), levels=c(0.10,0.05), force.in = ~Average_sleep_Real_num_hours)

response_var <- deparse(formula(m5.1.4.b)[[2]])

# 2. Combine response variable with final selected predictors
final_formula <- as.formula(paste(response_var, Step_Result4.b$final))

# 3. Refit model using the winning formula
final_model <- update(m5.1.4.b, formula = final_formula)

# 4. Extract coefficient summary table
coef_table4.b <- as.data.frame(coef(summary(final_model)))

# 1. Get the summary table containing robust standard errors
sum_final <- summary(final_model)
coef_raw  <- as.data.frame(sum_final$coefficients)
print(coef_raw)

# 2. Extract estimates and calculate 95% Confidence Intervals
# Note: For GEE, we use the Robust/Empirical SE to build the CI
est       <- coef_raw[, "Estimate"]
robust_se <- coef_raw[, "Std.Error"]
lower_ci  <- est - (1.96 * robust_se)
upper_ci  <- est + (1.96 * robust_se)
p_vals    <- coef_raw[, "Pr(>|z|)"]

# 3. Build the clean, parsimonious dataframe
parsimonious_table4.b <- data.frame(
  Predictor  = rownames(coef_raw),
  Beta       = round(est, 3),
  Std.E      = round(robust_se, 3),
  CI_95      = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
  P_Value    = ifelse(p_vals < 0.001, "< .001", sprintf("%.3f", p_vals))
)

# Remove the Intercept row since journals rarely require it
parsimonious_table4.b <- parsimonious_table4.b[parsimonious_table4.b$Predictor != "(Intercept)", ]
kable(parsimonious_table4.b)

writeFormula(wb, "Aim5", "Aim5.1.4 Gestational Age", xy = c(30,16))
writeData(wb,"Aim5", parsimonious_table4.b, rowNames=TRUE, colNames=TRUE, xy = c(30,17), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

saveWorkbook(wb,"/Users/lijeannecheam/Desktop/Aim5(Ref6-9hours).xlsx", overwrite = TRUE)

```

```{r}
wb <- createWorkbook()
addWorksheet(wb,"Aim2")

vars <- c("Birth_Complications_Cat", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.1 <- glmgee (Birth_Complications_Cat ~ Average_sleep_Real_num_hours + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(Aim_2.1.1)
SummaryAim2.1.1 <-tidy(Aim_2.1.1, conf.int = TRUE)
kable(SummaryAim2.1.1)

writeFormula(wb, "Aim2", "m2.1.1 Birth Complications", xy = c(1,3))
writeData(wb,"Aim2", SummaryAim2.1.1, rowNames=TRUE, colNames=TRUE, xy = c(1,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Longitudinal Association between depression at visit 1 and delivery type
vars <- c("PCE4_Delivery_Type", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.2 <- glmgee (PCE4_Delivery_Type~ Depression_Baseline + Average_sleep_Real_num_hours + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
SummaryAim2.1.2 <-tidy(Aim_2.1.2, conf.int = TRUE)
summary(Aim_2.1.2)

writeFormula(wb, "Aim2", "m2.1.2 Delivery Type", xy = c(10,3))
writeData(wb,"Aim2", SummaryAim2.1.2, rowNames=TRUE, colNames=TRUE, xy = c(10,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Longitudinal Association between depression at visit 1 and Gestational age

vars <- c("PCE7C_G", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "Average_sleep_Real_num_hours")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.3 <- glmgee (PCE7C_G ~ Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Average_sleep_Real_num_hours + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, id = PID, corstr = "exchangeable")
summary(Aim_2.1.3)
SummaryAim2.1.3 <-tidy(Aim_2.1.3, conf.int = TRUE)

writeFormula(wb, "Aim2", "m2.1.3 Birth Weight", xy = c(20,3))
writeData(wb,"Aim2", SummaryAim2.1.3, rowNames=TRUE, colNames=TRUE, xy = c(20,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE2A_WEEKS", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2", "Receipt_of_Prenatal_Care","income","AGE_diff", "Average_sleep_Real_num_hours", "Depression_Baseline")

Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.4 <- glmgee (PCE2A_WEEKS ~ Depression_Baseline + Average_sleep_Real_num_hours + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data= Final_Data_with_weights_no_Missing_sleep, id = PID, corstr = "exchangeable")
summary(Aim_2.1.4)
SummaryAim2.1.4 <-tidy(Aim_2.1.4, conf.int = TRUE)

writeFormula(wb, "Aim2", "Aim_2.1.4 Gesational Age", xy = c(30,3))
writeData(wb,"Aim2", SummaryAim2.1.4, rowNames=TRUE, colNames=TRUE, xy = c(30,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

saveWorkbook(wb,"/Users/lijeannecheam/Desktop/Supplement.xlsx", overwrite = TRUE)
```

```{r}
wb <- createWorkbook()
addWorksheet(wb,"Aim2")

vars <- c("Birth_Complications_Cat", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "Average_sleep_Real")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.1 <- glmgee (Birth_Complications_Cat ~ Average_sleep_Real + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(Aim_2.1.1)
SummaryAim2.1.1 <-tidy(Aim_2.1.1, conf.int = TRUE)
kable(SummaryAim2.1.1)

writeFormula(wb, "Aim2", "m2.1.1 Birth Complications", xy = c(1,3))
writeData(wb,"Aim2", SummaryAim2.1.1, rowNames=TRUE, colNames=TRUE, xy = c(1,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Longitudinal Association between depression at visit 1 and delivery type
vars <- c("PCE4_Delivery_Type", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "Average_sleep_Real")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.2 <- glmgee (PCE4_Delivery_Type~ Depression_Baseline + Average_sleep_Real + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
SummaryAim2.1.2 <-tidy(Aim_2.1.2, conf.int = TRUE)
summary(Aim_2.1.2)

writeFormula(wb, "Aim2", "m2.1.2 Delivery Type", xy = c(10,3))
writeData(wb,"Aim2", SummaryAim2.1.2, rowNames=TRUE, colNames=TRUE, xy = c(10,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Longitudinal Association between depression at visit 1 and Gestational age

vars <- c("PCE7C_G", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "Average_sleep_Real")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.3 <- glmgee (PCE7C_G ~ Depression_Baseline + Average_sleep_Real + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, id = PID, corstr = "exchangeable")
summary(Aim_2.1.3)
SummaryAim2.1.3 <-tidy(Aim_2.1.3, conf.int = TRUE)

writeFormula(wb, "Aim2", "m2.1.3 Birth Weight", xy = c(20,3))
writeData(wb,"Aim2", SummaryAim2.1.3, rowNames=TRUE, colNames=TRUE, xy = c(20,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE2A_WEEKS", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2", "Receipt_of_Prenatal_Care","income","AGE_diff", "Average_sleep_Real_num_hours", "Depression_Baseline")

Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.4 <- glmgee (PCE2A_WEEKS ~ Depression_Baseline + Average_sleep_Real + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data= Final_Data_with_weights_no_Missing_sleep, id = PID, corstr = "exchangeable")
summary(Aim_2.1.4)
SummaryAim2.1.4 <-tidy(Aim_2.1.4, conf.int = TRUE)

writeFormula(wb, "Aim2", "Aim_2.1.4 Gesational Age", xy = c(30,3))
writeData(wb,"Aim2", SummaryAim2.1.4, rowNames=TRUE, colNames=TRUE, xy = c(30,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

saveWorkbook(wb,"/Users/lijeannecheam/Desktop/Supplement2.xlsx", overwrite = TRUE)
```

```{r}
wb <- createWorkbook()
addWorksheet(wb,"Aim2")

vars <- c("Birth_Complications_Cat", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "WHIIRS_Real")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.1 <- glmgee (Birth_Complications_Cat ~ WHIIRS_Real + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
summary(Aim_2.1.1)
SummaryAim2.1.1 <-tidy(Aim_2.1.1, conf.int = TRUE)
kable(SummaryAim2.1.1)

writeFormula(wb, "Aim2", "m2.1.1 Birth Complications", xy = c(1,3))
writeData(wb,"Aim2", SummaryAim2.1.1, rowNames=TRUE, colNames=TRUE, xy = c(1,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Longitudinal Association between depression at visit 1 and delivery type
vars <- c("PCE4_Delivery_Type", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "WHIIRS_Real")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.2 <- glmgee (PCE4_Delivery_Type~ Depression_Baseline + WHIIRS_Real + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, family = binomial(link = "logit"), id = PID, corstr = "exchangeable")
SummaryAim2.1.2 <-tidy(Aim_2.1.2, conf.int = TRUE)
summary(Aim_2.1.2)

writeFormula(wb, "Aim2", "m2.1.2 Delivery Type", xy = c(10,3))
writeData(wb,"Aim2", SummaryAim2.1.2, rowNames=TRUE, colNames=TRUE, xy = c(10,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

#Longitudinal Association between depression at visit 1 and Gestational age

vars <- c("PCE7C_G", "Depression_Baseline",
          "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2",
          "Receipt_of_Prenatal_Care","income","AGE_diff", "WHIIRS_Real")
Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.3 <- glmgee (PCE7C_G ~ WHIIRS_Real + Depression_Baseline + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data=Final_Data_with_weights_no_Missing_sleep, id = PID, corstr = "exchangeable")
summary(Aim_2.1.3)
SummaryAim2.1.3 <-tidy(Aim_2.1.3, conf.int = TRUE)

writeFormula(wb, "Aim2", "m2.1.3 Birth Weight", xy = c(20,3))
writeData(wb,"Aim2", SummaryAim2.1.3, rowNames=TRUE, colNames=TRUE, xy = c(20,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

vars <- c("PCE2A_WEEKS", "AGE","BMI","Num_preg_prior_V1","Tobacco_Use_T2", "Receipt_of_Prenatal_Care","income","AGE_diff", "Average_sleep_Real_num_hours", "Depression_Baseline", "WHIIRS_Real")

Final_Data_with_weights_no_Missing_sleep <- Final_Data_with_weights_4[complete.cases(
  Final_Data_with_weights_4[vars]
), ]

Aim_2.1.4 <- glmgee (PCE2A_WEEKS ~ Depression_Baseline + WHIIRS_Real + AGE + BMI + Num_preg_prior_V1 + Tobacco_Use_T2 + Receipt_of_Prenatal_Care + income + AGE_diff, data= Final_Data_with_weights_no_Missing_sleep, id = PID, corstr = "exchangeable")
summary(Aim_2.1.4)
SummaryAim2.1.4 <-tidy(Aim_2.1.4, conf.int = TRUE)

writeFormula(wb, "Aim2", "Aim_2.1.4 Gesational Age", xy = c(30,3))
writeData(wb,"Aim2", SummaryAim2.1.4, rowNames=TRUE, colNames=TRUE, xy = c(30,4), borders = openxlsx_getOp("borders", "all"), borderColour = openxlsx_getOp("borderColour", "black"), borderStyle = openxlsx_getOp("borderStyle", "medium"))

saveWorkbook(wb,"/Users/lijeannecheam/Desktop/Supplement3.xlsx", overwrite = TRUE)
```

```{r}
Final_Data_with_weights_4 %>%
  select(PAG2008YN, PAG2008, GPAQ_PAG2008,AGE, AGE_V2, income, BMI, Tobacco_Use_T2, Num_preg_prior_V1, 
         Depression_Baseline, Receipt_of_Prenatal_Care, PCE4_Delivery_Type, PCE2A_WEEKS, PCE7C_G, 
         Birth_Complications_Cat, Hypertension_Visit_1, Diabetes_Visit_1, Tobacco_Use_1, LiveBirth_orNot2, Infant_Gender, AHEI2010, Average_sleep_Real, Average_sleep_Real_num_hours, WHIIRS_Real, Depression_V2, New_PA) %>%
  tbl_summary(
  by = "Average_sleep_Real",
    label = list(PAG2008YN ~ "Actical (accelerometer) - Met Guidelines",
      PAG2008 ~ "Actical (accelerometer) - 4 Categories",
      GPAQ_PAG2008 ~ "GPAQ (4 Categories)",
      AGE ~ "Maternal age V1",
      AGE_V2 ~ "Maternal age V2",
      income ~ "Household Income",
      BMI ~ "BMI V1",
      Num_preg_prior_V1 ~ "Pregnancies prior to Visit One",
      Depression_Baseline ~ "V1 Maternal Depression",
      PCE2A_WEEKS ~ "Gestational Age",
      PCE7C_G ~ "Infant Weight at Birth (grams)",
      Receipt_of_Prenatal_Care ~ "Receipt of Prenatal Care",
      Hypertension_Visit_1 ~ "Pre-pregnancy Hypertension",
      Diabetes_Visit_1 ~ "Pre-pregnancy Diabetes",
      Tobacco_Use_1 ~ "Smoking V1",
      Birth_Complications_Cat ~ "Pregnancy Complications",
      Tobacco_Use_T2 ~ "Smoking V2",
      Infant_Gender ~ "Infant Gender",
      PCE4_Delivery_Type ~ "Delivery Type",
      LiveBirth_orNot2 ~ "Fetal Demise (2)",
      AHEI2010 ~ "Dietary Quality (Alternative Healthy Eating Index (AHE2010))",
      Average_sleep_Real_num_hours ~ "Average Sleep (Weekdays and Weekends)",
      WHIIRS_Real ~ "Insomnia Symptoms (Self Reported)",
      New_PA ~ "GPAQ - Met Guidelines"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = list(
    all_continuous() ~ 2,
    all_categorical() ~ c(0, 0)# 0 for count, 2 for percentage
  ),
    type = list(
      AGE ~ "continuous",
      AGE_V2 ~ "continuous",
      income ~ "categorical",
      BMI ~ "continuous",
      Num_preg_prior_V1 ~ "continuous",
      Depression_Baseline ~ "continuous",
      Depression_V2 ~ "continuous",
      Receipt_of_Prenatal_Care ~ "categorical",
      PCE4_Delivery_Type ~ "categorical",
      PCE2A_WEEKS ~ "continuous",
      PCE7C_G ~ "continuous",
      Hypertension_Visit_1 ~ "categorical",
      Diabetes_Visit_1 ~ "categorical",
      Tobacco_Use_1 ~ "categorical",
      Birth_Complications_Cat ~ "categorical",
      Tobacco_Use_T2 ~ "categorical",
      Infant_Gender ~ "categorical",
      LiveBirth_orNot2 ~ "categorical",
      AHEI2010 ~ "continuous",
      WHIIRS_Real ~ "continuous",
      PAG2008YN ~ "categorical",
      PAG2008 ~ "categorical",
      GPAQ_PAG2008 ~ "categorical",
      Average_sleep_Real_num_hours ~ "continuous",
      New_PA ~ "categorical"
    )
  ) %>%
  add_p(test.args = list(PAG2008YN ~ list(simulate.p.value = TRUE), GPAQ_PAG2008 ~ list(simulate.p.value = TRUE), PAG2008 ~ list(simulate.p.value = TRUE)), pvalue_fun = ~style_pvalue(., digits = 3),
    test = list(
      AGE ~ "aov",
      AGE_V2 ~ "aov",
      BMI ~ "aov",
      PCE2A_WEEKS ~ "aov",
      PCE7C_G ~ "aov",
      Depression_V2 ~ "aov",
      Depression_Baseline ~ "aov",
      Num_preg_prior_V1 ~ "aov",
      Hypertension_Visit_1 ~ "fisher.test",
     Diabetes_Visit_1 ~ "fisher.test",
      Tobacco_Use_1 ~ "fisher.test",
      income ~ "fisher.test",
      Tobacco_Use_T2 ~ "fisher.test",
      Receipt_of_Prenatal_Care ~ "fisher.test",
      PCE4_Delivery_Type ~ "fisher.test",
      LiveBirth_orNot2 ~ "fisher.test",
      PAG2008YN ~ "chisq.test",
      PAG2008 ~ "chisq.test",
     GPAQ_PAG2008 ~ "chisq.test",
      Average_sleep_Real_num_hours ~ "aov",
      WHIIRS_Real ~ "aov",
      AHEI2010 ~ "aov",
      Infant_Gender ~ "fisher.test",
     New_PA ~ "chisq.test"
    )) %>% 
   modify_header(statistic ~ "**Test Statistic**") %>%
  modify_fmt_fun(statistic ~ style_sigfig) %>% 
  as_gt() %>%
  tab_header(
   title = md("**Descriptives**"),
    subtitle = md("*All comparisons stratified by Sleep Duration (3 Categories)*"))
```

```{r}
Final_Data_with_Weights_Deduplicated %>%
  select(PAG2008YN, PAG2008, GPAQ_PAG2008,AGE, AGE_V2, income, BMI, Tobacco_Use_T2, Num_preg_prior_V1, 
         Depression_Baseline, Receipt_of_Prenatal_Care, PCE4_Delivery_Type, PCE2A_WEEKS, PCE7C_G, 
         Birth_Complications_Cat, Hypertension_Visit_1, Diabetes_Visit_1, Tobacco_Use_1, LiveBirth_orNot2, Infant_Gender, AHEI2010, Average_sleep_Real, Average_sleep_Real_num_hours, WHIIRS_Real, Depression_V2) %>%
  tbl_summary(
  by = "Average_sleep_Real",
    label = list(PAG2008YN ~ "Actical (accelerometer) - Met Guidelines",
      PAG2008 ~ "Actical (accelerometer) - 4 Categories",
      GPAQ_PAG2008 ~ "GPAQ (4 Categories)",
      AGE ~ "Maternal age V1",
      AGE_V2 ~ "Maternal age V2",
      income ~ "Household Income",
      BMI ~ "BMI V1",
      Num_preg_prior_V1 ~ "Pregnancies prior to Visit One",
      Depression_Baseline ~ "V1 Maternal Depression",
      PCE2A_WEEKS ~ "Gestational Age",
      PCE7C_G ~ "Infant Weight at Birth (grams)",
      Receipt_of_Prenatal_Care ~ "Receipt of Prenatal Care",
      Hypertension_Visit_1 ~ "Pre-pregnancy Hypertension",
      Diabetes_Visit_1 ~ "Pre-pregnancy Diabetes",
      Tobacco_Use_1 ~ "Smoking V1",
      Birth_Complications_Cat ~ "Pregnancy Complications",
      Tobacco_Use_T2 ~ "Smoking V2",
      Infant_Gender ~ "Infant Gender",
      PCE4_Delivery_Type ~ "Delivery Type",
      LiveBirth_orNot2 ~ "Fetal Demise (2)",
      AHEI2010 ~ "Dietary Quality (Alternative Healthy Eating Index (AHE2010))",
      Average_sleep_Real_num_hours ~ "Average Sleep (Weekdays and Weekends)",
      WHIIRS_Real ~ "Insomnia Symptoms (Self Reported)"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = list(
    all_continuous() ~ 2,
    all_categorical() ~ c(0, 0)# 0 for count, 2 for percentage
  ),
    type = list(
      AGE ~ "continuous",
      AGE_V2 ~ "continuous",
      income ~ "categorical",
      BMI ~ "continuous",
      Num_preg_prior_V1 ~ "continuous",
      Depression_Baseline ~ "continuous",
      Depression_V2 ~ "continuous",
      Receipt_of_Prenatal_Care ~ "categorical",
      PCE4_Delivery_Type ~ "categorical",
      PCE2A_WEEKS ~ "continuous",
      PCE7C_G ~ "continuous",
      Hypertension_Visit_1 ~ "categorical",
      Diabetes_Visit_1 ~ "categorical",
      Tobacco_Use_1 ~ "categorical",
      Birth_Complications_Cat ~ "categorical",
      Tobacco_Use_T2 ~ "categorical",
      Infant_Gender ~ "categorical",
      LiveBirth_orNot2 ~ "categorical",
      AHEI2010 ~ "continuous",
      WHIIRS_Real ~ "continuous",
      PAG2008YN ~ "categorical",
      PAG2008 ~ "categorical",
      GPAQ_PAG2008 ~ "categorical",
      Average_sleep_Real_num_hours ~ "continuous"
    )
  ) %>%
  add_p(test.args = list(PAG2008YN ~ list(simulate.p.value = TRUE), GPAQ_PAG2008 ~ list(simulate.p.value = TRUE), PAG2008 ~ list(simulate.p.value = TRUE)), pvalue_fun = ~style_pvalue(., digits = 3),
    test = list(
      AGE ~ "aov",
      AGE_V2 ~ "aov",
      BMI ~ "aov",
      PCE2A_WEEKS ~ "aov",
      PCE7C_G ~ "aov",
      Depression_V2 ~ "aov",
      Depression_Baseline ~ "aov",
      Num_preg_prior_V1 ~ "aov",
      Hypertension_Visit_1 ~ "fisher.test",
     Diabetes_Visit_1 ~ "fisher.test",
      Tobacco_Use_1 ~ "fisher.test",
      income ~ "fisher.test",
      Tobacco_Use_T2 ~ "fisher.test",
      Receipt_of_Prenatal_Care ~ "fisher.test",
      PCE4_Delivery_Type ~ "fisher.test",
      LiveBirth_orNot2 ~ "fisher.test",
      PAG2008YN ~ "chisq.test",
      PAG2008 ~ "chisq.test",
     GPAQ_PAG2008 ~ "chisq.test",
      Average_sleep_Real_num_hours ~ "aov",
      WHIIRS_Real ~ "aov",
      AHEI2010 ~ "aov",
      Infant_Gender ~ "fisher.test"
    )) %>% 
   modify_header(statistic ~ "**Test Statistic**") %>%
  modify_fmt_fun(statistic ~ style_sigfig) %>% 
  as_gt() %>%
  tab_header(
   title = md("**Descriptives: Relationship between Sleep Duration (Categorical) and Health/Behavioral Characteristcis**"),
    subtitle = md("*All comparisons stratified by Sleep Duration (3 Categories)*"))
```

```{r}
Final_Data_with_weights_5 <- Final_Data_with_weights_4 %>% 
  filter(LiveBirth_orNot2 == "LiveBirth")
```

```{r}
Final_Data_with_weights_5 %>%
  select(Receipt_of_Prenatal_Care, PCE4_Delivery_Type, PCE2A_WEEKS, PCE7C_G, 
         Birth_Complications_Cat, LiveBirth_orNot2, Infant_Gender, low_birthweight, low_GA, Average_sleep_Real) %>%
  tbl_summary(
  by = "Average_sleep_Real",
    label = list(
      PCE2A_WEEKS ~ "Gestational Age",
      PCE7C_G ~ "Infant Weight at Birth (grams)",
      Receipt_of_Prenatal_Care ~ "Receipt of Prenatal Care",
      Infant_Gender ~ "Infant Gender",
      PCE4_Delivery_Type ~ "Delivery Type",
      LiveBirth_orNot2 ~ "Fetal Demise (2)",
      Birth_Complications_Cat ~ "Birth Complications",
      low_birthweight ~ "Low Birth Weight (categorical)",
      low_GA ~ "Low Gestational Age (categorical)"
    ),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = list(
    all_continuous() ~ 2,
    all_categorical() ~ c(0, 0)# 0 for count, 2 for percentage
  ),
    type = list(
      Receipt_of_Prenatal_Care ~ "categorical",
      PCE4_Delivery_Type ~ "categorical",
      PCE2A_WEEKS ~ "continuous",
      PCE7C_G ~ "continuous",
      Infant_Gender ~ "categorical",
      LiveBirth_orNot2 ~ "categorical",
      low_birthweight ~ "categorical",
      low_GA ~ "categorical",
      Birth_Complications_Cat ~ "categorical"
    )
  ) %>%
  add_p(pvalue_fun = ~style_pvalue(., digits = 3),
    test = list(
      PCE2A_WEEKS ~ "aov",
      PCE7C_G ~ "aov",
      Receipt_of_Prenatal_Care ~ "fisher.test",
      PCE4_Delivery_Type ~ "fisher.test",
      LiveBirth_orNot2 ~ "fisher.test",
      Infant_Gender ~ "fisher.test",
      low_birthweight ~ "chisq.test",
      low_GA ~ "chisq.test",
      Birth_Complications_Cat ~ "chisq.test"
    )) %>% 
   modify_header(statistic ~ "**Test Statistic**") %>%
  modify_fmt_fun(statistic ~ style_sigfig) %>% 
  as_gt() %>%
  tab_header(
   title = md("**Descriptives: Relationship between Sleep Duration and Birth/Infant Outcomes**"),
    subtitle = md("*All comparisons stratified by Sleep Duration (3 Categories)*"))
```
