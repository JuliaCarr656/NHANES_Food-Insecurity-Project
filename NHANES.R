# ===============================================================
# NHANES Food Insecurity Analysis - Setup
# Author: Julia Carreon-Sanchez
# Purpose: Load required packages and prepare environment for analysis
# ===============================================================

# 1. Install and Load Packages
install.packages("devtools")
library(devtools)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("shiny")
library(shiny)
install.packages("nhanesA")
library(nhanesA)
install.packages("data.table")
library(data.table)
library(stringr)
install.packages("tableone")
library(tableone)
install.packages("tidyr")
library(tidyr)

# ===============================================================
#FUNCTION: import 
# Purpose: Download and combine NHANES datasets for a given year
# Inputs:
#   x = year suffix (for example: 11 for 2011)
#   y = letter code corresponding to the NHANES dataset for that year
# This function imports multiple questionnaires and lab files, then merges them into one dataset.

import<-function(x,y){
    assign(paste0('demo20',x),nhanes(paste0('DEMO_',y)), envir=.GlobalEnv)
    assign(paste0('BloodPQ',x),  nhanes(paste0('BPQ_',y)), envir=.GlobalEnv)
    assign(paste0('FoodSQ',x), nhanes(paste0('FSQ_',y)), envir=.GlobalEnv)
    assign(paste0('DiabetQ',x), nhanes(paste0('DIQ_',y)), envir=.GlobalEnv)
    assign(paste0('InsuraQ',x),nhanes(paste0('HIQ_',y)), envir=.GlobalEnv)
    assign(paste0('SmokinQ',x), nhanes(paste0('SMQ_',y)), envir=.GlobalEnv)
    assign(paste0('PregnaQ',x), nhanes(paste0('RHQ_',y)), envir=.GlobalEnv)
    assign(paste0('BloodPX',x), nhanes(paste0('BPX_',y)), envir=.GlobalEnv)
    assign(paste0('TCholX',x), nhanes(paste0('TCHOL_',y)), envir=.GlobalEnv)
    assign(paste0('GlucosX',x), nhanes(paste0('GLU_',y)),  envir=.GlobalEnv)
    assign(paste0('LDLChol',x), nhanes(paste0('TRIGLY_',y)), envir=.GlobalEnv)
    assign(paste0('HealthStat',x), nhanes(paste0('HSQ_',y)), envir=.GlobalEnv)
    assign(paste0('Exercise',x), nhanes(paste0('PAQ_',y)), envir=.GlobalEnv)
    assign(paste0('BMI',x), nhanes(paste0('BMX_',y)),envir=.GlobalEnv)

    # Merge all datasets by participant ID (SEQN)

    fulldata <- get(paste0('demo20',x)) %>%
    full_join(get(paste0('BMI',x)), by = "SEQN") %>%
    full_join(get(paste0('BloodPQ',x)), by = "SEQN") %>%
    full_join(get(paste0('BloodPX',x)), by = "SEQN") %>%
    full_join(get(paste0('DiabetQ',x)), by = "SEQN") %>%
    full_join(get(paste0('FoodSQ',x)), by = "SEQN") %>%
    full_join(get(paste0('GlucosX',x)), by = "SEQN") %>%
    full_join(get(paste0('InsuraQ',x)), by = "SEQN") %>%
    full_join(get(paste0('LDLChol',x)), by = "SEQN") %>%
    full_join(get(paste0('PregnaQ',x)), by = "SEQN") %>%
    full_join(get(paste0('SmokinQ',x)), by = "SEQN") %>%
    full_join(get(paste0('HealthStat',x)), by = "SEQN") %>%
    full_join(get(paste0('Exercise',x)), by = "SEQN") %>%
    full_join(get(paste0('TCholX',x)), by = "SEQN")
    
    assign(paste0('Afulldata',x), fulldata, envir=.GlobalEnv)
}

# ===============================================================
# FUNCTION: NHANESrename 
# Purpose: Change variable names across all NHANES datasets
# Input:
#   x = year suffix
#   y = dataset to rename
# Output:
#   Saves cleaned and renamed dataset to the global environment as final20<x>

NHANESrename<-function(x,y){
#x is years after 2000
  cooldata <- rename(y, 
                     ID = SEQN, 
                     Gender=RIAGENDR,
                     Age=RIDAGEYR,
                     Race=RIDRETH1,
                     Education=DMDEDUC2,
                     Percent_FPL=INDFMPIR, 
                     Pregnancy=RIDEXPRG, 
                     Doctor_Report_HBP=BPQ020, 
                     Doctor_Report_BChol=BPQ080, 
                     Doctor_Report_Diabetes=DIQ010, 
                     SystolicBP_1=BPXSY1, 
                     SystolicBP_2=BPXSY2,
                     SystolicBP_3=BPXSY3,
                     SystolicBP_4=BPXSY4,
                     DiastolicBP_1=BPXDI1, 
                     DiastolicBP_2=BPXDI2,
                     DiastolicBP_3=BPXDI3,
                     DiastolicBP_4=BPXDI4,
                     Taking_HBP_Meds=BPQ040A, 
                     TCholesterol_mg=LBXTC,
                     TCholesterol_mmol=LBDTCSI,
                     LDLCholesterol_mg=LBDLDL, 
                     LDLCholesterol_mmol=LBDLDLSI,
                     Taking_Chol_Meds=BPQ100D,
                     Glucose_Level_mg=LBXGLU, 
                     Glucose_Level_mmol=LBDGLUSI,
                     Taking_Insulin=DIQ050,
                     Taking_Glucose_Pill=DIQ070,
                     Health_Insurance=HIQ011,
                     Private_Insurance=HIQ031A,
                     Time_WO_Insurance=HIQ210,
                     Parity=RHQ160,
                     BMI=BMXBMI,
                     Smoking=SMQ040,
                     FQ1=FSD032A,
                     FQ2=FSD032B,
                     FQ3=FSD032C,
                     FQ4=FSD041,
                     FQ5=FSD052,
                     FQ6=FSD061,
                     FQ7=FSD071,
                     FQ8=FSD081,
                     FQ9=FSD092,
                     FQ10=FSD102,
                     HealthStatus=HSD010,
                     Work_High=PAQ605, 
                     Work_Med=PAQ620,
                     Self_High=PAQ650, 
                     Self_Med=PAQ665
                     )
  
    
  # Select only variables needed for analysis
  coolerdata<-select(cooldata, c(  ID, 
                                   Gender,
                                   Age,
                                   Race,
                                   Education,
                                   Percent_FPL, 
                                   Pregnancy, 
                                   Doctor_Report_HBP, 
                                   Doctor_Report_BChol, 
                                   Doctor_Report_Diabetes, 
                                   SystolicBP_1, 
                                   SystolicBP_2,
                                   SystolicBP_3,
                                   SystolicBP_4,
                                   DiastolicBP_1, 
                                   DiastolicBP_2,
                                   DiastolicBP_3,
                                   DiastolicBP_4,
                                   Taking_HBP_Meds, 
                                   TCholesterol_mg,
                                   TCholesterol_mmol,
                                   LDLCholesterol_mg, 
                                   LDLCholesterol_mmol,
                                   Taking_Chol_Meds,
                                   Glucose_Level_mg, 
                                   Glucose_Level_mmol,
                                   Taking_Insulin,
                                   Taking_Glucose_Pill,
                                   Health_Insurance,
                                   Private_Insurance,
                                   Time_WO_Insurance,
                                   Parity,
                                   BMI,
                                   Smoking,
                                   FQ1,
                                   FQ2,
                                   FQ3,
                                   FQ4,
                                   FQ5,
                                   FQ6,
                                   FQ7,
                                   FQ8,
                                   FQ9,
                                   FQ10,
                                   HealthStatus,
                                   Work_High,
                                   Work_Med,
                                   Self_High,
                                   Self_Med
                                  
  ))
  
  assign(paste0('final20',x), coolerdata, envir=.GlobalEnv)
  
}

# Download datasets for years 2011, 2013, and 2015 using import function
import(11, "G")
import(13,"H")
import(15, "I")

# Apply NHANESrename function to standardize datasets for 2011, 2013, 2015
NHANESrename(11,Afulldata11)
NHANESrename(13,Afulldata13)
NHANESrename(15,Afulldata15)


# Combine datasets from multiple years into a single dataframe
alldata <-bind_rows(final2011,final2013,final2015)


# Include only non-pregnant adults aged 18-65 and under 200% FPL
alldata <- alldata %>%
  filter(Age>=18 & Age<=65) %>%
  filter(Pregnancy=="The participant was not pregnant at exam"|
           is.na(Pregnancy) | 
           Pregnancy=="Cannot ascertain if the participant is pregnant at exam") %>%
  filter(Percent_FPL<2.00)


# ===============================================================
# FOOD INSECURITY VARIABLES
# Create a score for food insecurity (FSQcount) and a binary indicator (Has_FISQ)
# FSQcount: counts affirmative responses to 10 food security questions
# Has_FISQ: defines food insecurity; low sensitivity = 3+ affirmative responses

alldata <- alldata %>%
rowwise() %>%
  mutate(
    FSQcount = sum(
      c(
        FQ1 %in% c("Often true", "Sometimes true"),
        FQ2 %in% c("Often true", "Sometimes true"),
        FQ3 %in% c("Often true", "Sometimes true"),
        FQ4 == "Yes",
        FQ5 %in% c("Almost every month,", "Some months but not every month, or"),
        FQ6 == "Yes",
        FQ7 == "Yes",
        FQ8 == "Yes",
        FQ9 == "Yes",
        FQ10 %in% c("Almost every month,", "Some months but not every month, or")
      ),
      na.rm = TRUE   # treats N/As like FALSE
      ),
    Has_FISQ = FSQcount >= 3) %>%  #this is low sensitivity, for high: >=6
  ungroup()


# ===============================================================
#CHRONIC ILLNESS VARIABLES 
# Hypertension (HBP), Hyperlipidemia (Cholesterol), Diabetes
# Each condition includes self-reported and clinical measures

#HYPERTENSION 
#Self reported 
alldata <- alldata %>%
  rename(SR_HBP=Doctor_Report_HBP)

#Clinical
alldata <- alldata %>%
  mutate(
    AvgSystolicBP = rowMeans(
    select(., SystolicBP_1, SystolicBP_2, SystolicBP_3, SystolicBP_4),
    na.rm = TRUE     # <- ignores NAs
  )) %>%
  
  mutate(
    AvgDiastolicBP = rowMeans(
      select(., DiastolicBP_1, DiastolicBP_2, DiastolicBP_3, DiastolicBP_4),
      na.rm = TRUE     # <- ignores NAs
    )) %>%
  
  mutate(Clinical_HBP= ifelse(AvgSystolicBP>140|
                                 AvgDiastolicBP>90|
                                 Taking_HBP_Meds=="Yes", TRUE, FALSE)) %>%
  
  mutate(
    Clinical_HBP = replace_na(Clinical_HBP, FALSE)
  )

  
#HYPERLIPIDEMIA 
#Self reported 
alldata <- alldata %>%
  rename(SR_Chol=Doctor_Report_BChol)

#Clinical
alldata <- alldata %>%
  mutate(Clinical_Chol= ifelse(TCholesterol_mg>=240|
                              TCholesterol_mmol>=6.22|
                              LDLCholesterol_mg>=160|
                              LDLCholesterol_mmol>=4.14|
                              Taking_Chol_Meds=="Yes", TRUE, FALSE)) %>%
  
  mutate(
    Clinical_Chol = replace_na(Clinical_Chol, FALSE)
  )


#DIABETES
#Self reported 
alldata <- alldata %>%
  rename(SR_Diabetes=Doctor_Report_Diabetes)

#Clinical
alldata <- alldata %>%
  mutate(Clinical_Diab= ifelse(Glucose_Level_mg>=126|
                               Glucose_Level_mmol>=6.99|
                               Taking_Insulin=="Yes"|
                               Taking_Glucose_Pill=="Yes", TRUE, FALSE)) %>%
  
  mutate(
    Clinical_Diab = replace_na(Clinical_Diab, FALSE)
  )


# Categorize participants by income as % of federal poverty level
alldata <- alldata %>%
  mutate(Income=cut(Percent_FPL, 
                    breaks = c(0, 0.5, 1, 1.3, 2),      
                    labels = c("0-50% of the FPL", ">50-100%", ">100-130%","130-200%"),
                    include.lowest = TRUE))
  


# Categorize participants based on insurance type: Private, Public, or None
alldata <- alldata %>%
  mutate(
    Insurance_Type=case_when(
      Private_Insurance=="Covered by private insurance" ~ "Private",
      Health_Insurance=="Yes"& is.na(Private_Insurance) ~ "Public",
      TRUE ~ "None"
    )
  )


# Convert binary food insecurity to descriptive label
alldata <- alldata %>%
  mutate(
    Has_FISQ=if_else(Has_FISQ, "Food Insecure", "Food Secure" )
  )

# Separate BMI values by gender for analysis or table display
alldata <- alldata %>%
  mutate(
    BMI_men   = ifelse(Gender == "Male", BMI, NA),
    BMI_women = ifelse(Gender == "Female", BMI, NA)
  )

# Occupational physical activity: heavy, moderate, or light
alldata <- alldata %>%
  mutate(
    Occupational_PA=case_when(
      Work_High=="Yes" ~ "Does heavy work or carries heavy loads",
      Work_Med=="Yes" ~ "Lifts light loads, or climbs hills or stairs often",
      TRUE ~ "Stands or walks, but doesn't carry or lift"
    )
  )

# Leisurely physical activity: vigorous, moderate, or light
alldata <- alldata %>%
  mutate(
    Leisurely_PA=case_when(
      Self_High=="Yes" ~ "Vigorous physical activity (>10 min in last 30 d)",
      Self_Med=="Yes" ~ "Moderate physical activity (>10 min in last 30 d)",
      TRUE ~ "Stands or walks, but doesn't carry or lift"
    )
  )


# Convert original HealthStatus variable into 4 levels for analysis
alldata <- alldata %>%
  mutate(HealthStatus=case_when(
    HealthStatus=="Excellent" ~ "Excellent",
    HealthStatus=="Very good," ~ "Very good",
    HealthStatus=="Good," ~ "Good",
    TRUE~"Fair/Poor"
    )
    )


# Group education into 3 levels
alldata <- alldata %>%
  mutate(Education=case_when(
    Education=="Less than 9th grade" | 
      Education == "9-11th grade (Includes 12th grade with no diploma)" ~ "<High school",
    Education=="High school graduate/GED or equivalent" ~ "High school degree/GED",
    Education=="Some college or AA degree"|Education=="College graduate or above" ~ ">High school degree"
  )
  )


  

# ===============================================================
# TABLE CREATION SETUP
# Install and load packages for summary tables

#Table1- Summary Stats 
install.packages("gtsummary")
library(gtsummary)
install.packages("broom")
library(broom)
install.packages("gt")
library(gt)
install.packages("webshot2")
library(webshot2)

tbl1<-alldata %>%
  select(c(Gender,Age,Race,Education,Income,Insurance_Type,
           Parity, BMI_women,BMI_men,Smoking,Occupational_PA,
           Leisurely_PA, HealthStatus,Has_FISQ)) %>%
  tbl_summary(by=Has_FISQ) %>%
  add_p()

#Note: uncomment this to save to desktop
#tbl %>%
  #as_gt() %>%
  #gtsave("~/Desktop/my_table.pdf")



#---------------------------------------------------------------------
#Table 2
install.packages("tidyverse")
library(tidyverse)
install.packages("gt")
library(gt)
install.packages("epitools")
library(epitools)
install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)

#--- Recoding vars to binary  -----------------------------------
# Food insecure = 1, Food secure = 0
alldata <- alldata %>%
  mutate(food_insecure = ifelse(Has_FISQ == "Food Insecure", 1, 0),
          SR_HBP_num = ifelse(SR_HBP == "Yes", 1, 0),
         SR_Chol_num = ifelse(SR_Chol == "Yes", 1, 0),
         SR_Diabetes_num = ifelse(SR_Diabetes == "Yes", 1, 0),
          Clinical_HBP_num = as.numeric(Clinical_HBP),
          Clinical_Chol_num = as.numeric(Clinical_Chol),
          Clinical_Diab_num = as.numeric(Clinical_Diab))

#--- Function to compute prevalence, n, CRR, ARR ------------------------
calc_stats <- function(df, condition, condition_name) {
  
  semi_df <- df %>%
    select(
      outcome = {{condition}}, 
      food_insecure, Age, Gender, Race, Education, Income
    ) %>%
    drop_na()
  
# Prevalence
  prev_secure <- mean(semi_df$outcome[semi_df$food_insecure == 0]) * 100
  prev_insecure <- mean(semi_df$outcome[semi_df$food_insecure == 1]) * 100
  n_secure <- sum(semi_df$food_insecure == 0)
  n_insecure <- sum(semi_df$food_insecure == 1)
  
# CRR
  cr_model <- glm(outcome ~ food_insecure + Age + Gender + Race,
                  family = poisson(link = "log"),
                  data = semi_df)
  cr_coef <- coeftest(cr_model, vcov = sandwich)
  cr_val <- exp(cr_coef["food_insecure", "Estimate"])
  cr_l <- exp(cr_coef["food_insecure", "Estimate"] - 1.96 * cr_coef["food_insecure", "Std. Error"])
  cr_u <- exp(cr_coef["food_insecure", "Estimate"] + 1.96 * cr_coef["food_insecure", "Std. Error"])
  CRR <- paste0(round(cr_val,2), " (", round(cr_l,2), "-", round(cr_u,2), ")")
  
# ARR
  ar_model <- glm(outcome ~ food_insecure + Age + Gender + Race + Education + Income,
                  family = poisson(link = "log"),
                  data = semi_df)
  ar_coef <- coeftest(ar_model, vcov = sandwich)
  ar_val <- exp(ar_coef["food_insecure", "Estimate"])
  ar_l <- exp(ar_coef["food_insecure", "Estimate"] - 1.96 * ar_coef["food_insecure", "Std. Error"])
  ar_u <- exp(ar_coef["food_insecure", "Estimate"] + 1.96 * ar_coef["food_insecure", "Std. Error"])
  ARR <- paste0(round(ar_val,2), " (", round(ar_l,2), "-", round(ar_u,2), ")")
  
  tibble(
    Condition = condition_name,
    Food.Secure = paste0(round(prev_secure,1), "% (n=", n_secure, ")"),
    Food.Insecure = paste0(round(prev_insecure,1), "% (n=", n_insecure, ")"),
    CRR = CRR,
    ARR = ARR
  )
}

#--- Apply function to the 6 conditions -------------------------------
results <- bind_rows(
  calc_stats(alldata, SR_HBP_num, "Hypertension (Self-report)"),
  calc_stats(alldata, Clinical_HBP_num, "Hypertension (Clinical)"),
  calc_stats(alldata, SR_Chol_num, "Hyperlipidemia (Self-report)"),
  calc_stats(alldata, Clinical_Chol_num, "Hyperlipidemia (Clinical)"),
  calc_stats(alldata, SR_Diabetes_num, "Diabetes (Self-report)"),
  calc_stats(alldata, Clinical_Diab_num, "Diabetes (Clinical)")
)

#--- the actual table --------------------------------------------
final_table <- results %>%
  gt() %>%
  cols_label(
    Condition = "Condition",
    Food.Secure = "Food Secure, % (n)",
    Food.Insecure = "Food Insecure, % (n)",
    CRR = "CRR (95% CI)",
    ARR = "ARR (95% CI)"
  ) %>%
  tab_options(
    table.font.size = px(13),
    table.width = pct(100)
  )

results_long <- results %>%
  pivot_longer(
    cols = c(Food.Secure, Food.Insecure, CRR, ARR),  # columns to turn into rows
    names_to = "Assessment of Diagnosis",                                           # new column with old column names
    values_to = "Value"                                            # new column with old values
  )

table2 <- results_long %>%
  pivot_wider(
    names_from = Condition,
    values_from = Value
  )


#run either of these 2 lines of code to see their respective tables.
tbl1

table2 %>%
  gt()


#Cleaning up_____________________________________________________________

# Delete everything but the following dataframes from the Global Environment
keep <- c("alldata", "final_table", "results", "results_long", "results_wide", "tbl1",
          "food_insecure", "calc_stats","import", "NHANESrename") 

rm(list = setdiff(ls(), keep))

#_______________________________________________________________________

#And that's all folks! Thanks for taking a peek at my code.
#-Julia Carreon-Sanchez

