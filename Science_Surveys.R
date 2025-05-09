# Image de la Science - Surveys

#0) Packages 

library(haven)
library(summarytools)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(classInt)
library(janitor)
library(reshape2)
library(plm)
library(stargazer)
library(sandwich)
library(lmtest)
library(tidyr)
library(FactoMineR)
library(explor)
library(factoextra)
library(flexmix)
library(psych)
library(ltm)
library(scales)

#I) Dataset

data_1982 <- read_sas("1982/fr.cdsp.ddi.science1982.sas7bdat")
data_1989 <- read_sas("1989/fr.cdsp.ddi.science1989.sas7bdat")

#II) Outcome variables 

#A) Trust in researchers for the construction of a nuclear power plant

data_1982 <- data_1982 %>%
  mutate(Trust = ifelse(v27 == 0, NA, 5 - v27))

freq(data_1982$Trust)

data_1989 <- data_1989 %>%
  mutate(Trust = ifelse(q25 == 0, NA, 5 - q25))

freq(data_1989$Trust)

#B) Nuclear power plant approval scale 

data_1982 <- data_1982 %>%
  mutate(Approval = ifelse(v23 == 0, NA, v23))

freq(data_1982$Approval)

data_1989 <- data_1989 %>%
  mutate(Approval = ifelse(q21 == 0, NA, q21))

freq(data_1989$Approval)


#C) On the subject of nuclear power plants, do you think it would be better
    # Stop them, Maintain old ones, New ones

data_1982 <- data_1982 %>%
  mutate(Construction = ifelse(v30 == 0, NA, 4 - v30))

freq(data_1982$Construction)

# Missing question for 1989 Survey

#D) Closeness to the environmental movement

data_1982 <- data_1982 %>%
  mutate(Green = ifelse(vs15b == 0, NA, 5 - vs15b))

data_1989 <- data_1989 %>%
  mutate(Green = ifelse(rs34a2 == 0, NA, 5 - rs34a2))


#III) Control variables 

# Gender

data_1982$Women <- ifelse(data_1982$vs1 == 2, 1, 0)
data_1989$Women <- ifelse(data_1989$rs1 == 2, 1, 0)

# Age 

data_1982$Age <- as.numeric(data_1982$vs2)
data_1989$Age <- as.numeric(data_1989$rs2)

# Diploma 

data_1982 <- data_1982 %>%
  mutate(Diploma = case_when(
    vs7 %in% c(1, 6, 13) ~ "Low",
    vs7 %in% c(2, 3, 4, 5, 7) ~ "Medium",
    vs7 %in% c(8, 9, 10, 11, 12) ~ "High",
    TRUE ~ NA_character_
  ),
  Diploma = factor(Diploma, levels = c("Low", "Medium", "High"))) # Un peu trop de High, vérifier classification.

freq(data_1982$Diploma)

data_1989 <- data_1989 %>%
  mutate(Diploma = case_when(
    rs3a1 %in% c(1, 2) ~ "Low",
    rs3a1 %in% c(3, 4, 5) ~ "Medium",
    rs3a1 %in% c(6, 7, 8, 9) ~ "High",
    TRUE ~ NA_character_
  ),
  Diploma = factor(Diploma, levels = c("Low", "Medium", "High")))

freq(data_1989$Diploma)

# Occupation 

data_1982 <- data_1982 %>%
  mutate(Occupation = case_when(
    vs3 %in% c(1, 2) ~ "Farmer",
    vs3 %in% c(3, 4) ~ "Craftmen",
    vs3 %in% c(5, 6) ~ "Executive",
    vs3 %in% c(7, 8) ~ "PI",
    vs3 == 9 ~ "Employee",
    vs3 %in% c(10, 11, 12) ~ "Worker",
    vs3 == 13 ~ "Inactive"
  ),
  PCS = relevel(factor(Occupation), ref = "Worker"))

freq(data_1982$Occupation)

freq(data_1989$rs9a1)

data_1989 <- data_1989 %>%
  mutate(Occupation = case_when(
    rs9a1 %in% c(151) ~ "Farmer",
    rs9a1 %in% c(253, 254, 255) ~ "Craftmen",
    rs9a1 %in% c(356, 357) ~ "Executive",
    rs9a1 %in% c(458, 460) ~ "PI",
    rs9a1 %in% c(559, 563) ~ "Employee",
    rs9a1 %in% c(652, 661, 662) ~ "Worker",
    rs9a1 %in% c(88) ~ "Inactive"
  ),
  PCS = relevel(factor(Occupation), ref = "Worker"))

freq(data_1989$Occupation)

# Income 

freq(data_1982$vs9)

data_1982 <- data_1982 %>%
  mutate(
    Income = case_when(
      vs9 == 15 ~ NA_character_,
      vs9 == 14 ~ "0",
      TRUE ~ as.character(vs9)
    ),
    Income = factor(Income, levels = as.character(0:13)),
    Income = relevel(Income, ref = "1")
  )

freq(data_1982$Income)