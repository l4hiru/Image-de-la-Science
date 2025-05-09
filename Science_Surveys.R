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