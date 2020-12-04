#####################################################################################################################################################
# NATURE MEDICINE - "The effects of tobacco control policies on glonal smoking prevalence"
# Code for running lmer models - produce geenral results for both the main model (tobacco control achievement scores) and additional models 
# Revised on: 10/10/2020
# Inputs:   input data file (policy indicators and current smoking prevalence data) and model tracking sheet)
#           input data file is available at `http://ghdx.healthdata.org/record/ihme-data/global-tobacco-control-and-smoking-prevalence-scenarios-2017`  
# Outputs:  models workspace
#
#
# @<<PUBLIC_INPUT_DIR>>: filepath for data available in the public GHDX repository
# @<<REPO_INPUT_DIR>>:  filepath for files available in the public Github repository (`https://github.com/ihmeuw/team/tree/effects_tobacco_policies/extra_input_files/`)
# @<<OUTPUT_DIR>>: Desired output filepath
################# SET UP #############################################################################################################################

rm(list=ls())

##----Packages
library(lme4)
library(lmerTest)
library(data.table)

##----Import input data (smoking prevalence and policy aspects combined)
input_data <- read.csv('<< PUBLIC_INPUT_DIR >>/IHME_SMOKING_TOBACCO_CONTROL_2017_INPUT_DATA_Y2020M12D04.CSV')
input_data <- as.data.table(input_data)

##----Set up lists to store regressions and RMSE
reg_list <- list()
rmse_list <- list()
mname <- NULL

##----Import model tracking sheet
model_list<-read.csv('<< REPO_INPUT_DIR >>/lagged_models_tracker.csv')
head(model_list)
model_list$id <- as.character(model_list$id)

##----Define parameters for looping. First, each age and sex.
## Sex
input_data$sex_name <- as.character(input_data$sex_name)

## Change naming for better file names 
input_data <- input_data[sex_name == "Male", sex_name := "male"]
input_data <- input_data[sex_name == "Female", sex_name := "female"]
input_data <- input_data[sex_name == "Both", sex_name := "both"]

sexes <- unique(input_data$sex_name)

## Age
input_data$age_group_name <- as.character(input_data$age_group_name)

## Change naming for better file names 
input_data <- input_data[age_group_name == "15+ years", age_group_name := "15plus"]
input_data <- input_data[age_group_name == "15 to 29", age_group_name := "15-29"]
input_data <- input_data[age_group_name == "30 to 49", age_group_name := "30-49"]
input_data <- input_data[age_group_name == "50 plus", age_group_name := "50plus"]

ages <- unique(input_data$age_group_name)

##----Running models
for (sex in sexes) {
   for (age in ages) {
    mdata<-subset(input_data, sex_name==sex & age_group_name == age)
    
    ## Now, loop thorugh models (main model for publication == lachieve)
    for(i in 1:nrow(model_list)){
      modelid      <-model_list$id[i]           
      mname        <-c(mname, modelid)
      fma          <-formula(as.character(model_list$equation[model_list$id==modelid]))
      model        <- lmer(fma, data = mdata)
      reg_list <- append(reg_list, model)
      save(list=ls(), file=paste0('<< OUTPUT_DIR >>/',modelid,'_', sex, '_', age,'.RData'))
    }
  }
}
