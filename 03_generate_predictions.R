#####################################################################################################################################################
# NATURE MEDICINE - "The effects of tobacco control policies on glonal smoking prevalence"
# Code for generating predictions from fixed effects  
# Revised on: 10/10/2020
# Input: Main model workspace .RData produced by 01_run_lmer_models.R and saved to << OUTPUT_DIR >> 
# Output: CSV table with predictions and CI, for each scenario for each age-sex model
#   a. Scenario 1: P, W and E at the highest level & status quo RIP (Nature Med paper, reprted as scenario 3)
#   b. Scenario 2: Price = at least 90th pctle and status quo P, W, E (Same as Nature Med paper)
#   c. Scenario 3: Price 90th pctle and P, W, E highest level (Nature Med paper, reported as scenario 4)
#   d. Scenario 4: RIP & PWE == 2009 (Nature Med paper, reported as scenario 1)
#
# @<<REPO_INPUT_DIR>>:  filepath for files available in the public Github repository (`https://github.com/ihmeuw/team/tree/effects_tobacco_policies/extra_input_files/`)
# @<<OUTPUT_DIR>>: filepath to where workspaces were saved and where output csv wil be saved
################# SET UP #############################################################################################################################

rm(list=ls())

##----Packages
library(data.table)
library(reshape)

##----Main model id (can be updatd if generating outputs for additional/altrnative models, or create a loop)
modelid<-'lachieve'

ages <- c("15-29", "30-49", "50plus", "15plus")
sexes <- c("male", "female", "both") 

##----Dataframes to store results
save_global <- data.table()
save_countries <- data.table()

for (age in ages){
  for(sex in sexes){
##----Load best model workspace .RData 
load(file=paste0('<< OUTPUT_DIR >>/',modelid,'_', sex, '_', age,'.RData'))

##----Clean model data for merging and avoid NAs
mdata <- subset(mdata, year=="2009" | year =="2011"| year =="2013" | year =="2015" | year =="2017")
mdata<-mdata[,.(location_id, year, smoking_prev, lag_rip, lag_p_achievement, lag_w_achievement, lag_e_achievement )]
mdata <- na.omit(mdata, cols=c("smoking_prev", "lag_rip", "lag_p_achievement", "lag_w_achievement", "lag_e_achievement"))

##----Create 1,000 draws for fixed effect uncertainty calculation
set.seed(33)
draws <- mvrnorm(1000, fixef(model), vcov(model))

#----Create fixed effect design matrix 
matrix <- t(draws %*% t(model.matrix(model))) # in-sample estimates
matrix <- as.data.table(matrix)

#----Merging
df_fit <- cbind(mdata[,.(location_id, year)], matrix)

#----Melt for later merging and CI
df_fit<- melt(df_fit, id.vars = c("location_id", "year"),
            variable.name = "draw", value.name = "prev")
df_fit$prev <- inv.logit(df_fit$prev)

#----Extra id columns for later merging 
df_fit$sex_name <- sex 
df_fit$age_group_name <- age 
df_fit$scenario <- 0 #Model fit


#################################################################################################
##########      SCENARIO 1: : P, W and E at the highest level & status quo RIP       ############
#################################################################################################

scenario1<-copy(mdata)
scenario1 <- scenario1[, c("lag_p_achievement", "lag_w_achievement", "lag_e_achievement"):=5]

#----Make model matrix for out-of-sample estimation
n <- nrow(scenario1)
model_matrix_oos <- cbind(rep(1, n), as.matrix(scenario1[, colnames(draws)[2:ncol(draws)], with=FALSE]))
colnames(model_matrix_oos)[colnames(model_matrix_oos)=='']<-'(Intercept)'

df_s1 <- t(draws %*% t(model_matrix_oos))
df_s1 <- as.data.table(df_s1)

#----Merging
df_s1 <- cbind(mdata[,.(location_id, year)], df_s1)

#----Melt for later merging and CI
df_s1<- melt(df_s1, id.vars = c("location_id", "year"),
              variable.name = "draw", value.name = "prev")
df_s1$prev <- inv.logit(df_s1$prev)

#----Extra id columns for later merging 
df_s1$sex_name <- sex 
df_s1$age_group_name <- age 
df_s1$scenario <- 1 


#################################################################################################
##########      SCENARIO 2: Price = at least 90th pctle and status quo P, W, E       ############
#################################################################################################

cig_prices <- input_data[,.(location_id, year, lag_cig_price_pack)]
cig_prices <- cig_prices[!is.na(lag_cig_price_pack)]
price <- quantile(cig_prices$lag_cig_price_pack, 0.9) 

#---Now, prepare to recalculate lag_rip with the new suggested price
scenario2 <- copy(mdata)

temp <- input_data[,.(location_id, year, lag_cig_price_pack, lag_gdp)]
temp <- unique(temp)
scenario2 <- merge(scenario2, temp, by= c("location_id", "year"), all.x = TRUE)
setkey(scenario2, "year", "location_id")
rm(temp)

scenario2 <- scenario2[lag_cig_price_pack < price, lag_cig_price_pack := price]

#----RIP = percentage of GDP required to buy half pack of cigarettes per day in a year (182.5 packs/year)
scenario2 <- scenario2[,lag_rip := (((lag_cig_price_pack*182.5)/lag_gdp)*100)]

#----Make model matrix for out-of-sample estimation
n <- nrow(scenario2)
model_matrix_oos <- cbind(rep(1, n), as.matrix(scenario2[, colnames(draws)[2:ncol(draws)], with=FALSE]))
colnames(model_matrix_oos)[colnames(model_matrix_oos)=='']<-'(Intercept)'

df_s2 <- t(draws %*% t(model_matrix_oos))
df_s2 <- as.data.table(df_s2)

#----Merging
df_s2 <- cbind(mdata[,.(location_id, year)], df_s2)

#----Melt for later merging and CI
df_s2<- melt(df_s2, id.vars = c("location_id", "year"),
             variable.name = "draw", value.name = "prev")
df_s2$prev <- inv.logit(df_s2$prev)

#----Extra id columns for later merging 
df_s2$sex_name <- sex 
df_s2$age_group_name <- age 
df_s2$scenario <- 2 


#################################################################################################
##########          SCENARIO 3: Price 90th pctle and P, W, E highest level           ############
#################################################################################################

scenario3<-copy(scenario2)
scenario3<-scenario3[, c("lag_p_achievement", "lag_w_achievement", "lag_e_achievement"):=5]

#----Make model matrix for out-of-sample estimation
n <- nrow(scenario3)
model_matrix_oos <- cbind(rep(1, n), as.matrix(scenario3[, colnames(draws)[2:ncol(draws)], with=FALSE]))
colnames(model_matrix_oos)[colnames(model_matrix_oos)=='']<-'(Intercept)'

df_s3 <- t(draws %*% t(model_matrix_oos))
df_s3 <- as.data.table(df_s3)

#----Merging
df_s3 <- cbind(mdata[,.(location_id, year)], df_s3)

#----Melt for later merging and CI
df_s3<- melt(df_s3, id.vars = c("location_id", "year"),
             variable.name = "draw", value.name = "prev")
df_s3$prev <- inv.logit(df_s3$prev)

#----Extra id columns for later merging 
df_s3$sex_name <- sex 
df_s3$age_group_name <- age 
df_s3$scenario <- 3 


#################################################################################################
##########                      SCENARIO 4: RIP & PWE == 2009                        ############
#################################################################################################

scenario4<-copy(mdata)
scenario4<-as.data.table(scenario4)

#----Get policy indicators values for 2009
temp<-scenario4[year ==2009,]
temp<-temp[,.(location_id, lag_rip, lag_p_achievement, lag_w_achievement, lag_e_achievement)]

#----Prepare to merge back 2009 values to all years
scenario4<-scenario4[,.(location_id, year, smoking_prev)]
scenario4<- merge(scenario4, temp, by="location_id", all.x=TRUE)
setkey(scenario4, "year", "location_id")
rm(temp)

#----Make model matrix for out-of-sample estimation
n <- nrow(scenario4)
model_matrix_oos <- cbind(rep(1, n), as.matrix(scenario4[, colnames(draws)[2:ncol(draws)], with=FALSE]))
colnames(model_matrix_oos)[colnames(model_matrix_oos)=='']<-'(Intercept)'

df_s4 <- t(draws %*% t(model_matrix_oos))
df_s4 <- as.data.table(df_s4)

#----Merging
df_s4 <- cbind(mdata[,.(location_id, year)], df_s4)

#----Melt for later merging and CI
df_s4<- melt(df_s4, id.vars = c("location_id", "year"),
             variable.name = "draw", value.name = "prev")
df_s4$prev <- inv.logit(df_s4$prev)

#----Extra id columns for later merging 
df_s4$sex_name <- sex 
df_s4$age_group_name <- age 
df_s4$scenario <- 4 


#################################################################################################
##########        SUMMARIZING COUTRY & GLOBAL LEVEL RESULTS FOR EACH SCENARIO         ###########
#################################################################################################

#**************************************************************************************
#-NOTE: Given the different number of countries with data available for each scenario
#-and that we will need to avarage countries results to get global estimates, we need 
#-have the same set of countries in each scenario to allow for proper comparison. We    
#-decided to limit our counterfactual analysis to all countries with no missing policy  
#-indicator in 2009 (scenario 3) and 2017 (status quo).
#*************************************************************************************

#----Creating a list of countries we want to summarize include in our global summary values
no_na_2009 <- input_data[year == 2009 & (!is.na(lag_rip) & !is.na(lag_p_achievement) & !is.na(lag_w_achievement) & !is.na(lag_e_achievement)),]
no_na_2009 <- unique(no_na_2009$location_id)

no_na_2017 <- input_data[year == 2017 & (!is.na(lag_rip) & !is.na(lag_p_achievement) & !is.na(lag_w_achievement) & !is.na(lag_e_achievement)),]
no_na_2017 <- unique(no_na_2017$location_id)

pred_countries <- Reduce(intersect, list(no_na_2009, no_na_2017))
length(pred_countries) #155

draws_countries <- rbind(df_fit, df_s1, df_s2, df_s3, df_s4)
draws_countries <- draws_countries[location_id %in% pred_countries]
draws_countries <- draws_countries[year == 2017] #Restricting to prediction year


##############
##  GLOBAL  ##
##############

##---Merge population to compute global population average
draws_global <- copy(draws_countries)
draws_global <- merge(draws_global, input_data[,.(location_id, year, sex_name, age_group_name, population)], by = c("location_id", "year", "sex_name", "age_group_name"))
draws_global <- draws_global[, global_prev := sum(prev*population)/sum(population), by = c("draw", "year", "scenario", "sex_name", "age_group_name")]
draws_global <- draws_global[,.(draw, year, sex_name, age_group_name, scenario, global_prev)]
draws_global <- unique(draws_global)

##Mean, lower, and upper
draws_global<-draws_global[, mean:=mean(global_prev), by = c("year", "scenario")]
draws_global<-draws_global[, lower:=quantile(global_prev, .025), by = c("year", "scenario")]
draws_global<-draws_global[, upper:=quantile(global_prev, .975), by = c("year", "scenario")]

##Keeping unique values
final_global_preds<-unique(draws_global[,.(year, sex_name, age_group_name, scenario, mean, lower, upper)])

##Adding ID variables
final_global_preds <- final_global_preds[age_group_name == "15-29", age_group_id := 195]
final_global_preds <- final_global_preds[age_group_name == "30-49", age_group_id := 212]
final_global_preds <- final_global_preds[age_group_name == "50plus", age_group_id := 40]
final_global_preds <- final_global_preds[age_group_name == "15plus", age_group_id := 29]

final_global_preds <- final_global_preds[sex_name == "male", sex_id := 1]
final_global_preds <- final_global_preds[sex_name == "female", sex_id := 2]
final_global_preds <- final_global_preds[sex_name == "both", sex_id := 3]

final_global_preds<-final_global_preds[,.(year, sex_id, sex_name, age_group_id, age_group_name, scenario, mean, lower, upper)]
final_global_preds<- melt(final_global_preds, id = c("year", "sex_id", "sex_name", "age_group_id", "age_group_name", "scenario"))
final_global_preds<- dcast(final_global_preds, year + sex_id + sex_name + age_group_id + age_group_name + variable ~ scenario, value.var="value")

##----Bring in actual GBD 2017 estimated smoking prevalence to create ratio adjustment 
global_dt<-fread('<< REPO_INPUT_DIR >>/global_155_smoking_prevalence.csv')
global_dt<-global_dt[year == 2017,]
global_dt<-global_dt[sex_name == sex,]
global_dt<-global_dt[age_group_name == age,]
global_prev <- global_dt[,scenario :="gbd_prev"]
global_prev<-global_prev[,.(year, sex_id, sex_name, age_group_id, age_group_name, scenario, mean, lower, upper)]
global_prev<- melt(global_prev, id = c("year", "sex_id", "sex_name", "age_group_id", "age_group_name", "scenario"))
global_prev<- dcast(global_prev, year + sex_id + sex_name + age_group_id + age_group_name + variable ~ scenario, value.var="value")

##----Merge predicted and gbd estimation to compute ratios
predictions_complete <- merge(final_global_preds, global_prev, by=c("year", "sex_id", "sex_name", "age_group_id", "age_group_name", "variable"))
setnames(predictions_complete, "0", "fit")
setnames(predictions_complete, "1", "scenario1")
setnames(predictions_complete, "2", "scenario2")
setnames(predictions_complete, "3", "scenario3")
setnames(predictions_complete, "4", "scenario4")

##Apllying ratio adjustment to gbd estimates 
predictions_complete <- as.data.table(predictions_complete)
predictions_complete <- predictions_complete[, scenario1 := (scenario1/fit)*gbd_prev]
predictions_complete <- predictions_complete[, scenario2 := (scenario2/fit)*gbd_prev]
predictions_complete <- predictions_complete[, scenario3 := (scenario3/fit)*gbd_prev]
predictions_complete <- predictions_complete[, scenario4 := (scenario4/fit)*gbd_prev]

##Melt for formatting
setnames(predictions_complete, "variable", "metric")
pred_temp <- copy(predictions_complete) 
pred_temp$fit <- NULL
pred_temp$gbd_prev <- NULL
pred_temp<- melt(pred_temp, id = c("year", "sex_id", "sex_name", "age_group_id", "age_group_name", "metric"))
setnames(pred_temp, "value", "prev")
pred_temp <- dcast(pred_temp, year + sex_id + sex_name + age_group_id + age_group_name + variable ~ metric , value.var="prev")
setnames(pred_temp, "variable", "scenario")
setnames(pred_temp, "mean", "scenario_mean")
setnames(pred_temp, "lower", "scenario_lower")
setnames(pred_temp, "upper", "scenario_upper")

##Merge GBD prev
prev_temp <- copy(predictions_complete)
prev_temp <- prev_temp[,.(year, sex_id, sex_name, age_group_id, age_group_name, metric, gbd_prev)]
prev_temp <- dcast(prev_temp, year + sex_id + sex_name + age_group_id + age_group_name ~ metric , value.var="gbd_prev")
final_global <- merge(pred_temp, prev_temp, by=c("year", "sex_id", "sex_name", "age_group_id", "age_group_name"))
setnames(final_global, "mean", "gbd_prev_mean")
setnames(final_global, "lower", "gbd_prev_lower")
setnames(final_global, "upper", "gbd_prev_upper")

##----Merge population to calculate number of smokers in each scenario 
final_global <- merge(final_global, global_dt[,.(year, sex_id, sex_name, age_group_id, age_group_name,population)], by =c("year", "sex_id", "sex_name", "age_group_id", "age_group_name"))
setDT(final_global)
final_global <- final_global[, scenario_smokers := scenario_mean*population]
final_global <- final_global[, gbd_smokers := gbd_prev_mean*population]

##--Calculating changes
final_global <- final_global[, abs_prev_change := scenario_mean - gbd_prev_mean]
final_global <- final_global[, rel_prev_change := ((scenario_mean - gbd_prev_mean)/gbd_prev_mean)*100]

final_global <- final_global[, abs_smokers_change := (scenario_smokers - gbd_smokers)/1000000]
final_global <- final_global[, rel_smokers_change := ((scenario_smokers - gbd_smokers)/gbd_smokers)*100]

##Final formatting 
final_global <- final_global[,location_id :=1]
final_global <- final_global[,location_name :="Global"]
final_global <- final_global[,.(location_id, location_name, year, sex_id, sex_name, age_group_id, age_group_name, gbd_prev_mean, gbd_prev_lower, gbd_prev_upper, gbd_smokers,
                                scenario, scenario_mean, scenario_lower, scenario_upper, scenario_smokers, abs_prev_change, rel_prev_change, abs_smokers_change, rel_smokers_change)]
                             
save_global <- rbind(save_global, final_global)

#################
##  COUNTRIES  ##
#################
head(draws_countries)

##Mean, lower, and upper
draws_countries<-draws_countries[, mean:=mean(prev), by = c("year", "location_id", "sex_name", "age_group_name", "scenario")]
draws_countries<-draws_countries[, lower:=quantile(prev, .025), by = c("year", "location_id", "sex_name", "age_group_name", "scenario")]
draws_countries<-draws_countries[, upper:=quantile(prev, .975), by = c("year", "location_id", "sex_name", "age_group_name", "scenario")]

##Keeping unique values
final_country_preds<-unique(draws_countries[,.(location_id, year, sex_name, age_group_name, scenario, mean, lower, upper)])

##Adding ID variables
final_country_preds <- final_country_preds[age_group_name == "15-29", age_group_id := 195]
final_country_preds <- final_country_preds[age_group_name == "30-49", age_group_id := 212]
final_country_preds <- final_country_preds[age_group_name == "50plus", age_group_id := 40]
final_country_preds <- final_country_preds[age_group_name == "15plus", age_group_id := 29]

final_country_preds <- final_country_preds[sex_name == "male", sex_id := 1]
final_country_preds <- final_country_preds[sex_name == "female", sex_id := 2]
final_country_preds <- final_country_preds[sex_name == "both", sex_id := 3]

final_country_preds<-final_country_preds[,.(year, location_id, sex_id, sex_name, age_group_id, age_group_name, scenario, mean, lower, upper)]
final_country_preds<- melt(final_country_preds, id = c("year", "location_id", "sex_id", "sex_name", "age_group_id", "age_group_name", "scenario"))
final_country_preds<- dcast(final_country_preds, year + location_id + sex_id + sex_name + age_group_id + age_group_name + variable ~ scenario, value.var="value")

##----Bring in actual GBD 2017 estimated smoking prevalence to create ratio adjustment 
countries_dt<-input_data[,.(year, location_id, location_name, sex_id, sex_name, age_group_id, age_group_name,smoking_prev, smoking_prev_lower, smoking_prev_upper, population)]
countries_dt<-countries_dt[year == 2017,]
countries_dt<-countries_dt[sex_name == sex,]
countries_dt<-countries_dt[age_group_name == age,]
countries_dt<-countries_dt[location_id %in% pred_countries,]
countries_prev <- countries_dt[,scenario :="gbd_prev"]
setnames(countries_prev, "smoking_prev", "mean")
setnames(countries_prev, "smoking_prev_lower", "lower")
setnames(countries_prev, "smoking_prev_upper", "upper")

countries_prev<-countries_prev[,.(year, location_id, sex_id, sex_name, age_group_id, age_group_name, scenario, mean, lower, upper)]
countries_prev<- melt(countries_prev, id = c("year","location_id", "sex_id", "sex_name", "age_group_id", "age_group_name", "scenario"))
countries_prev<- dcast(countries_prev, year + location_id + sex_id + sex_name + age_group_id + age_group_name + variable ~ scenario, value.var="value")

##----Merge predicted and gbd estimation to compute ratios
complete_countries_preds <- merge(final_country_preds, countries_prev, by=c("year", "location_id", "sex_id", "sex_name", "age_group_id", "age_group_name", "variable"))
setnames(complete_countries_preds, "0", "fit")
setnames(complete_countries_preds, "1", "scenario1")
setnames(complete_countries_preds, "2", "scenario2")
setnames(complete_countries_preds, "3", "scenario3")
setnames(complete_countries_preds, "4", "scenario4")

##Apllying ratio adjustment to gbd estimates 
complete_countries_preds <- as.data.table(complete_countries_preds)
complete_countries_preds <- complete_countries_preds[, scenario1 := (scenario1/fit)*gbd_prev]
complete_countries_preds <- complete_countries_preds[, scenario2 := (scenario2/fit)*gbd_prev]
complete_countries_preds <- complete_countries_preds[, scenario3 := (scenario3/fit)*gbd_prev]
complete_countries_preds <- complete_countries_preds[, scenario4 := (scenario4/fit)*gbd_prev]

##Melt for formatting
setnames(complete_countries_preds, "variable", "metric")
pred_temp <- copy(complete_countries_preds) 
pred_temp$fit <- NULL
pred_temp$gbd_prev <- NULL
pred_temp<- melt(pred_temp, id = c("year","location_id", "sex_id", "sex_name", "age_group_id", "age_group_name", "metric"))
setnames(pred_temp, "value", "prev")
pred_temp <- dcast(pred_temp, year + location_id + sex_id + sex_name + age_group_id + age_group_name + variable ~ metric , value.var="prev")
setnames(pred_temp, "variable", "scenario")
setnames(pred_temp, "mean", "scenario_mean")
setnames(pred_temp, "lower", "scenario_lower")
setnames(pred_temp, "upper", "scenario_upper")

##Merge GBD prev
prev_temp <- copy(complete_countries_preds)
prev_temp <- prev_temp[,.(year, location_id, sex_id, sex_name, age_group_id, age_group_name, metric, gbd_prev)]
prev_temp <- dcast(prev_temp, year + location_id + sex_id + sex_name + age_group_id + age_group_name ~ metric , value.var="gbd_prev")
final_countries <- merge(pred_temp, prev_temp, by=c("year","location_id", "sex_id", "sex_name", "age_group_id", "age_group_name"))
setnames(final_countries, "mean", "gbd_prev_mean")
setnames(final_countries, "lower", "gbd_prev_lower")
setnames(final_countries, "upper", "gbd_prev_upper")

##----Merge population to calculate number of smokers in each scenario 
final_countries <- merge(final_countries, countries_dt[,.(year, location_id, location_name, sex_id, sex_name, age_group_id, age_group_name,population)], by =c("year", "location_id", "sex_id", "sex_name", "age_group_id", "age_group_name"))
setDT(final_countries)
final_countries <- final_countries[, scenario_smokers := scenario_mean*population]
final_countries <- final_countries[, gbd_smokers := gbd_prev_mean*population]

##--Calculating changes
final_countries <- final_countries[, abs_prev_change := scenario_mean - gbd_prev_mean]
final_countries <- final_countries[, rel_prev_change := ((scenario_mean - gbd_prev_mean)/gbd_prev_mean)*100]

final_countries <- final_countries[, abs_smokers_change := (scenario_smokers - gbd_smokers)/1000000]
final_countries <- final_countries[, rel_smokers_change := ((scenario_smokers - gbd_smokers)/gbd_smokers)*100]

##Final formatting 
final_countries <- final_countries[,.(location_id, location_name, year, sex_id, sex_name, age_group_id, age_group_name, gbd_prev_mean, gbd_prev_lower, gbd_prev_upper, gbd_smokers,
                                scenario, scenario_mean, scenario_lower, scenario_upper, scenario_smokers, abs_prev_change, rel_prev_change, abs_smokers_change, rel_smokers_change)]

save_countries <- rbind(save_countries, final_countries)
  }
}

##----Combining glbal and country results 
save_all <- rbind(save_global, save_countries)

##----Change scenarios numbering to match publication
save_all <- save_all[scenario == "scenario4", nature_sce := 1]
save_all <- save_all[scenario == "scenario2", nature_sce := 2]
save_all <- save_all[scenario == "scenario1", nature_sce := 3]
save_all <- save_all[scenario == "scenario3", nature_sce := 4]

save_all$scenario<-NULL
setnames(save_all, "nature_sce", "scenario")

save_all <- save_all[scenario ==1, scenario_name := "no_change"]
save_all <- save_all[scenario ==2, scenario_name := "price_change"]
save_all <- save_all[scenario ==3, scenario_name := "pwe_change"]
save_all <- save_all[scenario ==4, scenario_name := "all_change"]

setkey(save_all, "location_id", "sex_id", "age_group_id", "scenario")

save_all <- save_all[,.(location_id, location_name, year, sex_id, sex_name, age_group_id, age_group_name, gbd_prev_mean, gbd_prev_lower, gbd_prev_upper, gbd_smokers,
                        scenario, scenario_name, scenario_mean, scenario_lower, scenario_upper, scenario_smokers, abs_prev_change, rel_prev_change, abs_smokers_change, rel_smokers_change)]


write.csv(save_all, paste0('<< OUTPUT_DIR >>/IHME_SMOKING_TOBACCO_CONTROL_2017_SCENARIOS.csv'), row.names = F)




