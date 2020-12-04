#####################################################################################################################################################
# NATURE MEDICINE - "The effects of tobacco control policies on glonal smoking prevalence"
# Code for generating main model coefficients and CI for paper table  
# Revised on: 10/10/2020
# Input:  Main model workspace .RData produced by 01_run_lmer_models.R and saved to << OUTPUT_DIR >> 
# Output: CSV table with coefficients, CI, and p value for each age-sex model (paper table 1)
# @<<OUTPUT_DIR>>: filepath to where workspaces were saved and where output csv wil be saved
################# SET UP #############################################################################################################################


rm(list=ls())

##----Packages
library(data.table)
library(reshape)

##----Main model id (can be updated if generating outputs for additional/alternative models)
modelid<-'ladopall'

##----Define parameters for looping. First, each age and sex.
ages <- c("15-29", "30-49", "50plus", "15plus")
sexes <- c("male", "female", "both")

##----DT where to store results
coeff_ci <- data.table()

cols <- c("pval_rip", "pval_pwe")
for (age in ages){
  for(sex in sexes){
    
##----Load best model workspace .RData 
load(file=paste0('<< OUTPUT_DIR >>/',modelid,'_', sex, '_', age,'.RData'))

##----Get fixed effects coef draws & CI
set.seed(33)
draws <- mvrnorm(1000, fixef(model), vcov(model))
drawsdt <- as.data.table(draws)
setnames(drawsdt, "(Intercept)", "int")

##----Intercept
ci<-drawsdt[, mean_int:=mean(int)]
ci<-drawsdt[, lower_int:=quantile((int), .025)]
ci<-drawsdt[, upper_int:=quantile((int), .975)]

##----lag_rip
ci<-drawsdt[, mean_rip:=mean(lag_rip)]
ci<-drawsdt[, lower_rip:=quantile((lag_rip), .025)]
ci<-drawsdt[, upper_rip:=quantile((lag_rip), .975)]

##----lag_p_achievement
ci<-drawsdt[, mean_p:=mean(lag_p_achievement)]
ci<-drawsdt[, lower_p:=quantile((lag_p_achievement), .025)]
ci<-drawsdt[, upper_p:=quantile((lag_p_achievement), .975)]

##----lag_w_achievement
ci<-drawsdt[, mean_w:=mean(lag_w_achievement)]
ci<-drawsdt[, lower_w:=quantile((lag_w_achievement), .025)]
ci<-drawsdt[, upper_w:=quantile((lag_w_achievement), .975)]

##----lag_e_achievement
ci<-drawsdt[, mean_e:=mean(lag_e_achievement)]
ci<-drawsdt[, lower_e:=quantile((lag_e_achievement), .025)]
ci<-drawsdt[, upper_e:=quantile((lag_e_achievement), .975)]

##----p value
p_vals <- summary(model)$coefficients[,5]
p_vals<-data.frame(matrix(as.numeric(p_vals),ncol=5,nrow=1))
cols <- c("pval_int", "pval_rip", "pval_p", "pval_w", "pval_e")
setnames(p_vals, cols)
p_vals<-round(p_vals, digits=3)

##---Cleaning and merging
ci<-ci[,.(mean_rip, lower_rip, upper_rip, mean_p, lower_p, upper_p, mean_w, lower_w, upper_w, mean_e, lower_e, upper_e)]
ci<- ci[!duplicated(ci),]
ci <- ci*100             #Multiplying by 100 to report %change instead of actual coefficient (logit transformed outcome).
ci<-round(ci, digits=1)  #Chagne if want to report more digits

ci <- cbind(ci, p_vals)
ci<-ci[,.(mean_rip, lower_rip, upper_rip, pval_rip, mean_p, lower_p, upper_p, pval_p, mean_w, lower_w, upper_w, pval_w, mean_e, lower_e, upper_e, pval_e)]

ci$sex <- sex
ci$age <-age
coeff_ci <- rbind(coeff_ci, ci)
  }
}

##----Formatting table 
mean = melt(data = coeff_ci,id=c("sex", "age"),measure.vars = c("mean_rip","mean_p", "mean_w", "mean_e"),value.name = "mean",
            variable.name = "policy")
mean$policy <- sub(".*_", "", mean$policy)

lower = melt(data = coeff_ci,id=c("sex", "age"),measure.vars = c("lower_rip","lower_p", "lower_w", "lower_e"),value.name = "lower",
             variable.name = "policy")
lower$policy <- sub(".*_", "", lower$policy)

upper = melt(data = coeff_ci,id=c("sex", "age"),measure.vars = c("upper_rip","upper_p", "upper_w", "mean_e"),value.name = "upper",
             variable.name = "policy")
upper$policy <- sub(".*_", "", upper$policy)

pval = melt(data = coeff_ci,id=c("sex", "age"),measure.vars = c("pval_rip","pval_p", "pval_w", "pval_e"),value.name = "pval",
            variable.name = "policy")
pval$policy <- sub(".*_", "", pval$policy)  

ci_final <- merge(mean, lower, by=c("sex", "age", "policy"))
ci_final <- merge(ci_final, upper, by=c("sex", "age", "policy"))
ci_final <- merge(ci_final, pval, by=c("sex", "age", "policy"))

##Turning come variables into factors for sorting & reporting purposes 
ci_final$sex <- factor(ci_final$sex, levels=c("male", "female", "both"))
ci_final$age <- factor(ci_final$age, levels=c("15plus","15-29", "30-49", "50plus"))

ci_final$policy <- factor(ci_final$policy, levels=c("rip", "p", "w", "e"))

##Sorting
setkey(ci_final, "sex", "age", "policy")
ci_final <- ci_final[, ci_95 := paste0(lower," to ", upper)]
ci_final$pval <-as.character(ci_final$pval)
ci_final <- ci_final[pval == "0", pval := "<0.001"]

##Final cleaning
ci_final<-ci_final[,.(sex, age, policy, mean, ci_95, pval)]
setnames(ci_final, "mean", "change")

write.csv(ci_final, paste0('<< OUTPUT_DIR >>/',modelid,'_coeff_ci.csv'), row.names = F)


