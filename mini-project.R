# load required library
library(tidyverse)
library(patchwork)
library(GGally)
library(ggridges)
library(survival)
library(survminer)

# set working directory
path <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)

# load data
mi_sglt2 <- as_tibble(read.csv("mi_sglt2.csv",header=TRUE))

# explore the basic information of the dataset
colSums(is.na(mi_sglt2))

# EDA
## Table 1
table(mi_sglt2$sex,mi_sglt2$alloc) # sex
mi_sglt2 |> group_by(alloc) |> summarize(mean = mean(age), sd = sd(age)) # age total
mi_sglt2 |> group_by(alloc,sex) |> summarize(mean = mean(age), sd = sd(age)) # age
mi_sglt2 |> group_by(alloc,sex,region) |> summarize(n()) # region
mi_sglt2 |> group_by(alloc) |> summarize(mean = mean(weight_bl), sd = sd(weight_bl)) # weight_bl total
mi_sglt2 |> group_by(alloc,sex) |> summarize(mean = mean(weight_bl), sd = sd(weight_bl)) # weight_bl
mi_sglt2 |> group_by(alloc) |> summarize(mean = mean(height_bl), sd = sd(height_bl)) # height_bl total
mi_sglt2 |> group_by(alloc,sex) |> summarize(mean = mean(height_bl), sd = sd(height_bl)) # height_bl
mi_sglt2 |> group_by(alloc) |> summarize(mean = mean(bmi_bl), sd = sd(bmi_bl)) # bmi_bl total
mi_sglt2 |> group_by(alloc,sex) |> summarize(mean = mean(bmi_bl), sd = sd(bmi_bl)) # bmi_bl
mi_sglt2 |> group_by(alloc) |> summarize(mean = mean(sbp_bl), sd = sd(sbp_bl)) # sbp_bl total
mi_sglt2 |> group_by(alloc,sex) |> summarize(mean = mean(sbp_bl), sd = sd(sbp_bl)) # sbp_bl
mi_sglt2 |> group_by(alloc) |> summarize(mean = mean(weight_fup), sd = sd(weight_fup)) # weight_fup total
mi_sglt2 |> group_by(alloc,sex) |> summarize(mean = mean(weight_fup), sd = sd(weight_fup)) # weight_fup
mi_sglt2 |> group_by(alloc) |> summarize(mean = mean(sbp_fup), sd = sd(sbp_fup)) # sbp_fup total
mi_sglt2 |> group_by(alloc,sex) |> summarize(mean = mean(sbp_fup), sd = sd(sbp_fup)) # sbp_fup


## workflow data
table(mi_sglt2$alloc) # group allocation
table(mi_sglt2$mi_flg) # MI prevelance
table(mi_sglt2$alloc,mi_sglt2$mi_flg) # group, MI prevelance
table(mi_sglt2$alloc,mi_sglt2$adherence_flg) # adherence rate group

########################################################################################################
# Aim 1
## subset groups
mi_sglt2_exp <- subset(mi_sglt2,mi_sglt2$alloc == 'SGLT2') # case group
mi_sglt2_exp_ad <- subset(mi_sglt2_exp,adherence_flg == 1) # case-adherence 813 
mi_sglt2_exp_unad <- subset(mi_sglt2_exp,adherence_flg == 0) # case-unadherence 184

mi_sglt2_placebo <- subset(mi_sglt2,mi_sglt2$alloc == 'Usual Care') # control group
# age
## case group average age
mi_sglt2_exp |> group_by(adherence_flg) |> summarize(mean = mean(age), median = median(age))

## logistic regression，adherence~age
fit_adhere_age = glm(formula = adherence_flg ~ age, 
                     family = binomial(link = "logit"), data = mi_sglt2_exp)
summary(fit_adhere_age)
confint(fit_adhere_age)

## t test: age difference among two groups
#t.test(mi_sglt2_exp_ad$age, mi_sglt2_exp_unad$age, alternative = "two.sided")

## plots age distribution between adhere and unadhere
ad_age_mean = mean(mi_sglt2_exp_ad$age) # [1] 60.05781
unad_age_mean = mean(mi_sglt2_exp_unad$age) # [1] 60.11957
ggplot(mi_sglt2_exp, aes(x = age, color = as.factor(adherence_flg))) +
  geom_density() + 
  scale_color_manual(labels = c('False', 'True'), values = c("blue", "red")) +
  geom_vline(aes(xintercept = unad_age_mean), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(aes(xintercept = ad_age_mean), linetype = "dashed", color = "red", size = 1) +
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(
    #title = "Body mass and flipper length",
    #x = "pre-randomisation BMI", 
    #y = "Body mass (g)",
    color = "Adherence to the Regime"
  )

# sex
## 2by2 table
sex_ad_tab <- table(mi_sglt2_exp$sex,mi_sglt2_exp$adherence_flg)
sex_ad_tab
## chi-square test: adhere sex proportion difference
chisq.test(sex_ad_tab, correct = FALSE)

## 拟合logistic regression，adhere~sex
fit_adhere_sex = glm(formula = adherence_flg ~ sex, 
                     family = binomial(link = "logit"), data = mi_sglt2_exp)
summary(fit_adhere_sex)
confint(fit_adhere_sex)

# BMI baseline
## case group average bmi
mi_sglt2_exp |> group_by(adherence_flg) |> summarize(mean = mean(bmi_bl), median = median(bmi_bl))

## logistic regression，adhere~bmi baseline
fit_adhere_bmibl = glm(formula = adherence_flg ~ bmi_bl, 
                     family = binomial(link = "logit"), data = mi_sglt2_exp)
summary(fit_adhere_bmibl)
confint(fit_adhere_bmibl)

## t test: average bmi difference
#t.test(mi_sglt2_exp_ad$bmi_bl, mi_sglt2_exp_unad$bmi_bl, alternative = "two.sided")

## plots bmi distributions
ad_bmi_bl_mean = mean(mi_sglt2_exp_ad$bmi_bl) # [1] 30.58255
unad_bmi_bl_mean = mean(mi_sglt2_exp_unad$bmi_bl) # [1] 29.65395
ggplot(mi_sglt2_exp, aes(x = bmi_bl, color = as.factor(adherence_flg))) +
  geom_density() + 
  scale_color_manual(labels = c('Not Adhere', 'Adhere'), values = c("blue", "red")) +
  geom_vline(aes(xintercept = unad_bmi_bl_mean), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(aes(xintercept = ad_bmi_bl_mean), linetype = "dashed", color = "red", size = 1) +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 13)) + 
  labs(
    #title = "Adherence to the SGLT2",
    x = "BMI baseline", 
    #y = "Body mass (g)",
    color = ""
  )

# SBP baseline
## case group average sbp
mi_sglt2_exp |> group_by(adherence_flg) |> summarize(mean = mean(sbp_bl), median = median(sbp_bl))

## logistic regression，adhere~sbp baseline
fit_adhere_sbpbl = glm(formula = adherence_flg ~ sbp_bl, 
                       family = binomial(link = "logit"), data = mi_sglt2_exp)
summary(fit_adhere_sbpbl)
confint(fit_adhere_sbpbl)

## plot sbp baseline distribution
ad_sbp_bl_mean = mean(mi_sglt2_exp_ad$sbp_bl) # [1] 60.05781
unad_sbp_bl_mean = mean(mi_sglt2_exp_unad$sbp_bl) # [1] 60.11957
ggplot(mi_sglt2_exp, aes(x = sbp_bl, color = as.factor(adherence_flg))) +
  geom_density() + 
  scale_color_manual(labels = c('False', 'True'), values = c("blue", "red")) +
  geom_vline(aes(xintercept = unad_sbp_bl_mean), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(aes(xintercept = ad_sbp_bl_mean), linetype = "dashed", color = "red", size = 1) +
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(
    #title = "Body mass and flipper length",
    x = "pre-randomisation SBP", 
    #y = "Body mass (g)",
    color = "Adherence to the Regime"
  )


# weight baseline
## logistic regression，adhere~weight baseline
mi_sglt2_exp |> group_by(adherence_flg) |> summarize(mean = mean(weight_bl), median = median(weight_bl))
temp_fit <- glm(formula = adherence_flg ~ weight_bl, 
                       family = binomial(link = "logit"), data = mi_sglt2_exp)
summary(temp_fit)

# height baseline
## logistic regression，adhere~height baseline
mi_sglt2_exp |> group_by(adherence_flg) |> summarize(mean = mean(height_bl), median = median(height_bl))
temp_fit <- glm(formula = adherence_flg ~ height_bl, 
                family = binomial(link = "logit"), data = mi_sglt2_exp)
summary(temp_fit)

## logistic regression，adhere~weight + height baseline
temp_fit <- glm(formula = adherence_flg ~ weight_bl+height_bl, 
                family = binomial(link = "logit"), data = mi_sglt2_exp)
summary(temp_fit)

# weight follow up
## logistic regression，adhere~weight follow up
mi_sglt2_exp |> group_by(adherence_flg) |> summarize(mean = mean(weight_fup), median = median(weight_fup))
temp_fit <- glm(formula = adherence_flg ~ weight_fup, 
                family = binomial(link = "logit"), data = mi_sglt2_exp)
summary(temp_fit)

## logistic regression，adhere~sbp follow up
mi_sglt2_exp |> group_by(adherence_flg) |> summarize(mean = mean(sbp_fup), median = median(sbp_fup))
temp_fit = glm(formula = adherence_flg ~ sbp_fup, 
                       family = binomial(link = "logit"), data = mi_sglt2_exp)
summary(temp_fit)
confint(temp_fit)



## ggpair if variables exist co-linearity
mi_sglt2_exp_subset <- mi_sglt2_exp |> select(bmi_bl, weight_bl, height_bl)
ggpairs(mi_sglt2_exp_subset)

########################################################################################################
# Aim 2
## survive time
mi_sglt2$surv_days <- as.numeric(as.Date(mi_sglt2$mi_dte) - as.Date(mi_sglt2$randomisation_dte))
mi_sglt2$surv_days <- if_else(is.na(mi_sglt2$surv_days),1826,mi_sglt2$surv_days)
mi_sglt2$surv_weeks <- mi_sglt2$surv_days/7

## Kaplan-Meier 
fit <- survfit(Surv(surv_weeks, mi_flg) ~ 1, data = mi_sglt2)
plot(fit, xlab = "Survival Weeks", ylab = "Survival probability", 
     main="Overall Survival",
     cex.lab=1.4, cex.axis=1.2, cex.main=1.6
)
#summary(fit)

## Kaplan-Meier case group/control group
### stratification by sex
tempfit <- survfit(Surv(surv_weeks, mi_flg, type='right') ~ alloc + sex, data = mi_sglt2)

plot(tempfit, col = c('red', 'red','blue', 'blue'), lwd=1, lty=c(1,2,1,2),
     xlab = "Survival Weeks", 
     ylab = "Survival probability", 
     main="Survival function",
     cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
#Make a legend in the plot
legend("bottomleft", legend = c('SGLT2:Female','SGLT2:Male','Ususal Care:Female','Ususal Care:Male'), 
       lty=c(1,2,1,2), col = c('red', 'red','blue', 'blue'), 
       lwd = 1, cex=1) 
## Plot log cumulative hazard 
plot(tempfit, fun = "cloglog", col = c('red', 'red','blue', 'blue'), lwd=1, lty=c(1,2,1,2),
     xlab = "Follow-up Weeks (log scale)", 
     ylab = "Cumulative hazard (log scale)", 
     main="Cumulative Hazard",
     cex.lab=1.2, cex.axis=1.2, cex.main=1.2)
#Make a legend in the plot
legend("topleft", legend = c('SGLT2:Female','SGLT2:Male','Ususal Care:Female','Ususal Care:Male'), 
       lty=c(1,2,1,2), col = c('red', 'red','blue', 'blue'), 
       lwd = 1, cex=1) 
  

### no stratification by sex
fit01 <- survfit(Surv(surv_weeks, mi_flg, type='right') ~ alloc, data = mi_sglt2)
plot(fit01, col = c('red', 'blue'), lwd=1, lty=c(1,1),
     xlab = "Survival Weeks", 
     ylab = "Survival probability", 
     main="Survival function of different treatment",
     cex.lab=1.2, cex.axis=1.2, cex.main=1.4)
#Make a legend in the plot
legend("bottomright", legend = levels(factor(mi_sglt2$alloc)), 
       lty = c(1, 1), col = c('red', 'blue'), 
       lwd = 1, cex=1) 

## Plot log cumulative hazard 
plot(fit01, fun = "cloglog", col = c('red', 'blue'), lwd=1, lty=c(1,1),
     xlab = "Follow-up Weeks (log scale)", 
     ylab = "Cumulative hazard (log scale)", 
     main="Cumulative Hazard",
     cex.lab=1.2, cex.axis=1.2, cex.main=1.4)
#Make a legend in the plot
legend("bottomright", legend = levels(factor(mi_sglt2$alloc)), 
       lty = c(1, 1), col = c('red', 'blue'), 
       lwd = 1, cex=1) 

## log rank test
survdiff(Surv(surv_weeks, mi_flg) ~ alloc + sex, data = mi_sglt2)

## cox model
mi_sglt2$alloc <- relevel(factor(mi_sglt2$alloc), ref = "Usual Care")
mi_sglt2$sex <- relevel(factor(mi_sglt2$sex), ref = "Male")

### limited to treatment
coxfit0 <- coxph(formula = Surv(surv_weeks, mi_flg) ~ alloc, data = mi_sglt2)
summary(coxfit0)

### adjust for sex
mi_sglt2_female <- subset(mi_sglt2,sex=='Female')
mi_sglt2_male <- subset(mi_sglt2,sex=='Male')

coxfit0_sex <- coxph(formula = Surv(surv_weeks, mi_flg) ~ alloc*sex + bmi_bl, data = mi_sglt2)
summary(coxfit0_sex)

coxfit0_f <- coxph(formula = Surv(surv_weeks, mi_flg) ~ alloc + bmi_bl, data = mi_sglt2_female) # 只用female数据集
summary(coxfit0_f)
coxfit0_m <- coxph(formula = Surv(surv_weeks, mi_flg) ~ alloc+ bmi_bl, data = mi_sglt2_male) # 只用male数据集
summary(coxfit0_m)

### adjust for bmi baseline
coxfit1 <- coxph(formula = Surv(surv_weeks, mi_flg) ~ alloc + bmi_bl, data = mi_sglt2) #  
summary(coxfit1)
### ph assumption check
survminer::ggcoxzph(cox.zph(coxfit0_sex))[4]
survminer::ggcoxzph(cox.zph(coxfit0_f))
survminer::ggcoxzph(cox.zph(coxfit0_m))[1]

phtest <- cox.zph(coxfit0) 
phtest
plot(phtest)

phtest <- cox.zph(coxfit1) 
phtest
plot(phtest)

anova(coxfit0, coxfit1)

## cox model (with time split)
tcuts <- c(52*1.5) # 95 cut
mi_sglt2_split <- survSplit(Surv(surv_weeks, mi_flg) ~ ., data=mi_sglt2, cut=tcuts, episode="ftime_group") 
mi_sglt2_split[, "ftime_group"] <- factor(mi_sglt2_split[, "ftime_group"])

coxfit2 <- coxph(formula = Surv(surv_weeks, mi_flg) ~ alloc*sex*ftime_group, data = mi_sglt2_split)
summary(coxfit2)


mi_sglt2_split_female <- subset(mi_sglt2_split,sex=='Female')
mi_sglt2_split_male <- subset(mi_sglt2_split,sex=='Male')

coxfit2_f <- coxph(formula = Surv(surv_weeks, mi_flg) ~ alloc*ftime_group, data = mi_sglt2_split_female)
summary(coxfit2_f)
coxfit2_m <- coxph(formula = Surv(surv_weeks, mi_flg) ~ alloc*ftime_group + bmi_bl, data = mi_sglt2_split_male)
summary(coxfit2_m)

survminer::ggcoxzph(cox.zph(coxfit2))
survminer::ggcoxzph(cox.zph(coxfit2_f))
survminer::ggcoxzph(cox.zph(coxfit2_m))
### ph assumption check
phtest <- cox.zph(coxfit2) 
phtest
plot(phtest)

phtest <- cox.zph(coxfit2) 
phtest
plot(phtest)

anova(coxfit2, coxfit3)
anova(coxfit2, coxfit4)
anova(coxfit2, coxfit5)
anova(coxfit3, coxfit4)



########################################################################################################
# Aim 3
mi_sglt2$sbp_diff <- mi_sglt2$sbp_fup - mi_sglt2$sbp_bl

mi_sglt2$alloc <- relevel(factor(mi_sglt2$alloc), ref = "Usual Care")
lm_sbpdiff_group <- lm(sbp_diff ~  mi_sglt2$alloc, data=mi_sglt2)
summary(lm_sbpdiff_group)
confint(lm_sbpdiff_group)

# if sbp_diff < 0, decrease in sbp during follow-up
mi_sglt2_exp$sbp_diff <- mi_sglt2_exp$sbp_fup - mi_sglt2_exp$sbp_bl
mi_sglt2_placebo$sbp_diff <- mi_sglt2_placebo$sbp_fup - mi_sglt2_placebo$sbp_bl


t.test(mi_sglt2_exp$sbp_diff, mi_sglt2_placebo$sbp_diff, alternative = "less")

ggplot(mi_sglt2, aes(x = sbp_diff, color = as.factor(alloc))) +
  geom_density() + 
  scale_color_manual(labels = c('SGLT2','Usual Care'), values = c("blue", "red")) +
  geom_vline(aes(xintercept = mean(mi_sglt2_exp$sbp_diff)), linetype = "dashed", color = "blue", size = 1) +
  geom_vline(aes(xintercept = mean(mi_sglt2_placebo$sbp_diff)), linetype = "dashed", color = "red", size = 1) +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(
    #title = "Body mass and flipper length",
    x = "Difference in SBP", 
    #y = "Body mass (g)",
    color = ""
  )

