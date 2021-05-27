# summer biostats program
# https://wordpress.its.virginia.edu/VA_K-TUTOR/

# Data source: https://archive.ics.uci.edu/ml/datasets/chronic_kidney_disease

# Citation

# Dua, D. and Graff, C. (2019). UCI Machine Learning Repository
# [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California, School
# of Information and Computer Science.

# Statistics at Square One (online book)
# https://www.bmj.com/about-bmj/resources-readers/publications/statistics-square-one

# See attribute section for variable definition
d <- readRDS('kidney_disease.rds')

# 1.Age(numerical) age in years
# 2.Blood Pressure(numerical) bp in mm/Hg
# 3.Specific Gravity(nominal) sg - (1.005,1.010,1.015,1.020,1.025)
# 4.Albumin(nominal) al - (0,1,2,3,4,5)
# 5.Sugar(nominal) su - (0,1,2,3,4,5)
# 6.Red Blood Cells(nominal) rbc - (normal,abnormal)
# 7.Pus Cell (nominal) pc - (normal,abnormal)
# 8.Pus Cell clumps(nominal) pcc - (present,notpresent)
# 9.Bacteria(nominal) ba - (present,notpresent)
# 10.Blood Glucose Random(numerical) bgr in mgs/dl
# 11.Blood Urea(numerical) bu in mgs/dl
# 12.Serum Creatinine(numerical) sc in mgs/dl
# 13.Sodium(numerical) sod in mEq/L
# 14.Potassium(numerical) pot in mEq/L
# 15.Hemoglobin(numerical) hemo in gms
# 16.Packed Cell Volume(numerical) pcv
# 17.White Blood Cell Count(numerical) wbcc in cells/cumm
# 18.Red Blood Cell Count(numerical) rbcc in millions/cmm
# 19.Hypertension(nominal) htn - (yes,no)
# 20.Diabetes Mellitus(nominal) dm - (yes,no)
# 21.Coronary Artery Disease(nominal) cad - (yes,no)
# 22.Appetite(nominal) appet - (good,poor)
# 23.Pedal Edema(nominal) pe - (yes,no)
# 24.Anemia(nominal) ane - (yes,no)
# 25.Class (nominal) class - (ckd,notckd)

library(epitools)
library(dplyr)

# ckd = chronic kidney disease
summary(d$class)
summary(d$pcc)
summary(d$rbcc)
table(d$class)

hist(d$bp, xlab = var_label(d$bp), main = "Histogram of BP")

xtabs(~ class + dm, data = d)
xtabs(~ class + htn, data = d)
xtabs(~ class + cad, data = d)
xtabs(~ class + appet, data = d)
xtabs(~ class + pe, data = d)
xtabs(~ class + ba, data = d)
xtabs(~ class + pcc, data = d)

aggregate(wbcc ~ class, data = d, mean)
boxplot(wbcc ~ class, data = d, mean)
hist(d$wbcc[d$class == "ckd"])
hist(d$wbcc[d$class == "notckd"])

m <- glm(class ~ ba + pcc, data = d, family = binomial)
summary(m)

library(tableone)
CreateTableOne(data = d)
CreateTableOne(data = d, strata = "class")
CreateTableOne(vars = names(d)[-25], data = d, strata = "class")
CreateTableOne(vars = names(d)[-25], data = d, strata = "class", test = FALSE)
T1 <- CreateTableOne(vars = names(d)[-25], data = d, strata = "class", test = FALSE)
print(T1, showAllLevels = TRUE)
print(T1, showAllLevels = TRUE, nonnormal = TRUE)

summary(T1)



# uncertainty in estimation (standard errors, confidence intervals, and propagation of errors)

# Table 4
# parathyroid hormone (PTH)

# parameters
# black race 295.7 (32.9)
# age (yr) 24.11 (1.1)
# male 269.6 (32.7) 
# phosphorus (mg/dl) 53.8 (8.1)
# calcium (mg/dl) 55.8 (19.7)
# albumin50.6 (39.0)
# aluminum0.12 (1.01)
# interdialytic weight gain 8.4 (12.0)
# diabetes mellitus 2158.9 (34.1)

library(ISwR)
data("bp.obese")
str(bp.obese)
m <- lm(bp ~ obese + sex, data = bp.obese)
summary(m)
plot(m)
bp.obese[c(15,102),]



# uncertainty in proportions
table(d$rbc, d$class)
prop.table(table(d$rbc, d$class), margin = 1)
addmargins(table(d$rbc, d$class))
pout <- prop.test(x = c(60,141), n = c(201, 201))
# risk difference
diff(pout$estimate)

# risk ratio
pout$estimate[1]/pout$estimate[2]

# odds ratio
o1 <- pout$estimate[1]/(1 - pout$estimate[1])
o2 <- pout$estimate[2]/(1 - pout$estimate[2])

o1/o2

# uncertainty in means
d %>% 
  filter(class == "ckd") %>% 
  pull(sc) %>% 
  t.test()

t.test(d$sc[d$class == "ckd"])



t.test(sc ~ class, data = d)


# risk difference, relative risk, odds, and odds ratios

# https://jasn.asnjournals.org/content/11/2/330
# Table 5
m1 <- matrix(c(34,(264 - 34),
              73, (202 - 73)), ncol = 2, dimnames = list(c("<150",">150"),
                                                         c("black", "white")))
m1

p_m1 <- proportions(m1, margin = 2)
p_m1
(p_m1[1,1]/(1 - p_m1[1,1])) / 
  (p_m1[1,2]/(1 - p_m1[1,2]))


oddsratio(t(m1), rev = "column", method = "wald")

m2 <- matrix(c(45,(264 - 45),
              9, (202 - 9)), ncol = 2, dimnames = list(c(">500","<500"),
                                                       c("black", "white")))
m2
p_m2 <- proportions(m2, margin = 2)
p_m2
(p_m2[1,1]/(1 - p_m2[1,1])) / 
  (p_m2[1,2]/(1 - p_m2[1,2]))

oddsratio(t(m2), rev = "both", method = "wald")


# basics of implementing and interpreting linear and logistic regression models

# survival analysis

library(survival)
library(ISwR)
data("melanom")
str(melanom)
melanom <- melanom %>% 
  mutate(ulc = factor(ulc,labels = c("present","absent")),
         sex = factor(sex, labels = c("female", "male")))

# Create a Surv object
Surv(melanom$days, melanom$status == 1)

# Estimated survival function
# Kaplan-Meier estimate
# Prob of being alive (not experiencing some event) at a given time

surv.all <- survfit(Surv(days, status == 1) ~ 1, data = melanom)
# values of survival function at event times
summary(surv.all)

# Kaplan-Meier plot, or Sutvivial function
plot(surv.all)
# add censoring times
plot(surv.all, mark.time = TRUE)

# accumulated risk up until a certain time
plot(surv.all, mark.time = TRUE, cumhaz = TRUE)



# survival time by gender
surv.sex <- survfit(Surv(days, status == 1) ~ sex, data = melanom)
summary(surv.sex)
plot(surv.sex)
plot(surv.sex, conf.int = TRUE, col = c("black", "grey"))

# install.packages('survminer')
library(survminer)
ggsurvplot(surv.sex, data = melanom)

# are survival curves meaningfully different?
# log-rank test
survdiff(Surv(days, status == 1) ~ sex, data = melanom)

# stratafied by ulcer
survdiff(Surv(days, status == 1) ~ sex + strata(ulc), data = melanom)

# The Cox proportional hazards model
cm1 <- coxph(Surv(days, status == 1) ~ sex, data = melanom)
summary(cm1)

# exp(coef) is the hazard ratio

# A HR < 1 indicates reduced hazard of death whereas a HR > 1 indicates an
# increased hazard of death.
cm2 <- coxph(Surv(days, status == 1) ~ sex + log(thick) + strata(ulc), 
             data = melanom)
summary(cm2)
cm2_sf <- survfit(cm2)

# pretty bland
plot(survfit(cm2), conf.int = TRUE, col = 1:2, mark.time = TRUE)
legend("bottomleft", legend = c("present", "absent"), 
       lty = 1, col = 1:2)

# something wrong with this...
ggsurvplot(survfit(cm2), data = melanom, conf.int = TRUE)


# plot by hand
broom::tidy(cm2_sf) %>% 
  ggplot() +
  aes(x = time, y = estimate, color = strata, fill = strata) +
  geom_line() +
  geom_point(data = subset(broom::tidy(cm2_sf), n.censor > 0), 
             shape = 3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 1/4) +
  ylim(c(0,1)) +
  theme_minimal()


