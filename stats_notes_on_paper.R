# stats notes
# The study endpoint was death that occurred within 28 and 90
# days of CRRT initiation.
crrt <- readRDS('crrt.rds')

table(crrt$CRRTcause) %>% 
  proportions()
table(crrt$AKIN)
table(crrt$AKIcause)
summary(crrt$CRP_0h)


summary(crrt$SOFA_CRRT_0h)
hist(crrt$SOFA_CRRT_0h)


hist(crrt$APA_II_CRRT_0h)

cor.test(crrt$SOFA_CRRT_0h, crrt$P_0h)
cor.test(crrt$APA_II_CRRT_0h, crrt$P_0h)


# In a multivariable linear regression analysis after adjustment for age, sex,
# CCI, MAP, and urine volume, phosphate levels were significantly associated
# with the SOFA (β = 0.10, P = 0.02) and APACHE II (β = 0.58, P < 0.001) scores
# (Table 2).

m <- lm(SOFA_CRRT_0h ~ Age + Sex + CCI + MAP_0h + UO_2hrs + P_0h, data = crrt)
summary(m)

m2 <- lm(APA_II_CRRT_0h ~ Age + Sex + CCI + MAP_0h + UO_2hrs + P_0h, 
         data = crrt)
summary(m2)

m <- lm(APA_II_CRRT_0h ~ Age + survivor, data = d)
summary(m)
predict(m, newdata = data.frame(Age = c(50, 60), survivor = c("Non-Survivor","Non-Survivor")))

# ## Mann-Whitney test (aka Wilcoxon test)
# 
# From the article: "Variables that did not show normal distribution were compared using Mann-Whitney test or Kruskal-Wallis test. The Kolmogorov-Smirnov test was used to examine the normality of the distribution of parameters."
# 
# The Mann-Whitney test is another name for the Wilcoxon Test.
# 
# Let's look at White Blood Cell count by `Death_90D`. These values are reported in Table 1.
# 
# ```{r}
# tapply(d$WBC_0h, d$Death_90D, summary)
# ```
# 
# They decided to report medians instead of means, so presumably WBC is not normally distributed. The authors said they used the Kolmogorov-Smirnov test for normality. The null is the data are normal. A small p-value provides evidence against the null. Below we use `tapply` to apply the `ks.test` function to WBC_0h by survivor group. We specify "dnorm" because we want to compare the distribution of WBC to a normal distribution. 
# 
# ```{r}
# tapply(d$WBC_0h, d$Death_90D, ks.test, "dnorm")
# ```
# 
# It's probably more instructive to simply look at histograms. We don't really need a hypothesis test to sanctify the fact the distributions are not normal.
# 
# ```{r}
# ggplot(d) +
#   aes(x = WBC_0h) +
#   geom_histogram(bins = 20) +
#   facet_wrap(~Death_90D)
# ```
# 
# Since these distributions are not normal looking, the authors elected to compare the medians instead of the means using the Mann-Whitney test, aka Wilcoxon test.
# 
# ```{r}
# wilcox.test(WBC_0h ~ Death_90D, data = d)
# ```


