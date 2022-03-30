library(readxl)
library(dplyr)
library(rstatix)

# set your working directory somewhere that contains the 'SummaryData.xlsx' spreadsheet
setwd("/your/directory/here")

# Load data
d <- read_excel("SummaryData.xlsx", sheet=1)

# attach the entire dataset so that we can refer to all variables directly by name.
attach(d)
names(d)


### paired t-tests comparing relative D1R, D2R and D1/D2 ratio between association and sensorimotor cortices ###
t.test(nncRel_ASSOCIATION, nncRel_SENSORIMOTOR, paired=TRUE)
t.test(racRel_ASSOCIATION, racRel_SENSORIMOTOR, paired=TRUE)
t.test(D1D2rel_ASSOCIATION, D1D2rel_SENSORIMOTOR, paired=TRUE)  ## Figure 2C ##



### paired t-tests comparing methylphenidate-induced changes in brain activity (fALFF) and functional connectivity (rsFC) ###
# (methylpenidate minus placebo subtraction has already been performed here, so it *looks* like a one-sample t-test, but it's paired)
# note: Bonferroni correction was performed afterwards, p-values are uncorrected here
t.test(MPHminusPLA_fALFF_ASSOCIATION)
t.test(MPHminusPLA_fALFF_SENSORIMOTOR)

# and for within-network rsFC
t.test(MPHminusPLA_within_ASSOCIATION)
t.test(MPHminusPLA_within_SENSORIMOTOR)



###### Two-Way ANOVA: Drug (MPH vs. Placebo) by Network (Association vs. Sensorimotor) for fALFF and rsFC (Figure 3) #####
# The same data has been reorganized for ANOVA in a separate sheet #
d_ANOVA <- read_excel("SummaryData.xlsx",sheet=2)

fALFF.aov <- anova_test(data = d_ANOVA, dv = fALFF, wid = Subject, within = c(Network, Drug))
get_anova_table(fALFF.aov)

rsFC.aov <- anova_test(data = d_ANOVA, dv = rsFC, wid = Subject, within = c(Network, Drug))
get_anova_table(rsFC.aov)



### testing whether non-relative Striatal D1R, D2R, D1R/D2R ratio, and MPH-induced dopamine increases correlate with fALFF and rsFC:

# first create a dorsal striatum ROI by averaging caudate and putamen regions together): 
DS_nncBP <- rowMeans(cbind(cau_nncBP,put_nncBP)) # D1
DS_racBP <- rowMeans(cbind(cau_racBP,put_racBP)) # D2
DS_d1d2 <- rowMeans(cbind(cau_d1d2, put_d1d2))   # D1/D2 Ratio
DS_DAinc <- rowMeans(cbind(cau_DAinc,put_DAinc)) # dopamine increases

# test for significant dopamine increases in response to methylphenidate, in the nucleus accumbens and dorsal striatum 
t.test(nacc_DAinc)
t.test(DS_DAinc)



# Exploratory: a bunch of correlations between non-relative measures (Striatal D1R, D2R, D1R/D2R ratio and MPH-induced dopamine increases) and baseline brain activity (placebo fALFF) in association/sensorimotor cortices 
cor.test(DS_nncBP, PLA_fALFF_ASSOCIATION, use="pairwise.complete.obs", method = 'pearson')
cor.test(DS_racBP, PLA_fALFF_ASSOCIATION, use="pairwise.complete.obs", method = 'pearson')
cor.test(DS_d1d2, PLA_fALFF_ASSOCIATION, use="pairwise.complete.obs", method = 'pearson')
cor.test(DS_DAinc, PLA_fALFF_ASSOCIATION, use="pairwise.complete.obs", method = 'pearson')

cor.test(DS_nncBP, PLA_fALFF_SENSORIMOTOR, use="pairwise.complete.obs", method = 'pearson')
cor.test(DS_racBP, PLA_fALFF_SENSORIMOTOR, use="pairwise.complete.obs", method = 'pearson')
cor.test(DS_d1d2, PLA_fALFF_SENSORIMOTOR, use="pairwise.complete.obs", method = 'pearson')
cor.test(DS_DAinc, PLA_fALFF_SENSORIMOTOR, use="pairwise.complete.obs", method = 'pearson')

cor.test(nacc_nncBP, PLA_fALFF_ASSOCIATION, use="pairwise.complete.obs", method = 'pearson')
cor.test(nacc_racBP, PLA_fALFF_ASSOCIATION, use="pairwise.complete.obs", method = 'pearson')
cor.test(nacc_d1d2, PLA_fALFF_ASSOCIATION, use="pairwise.complete.obs", method = 'pearson')
cor.test(nacc_DAinc, PLA_fALFF_ASSOCIATION, use="pairwise.complete.obs", method = 'pearson')

cor.test(nacc_nncBP, PLA_fALFF_SENSORIMOTOR, use="pairwise.complete.obs", method = 'pearson')
cor.test(nacc_racBP, PLA_fALFF_SENSORIMOTOR, use="pairwise.complete.obs", method = 'pearson')
cor.test(nacc_d1d2, PLA_fALFF_SENSORIMOTOR, use="pairwise.complete.obs", method = 'pearson')
cor.test(nacc_DAinc, PLA_fALFF_SENSORIMOTOR, use="pairwise.complete.obs", method = 'pearson')



### SUPPLEMENT: test for Association Cortical D1/D2 Ratio with Age/Spatial Working Memory while controlling for IQ and Sex, at reviewer request:

summary(lm(Age ~ D1D2rel_ASSOCIATION + IQ + Sex))
summary(lm(SWM_Total_errors ~ D1D2rel_ASSOCIATION + IQ + Sex))

summary(lm(Age ~ D1D2rel_ASSOCIATION + Sex))
summary(lm(Age ~ D1D2rel_ASSOCIATION + IQ))

summary(lm(SWM_Total_errors ~ D1D2rel_ASSOCIATION + Sex))
summary(lm(SWM_Total_errors ~ D1D2rel_ASSOCIATION + IQ))


 

