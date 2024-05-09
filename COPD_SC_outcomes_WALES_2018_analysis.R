#----------------------------------------------------------------------------------------------------------#
#                                                                                                          #
#   C O P D   S C   O U T C O M E S   A N A L Y S I S   s c R I P T                                        #
#                                                                                                          #
#   Author: Alex Adamson                                                                                   #
#----------------------------------------------------------------------------------------------------------#

# Let's set up something new so that we don't end up overwriting logs accidentally

# Things to note:

# Sink can write to both the console and a text file if you write 'split = TRUE'.
# If you use the 'cat' command instead of 'print' command it doesn't have a number before it
# when it's printed. You can also use /n to signal a new line. Good to use it for when you
# just want to paste something.
# Use sink() to stop the sink.

sink() # Just putting this here so that if I run it over again it doesn't create more and more sinks...

filename <- "D:/Alex/COPD/SC Outcomes 2018/Logs/COPD_SC_Outcomes_2018_analysis_log_WALES"
filedate <- Sys.Date()

sink(file = paste(filename, filedate, ".txt", sep = ""),
     append = FALSE,
     split = TRUE)

cat("\n START \n") # This means that every time I run it it restarts the document instead of getting an
# unuseable document at the end

sink()

sink(file = paste(filename, filedate, ".txt", sep = ""),
     append = TRUE,
     split = TRUE)


# Set up the libraries

library(dplyr)
# library(readstata13)
# library(xlsx)
source("H:/My R functions/MySummary.R")
library(janitor)
library(officer)
library(flextable)
library(tidyverse)
library(survival)
library(survminer)
library(ggplot2)
library(survsup)
# library(epitools)
library(psych)
library(comorbidity)
library(lme4)

nlc <- function(x) {cat(paste("\n", x, "\n", sep = ""))}
CP <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = FALSE)}
CPwithrn <- function(x) {write.table(x, "clipboard", sep = "\t", row.names = TRUE)}


# Read in the clean data from the analysis script:

dat <- readRDS(
"D:/Alex/COPD/SC Outcomes 2018/data/tidyData/linked_audit_HES_PEDW_ONS_data_clean_from_20191218_build_script.RDS")

# # # # # Only change!!! # # # # #

dat <- dat %>% filter(country == "Wales")

# # # # # # # # # # # # # # # # #



# Don't forget to ensure that you are using the correct dataset for analyses - for some things you need to use all
# admissions, for others you need to only use the index admissions.

nlc("Total number of admissions in dataset:")
nrow(dat)

nlc("Total number of index admissions in dataset:")
dat %>% filter(indexadmission == 1) %>% nrow()


nlc("First off, do the analysis for which you require all the admissions, not just index admissions. This analysis is
the 30 day / 90 day readmission ICD10 code analysis.")



# Number
top530 <- dat %>% filter(read30 == 1) %>% select(DIAG_01_conv) %>% table() %>% sort() %>% tail(5) %>% as_tibble()
colnames(top530) <- c("ICD10code", "n")


# Percentage
top530p <- dat %>% filter(read30 == 1) %>% select(DIAG_01_conv) %>% table() %>% prop.table() %>% sort() %>% 
         tail(5) %>% as_tibble()
colnames(top530p) <- c("ICD10code", "p")


top530 <- left_join(top530, top530p, by = "ICD10code")
top530$p <- top530$p*100
top530 <- top530 %>% arrange(desc(n))

nlc("Top 5 reasons for readmission within 30 days:")

top530
cbind(top530$ICD10code, paste0(top530$n, " (", round(top530$p, 1), "%)")) %>% CP()


nlc("Top 5 reasons for readmission within 30 days (table N):")
dat %>% filter(indexadmission == 1) %>% select(read30total) %>% sum()


# Number
top590 <- dat %>% filter(read90 == 1) %>% select(DIAG_01_conv) %>% table() %>% sort() %>% tail(5) %>% as_tibble()
colnames(top590) <- c("ICD10code", "n")


# Percentage
top590p <- dat %>% filter(read90 == 1) %>% select(DIAG_01_conv) %>% table() %>% prop.table() %>% sort() %>% 
  tail(5) %>% as_tibble()
colnames(top590p) <- c("ICD10code", "p")


top590 <- left_join(top590, top590p, by = "ICD10code")
top590$p <- top590$p*100
top590 <- top590 %>% arrange(desc(n))

nlc("Top 5 reasons for readmission within 90 days:")

top590
cbind(top590$ICD10code, paste0(top590$n, " (", round(top590$p, 1), "%)")) %>% CP()


nlc("Top 5 reasons for readmission within 90 days (table N):")
dat %>% filter(indexadmission == 1) %>% select(read90total) %>% sum()



# And now we drop everyone who wasn't an index admission

dat <- dat %>% filter(indexadmission == 1)


# May as well carry on with the readmission table here:
# 30 day readmissions

# Number
readtab530 <- dat %>% filter(died != "Yes") %>% select(read30totalcat) %>% table() %>% as_tibble()
colnames(readtab530) <- c("readmissions", "n")


# Percentage
readtab530p <- dat %>% filter(died != "Yes") %>% select(read30totalcat) %>% table() %>% prop.table() %>% as_tibble()
colnames(readtab530p) <- c("readmissions", "p")


readtab530 <- left_join(readtab530, readtab530p, by = "readmissions")
readtab530$p <- readtab530$p*100


nlc("Readmission counts within 30 days:")

readtab530
cbind(readtab530$readmissions, paste0(readtab530$n, " (", round(readtab530$p, 1), "%)")) %>% CP()


nlc("Readmission counts within 30 days (table N):")

dat %>% filter(died != "Yes") %>% nrow()

# 90 day readmissions

# Number
readtab590 <- dat %>% filter(died != "Yes") %>% select(read90totalcat) %>% table() %>% as_tibble()
colnames(readtab590) <- c("readmissions", "n")


# Percentage
readtab590p <- dat %>% filter(died != "Yes") %>% select(read90totalcat) %>% table() %>% prop.table() %>% as_tibble()
colnames(readtab590p) <- c("readmissions", "p")


readtab590 <- left_join(readtab590, readtab590p, by = "readmissions")
readtab590$p <- readtab590$p*100


nlc("Readmission counts within 90 days:")

readtab590
cbind(readtab590$readmissions, paste0(readtab590$n, " (", round(readtab590$p, 1), "%)")) %>% CP()


nlc("Readmission counts within 90 days (table N):")
dat %>% filter(died != "Yes") %>% nrow()

# Now, we go back to do the death stuff


nlc("Number who died within 30 days (1 = death)")
dat %>% select(died30) %>% table %>% as_tibble()

nlc("% who died within 30 days")
dat %>% select(died30) %>% table %>% prop.table()*100 %>% round(., 1)

nlc("Number who died within 90 days (1 = death)")
dat %>% select(died90) %>% table %>% as_tibble()

nlc("% who died within 90 days")
dat %>% select(died90) %>% table %>% prop.table()*100 %>% round(., 1)


# For 30 days:

# Number
topde530 <- dat %>% filter(died30 == 1) %>% select(CAUSE_OF_DEATH_conv) %>% table() %>% sort() %>% tail(5) %>% as_tibble()
colnames(topde530) <- c("ICD10code", "n")


# Percentage
topde530p <- dat %>% filter(died30 == 1) %>% select(CAUSE_OF_DEATH_conv) %>% table() %>% prop.table() %>% sort() %>% 
  tail(5) %>% as_tibble()
colnames(topde530p) <- c("ICD10code", "p")


topde530 <- left_join(topde530, topde530p, by = "ICD10code")
topde530$p <- topde530$p*100
topde530 <- topde530 %>% arrange(desc(n))

nlc("Top 5 reasons for death within 30 days:")

topde530
cbind(topde530$ICD10code, paste0(topde530$n, " (", round(topde530$p, 1), "%)")) %>% CP()


# For 90 days:

# Number
topde590 <- dat %>% filter(died90 == 1) %>% select(CAUSE_OF_DEATH_conv) %>% table() %>% sort() %>% tail(5) %>% as_tibble()
colnames(topde590) <- c("ICD10code", "n")


# Percentage
topde590p <- dat %>% filter(died90 == 1) %>% select(CAUSE_OF_DEATH_conv) %>% table() %>% prop.table() %>% sort() %>% 
  tail(10) %>% as_tibble()
colnames(topde590p) <- c("ICD10code", "p")


topde590 <- left_join(topde590, topde590p, by = "ICD10code")
topde590$p <- topde590$p*100
topde590 <- topde590 %>% arrange(desc(n))

nlc("Top 5 reasons for death within 90 days:")

topde590
cbind(topde590$ICD10code, paste0(topde590$n, " (", round(topde590$p, 1), "%)")) %>% CP()



# Okaaaaay lets do some odds ratios.

# Need a separate dataset for when we're looking at readmission

datalive <- dat %>% filter(died != "Yes")


tidyoutput <- function(x) {
  tidee <- exp(cbind(summary(x)$coefficients[ ,1], confint(x, parm="beta_", method = "Wald")))
  colnames(tidee) <- c("est", "lo", "hi")
  tidee <- round(tidee, 2)
  return(tidee)
}


dat$agecat <- relevel(dat$agecat, ref = "65-74")
datalive$agecat <- relevel(datalive$agecat, ref = "65-74")

##### died 30 days #####

nlc("For died within 30 days, unadjusted:")

m1 <- glmer(died30flag ~ 1 + gender + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR1 <- tidyoutput(m1)
unadjOR1

m2 <- glmer(died30flag ~ 1 + IMD.quintile + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR2 <- tidyoutput(m2)
unadjOR2 

m3 <- glmer(died30flag ~ 1 + agecat + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR3 <- tidyoutput(m3)
unadjOR3


m4 <- glmer(died30flag ~ 1 + CCIweightedcat + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR4 <- tidyoutput(m4)
unadjOR4 

m5 <- glmer(died30flag ~ 1 + longstayHES + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR5 <- tidyoutput(m5)
unadjOR5 

m6 <- glmer(died30flag ~ 1 + NIVgiven + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR6 <- tidyoutput(m6)
unadjOR6 


unadjORdied30 <- rbind(unadjOR1, unadjOR2, unadjOR3, unadjOR4, unadjOR5, unadjOR6)

unadjORdied30 %>% as.data.frame() %>% rownames_to_column() %>% mutate_at(vars(est, lo, hi), ~sprintf(., fmt = "%.2f")) %>%
  unite(CI, -rowname, -est, sep = " to ") %>% CPwithrn()



nlc("For died within 30 days, adjusted analysis:")

m9 <- glmer(died30flag ~ 1 + gender + IMD.quintile + agecat + CCIweightedcat + longstayHES + 
                         NIVgiven + (1 | hospital), family=binomial(link = "logit"), data = dat,
            glmerControl(optimizer = "Nelder_Mead"))

summary(m9)
adjORdied30 <- tidyoutput(m9) 
adjORdied30
adjORdied30 %>% as.data.frame() %>% rownames_to_column() %>% mutate_at(vars(est, lo, hi), ~sprintf(., fmt = "%.2f")) %>%
  unite(CI, -rowname, -est, sep = " to ") %>% CPwithrn()



######

# died 90 days

######

nlc("For died within 90 days, unadjusted:")


m1 <- glmer(died90flag ~ 1 + gender + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR1 <- tidyoutput(m1)
unadjOR1

m2 <- glmer(died90flag ~ 1 + IMD.quintile + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR2 <- tidyoutput(m2)
unadjOR2 

m3 <- glmer(died90flag ~ 1 + agecat + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR3 <- tidyoutput(m3)
unadjOR3 %>% as.data.frame() %>% unite(CI, -est, sep = " to ") %>% CPwithrn()


m4 <- glmer(died90flag ~ 1 + CCIweightedcat + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR4 <- tidyoutput(m4)
unadjOR4 

m5 <- glmer(died90flag ~ 1 + longstayHES + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR5 <- tidyoutput(m5)
unadjOR5 

m6 <- glmer(died90flag ~ 1 + NIVgiven + (1 | hospital), family=binomial(link = "logit"), data = dat)
unadjOR6 <- tidyoutput(m6)
unadjOR6 


unadjORdied90 <- rbind(unadjOR1, unadjOR2, unadjOR3, unadjOR4, unadjOR5, unadjOR6)
unadjORdied90
unadjORdied90 %>% as.data.frame() %>% rownames_to_column() %>% mutate_at(vars(est, lo, hi), ~sprintf(., fmt = "%.2f")) %>%
  unite(CI, -rowname, -est, sep = " to ") %>% CPwithrn()


m10 <- glmer(died90flag ~ 1 + gender + IMD.quintile + agecat + CCIweightedcat + longstayHES + 
              NIVgiven + (1 | hospital), family=binomial(link = "logit"), data = dat,
             glmerControl(optimizer = "Nelder_Mead"))

summary(m10)
adjORdied90 <- tidyoutput(m10) 
adjORdied90
adjORdied90 %>% as.data.frame() %>% rownames_to_column() %>% mutate_at(vars(est, lo, hi), ~sprintf(., fmt = "%.2f")) %>%
  unite(CI, -rowname, -est, sep = " to ") %>% CPwithrn()

# # Try a different optimiser and see what happens
# 
# m10v2 <- glmer(died90flag ~ 1 + gender + IMD.quintile + agecat + CCIweightedcat + longstayHES + 
#                NIVgiven + (1 | hospital), family=binomial(link = "logit"), data = dat,
#                glmerControl(optimizer = "Nelder_Mead"))
# 
# summary(m10v2)
# tidyoutput(m10v2)
# tidyoutput(m10v2) %>% as.data.frame() %>% unite(CI, -est, sep = " to ") %>% CPwithrn()



######





##### readmitted 30 days #####

nlc("For readmitted within 30 days, unadjusted:")


m1 <- glmer(read30flag ~ 1 + gender + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR1 <- tidyoutput(m1)
unadjOR1

m2 <- glmer(read30flag ~ 1 + IMD.quintile + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR2 <- tidyoutput(m2)
unadjOR2 

m3 <- glmer(read30flag ~ 1 + agecat + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR3 <- tidyoutput(m3)
unadjOR3 

m4 <- glmer(read30flag ~ 1 + CCIweightedcat + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR4 <- tidyoutput(m4)
unadjOR4 

m5 <- glmer(read30flag ~ 1 + longstayHES + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR5 <- tidyoutput(m5)
unadjOR5 

m6 <- glmer(read30flag ~ 1 + NIVgiven + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR6 <- tidyoutput(m6)
unadjOR6 


unadjORread30 <- rbind(unadjOR1, unadjOR2, unadjOR3, unadjOR4, unadjOR5, unadjOR6)
unadjORread30
unadjORread30 %>% as.data.frame() %>% rownames_to_column() %>% mutate_at(vars(est, lo, hi), ~sprintf(., fmt = "%.2f")) %>%
  unite(CI, -rowname, -est, sep = " to ") %>% CPwithrn()



m11 <- glmer(read30flag ~ 1 + gender + IMD.quintile + agecat + CCIweightedcat + longstayHES + 
              NIVgiven + (1 | hospital), family=binomial(link = "logit"), data = datalive,
            glmerControl(optimizer = "Nelder_Mead"))

summary(m11)
tidyoutput(m11)
adjORread30 <- tidyoutput(m11) 
adjORread30 %>% as.data.frame() %>% rownames_to_column() %>% mutate_at(vars(est, lo, hi), ~sprintf(., fmt = "%.2f")) %>%
  unite(CI, -rowname, -est, sep = " to ") %>% CPwithrn()


######

# readmitted 90 days

######

nlc("For readmitted within 90 days, unadjusted:")


m1 <- glmer(read90flag ~ 1 + gender + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR1 <- tidyoutput(m1)
unadjOR1

m2 <- glmer(read90flag ~ 1 + IMD.quintile + (1 | hospital), family=binomial(link = "logit"), data = datalive)
summary(m2)
unadjOR2 <- tidyoutput(m2)
unadjOR2 

m3 <- glmer(read90flag ~ 1 + agecat + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR3 <- tidyoutput(m3)
unadjOR3 

m4 <- glmer(read90flag ~ 1 + CCIweightedcat + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR4 <- tidyoutput(m4)
unadjOR4 

m5 <- glmer(read90flag ~ 1 + longstayHES + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR5 <- tidyoutput(m5)
unadjOR5 

m6 <- glmer(read90flag ~ 1 + NIVgiven + (1 | hospital), family=binomial(link = "logit"), data = datalive)
unadjOR6 <- tidyoutput(m6)
unadjOR6 


unadjORread90 <- rbind(unadjOR1, unadjOR2, unadjOR3, unadjOR4, unadjOR5, unadjOR6)
unadjORread90
unadjORread90 %>% as.data.frame() %>% rownames_to_column() %>% mutate_at(vars(est, lo, hi), ~sprintf(., fmt = "%.2f")) %>%
  unite(CI, -rowname, -est, sep = " to ") %>% CPwithrn()

m12 <- glmer(read90flag ~ 1 + gender + IMD.quintile + agecat + CCIweightedcat + longstayHES + 
               NIVgiven + (1 | hospital), family=binomial(link = "logit"), data = datalive,
             glmerControl(optimizer = "Nelder_Mead"))

summary(m12)
tidyoutput(m12)
adjORread90 <- tidyoutput(m12)
adjORread90 %>% as.data.frame() %>% rownames_to_column() %>% mutate_at(vars(est, lo, hi), ~sprintf(., fmt = "%.2f")) %>%
  unite(CI, -rowname, -est, sep = " to ") %>% CPwithrn()



######

datalive$readmittedbin <- 1
datalive$readmittedbin[is.na(datalive$timetofirstread) == TRUE] <- 0

datalive$timetoreadsurv <- datalive$timetofirstread

# JUST TO USE THE SURVIVAL FUNCTION, I'M MAKING IT CLEAR TO R THAT
# THEY WERE AROUND FOR 90 DAYS BUT DIDN'T COME IN, BY SAYING THEY CAME BACK 1000 DAYS LATER.

datalive$timetoreadsurv[is.na(datalive$timetoreadsurv) == TRUE] <- 1000



survfit(Surv(datalive$timetoreadsurv, datalive$readmittedbin) ~ 1, data = datalive) %>%
  ggsurvplot(fun = "event", surv.scale = "percent", conf.int = TRUE, xlim = c(0, 90), ylim = c(0, 1),
             break.x.by = 15, axes.offset = FALSE,
             legend = "none", ggtheme = theme_survminer()) +
  labs(x = "Time (days)", y = "Culmulative % of patients that have been readmitted") 



             
