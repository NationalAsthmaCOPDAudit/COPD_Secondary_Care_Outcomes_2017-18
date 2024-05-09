#----------------------------------------------------------------------------------------------------------#
#                                                                                                          #
#   C O P D   S C   O U T C O M E S   A N A L Y S I S   s c R I P T                                        #
#                                                                                                          #
#   Author: Alex Adamson                                                                                   #
#----------------------------------------------------------------------------------------------------------#




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

# Remove the hospadmissions column, because this is calculated by the audit data rather than the HES data
dat$hospadmissions <- NULL

# Don't forget to ensure that you are using the correct dataset for analyses - for some things you need to use all
# admissions, for others you need to only use the index admissions.

# Need to tally up total index admissions, total 30-day readmissions, and total 90-day readmissions.

dat <- dat %>% group_by(hospital) %>% add_tally(wt = indexadmission) %>% rename(hospindexadmissions = n) %>%
                               add_tally(wt = read30) %>% rename(read30hospcount_reason_N = n) %>%
                               add_tally(wt = read90) %>% rename(read90hospcount_reason_N = n) %>% ungroup()



# As a brief explanation: I'm grouping by hospital, keeping hospital 30 day admission number in the dataset
# because I'll need it later, filtering just those readmitted within 30 days, counting up all the reasons for 
# readmission (this is a 'summarise' function; from here all the single admissions go away), then we order the 
# dataset (within hospital) by the number of readmissions, then we just keep the top 5 (using slice), then use
# row_number so that we can write the top 5 most common diagnoses, and then we use the new 'pivot_wider' command
# that's been newly added to the tidy verse to transform the data from long to wide, and add percentages in for
# the number of diagnoses of a certain code divided by total number of 30 day readmissions.

dat30read <- dat %>%
  group_by(hospital, read30hospcount_reason_N) %>% filter(read30 == 1) %>% count(DIAG_01_conv) %>%
  rename(readmiss30diagcount = n) %>% arrange(hospital, desc(readmiss30diagcount)) %>% slice(1:5) %>%
  rename(readmiss30_diag_code = DIAG_01_conv) %>% mutate(readmissrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(hospital, read30hospcount_reason_N), names_from = readmissrank,
              values_from = c(readmiss30_diag_code, readmiss30diagcount)) %>% ungroup() %>%
  mutate(readmissdiag30perc_highest_diag_1 = (readmiss30diagcount_highest_diag_1/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_2 = (readmiss30diagcount_highest_diag_2/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_3 = (readmiss30diagcount_highest_diag_3/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_4 = (readmiss30diagcount_highest_diag_4/read30hospcount_reason_N)*100,
         readmissdiag30perc_highest_diag_5 = (readmiss30diagcount_highest_diag_5/read30hospcount_reason_N)*100) %>% ungroup()

# Then do the same for 90-day readmissions


dat90read <- dat %>%
  group_by(hospital, read90hospcount_reason_N) %>% filter(read90 == 1) %>% count(DIAG_01_conv) %>% 
  rename(readmiss90diagcount = n) %>% arrange(hospital, desc(readmiss90diagcount)) %>% slice(1:5) %>%
  rename(readmiss90_diag_code = DIAG_01_conv) %>%
  mutate(readmissrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(hospital, read90hospcount_reason_N), names_from = readmissrank,
              values_from = c(readmiss90_diag_code, readmiss90diagcount)) %>% ungroup() %>%
  mutate(readmissdiag90perc_highest_diag_1 = (readmiss90diagcount_highest_diag_1/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_2 = (readmiss90diagcount_highest_diag_2/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_3 = (readmiss90diagcount_highest_diag_3/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_4 = (readmiss90diagcount_highest_diag_4/read90hospcount_reason_N)*100,
         readmissdiag90perc_highest_diag_5 = (readmiss90diagcount_highest_diag_5/read90hospcount_reason_N)*100) %>% ungroup()


# Now, we don't need anyone else who wasn't an index admission, so we drop them


dat <- dat %>% filter(indexadmission == 1) 

# And now we add the died30/90 tallies.

dat <- dat %>% group_by(hospital) %>%
  add_tally(wt = died30) %>% rename(died30hospcount_reason_N = n) %>%
  add_tally(wt = died90) %>% rename(died90hospcount_reason_N = n) %>% ungroup()

# we also create the readmission tables
# quite difficult working with the '3+' variable so we recode it

dat$read30totalcat <- as.character(dat$read30totalcat)
dat$read30totalcat[dat$read30totalcat == "3+"] <- "3plus"
dat$read30totalcat <- factor(dat$read30totalcat, levels = c("0", "1", "2", "3plus"))

dat$read90totalcat <- as.character(dat$read90totalcat)
dat$read90totalcat[dat$read90totalcat == "3+"] <- "3plus"
dat$read90totalcat <- factor(dat$read90totalcat, levels = c("0", "1", "2", "3plus"))

# #
# Must add .drop = FALSE so that empty groups are still counted
# #

dat30read2 <- dat %>% filter(died != "Yes") %>% group_by(hospital) %>% add_count() %>% 
  rename(read30and90hospcount_readnumber_N = n) %>%
  group_by(hospital, read30and90hospcount_readnumber_N) %>% count(read30totalcat, .drop = FALSE) %>%
  rename(read30groups = read30totalcat) %>%
  pivot_wider(id_cols = c(hospital, read30and90hospcount_readnumber_N), names_from = read30groups,
              values_from = n, names_prefix = "read30no_cat_") %>% 
  rename_at(vars(starts_with("read30no_cat_")), funs(paste0(., "_n"))) %>% ungroup() %>%
  mutate(read30no_cat_0_perc = (read30no_cat_0_n/read30and90hospcount_readnumber_N)*100,
         read30no_cat_1_perc = (read30no_cat_1_n/read30and90hospcount_readnumber_N)*100,
         read30no_cat_2_perc = (read30no_cat_2_n/read30and90hospcount_readnumber_N)*100,
         read30no_cat_3plus_perc = (read30no_cat_3plus_n/read30and90hospcount_readnumber_N)*100)


dat90read2 <- dat %>% filter(died != "Yes") %>% group_by(hospital) %>% add_count() %>% 
  rename(read30and90hospcount_readnumber_N = n) %>%
  group_by(hospital, read30and90hospcount_readnumber_N) %>% count(read90totalcat, .drop = FALSE) %>%
  rename(read90groups = read90totalcat) %>%
  pivot_wider(id_cols = c(hospital, read30and90hospcount_readnumber_N), names_from = read90groups,
              values_from = n, names_prefix = "read90no_cat_") %>% 
  rename_at(vars(starts_with("read90no_cat_")), funs(paste0(., "_n"))) %>% ungroup() %>%
  mutate(read90no_cat_0_perc = (read90no_cat_0_n/read30and90hospcount_readnumber_N)*100,
         read90no_cat_1_perc = (read90no_cat_1_n/read30and90hospcount_readnumber_N)*100,
         read90no_cat_2_perc = (read90no_cat_2_n/read30and90hospcount_readnumber_N)*100,
         read90no_cat_3plus_perc = (read90no_cat_3plus_n/read30and90hospcount_readnumber_N)*100)



# Now we go back to the mortality.

hospsumm <- dat %>% group_by(hospital) %>%
  summarise(mortality_total_N = n(),
            died30_n = sum(died30),
            died30_perc = (died30_n/mortality_total_N)*100,
            died90_n = sum(died90),
            died90_perc = (died90_n/mortality_total_N)*100)


# Cause of mortality tables:
            
dat30died <- dat %>%
  group_by(hospital, died30hospcount_reason_N) %>% filter(died30 == 1) %>% count(CAUSE_OF_DEATH_conv) %>%
  rename(COD30diagcount = n) %>% arrange(hospital, desc(COD30diagcount)) %>% slice(1:5) %>%
  rename(COD30_diag_code = CAUSE_OF_DEATH_conv) %>% mutate(CODrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(hospital, died30hospcount_reason_N), names_from = CODrank,
              values_from = c(COD30_diag_code, COD30diagcount)) %>% ungroup() %>%
  mutate(CODdiag30perc_highest_diag_1 = (COD30diagcount_highest_diag_1/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_2 = (COD30diagcount_highest_diag_2/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_3 = (COD30diagcount_highest_diag_3/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_4 = (COD30diagcount_highest_diag_4/died30hospcount_reason_N)*100,
         CODdiag30perc_highest_diag_5 = (COD30diagcount_highest_diag_5/died30hospcount_reason_N)*100) %>% ungroup()



dat90died <- dat %>%
  group_by(hospital, died90hospcount_reason_N) %>% filter(died90 == 1) %>% count(CAUSE_OF_DEATH_conv) %>%
  rename(COD90diagcount = n) %>% arrange(hospital, desc(COD90diagcount)) %>% slice(1:5) %>%
  rename(COD90_diag_code = CAUSE_OF_DEATH_conv) %>% mutate(CODrank = paste0("highest_diag_", row_number())) %>%
  pivot_wider(id_cols = c(hospital, died90hospcount_reason_N), names_from = CODrank,
              values_from = c(COD90_diag_code, COD90diagcount)) %>% ungroup() %>%
  mutate(CODdiag90perc_highest_diag_1 = (COD90diagcount_highest_diag_1/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_2 = (COD90diagcount_highest_diag_2/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_3 = (COD90diagcount_highest_diag_3/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_4 = (COD90diagcount_highest_diag_4/died90hospcount_reason_N)*100,
         CODdiag90perc_highest_diag_5 = (COD90diagcount_highest_diag_5/died90hospcount_reason_N)*100) %>% ungroup()




# And then we stitch them all together in order by hospital

all_dat <- left_join(hospsumm, dat30died, by = "hospital") %>% left_join(., dat90died, by = "hospital") %>%
           left_join(., dat30read2, by = "hospital") %>% left_join(., dat30read, by = "hospital") %>%
           left_join(., select(dat90read2, -read30and90hospcount_readnumber_N), by = "hospital") %>%
           left_join(., dat90read, by = "hospital")



all_dat$died30hospcount_reason_N[is.na(all_dat$died30hospcount_reason_N)] <- 0
all_dat$died90hospcount_reason_N[is.na(all_dat$died90hospcount_reason_N)] <- 0
all_dat$read30hospcount_reason_N[is.na(all_dat$read30hospcount_reason_N)] <- 0
all_dat$read90hospcount_reason_N[is.na(all_dat$read90hospcount_reason_N)] <- 0


all_dat <- all_dat %>% mutate_at(vars(contains('perc')), ~ sprintf("%.1f", round(., 1), nsmall = 1))

glimpse(all_dat)

# Now we save it

# write.csv(all_dat, "D:/Alex/COPD/SC Outcomes 2018/data/tidyData/COPD_SC_outcomes_2018_hosp_level_analysis_20191219.csv",
#          row.names = FALSE)

# Done!


