################################################################################
# Purpose: POINT overdose paper - Table 1
# Author: Phillip Hungerford
# Date: 30/07/2019
################################################################################
# 0. dependencies with mean_sd, median_iqr and %_CI functions
source("tables.R")
con <- file("../reports/table1-characteristics-output.log")
sink(con, append=TRUE)
################################################################################
# 1. Load data
df <- read.csv("J:/Projects/Pharmaceutical Opioid Cohort/xPhillipH/ndarc-point-cleaning/data/1_raw/t1.csv")
################################################################################
print("Table 1. – Characteristics of the POINT cohort at baseline (n= 1,514)")

################################################################################
# 1. Demographics ##############################################################
# Age
# Male
# Unemployed

m_iqr <- median_iqr("Median Age", "T1_Actual_age")
pci <- percent_ci(df, "Male", "T1_sex", outcome="Male")
pci <- percent_ci(df, "Unemployed", "T1_Employ", outcome="Not employed")
################################################################################
# 2. Pain and Physical Health ##################################################
# 2.1. Median years living with Pain (IQR)
# 2.2-3. Pain severity score M(SD), Pain interference M(SD)

m_iqr <- median_iqr("Median years living with Pain", "T1_pain_duration_yrs")
names_list <- c("Pain severity score", "Pain interference", "PSEQ")
mean_sd_vars <- c("T1_BPI_PScore", "T1_BPI_interference", "T1_PSEQ_Score")
                        
for(i in 1:length(mean_sd_vars)){
  mean_sd(names_list[i], mean_sd_vars[i])
}

################################################################################
# 3. Pain conditions in past years 
# Arthritis or rheumatism 
# Back or neck problems
# Frequent/severe headaches
# Visceral
# Median 12m chronic pain conditions   (IQR)
# SF12 physical health score Median  (IQR)

names_list <- c("Arthritis or rheumatism", "Back or neck problems", "Frequent/severe headaches", "Visceral")
vars <- c("T1_Arth_12m", "T1_Back_12m", "T1_Head_12m", "T1_Visc_12m")
for (i in 1:length(vars)){
  pci <- percent_ci(df, names_list[i], vars[i], outcome="Yes")
}
m_iqr <- median_iqr("Median 12m chronic pain conditions", "T1_num_chronic_cond_12m")
m_iqr <- median_iqr("SF12 physical health score", "T1_SF12_PCS")

################################################################################
# 4. Mental and physical health (median(IQR) ###################################
# % Mod-severe depression
# % Mod-severe anxiety
# % Child-hood abuse/neglect
pci <- percent_ci(df, "% Mod-severe depression", "T1_PHQ9_Mod_sev", outcome="Yes, mod-severe dep")
pci <- percent_ci(df, "% Mod-severe anxiety", "T1_GADMod2Sev")
df$childhood_abuse_neglect <- df$T1_ca_1=="Yes" | df$T1_ca_2=="Yes"  | df$T1_ca_3=="Yes"  | df$T1_ca_4=="Yes"  | df$T1_ca_5=="Yes" 
pci <- percent_ci(df, "% Child-hood abuse/neglect", "childhood_abuse_neglect", outcome=TRUE)

################################################################################
# 4. Substance use #############################################################
# % Substance use history
pci <- percent_ci(df, "% Substance use history", "T1_Any_lifetime_sub_harm")

################################################################################
sink() # Restore output to console
sink(type="message")
################################################################################
################################### END ########################################
################################################################################