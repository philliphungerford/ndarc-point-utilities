# Functions
median_iqr <- function(name, var){
  # m_iqr <- median_iqr("SF12 Mental health score", "T1_SF12_MCS")
  tmp <- df[,var]
  m <- round(median(tmp, na.rm=T),2)
  # Calculate IQR
  q <- quantile(tmp, na.rm=T)
  cat(name, "(median (IQR)):", m, "(", round(q[2],2), "-", round(q[4],2), ")", "\n")
}

mean_sd <- function(name, var){
  # msd <- mean_sd("mean_sd_vars[i]"BPI Score", "T1_BPI_PScore")
  tmp <- df[,var]
  cat(name,"(M(SD)):", round(mean(tmp, na.rm=T),2), "(", round(sd(tmp, na.rm=T),2), ") \n")
}

percent_ci <- function(df, name, var, outcome="Yes"){
  # pci <- percent_ci(df, "Male", "T1_sex", outcome="Male")
  tmp <- df[,var]
  observed <- sum(df[, var] == outcome, na.rm=T)
  total <- length(tmp)
  ci <- binom.test(observed, total, 0.5, alternative='two.sided', conf.level = 0.95)
  estimate = round(as.numeric(ci$estimate)*100,2)
  lower = round(ci$conf.int[1]*100,2)
  upper = round(ci$conf.int[2]*100,2)
  cat(name, "(%(CI)):", estimate, "(", lower, "-", upper, ")", "\n")
}