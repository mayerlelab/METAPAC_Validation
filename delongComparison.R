compare_auc_manual <- function(auc1, ci1_lower, ci1_upper, 
                               auc2, ci2_lower, ci2_upper) {
  # Create results data frame
  results <- data.frame(
    Model = c("Model 1", "Model 2"),
    AUC = c(auc1, auc2),
    CI_lower = c(ci1_lower, ci2_lower),
    CI_upper = c(ci1_upper, ci2_upper)
  )
  
  # Calculate standard error from CI
  # For 95% CI, z = 1.96
  se1 <- (ci1_upper - ci1_lower) / (2 * 1.96)
  se2 <- (ci2_upper - ci2_lower) / (2 * 1.96)
  
  # Calculate z-statistic
  z_stat <- (auc1 - auc2) / sqrt(se1^2 + se2^2)
  
  # Calculate p-value (two-sided test)
  # Use pnorm() with log.p=TRUE for better numerical precision
  log_p <- pnorm(abs(z_stat), lower.tail=FALSE, log.p=TRUE)
  p_value <- format(2 * exp(log_p), scientific=TRUE)
  
  return(list(results=results, z_stat=z_stat, p_value=p_value))
}
## comaprison between CA19.9 alone and i -met
results <- compare_auc_manual(
  auc1 = 0.861    ,
  ci1_lower = 0.854   ,
  ci1_upper = 0.869,
  auc2 = 0.818 , # Replace with your second AUC value
  ci2_lower = 0.814  , # Replace with your second CI lower bound
  ci2_upper = 0.822 # Replace with your second CI upper bound
)

results$z_stat
results$p_value
