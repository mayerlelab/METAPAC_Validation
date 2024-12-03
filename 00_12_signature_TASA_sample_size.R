# Function to calculate sample size for diagnostic test
calculate_sample_size <- function(sensitivity, specificity, 
                                  prevalence, power = 0.8, 
                                  alpha = 0.05, margin_of_error = 0.05) {
  # Calculate required parameters
  z_alpha <- qnorm(1 - alpha/2)  # Two-sided test
  z_beta <- qnorm(power)
  
  # Calculate minimum sample size for sensitivity
  n_sens <- ((z_alpha + z_beta)^2 * sensitivity * (1 - sensitivity)) / 
    (margin_of_error^2 * prevalence)
  
  # Calculate minimum sample size for specificity
  n_spec <- ((z_alpha + z_beta)^2 * specificity * (1 - specificity)) / 
    (margin_of_error^2 * (1 - prevalence))
  
  # Total sample size needed
  total_n <- max(n_sens, n_spec)
  
  # Calculate standard errors with the determined total_n
  SE_sens <- sqrt((sensitivity * (1 - sensitivity)) / 
                    (prevalence * total_n))
  
  SE_spec <- sqrt((specificity * (1 - specificity)) / 
                    ((1 - prevalence) * total_n))
  
  return(list(
    total_sample_size = ceiling(total_n),
    cases = ceiling(total_n * prevalence),
    controls = ceiling(total_n * (1 - prevalence)),
    SE_sensitivity = SE_sens,
    SE_specificity = SE_spec
  ))
}

# Parameters from your study
sensitivity <- 0.88  # Target sensitivity
specificity <- 0.85  # Fixed specificity
margin_of_error <- 0.055  # Acceptable margin of error
power <- 0.80  # Desired power

# Calculate for both prevalence scenarios
prev_diabetes <- 0.0085  # 0.85% prevalence in diabetes mellitus
prev_focal <- 0.20      # 20% prevalence in focal lesions

# Calculate sample sizes
result_diabetes <- calculate_sample_size(sensitivity, specificity, 
                                         prev_diabetes, power,
                                         margin_of_error = margin_of_error)
result_focal <- calculate_sample_size(sensitivity, specificity, 
                                      prev_focal, power,
                                      margin_of_error = margin_of_error)

# Print results
cat("\nFor diabetes mellitus population (prevalence = 0.85%):\n")
cat("Total sample size:", result_diabetes$total_sample_size, "\n")
cat("Number of cases:", result_diabetes$cases, "\n")
cat("Number of controls:", result_diabetes$controls, "\n")
cat("SE sensitivity:", round(result_diabetes$SE_sensitivity, 4), "\n")
cat("SE specificity:", round(result_diabetes$SE_specificity, 4), "\n")

cat("\nFor focal lesions population (prevalence = 20%):\n")
cat("Total sample size:", result_focal$total_sample_size, "\n")
cat("Number of cases:", result_focal$cases, "\n")
cat("Number of controls:", result_focal$cases, "\n")
cat("SE sensitivity:", round(result_focal$SE_sensitivity, 4), "\n")
cat("SE specificity:", round(result_focal$SE_specificity, 4), "\n")

# Calculate final sample size with dropout rate
dropout_rate <- 0.05
final_sample_size <- ceiling(result_focal$total_sample_size * (1 + dropout_rate))
cat("\nTotal sample size with", dropout_rate*100, "% dropouts:", final_sample_size, "\n")