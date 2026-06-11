set.seed(12345)
options(max.print = 100000)

#################################################
# GENERATE COVARIATE DIRECTLY IN R 
#################################################

# Set sample size
n <- 100  # Change to 50, 100, 200, 500, 1000, or 5000 as needed

# Generate covariate from standard normal distribution
X <- rnorm(n, mean = 0, sd = 1)

# Optional: View histogram
hist(X, main = "Histogram of Generated Covariate", xlab = "X")

# Scale the covariate (already mean 0, sd 1 from rnorm, but keep for consistency)
X_scaled <- (X - mean(X)) / sd(X)

#################################################
# TRUE PARAMETER VALUES
#################################################

beta_0 <- 1.5
beta_1 <- 1.5
beta_2 <- 1.5

#################################################
# GENERATE RESPONSE (POISSON GLM)
#################################################

lam <- exp(beta_0 + beta_1 * X_scaled + beta_2 * (X_scaled^2))
Y_complete <- rpois(length(X), lam)

#################################################
# MAR MISSINGNESS AT 10%
#################################################

# Generate missingness probability based on covariate (MAR)
missing_rate <- 0.10
prob_missing <- plogis(-2 + 0.5 * X_scaled)  # Probability depends on X

# Create missing indicator
missing_indicator <- rbinom(length(X), 1, prob_missing)

# Adjust to achieve exactly 10% missing (optional)
if (sum(missing_indicator) > floor(missing_rate * n)) {
  excess <- sum(missing_indicator) - floor(missing_rate * n)
  missing_indicator[sample(which(missing_indicator == 1), excess)] <- 0
} else if (sum(missing_indicator) < floor(missing_rate * n)) {
  deficit <- floor(missing_rate * n) - sum(missing_indicator)
  missing_indicator[sample(which(missing_indicator == 0), deficit)] <- 1
}

# Apply missingness to response
Y <- Y_complete
Y[missing_indicator == 1] <- NA

# Report actual missing rate
actual_missing_rate <- mean(missing_indicator) * 100
cat("Sample size: n =", n, "\n")
cat("Actual missing rate:", round(actual_missing_rate, 2), "%\n\n")

#################################################
# EM SETTINGS
#################################################

n_iter <- 1000  # Reduced from 1,000,000 (reasonable with convergence criterion)
tol <- 1e-4     # Convergence tolerance

beta_0_est <- 1
beta_1_est <- 1
beta_2_est <- 1

# Store parameter values
beta_0_values <- numeric(n_iter)
beta_1_values <- numeric(n_iter)
beta_2_values <- numeric(n_iter)

#################################################
# EM ALGORITHM
#################################################

for (i in 1:n_iter) {
  
  # Make a copy of Y for imputation
  Y_imp <- Y
  
  # E-STEP: Impute missing values
  missing_index <- which(is.na(Y_imp))
  if (length(missing_index) > 0) {
    lam_missing <- exp(beta_0_est + beta_1_est * X_scaled[missing_index] + 
                         beta_2_est * (X_scaled[missing_index]^2))
    # For Poisson, impute with rounded expected value (or use rpois)
    Y_imp[missing_index] <- round(lam_missing)
  }
  
  # M-STEP: Fit Poisson GLM
  fit <- glm(Y_imp ~ X_scaled + I(X_scaled^2), family = poisson(link = "log"))
  
  # Update parameter estimates
  beta_0_new <- coef(fit)[1]
  beta_1_new <- coef(fit)[2]
  beta_2_new <- coef(fit)[3]
  
  # Store estimates
  beta_0_values[i] <- beta_0_new
  beta_1_values[i] <- beta_1_new
  beta_2_values[i] <- beta_2_new
  
  # Check convergence
  if (abs(beta_0_new - beta_0_est) < tol && 
      abs(beta_1_new - beta_1_est) < tol && 
      abs(beta_2_new - beta_2_est) < tol) {
    cat("Converged at iteration:", i, "\n")
    # Trim the storage arrays to actual iterations
    beta_0_values <- beta_0_values[1:i]
    beta_1_values <- beta_1_values[1:i]
    beta_2_values <- beta_2_values[1:i]
    break
  }
  
  beta_0_est <- beta_0_new
  beta_1_est <- beta_1_new
  beta_2_est <- beta_2_new
}

#################################################
# RESULTS
#################################################

cat("\n========================================\n")
cat("FINAL RESULTS\n")
cat("========================================\n\n")

cat("Sample size: n =", n, "\n")
cat("Missing rate:", round(actual_missing_rate, 2), "%\n")
cat("Convergence tolerance: tol =", tol, "\n\n")

cat("Estimated Parameters:\n")
cat("beta_0 =", round(beta_0_est, 4), "(True:", beta_0, ")\n")
cat("beta_1 =", round(beta_1_est, 4), "(True:", beta_1, ")\n")
cat("beta_2 =", round(beta_2_est, 4), "(True:", beta_2, ")\n\n")

# Model summary
cat("GLM Model Summary:\n")
print(summary(fit))
