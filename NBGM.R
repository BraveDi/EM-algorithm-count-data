set.seed(12345)
options(max.print = 100000)

#################################################
# GENERATE COVARIATE DIRECTLY IN R 
#################################################

# Set sample size
n <- 5000  # Change to 50, 100, 200, 500, 1000, or 5000 as needed

# Generate covariate from standard normal distribution
X <- rnorm(n, mean = 0, sd = 1)

# Scale the covariate (already mean 0, sd 1 from rnorm, but keep for consistency)
X_scaled <- (X - mean(X)) / sd(X)

# Optional: View histogram
hist(X_scaled, main = "Histogram of Generated Covariate", xlab = "X_scaled")

#################################################
# TRUE PARAMETERS
#################################################

beta_0 <- 1.5
beta_1 <- 1.5
beta_2 <- 1.5
theta_true <- 1

#################################################
# GENERATE NB RESPONSE
#################################################

mu <- exp(
  beta_0 +
    beta_1 * X_scaled +
    beta_2 * (X_scaled^2)
)

Y_complete <- rnbinom(
  length(X_scaled),
  size = theta_true,
  mu = mu
)

#################################################
# 10% MAR MISSINGNESS (Based on Covariate)
#################################################

missing_rate <- 0.10

# Generate missingness probability based on covariate (MAR)
prob_missing <- plogis(-2 + 0.5 * X_scaled)

# Create missing indicator
missing_indicator <- rbinom(length(X_scaled), 1, prob_missing)

# Adjust to achieve exactly 10% missing 
if (sum(missing_indicator) > floor(missing_rate * n)) {
  excess <- sum(missing_indicator) - floor(missing_rate * n)
  missing_indicator[sample(which(missing_indicator == 1), excess)] <- 0
} else if (sum(missing_indicator) < floor(missing_rate * n)) {
  deficit <- floor(missing_rate * n) - sum(missing_indicator)
  missing_indicator[sample(which(missing_indicator == 0), deficit)] <- 1
}

# Apply missingness
Y_obs <- Y_complete
Y_obs[missing_indicator == 1] <- NA

# Report actual missing rate
actual_missing_rate <- mean(missing_indicator) * 100
cat("Sample size: n =", n, "\n")
cat("Actual missing rate:", round(actual_missing_rate, 2), "%\n\n")

#################################################
# EM SETTINGS
#################################################

n_iter <- 1000
tol <- 1e-4

beta_0_est <- 1
beta_1_est <- 1
beta_2_est <- 1
theta_est <- 1

#################################################
# STORE VALUES
#################################################

beta_0_values <- numeric(n_iter)
beta_1_values <- numeric(n_iter)
beta_2_values <- numeric(n_iter)
theta_values  <- numeric(n_iter)

#################################################
# EM ALGORITHM
#################################################

library(MASS)

for(i in 1:n_iter){
  
  #################################################
  # E STEP: Impute missing values
  #################################################
  
  Y_imp <- Y_obs
  miss <- which(is.na(Y_imp))
  
  if(length(miss) > 0){
    
    mu_missing <- exp(
      beta_0_est +
        beta_1_est * X_scaled[miss] +
        beta_2_est * (X_scaled[miss]^2)
    )
    
    # For Negative Binomial, impute with rounded expected value
    Y_imp[miss] <- round(mu_missing)
  }
  
  #################################################
  # M STEP: Fit Negative Binomial GLM
  #################################################
  
  fit <- glm.nb(Y_imp ~ X_scaled + I(X_scaled^2))
  
  beta_0_new <- coef(fit)[1]
  beta_1_new <- coef(fit)[2]
  beta_2_new <- coef(fit)[3]
  theta_new <- fit$theta
  
  #################################################
  # STORE VALUES
  #################################################
  
  beta_0_values[i] <- beta_0_new
  beta_1_values[i] <- beta_1_new
  beta_2_values[i] <- beta_2_new
  theta_values[i]  <- theta_new
  
  #################################################
  # CONVERGENCE CHECK
  #################################################
  
  if(
    abs(beta_0_new - beta_0_est) < tol &&
    abs(beta_1_new - beta_1_est) < tol &&
    abs(beta_2_new - beta_2_est) < tol &&
    abs(theta_new - theta_est) < tol
  ){
    
    cat("Converged at iteration:", i, "\n")
    
    # Trim storage arrays to actual iterations
    beta_0_values <- beta_0_values[1:i]
    beta_1_values <- beta_1_values[1:i]
    beta_2_values <- beta_2_values[1:i]
    theta_values  <- theta_values[1:i]
    
    break
  }
  
  #################################################
  # UPDATE ESTIMATES
  #################################################
  
  beta_0_est <- beta_0_new
  beta_1_est <- beta_1_new
  beta_2_est <- beta_2_new
  theta_est <- theta_new
}

#################################################
# FINAL ESTIMATES
#################################################

cat("\n========================================\n")
cat("FINAL RESULTS\n")
cat("========================================\n\n")

cat("Sample size: n =", n, "\n")
cat("Actual missing rate:", round(actual_missing_rate, 2), "%\n")
cat("Convergence tolerance: tol =", tol, "\n")
cat("Iterations to convergence:", length(beta_0_values), "\n\n")

cat("Estimated Parameters:\n")
cat("beta_0 =", round(beta_0_est, 4), "(True:", beta_0, ")\n")
cat("beta_1 =", round(beta_1_est, 4), "(True:", beta_1, ")\n")
cat("beta_2 =", round(beta_2_est, 4), "(True:", beta_2, ")\n")
cat("theta  =", round(theta_est, 4), "(True:", theta_true, ")\n\n")