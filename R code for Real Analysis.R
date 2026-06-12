set.seed(12345)
D = read.csv("FINALDATA.csv")
Y = D[,1]
X1 = D[,2]
X2 = D[,3]
X3 = D[,4]
X4 = D[,5]
X5 = D[,6]
X6 = D[,7]

X1_s = (X1-mean(X1))/sd(X1)
X2_s = (X2-mean(X2))/sd(X2)
X3_s = (X3-mean(X3))/sd(X3)
X4_s = (X4-mean(X4))/sd(X4)
X5_s = (X5-mean(X5))/sd(X5)
X6_s = (X6-mean(X6))/sd(X6)

missing_indices = sample(1:length(Y), size = floor(0.05*length(Y)), replace = FALSE)
Y[missing_indices] = NA
Y
n_iter = 500
tol = 1e-2

#initializing 
beta_0_est = 10
beta_1_est = 10
beta_2_est = 10
beta_3_est = 10
beta_4_est = 10
beta_5_est = 10
beta_6_est = 10

beta_0_values = numeric(n_iter)
beta_1_values = numeric(n_iter)
beta_2_values = numeric(n_iter)
beta_3_values = numeric(n_iter)
beta_4_values = numeric(n_iter)
beta_5_values = numeric(n_iter)
beta_6_values = numeric(n_iter)


for (i in 1:n_iter) {
  
  # Impute missing Y using current parameters
  missing_index <- which(is.na(Y))
  if (length(missing_index) > 0) {
    lam_missing <- exp(pmin(beta_0_est + beta_1_est * X1_s[missing_index] + beta_2_est * X2_s[missing_index] + beta_3_est *X3_s[missing_index] + beta_4_est *X4_s[missing_index] + beta_5_est *X5_s[missing_index] + beta_6_est *X6_s[missing_index], 10))
    Y[missing_index] <- rpois(length(missing_index), lam_missing)
  }
  
  # Define negative log-likelihood function
  log_likelihood <- function(params) {
    beta_0 <- params[1]
    beta_1 <- params[2]
    beta_2 <- params[3]
    beta_3 <- params[4]
    beta_4 <- params[5]
    beta_5 <- params[6]
    beta_6 <- params[7]
    
    
    lambda <- exp(beta_0 + beta_1 * X1_s + beta_2 * X2_s + beta_3 * X3_s + beta_4 * X4_s + beta_5 * X5_s + beta_6 * X6_s)
    loglik <- sum(Y * log(lambda) - lambda - lgamma(Y + 1))
    return(-loglik)  # Minimize negative log-likelihood
  }
  
  # Optimize to find new parameter estimates
  opt_result <- optim(c(beta_0_est, beta_1_est, beta_2_est, beta_3_est, beta_4_est, beta_5_est, beta_6_est), log_likelihood, method = "BFGS")
  
  beta_0_new <- opt_result$par[1]
  beta_1_new <- opt_result$par[2]
  beta_2_new <- opt_result$par[3]
  beta_3_new <- opt_result$par[4]
  beta_4_new <- opt_result$par[5]
  beta_5_new <- opt_result$par[6]
  beta_6_new <- opt_result$par[7]
  
  # Store estimates
  beta_0_values[i] <- beta_0_new
  beta_1_values[i] <- beta_1_new
  beta_2_values[i] <- beta_2_new
  beta_3_values[i] <- beta_3_new
  beta_4_values[i] <- beta_4_new
  beta_5_values[i] <- beta_5_new
  beta_6_values[i] <- beta_6_new
  
  # Check for convergence
  if (abs(beta_0_new - beta_0_est) < tol &&
      abs(beta_1_new - beta_1_est) < tol &&
      abs(beta_2_new - beta_2_est) < tol &&
      abs(beta_3_new - beta_3_est) < tol &&
      abs(beta_4_new - beta_4_est) < tol &&
      abs(beta_5_new - beta_5_est) < tol &&
      abs(beta_6_new - beta_6_est) < tol) {
    cat("Converged at iteration:", i, "\n")
    beta_0_values <- beta_0_values[1:i]
    beta_1_values <- beta_1_values[1:i]
    beta_2_values <- beta_2_values[1:i]
    beta_3_values <- beta_3_values[1:i]
    beta_4_values <- beta_4_values[1:i]
    beta_5_values <- beta_5_values[1:i]
    beta_6_values <- beta_6_values[1:i]
    break
  }
  
  # Update estimates
  beta_0_est <- beta_0_new
  beta_1_est <- beta_1_new
  beta_2_est <- beta_2_new
  beta_3_est <- beta_3_new
  beta_4_est <- beta_4_new
  beta_5_est <- beta_5_new
  beta_6_est <- beta_6_new
}

# Final estimates
cat("Final estimates:\n")
cat("beta_0 =", beta_0_est, "\n")
cat("beta_1 =", beta_1_est, "\n")
cat("beta_2 =", beta_2_est, "\n")
cat("beta_3 =", beta_3_est, "\n")
cat("beta_4 =", beta_4_est, "\n")
cat("beta_5 =", beta_5_est, "\n")
cat("beta_6 =", beta_6_est, "\n")

cat("Estimated beta_0 =", beta_0_est, "\n")
cat("Estimated beta_1 =", beta_1_est, "\n")
cat("Estimated beta_2 =", beta_2_est, "\n")
cat("Estimated beta_3 =", beta_3_est, "\n")
cat("Estimated beta_4 =", beta_4_est, "\n")
cat("Estimated beta_5 =", beta_5_est, "\n")
cat("Estimated beta_5 =", beta_6_est, "\n")

#####################################################################
#################################################################
###############################################################
set.seed(12345)
D = read.csv("FinalDATA.csv")
Y = D[,1]
X1 = D[,2]
X2 = D[,3]
X3 = D[,4]
X4 = D[,5]
X5 = D[,6]
X6 = D[,7]

X1_s = (X1 - mean(X1)) / sd(X1)
X2_s = (X2 - mean(X2)) / sd(X2)
X3_s = (X3 - mean(X3)) / sd(X3)
X4_s = (X4 - mean(X4)) / sd(X4)
X5_s = (X5 - mean(X5)) / sd(X5)
X6_s = (X6 - mean(X6)) / sd(X6)

missing_indices = sample(1:length(Y), size = floor(0.05 * length(Y)), replace = FALSE)
Y[missing_indices] = NA
Y

n_iter = 500
tol = 1e-2

# initializing 
beta_0_est = 10
beta_1_est = 10
beta_2_est = 10
beta_3_est = 10
beta_4_est = 10
beta_5_est = 10
beta_6_est = 10
theta_est = 10  # NB dispersion parameter

beta_0_values = numeric(n_iter)
beta_1_values = numeric(n_iter)
beta_2_values = numeric(n_iter)
beta_3_values = numeric(n_iter)
beta_4_values = numeric(n_iter)
beta_5_values = numeric(n_iter)
beta_6_values = numeric(n_iter)
theta_values  = numeric(n_iter)

for (i in 1:n_iter) {
  
  # Impute missing Y using current parameters
  missing_index <- which(is.na(Y))
  if (length(missing_index) > 0) {
    lam_missing <- exp(pmin(beta_0_est + beta_1_est * X1_s[missing_index] + beta_2_est * X2_s[missing_index] +
                              beta_3_est * X3_s[missing_index] + beta_4_est * X4_s[missing_index] +
                              beta_5_est * X5_s[missing_index] + beta_6_est * X6_s[missing_index], 10))
    Y[missing_index] <- rnbinom(length(missing_index), size = theta_est, mu = lam_missing)
  }
  
  # Define negative log-likelihood function
  log_likelihood <- function(params) {
    beta_0 <- params[1]
    beta_1 <- params[2]
    beta_2 <- params[3]
    beta_3 <- params[4]
    beta_4 <- params[5]
    beta_5 <- params[6]
    beta_6 <- params[7]
    theta  <- exp(params[8])  # log-link to ensure positivity
    
    lambda <- exp(beta_0 + beta_1 * X1_s + beta_2 * X2_s + beta_3 * X3_s +
                    beta_4 * X4_s + beta_5 * X5_s + beta_6 * X6_s)
    loglik <- sum(dnbinom(Y, size = theta, mu = lambda, log = TRUE))
    return(-loglik)  # Minimize negative log-likelihood
  }
  
  # Optimize to find new parameter estimates
  opt_result <- optim(c(beta_0_est, beta_1_est, beta_2_est, beta_3_est,
                        beta_4_est, beta_5_est, beta_6_est, log(theta_est)),
                      log_likelihood, method = "Nelder-Mead")
  
  beta_0_new <- opt_result$par[1]
  beta_1_new <- opt_result$par[2]
  beta_2_new <- opt_result$par[3]
  beta_3_new <- opt_result$par[4]
  beta_4_new <- opt_result$par[5]
  beta_5_new <- opt_result$par[6]
  beta_6_new <- opt_result$par[7]
  theta_new  <- exp(opt_result$par[8])
  
  # Store estimates
  beta_0_values[i] <- beta_0_new
  beta_1_values[i] <- beta_1_new
  beta_2_values[i] <- beta_2_new
  beta_3_values[i] <- beta_3_new
  beta_4_values[i] <- beta_4_new
  beta_5_values[i] <- beta_5_new
  beta_6_values[i] <- beta_6_new
  theta_values[i]  <- theta_new
  
  # Check for convergence
  if (abs(beta_0_new - beta_0_est) < tol &&
      abs(beta_1_new - beta_1_est) < tol &&
      abs(beta_2_new - beta_2_est) < tol &&
      abs(beta_3_new - beta_3_est) < tol &&
      abs(beta_4_new - beta_4_est) < tol &&
      abs(beta_5_new - beta_5_est) < tol &&
      abs(beta_6_new - beta_6_est) < tol &&
      abs(theta_new - theta_est) < tol) {
    cat("Converged at iteration:", i, "\n")
    beta_0_values <- beta_0_values[1:i]
    beta_1_values <- beta_1_values[1:i]
    beta_2_values <- beta_2_values[1:i]
    beta_3_values <- beta_3_values[1:i]
    beta_4_values <- beta_4_values[1:i]
    beta_5_values <- beta_5_values[1:i]
    beta_6_values <- beta_6_values[1:i]
    theta_values  <- theta_values[1:i]
    break
  }
  
  # Update estimates
  beta_0_est <- beta_0_new
  beta_1_est <- beta_1_new
  beta_2_est <- beta_2_new
  beta_3_est <- beta_3_new
  beta_4_est <- beta_4_new
  beta_5_est <- beta_5_new
  beta_6_est <- beta_6_new
  theta_est  <- theta_new
}

# Final estimates
cat("Final estimates:\n")
cat("beta_0 =", beta_0_est, "\n")
cat("beta_1 =", beta_1_est, "\n")
cat("beta_2 =", beta_2_est, "\n")
cat("beta_3 =", beta_3_est, "\n")
cat("beta_4 =", beta_4_est, "\n")
cat("beta_5 =", beta_5_est, "\n")
cat("beta_6 =", beta_6_est, "\n")
cat("theta   =", theta_est, "\n")

###Observed Vs Fitted value Plot 
fitted_pois <- fitted(glm(
  Y ~ X1_s + X2_s + X3_s + X4_s + X5_s + X6_s,
  family = poisson(link="log")
))

plot(
  fitted_pois,
  D[,1],
  xlab="Fitted Values",
  ylab="Observed Values",
  main="Observed vs Fitted Values (PoGM)"
)
abline(0,1,col="red",lwd=2)

#### Neagtive Binomal Generalized Model
fitted_pois <- fitted(glm(
  Y ~ X1_s + X2_s + X3_s + X4_s + X5_s + X6_s,
  family = poisson(link="log")
))

plot(
  fitted_pois,
  D[,1],
  xlab="Fitted Values",
  ylab="Observed Values",
  main="Observed vs Fitted Values (PoGM)"
)
abline(0,1,col="red",lwd=2)

#############################################################
#### Pearson Residual Plot
#########################################
pearson_pois <- residuals(
  glm(
    D[,1] ~ X1_s + X2_s + X3_s + X4_s + X5_s + X6_s,
    family=poisson
  ),
  type="pearson"
)

plot(
  pearson_pois,
  ylab="Pearson Residuals",
  xlab="Observation",
  main="Pearson Residuals (PoGM)"
)

abline(h=0,col="red",lwd=2)
####Negative Binomial Generalized Model
pearson_nb <- residuals(
  fit_nb,
  type="pearson"
)

plot(
  pearson_nb,
  ylab="Pearson Residuals",
  xlab="Observation",
  main="Pearson Residuals (NBGM)"
)

abline(h=0,col="red",lwd=2)

#######################################
##Residual ACF plot
#######################################
acf(
  pearson_pois,
  main="ACF of Pearson Residuals (PoGM)"
)
acf(
  pearson_nb,
  main="ACF of Pearson Residuals (NBGM)"

#############################################################
#### Cameron-Trivedi Overdipersion Test 
#########################################
install.packages("AER")
library(AER)
pois_fit <- glm(
  D[,1] ~ X1_s + X2_s + X3_s + X4_s + X5_s + X6_s,
  family=poisson
)

dispersiontest(pois_fit)

#################################
##Likelihood Ratio test
########################
library(lmtest)

lrtest(
  pois_fit,
  fit_nb
)

###############################################
######AIC and BIC Comparison Table
###########################################
data.frame(
  Model=c("Poisson","Negative Binomial"),
  LogLik=c(logLik(pois_fit),
           logLik(fit_nb)),
  AIC=c(AIC(pois_fit),
        AIC(fit_nb)),
  BIC=c(BIC(pois_fit),
        BIC(fit_nb))
)


