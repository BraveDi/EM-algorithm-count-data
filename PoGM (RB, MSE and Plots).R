library(readxl)
library(ggplot2)

raw <- read_excel(
  "PoGM.xlsx",
  col_names = FALSE
)

####################################################
# TRUE VALUES
####################################################

truth <- data.frame(
  Setting = paste("Setting",1:10),
  beta0 = c(0.1,0.3,0.4,0.5,0.7,0.8,0.9,1.0,1.5,2.0),
  beta1 = c(0.2,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.5,3.0),
  beta2 = c(0.3,0.5,0.6,0.7,0.7,0.8,0.9,1.0,1.5,4.0)
)

####################################################
# SAMPLE SIZES
####################################################

n_values <- c(50,100,200,500,1000,5000)

####################################################
# EXTRACT RESULTS
####################################################

results <- data.frame()

setting_names <- paste("Setting",1:10)

# columns for settings
estimate_cols <- c(8,10,12,14,16,18,20,22,24,26)
se_cols       <- c(9,11,13,15,17,19,21,23,25,27)

# rows for sample sizes
start_rows <- c(4,7,10,13,16,19)

for(i in 1:10){
  
  for(j in 1:6){
    
    r1 <- start_rows[j]
    r2 <- r1 + 1
    r3 <- r1 + 2
    
    tmp <- data.frame(
      Setting = setting_names[i],
      n = n_values[j],
      Parameter = c("beta0","beta1","beta2"),
      Estimate = c(
        as.numeric(raw[r1,estimate_cols[i]]),
        as.numeric(raw[r2,estimate_cols[i]]),
        as.numeric(raw[r3,estimate_cols[i]])
      ),
      SE = c(
        as.numeric(raw[r1,se_cols[i]]),
        as.numeric(raw[r2,se_cols[i]]),
        as.numeric(raw[r3,se_cols[i]])
      )
    )
    
    results <- rbind(results,tmp)
  }
}

####################################################
# ADD TRUE VALUES
####################################################

results$True <- NA

for(i in 1:nrow(results)){
  
  s <- results$Setting[i]
  p <- results$Parameter[i]
  
  if(p=="beta0"){
    results$True[i] <- truth$beta0[
      truth$Setting==s
    ]
  }
  
  if(p=="beta1"){
    results$True[i] <- truth$beta1[
      truth$Setting==s
    ]
  }
  
  if(p=="beta2"){
    results$True[i] <- truth$beta2[
      truth$Setting==s
    ]
  }
}

####################################################
# RELATIVE BIAS
####################################################

results$RelativeBias <-
  (results$Estimate -
     results$True)/
  results$True *100

####################################################
# MSE
####################################################

results$Bias <-
  results$Estimate -
  results$True

results$MSE <-
  results$Bias^2 +
  results$SE^2

####################################################
# SAVE TABLE
####################################################

write.csv(
  results,
  "PoGM_Bias_MSE.csv",
  row.names = FALSE
)

####################################################
# PLOT 1 : RELATIVE BIAS
####################################################

ggplot(
  results,
  aes(
    x=n,
    y=RelativeBias,
    colour=Parameter,
    group=Parameter
  )
)+
  geom_line()+
  geom_point(size=2)+
  facet_wrap(~Setting,ncol=5)+
  theme_bw()+
  labs(
    title="Relative Bias Across Simulation Settings",
    x="Sample Size",
    y="Relative Bias (%)"
  )

ggsave(
  "Relative_Bias.png",
  width=12,
  height=8,
  dpi=300
)

####################################################
# PLOT 2 : MSE
####################################################

ggplot(
  results,
  aes(
    x=n,
    y=MSE,
    colour=Parameter,
    group=Parameter
  )
)+
  geom_line()+
  geom_point(size=2)+
  facet_wrap(~Setting,ncol=5)+
  theme_bw()+
  labs(
    title="MSE Across Simulation Settings",
    x="Sample Size",
    y="MSE"
  )

ggsave(
  "MSE.png",
  width=12,
  height=8,
  dpi=300
)

####################################################
# PLOT 3 : STANDARD ERROR
####################################################

ggplot(
  results,
  aes(
    x=n,
    y=SE,
    colour=Parameter,
    group=Parameter
  )
)+
  geom_line()+
  geom_point(size=2)+
  facet_wrap(~Setting,ncol=5)+
  theme_bw()+
  labs(
    title="Standard Error Across Simulation Settings",
    x="Sample Size",
    y="Standard Error"
  )

ggsave(
  "SE.png",
  width=12,
  height=8,
  dpi=300
)

