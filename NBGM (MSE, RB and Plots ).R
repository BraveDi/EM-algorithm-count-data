########################################################
# NEGATIVE BINOMIAL MODEL
# RELATIVE BIAS + MSE + PLOTS
########################################################

library(readxl)
library(ggplot2)

########################################################
# READ EXCEL FILE
########################################################

raw <- read_excel(
  "NBGM.xlsx",
  col_names = FALSE
)

########################################################
# TRUE VALUES
########################################################

truth <- data.frame(
  
  Setting = paste("Setting",1:10),
  
  beta0 = c(
    0.1,0.3,0.4,0.5,0.7,
    0.8,0.9,1.0,1.5,2.0
  ),
  
  beta1 = c(
    0.2,0.4,0.5,0.6,0.7,
    0.8,0.9,1.0,1.5,3.0
  ),
  
  beta2 = c(
    0.3,0.5,0.6,0.7,0.7,
    0.8,0.9,1.0,1.5,4.0
  ),
  
  theta = c(
    0.1,0.2,0.3,0.4,0.5,
    0.6,0.7,0.8,0.9,1.0
  )
)

########################################################
# SAMPLE SIZES
########################################################

n_values <- c(
  50,
  100,
  200,
  500,
  1000,
  5000
)

########################################################
# CREATE RESULTS DATA FRAME
########################################################

results <- data.frame()

########################################################
# SETTINGS
########################################################

setting_names <- paste(
  "Setting",
  1:10
)

########################################################
# COLUMN POSITIONS
########################################################

estimate_cols <- c(
  8,10,12,14,16,
  18,20,22,24,26
)

se_cols <- c(
  9,11,13,15,17,
  19,21,23,25,27
)

########################################################
# ROW POSITIONS
########################################################

start_rows <- c(
  4,
  8,
  12,
  16,
  20,
  24
)

########################################################
# EXTRACT DATA
########################################################

for(i in 1:10){
  
  for(j in 1:6){
    
    r1 <- start_rows[j]
    r2 <- r1 + 1
    r3 <- r1 + 2
    r4 <- r1 + 3
    
    tmp <- data.frame(
      
      Setting =
        setting_names[i],
      
      n =
        n_values[j],
      
      Parameter =
        c(
          "beta0",
          "beta1",
          "beta2",
          "theta"
        ),
      
      Estimate =
        c(
          
          as.numeric(
            raw[r1,
                estimate_cols[i]]
          ),
          
          as.numeric(
            raw[r2,
                estimate_cols[i]]
          ),
          
          as.numeric(
            raw[r3,
                estimate_cols[i]]
          ),
          
          as.numeric(
            raw[r4,
                estimate_cols[i]]
          )
        ),
      
      SE =
        c(
          
          as.numeric(
            raw[r1,
                se_cols[i]]
          ),
          
          as.numeric(
            raw[r2,
                se_cols[i]]
          ),
          
          as.numeric(
            raw[r3,
                se_cols[i]]
          ),
          
          as.numeric(
            raw[r4,
                se_cols[i]]
          )
        )
    )
    
    results <- rbind(
      results,
      tmp
    )
  }
}

########################################################
# ADD TRUE VALUES
########################################################

results$True <- NA

for(i in 1:nrow(results)){
  
  s <- results$Setting[i]
  p <- results$Parameter[i]
  
  if(p=="beta0"){
    
    results$True[i] <-
      truth$beta0[
        truth$Setting==s
      ]
  }
  
  if(p=="beta1"){
    
    results$True[i] <-
      truth$beta1[
        truth$Setting==s
      ]
  }
  
  if(p=="beta2"){
    
    results$True[i] <-
      truth$beta2[
        truth$Setting==s
      ]
  }
  
  if(p=="theta"){
    
    results$True[i] <-
      truth$theta[
        truth$Setting==s
      ]
  }
}

########################################################
# RELATIVE BIAS
########################################################

results$RelativeBias <-
  (results$Estimate -
     results$True) /
  results$True * 100

########################################################
# BIAS
########################################################

results$Bias <-
  results$Estimate -
  results$True

########################################################
# MSE
########################################################

results$MSE <-
  results$Bias^2 +
  results$SE^2

########################################################
# SAVE TABLE
########################################################

write.csv(
  results,
  "NBGM_Bias_MSE.csv",
  row.names = FALSE
)

########################################################
# PLOT 1
########################################################

p1 <- ggplot(
  results,
  aes(
    x = n,
    y = RelativeBias,
    colour = Parameter,
    group = Parameter
  )
)+
  geom_line()+
  geom_point(size=2)+
  facet_wrap(
    ~Setting,
    ncol = 5
  )+
  theme_bw()+
  labs(
    title =
      "Relative Bias Across Simulation Settings",
    x =
      "Sample Size",
    y =
      "Relative Bias (%)"
  )

print(p1)

ggsave(
  "NBGM_Relative_Bias.png",
  p1,
  width = 12,
  height = 8,
  dpi = 300
)

########################################################
# PLOT 2
########################################################

p2 <- ggplot(
  results,
  aes(
    x = n,
    y = MSE,
    colour = Parameter,
    group = Parameter
  )
)+
  geom_line()+
  geom_point(size=2)+
  facet_wrap(
    ~Setting,
    ncol = 5
  )+
  theme_bw()+
  labs(
    title =
      "MSE Across Simulation Settings",
    x =
      "Sample Size",
    y =
      "MSE"
  )

print(p2)

ggsave(
  "NBGM_MSE.png",
  p2,
  width = 12,
  height = 8,
  dpi = 300
)

########################################################
# PLOT 3
########################################################

p3 <- ggplot(
  results,
  aes(
    x = n,
    y = SE,
    colour = Parameter,
    group = Parameter
  )
)+
  geom_line()+
  geom_point(size=2)+
  facet_wrap(
    ~Setting,
    ncol = 5
  )+
  theme_bw()+
  labs(
    title =
      "Standard Error Across Simulation Settings",
    x =
      "Sample Size",
    y =
      "Standard Error"
  )

print(p3)

ggsave(
  "NBGM_SE.png",
  p3,
  width = 12,
  height = 8,
  dpi = 300
)


