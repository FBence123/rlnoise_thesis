### Script for analysis of choice sensitivity

# Call libraries

library(R.matlab)
library(tidyverse)
library(ggplot2)
library(ggsci)
library(data.table)

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")

# Load preprocessed data from mat files and wrangle it to a proper format

data <- readMat("partial_sens_data.mat")
data <- data$sens.data

human <- data[[1]]
mod01 <- data[[2]]
mod20 <- data[[3]]
mod21 <- data[[4]]

# Median split prediction errors
# Logistic regression of choices based on Q differences, separately for large and small PEs on the previous trial
Coefs <- matrix(0L, nrow = 175,ncol = 2)
Ps <- matrix(0L, nrow = 175,ncol = 2)
for (i in 1:175) {
  PE <- as.numeric(mod21[[2]][i,])
  PE[is.na(PE)] <- 0
  PE_t_1 <- ifelse(abs(PE) >= median(abs(PE),na.rm = T), 1, 0)
  Actions <- mod21[[1]][i,]
  Q_diff <- mod21[[3]][i,]
  regdat <- data.frame(PE_t_1,Actions,Q_diff)
  #regdat$PE_t_1 <- shift(regdat$PE_t_1,1)
  regdat <- regdat[-c(1,73),]
  regdat$Actions <- recode(regdat$Actions, "2" = 0)
  regdat_small <- regdat[regdat$PE_t_1 == 0,]
  regdat_large <- regdat[regdat$PE_t_1 == 1,]
  mod_s <- glm(Actions ~ 0 + Q_diff, data=regdat_small, family = binomial)
  mod_l <- glm(Actions ~ 0 + Q_diff, data=regdat_large, family = binomial)
  Coefs[i,1] <- 1 / coef(summary(mod_s))[1,1]
  Coefs[i,2] <- 1 / coef(summary(mod_l))[1,1]
  Ps[i,1] <- coef(summary(mod_s))[1,4]
  Ps[i,2] <- coef(summary(mod_l))[1,4]
  i <- i+1
}

# Create data for plotting

Coefs <- as.data.frame(Coefs)
Coefs <- gather(Coefs,PE,Coef,V1:V2)
Ps <- as.data.frame(Ps)
Ps <- gather(Ps,PE,P,V1:V2)
Coefs$P <- Ps$P
Coefs$PE <- factor(Coefs$PE, levels = c("V1","V2"), labels = c("Small", "Large"))
Coefs <- mutate(Coefs,
                   idx = case_when(
                     P < 0.05 & PE == "Small" ~ 1, 
                     P < 0.05 & PE == "Large" ~ 2,
                     P >= 0.05 ~ 0
                   ))
Coefs$idx <- as.factor(Coefs$idx)
summary(Coefs$idx)
Coefs <- Coefs[abs(Coefs$Coef) < median(Coefs$Coef)+10*mad(Coefs$Coef),]
Coefs <- Coefs[Coefs$idx != 0,]

# Plot

ggplot(Coefs,aes(PE,Coef,fill = PE)) + geom_violin(linetype = 0, alpha = 0.4) + geom_jitter(aes(color = PE), size = 3, width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  theme_classic(base_size = 30) +  theme(legend.position = "none") +
  xlab(expression(PE[t-1])) + ylab("Choice temperature") + ggtitle("Noisy softmax") +
  scale_fill_manual(values = c("#DA7872","#BA5952")) +
  scale_color_manual(values = c("#DA7872","#BA5952"))

wilcox.test(Coefs$Coef ~ Coefs$PE)
median(Coefs$Coef[Coefs$PE == "Small"])
mad(Coefs$Coef[Coefs$PE == "Small"])
median(Coefs$Coef[Coefs$PE == "Large"])
mad(Coefs$Coef[Coefs$PE == "Large"])

t.test(Coefs$Coef ~ Coefs$PE)
sd(Coefs$Coef[Coefs$PE == "Small"])
sd(Coefs$Coef[Coefs$PE == "Large"])
