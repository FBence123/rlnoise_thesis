# Call libraries

library(R.matlab)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(data.table)
library(ggsci)
library(readr)
library(faux)

# Load preprocessed data from mat files and wrangle it to a proper format

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")
simparams <- readMat("simparams.mat")
simparams <- as.data.frame(simparams$simparams)
names(simparams) <- c("Sim par alpha","Sim par delta","Sim par zeta","Sim par tau",
                   "Sim comp alpha","Sim comp zeta","Sim comp tau",
                   "Rec par alpha","Rec par delta","Rec par zeta","Rec par tau",
                   "Rec comp alpha","Rec comp zeta","Rec comp tau", "idx")
params <- readMat("params.mat")
params <- as.data.frame(params$params)
names(params) <- c("p_alpha","p_delta","p_zeta","p_tau","p_switches",
                   "p_nongreedy","p_frac_noise","c_alpha","c_zeta","c_tau",
                   "c_switches","c_nongreedy","c_frac_noise","p_alpha_std",
                   "c_alpha_std","p_delta_std","p_zeta_std","c_zeta_std",
                   "p_tau_std","c_tau_std","idx")
data = params[params$idx == 1,c(1:4,8:10)]

# Confusion matrix

corm <- cor(simparams[,1:7],simparams[,8:14])
corrplot(corm,method = "color",tl.col = "black")

###### Alpha ######

corm[1,1]
corm[5,5]

var_par_noise <- sum((simparams[,1] - simparams[,8])^2) / 195
var_comp_noise <- sum((simparams[,5] - simparams[,12])^2) / 195
mu_par <- mean(simparams[,1])
mu_comp <- mean(simparams[,5])
sd_par <- sd(data$p_alpha)
sd_comp <- sd(data$c_alpha)
var_par <- sd_par^2
var_comp <- sd_comp^2
pardev <- sqrt(var_par_noise)
compdev <- sqrt(var_comp_noise)

A <- sqrt((1 + (var_par_noise/var_par))*(1 + (var_comp_noise/var_comp)))
r_corr <- 0.406*A

rcor <- c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99)
cor_ori <- matrix(data=NA,nrow=1000,ncol=length(rcor))
cor_noise <- matrix(data=NA,nrow=1000,ncol=length(rcor))

for (i in 1:nrow(cor_ori)){
  for (j in 1:ncol(cor_ori)){
    
    data_ori <- rnorm_multi(n = nrow(simparams),mu = c(mu_par,mu_comp),sd = c(sd_par,sd_comp),r = rcor[j])
    cor_ori[i,j] <- cor(data_ori)[1,2]
    
    data_noise <- data_ori
    data_noise[,1] <- data_noise[,1] + rnorm(nrow(data_noise),0,pardev)
    data_noise[,2] <- data_noise[,2] + rnorm(nrow(data_noise),0,compdev)
    cor_noise[i,j] <- cor(data_noise)[1,2]
  }
}

cor_ori <- colMeans(cor_ori)
cor_noise <- colMeans(cor_noise)
cor_mat <- as.data.frame(cbind(cor_ori,cor_noise,rcor))
names(cor_mat) <- c("Original","Noisy","Simulated")

ggplot(cor_mat,aes(Simulated,Noisy)) + geom_point(size = 3) + geom_line(size = 1) +
  theme_classic(base_size = 20) + xlim(c(0,1)) + ylim(0,1) + ylab("Noise corrupted r") +
  xlab("Simulated r") + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),linetype = "dashed") +
  geom_hline(yintercept = 0.406,linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(0,1.1,0.1)) + scale_y_continuous(breaks=seq(0,1.1,0.1)) +
  ggtitle(expression(paste(alpha," across conditions")))

###### Zeta ######

corm[3,3]
corm[6,6]

var_par_noise <- sum((simparams[,3] - simparams[,10])^2) / 195
var_comp_noise <- sum((simparams[,6] - simparams[,13])^2) / 195
mu_par <- mean(simparams[,3])
mu_comp <- mean(simparams[,6])
sd_par <- sd(data$p_zeta)
sd_comp <- sd(data$c_zeta)
var_par <- sd_par^2
var_comp <- sd_comp^2
pardev <- sqrt(var_par_noise)
compdev <- sqrt(var_comp_noise)

A <- sqrt((1 + (var_par_noise/var_par))*(1 + (var_comp_noise/var_comp)))
r_corr <- 0.418*A

rcor <- c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99)
cor_ori <- matrix(data=NA,nrow=1000,ncol=length(rcor))
cor_noise <- matrix(data=NA,nrow=1000,ncol=length(rcor))

for (i in 1:nrow(cor_ori)){
  for (j in 1:ncol(cor_ori)){
    
    data_ori <- rnorm_multi(n = nrow(simparams),mu = c(mu_par,mu_comp),sd = c(sd_par,sd_comp),r = rcor[j])
    cor_ori[i,j] <- cor(data_ori)[1,2]
    
    data_noise <- data_ori
    data_noise[,1] <- data_noise[,1] + rnorm(nrow(data_noise),0,pardev)
    data_noise[,2] <- data_noise[,2] + rnorm(nrow(data_noise),0,compdev)
    cor_noise[i,j] <- cor(data_noise)[1,2]
  }
}

cor_ori <- colMeans(cor_ori)
cor_noise <- colMeans(cor_noise)
cor_mat <- as.data.frame(cbind(cor_ori,cor_noise,rcor))
names(cor_mat) <- c("Original","Noisy","Simulated")

ggplot(cor_mat,aes(Simulated,Noisy)) + geom_point(size = 3) + geom_line(size = 1) +
  theme_classic(base_size = 20) + xlim(c(0,1)) + ylim(0,1) + ylab("Noise corrupted r") +
  xlab("Simulated r") + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),linetype = "dashed") +
  geom_hline(yintercept = 0.418,linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(0,1.1,0.1)) + scale_y_continuous(breaks=seq(0,1.1,0.1)) +
  ggtitle(expression(paste(zeta," across conditions")))

###### Alpha and zeta partial ######

var_alpha_noise <- sum((simparams[,1] - simparams[,8])^2) / 195
var_zeta_noise <- sum((simparams[,3] - simparams[,10])^2) / 195
mu_alpha <- mean(simparams[,1])
mu_zeta <- mean(simparams[,3])
sd_alpha <- sd(data$p_alpha)
sd_zeta <- sd(data$p_zeta)
var_alpha <- sd_par^2
var_zeta <- sd_zeta^2
alphadev <- sqrt(var_alpha_noise)
zetadev <- sqrt(var_zeta_noise)

A <- sqrt((1 + (var_alpha_noise/var_alpha))*(1 + (var_zeta_noise/var_zeta)))
r_corr <- 0.325*A

rcor <- c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99)
cor_ori <- matrix(data=NA,nrow=1000,ncol=length(rcor))
cor_noise <- matrix(data=NA,nrow=1000,ncol=length(rcor))

for (i in 1:nrow(cor_ori)){
  for (j in 1:ncol(cor_ori)){
    
    data_ori <- rnorm_multi(n = nrow(simparams),mu = c(mu_alpha,mu_zeta),sd = c(sd_alpha,sd_zeta),r = rcor[j])
    cor_ori[i,j] <- cor(data_ori)[1,2]
    
    data_noise <- data_ori
    data_noise[,1] <- data_noise[,1] + rnorm(nrow(data_noise),0,alphadev)
    data_noise[,2] <- data_noise[,2] + rnorm(nrow(data_noise),0,zetadev)
    cor_noise[i,j] <- cor(data_noise)[1,2]
  }
}

cor_ori <- colMeans(cor_ori)
cor_noise <- colMeans(cor_noise)
cor_mat <- as.data.frame(cbind(cor_ori,cor_noise,rcor))
names(cor_mat) <- c("Original","Noisy","Simulated")

ggplot(cor_mat,aes(Simulated,Noisy)) + geom_point(size = 3) + geom_line(size = 1) +
  theme_classic(base_size = 20) + xlim(c(0,1)) + ylim(0,1) + ylab("Noise corrupted r") +
  xlab("Simulated r") + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),linetype = "dashed") +
  geom_hline(yintercept = 0.325,linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(0,1.1,0.1)) + scale_y_continuous(breaks=seq(0,1.1,0.1)) +
  ggtitle(expression(paste(alpha," and ",zeta," in partial")))

###### Alpha and zeta complete ######

var_alpha_noise <- sum((simparams[,5] - simparams[,12])^2) / 195
var_zeta_noise <- sum((simparams[,6] - simparams[,13])^2) / 195
mu_alpha <- mean(simparams[,5])
mu_zeta <- mean(simparams[,6])
sd_alpha <- sd(data$c_alpha)
sd_zeta <- sd(data$c_zeta)
var_alpha <- sd_par^2
var_zeta <- sd_zeta^2
alphadev <- sqrt(var_alpha_noise)
zetadev <- sqrt(var_zeta_noise)

A <- sqrt((1 + (var_alpha_noise/var_alpha))*(1 + (var_zeta_noise/var_zeta)))
r_corr <- 0.384*A

rcor <- c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99)
cor_ori <- matrix(data=NA,nrow=1000,ncol=length(rcor))
cor_noise <- matrix(data=NA,nrow=1000,ncol=length(rcor))

for (i in 1:nrow(cor_ori)){
  for (j in 1:ncol(cor_ori)){
    
    data_ori <- rnorm_multi(n = nrow(simparams),mu = c(mu_alpha,mu_zeta),sd = c(sd_alpha,sd_zeta),r = rcor[j])
    cor_ori[i,j] <- cor(data_ori)[1,2]
    
    data_noise <- data_ori
    data_noise[,1] <- data_noise[,1] + rnorm(nrow(data_noise),0,alphadev)
    data_noise[,2] <- data_noise[,2] + rnorm(nrow(data_noise),0,zetadev)
    cor_noise[i,j] <- cor(data_noise)[1,2]
  }
}

cor_ori <- colMeans(cor_ori)
cor_noise <- colMeans(cor_noise)
cor_mat <- as.data.frame(cbind(cor_ori,cor_noise,rcor))
names(cor_mat) <- c("Original","Noisy","Simulated")

ggplot(cor_mat,aes(Simulated,Noisy)) + geom_point(size = 3) + geom_line(size = 1) +
  theme_classic(base_size = 20) + xlim(c(0,1)) + ylim(0,1) + ylab("Noise corrupted r") +
  xlab("Simulated r") + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),linetype = "dashed") +
  geom_hline(yintercept = 0.384,linetype = "dashed", color = "red") +
  scale_x_continuous(breaks=seq(0,1.1,0.1)) + scale_y_continuous(breaks=seq(0,1.1,0.1)) +
  ggtitle(expression(paste(alpha," and ",zeta," in complete")))
