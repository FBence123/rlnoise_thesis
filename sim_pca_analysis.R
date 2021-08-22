### Script for analyses of the simulated data with PCA derived parameters
##### Setup #####
# Call libraries

library(R.matlab)
library(tidyverse)
library(ggplot2)
library(data.table)
library(ggsci)
library(lme4)
library(wesanderson)

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")

dim1h <- readMat('Dim1_high_partial.mat')
dim1l <- readMat('Dim1_low_partial.mat')
dim2h <- readMat('Dim2_high_partial.mat')
dim2l <- readMat('Dim2_low_partial.mat')

###### Plot fraction switches ######

V1 <- rowSums(dim1h$pca.simdat[[5]])
temp <- as.data.frame(t(rbind(V1,rowSums(dim1l$pca.simdat[[5]]),rowSums(dim2h$pca.simdat[[5]]),rowSums(dim2l$pca.simdat[[5]]))))
temp <- temp / 144

dim1 <- gather(temp,Dim1,switches,1:2)[3:4]
dim1$Dim1 <- factor(dim1$Dim1, levels = c("V2","V1"), labels = c("Low", "High"))

ggplot(dim1,aes(Dim1,switches)) + geom_violin(aes(fill = Dim1),linetype = 0, alpha = 0.4) + geom_jitter(aes(color = Dim1),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  theme_classic(base_size = 20) + theme(legend.position = "none") + ylab("Fraction of reversals") + xlab("PC 1") +
  scale_fill_manual(values = wes_palette("Chevalier1")) + scale_color_manual(values = wes_palette("Chevalier1")) + ylim(c(0,0.5))
wilcox.test(dim1$switches ~ dim1$Dim1)
median(dim1$switches[dim1$Dim1 == "Low"])
mad(dim1$switches[dim1$Dim1 == "Low"])
median(dim1$switches[dim1$Dim1 == "High"])
mad(dim1$switches[dim1$Dim1 == "High"])

dim2 <- gather(temp,Dim2,switches,3:4)[3:4]
dim2$Dim2 <- factor(dim2$Dim2, levels = c("V4","V3"), labels = c("Low", "High"))

ggplot(dim2,aes(Dim2,switches)) + geom_violin(aes(fill = Dim2),linetype = 0, alpha = 0.4) + geom_jitter(aes(color = Dim2),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") +
  theme_classic(base_size = 20) + theme(legend.position = "none") + ylab("Fraction of reversals") + xlab("PC 2") +
  scale_fill_manual(values = wes_palette("Chevalier1")) + scale_color_manual(values = wes_palette("Chevalier1")) + ylim(c(0,0.5))
wilcox.test(dim2$switches ~ dim2$Dim2)
median(dim2$switches[dim2$Dim2 == "Low"])
mad(dim2$switches[dim2$Dim2 == "Low"])
median(dim2$switches[dim2$Dim2 == "High"])
mad(dim2$switches[dim2$Dim2 == "High"])


###### Plot fraction nongreedy ######

V1 <- rowSums(dim1h$pca.simdat[[1]])
temp <- as.data.frame(t(rbind(V1,rowSums(dim1l$pca.simdat[[1]]),rowSums(dim2h$pca.simdat[[1]]),rowSums(dim2l$pca.simdat[[1]]))))
temp <- temp / 144

dim1 <- gather(temp,Dim1,nongreedy,1:2)[3:4]
dim1$Dim1 <- factor(dim1$Dim1, levels = c("V2","V1"), labels = c("Low", "High"))

ggplot(dim1,aes(Dim1,nongreedy)) + geom_violin(aes(fill = Dim1),linetype = 0, alpha = 0.4) + geom_jitter(aes(color = Dim1),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + theme_classic(base_size = 20) +
  theme(legend.position = "none") + ylab("Fraction of suboptimal choices") + xlab("PC 1") +
  scale_fill_manual(values = wes_palette("Chevalier1")) + scale_color_manual(values = wes_palette("Chevalier1")) + ylim(c(0,0.6))
wilcox.test(dim1$nongreedy ~ dim1$Dim1)
median(dim1$nongreedy[dim1$Dim1 == "Low"])
mad(dim1$nongreedy[dim1$Dim1 == "Low"])
median(dim1$nongreedy[dim1$Dim1 == "High"])
mad(dim1$nongreedy[dim1$Dim1 == "High"])

dim2 <- gather(temp,Dim2,nongreedy,3:4)[3:4]
dim2$Dim2 <- factor(dim2$Dim2, levels = c("V4","V3"), labels = c("Low", "High"))

ggplot(dim2,aes(Dim2,nongreedy)) + geom_violin(aes(fill = Dim2),linetype = 0, alpha = 0.4) + geom_jitter(aes(color = Dim2),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + theme_classic(base_size = 20) +
  theme(legend.position = "none") + ylab("Fraction of suboptimal choices") + xlab("PC 2") +
  scale_fill_manual(values = wes_palette("Chevalier1")) + scale_color_manual(values = wes_palette("Chevalier1")) + ylim(c(0,0.6))
wilcox.test(dim2$nongreedy ~ dim2$Dim2)
median(dim2$nongreedy[dim2$Dim2 == "Low"])
mad(dim2$nongreedy[dim2$Dim2 == "Low"])
median(dim2$nongreedy[dim2$Dim2 == "High"])
mad(dim2$nongreedy[dim2$Dim2 == "High"])


###### Plot fraction noisy ######

temp <- as.data.frame(t(rbind(dim1h$pca.simdat[[2]],dim1l$pca.simdat[[2]],dim2h$pca.simdat[[2]],dim2l$pca.simdat[[2]])))

dim1 <- gather(temp,Dim1,noisy,1:2)[3:4]
dim1$Dim1 <- factor(dim1$Dim1, levels = c("V2","V1"), labels = c("Low", "High"))

ggplot(dim1,aes(Dim1,noisy)) + geom_violin(aes(fill = Dim1),linetype = 0, alpha = 0.4) + geom_jitter(aes(color = Dim1),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + theme_classic(base_size = 20) +
  theme(legend.position = "none") + ylab("Fraction of suboptimal choices\nexplained by noise") + xlab("PC 1") +
  scale_fill_manual(values = wes_palette("Chevalier1")) + scale_color_manual(values = wes_palette("Chevalier1")) + ylim(c(0,1))
wilcox.test(dim1$noisy ~ dim1$Dim1)
median(dim1$noisy[dim1$Dim1 == "Low"])
mad(dim1$noisy[dim1$Dim1 == "Low"])
median(dim1$noisy[dim1$Dim1 == "High"])
mad(dim1$noisy[dim1$Dim1 == "High"])

dim2 <- gather(temp,Dim2,noisy,3:4)[3:4]
dim2$Dim2 <- factor(dim2$Dim2, levels = c("V4","V3"), labels = c("Low", "High"))

ggplot(dim2,aes(Dim2,noisy)) + geom_violin(aes(fill = Dim2),linetype = 0, alpha = 0.4) + geom_jitter(aes(color = Dim2),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + theme_classic(base_size = 20) +
  theme(legend.position = "none") + ylab("Fraction of suboptimal choices\nexplained by noise") + xlab("PC 2") +
  scale_fill_manual(values = wes_palette("Chevalier1")) + scale_color_manual(values = wes_palette("Chevalier1")) + ylim(c(0,1))
wilcox.test(dim2$noisy ~ dim2$Dim2)
median(dim2$noisy[dim2$Dim2 == "Low"],na.rm = TRUE)
mad(dim2$noisy[dim2$Dim2 == "Low"],na.rm = TRUE)
median(dim2$noisy[dim2$Dim2 == "High"])
mad(dim2$noisy[dim2$Dim2 == "High"])

###### Reward sensitivity ######

# Logistic regression for Dim 1 high
regact <- dim1h$pca.simdat[[3]] == 1
regrew <- (3-2*dim1h$pca.simdat[[3]])*(dim1h$pca.simdat[[4]]-50)
sens_dim1h <- matrix(0L, nrow = nrow(regact),ncol = 1)
for(i in 1:nrow(regact)){
  regdat <- as.data.frame(as.numeric(regact[i,]))
  regdat$rew <- shift(regrew[i,],1)
  regdat <- regdat[-c(1,73),]
  names(regdat) <- c("resp","rew")
  mod <- glm(resp ~ rew, data = regdat, family = binomial)
  sens_dim1h[i] <- mod$coefficients[2]
}

# Logistic regression for Dim 1 low
regact <- dim1l$pca.simdat[[3]] == 1
regrew <- (3-2*dim1l$pca.simdat[[3]])*(dim1l$pca.simdat[[4]]-50)
sens_dim1l <- matrix(0L, nrow = nrow(regact),ncol = 1)
for(i in 1:nrow(regact)){
  regdat <- as.data.frame(as.numeric(regact[i,]))
  regdat$rew <- shift(regrew[i,],1)
  regdat <- regdat[-c(1,73),]
  names(regdat) <- c("resp","rew")
  mod <- glm(resp ~ rew, data = regdat, family = binomial)
  sens_dim1l[i] <- mod$coefficients[2]
}

# Logistic regression for Dim 2 high
regact <- dim2h$pca.simdat[[3]] == 1
regrew <- (3-2*dim2h$pca.simdat[[3]])*(dim2h$pca.simdat[[4]]-50)
sens_dim2h <- matrix(0L, nrow = nrow(regact),ncol = 1)
for(i in 1:nrow(regact)){
  regdat <- as.data.frame(as.numeric(regact[i,]))
  regdat$rew <- shift(regrew[i,],1)
  regdat <- regdat[-c(1,73),]
  names(regdat) <- c("resp","rew")
  mod <- glm(resp ~ rew, data = regdat, family = binomial)
  sens_dim2h[i] <- mod$coefficients[2]
}

# Logistic regression for Dim 2 low
regact <- dim2l$pca.simdat[[3]] == 1
regrew <- (3-2*dim2l$pca.simdat[[3]])*(dim2l$pca.simdat[[4]]-50)
sens_dim2l <- matrix(0L, nrow = nrow(regact),ncol = 1)
for(i in 1:nrow(regact)){
  regdat <- as.data.frame(as.numeric(regact[i,]))
  regdat$rew <- shift(regrew[i,],1)
  regdat <- regdat[-c(1,73),]
  names(regdat) <- c("resp","rew")
  mod <- glm(resp ~ rew, data = regdat, family = binomial)
  sens_dim2l[i] <- mod$coefficients[2]
}

dim1 <- data.frame(sens_dim1h,sens_dim1l)
dim1 <- gather(dim1,Dim1,rew_sens,1:2)
dim1$Dim1 <- factor(dim1$Dim1, levels = c("sens_dim1l","sens_dim1h"), labels = c("Low","High"))

ggplot(dim1,aes(Dim1,rew_sens)) + geom_violin(aes(fill = Dim1),linetype = 0, alpha = 0.4) + geom_jitter(aes(color = Dim1),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + theme_classic(base_size = 20) +
  theme(legend.position = "none") + ylab("Reward sensitivity") + xlab("PC 1") +
  scale_fill_manual(values = wes_palette("Chevalier1")) + scale_color_manual(values = wes_palette("Chevalier1")) + ylim(c(0,0.16))
wilcox.test(dim1$rew_sens ~ dim1$Dim1)
median(dim1$rew_sens[dim1$Dim1 == "Low"])
mad(dim1$rew_sens[dim1$Dim1 == "Low"])
median(dim1$rew_sens[dim1$Dim1 == "High"])
mad(dim1$rew_sens[dim1$Dim1 == "High"])

dim2 <- data.frame(sens_dim2h,sens_dim2l)
dim2 <- gather(dim2,Dim2,rew_sens,1:2)
dim2$Dim2 <- factor(dim2$Dim2, levels = c("sens_dim2l","sens_dim2h"), labels = c("Low","High"))

ggplot(dim2,aes(Dim2,rew_sens)) + geom_violin(aes(fill = Dim2),linetype = 0, alpha = 0.4) + geom_jitter(aes(color = Dim2),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") + theme_classic(base_size = 20) +
  theme(legend.position = "none") + ylab("Reward sensitivity") + xlab("PC 2") +
  scale_fill_manual(values = wes_palette("Chevalier1")) + scale_color_manual(values = wes_palette("Chevalier1")) + ylim(c(0,0.16))
wilcox.test(dim2$rew_sens ~ dim2$Dim2)
median(dim2$rew_sens[dim2$Dim2 == "Low"])
mad(dim2$rew_sens[dim2$Dim2 == "Low"])
median(dim2$rew_sens[dim2$Dim2 == "High"])
mad(dim2$rew_sens[dim2$Dim2 == "High"])
