### Script to compare the model parameters and some behavioural signatures
### of interest in the partial and complete information conditions.

###### Call libraries ######

library(R.matlab)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(data.table)
library(ggsci)

# Load preprocessed data from mat files and wrangle it to a proper format

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")
params <- readMat("params.mat")
params <- as.data.frame(params$params)
names(params) <- c("p_alpha","p_delta","p_zeta","p_tau","p_switches",
                   "p_nongreedy","p_frac_noise","c_alpha","c_zeta","c_tau",
                   "c_switches","c_nongreedy","c_frac_noise","p_alpha_std",
                   "c_alpha_std","p_delta_std","p_zeta_std","c_zeta_std",
                   "p_tau_std","c_tau_std","idx")
data = params[params$idx == 1,]
Rew_sens <- read_csv("Rew_sens.csv")
names(Rew_sens) <- c("Partial_sens","Complete_sens")

####### Fraction explained by noise across conditions ######
p1 <- gather(data,Condition,frac_noise,p_frac_noise,c_frac_noise)[,21]
plotdata <- as.data.frame(cbind(p1,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("frac_noise","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Condition,frac_noise)) + geom_violin(aes(fill = Condition),linetype = 0,alpha=0.4) +
  geom_jitter(aes(color = Condition),size = 3,width = 0.2) + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  theme_classic(base_size = 20) +  theme(legend.position = "none") +
  xlab("Condition") + ylab("Fraction of suboptimal choices\nexplained by noise")

wilcox.test(plotdata$frac_noise ~ plotdata$Condition)
median(data$p_frac_noise)
median(data$c_frac_noise)
mad(data$p_frac_noise)
mad(data$c_frac_noise)

####### Alpha across conditions ######
ggplot(data,aes(p_alpha,c_alpha)) + geom_errorbar(aes(xmin = p_alpha - p_alpha_std, xmax = p_alpha + p_alpha_std),color = "grey",size=1) +
  geom_errorbar(aes(ymin = c_alpha - c_alpha_std, ymax = c_alpha + c_alpha_std),color = "grey",size=1) +
  geom_point(color = "grey20", size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Partial learning rate ", alpha))) + ylab(expression(paste("Complete learning rate ", alpha)))

cor.test(data$p_alpha,data$c_alpha, method = "pearson")
cor.test(data$p_alpha,data$c_alpha, method = "spearman")

####### Zeta across conditions ######
ggplot(data,aes(p_zeta,c_zeta)) + geom_errorbar(aes(xmin = p_zeta - p_zeta_std, xmax = p_zeta + p_zeta_std),color = "grey",size=1) +
  geom_errorbar(aes(ymin = c_zeta - c_zeta_std, ymax = c_zeta + c_zeta_std),color = "grey",size=1) +
  geom_point(color = "grey20",size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Partial computational noise ", zeta))) + ylab(expression(paste("Complete computational noise ", zeta)))

cor.test(data$p_zeta,data$c_zeta, method = "pearson")
cor.test(data$p_zeta,data$c_zeta, method = "spearman")

####### Tau across conditions ######
ggplot(data,aes(p_tau,c_tau)) + geom_errorbar(aes(xmin = p_tau - p_tau_std, xmax = p_tau + p_tau_std),color = "grey",size=1) +
  geom_errorbar(aes(ymin = c_tau - c_tau_std, ymax = c_tau + c_tau_std),color = "grey",size=1) +
  geom_point(color = "grey20") + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Partial policy temperature ", tau))) + ylab(expression(paste("Complete policy temperature ", tau)))

cor.test(data$p_tau,data$c_tau, method = "spearman")
cor.test(data$p_tau,data$c_tau, method = "pearson")

####### Alpha and switches ######
p1 <- gather(data,Condition,Alpha,p_alpha,c_alpha)[,21]
p2 <- gather(data,Condition,Switches,p_alpha_std,c_alpha_std)[,21]
p3 <- gather(data,Condition,Switches,p_switches,c_switches)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Alpha", "Std", "Switches","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Alpha,Switches)) + geom_errorbar(aes(xmin = Alpha - Std, xmax = Alpha + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Learning rate ", alpha))) + ylab("Fraction of reversals") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_alpha,data$p_switches, method = "spearman")
cor.test(data$c_alpha,data$c_switches, method = "spearman")

####### Alpha and reward sensitivity ######
p1 <- gather(data,Condition,Alpha,p_alpha,c_alpha)[,21]
p2 <- gather(data,Condition,Std,p_alpha_std,c_alpha_std)[,21]
p3 <- gather(Rew_sens,Condition,Rew_sens,Partial_sens,Complete_sens)[,2]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Alpha", "Std", "Rew_sens","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Alpha,Rew_sens)) + geom_errorbar(aes(xmin = Alpha - Std, xmax = Alpha + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Learning rate ", alpha))) + ylab("Reward sensitivity") + facet_grid(. ~ Condition) +
  theme(legend.position = "none") + ylim(c(0,0.2))

cor.test(data$p_alpha,Rew_sens$Partial_sens, method = "spearman")
cor.test(data$c_alpha,Rew_sens$Complete_sens, method = "spearman")

####### Alpha and nongreedy ######
p1 <- gather(data,Condition,Alpha,p_alpha,c_alpha)[,21]
p2 <- gather(data,Condition,Std,p_alpha_std,c_alpha_std)[,21]
p3 <- gather(data,Condition,Nongreedy,p_nongreedy,c_nongreedy)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Alpha", "Std", "Nongreedy","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Alpha,Nongreedy)) + geom_errorbar(aes(xmin = Alpha - Std, xmax = Alpha + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Learning rate ", alpha))) + ylab("Fraction of suboptimal choices") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_alpha,data$p_nongreedy, method = "spearman")
cor.test(data$c_alpha,data$c_nongreedy, method = "spearman")

####### Alpha and noisy decisions ######
p1 <- gather(data,Condition,Alpha,p_alpha,c_alpha)[,21]
p2 <- gather(data,Condition,Std,p_alpha_std,c_alpha_std)[,21]
p3 <- gather(data,Condition,Noisy,p_frac_noise,c_frac_noise)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Alpha", "Std", "Noisy","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Alpha,Noisy)) + geom_errorbar(aes(xmin = Alpha - Std, xmax = Alpha + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Learning rate ", alpha))) + ylab("Fraction of suboptimal choices\nexplained by noise") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_alpha,data$p_frac_noise, method = "spearman")
cor.test(data$c_alpha,data$c_frac_noise, method = "spearman")

####### Zeta and switches ######
p1 <- gather(data,Condition,Zeta,p_zeta,c_zeta)[,21]
p2 <- gather(data,Condition,Std,p_zeta_std,c_zeta_std)[,21]
p3 <- gather(data,Condition,Switches,p_switches,c_switches)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Zeta", "Std", "Switches","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Zeta,Switches)) + geom_errorbar(aes(xmin = Zeta - Std, xmax = Zeta + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Computational noise ", zeta))) + ylab("Fraction of reversals") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_zeta,data$p_switches, method = "spearman")
cor.test(data$c_zeta,data$c_switches, method = "spearman")

####### Zeta and reward sensitivity ######
p1 <- gather(data,Condition,Zeta,p_zeta,c_zeta)[,21]
p2 <- gather(data,Condition,Std,p_zeta_std,c_zeta_std)[,21]
p3 <- gather(Rew_sens,Condition,Rew_sens,Partial_sens,Complete_sens)[,2]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Zeta", "Std", "Rew_sens","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Zeta,Rew_sens)) + geom_errorbar(aes(xmin = Zeta - Std, xmax = Zeta + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Computational noise ", zeta))) + ylab("Reward sensitivity") + facet_grid(. ~ Condition) +
  theme(legend.position = "none") + ylim(c(0,0.2))

cor.test(data$p_zeta,Rew_sens$Partial_sens, method = "spearman")
cor.test(data$c_zeta,Rew_sens$Complete_sens, method = "spearman")

####### Zeta and nongreedy ######
p1 <- gather(data,Condition,Zeta,p_zeta,c_zeta)[,21]
p2 <- gather(data,Condition,Std,p_zeta_std,c_zeta_std)[,21]
p3 <- gather(data,Condition,Nongreedy,p_nongreedy,c_nongreedy)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Zeta", "Std", "Nongreedy","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Zeta,Nongreedy)) + geom_errorbar(aes(xmin = Zeta - Std, xmax = Zeta + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Computational noise ", zeta))) + ylab("Fraction of suboptimal choices") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_zeta,data$p_nongreedy, method = "spearman")
cor.test(data$c_zeta,data$c_nongreedy, method = "spearman")

####### Zeta and noisy decisions ######
p1 <- gather(data,Condition,Zeta,p_zeta,c_zeta)[,21]
p2 <- gather(data,Condition,Std,p_zeta_std,c_zeta_std)[,21]
p3 <- gather(data,Condition,Noisy,p_frac_noise,c_frac_noise)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Zeta", "Std", "Noisy","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Zeta,Noisy)) + geom_errorbar(aes(xmin = Zeta - Std, xmax = Zeta + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Computational noise ", zeta))) + ylab("Fraction of suboptimal choices\nexplained by noise") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_zeta,data$p_frac_noise, method = "spearman")
cor.test(data$c_zeta,data$c_frac_noise, method = "spearman")

####### Tau and switches ######
p1 <- gather(data,Condition,Tau,p_tau,c_tau)[,21]
p2 <- gather(data,Condition,Std,p_tau_std,c_tau_std)[,21]
p3 <- gather(data,Condition,Switches,p_switches,c_switches)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Tau", "Std", "Switches","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Tau,Switches)) + geom_errorbar(aes(xmin = Tau - Std, xmax = Tau + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Policy temperature ", tau))) + ylab("Fraction of reversals") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_tau,data$p_switches, method = "spearman")
cor.test(data$c_tau,data$c_switches, method = "spearman")

####### Tau and reward sensitivity ######
p1 <- gather(data,Condition,Tau,p_tau,c_tau)[,21]
p2 <- gather(data,Condition,Std,p_tau_std,c_tau_std)[,21]
p3 <- gather(Rew_sens,Condition,Rew_sens,Partial_sens,Complete_sens)[,2]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Tau", "Std", "Rew_sens","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Tau,Rew_sens)) + geom_errorbar(aes(xmin = Tau - Std, xmax = Tau + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Policy temperature ", tau))) + ylab("Reward sensitivity") + facet_grid(. ~ Condition) +
  theme(legend.position = "none") + ylim(c(0,0.2))

cor.test(data$p_tau,Rew_sens$Partial_sens, method = "spearman")
cor.test(data$c_tau,Rew_sens$Complete_sens, method = "spearman")

####### Tau and nongreedy ######
p1 <- gather(data,Condition,Tau,p_tau,c_tau)[,21]
p2 <- gather(data,Condition,Std,p_tau_std,c_tau_std)[,21]
p3 <- gather(data,Condition,Nongreedy,p_nongreedy,c_nongreedy)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Tau", "Std", "Nongreedy","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Tau,Nongreedy)) + geom_errorbar(aes(xmin = Tau - Std, xmax = Tau + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Policy temperature ", tau))) + ylab("Fraction of suboptimal choices") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_tau,data$p_nongreedy, method = "spearman")
cor.test(data$c_tau,data$c_nongreedy, method = "spearman")

####### Tau and noisy decisions ######
p1 <- gather(data,Condition,Tau,p_tau,c_tau)[,21]
p2 <- gather(data,Condition,Std,p_tau_std,c_tau_std)[,21]
p3 <- gather(data,Condition,Noisy,p_frac_noise,c_frac_noise)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Tau", "Std", "Noisy","Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Tau,Noisy)) + geom_errorbar(aes(xmin = Tau - Std, xmax = Tau + Std, color = Condition), alpha = 0.4, size = 1) +
  geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Policy temperature ", tau))) + ylab("Fraction of suboptimal choices\nexplained by noise") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_tau,data$p_frac_noise, method = "spearman")
cor.test(data$c_tau,data$c_frac_noise, method = "spearman")

####### Alpha and zeta ######
p1 <- gather(data,Condition,Zeta,p_zeta,c_zeta)[,21]
p2 <- gather(data,Condition,Z_Std,p_zeta_std,c_zeta_std)[,21]
p3 <- gather(data,Condition,Alpha,p_alpha,c_alpha)[,21]
p4 <- gather(data,Condition,A_Std,p_alpha_std,c_alpha_std)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,p4,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Zeta", "Z_Std", "Alpha", "A_Std", "Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Zeta,Alpha)) + geom_errorbar(aes(xmin = Zeta - Z_Std, xmax = Zeta + Z_Std, color = Condition), alpha = 0.4, size = 1) +
  geom_errorbar(aes(ymin = Alpha - A_Std, ymax = Alpha + A_Std, color = Condition), alpha = 0.4, size = 1) + geom_point(aes(color = Condition), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Computational noise ", zeta))) + ylab(expression(paste("Learning rate ", alpha))) + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_zeta,data$p_alpha, method = "pearson")
cor.test(data$p_zeta,data$p_alpha, method = "spearman")
cor.test(data$c_zeta,data$c_alpha, method = "pearson")
cor.test(data$c_zeta,data$c_alpha, method = "spearman")

####### Alpha and tau ######
p1 <- gather(data,Condition,Tau,p_tau,c_tau)[,21]
p2 <- gather(data,Condition,T_Std,p_tau_std,c_tau_std)[,21]
p3 <- gather(data,Condition,Alpha,p_alpha,c_alpha)[,21]
p4 <- gather(data,Condition,A_Std,p_alpha_std,c_alpha_std)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,p4,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Tau", "T_Std", "Alpha", "A_Std", "Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Tau,Alpha,color = Condition)) + geom_errorbar(aes(xmin = Tau - T_Std, xmax = Tau + T_Std)) +
  geom_errorbar(aes(ymin = Alpha - A_Std, ymax = Alpha + A_Std)) + geom_point(color = "grey40") + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 18) + xlab("Tau") + ylab("Alpha") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_tau,data$p_alpha, method = "pearson")
cor.test(data$c_tau,data$c_alpha, method = "pearson")

####### Tau and zeta ######
p1 <- gather(data,Condition,Zeta,p_zeta,c_zeta)[,21]
p2 <- gather(data,Condition,Z_Std,p_zeta_std,c_zeta_std)[,21]
p3 <- gather(data,Condition,Tau,p_tau,c_tau)[,21]
p4 <- gather(data,Condition,T_Std,p_tau_std,c_tau_std)[,21]
plotdata <- as.data.frame(cbind(p1,p2,p3,p4,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Zeta", "Z_Std", "Tau", "T_Std", "Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Zeta,Tau,color = Condition)) + geom_errorbar(aes(xmin = Zeta - Z_Std, xmax = Zeta + Z_Std)) +
  geom_errorbar(aes(ymin = Tau - T_Std, ymax = Tau + T_Std)) + geom_point(color = "grey40") + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 18) + xlab("Zeta") + ylab("Tau") + facet_grid(. ~ Condition) +
  theme(legend.position = "none")

cor.test(data$p_zeta,data$p_tau, method = "spearman")
cor.test(data$c_zeta,data$c_tau, method = "spearman")

####### Delta and switches #######
plotdata <- select(data,p_delta,p_delta_std,p_switches)
names(plotdata) <- c("Delta", "Std", "Switches")

ggplot(plotdata,aes(Delta,Switches)) + geom_errorbar(aes(xmin = Delta - Std, xmax = Delta + Std), color = "#F8766D", alpha = 0.4, size = 1) +
  geom_point(aes(color = "#F8766D"), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Decay rate ", delta))) + ylab("Fraction of reversals") +
  theme(legend.position = "none")

cor.test(data$p_delta,data$p_switches, method = "spearman")

####### Delta and reward sensitivity #######
plotdata <- select(data,p_delta,p_delta_std)
plotdata$Rew_sens <- Rew_sens$Partial_sens
names(plotdata) <- c("Delta", "Std", "Rew_sens")

ggplot(plotdata,aes(Delta,Rew_sens)) + geom_errorbar(aes(xmin = Delta - Std, xmax = Delta + Std), color = "#F8766D", alpha = 0.4, size = 1) +
  geom_point(aes(color = "#F8766D"), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Decay rate ", delta))) + ylab("Reward sensitivity") +
  theme(legend.position = "none")

cor.test(data$p_delta,Rew_sens$Partial_sens, method = "spearman")

####### Delta and nongreedy #######
plotdata <- select(data,p_delta,p_delta_std,p_nongreedy)
names(plotdata) <- c("Delta", "Std", "Nongreedy")

ggplot(plotdata,aes(Delta,Nongreedy)) + geom_errorbar(aes(xmin = Delta - Std, xmax = Delta + Std), color = "#F8766D", alpha = 0.4, size = 1) +
  geom_point(aes(color = "#F8766D"), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Decay rate ", delta))) + ylab("Fraction of suboptimal choices") +
  theme(legend.position = "none")

cor.test(data$p_delta,data$p_nongreedy, method = "spearman")

####### Delta and noisy decisions #######
plotdata <- select(data,p_delta,p_delta_std,p_frac_noise)
names(plotdata) <- c("Delta", "Std", "Noisy")

ggplot(plotdata,aes(Delta,Noisy)) + geom_errorbar(aes(xmin = Delta - Std, xmax = Delta + Std), color = "#F8766D", alpha = 0.4, size = 1) +
  geom_point(aes(color = "#F8766D"), size = 3) + geom_smooth(method = "lm", linetype = "dashed",color = "grey20",se = F) +
  theme_classic(base_size = 20) + xlab(expression(paste("Decay rate ", delta))) + ylab("Fraction of suboptimal choices\nexplained by noise") +
  theme(legend.position = "none")

cor.test(data$p_delta,data$p_frac_noise, method = "spearman")

###### Parameters between conditions ######

p1 <- gather(data,Condition,Alpha,p_alpha,c_alpha)[,21]
plotdata <- as.data.frame(cbind(p1,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Alpha", "Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Condition,Alpha)) + geom_violin(aes(fill = Condition),linetype = 0,alpha=0.4) +
  geom_jitter(aes(color = Condition),size = 3,width = 0.2) + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  theme_classic(base_size = 20) +  theme(legend.position = "none") +
  xlab("Condition") + ylab(expression(paste("Learning rate ", alpha)))

wilcox.test(plotdata$Alpha ~ plotdata$Condition)
median(data$p_alpha)
mad(data$p_alpha)
median(data$c_alpha)
mad(data$c_alpha)

p1 <- gather(data,Condition,Zeta,p_zeta,c_zeta)[,21]
plotdata <- as.data.frame(cbind(p1,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Zeta", "Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Condition,Zeta)) + geom_violin(aes(fill = Condition),linetype = 0,alpha=0.4) +
  geom_jitter(aes(color = Condition),size = 3,width = 0.2) + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  theme_classic(base_size = 20) +  theme(legend.position = "none") +
  xlab("Condition") + ylab(expression(paste("Computational noise ", zeta)))

wilcox.test(plotdata$Zeta ~ plotdata$Condition)
median(data$p_zeta)
mad(data$p_zeta)
median(data$c_zeta)
mad(data$c_zeta)

p1 <- gather(data,Condition,Tau,p_tau,c_tau)[,21]
plotdata <- as.data.frame(cbind(p1,c(rep(1,175),rep(2,175))))
names(plotdata) <- c("Tau", "Condition")
plotdata$Condition <- factor(plotdata$Condition, levels = c(1,2), labels = c("Partial","Complete"))

ggplot(plotdata,aes(Condition,Tau)) + geom_violin(aes(fill = Condition),linetype = 0,alpha=0.4) +
  geom_jitter(aes(color = Condition),size = 3,width = 0.2) + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  theme_classic(base_size = 20) +  theme(legend.position = "none") +
  xlab("Condition") + ylab(expression(paste("Policy temperature ", tau)))

wilcox.test(plotdata$Tau ~ plotdata$Condition)
median(data$p_tau)
mad(data$p_tau)
median(data$c_tau)
mad(data$c_tau)

ggplot(data,aes(1,p_delta,fill = "#00BFC4")) + geom_violin(linetype = 0) +
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  theme_classic(base_size = 18) +  theme(legend.position = "none") + ylab("Delta") + xlab("")
