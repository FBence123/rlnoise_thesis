### Script for some basic analyses of behavioural data
##### Setup #####
# Call libraries

library(R.matlab)
library(tidyverse)
library(ggplot2)
library(data.table)
library(ggsci)
library(lme4)

# Load preprocessed data from mat files and wrangle it to a proper format

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")

# Partial condition

partial_switch_prob_t_1 <- readMat('partial_switch_prob_t_1.mat')
partial_switch_prob_t_1 <- as.data.frame(partial_switch_prob_t_1$switch.prob.t.1)
partial_switch_prob_t_2 <- readMat('partial_switch_prob_t_2.mat')
partial_switch_prob_t_2 <- as.data.frame(partial_switch_prob_t_2$switch.prob.t.2)
partial_switch_prob_t_3 <- readMat('partial_switch_prob_t_3.mat')
partial_switch_prob_t_3 <- as.data.frame(partial_switch_prob_t_3$switch.prob.t.3)
partial_switch_prob_t_4 <- readMat('partial_switch_prob_t_4.mat')
partial_switch_prob_t_4 <- as.data.frame(partial_switch_prob_t_4$switch.prob.t.4)
partial_switch_prob_t_5 <- readMat('partial_switch_prob_t_5.mat')
partial_switch_prob_t_5 <- as.data.frame(partial_switch_prob_t_5$switch.prob.t.5)
partial_switch_prob_t_6 <- readMat('partial_switch_prob_t_6.mat')
partial_switch_prob_t_6 <- as.data.frame(partial_switch_prob_t_6$switch.prob.t.6)
partial_behavdat <- readMat('behavdat_fb1_21.mat')
partial_Q_rts <- readMat('partial_Q_rts.mat')
partial_Q_rts <- as.data.frame(partial_Q_rts$Q.rt)
partial_Q_corrs <- readMat('partial_Q_corrs.mat')
partial_Q_corrs <- as.data.frame(partial_Q_corrs$Q.corr)

partial_actions <- as.matrix(partial_behavdat$behavdat[[6]])
partial_switches <- as.matrix(partial_behavdat$behavdat[[7]])
partial_rt <- as.matrix(partial_behavdat$behavdat[[8]])
partial_rewards <- as.matrix(partial_behavdat$behavdat[[11]])
partial_correct <- as.matrix(partial_behavdat$behavdat[[12]])
partial_nongreedy <- as.matrix(partial_behavdat$behavdat[[13]])

idx <- partial_behavdat$behavdat[[19]] == 1

# Complete condition

complete_switch_prob_t_1 <- readMat('complete_switch_prob_t_1.mat')
complete_switch_prob_t_1 <- as.data.frame(complete_switch_prob_t_1$switch.prob.t.1)
complete_switch_prob_t_2 <- readMat('complete_switch_prob_t_2.mat')
complete_switch_prob_t_2 <- as.data.frame(complete_switch_prob_t_2$switch.prob.t.2)
complete_switch_prob_t_3 <- readMat('complete_switch_prob_t_3.mat')
complete_switch_prob_t_3 <- as.data.frame(complete_switch_prob_t_3$switch.prob.t.3)
complete_switch_prob_t_4 <- readMat('complete_switch_prob_t_4.mat')
complete_switch_prob_t_4 <- as.data.frame(complete_switch_prob_t_4$switch.prob.t.4)
complete_switch_prob_t_5 <- readMat('complete_switch_prob_t_5.mat')
complete_switch_prob_t_5 <- as.data.frame(complete_switch_prob_t_5$switch.prob.t.5)
complete_switch_prob_t_6 <- readMat('complete_switch_prob_t_6.mat')
complete_switch_prob_t_6 <- as.data.frame(complete_switch_prob_t_6$switch.prob.t.6)
complete_behavdat <- readMat('behavdat_fb2_21.mat')
complete_Q_rts <- readMat('complete_Q_rts.mat')
complete_Q_rts <- as.data.frame(complete_Q_rts$Q.rt)
complete_Q_corrs <- readMat('complete_Q_corrs.mat')
complete_Q_corrs <- as.data.frame(complete_Q_corrs$Q.corr)

complete_actions <- as.matrix(complete_behavdat$behavdat[[6]])
complete_switches <- as.matrix(complete_behavdat$behavdat[[7]])
complete_rt <- as.matrix(complete_behavdat$behavdat[[8]])
complete_rewards <- as.matrix(complete_behavdat$behavdat[[11]])
complete_correct <- as.matrix(complete_behavdat$behavdat[[12]])
complete_nongreedy <- as.matrix(complete_behavdat$behavdat[[13]])

###### Plot RTs ######

plotdata <- as.matrix(apply(partial_rt[idx,c(2:72,74:144)],2,median))
plotdata <- rbind(plotdata,as.matrix(apply(complete_rt[idx,c(2:72,74:144)],2,median)))
plotdata <- as.data.frame(plotdata)
plotdata$Condition <- c(rep(1,142),rep(2,142))
plotdata$Trial <- c(1:71,1:71,1:71,1:71)
plotdata$Block <- c(rep(1,71),rep(2,71),rep(1,71),rep(2,71))
names(plotdata) <- c("RT","Condition","Trial","Block")
plotdata$Condition <- factor(plotdata$Condition,levels = c(1,2), labels = c("Partial","Complete"))
plotdata$Block <- factor(plotdata$Block, levels = c(1,2), labels = c("Block 1", "Block 2"))

ggplot(plotdata,aes(Trial,RT,color = Condition)) + geom_line(size = 1.2) + 
  theme_classic(base_size = 18) + ylab("Median RT (ms)") + facet_grid(. ~ Block)

summary(lm(data = plotdata, RT ~ Trial * Condition))

###### Plot Correct ######

plotdata <- as.matrix(colSums(partial_correct[idx,c(2:72,74:144)]))
plotdata <- rbind(plotdata,as.matrix(colSums(complete_correct[idx,c(2:72,74:144)])))
plotdata <- plotdata / sum(idx)
plotdata <- as.data.frame(plotdata)
plotdata$Condition <- c(rep(1,142),rep(2,142))
plotdata$Trial <- c(1:71,1:71,1:71,1:71)
plotdata$Block <- c(rep(1,71),rep(2,71),rep(1,71),rep(2,71))
names(plotdata) <- c("Pcorr","Condition","Trial","Block")
plotdata$Condition <- factor(plotdata$Condition,levels = c(1,2), labels = c("Partial","Complete"))
plotdata$Block <- factor(plotdata$Block, levels = c(1,2), labels = c("Block 1", "Block 2"))

ggplot(plotdata,aes(Trial,Pcorr,color = Condition)) + geom_line(size = 1.2) + 
  theme_classic(base_size = 18) + ylab("P(correct)") + facet_grid(. ~ Block)

summary(lm(data = plotdata, Pcorr ~ Trial * Condition))

###### Plot fraction switches ######
plotdata <- as.matrix(colSums(partial_switches[idx,c(2:72,74:144)]))
plotdata <- rbind(plotdata,as.matrix(colSums(complete_switches[idx,c(2:72,74:144)])))
plotdata <- plotdata / sum(idx)
plotdata <- as.data.frame(plotdata)
plotdata$Condition <- c(rep(1,142),rep(2,142))
plotdata$Trial <- c(1:71,1:71,1:71,1:71)
plotdata$Block <- c(rep(1,71),rep(2,71),rep(1,71),rep(2,71))
names(plotdata) <- c("Pcorr","Condition","Trial","Block")
plotdata$Condition <- factor(plotdata$Condition,levels = c(1,2), labels = c("Partial","Complete"))
plotdata$Block <- as.factor(plotdata$Block)

ggplot(plotdata,aes(Trial,Pcorr,color = Condition)) + geom_line(size = 1.2) + 
  theme_classic() + ylab("Fraction of subjects switching on trial") + facet_grid(. ~ Block)

###### Plot fraction nongreedy ######

plotdata <- as.matrix(colSums(partial_nongreedy[idx,c(2:72,74:144)]))
plotdata <- rbind(plotdata,as.matrix(colSums(complete_nongreedy[idx,c(2:72,74:144)])))
plotdata <- plotdata / sum(idx)
plotdata <- as.data.frame(plotdata)
plotdata$Condition <- c(rep(1,142),rep(2,142))
plotdata$Trial <- c(1:71,1:71,1:71,1:71)
plotdata$Block <- c(rep(1,71),rep(2,71),rep(1,71),rep(2,71))
names(plotdata) <- c("P_nongreedy","Condition","Trial","Block")
plotdata$Condition <- factor(plotdata$Condition,levels = c(1,2), labels = c("Partial","Complete"))
plotdata$Block <- as.factor(plotdata$Block)

ggplot(plotdata,aes(Trial,P_nongreedy,color = Condition)) + geom_line(size = 1.2) + 
  theme_classic() + ylab("P(nongreedy)") + facet_grid(. ~ Block)

plotdata <- as.matrix(rowSums(partial_nongreedy[idx,])/144)
plotdata <- rbind(plotdata,as.matrix(rowSums(complete_nongreedy[idx,])/144))
plotdata <- as.data.frame(plotdata)
plotdata$Condition <- c(rep(1,175),rep(2,175))
plotdata$Condition <- factor(plotdata$Condition,levels = c(1,2), labels = c("Partial","Complete"))
names(plotdata) <- c("Frac_nongreedy","Condition")

ggplot(plotdata,aes(Condition,Frac_nongreedy)) + geom_violin(aes(fill = Condition),alpha = 0.4, linetype = 0) +
  geom_jitter(aes(color = Condition),size = 3,width = 0.2) + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") + ylab("Fraction of suboptimal choices")

wilcox.test(plotdata$Frac_nongreedy ~ plotdata$Condition)
median(plotdata$Frac_nongreedy[plotdata$Condition == "Partial"])
median(plotdata$Frac_nongreedy[plotdata$Condition == "Complete"])
mad(plotdata$Frac_nongreedy[plotdata$Condition == "Partial"])
mad(plotdata$Frac_nongreedy[plotdata$Condition == "Complete"])

###### Plot rewards obtained ######

plotdata <- as.matrix(rowSums(partial_rewards[idx,]))
plotdata <- rbind(plotdata,as.matrix(rowSums(complete_rewards[idx,])))
plotdata <- as.data.frame(plotdata)
plotdata$Condition <- c(rep(1,175),rep(2,175))
plotdata$Condition <- factor(plotdata$Condition,levels = c(1,2), labels = c("Partial","Complete"))
names(plotdata) <- c("Rews","Condition")

ggplot(plotdata,aes(Condition,Rews)) + geom_violin(aes(fill = Condition),linetype = 0, alpha = 0.4) +
  geom_jitter(aes(color = Condition),size = 3,width = 0.2) + stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  theme_classic(base_size = 20) +
  theme(legend.position = "none") + ylab("Sum of obtained rewards")

wilcox.test(plotdata$Rews ~ plotdata$Condition)
median(plotdata$Rews[plotdata$Condition == "Partial"])
median(plotdata$Rews[plotdata$Condition == "Complete"])
mad(plotdata$Rews[plotdata$Condition == "Partial"])
mad(plotdata$Rews[plotdata$Condition == "Complete"])

###### Plot switch probabilities ######

plotdata <- as.data.frame(c(colMeans(partial_switch_prob_t_1),colMeans(partial_switch_prob_t_2),colMeans(partial_switch_prob_t_3),colMeans(partial_switch_prob_t_4),colMeans(partial_switch_prob_t_5),colMeans(partial_switch_prob_t_6)))
plotdata$rew <- rep(c(-31,-14,0,13,31),6)
plotdata$Trial <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5),rep(6,5))
names(plotdata) <- c("switch_prob","rew","Trial")
plotdata$Trial <- factor(plotdata$Trial, levels = c(1,2,3,4,5,6), labels = c("t-1","t-2","t-3","t-4","t-5","t-6"))

ggplot(plotdata,aes(rew,switch_prob,color = Trial)) + geom_point(size = 3) + geom_line(size = 1.2) +
  theme_classic(base_size = 20) + scale_color_brewer(palette = "Reds") + ylab("P(A)") +
  xlab("Reward received wrt A") + ggtitle("Partial") + ylim(0,1)

plotdata <- as.data.frame(c(colMeans(complete_switch_prob_t_1),colMeans(complete_switch_prob_t_2),colMeans(complete_switch_prob_t_3),colMeans(complete_switch_prob_t_4),colMeans(complete_switch_prob_t_5),colMeans(complete_switch_prob_t_6)))
plotdata$rew <- rep(c(-31,-14,0,13,31),6)
plotdata$Trial <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5),rep(6,5))
names(plotdata) <- c("switch_prob","rew","Trial")
plotdata$Trial <- factor(plotdata$Trial, levels = c(1,2,3,4,5,6), labels = c("t-1","t-2","t-3","t-4","t-5","t-6"))

ggplot(plotdata,aes(rew,switch_prob,color = Trial)) + geom_point(size = 3) + geom_line(size = 1.2) +
  theme_classic(base_size = 20) + scale_color_brewer(palette = "Blues") + ylab("P(A)") +
  xlab("Reward received wrt A") + ggtitle("Complete") + ylim(0,1)

###### Fit a logistic regression for each subject separately ######

# Partial

regact <- partial_actions[idx,] == 1
regrew <- (3-2*partial_actions[idx,])*(partial_rewards[idx,]-50)
Coefs <- matrix(0L, nrow = nrow(regact),ncol = 7)
Ps <- matrix(0L, nrow = nrow(regact),ncol = 7)
Rew_sens <- matrix(0L, nrow = nrow(regact),ncol = 1)
for(i in 1:nrow(regact)){
  regdat <- as.data.frame(as.numeric(regact[i,]))
  regdat$rew_1 <- shift(regrew[i,],1)
  regdat$rew_2 <- shift(regrew[i,],2)
  regdat$rew_3 <- shift(regrew[i,],3)
  regdat$rew_4 <- shift(regrew[i,],4)
  regdat$rew_5 <- shift(regrew[i,],5)
  regdat$rew_6 <- shift(regrew[i,],6)
  
  regdat <- regdat[-c(1:6,73:78),]
  names(regdat) <- c("resp","rew_1","rew_2","rew_3","rew_4","rew_5","rew_6")
  mod <- glm(resp ~ rew_1 + rew_2 + rew_3 + rew_4 + rew_5 + rew_6, data = regdat, family = binomial)
  mod_1 <- glm(resp ~ rew_1, data = regdat, family = binomial)
  
  Coefs[i,] <- mod$coefficients
  Ps[i,] <- coef(summary(mod))[,4]
  Rew_sens[i] <- mod$coefficients[2]
}

# and plot the coefficients' and p values' distributions

plotdata <- as.data.frame(c(Coefs[,2],Coefs[,3],Coefs[,4],Coefs[,5],Coefs[,6],Coefs[,7]))
plotdata$Coefficients <- c(rep(1,175),rep(2,175),rep(3,175),rep(4,175),rep(5,175),rep(6,175))
names(plotdata) <- c("Values", "Coefficients")
plotdata$Coefficients <- factor(plotdata$Coefficients, levels = c(1,2,3,4,5,6), labels = c("t-1","t-2","t-3","t-4","t-5","t-6"))

ggplot(plotdata,aes(Coefficients,Values,fill = Coefficients)) + geom_violin(linetype = 0) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") +
  theme_classic(base_size = 20) + scale_fill_brewer(palette = "Reds") +
  theme(legend.position = "none") + xlab("Reward") + ylab("Decision weight") +
  ggtitle("Partial") + ylim(-0.1,0.2)

summary(aov(data = plotdata, Values ~ Coefficients))

#plotdata <- as.data.frame(c(Ps[,2],Ps[,3],Ps[,4],Ps[,5]))
#plotdata$Ps <- c(rep(1,175),rep(2,175),rep(3,175),rep(4,175))
#names(plotdata) <- c("Values", "Ps")
#plotdata$Ps <- factor(plotdata$Ps, levels = c(1,2,3,4), labels = c("Current reward", "Current-1 reward","Current-2 reward","Current-3 reward"))

#plotdata <- mutate(plotdata,
#                  idx = case_when(
#                    Values < 0.05 & Ps == "Current reward" ~ 1, 
#                    Values < 0.05 & Ps == "Current-1 reward" ~ 2,
#                    Values < 0.05 & Ps == "Current-2 reward" ~ 3,
#                    Values < 0.05 & Ps == "Current-3 reward" ~ 4,
#                    Values >= 0.05 ~ 0
#                  ))
#plotdata$idx <- as.factor(plotdata$idx)

#ggplot(plotdata,aes(Ps,Values,color = idx)) + geom_jitter() +
#  theme_classic(base_size = 18) + scale_color_manual(values=c("black","red4","red3","red2","red1")) +
#  theme(legend.position = "none") + xlab("Predictors") + ylab("P values") + ggtitle("Partial")

# Complete

regact <- complete_actions[idx,] == 1
regrew <- (3-2*complete_actions[idx,])*(complete_rewards[idx,]-50)
Coefs <- matrix(0L, nrow = nrow(regact),ncol = 7)
Ps <- matrix(0L, nrow = nrow(regact),ncol = 7)
Rew_sens_c <- matrix(0L, nrow = nrow(regact),ncol = 1)
for(i in 1:nrow(regact)){
  regdat <- as.data.frame(as.numeric(regact[i,]))
  regdat$rew_1 <- shift(regrew[i,],1)
  regdat$rew_2 <- shift(regrew[i,],2)
  regdat$rew_3 <- shift(regrew[i,],3)
  regdat$rew_4 <- shift(regrew[i,],4)
  regdat$rew_5 <- shift(regrew[i,],5)
  regdat$rew_6 <- shift(regrew[i,],6)
  
  regdat <- regdat[-c(1:6,73:78),]
  names(regdat) <- c("resp","rew_1","rew_2","rew_3","rew_4","rew_5","rew_6")
  mod <- glm(resp ~ rew_1 + rew_2 + rew_3 + rew_4 + rew_5 + rew_6, data = regdat, family = binomial)
  mod_1 <- glm(resp ~ rew_1, data = regdat, family = binomial)
  
  Coefs[i,] <- mod$coefficients
  Ps[i,] <- coef(summary(mod))[,4]
  Rew_sens_c[i] <- mod$coefficients[2]
}

Rew_sens <- cbind(Rew_sens,Rew_sens_c)
write.csv(Rew_sens,"Rew_sens.csv",row.names = FALSE)

# and plot the coefficients' and p values' distributions

plotdata <- as.data.frame(c(Coefs[,2],Coefs[,3],Coefs[,4],Coefs[,5],Coefs[,6],Coefs[,7]))
plotdata$Coefficients <- c(rep(1,175),rep(2,175),rep(3,175),rep(4,175),rep(5,175),rep(6,175))
names(plotdata) <- c("Values", "Coefficients")
plotdata$Coefficients <- factor(plotdata$Coefficients, levels = c(1,2,3,4,5,6), labels = c("t-1","t-2","t-3","t-4","t-5","t-6"))

ggplot(plotdata,aes(Coefficients,Values,fill = Coefficients)) + geom_violin(linetype = 0) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black") +
  theme_classic(base_size = 20) + scale_fill_brewer(palette = "Blues") +
  theme(legend.position = "none") + xlab("Reward") + ylab("Decision weight") +
  ggtitle("Complete") + ylim(-0.1,0.2)

summary(aov(data = plotdata, Values ~ Coefficients))
t.test(plotdata$Values[plotdata$Coefficients == "t-6"])

#plotdata <- as.data.frame(c(Ps[,2],Ps[,3],Ps[,4],Ps[,5]))
#plotdata$Ps <- c(rep(1,175),rep(2,175),rep(3,175),rep(4,175))
#names(plotdata) <- c("Values", "Ps")
#plotdata$Ps <- factor(plotdata$Ps, levels = c(1,2,3,4), labels = c("Current reward", "Current-1 reward","Current-2 reward","Current-3 reward"))

#plotdata <- mutate(plotdata,
#                   idx = case_when(
#                     Values < 0.05 & Ps == "Current reward" ~ 1, 
#                     Values < 0.05 & Ps == "Current-1 reward" ~ 2,
#                     Values < 0.05 & Ps == "Current-2 reward" ~ 3,
#                     Values < 0.05 & Ps == "Current-3 reward" ~ 4,
#                     Values >= 0.05 ~ 0
#                   ))
#plotdata$idx <- as.factor(plotdata$idx)

#ggplot(plotdata,aes(Ps,Values,color = idx)) + geom_jitter() +
#  theme_classic(base_size = 18) + scale_color_manual(values=c("black","royalblue4","royalblue3","royalblue2","royalblue1")) +
#  theme(legend.position = "none") + xlab("Predictors") + ylab("P values") + ggtitle("Complete")

###### Plot Q difference RTs ######

# Partial

Q_rts <- gather(partial_Q_rts,Q_diff,RT,V1:V7)
Q_rts$Q_diff[Q_rts$Q_diff == "V1"] = -0.40 # Values from matlab, mean values of Q_diff in each bin
Q_rts$Q_diff[Q_rts$Q_diff == "V2"] = -0.22
Q_rts$Q_diff[Q_rts$Q_diff == "V3"] = -0.10
Q_rts$Q_diff[Q_rts$Q_diff == "V4"] = 0
Q_rts$Q_diff[Q_rts$Q_diff == "V5"] = 0.10
Q_rts$Q_diff[Q_rts$Q_diff == "V6"] = 0.22
Q_rts$Q_diff[Q_rts$Q_diff == "V7"] = 0.40
Q_rts$Q_diff <- as.numeric(Q_rts$Q_diff)

ggplot(Q_rts,aes(Q_diff,RT)) + stat_summary(fun.y = mean, geom = "point", size = 3, color = "#F8766D") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.2, color = "#F8766D") +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange",size = 1.2, color = "#F8766D") +
  theme_classic(base_size = 18) + ylab("Reaction time (ms)") + xlab(expression(Q[A]-Q[B])) +
  theme(legend.position = "none") + ggtitle("Partial")

Q_rts$Q_diff <- as.factor(Q_rts$Q_diff)
summary(aov(data = Q_rts, RT ~ Q_diff))

# Complete

Q_rts <- gather(complete_Q_rts,Q_diff,RT,V1:V7)
Q_rts$Q_diff[Q_rts$Q_diff == "V1"] = -0.40 # Values from matlab, mean values of Q_diff in each bin
Q_rts$Q_diff[Q_rts$Q_diff == "V2"] = -0.22
Q_rts$Q_diff[Q_rts$Q_diff == "V3"] = -0.10
Q_rts$Q_diff[Q_rts$Q_diff == "V4"] = 0
Q_rts$Q_diff[Q_rts$Q_diff == "V5"] = 0.10
Q_rts$Q_diff[Q_rts$Q_diff == "V6"] = 0.22
Q_rts$Q_diff[Q_rts$Q_diff == "V7"] = 0.40
Q_rts$Q_diff <- as.numeric(Q_rts$Q_diff)

ggplot(Q_rts,aes(Q_diff,RT)) + stat_summary(fun.y = mean, geom = "point", size = 3, color = "#00BFC4") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.2, color = "#00BFC4") +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange",size = 1.2, color = "#00BFC4") +
  theme_classic(base_size = 18) + ylab("Reaction time (ms)") + xlab(expression(Q[A]-Q[B])) +
  theme(legend.position = "none") + ggtitle("Complete")

Q_rts$Q_diff <- as.factor(Q_rts$Q_diff)
summary(aov(data = Q_rts, RT ~ Q_diff))

###### Plot Q difference Corrs ######

# Partial

Q_corrs <- gather(partial_Q_corrs,Q_diff,Corr,V1:V7)
Q_corrs$Q_diff[Q_corrs$Q_diff == "V1"] = -0.40 # Values from matlab, mean values of Q_diff in each bin
Q_corrs$Q_diff[Q_corrs$Q_diff == "V2"] = -0.22
Q_corrs$Q_diff[Q_corrs$Q_diff == "V3"] = -0.11
Q_corrs$Q_diff[Q_corrs$Q_diff == "V4"] = 0
Q_corrs$Q_diff[Q_corrs$Q_diff == "V5"] = 0.11
Q_corrs$Q_diff[Q_corrs$Q_diff == "V6"] = 0.22
Q_corrs$Q_diff[Q_corrs$Q_diff == "V7"] = 0.40
Q_corrs$Q_diff <- as.numeric(Q_corrs$Q_diff)

ggplot(Q_corrs,aes(Q_diff,Corr)) + stat_summary(fun.y = mean, geom = "point", size = 3, color = "#F8766D") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.2, color = "#F8766D") +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange",size = 1.2, color = "#F8766D") +
  theme_classic(base_size = 20) + ylab("P(correct)") + xlab(expression(mu[A]-mu[B])) +
  theme(legend.position = "none") + ggtitle("Partial")

Q_corrs$Q_diff <- as.factor(Q_corrs$Q_diff)
summary(aov(data = Q_corrs, Corr ~ Q_diff))

# Complete

Q_corrs <- gather(complete_Q_corrs,Q_diff,Corr,V1:V7)
Q_corrs$Q_diff[Q_corrs$Q_diff == "V1"] = -0.40 # Values from matlab, mean values of Q_diff in each bin
Q_corrs$Q_diff[Q_corrs$Q_diff == "V2"] = -0.22
Q_corrs$Q_diff[Q_corrs$Q_diff == "V3"] = -0.11
Q_corrs$Q_diff[Q_corrs$Q_diff == "V4"] = 0
Q_corrs$Q_diff[Q_corrs$Q_diff == "V5"] = 0.11
Q_corrs$Q_diff[Q_corrs$Q_diff == "V6"] = 0.22
Q_corrs$Q_diff[Q_corrs$Q_diff == "V7"] = 0.40
Q_corrs$Q_diff <- as.numeric(Q_corrs$Q_diff)

ggplot(Q_corrs,aes(Q_diff,Corr)) + stat_summary(fun.y = mean, geom = "point", size = 3, color = "#00BFC4") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1), size = 1.2, color = "#00BFC4") +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange",size = 1.2, color = "#00BFC4") +
  theme_classic(base_size = 20) + ylab("P(correct)") + xlab(expression(mu[A]-mu[B])) +
  theme(legend.position = "none") + ggtitle("Complete")

Q_corrs$Q_diff <- as.factor(Q_corrs$Q_diff)
summary(aov(data = Q_corrs, Corr ~ Q_diff))

###### Mixed effect logistic regression ######

# Partial
regact <- partial_actions[idx,] == 1
regrew <- (3-2*partial_actions[idx,])*(partial_rewards[idx,]-50)
regdat <- as.data.frame(as.numeric(regact[1,]))
regdat$rew <- regrew[1,]
regdat$rew_1 <- shift(regrew[1,],1)
regdat$rew_2 <- shift(regrew[1,],2)
regdat$rew_3 <- shift(regrew[1,],3)
regdat <- regdat[-c(1:3,73:75),]
regdat$subj <- rep(1,138)

for(i in 2:nrow(regdat)){
  temp <- as.data.frame(as.numeric(regact[i,]))
  temp$rew <- regrew[i,]
  temp$rew_1 <- shift(regrew[i,],1)
  temp$rew_2 <- shift(regrew[i,],2)
  temp$rew_3 <- shift(regrew[i,],3)
  temp <- temp[-c(1:3,73:75),]
  temp$subj <- rep(i,138)
  names(temp) <- names(regdat)
  regdat <- rbind(regdat,temp)
}

names(regdat) <- c("resp","rew","rew_1","rew_2","rew_3","subj")
regdat$subj <- as.factor(regdat$subj)

# Compare models of fixed effects only, fixed + random effects, random effects only

mod0 <- glm(resp ~ rew + rew_1 + rew_2 + rew_3, data = regdat, family = binomial)
mod1 <- glmer(resp ~ rew + rew_1 + rew_2 + rew_3 + (1+rew|subj) + (1+rew_1|subj) + (1+rew_2|subj) + (1+rew_3|subj), data = regdat, family = binomial)
mod2 <- glmer(resp ~ (1+rew|subj) + (1+rew_1|subj) + (1+rew_2|subj) + (1+rew_3|subj), data = regdat, family = binomial)
anova(mod2,mod1,mod0)

# Fixed + random effects model best, summarize it

summary(mod1)

# Iffy convergence... but still works

# Complete
regact <- complete_actions[idx,] == 1
regrew <- (3-2*complete_actions[idx,])*(complete_rewards[idx,]-50)
regdat <- as.data.frame(as.numeric(regact[1,]))
regdat$rew <- regrew[1,]
regdat$rew_1 <- shift(regrew[1,],1)
regdat$rew_2 <- shift(regrew[1,],2)
regdat$rew_3 <- shift(regrew[1,],3)
regdat <- regdat[-c(1:3,73:75),]
regdat$subj <- rep(1,138)

for(i in 2:nrow(regdat)){
  temp <- as.data.frame(as.numeric(regact[i,]))
  temp$rew <- regrew[i,]
  temp$rew_1 <- shift(regrew[i,],1)
  temp$rew_2 <- shift(regrew[i,],2)
  temp$rew_3 <- shift(regrew[i,],3)
  temp <- temp[-c(1:3,73:75),]
  temp$subj <- rep(i,138)
  names(temp) <- names(regdat)
  regdat <- rbind(regdat,temp)
}

names(regdat) <- c("resp","rew","rew_1","rew_2","rew_3","subj")
regdat$subj <- as.factor(regdat$subj)

# Compare models of fixed effects only, fixed + random effects, random effects only

mod0 <- glm(resp ~ rew + rew_1 + rew_2 + rew_3, data = regdat, family = binomial)
mod1 <- glmer(resp ~ rew + rew_1 + rew_2 + rew_3 + (1+rew|subj) + (1+rew_1|subj) + (1+rew_2|subj) + (1+rew_3|subj), data = regdat, family = binomial)
mod2 <- glmer(resp ~ (1+rew|subj) + (1+rew_1|subj) + (1+rew_2|subj) + (1+rew_3|subj), data = regdat, family = binomial)
anova(mod2,mod1,mod0)

# Fixed + random effects model best, summarize it

summary(mod1)

# Iffy convergence... but still works
