### Script for plotting mutual information and nongreedy actions

# Call libraries

library(R.matlab)
library(tidyverse)
library(ggplot2)
library(ggsci)

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")

# Partial condition

partial_mi <- readMat("partial_mi.mat")
partial_frac_nongreedy <- readMat("partial_frac_nongreedy.mat")
partial_mi <- as.data.frame(partial_mi$mi)
partial_frac_nongreedy <- as.data.frame(partial_frac_nongreedy$frac.nongreedy)
temp <- gather(partial_frac_nongreedy,Model,frac_nongreedy,V1:V4)

partial_data <- gather(partial_mi,Model,mi,V1:V4)
partial_data$Model <- factor(partial_data$Model, levels = c("V1","V2","V3","V4"), labels = c("Human","Exact softmax","Noisy argmax", "Noisy softmax"))
partial_data$frac_nongreedy <- temp$frac_nongreedy

partial_human <- partial_data[partial_data$Model == "Human",]

ggplot(partial_human,aes(frac_nongreedy,mi,color = "#F8766D")) + geom_point(size = 3) + geom_smooth(color = "black") +
  theme_classic(base_size = 20) + xlab("Fraction of suboptimal actions") + ylab("Mutual information (bits)") +
  ggtitle("Partial") + theme(legend.position = "none") + ylim(c(-0.06,0.45))

cor.test(partial_human$mi,partial_human$frac_nongreedy,method = "spearman")

ggplot(partial_data,aes(Model,mi,fill = Model)) + geom_violin(linetype = 0, alpha = 0.4) +
  theme_classic(base_size = 20) + geom_jitter(aes(color = Model),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  xlab("Model") + ylab("Mutual information (bits)") + theme(legend.position = "none") +
  ggtitle("Partial") + ylim(c(-0.06,0.45)) + scale_fill_manual(values = c("grey40","#F9877F","#F8766D","#D9675F")) +
  scale_color_manual(values = c("grey40","#F9877F","#F8766D","#D9675F"))

ks.test(partial_frac_nongreedy$V1,partial_frac_nongreedy$V2)
ks.test(partial_frac_nongreedy$V1,partial_frac_nongreedy$V3)
ks.test(partial_frac_nongreedy$V1,partial_frac_nongreedy$V4)

dat <- partial_data[partial_data$Model == "Human" | partial_data$Model == "Exact softmax",]
wilcox.test(dat$mi ~ dat$Model)
mean(dat$mi[dat$Model == "Human"]) - mean(dat$mi[dat$Model == "Exact softmax"])
dat <- partial_data[partial_data$Model == "Human" | partial_data$Model == "Noisy argmax",]
wilcox.test(dat$mi ~ dat$Model)
mean(dat$mi[dat$Model == "Human"]) - mean(dat$mi[dat$Model == "Weber argmax"])
dat <- partial_data[partial_data$Model == "Human" | partial_data$Model == "Noisy softmax",]
wilcox.test(dat$mi ~ dat$Model)
mean(dat$mi[dat$Model == "Human"]) - mean(dat$mi[dat$Model == "Noisy softmax"])
mad(partial_data$mi[partial_data$Model == "Human"])
mad(partial_data$mi[partial_data$Model == "Exact softmax"])
mad(partial_data$mi[partial_data$Model == "Noisy argmax"])
mad(partial_data$mi[partial_data$Model == "Noisy softmax"])
median(partial_data$mi[partial_data$Model == "Human"])
median(partial_data$mi[partial_data$Model == "Exact softmax"])
median(partial_data$mi[partial_data$Model == "Noisy argmax"])
median(partial_data$mi[partial_data$Model == "Noisy softmax"])


ggplot(partial_data,aes(Model,frac_nongreedy,fill = Model)) + geom_violin(linetype = 0, alpha = 0.4) +
  theme_classic(base_size = 20) + geom_jitter(aes(color = Model),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  xlab("Model") + ylab("Fraction of suboptimal choices") + theme(legend.position = "none") +
  ggtitle("Partial") + ylim(c(-0.05,0.5)) + scale_fill_manual(values = c("grey40","#F9877F","#F8766D","#D9675F")) +
  scale_color_manual(values = c("grey40","#F9877F","#F8766D","#D9675F"))
ks.test(partial_mi$V1,partial_mi$V2)
ks.test(partial_mi$V1,partial_mi$V3)
ks.test(partial_mi$V1,partial_mi$V4)

dat <- partial_data[partial_data$Model == "Human" | partial_data$Model == "Exact softmax",]
wilcox.test(dat$frac_nongreedy ~ dat$Model)
mean(dat$frac_nongreedy[dat$Model == "Human"]) - mean(dat$frac_nongreedy[dat$Model == "Exact softmax"])
dat <- partial_data[partial_data$Model == "Human" | partial_data$Model == "Noisy argmax",]
wilcox.test(dat$frac_nongreedy ~ dat$Model)
mean(dat$frac_nongreedy[dat$Model == "Human"]) - mean(dat$frac_nongreedy[dat$Model == "Noisy argmax"])
dat <- partial_data[partial_data$Model == "Human" | partial_data$Model == "Noisy softmax",]
wilcox.test(dat$frac_nongreedy ~ dat$Model)
mean(dat$frac_nongreedy[dat$Model == "Human"]) - mean(dat$frac_nongreedy[dat$Model == "Noisy softmax"])
mad(partial_data$frac_nongreedy[partial_data$Model == "Human"])
mad(partial_data$frac_nongreedy[partial_data$Model == "Exact softmax"])
mad(partial_data$frac_nongreedy[partial_data$Model == "Noisy argmax"])
mad(partial_data$frac_nongreedy[partial_data$Model == "Noisy softmax"])
median(partial_data$frac_nongreedy[partial_data$Model == "Human"])
median(partial_data$frac_nongreedy[partial_data$Model == "Exact softmax"])
median(partial_data$frac_nongreedy[partial_data$Model == "Noisy argmax"])
median(partial_data$frac_nongreedy[partial_data$Model == "Noisy softmax"])

# Complete condition

complete_mi <- readMat("complete_mi.mat")
complete_frac_nongreedy <- readMat("complete_frac_nongreedy.mat")
complete_mi <- as.data.frame(complete_mi$mi)
complete_frac_nongreedy <- as.data.frame(complete_frac_nongreedy$frac.nongreedy)
temp <- gather(complete_frac_nongreedy,Model,frac_nongreedy,V1:V4)

complete_data <- gather(complete_mi,Model,mi,V1:V4)
complete_data$Model <- factor(complete_data$Model, levels = c("V1","V2","V3","V4"), labels = c("Human","Exact softmax","Noisy argmax", "Noisy softmax"))
complete_data$frac_nongreedy <- temp$frac_nongreedy

complete_human <- complete_data[complete_data$Model == "Human",]

ggplot(complete_human,aes(frac_nongreedy,mi)) + geom_point(color = "#00BFC4",size = 3) + geom_smooth(color = "black") +
  theme_classic(base_size = 20) + xlab("Fraction of suboptimal actions") + ylab("Mutual information (bits)") +
  ggtitle("Complete") + theme(legend.position = "none") + ylim(c(-0.06,0.45))

cor.test(complete_human$mi,complete_human$frac_nongreedy,method = "spearman")

ggplot(complete_data,aes(Model,mi,fill = Model)) + geom_violin(linetype = 0, alpha = 0.4) +
  theme_classic(base_size = 20) + geom_jitter(aes(color = Model),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  xlab("Model") + ylab("Mutual information (bits)") + theme(legend.position = "none") +
  ggtitle("Complete") + ylim(c(-0.06,0.45)) + scale_fill_manual(values = c("grey40","#20C7CB","#00BFC4","#00A7AC")) +
  scale_color_manual(values = c("grey40","#20C7CB","#00BFC4","#00A7AC"))

ks.test(complete_frac_nongreedy$V1,complete_frac_nongreedy$V2)
ks.test(complete_frac_nongreedy$V1,complete_frac_nongreedy$V3)
ks.test(complete_frac_nongreedy$V1,complete_frac_nongreedy$V4)

dat <- complete_data[complete_data$Model == "Human" | complete_data$Model == "Exact softmax",]
wilcox.test(dat$mi ~ dat$Model)
mean(dat$mi[dat$Model == "Human"]) - mean(dat$mi[dat$Model == "Exact softmax"])
dat <- complete_data[complete_data$Model == "Human" | complete_data$Model == "Noisy argmax",]
wilcox.test(dat$mi ~ dat$Model)
mean(dat$mi[dat$Model == "Human"]) - mean(dat$mi[dat$Model == "Noisy argmax"])
dat <- complete_data[complete_data$Model == "Human" | complete_data$Model == "Noisy softmax",]
wilcox.test(dat$mi ~ dat$Model)
mean(dat$mi[dat$Model == "Human"]) - mean(dat$mi[dat$Model == "Noisy softmax"])
mad(complete_data$mi[complete_data$Model == "Human"])
mad(complete_data$mi[complete_data$Model == "Exact softmax"])
mad(complete_data$mi[complete_data$Model == "Noisy argmax"])
mad(complete_data$mi[complete_data$Model == "Noisy softmax"])
median(complete_data$mi[complete_data$Model == "Human"])
median(complete_data$mi[complete_data$Model == "Exact softmax"])
median(complete_data$mi[complete_data$Model == "Noisy argmax"])
median(complete_data$mi[complete_data$Model == "Noisy softmax"])


ggplot(complete_data,aes(Model,frac_nongreedy,fill = Model)) + geom_violin(linetype = 0, alpha = 0.4) +
  theme_classic(base_size = 20) + geom_jitter(aes(color = Model),size = 3,width = 0.2) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult = 1), geom="pointrange", color="black", size = 1.2) +
  xlab("Model") + ylab("Fraction of suboptimal choices") + theme(legend.position = "none") +
  ggtitle("Complete") + ylim(c(-0.05,0.5)) + scale_fill_manual(values = c("grey40","#20C7CB","#00BFC4","#00A7AC")) +
  scale_color_manual(values = c("grey40","#20C7CB","#00BFC4","#00A7AC"))
ks.test(complete_mi$V1,complete_mi$V2)
ks.test(complete_mi$V1,complete_mi$V3)
ks.test(complete_mi$V1,complete_mi$V4)

dat <- complete_data[complete_data$Model == "Human" | complete_data$Model == "Exact softmax",]
wilcox.test(dat$frac_nongreedy ~ dat$Model)
mean(dat$frac_nongreedy[dat$Model == "Human"]) - mean(dat$frac_nongreedy[dat$Model == "Exact softmax"])
dat <- complete_data[complete_data$Model == "Human" | complete_data$Model == "Noisy argmax",]
wilcox.test(dat$frac_nongreedy ~ dat$Model)
mean(dat$frac_nongreedy[dat$Model == "Human"]) - mean(dat$frac_nongreedy[dat$Model == "Noisy argmax"])
dat <- complete_data[complete_data$Model == "Human" | complete_data$Model == "Noisy softmax",]
wilcox.test(dat$frac_nongreedy ~ dat$Model)
mean(dat$frac_nongreedy[dat$Model == "Human"]) - mean(dat$frac_nongreedy[dat$Model == "Noisy softmax"])
mad(complete_data$frac_nongreedy[complete_data$Model == "Human"])
mad(complete_data$frac_nongreedy[complete_data$Model == "Exact softmax"])
mad(complete_data$frac_nongreedy[complete_data$Model == "Noisy argmax"])
mad(complete_data$frac_nongreedy[complete_data$Model == "Noisy softmax"])
median(complete_data$frac_nongreedy[complete_data$Model == "Human"])
median(complete_data$frac_nongreedy[complete_data$Model == "Exact softmax"])
median(complete_data$frac_nongreedy[complete_data$Model == "Noisy argmax"])
median(complete_data$frac_nongreedy[complete_data$Model == "Noisy softmax"])

