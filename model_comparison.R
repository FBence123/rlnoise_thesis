### Script for plotting the model comparison results

# Call libraries and set the data up
library(R.matlab)
library(tidyverse)
library(ggplot2)
library(data.table)
library(ggsci)
library(lme4)

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")
partial <- readMat('metrics_fb1.mat')
complete <- readMat('metrics_fb2.mat')
partial <- as.data.frame(partial$metrics)
complete <- as.data.frame(complete$metrics)
names(partial) <- c("LL", "LP", "ELBO", "Alpha", "XP")
names(complete) <- c("LL", "LP", "ELBO", "Alpha", "XP")
partial$Model <- factor(1:3, levels = 1:3, labels = c("Exact softmax", "Noisy argmax", "Noisy softmax"))
complete$Model <- factor(1:3, levels = 1:3, labels = c("Exact softmax", "Noisy argmax", "Noisy softmax"))

# Partial plots
ggplot(partial,aes(Model,LL, fill = Model)) + geom_bar(stat = "identity") + theme_classic(base_size = 20) +
  scale_fill_manual(values=c("red4","red3","red2")) + xlab("Models") + ylab("Log likelihood") +
  ggtitle("Partial") + theme(legend.position = "none")

ggplot(partial,aes(Model,LP, fill = Model)) + geom_bar(stat = "identity") + theme_classic(base_size = 20) +
  scale_fill_manual(values=c("red4","red3","red2")) + xlab("Models") + ylab("Log prior") +
  ggtitle("Partial") + theme(legend.position = "none")

ggplot(partial,aes(Model,ELBO, fill = Model)) + geom_bar(stat = "identity", color = "black", size = 2) + theme_classic(base_size = 20) +
  scale_fill_manual(values=c("#DA7872","#F8766D","#BA5952")) + xlab("Models") + ylab("Evidence lower bound") +
  ggtitle("Partial") + theme(legend.position = "none")

ggplot(partial,aes(Model,XP, fill = Model)) + geom_bar(stat = "identity") + theme_classic(base_size = 20) +
  scale_fill_manual(values=c("red4","red3","red2")) + xlab("Models") + ylab("Exceedance probability") +
  ggtitle("Partial") + theme(legend.position = "none")

ggplot(partial,aes(Model,Alpha/178, fill = Model)) + geom_bar(stat = "identity", color = "black", size = 2) + theme_classic(base_size = 20) +
  scale_fill_manual(values=c("#DA7872","#F8766D","#BA5952")) + xlab("Models") + ylab("Model frequency") +
  ggtitle("Partial") + theme(legend.position = "none") + ylim(c(0,1))

# Complete plots
ggplot(complete,aes(Model,LL, fill = Model)) + geom_bar(stat = "identity") + theme_classic(base_size = 18) +
  scale_fill_manual(values=c("royalblue4","royalblue3","royalblue2")) + xlab("Models") + ylab("Log likelihood") +
  ggtitle("Complete") + theme(legend.position = "none")

ggplot(complete,aes(Model,LP, fill = Model)) + geom_bar(stat = "identity") + theme_classic(base_size = 18) +
  scale_fill_manual(values=c("royalblue4","royalblue3","royalblue2")) + xlab("Models") + ylab("Log prior") +
  ggtitle("Complete") + theme(legend.position = "none")

ggplot(complete,aes(Model,ELBO, fill = Model)) + geom_bar(stat = "identity", color = "black", size = 2) + theme_classic(base_size = 20) +
  scale_fill_manual(values=c("#20C7CB","#00BFC4","#00A7AC")) + xlab("Models") + ylab("Evidence lower bound") +
  ggtitle("Complete") + theme(legend.position = "none")

ggplot(complete,aes(Model,XP, fill = Model)) + geom_bar(stat = "identity") + theme_classic(base_size = 18) +
  scale_fill_manual(values=c("royalblue4","royalblue3","royalblue2")) + xlab("Models") + ylab("Exceedance probability") +
  ggtitle("Complete") + theme(legend.position = "none")

ggplot(complete,aes(Model,Alpha/178, fill = Model)) + geom_bar(stat = "identity", color = "black", size = 2) + theme_classic(base_size = 20) +
  scale_fill_manual(values=c("#20C7CB","#00BFC4","#00A7AC")) + xlab("Models") + ylab("Model frequency") +
  ggtitle("Complete") + theme(legend.position = "none") + ylim(c(0,1))
