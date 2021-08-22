### Plot RT as a function of Q value differences

# Call libraries

library(R.matlab)
library(tidyverse)
library(ggplot2)
library(data.table)
library(ggsci)
library(lme4)

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")

cond <- 2 # 1 for partial 2 for complete

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")
if (cond == 1) {
  Q_rts <- readMat('partial_Q_rts.mat')
} else {
  Q_rts <- readMat('complete_Q_rts.mat')
}

Q_rts <- as.data.frame(Q_rts$Q.rt)
Q_rts <- gather(Q_rts,Q_diff,RT,V1:V5)
Q_rts$Q_diff[Q_rts$Q_diff == "V1"] = -0.18 # Values from matlab, mean values of Q_diff in each bin
Q_rts$Q_diff[Q_rts$Q_diff == "V2"] = -0.04
Q_rts$Q_diff[Q_rts$Q_diff == "V3"] = 0
Q_rts$Q_diff[Q_rts$Q_diff == "V4"] = 0.06
Q_rts$Q_diff[Q_rts$Q_diff == "V5"] = 0.20
Q_rts$Q_diff <- as.numeric(Q_rts$Q_diff)

ggplot(Q_rts,aes(Q_diff,RT)) + stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group = 1)) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange") +
  theme_light() + ylab("RT (ms)") + xlab("Q value difference") 

