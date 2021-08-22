### Script for PCA of the 4 model parameters in the two conditions

# Set wd and call libraries

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")
library(R.matlab)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(data.table)
library(ggsci)
library(factoextra)
library(paran)
source('pca_eigenperm.R')
source('pca_loadperm.R')

# Load preprocessed data from mat files and wrangle it to a proper format

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")
params <- readMat("params.mat")
params <- as.data.frame(params$params)
names(params) <- c("p_alpha","p_delta","p_zeta","p_tau","p_switches",
                   "p_nongreedy","p_frac_noise","c_alpha","c_zeta","c_tau",
                   "c_switches","c_nongreedy","c_frac_noise","p_alpha_std",
                   "c_alpha_std","p_delta_std","p_zeta_std","c_zeta_std",
                   "p_tau_std","c_tau_std","idx")
data = params[params$idx == 1,c(1:4,8:10)]

# PCA and its visualizations and summaries

data_pca <- data
names(data_pca) <- c("Partial\nalpha","Partial\ndelta","Partial\nzeta","Partial\ntau","Complete\nalpha","Complete\nzeta","Complete\ntau")
pca_res <- prcomp(data_pca,center = T, scale. = T)
fviz_eig(pca_res, ylab = "Percentage of\n variance explained",ggtheme = theme_classic(base_size = 18))
summary(pca_res)
fviz_pca_biplot(pca_res,repel = T,col.var = "#2E9FDF", col.ind = "#696969", label = "var", labelsize = 6, arrowsize = 1, pointsize = 3, ggtheme = theme_classic()) +
  theme(text = element_text(size = 20),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20)) +
  labs(x = "PC1 (33.1 %)",
       y = "PC2 (25.1%)")

pca_var <- get_pca_var(pca_res)
pca_res$rotation
pca_perm <- pca_eigenperm(data,pca_res$sdev)
paran(data_pca, iterations=5000, quietly=F, 
      status=T, all=F, cfa=F, graph=T, 
      color=T, legend=T)
loadvar <- pca_loadperm(data,pca_res$rotation,2)

xlabels <- expression(paste("Part. ",alpha),paste("Part. ",delta),paste("Part. ",zeta),paste("Part. ",tau),
                      paste("Comp. ",alpha),paste("Comp. ",zeta),paste("Comp. ",tau))

pltdat <- as.data.frame(loadvar)
pltdat$PC <- c(rep(1:2,10000))
pltdat <- gather(pltdat,key = "Variable",value = "Val",V1:V7)
pltdat$PC <- factor(pltdat$PC, levels = c(1,2), labels = c("PC1","PC2"))
pltdat$Variable <- factor(pltdat$Variable, levels = c("V1","V2","V3","V4","V5","V6","V7"),
                          labels = xlabels)
ggplot(pltdat,aes(Variable,Val,fill = Variable)) + geom_violin(linetype = 0) + stat_summary(fun.data="mean_sdl",fun.args = list(mult = 1),geom="pointrange",color="black") +
  facet_grid(. ~ PC) + ylab("Loading") + scale_fill_manual(values = c("#F8766D","#F8766D","#F8766D","#F8766D","#00BFC4","#00BFC4","#00BFC4","#F8766D","#F8766D","#F8766D","#F8766D","#00BFC4","#00BFC4","#00BFC4")) +
  theme_classic(base_size = 20) + theme(legend.position = "none") +
  scale_x_discrete(labels = xlabels)

# Create parameter sets with low and high values on the first 2 PCA dimensions

wgt <- pca_res$rotation[,1:2] * pca_res$scale
Dim1_high <- colMeans(data_pca)+3*wgt[,1]
Dim1_low <- colMeans(data_pca)-3*wgt[,1]
Dim2_high <- colMeans(data_pca)+3*wgt[,2]
Dim2_low <- colMeans(data_pca)-3*wgt[,2]
Dim2_low[c(4,7)] <- 0.01 #To avoid negative tau set it to 0.01 instead

PCA_pars <- data.frame(Dim1_high,Dim1_low,Dim2_high,Dim2_low)
write.csv(PCA_pars,"PCA_pars.csv",row.names = F)
