library(readr)
library(readxl)
library(openxlsx)
library(corrplot)
library(geometry)

setwd("C:/Users/farka/OneDrive/Desktop/RLNOISE")
complete <- read_csv("complete_task_exc.csv")
partial <- read_csv("partial_task_exc.csv")
all_outliers <- read_excel("all.outliers.ids.xlsx")
all_outliers <- all_outliers[all_outliers$prolific_id %in% complete$pid,]

idx <- match(complete$pid,all_outliers$prolific_id)
list_outliers <- all_outliers[idx,4]
list_outliers$partial_ll_chance <- partial$ll_chance
list_outliers$complete_ll_chance <- complete$ll_chance
list_outliers$partial_cor_chance <- partial$cor_chance
list_outliers$complete_cor_chance <- complete$cor_chance
list_outliers$partial_var <- partial$var
list_outliers$complete_var <- complete$var
list_outliers$partial_rew <- partial$rew_bad
list_outliers$complete_rew <- complete$rew_bad
list_outliers$partial_param <- partial$param_bad
list_outliers$complete_param <- complete$param_bad


list_outliers$any_ll <- ifelse((list_outliers$partial_ll_chance | list_outliers$complete_ll_chance) == 1, 1, 0)
list_outliers$any_cor <- ifelse((list_outliers$partial_cor_chance | list_outliers$complete_cor_chance) == 1, 1, 0)
list_outliers$any_var <- ifelse((list_outliers$partial_var | list_outliers$complete_var) == 1, 1, 0)
list_outliers$any_rew <- ifelse((list_outliers$partial_rew | list_outliers$complete_rew) == 1, 1, 0)
list_outliers$any_par <- ifelse((list_outliers$partial_param | list_outliers$complete_param) == 1, 1, 0)
list_outliers$questionnaire <- all_outliers$questionnaire_outliers
list_outliers$final <- ifelse((list_outliers$any_ll | list_outliers$questionnaire) == 1, 1, 0)

write.csv(list_outliers,"Bence_list_outliers.csv",row.names = FALSE)
write.xlsx(list_outliers,"Bence_list_outliers.xlsx")

agree <- matrix(0L,nrow = 6,ncol = 6)
dice <- matrix(0L,nrow = 6,ncol = 6)
for (i in 1:6) {
  for (j in 1:6) {
    agree[i,j] = sum(list_outliers[,i+11] == list_outliers[,j+11])/213
    dice[i,j] = 2*dot(list_outliers[,i+11],list_outliers[,j+11])/(sum(list_outliers[,i+11]^2)+sum(list_outliers[,j+11]^2))
  }
}

colSums(list_outliers[,12:17])
colSums(list_outliers[,12:17])/213*100

sum(list_outliers$final)
sum(list_outliers$final)/213*100

colnames(agree) <- c("ll_chance","cor_chance","var_chance","rew","param","questionnaire")
rownames(agree) <- c("ll_chance","cor_chance","var_chance","rew","param","questionnaire")
colnames(dice) <- c("ll_chance","cor_chance","var_chance","rew","param","questionnaire")
rownames(dice) <- c("ll_chance","cor_chance","var_chance","rew","param","questionnaire")

corrplot.mixed(agree,upper = "square", lower.col = "black", tl.col = "black",  title = "Pairwise agreement",mar=c(0,0,1,0))
corrplot.mixed(dice,upper = "square", lower.col = "black", tl.col = "black", title = "Dice similarity",mar=c(0,0,1,0))
