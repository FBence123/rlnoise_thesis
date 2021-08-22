pca_loadperm<- function(data, load_ori, k, nperm = 10000){
  library(ggplot2);library(ggsci)
  pca_out<- prcomp(data, scale. = T, center = T)
  nsamp <- floor(nrow(data)*0.9)
  data_i<- data.frame(matrix(NA, nsamp, ncol(data)))
  loadvar <- matrix(NA,nperm*k,nrow(load_ori))
  for (j in 1: nperm){
    idx <- sample(nrow(data),size = nsamp, replace = FALSE)
    data_i <- data[idx,]
    pca.perm<- prcomp(data_i, scale. = T, center = T)
    loadvar[((j*k)-(k-1)):(j*k),] <- abs(t(pca.perm$rotation[,1:k]))
  }

  loadvar
}