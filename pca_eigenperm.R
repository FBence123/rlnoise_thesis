pca_eigenperm<- function(data, sdev_ori, nperm = 10000){
  pca_out<- prcomp(data, scale. = T, center = T)
  eigenperm<- data.frame(matrix(NA, nperm, ncol(data)))
  n<- ncol(data)
  data_i<- data.frame(matrix(NA, nrow(data), ncol(data)))
  for (j in 1: nperm){
    for (i in 1:n){
      data_i[,i]<- sample(data[,i], replace = FALSE)
    }
    pca.perm<- prcomp(data_i, scale. = T, center = T)
    eigenperm[j,]<- pca.perm$sdev
  }
  colnames(eigenperm)<- colnames(pca_out$rotation)
  p <- data.frame(matrix(NA,1,ncol(data)))
  for (i in 1:n){
    exc <- sdev_ori[i] < eigenperm[,i]
    p[i] <- sum(exc)/length(exc)
  }
  list(p,eigenperm)
}