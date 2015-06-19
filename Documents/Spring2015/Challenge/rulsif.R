library(MASS)
library(kernlab)
library(foreach)
library(doParallel)
## Function to compute median distance ##
calcMed = function(x){
  # x: the vector containing samples to calculate the distance
  len = length(x)
  g = x^2
  t = matrix(rep(g,len),len)
  d = t - 2 * x %*% t(x) + t(t)
  d = d[lower.tri(d)]
  med = sqrt(0.5 * median(d[d>0]))
  return(med)
}

rulsif = function(xnu,xde,alpha){
  # xnu : samples from numerator distribution
  # xde : samples from denominator distribution
  # alpha: alpha value in relative density ratio
  # 5-fold cross validation is used to choose parameters
  # rPE: relative Pearson Divergence is returned
  med = calcMed(c(xnu,xde))
  sigma_list = med * seq(0.6,1.4,0.2) # The parameters for model selection
  lambda_list = 10^c(-3:1)
  # 5- fold cross validation to select sigma and lambda
    len1 = length(sigma_list)
    len2 = length(lambda_list)
    score = matrix(rep(0,len1*len2),nrow = len1)
    registerDoParallel()
    # function to calculate the squared loss of sigma and lambda_list
    # with 5-fold cross validation
    lambda.cv = function(sigma){
      knu = kernelMatrix(rbfdot(sigma=1/(2*sigma^2)), xnu, xnu)
      kde = kernelMatrix(rbfdot(sigma=1/(2*sigma^2)), xde, xnu)
      result = rep(0,length(lambda_list))
      for (i in 1:length(lambda_list)){
        lambda = lambda_list[i]
        cv_nu = sample(1:length(xnu))
        cv_de = sample(1:length(xde))
        index_nu = floor(seq(1,length(xnu),length.out = 6))
        index_de = floor(seq(1,length(xde),length.out = 6))
        loss = 0
        for(k in 1:5){
          # Get train and test data
          de_test = kde[cv_de[index_de[k]:index_de[k+1]],]
          de_train = kde[-cv_de[(index_de[k]:index_de[k+1])],]
          nu_test = knu[cv_nu[index_nu[k]:index_nu[k+1]],]
          nu_train = knu[-cv_nu[(index_nu[k]:index_nu[k+1])],]
          # estimate with train data
          H = alpha* t(nu_train) %*% nu_train/dim(nu_train)[1] + (1-alpha) * t(de_train) %*% de_train/dim(de_train)[1];
          h = colMeans(nu_train)
          theta = pmax(ginv(H + lambda * diag(dim(H)[1])) %*% h,0)
          # calculate squared loss
          J = alpha/2 * mean((nu_test %*% theta)^2) + (1 - alpha)/2 * mean((de_test %*% theta)^2)
          loss = loss + J 
        }
        result[i] = loss
      }
      return(result)
    }
    
    # Use function defined above to do cross validation
    score = foreach(sigma = sigma_list,.combine = rbind) %dopar% lambda.cv(sigma)
    index = which(score == min(score),arr.ind = TRUE)
    sigma_chosen = sigma_list[index[1]]
    lambda_chosen = lambda_list[index[2]]
    
    # Compute final result
    knu = kernelMatrix(rbfdot(sigma=1/(2*sigma_chosen^2)), xnu, xnu)
    kde = kernelMatrix(rbfdot(sigma=1/(2*sigma_chosen^2)), xde, xnu)
    H = alpha* t(knu) %*% knu/dim(knu)[1] + (1-alpha) * t(kde) %*% kde/dim(kde)[1];
    h = colMeans(knu)
    theta = pmax(ginv(H + lambda_chosen * diag(dim(H)[1])) %*% h,0)
    g_nu = knu %*% theta
    g_de = kde %*% theta
    rPE = mean(g_nu) - 0.5 * alpha * mean(g_nu^2) + (1-alpha)*mean(g_de^2) - 0.5
    return(rPE) 
}
