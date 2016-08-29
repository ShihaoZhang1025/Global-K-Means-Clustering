########################## global.kmeans #########################
global.kmeans <- function(H, k, tau){
  #H: the input data set, each row corresponding to an observation
  #k: the number of the clusters
  #tau is the threshold parameter
  
  N<-dim(H)[1] #N is the number of the observations
  D<-dim(H)[2] #D is the dimensions
  
  #ctr <- matrix(rep(0,k*nc),k,nc)
  ctr <- colMeans(H) # when k=1, the centroid of the k-means is colMeans(x)
  ctr <- t(as.data.frame(ctr))
  #ctr_index <- NULL
  #count <- c(1:N)
  
  for(i in 2:k){
    # try every stock point as initial to run k.means, get all the possible results for i-th center
    ctr_temp <- apply(H, 1, k.means, H = H, K = ctr, tau = tau)
    if(is.matrix(ctr_temp)) 
      ctr_temp <- as.list(data.frame(ctr_temp))
    
    # get every possible "ctr" in i-th iteration
    bb <- lapply(ctr_temp, rbind, x = ctr)
    
    #get the row number of each ctr
    get_row_ctr <- function(x) { dim(as.data.frame(x))[1] }
    row_ctr <- sapply(bb, get_row_ctr) 
    bb <- bb[row_ctr == i]
    
    #d <- lapply(bb, dist2, x = H)
    # assign lables to every stock point
    assigh_lable <- function(x) {apply(as.data.frame(x), 1, which.min)}
    comb_lable_1toi <- lapply(lapply(bb, dist2, x = H), assigh_lable)
    
    # filtering all the unqualified label list
    yy <- lapply(comb_lable_1toi, table) 
    # "comb_lable_1toi" -- each list -- contains assigned label for H corresponding to each poiont
    label_filter <- (lapply(comb_lable_1toi, max) == i)& (lapply(yy, min) > 20)
    bb <- bb[label_filter]
    comb_lable_1toi <- comb_lable_1toi[label_filter]
    
    # get the mean of each group
    mean_groups <- function(cl) {apply(H, 2, function(x) tapply(x, cl, mean))}
    comb_mean_groups <- lapply(comb_lable_1toi, mean_groups)
    #"comb_mean_groups" -- each list -- contains the seperate means of each group
    mean_all <- apply(H, 2, mean)
    
    cc <- function(x) {dist2(as.matrix(x), mean_all)^2}
    dd <- lapply(comb_mean_groups, cc)
    
    cal_var <- function(x,y) { table(x)%*% y}
    var_btw <- mapply(cal_var, comb_lable_1toi, dd) # variance between groups
    
    ctr<- as.data.frame(bb[which.max(var_btw)]) # select the max variance between groups
    
  }
 
  overall_d <- dist2(H, ctr) # the final distence between all the centroid and the input data,
  cluster<- apply(overall_d,1,which.min) # return the final clusters.
  
  return(cluster)
}
##############################################################









################### one by one k.means function ##################
k.means <- function(tk, H, K, tau){
  #tk is the input initial point
  #H is the matrix of input stock data set
  #K is the matrix of initial centers, which is already known
  #tau is the threshold parameter
  
  k<-dim(K)[1] #k is the number of the known centroids
  # select each point in H as the new center of the new cluster.
  
  if(VecMatch(K, tk)) return(NULL)
  
  else{
  DM <- dist2(H,K) #dist2 matrix between the known center and the observations
  delta <- 1
  
  while(delta >= tau){
    #E-step
    dm <- dist2(H,tk) # dist2 between the new center and the observations
    A <- cbind(DM,dm)
    m <- apply(A, 1, which.min) # reassign the hidden label(cluster)
    
    if(max(m) < k+1) {
      tk <-NULL
      break
    }
    
    #M-step
    tk_old <- tk
    if(is.vector(H[m==k+1,])) 
      tk <- H[m==k+1,]
    else
      tk <- colMeans(H[m==k+1,]) # update the new centroid for the (k+1)th cluster
    #a measure of change
    delta <- sqrt(sum((tk-tk_old)^2)) # untill the new centroid does not change.
    }
  
  return(tk)
  }
}
########################################################









#################### Check Whether a Vector is Contained in a Matrix ##################
VecMatch <- function(x, want){
  out <- apply(x, 1, function(x, want) isTRUE(all.equal(x, want)), want)
  any(out)
}
#######################################################################################

debugonce(global.kmeans)

ptm <- proc.time()
label <- global.kmeans(t(a), 15, 0.0001)
proc.time() - ptm


###################### Test the stabel the this Algorithm ######################
freq_all <- NULL
for(i in 1:5){
  freq <- as.data.frame(table(label$x))[,-1]
  freq_all <- cbind(freq_all,freq)
}

colnames(freq_all) <- c(1:5)
barplot(freq_all,col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk", 
                         "darkgoldenrod1", "bisque", "cadetblue3", "azure", "aquamarine", "cyan", "blanchedalmond"))

###############################################################################