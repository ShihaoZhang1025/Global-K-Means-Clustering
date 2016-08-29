#####################Similarity Measure######################
#The official classification which can be considered as ground truth
label_compare <- read.csv("label_compare.csv", stringsAsFactors = FALSE, header = TRUE, sep = ",")
head(label_compare)
class(label_compare[40,1])
label_compare <- label_compare[label_compare[,1]!="",]
label_compare <- label_compare[label_compare[,2]!="",]
label_compare <- label_compare[,-c(2,3)]
colnames(label_compare) <- c("No.stock","Abbr","Label_1","Label_2")


intersect <- function(x, y) y[match(x, y, nomatch = 0)]
colnames(FD)

Original_lable <- read.csv("label_compare.csv")
#head(Original_lable)
#class(Original_lable)
colnames(Original_lable)[1] <- c("No.stock")


#combined_label[1:5,]

Sim <- function(memb){
  ###
  #This is Similarity Measure function
  ###
  Result <- cbind(colnames(B), memb)
  colnames(Result)[1] <- c("No.stock")
  combined_label <- merge(Original_lable, Result, by = "No.stock")
  combined_label[,3] <- as.numeric(combined_label[,3])
  combined_label[,4] <- as.numeric(combined_label[,4])
  colnames(combined_label)[3:4] <- c("ZJH","Wind")
  m <- length(table(combined_label$memb))
  n <- length(table(combined_label$ZJH))
  S <- matrix(rep(0,m*n), nrow = m)
  for (i in  1:m){
    for(j in 1:n){
      S[i,j] <- 2*length(intersect(colnames(FD)[combined_label$memb == i],colnames(FD)[combined_label$ZJH==j])) / (length(colnames(FD)[combined_label$memb == i])+length(colnames(FD)[combined_label$ZJH==j]))
    }
  }
  
  sim <- sum(apply(S, 2, max))/m
  return(sim)
}

Sim(memb)
Sim(label$x)
