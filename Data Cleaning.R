CLS <- function(x){
  ###
  #this function keep "Stock Number" and "CLOSE*ADJFACTOR" 
  ###
  x <- as.data.frame(x)
  x <- data.frame(X = x[,1], Price = x[,5]*x[,11])
}

files <- dir(pattern = '\\.csv', full.names = TRUE) #read all the csv files
tables <- lapply(files, read.csv) #read all the csv files
temp <- lapply(tables, CLS)

A <- Reduce(function(x, y) merge(x, y, by = "X"), temp) # merge all the data set with commen Stock Number to multiple time series
A_1<- na.omit(A) #remove all the Stock containing 'NA'
NoStock <- A_1$X
B <- A_1[,-1]
B <- t(B)
colnames(B) <- NoStock # 'B' is the final version of original data






############ Normalization: #############
A <- B
library(zoo)
library(pracma)
rollingtime <- 10
dim(A)
smo <- rollmean(A, rollingtime) #smooth every time series
dim(smo)
head(smo) # 1077 days, 1673 stocks

# compare the smooth one with the original
plot(smo[,10],type = "l")
lines(A[,10],col = "red")

# remove the trend in each time series
smo.detr <- detrend(smo,tt = 'linear')

# normalization:
smo.detr.norm <- scale(smo.detr, scale = TRUE)
#write.csv(smo.detr.norm, file = "STOCK DATA.csv")



# PCA on transformed data
#head(smo.detr.piecenorm)
#pc.cr <- princomp(t(smo.detr.piecenorm), cor = FALSE)
#plot(pc.cr)
#summary(pc.cr)
#calculate the distance between each pair of time series:


########################### Visualization ####################
#smo.detr.norm <- read.csv("STOCK DATA.csv")
#smo.detr.norm <- smo.detr.norm[,-1]
ptm <- proc.time()
label <- global.kmeans(t(smo.detr.norm), 25, 0.0000001)
proc.time() - ptm

#label <- read.csv("Label.csv")
table(label$x)


# Clustering Results (Transformed Data) Visualization:
for(k in 1:25){
  png(file = paste("Cluster", k, ".png"), bg = "aquamarine3")
  plot(smo.detr.norm[,label$x == k][,1], type = 'l', xlab = "Date", ylab = "Transformed Data",
       ylim = c(-5,5), main = paste("Cluster", k), col = k)
  for(i in 2:table(label$x)[k]){
    lines(smo.detr.norm[,label$x == k][,i],col = k)
  }
  dev.off()
} # please see all the .png pictures in R working dictionary


# All Transformed Data in One Picture:
png(file = paste("All Transformed Data.png"), bg = "transparent")
plot(smo.detr.norm[,1], type = 'l', xlab = "Date", ylab = "Transformed Data",
     ylim = c(-5,5), main = "All Transformed Data", col = label$x[1])
for(i in 2:dim(smo.detr.norm)[2]){
  lines(smo.detr.norm[,i], col = label$x[i])
}
dev.off()

