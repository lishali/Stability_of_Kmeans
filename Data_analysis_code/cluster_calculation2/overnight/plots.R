library(ggplot2)
library(dplyr)
library(gdata)
library(reshape)

working.directory <- file.path("/Users/LishaLi/Desktop/cluster2/overnight")
setwd("/Users/LishaLi/Desktop/cluster2/overnight")
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) #reading all csv files

l.df <- setNames(lapply(ls(pattern="work"), function(x) get(x)), ls(pattern="work")) #get list of dataframes

freq <- lapply(names(l.df), function(x) upperTriangle(l.df[[x]])) #extracting upper triangular list
# we should remember to delete 20 from each count of correlation similiarity since we also sampled pairs of the form (X,X) (when written uniquely, the list of pairs was not written in a way conducive for producing a nice csv file)
freq[length(freq)] 
freq <- freq[1:length(freq)-1]
names(freq) <- names(l.df)[1:length(freq)-1]
freq <- as.data.frame(freq)
#checked that each had a 190/780 list of things.  

freq <-tbl_df(freq)

d <- melt(freq[,(1:length(freq))])
ggplot(d, aes(x=value))+facet_wrap(~variable, ncol=2)+geom_histogram(binwidth = 0.002)

ggplot(d, aes(x=value))+facet_wrap(~variable, ncol=2)+geom_density()


temp = list.files(pattern="AUC*")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) #reading all csv files
l.df <- setNames(lapply(ls(pattern="AUC"), function(x) get(x)), ls(pattern="AUC")) #get list of dataframes

image.AUC <- l.df[23:25]


image <- cbind(image.AUC[[1]], image.AUC[[2]], image.AUC[[3]])
image$X <- NULL
names(image)[1] <- "image1"
names(image)[2] <- "image2"
names(image)[3] <- "image3"
image
rownames(image) <- "AUC"
image <- t(image)
image <-as.data.frame(image)
image$data <- rownames(image)
image
png("imageAUC.png")
ggplot(image, aes(y=AUC, x=data))+geom_point()
dev.off()

CV.AUC <- l.df[1:11]
title <- c(1, 10, 11, 12, 2:6, 8:9)
crossvalAUC <- cbind(CV.AUC[[1]], CV.AUC[[2]], CV.AUC[[3]], CV.AUC[[4]], CV.AUC[[5]], CV.AUC[[6]], CV.AUC[[7]], CV.AUC[[8]], CV.AUC[[9]], CV.AUC[[10]], CV.AUC[[11]])
crossvalAUC$X <- NULL
colnames(crossvalAUC) <- title
rownames(crossvalAUC) <- "AUC"
crossvalAUC <- as.data.frame(crossvalAUC)
crossvalAUC <- t(crossvalAUC)
colnames(crossvalAUC) <- "AUC"

crossvalAUC <- as.data.frame(crossvalAUC)
crossvalAUC$data <- rownames(crossvalAUC)
crossvalAUC <- mutate(crossvalAUC, data = as.integer(data))
crossvalAUC <- arrange(crossvalAUC, data)
png("CVAUC.png")
ggplot(crossvalAUC, aes(y=AUC, x=data))+geom_point()
dev.off()

converge.AUC <- l.df[12:22]
cauc <- converge.AUC
cauc <- cbind(converge.AUC[[1]], converge.AUC[[2]], converge.AUC[[3]], converge.AUC[[4]], converge.AUC[[5]], converge.AUC[[6]], converge.AUC[[7]], converge.AUC[[8]], converge.AUC[[9]], converge.AUC[[10]], converge.AUC[[11]])
cauc$X <- NULL
title.2 <- c(1, 10:11, 2:9)
colnames(cauc) <- title.2
rownames(cauc) <- "AUC"
cauc <- as.data.frame(cauc)
cauc <- t(cauc)
cauc <- as.data.frame(cauc)
cauc$data <- rownames(cauc)
cauc <- mutate(cauc, data = as.integer(data))
cauc <-arrange(cauc, data)
cauc
png("AUCconverge.png")
ggplot(cauc, aes(y=AUC, x=data))+geom_point()+geom_smooth()
dev.off()


#look at 5=trees but all features dataset: 
setwd(file.path("/Users/LishaLi/Desktop/215A/Lab4/50_trees"))
temp = list.files(pattern="AUC*")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) #reading all csv files
l.df <- setNames(lapply(ls(pattern="AUC"), function(x) get(x)), ls(pattern="AUC")) #get list of dataframes

image.AUC <- l.df[23:25]

image <- cbind(image.AUC[[1]], image.AUC[[2]], image.AUC[[3]])
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
names(image)[1] <- "image1"
names(image)[2] <- "image2"
names(image)[3] <- "image3"
image
image <-as.data.frame(image)
rownames(image) <- "AUC"
image <- t(image)
image <-as.data.frame(image)
image$data <- rownames(image)
image <-as.data.frame(image)
colnames(image)[1] <- "AUC"
image$data <- rownames(image)
png("imageAUC.png")
ggplot(image, aes(y=AUC, x=data))+geom_point()
dev.off()

CV.AUC <- l.df[1:11]
title <- c(1, 10, 11, 12, 2:6, 8:9)
crossvalAUC <- cbind(CV.AUC[[1]], CV.AUC[[2]], CV.AUC[[3]], CV.AUC[[4]], CV.AUC[[5]], CV.AUC[[6]], CV.AUC[[7]], CV.AUC[[8]], CV.AUC[[9]], CV.AUC[[10]], CV.AUC[[11]])
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC <- as.data.frame(crossvalAUC)
colnames(crossvalAUC) <- title
rownames(crossvalAUC) <- "AUC"
crossvalAUC <- t(crossvalAUC)

crossvalAUC <- as.data.frame(crossvalAUC)
crossvalAUC$data <- rownames(crossvalAUC)
crossvalAUC <- mutate(crossvalAUC, data = as.integer(data))
crossvalAUC <- arrange(crossvalAUC, data)
summary(crossvalAUC$AUC)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.6180  0.8901  0.9714  0.9100  0.9901  0.9998 
png("CVAUC.png")
ggplot(crossvalAUC, aes(y=AUC, x=data))+geom_point()
dev.off()

converge.AUC <- l.df[12:22]
cauc <- converge.AUC
cauc <- cbind(converge.AUC[[1]], converge.AUC[[2]], converge.AUC[[3]], converge.AUC[[4]], converge.AUC[[5]], converge.AUC[[6]], converge.AUC[[7]], converge.AUC[[8]], converge.AUC[[9]], converge.AUC[[10]], converge.AUC[[11]])
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
title.2 <- c(1, 10:11, 2:9)
cauc <- as.data.frame(cauc)
colnames(cauc) <- title.2
rownames(cauc) <- "AUC"
cauc <- as.data.frame(cauc)
cauc <- t(cauc)
cauc <- as.data.frame(cauc)
cauc$data <- rownames(cauc)
cauc <- mutate(cauc, data = as.integer(data))
cauc <-arrange(cauc, data)
cauc
summary(cauc$AUC)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.4461  0.6645  0.6948  0.7224  0.7834  0.9482 
png("AUCconverge.png")
ggplot(cauc, aes(y=AUC, x=data))+geom_point()+geom_smooth()
dev.off()



# now look at the trees=25 dataset

setwd(file.path("/Users/LishaLi/Desktop/215A/Lab4/25tree3"))

temp = list.files(pattern="AUC*")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) #reading all csv files
l.df <- setNames(lapply(ls(pattern="AUC"), function(x) get(x)), ls(pattern="AUC")) #get list of dataframes

image.AUC <- l.df[23:25]


image <- cbind(image.AUC[[1]], image.AUC[[2]], image.AUC[[3]])
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
names(image)[1] <- "image1"
names(image)[2] <- "image2"
names(image)[3] <- "image3"
image
rownames(image) <- "AUC"
image <-as.data.frame(image)
image <- t(image)
image$data <- rownames(image)
image <-as.data.frame(image)
colnames(image)[1] <- "AUC"
image$data <- rownames(image)
png("imageAUC.png")
ggplot(image, aes(y=AUC, x=data))+geom_point()
dev.off()

CV.AUC <- l.df[1:11]
title <- c(1, 10, 11, 12, 2:6, 8:9)
crossvalAUC <- cbind(CV.AUC[[1]], CV.AUC[[2]], CV.AUC[[3]], CV.AUC[[4]], CV.AUC[[5]], CV.AUC[[6]], CV.AUC[[7]], CV.AUC[[8]], CV.AUC[[9]], CV.AUC[[10]], CV.AUC[[11]])
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC <- as.data.frame(crossvalAUC)
colnames(crossvalAUC) <- title
rownames(crossvalAUC) <- "AUC"
crossvalAUC <- t(crossvalAUC)

crossvalAUC <- as.data.frame(crossvalAUC)
crossvalAUC$data <- rownames(crossvalAUC)
crossvalAUC <- mutate(crossvalAUC, data = as.integer(data))
crossvalAUC <- arrange(crossvalAUC, data)
png("CVAUC.png")
ggplot(crossvalAUC, aes(y=AUC, x=data))+geom_point()
dev.off()

converge.AUC <- l.df[12:22]
cauc <- converge.AUC
cauc <- cbind(converge.AUC[[1]], converge.AUC[[2]], converge.AUC[[3]], converge.AUC[[4]], converge.AUC[[5]], converge.AUC[[6]], converge.AUC[[7]], converge.AUC[[8]], converge.AUC[[9]], converge.AUC[[10]], converge.AUC[[11]])
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
title.2 <- c(1, 10:11, 2:9)
cauc <- as.data.frame(cauc)
colnames(cauc) <- title.2
rownames(cauc) <- "AUC"
cauc <- as.data.frame(cauc)
cauc <- t(cauc)
cauc <- as.data.frame(cauc)
cauc$data <- rownames(cauc)
cauc <- mutate(cauc, data = as.integer(data))
cauc <-arrange(cauc, data)
cauc
png("AUCconverge.png")
ggplot(cauc, aes(y=AUC, x=data))+geom_point()+geom_smooth()
dev.off()


## plots with tree = 10 , all features
setwd(file.path("/Users/LishaLi/Desktop/215A/Lab4/10_trees"))

temp = list.files(pattern="AUC*")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) #reading all csv files
l.df <- setNames(lapply(ls(pattern="AUC"), function(x) get(x)), ls(pattern="AUC")) #get list of dataframes

image.AUC <- l.df[23:25]


image <- cbind(image.AUC[[1]], image.AUC[[2]], image.AUC[[3]])
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
names(image)[1] <- "image1"
names(image)[2] <- "image2"
names(image)[3] <- "image3"
image
rownames(image) <- "AUC"
image <-as.data.frame(image)
image <- t(image)
image <-as.data.frame(image)
image$data <- rownames(image)
image <-as.data.frame(image)
colnames(image)[1] <- "AUC"
image$data <- rownames(image)
summary(image$AUC)
png("imageAUC.png")
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.6261  0.7089  0.7917  0.7797  0.8565  0.9212 
ggplot(image, aes(y=AUC, x=data))+geom_point()
dev.off()

CV.AUC <- l.df[1:11]
title <- c(1, 10, 11, 12, 2:6, 8:9)
crossvalAUC <- cbind(CV.AUC[[1]], CV.AUC[[2]], CV.AUC[[3]], CV.AUC[[4]], CV.AUC[[5]], CV.AUC[[6]], CV.AUC[[7]], CV.AUC[[8]], CV.AUC[[9]], CV.AUC[[10]], CV.AUC[[11]])
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC <- as.data.frame(crossvalAUC)
colnames(crossvalAUC) <- title
rownames(crossvalAUC) <- "AUC"
crossvalAUC <- t(crossvalAUC)

crossvalAUC <- as.data.frame(crossvalAUC)
crossvalAUC$data <- rownames(crossvalAUC)
crossvalAUC <- mutate(crossvalAUC, data = as.integer(data))
crossvalAUC <- arrange(crossvalAUC, data)
summary(crossvalAUC$AUC)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.6191  0.8688  0.9569  0.9017  0.9852  0.9997 

png("CVAUC.png")
ggplot(crossvalAUC, aes(y=AUC, x=data))+geom_point()
dev.off()

converge.AUC <- l.df[12:22]
cauc <- converge.AUC
cauc <- cbind(converge.AUC[[1]], converge.AUC[[2]], converge.AUC[[3]], converge.AUC[[4]], converge.AUC[[5]], converge.AUC[[6]], converge.AUC[[7]], converge.AUC[[8]], converge.AUC[[9]], converge.AUC[[10]], converge.AUC[[11]])
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
title.2 <- c(1, 10:11, 2:9)
cauc <- as.data.frame(cauc)
colnames(cauc) <- title.2
rownames(cauc) <- "AUC"
cauc <- as.data.frame(cauc)
cauc <- t(cauc)
cauc <- as.data.frame(cauc)
cauc$data <- rownames(cauc)
cauc <- mutate(cauc, data = as.integer(data))
cauc <-arrange(cauc, data)
cauc
summary(cauc$AUC)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.3923  0.5967  0.7227  0.7105  0.8374  0.9303 
png("AUCconverge.png")
ggplot(cauc, aes(y=AUC, x=data))+geom_point()+geom_smooth()
dev.off()


# tree = 2, 3 features

setwd(file.path("/Users/LishaLi/Desktop/215A/Lab4/10tree3"))

temp = list.files(pattern="AUC*")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i])) #reading all csv files
l.df <- setNames(lapply(ls(pattern="AUC"), function(x) get(x)), ls(pattern="AUC")) #get list of dataframes

image.AUC <- l.df[23:25]


image <- cbind(image.AUC[[1]], image.AUC[[2]], image.AUC[[3]])
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
image$X <- NULL
names(image)[1] <- "image1"
names(image)[2] <- "image2"
names(image)[3] <- "image3"
image
rownames(image) <- "AUC"
image <-as.data.frame(image)
image <- t(image)
image <-as.data.frame(image)
image$data <- rownames(image)
image <-as.data.frame(image)
colnames(image)[1] <- "AUC"
image$data <- rownames(image)
summary(image$AUC)
png("imageAUC.png")
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.8458  0.8752  0.9046  0.8885  0.9098  0.9149  
ggplot(image, aes(y=AUC, x=data))+geom_point()
dev.off()

CV.AUC <- l.df[1:11]
title <- c(1, 10, 11, 12, 2:6, 8:9)
crossvalAUC <- cbind(CV.AUC[[1]], CV.AUC[[2]], CV.AUC[[3]], CV.AUC[[4]], CV.AUC[[5]], CV.AUC[[6]], CV.AUC[[7]], CV.AUC[[8]], CV.AUC[[9]], CV.AUC[[10]], CV.AUC[[11]])
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC$X <- NULL
crossvalAUC <- as.data.frame(crossvalAUC)
colnames(crossvalAUC) <- title
rownames(crossvalAUC) <- "AUC"
crossvalAUC <- t(crossvalAUC)

crossvalAUC <- as.data.frame(crossvalAUC)
crossvalAUC$data <- rownames(crossvalAUC)
crossvalAUC <- mutate(crossvalAUC, data = as.integer(data))
crossvalAUC <- arrange(crossvalAUC, data)
summary(crossvalAUC$AUC)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.5903  0.8474  0.9511  0.8854  0.9901  0.9987 

png("CVAUC.png")
ggplot(crossvalAUC, aes(y=AUC, x=data))+geom_point()
dev.off()

converge.AUC <- l.df[12:22]
cauc <- converge.AUC
cauc <- cbind(converge.AUC[[1]], converge.AUC[[2]], converge.AUC[[3]], converge.AUC[[4]], converge.AUC[[5]], converge.AUC[[6]], converge.AUC[[7]], converge.AUC[[8]], converge.AUC[[9]], converge.AUC[[10]], converge.AUC[[11]])
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
cauc$X <- NULL
title.2 <- c(1, 10:11, 2:9)
cauc <- as.data.frame(cauc)
colnames(cauc) <- title.2
rownames(cauc) <- "AUC"
cauc <- as.data.frame(cauc)
cauc <- t(cauc)
cauc <- as.data.frame(cauc)
cauc$data <- rownames(cauc)
cauc <- mutate(cauc, data = as.integer(data))
cauc <-arrange(cauc, data)
cauc
summary(cauc$AUC)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.8343  0.8800  0.8973  0.8970  0.9196  0.9428 
png("AUCconverge.png")
ggplot(cauc, aes(y=AUC, x=data))+geom_point()+geom_smooth()
dev.off()





