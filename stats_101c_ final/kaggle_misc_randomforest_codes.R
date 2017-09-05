rf_mse1
kaggle_data <- read.csv("/Users/z/Desktop/stats_101c/kaggle/testing.without.response")
kaggle_pred <- predict(rf.1, kaggle_data)
row.id <- kaggle_data$row.id
prediction <- kaggle_pred
avg_na_free <- mean(na.omit(prediction))
for(i in 1:length(prediction)){
if(is.na(prediction[i])){
prediction[i] = avg_na_free
}
}
factors(kaggle_pred[,6])
levels(kaggle_pred[,6])
levels(kaggle_data[,6])
levels(kaggle_data[,7])
kaggle_data[,10] <- as.numeric(kaggle_data[,10])
kaggle_pred <- predict(rf.1, kaggle_data)
row.id <- kaggle_data$row.id
prediction <- kaggle_pred
avg_na_free <- mean(na.omit(prediction))
for(i in 1:length(prediction)){
if(is.na(prediction[i])){
prediction[i] = avg_na_free
}
}
kaggle_submit3 <- cbind(row.id,prediction)
write.csv(kaggle_submit3, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod3.txt", row.names = FALSE, quote=FALSE)
set.seed(17)
train <- sample(c(1:2774370), 2774370*.55, replace = FALSE)
lafd <- read.csv("/Users/z/Desktop/stats_101c/kaggle/lafdtraining.csv")
lafd_rf_train <- data.frame(na.omit(lafd[complete.cases(lafd[train,]),c(6,7,11)]))
lafd_rf_test <- data.frame(na.omit(lafd[complete.cases(lafd[-train,]),c(6,7,11)]))
rf.2 <- randomForest(elapsed_time~., data = lafd_rf_train, ntree=30)
rf_pred2 <- predict(rf.2, lafd_rf_test)
rf_mse2 <- mean((rf_pred1 - lafd_rf_test$elapsed_time)^2)
rf_mse2
rf_mse2 <- mean((rf_pred2 - lafd_rf_test$elapsed_time)^2)
rf_mse2
rf_c <- combine(rf.1, rf.2)
set.seed(21)
train <- sample(c(1:2774370), 2774370*.55, replace = FALSE)
lafd <- read.csv("/Users/z/Desktop/stats_101c/kaggle/lafdtraining.csv")
lafd_rf_train <- data.frame(na.omit(lafd[complete.cases(lafd[train,]),c(6,7,11)]))
lafd_rf_test <- data.frame(na.omit(lafd[complete.cases(lafd[-train,]),c(6,7,11)]))
rf.3 <- randomForest(elapsed_time~., data = lafd_rf_train, ntree=10)
rf_pred3 <- predict(rf.3, lafd_rf_test)
rf_mse3 <- mean((rf_pred3 - lafd_rf_test$elapsed_time)^2)
rf_mse3
rf_c <- combine(rf.2,rf.3)
install.packages("rpart")
set.seed(17)
train <- sample(c(1:2774370), 2774370*.69, replace = FALSE)
lafd <- read.csv("/Users/z/Desktop/stats_101c/kaggle/lafdtraining.csv")
lafd_rf_train <- data.frame(na.omit(lafd[complete.cases(lafd[train,]),c(6,7,11)]))
lafd_rf_test <- data.frame(na.omit(lafd[complete.cases(lafd[-train,]),c(6,7,11)]))
rf.4 <- randomForest(elapsed_time~., data = lafd_rf_train, ntree=90)
rf_pred4 <- predict(rf.4, lafd_rf_test)
rf_mse4 <- mean((rf_pred4 - lafd_rf_test$elapsed_time)^2)
rf_mse4
kaggle_pred <- predict(rf.4, kaggle_data)
row.id <- kaggle_data$row.id
prediction <- kaggle_pred
avg_na_free <- mean(na.omit(prediction))
for(i in 1:length(prediction)){
if(is.na(prediction[i])){
prediction[i] = avg_na_free
}
}
kaggle_submit4a <- cbind(row.id,prediction)
write.csv(kaggle_submit4a, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4a.txt", row.names = FALSE, quote=FALSE)
set.seed(20)
train <- sample(c(1:2774370), 2774370*.69, replace = FALSE)
lafd_rf_train <- data.frame(na.omit(lafd[complete.cases(lafd[train,]),c(6,7,10,11)]))
lafd_rf_train[,3] <- as.numeric(lafd_rf_train[,3])
lafd_rf_test <- data.frame(na.omit(lafd[complete.cases(lafd[-train,]),c(6,7,10,11)]))
lafd_rf_test[,3] <- as.numeric(lafd_rf_test[,3])
rf.4b <- randomForest(elapsed_time~., data = lafd_rf_train, ntree=85)
rf_pred4b <- predict(rf.4b, lafd_rf_test)
rf_mse4b <- mean((rf_pred4b - lafd_rf_test$elapsed_time)^2)
rf_mse4b
rf_mse4b
kaggle_data <- read.csv("/Users/z/Desktop/stats_101c/kaggle/testing.without.response")
kaggle_data[,10] <- as.numeric(kaggle_data[,10])
kaggle_pred <- predict(rf.4b, kaggle_data)
prediction <- kaggle_pred
avg_na_free <- mean(na.omit(prediction))
for(i in 1:length(prediction)){
if(is.na(prediction[i])){
prediction[i] = avg_na_free
}
}
kaggle_submit4b <- cbind(row.id,prediction)
write.csv(kaggle_submit4b, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4b.txt", row.names = FALSE, quote=FALSE)
mod4a <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4a.csv")
mod4b <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4b.csv")
mod4c <- mod4b
mod4c_pred <- (mod4a[,2]+mod4b[,2])/2
head(mod4c_pred)
prediction <- mod4c_pred
row.id <- mod4a[,1]
kaggle_submit4c <- cbind(row.id, prediction)
write.csv(kaggle_submit4c, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4c.txt", row.names = FALSE, quote=FALSE)
head(lafd$4)
head(lafd[,4])
names(lafd)
pairs(lafd[,c(4,11)])
head(lafd[,6])
head(lafd[,7])
head(lafd[,8])
head(lafd[,6], 60)
levels(lafd[,6])
lafd_6 <- c("a",lafd[,6])
lafd_6 <- lafd_6[c(-1)]
head(lafd_6)
levels(lafd_6)
levels(as.factor(lafd_6))
head(lafd[,7])
head(lafd[,8])
head(lafd[,9])
set.seed(19)
train <- sample(c(1:2774370), 2774370*.8, replace = FALSE)
lafd_xgb_train <- data.frame(na.omit(lafd[complete.cases(lafd[train,]),c(6,7,8,9,10,11)]))
lafd_xgb_train[,5] <- as.numeric(lafd_rf_train[,5])
lafd_xgb_test <- data.frame(na.omit(lafd[complete.cases(lafd[-train,]),c(6,7,8,9,10,11)]))
lafd_xgb_test[,5] <- as.numeric(lafd_rf_test[,5])
sparse_matrix <- sparse.model.matrix(elapsed_time ~ .-1, data = lafd_xgb_train[,c(2,3,4,6)])
head(sparse_matrix)
head(lafd[,7])
head(as.numeric(lafd[,7]))
lafd_xgb_train <- as.numeric(lafd_xgb_train)
lafd_xgb_train[,2] <- as.numeric(lafd_xgb_train[,2])
lafd_xgb_train[,3] <- as.numeric(lafd_xgb_train[,3])
lafd_xgb_train[,4] <- as.numeric(lafd_xgb_train[,4])
head(lafd_xgb_train)
lafd_xgb_train[,5] <- as.numeric(lafd_xgb_train[,5])
head(lafd_xgb_train)
?xgboost
install.packages("xgboost")
library(xgboost)
?xgboost
set.seed(23)
train <- sample(c(1:2774370), 2774370*.69, replace = FALSE)
lafd_rf_train <- data.frame(na.omit(lafd[complete.cases(lafd[train,]),c(6,78,9,10,11)]))
lafd_rf_train[,5] <- as.numeric(lafd_rf_train[,5])
lafd_rf_test <- data.frame(na.omit(lafd[complete.cases(lafd[-train,]),c(6,7,8,9,10,11)]))
set.seed(23)
library(randomForest)
train <- sample(c(1:2774370), 2774370*.69, replace = FALSE)
lafd_rf_train <- data.frame(na.omit(lafd[complete.cases(lafd[train,]),c(6,7,8,9,10,11)]))
lafd_rf_train[,5] <- as.numeric(lafd_rf_train[,5])
lafd_rf_test <- data.frame(na.omit(lafd[complete.cases(lafd[-train,]),c(6,7,8,9,10,11)]))
lafd_rf_test[,5] <- as.numeric(lafd_rf_test[,5])
rf.5 <- randomForest(elapsed_time~., data = lafd_rf_train, ntree=100)
rf_pred5 <- predict(rf.5, lafd_rf_test)
rf_mse5 <- mean((rf_pred5 - lafd_rf_test$elapsed_time)^2)
rf_mse5
kaggle_data <- read.csv("/Users/z/Desktop/stats_101c/kaggle/testing.without.response")
kaggle_data[,10] <- as.numeric(kaggle_data[,10])
levels(kaggle_data[,7])
levels(kaggle_data[,6])
levels(kaggle_data[,8])
levels(lafd_rf_train[,2])
levels(lafd_rf_train[,3])
levels(kaggle_data[,8]) <- levels(lafd_rf_train[,3])
kaggle_pred <- predict(rf.4b, kaggle_data)
kaggle_pred <- predict(rf.5, kaggle_data)
row.id <- kaggle_data$row.id
prediction <- kaggle_pred
head(prediction)
avg_na_free <- mean(na.omit(prediction))
for(i in 1:length(prediction)){
if(is.na(prediction[i])){
prediction[i] = avg_na_free
}
}
kaggle_submit5 <- cbind(row.id,prediction)
write.csv(kaggle_submit5, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod5.txt", row.names = FALSE, quote=FALSE)
mod5_1 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4a.csv")
mod5_2 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4b.csv")
mod5_3 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kaggle_mod5.csv")
mod5_1.pred <- mod5_1[,2]
mod5_2.pred <- mod5_2[,2]
mod5_3.pred <- mod5_3[,2]
prediction <- (3*mod5_2.pred + 2*mod5_1.pred + mod5_3.pred)/6
kaggle_submit5c <- cbind(row.id,prediction)
write.csv(kaggle_submit5c, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod5c.txt", row.names = FALSE, quote=FALSE)
max(lafd[,4])
max(na.omit(lafd[,4]))
min(na.omit(lafd[,4]))
names(lafd)
head(lafd[,9])
length(lafd[,4])
levels(lafd[,4])
levels(as.factor(lafd[,4]))
as.numeric(levels(as.factor(lafd[,4])))
for(i in as.numeric(levels(as.factor(lafd[,4]))))
print(i)
temp <- names(lafd_rf_train)
temp
set.seed(27)
train <- sample(c(1:2774370), 2774370*.69, replace = FALSE)
lafd_rf_train <- data.frame(na.omit(lafd[complete.cases(lafd[train,]),c(4,6,7,9,10,11)]))
lafd_rf_train[,5] <- as.numeric(lafd_rf_train[,5])
lafd_rf_test <- data.frame(na.omit(lafd[complete.cases(lafd[-train,]),c(4,6,7,9,10,11)]))
lafd_rf_test[,5] <- as.numeric(lafd_rf_test[,5])
temp <- names(lafd_rf_train)
blah <- lafd_rf_train[3,]
rb <- rowbind(temp, blah)
rb <- rbind(temp, blah)
temp <- c()
rb <- rbind(temp, blah)
rb
blah <- lafd_rf_train[4,]
rb <- rbind(rb, blah)
rb
kaggle_lin <- c()
for(i in as.numeric(levels(as.factor(lafd[,4])))){
temp <- c()
for(j in 1:length(lafd_rf_train[,1])){
if(lafd_rf_train[j,1]==i)
temp <- rbind(temp,lafd_rf_train[j,])
}
kag <- c()
for(j in 1:length(kaggle_dat[,1])){
if(kaggle_dat[j,4]==i)
kag <- rbind(kag,lafd_rf_train[j,kaggle_dat[j,c(1,6,7,9,10,11)]])
}
mod_i <- lm(elapsed_time~., data = temp[,c(-1)])
kag[,5] <- as.numeric(kag[,5])
prediction <- predict(mod_i, kag)
row.id <- kag[,1]
pred <- cbind(row.id,prediction)
kaggle_lin <- rbind(kaggle_lin, pred)
print(i)
}
kaggle_lin <- c()
for(i in as.numeric(levels(as.factor(lafd[,4])))){
temp <- c()
for(j in 1:length(lafd_rf_train[,1])){
if(lafd_rf_train[j,1]==i)
temp <- rbind(temp,lafd_rf_train[j,])
}
kag <- c()
for(j in 1:length(kaggle_dat[,1])){
if(kaggle_dat[j,4]==i)
kag <- rbind(kag,lafd_rf_train[j,kaggle_dat[j,c(1,6,7,9,10,11)]])
}
#tester <- c()
#for(j in 1:length(lafd_rf_test[,1])){
#  if(lafd_rf_test[j,1]==i)
#    tester <- rbind(tester,lafd_rf_test[j,])
#}
mod_i <- lm(elapsed_time~., data = temp[,c(-1)])
kag[,5] <- as.numeric(kag[,5])
#test_pred <- predict(mod_i, tester)
prediction <- predict(mod_i, kag)
row.id <- kag[,1]
pred <- cbind(row.id,prediction)
kaggle_lin <- rbind(kaggle_lin, pred)
print(i)
}
kaggle_dat <- read.csv("/Users/z/Desktop/stats_101c/kaggle/testing.without.response")
lafd_rf_train <- data.frame(na.omit(lafd[complete.cases(lafd[train,]),c(4,6,7,9,10,11)]))
lafd_rf_train[,5] <- as.numeric(lafd_rf_train[,5])
lafd_rf_test <- data.frame(na.omit(lafd[complete.cases(lafd[-train,]),c(4,6,7,9,10,11)]))
lafd_rf_test[,5] <- as.numeric(lafd_rf_test[,5])
kaggle_lin <- c()
for(i in as.numeric(levels(as.factor(lafd[,4])))){
temp <- c()
for(j in 1:length(lafd_rf_train[,1])){
if(lafd_rf_train[j,1]==i)
temp <- rbind(temp,lafd_rf_train[j,])
}
kag <- c()
for(j in 1:length(kaggle_dat[,1])){
if(kaggle_dat[j,4]==i)
kag <- rbind(kag,lafd_rf_train[j,kaggle_dat[j,c(1,6,7,9,10,11)]])
}
#tester <- c()
#for(j in 1:length(lafd_rf_test[,1])){
#  if(lafd_rf_test[j,1]==i)
#    tester <- rbind(tester,lafd_rf_test[j,])
#}
mod_i <- lm(elapsed_time~., data = temp[,c(-1)])
kag[,5] <- as.numeric(kag[,5])
#test_pred <- predict(mod_i, tester)
prediction <- predict(mod_i, kag)
row.id <- kag[,1]
pred <- cbind(row.id,prediction)
kaggle_lin <- rbind(kaggle_lin, pred)
print(i)
}
kaggle_lin <- c()
for(i in as.numeric(levels(as.factor(lafd[,4])))){
temp <- c()
for(j in 1:length(lafd_rf_train[,1])){
if(lafd_rf_train[j,1]==i)
temp <- rbind(temp,lafd_rf_train[j,])
}
kag <- c()
for(j in 1:length(kaggle_dat[,1])){
if(kaggle_dat[j,4]==i)
kag <- rbind(kag,lafd_rf_train[j,kaggle_dat[j,c(1,6,7,9,10)]])
}
#tester <- c()
#for(j in 1:length(lafd_rf_test[,1])){
#  if(lafd_rf_test[j,1]==i)
#    tester <- rbind(tester,lafd_rf_test[j,])
#}
mod_i <- lm(elapsed_time~., data = temp[,c(-1)])
kag[,5] <- as.numeric(kag[,5])
#test_pred <- predict(mod_i, tester)
prediction <- predict(mod_i, kag)
row.id <- kag[,1]
pred <- cbind(row.id,prediction)
kaggle_lin <- rbind(kaggle_lin, pred)
print(i)
}
kaggle_lin <- c()
for(i in as.numeric(levels(as.factor(lafd[,4])))){
temp <- c()
for(j in 1:length(lafd_rf_train[,1])){
if(lafd_rf_train[j,1]==i)
temp <- rbind(temp,lafd_rf_train[j,])
}
kag <- c()
for(j in 1:length(kaggle_dat[,1])){
if(kaggle_dat[j,4]==i)
kag <- rbind(kag,kaggle_dat[j,c(1,6,7,9,10)])
}
#tester <- c()
#for(j in 1:length(lafd_rf_test[,1])){
#  if(lafd_rf_test[j,1]==i)
#    tester <- rbind(tester,lafd_rf_test[j,])
#}
mod_i <- lm(elapsed_time~., data = temp[,c(-1)])
kag[,5] <- as.numeric(kag[,5])
#test_pred <- predict(mod_i, tester)
prediction <- predict(mod_i, kag)
row.id <- kag[,1]
pred <- cbind(row.id,prediction)
kaggle_lin <- rbind(kaggle_lin, pred)
print(i)
}
library(randomForest)
set.seed(17)
lafd.1 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/lafd_1", sep = "\t")
train <- sample(c(1:1215717), 1215717*.70, replace = FALSE)
lafd.1.train <- lafd.1[train,c(2:6)]
lafd.1.test <- lafd.1[-train,c(2:6)]
lafd.1.train[,1] <- as.character(lafd.1.train[,1])
lafd.1.test[,1] <- as.character(lafd.1.test[,1])
rfz.1 <- randomForest(elapsed_time~., data = lafd.1.train, ntree=15)
rfz.1.pred <- predict(rfz.1, lafd.1.test)
rfz.1.mse <- mean((rfz.1.pred - lafd.1.test$elapsed_time)^2)
rfz.1.mse
lafd.1k.train <- lafd.1.train[,-1]
lafd.1k.test <- lafd.1.test[,-1]
rfz.1k <- randomForest(elapsed_time~., data = lafd.1k.train, ntree=15)
rfz.1k.pred <- predict(rfz.1k, lafd.1k.test)
rfz.1k.mse <- mean((rfz.1k.pred - lafd.1k.test$elapsed_time)^2)
rfz.1k.mse
set.seed(17)
lafd.2 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/lafd_2", sep = "\t")
length(lafd.2$row_id)
kag.1 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kag_1", sep = "\t")
names(kag.1)
kag.1[,2] <- as.character(kag.1[,2])
kag.1.pred <- predict(rfz.1, kag.1)
prediction <- kag.1.pred
row.id <- kag.1[,1]
avg_na_free <- mean(na.omit(prediction))
for(i in 1:length(prediction)){
if(is.na(prediction[i])){
prediction[i] = avg_na_free
}
}
kaggle_submit_10z_1 <- cbind(row.id,prediction)
write.csv(kaggle_submit_10z_1, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod_10z_1.txt", row.names = FALSE, quote=FALSE)
set.seed(17)
lafd.2 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/lafd_2", sep = "\t")
train <- sample(c(1:1099343), 1099343*.70, replace = FALSE)
lafd.2.train <- lafd.2[train,c(2:6)]
lafd.2.test <- lafd.2[-train,c(2:6)]
lafd.2.train[,1] <- as.character(lafd.2.train[,1])
lafd.2.test[,1] <- as.character(lafd.2.test[,1])
rfz.2 <- randomForest(elapsed_time~., data = lafd.2.train, ntree=22)
rfz.2.pred <- predict(rfz.2, lafd.2.test)
rfz.2.mse <- mean((rfz.2.pred - lafd.2.test$elapsed_time)^2)
rfz.2.mse
kag.2 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kag_2", sep = "\t")
kag.2[,2] <- as.character(kag.2[,2])
kag.2.pred <- predict(rfz.2, kag.2)
prediction <- kag.2.pred
row.id <- kag.2[,1]
avg_na_free <- mean(na.omit(prediction))
for(i in 1:length(prediction)){
if(is.na(prediction[i])){
prediction[i] = avg_na_free
}
}
kaggle_submit_10z_2 <- cbind(row.id,prediction)
write.csv(kaggle_submit_10z_2, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod_10z_2.txt", row.names = FALSE, quote=FALSE)
(1052410^2 + 1355601^2)^(1/2)
(0.5*(1052410^2 + 1355601^2))^(1/2)
(0.75*(1052410^2 + 1355601^2))^(1/2)
(0.75*(962410^2 + 1022410^2))^(1/2)
mesh_pred <- c(rfz.1.pred, rfz.2.pred)
mesh_test <- c(lafd.1.test$elapsed_time, lafd.2.test$elapsed_time)
mesh_mse <- mean((mesh_pred - mesh_test)^2)
mesh_mse
lafd.2k.train <- lafd.2.train[,-1]
lafd.2k.test <- lafd.2.test[,-1]
rfz.2k <- randomForest(elapsed_time~., data = lafd.2k.train, ntree=23)
rfz.2k.pred <- predict(rfz.2k, lafd.2k.test)
rfz.2k.mse <- mean((rfz.2k.pred - lafd.2k.test$elapsed_time)^2)
rfz.2k.mse
full_pred <- c(rfz.1k.pred, rfz.2k.pred)
full_test <- c(lafd.1k.test$elapsed_time, lafd.2k.test$elapsed_time)
full_mse <- mean((full_pred - full_test)^2)
full_mse
1211626/1269994
1417289.56585*0.9540407
lafd_clean_x <- na.omit(lafd[complete.cases(lafd[,c(1,4,6,7,10,11)]),])
length(lafd_clean_x$elapsed_time)
names(lafd_clean_x)
length(lafd$elapsed_time)
lafd_clean_x <- na.omit(lafd[complete.cases(lafd[,c(1,4,6,7,10,11)]),])
lafd_clean_x[,10] <- as.numeric(lafd_clean_x[,10])
write.csv(lafd_clean, file="/Users/z/Desktop/stats_101c/kaggle/lafd_clean_x", row.names = FALSE, quote=FALSE)
write.csv(lafd_clean_x, file="/Users/z/Desktop/stats_101c/kaggle/lafd_clean_x", row.names = FALSE, quote=FALSE)
set.seed(17)
lafd.1 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/lafd_1", sep = "\t")
length(lafd.1$elapsed_time)
library(randomForest)
set.seed(17)
lafd.1 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/lafd_1", sep = "\t")
train <- sample(c(1:1215717), 1215717*.71, replace = FALSE)
lafd.1.train <- lafd.1[train,c(2:6)]
lafd.1.test <- lafd.1[-train,c(2:6)]
lafd.1.train[,1] <- as.character(lafd.1.train[,1])
lafd.1.test[,1] <- as.character(lafd.1.test[,1])
rfz.1 <- randomForest(elapsed_time~., data = lafd.1.train, ntree=63)
rfz.1.pred <- predict(rfz.1, lafd.1.test)
rfz.1.mse <- mean((rfz.1.pred - lafd.1.test$elapsed_time)^2)
rfz.1.mse
kag.1 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kag_1", sep = "\t")
kag.1[,2] <- as.character(kag.1[,2])
kag.1.pred <- predict(rfz.1, kag.1)
prediction <- kag.1.pred
row.id <- kag.1[,1]
avg_na_free <- mean(na.omit(prediction))
for(i in 1:length(prediction)){
if(is.na(prediction[i])){
prediction[i] = avg_na_free
}
}
kaggle_submit_11z_1 <- cbind(row.id,prediction)
write.csv(kaggle_submit_11z_1, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod_11z_1.txt", row.names = FALSE, quote=FALSE)
kag2 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kaggle_mod_11z_c.csv")
?sort
blah1 <- c(1:8)
blah2 <- c(1, 4, 6, 2, 3, 5, 7, 9)
blahc <- rbind(blah1, blah2)
blahc
blahc[,sort(blahc[2,])]
blahc[,sort.list(blahc[2,])]
kag2.sort <- head(kag2[sort.list(kag2[,1])],10)
kag2.sort <- head(kag2[sort.list(kag2[,1]),],10)
kag2.sort
kag1 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4c.csv")
kag1.sort <- head(kag1[sort.list(kag1[,1]),],10)
kag1.sort
prediction <- (4*kag1$prediction + kag2$prediction)/5
prediction <- (4*kag1.sort$prediction + kag2.sort$prediction)/5
row.id <- kag1.sort$row.id
kag_zc <- cbind(row.id,prediction)
write.csv(kag_zc, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod_zc.txt", row.names = FALSE, quote=FALSE)
kag2.sort <- kag2[sort.list(kag2[,1]),]
kag1.sort <- kag1[sort.list(kag1[,1]),]
prediction <- (4*kag1.sort$prediction + kag2.sort$prediction)/5
row.id <- kag1.sort$row.id
kag_zc <- cbind(row.id,prediction)
head(kag_zc, 25)
write.csv(kag_zc, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod_zc.txt", row.names = FALSE, quote=FALSE)
kag_zzz1 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4a.csv")
kag_zzz2 <- read.csv("/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4a.csv")
prediction <- (4*kag_zzz2$prediction + 3*kag_zzz1$prediction)/7
row.id <- kag_zzz2$row.id
kag_zzz <- cbind(row.id,prediction)
write.csv(kag_zzz, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod_zzz.txt", row.names = FALSE, quote=FALSE)
set.seed(117)
train <- sample(c(1:2774370), 2774370*.69, replace = FALSE)
lafd <- read.csv("/Users/z/Desktop/stats_101c/kaggle/lafdtraining.csv")
lafd_rf_train <- data.frame(na.omit(lafd[complete.cases(lafd[train,]),c(6,7,11)]))
lafd_rf_test <- data.frame(na.omit(lafd[complete.cases(lafd[-train,]),c(6,7,11)]))
rf.4 <- randomForest(elapsed_time~., data = lafd_rf_train, ntree=90)
rf_pred4 <- predict(rf.4, lafd_rf_test)
rf_mse4 <- mean((rf_pred4 - lafd_rf_test$elapsed_time)^2)
rf_mse4
kaggle_data <- read.csv("/Users/z/Desktop/stats_101c/kaggle/testing.without.response")
kaggle_pred <- predict(rf.4, kaggle_data)
row.id <- kaggle_data$row.id
prediction <- kaggle_pred
avg_na_free <- mean(na.omit(prediction))
for(i in 1:length(prediction)){
if(is.na(prediction[i])){
prediction[i] = avg_na_free
}
}
kaggle_submit4a <- cbind(row.id,prediction)
write.csv(kaggle_submit4a, file = "/Users/z/Desktop/stats_101c/kaggle/kaggle_mod4z_a.txt", row.names = FALSE, quote=FALSE)
