# STT-481-Carson-Ashton-Ben
STT 481 Final Project Kaggle

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r, warning=FALSE}
library(leaps)
library(glmnet)
library(class)
library(DMwR)
library(pls)
library(boot)
library(plyr)
library(FNN)
library(gam)
library(tree)
library(randomForest)
library(gbm)
```


###Data Cleaning

```{r}
#Read in test and train data, accounting for headers and NA values
train <- read.csv("train.csv", header = TRUE, na.strings = "NA")
test <- read.csv("test.csv", header = TRUE, na.strings = "NA")

#How many NAs and a look at Lot Frontage NAs
colSums(is.na(train))
min(train$LotFrontage, na.rm = T)
```

```{r}
#Remove columns with large numbers of NA and utilities which has no variance in test data

#Lot Frontage NA to 0.
train$LotFrontage[is.na(train$LotFrontage)==TRUE] <- 0

#Fix columns where NA should be None
levels(train$Alley) <- c("Grvl", "Pave", "None")
train$Alley[is.na(train$Alley)==TRUE] <- "None" 

levels(train$FireplaceQu) <- c("Ex", "Fa", "Gd", "Po", "TA","None")
train$FireplaceQu[is.na(train$FireplaceQu)==TRUE] <- "None"

levels(train$BsmtQual) <- c(levels(train$BsmtQual), "None")
train$BsmtQual[is.na(train$BsmtQual)==TRUE] <- "None"

levels(train$BsmtCond) <- c(levels(train$BsmtCond), "None")
train$BsmtCond[is.na(train$BsmtCond)==TRUE] <- "None"

levels(train$BsmtExposure) <- c(levels(train$BsmtExposure), "None")
train$BsmtExposure[is.na(train$BsmtExposure)==TRUE] <- "None"

levels(train$BsmtFinType1) <- c(levels(train$BsmtFinType1), "None")
train$BsmtFinType1[is.na(train$BsmtFinType1)==TRUE] <- "None"

levels(train$BsmtFinType2) <- c(levels(train$BsmtFinType2), "None")
train$BsmtFinType2[is.na(train$BsmtFinType2)==TRUE] <- "None"

levels(train$GarageType) <- c(levels(train$GarageType), "None")
train$GarageType[is.na(train$GarageType)==TRUE] <- "None"

levels(train$GarageFinish) <- c(levels(train$GarageFinish), "None")
train$GarageFinish[is.na(train$GarageFinish)==TRUE] <- "None"

levels(train$BsmtQual) <- c(levels(train$BsmtQual), "None")
train$BsmtQual[is.na(train$BsmtQual)==TRUE] <- "None"

levels(train$GarageQual) <- c(levels(train$GarageQual), "None")
train$GarageQual[is.na(train$GarageQual)==TRUE] <- "None"

levels(train$GarageCond) <- c(levels(train$GarageCond), "None")
train$GarageCond[is.na(train$GarageCond)==TRUE] <- "None"

levels(train$PoolQC) <- c(levels(train$PoolQC), "None")
train$PoolQC[is.na(train$PoolQC)==TRUE] <- "None"

levels(train$Fence) <- c(levels(train$Fence), "None")
train$Fence[is.na(train$Fence)==TRUE] <- "None"

levels(train$MiscFeature) <- c(levels(train$MiscFeature), "None")
train$MiscFeature[is.na(train$MiscFeature)==TRUE] <- "None"

#How many NAs left
colSums(is.na(train))

#IRemove ID, Utilities, linearly dependent columns, save IDs to include in submission
test.ids <- test$Id
train <- train[,-c(1,10,39,47)]
test <- test[,-c(1,10,39,47)]

#Save colnames of df
df.names <- colnames(train)

#KNN imputation for rest of NAs. Fix column names
train <- data.frame(knnImputation(train[,1:76]),train[,77])
colnames(train) <- df.names
```

```{r}
#Fix NAs in test data

#Lot Frontage NA to 0.
test$LotFrontage[is.na(test$LotFrontage)==TRUE] <- 0

#Fix columns where NA should be None
levels(test$Alley) <- c(levels(test$Alley), "None")
test$Alley[is.na(test$Alley)==TRUE] <- "None" 

levels(test$FireplaceQu) <- c(levels(test$FireplaceQu), "None")
test$FireplaceQu[is.na(test$FireplaceQu)==TRUE] <- "None"

levels(test$BsmtQual) <- c(levels(test$BsmtQual), "None")
test$BsmtQual[is.na(test$BsmtQual)==TRUE] <- "None"

levels(test$BsmtCond) <- c(levels(test$BsmtCond), "None")
test$BsmtCond[is.na(test$BsmtCond)==TRUE] <- "None"

levels(test$BsmtExposure) <- c(levels(test$BsmtExposure), "None")
test$BsmtExposure[is.na(test$BsmtExposure)==TRUE] <- "None"

levels(test$BsmtFinType1) <- c(levels(test$BsmtFinType1), "None")
test$BsmtFinType1[is.na(test$BsmtFinType1)==TRUE] <- "None"

levels(test$BsmtFinType2) <- c(levels(test$BsmtFinType2), "None")
test$BsmtFinType2[is.na(test$BsmtFinType2)==TRUE] <- "None"

levels(test$GarageType) <- c(levels(test$GarageType), "None")
test$GarageType[is.na(test$GarageType)==TRUE] <- "None"

levels(test$GarageFinish) <- c(levels(test$GarageFinish), "None")
test$GarageFinish[is.na(test$GarageFinish)==TRUE] <- "None"

levels(test$BsmtQual) <- c(levels(test$BsmtQual), "None")
test$BsmtQual[is.na(test$BsmtQual)==TRUE] <- "None"

levels(test$GarageQual) <- c(levels(test$GarageQual), "None")
test$GarageQual[is.na(test$GarageQual)==TRUE] <- "None"

levels(test$GarageCond) <- c(levels(test$GarageCond), "None")
test$GarageCond[is.na(test$GarageCond)==TRUE] <- "None"

levels(test$PoolQC) <- c(levels(test$PoolQC), "None")
test$PoolQC[is.na(test$PoolQC)==TRUE] <- "None"

levels(test$Fence) <- c(levels(test$Fence), "None")
test$Fence[is.na(test$Fence)==TRUE] <- "None"

levels(test$MiscFeature) <- c(levels(test$MiscFeature), "None")
test$MiscFeature[is.na(test$MiscFeature)==TRUE] <- "None"

#How many NAs left
colSums(is.na(test))
```

```{r}
#Use Knn imputation with k = 10 to estimate missing values. Initialize SalePrice
knnOutput <- knnImputation(test)
knnOutput$SalePrice <- rep(0,1459)
test <- knnOutput
```

```{r}
#Put all into one df
all.df <- as.data.frame(matrix(nrow = 2919, ncol = 77))
all.df[1:1460,] <- train
all.df[1461:2919,] <- test
colnames(all.df) <- df.names
```

```{r}
#Log transform Sale Price
all.df$SalePrice[1:1460] <- log(all.df$SalePrice[1:1460])
```

###Linear Regression

```{r}
#Linear model without removing outliers or high leverage points
lin.model <- lm(SalePrice~., data = all.df[1:1460,])
lmpreds <- predict(lin.model, newdata = all.df[1461:2919,])
e <- exp(1)
lmpreds <- e^lmpreds

lm.sub.submit <- data.frame(test.ids, lmpreds)
colnames(lm.sub.submit) <- c("Id", "SalePrice")
#write.csv(lm.sub.submit, "lm.csv")
```

```{r}
#Check for and remove high leverage and outliers
plot(lin.model)
all.df.nolvg <- all.df[-c(1424, 1299, 524),]

#No leverage or outliers model
lin.model.nolvg <- lm(SalePrice~., data = all.df.nolvg[1:1457,])
lmpreds <- predict(lin.model.nolvg, newdata = all.df.nolvg[1458:2916,])
lmpreds <- e^lmpreds

lm.sub.submit <- data.frame(test.ids, lmpreds)
colnames(lm.sub.submit) <- c("Id", "SalePrice")
#write.csv(lm.sub.submit, "lm.csv.nolvg.csv")
```

###Subset Selection 

```{r}
predict.regsubsets <- function(object, newdata , id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  return(mat[,xvars] %*% coefi)
}
```

```{r}
#Forward subset selection with no outliers or leverage pts
forw.sub <- regsubsets(SalePrice~., data = all.df.nolvg[1:1457,], nvmax = 77, method = "forward")
forw.sub.sum <- summary(forw.sub)
#forw.sub.sum
plot(forw.sub.sum$cp, type = "b", ylab = "cp")
plot(forw.sub.sum$bic, type = "b", ylab = "bic")
plot(forw.sub.sum$adjr2, type = "b", ylab = "adjr2")

which.min(forw.sub.sum$bic)
coef(forw.sub, 26)
forw.sub.preds <- predict.regsubsets(forw.sub, newdata = all.df.nolvg[1458:2916,], id = 26)
forw.sub.preds <- e^forw.sub.preds
forw.sub.submit <- data.frame(test.ids, forw.sub.preds)
colnames(forw.sub.submit) <- c("Id", "SalePrice")
#write.csv(forw.sub.submit, "Forward.Subset.nolvg.csv")
```

```{r}
#Backward Subset Selection without outliers/leverage
back.sub <- regsubsets(SalePrice~., data = all.df.nolvg[1:1457,], nvmax = 77, method = "backward")
back.sub.sum <- summary(back.sub)
#back.sub.sum
plot(back.sub.sum$cp, type = "b", ylab = "cp")
plot(back.sub.sum$bic, type = "b", ylab = "bic")
plot(back.sub.sum$adjr2, type = "b", ylab = "adjr2")

which.min(back.sub.sum$bic)
coef(back.sub, 27)
back.sub.preds <- predict.regsubsets(back.sub, newdata = all.df.nolvg[1458:2916,], id = 27)
back.sub.preds <- e^back.sub.preds
back.sub.submit <- data.frame(test.ids, back.sub.preds)
colnames(back.sub.submit) <- c("Id", "SalePrice")
#write.csv(back.sub.submit, "Backward.Subset.nolvg.csv")
```

```{r}
#Mixed subsets with no outliers
mix.sub <- regsubsets(SalePrice~., data = all.df.nolvg[1:1457,], nvmax = 40, method = "seqrep")
mix.sub.sum <- summary(mix.sub)
plot(mix.sub.sum$cp, type = "b", ylab = "cp")
plot(mix.sub.sum$bic, type = "b", ylab = "bic")
plot(mix.sub.sum$adjr2, type = "b", ylab = "adjr2")

which.min(mix.sub.sum$bic)
coef(mix.sub, 27)
mix.sub.preds <- predict.regsubsets(mix.sub, newdata = all.df.nolvg[1458:2916,], id = 27)
mix.sub.preds <-e^mix.sub.preds
mix.sub.submit <- data.frame(test.ids, mix.sub.preds)
colnames(mix.sub.submit) <- c("Id", "SalePrice")
#write.csv(mix.sub.submit, "Mixed.Subset.nolvg.csv")
```

###Shrinkage Methods

```{r}
#Lasso Method without outliers
mod.mat <- model.matrix(SalePrice~., data = all.df.nolvg)[,-1]

lasso.model <- glmnet(mod.mat[1:1457,],all.df.nolvg$SalePrice[1:1457], alpha = 1)

cv.out <- cv.glmnet(mod.mat[1:1457,],all.df.nolvg$SalePrice[1:1457], alpha = 1, nfolds = 10)
cv.out$lambda.min
plot(cv.out)

lasso.pred <- predict(lasso.model, s = cv.out$lambda.min, newx = mod.mat[1458:2916,])
lasso.pred <- e^lasso.pred

lasso.pred.coef <- predict(lasso.model, s = cv.out$lambda.min, newx = mod.mat[1458:2916,], type = "coefficient")
lasso.pred.coef

lasso.sub.submit <- data.frame(test.ids, lasso.pred)
colnames(lasso.sub.submit) <- c("Id", "SalePrice")
#write.csv(lasso.sub.submit, "Lasso.nolvg.csv")
```

```{r}
#Ridge Regression with no lvg
mod.mat <- model.matrix(SalePrice~., data = all.df.nolvg)[,-1]

ridge.model <- glmnet(mod.mat[1:1457,],all.df.nolvg$SalePrice[1:1457], alpha = 0)

cv.out <- cv.glmnet(mod.mat[1:1457,],all.df.nolvg$SalePrice[1:1457], alpha = 0, nfolds = 10)
cv.out$lambda.min
plot(cv.out)

ridge.pred <- predict(ridge.model, s = cv.out$lambda.min, newx = mod.mat[1458:2916,])
ridge.pred <- e^ridge.pred

ridge.pred.coef <- predict(ridge.model, s = cv.out$lambda.min, newx = mod.mat[1458:2916,], type = "coefficient")
ridge.pred.coef

ridge.sub.submit <- data.frame(test.ids, ridge.pred)
colnames(ridge.sub.submit) <- c("Id", "SalePrice")
#write.csv(ridge.sub.submit, "Ridge.nolvg.csv")
```

###Dimension Reduction Methods

```{r}
#PCR without lvg
mod.mat <- model.matrix(SalePrice~., data = all.df.nolvg)[,-1]
pcr.fit <- pcr(SalePrice~., data = all.df.nolvg[1:1457,], scale = TRUE, validation = "CV")

pcr.cv.rmse <- RMSEP(pcr.fit)
pcr.preds <- predict(pcr.fit, newdata = mod.mat[1458:2916,], ncomp = which.min(pcr.cv.rmse$val[1,1,-1]))
pcr.preds <- e^pcr.preds

pcr.sub.submit <- data.frame(test.ids, pcr.preds)
colnames(pcr.sub.submit) <- c("Id", "SalePrice")
#write.csv(pcr.sub.submit, "pcr.nolvg.csv")
```


```{r}
#PLS without lvg
mod.mat <- model.matrix(SalePrice~., data = all.df.nolvg)[,-1]
pls.fit <- plsr(SalePrice~., data = all.df.nolvg[1:1457,], scale = TRUE, validation = "CV")

pls.cv.rmse <- RMSEP(pls.fit)
pls.preds <- predict(pls.fit, newdata = mod.mat[1458:2916,], ncomp = which.min(pls.cv.rmse$val[1,1,-1]))
pls.preds <- e^pls.preds

pls.sub.submit <- data.frame(test.ids, pls.preds)
colnames(pls.sub.submit) <- c("Id", "SalePrice")
#write.csv(pls.sub.submit, "pls.nolvg.csv")
```


```{r}
#AVG PCR and PLS without lvg
avg.preds <- (pls.preds + pcr.preds)/2
avg.sub.submit <- data.frame(test.ids, avg.preds)
colnames(avg.sub.submit) <- c("Id", "SalePrice")
#write.csv(avg.sub.submit, "avg.nolvg.csv")
```


###KNN Method

```{r}
knn.cv.errs <- rep(0,45)
for(k in 1:45){
  knn.pred.cv <- knn.reg(train=all.df[1:1460,1:76], y=all.df[1:1460,77], k=k)
  knn.cv.errs[k] <- knn.pred.cv$R2Pred
}
knn.cv.errs
which.max(knn.cv.errs)
```


```{r}
#Predict and export
knn.pred <- knn.reg(all.df[1:1460,1:76], all.df[1461:2919,1:76], all.df[1:1460,77], k=7)
knn.preds <- e^knn.pred$pred
knn.sub.submit <- data.frame(test.ids, knn.preds)
colnames(knn.sub.submit) <- c("Id", "SalePrice")
#write.csv(knn.sub.submit, "knn2.csv")
```

###Nonlinear Regression

```{r}
#Mixed subset natural splines
coef(mix.sub, 27)
gam.lm <- lm(SalePrice~bs(MSZoning)+bs(LotArea)+Street+bs(BldgType)+OverallQual+bs(OverallCond)+bs(YearBuilt)+bs(YearRemodAdd)+bs(ExterCond)+bs(Foundation)+bs(BsmtQual)+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+bs(HeatingQC)+CentralAir+X1stFlrSF+X2ndFlrSF+bs(BsmtFullBath)+bs(KitchenAbvGr)+bs(KitchenQual)+bs(Functional)+bs(FireplaceQu)+GarageCars+PavedDrive+ScreenPorch+bs(SaleCondition), data = all.df.nolvg[1:1457,])
#summary(gam.lm)
plot.Gam(gam.lm)
gam.preds <- newdata.predict.Gam(gam.lm, newdata = all.df.nolvg[1458:2916,])
gam.preds <- e^gam.preds
gam.submit <- data.frame(test.ids,gam.preds)
colnames(gam.submit) <- c("Id", "SalePrice")
#write.csv(gam.submit, "gam.csv")
```

```{r}
#Forward subset selection natural splines
coef(forw.sub, 26)
gam.lm <- lm(SalePrice~bs(MSZoning)+bs(LotArea)+Street+bs(BldgType)+OverallQual+bs(OverallCond)+bs(YearBuilt)+bs(YearRemodAdd)+bs(ExterCond)+bs(BsmtQual)+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+bs(HeatingQC)+CentralAir+X1stFlrSF+X2ndFlrSF+bs(BsmtFullBath)+bs(KitchenAbvGr)+bs(KitchenQual)+bs(Functional)+bs(FireplaceQu)+GarageCars+PavedDrive+ScreenPorch+bs(SaleCondition), data = all.df.nolvg[1:1457,])
#summary(gam.lm)
plot.Gam(gam.lm)
gam.preds <- newdata.predict.Gam(gam.lm, newdata = all.df.nolvg[1458:2916,])
gam.preds <- e^gam.preds
gam.submit <- data.frame(test.ids,gam.preds)
colnames(gam.submit) <- c("Id", "SalePrice")
#write.csv(gam.submit, "gam.forw1.csv")
```

```{r}
#Mixed subset natural splines
coef(back.sub, 27)
gam.lm <- lm(SalePrice~bs(MSZoning)+bs(LotArea)+Street+bs(BldgType)+OverallQual+bs(OverallCond)+bs(YearBuilt)+bs(YearRemodAdd)+bs(MasVnrType)+bs(ExterCond)+bs(BsmtQual)+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+bs(HeatingQC)+CentralAir+X1stFlrSF+X2ndFlrSF+bs(BsmtFullBath)+bs(KitchenAbvGr)+bs(KitchenQual)+bs(Functional)+bs(Fireplaces)+GarageCars+PavedDrive+ScreenPorch+bs(SaleCondition), data = all.df.nolvg[1:1457,])
#summary(gam.lm)
plot.Gam(gam.lm)
gam.preds <- newdata.predict.Gam(gam.lm, newdata = all.df.nolvg[1458:2916,])
gam.preds <- e^gam.preds
gam.submit <- data.frame(test.ids,gam.preds)
colnames(gam.submit) <- c("Id", "SalePrice")
#write.csv(gam.submit, "gam.back.csv")
```

###Tree models

```{r}
#Tree and pruned tree, no outliers
tree.fit <- tree(SalePrice~.,data=all.df.nolvg[1:1457,])

par(mfrow=c(1,2))
plot(tree.fit)
text(tree.fit, pretty=0)

cv.tree1 <- cv.tree(tree.fit, K = 10)
best.size <- cv.tree1$size[which.min(cv.tree1$dev)]
best.size

prune.tree1 <- prune.tree(tree.fit, best = best.size)
plot(prune.tree1)
text(prune.tree1, pretty=0)

tree.pred1 <- predict(prune.tree1, newdata = all.df.nolvg[1458:2916,-77])
tree.pred1 <- unname(tree.pred1)
tree.pred1 <- e^tree.pred1
tree.submit <- data.frame(test.ids,tree.pred1)
colnames(tree.submit) <- c("Id", "SalePrice")
#write.csv(tree.submit, "tree.csv")
```

```{r}
#Bagging, no outliers
bag.tree <- randomForest(SalePrice~., data = all.df.nolvg[1:1457,], mtry = 76, importance = TRUE, ntree = 500)

bag.preds <- predict(bag.tree, newdata = all.df.nolvg[1458:2916,-77])
bag.preds <- unname(bag.preds)
bag.preds <- e^bag.preds
bag.submit <- data.frame(test.ids,bag.preds)
colnames(bag.submit) <- c("Id", "SalePrice")
#write.csv(bag.submit, "bag.csv")
```

```{r}
#Random Forest, no outliers
rf.tree <- randomForest(SalePrice~., data = all.df.nolvg[1:1457,], mtry = round(sqrt(76)), importance = TRUE, ntree = 500)

rf.preds <- predict(rf.tree, newdata = all.df.nolvg[1458:2916,-77])
rf.preds <- unname(rf.preds)
rf.preds <- e^rf.preds
rf.submit <- data.frame(test.ids,rf.preds)
colnames(rf.submit) <- c("Id", "SalePrice")
#write.csv(rf.submit, "rf.csv")
```

```{r}
#Boosting, no outliers
gbm.tree <- gbm(SalePrice~., data = all.df.nolvg[1:1457,], distribution = "gaussian", shrinkage = .01, n.trees = 500, interaction.depth = 2)
boost.preds <- predict(gbm.tree, newdata = all.df.nolvg[1458:2916,-77], n.trees = 500)
boost.preds <- unname(boost.preds)
boost.preds <- e^boost.preds
boost.submit <- data.frame(test.ids,boost.preds)
colnames(boost.submit) <- c("Id", "SalePrice")
#write.csv(boost.submit, "boost.csv")
```
