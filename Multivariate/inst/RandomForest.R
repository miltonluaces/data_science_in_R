# IMPORTS ===========================================================================================================================================================================================

library(randomForest)


# Random Forest ===========================================================================================================================================================================================


# download Titanic Survivors data
data = read.table("http://math.ucdenver.edu/RTutorial/titanic.txt", h=T, sep="\t")
data[1:10,]
# make survived into a yes/no
data$Survived = as.factor(ifelse(data$Survived==1, "yes", "no")); data                 

# split into a training and test set
idx = runif(nrow(data)) <= 0.75
data.train = data[idx,]
data.test = data[-idx,]

# train a random forest
rf = randomForest(Survived ~ PClass + Age + Sex, data=data.train, importance=TRUE, na.action=na.omit)

# how important is each variable in the model
imp = importance(rf)
sImp = order(imp[,3], decreasing=T)
imp[sImp,]

datum = data.test[1,]; datum
pred = predict(rf, datum); pred

pred = predict(rf, data.test)
table(data.test$Survived, pred, dnn=list("actual", "predicted"))

