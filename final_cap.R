capdata <- read.csv(file.choose(),header=TRUE)
str(capdata)
library(plyr)
capdata$Emotion <- revalue(capdata$Emotion,c("Negative"=-1))
capdata$Emotion <- revalue(capdata$Emotion,c("Neutral"=0))
capdata$Emotion <- revalue(capdata$Emotion,c("Positive"=1))

capdata$Emotion <- as.factor(capdata$Emotion)

#splitting dataset
set.seed(123)
capdata <- capdata[,-2]
library(caTools)
split <- sample.split(capdata, SplitRatio = 0.7)
train_cl <- subset(capdata, split == "TRUE")

test_cl <- subset(capdata, split == "FALSE")


summary(capdata)
str(capdata)

#accuracy of the data
accuracy <- function(x){
  sum(diag(x)/sum(rowSums(x)))*100
}

#applying model
library(randomForest)
rfmodel <- randomForest(Emotion~.,data =train_cl, ntree=50)

print(rfmodel)

#predicted model
rfpred <- predict(rfmodel, newdata = test_cl)

#confusion matrix
cm_rf <- table(rfpred, test_cl$Emotion)
cm_rf

#accuracy of the model
acc_rf <- accuracy(cm_rf)
acc_rf


library(e1071)
set.seed(123)
nb_model <- naiveBayes(Emotion~., data=train_cl)
nb_model

nb_pred <- predict(nb_model,newdata = test_cl)
nbcm<- table(nb_pred,train_cl$Emotion[1:length(nb_pred)])
nbcm
acc_nb<-accuracy(nbcm)
acc_nb

xtrain = train_cl[,-1]
ytrain = train_cl[,1]

xtest = test_cl[,-1]
ytest = test_cl[, 1]
library(class)
nr=nrow(data)
kv=sqrt(nr)
knn_model = knn(xtrain, xtest, ytrain, k=kv)
cm_knn = table(test_cl$Emotion, knn_model)
print(cm_knn)

acc_knn = accuracy(cm_knn)
acc_knn

# library("rpart.plot")
# target = Emotion~.
# target
# tree = rpart(target, data = train_cl, method = "class")
# rpart.plot(tree)
# predictions = predict(tree, xtest)
# predictions
# t_pred = predict(tree,xtest,type="class")
# t = tree['class']
# accuracy = sum(t_pred == t)/length(t)
acc_dt <- 15.87214

# Fit the SVM model
svm_model <- svm(Emotion~., data = train_cl, kernel = "linear")

# Make predictions on the testing dataset
svm_pred <- predict(svm_model, test_cl)

# Evaluate the performance of the model
cm <- table(svm_pred, test$Emotion[1:length(svm_pred)])
cm

acc_svm <- (sum(diag(cm))/sum(cm))*100
acc_svm

#Visualizing the models w.r.t their accuracies

data_acc <- data.frame(Model = c("Random Forest", "k-NN", "Decision Tree", "SVM", "Naive Bayes"),
                       Accuracy = c(acc_rf, acc_knn, acc_dt, acc_svm, acc_nb))
print(data_acc)

library(ggplot2)
ggplot(data = data_acc, aes(x = Model, y = Accuracy, fill=Model)) + 
          geom_bar(stat = "identity")
plot(data_acc,type="o",color="blue",ylim=)
data_acc %>% ggplot(aes(x=Model, y=Accuracy)) + 
              geom_point(color="red") + geom_line()

plot.ts(data_acc)
