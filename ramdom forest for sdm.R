head(pa)
set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting

training = pa[ trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing

set.seed(825)

pb=as.factor(training$pb) #1 stands for presence and 0 for absence
land=as.factor(training$land) #land use categories are categorical


## caret
# define training control--> 10fold cv
train_control = trainControl(method="cv", number=10)

mod_fit=train(pb~.,data=training,trControl=train_control,method="rf", importance=TRUE)

summary(mod_fit)

## importance of the different predictors
varImp(mod_fit)

## test the model
p1=predict(mod_fit, newdata=testing) #predict on the test data

## otherways of acessing model accuracy

library(ModelMetrics)
auc(testing$pb,p1)
# confusion matrix
confusionMatrix(testing$pb,p1,cutoff = 0.5)
overall=(105+192)/(105+192+11+25)

#test model fit-auc
library(pROC)

roc.glmModel = pROC::roc(testing[,"pb"], p1) #compare testing data
#with predicted responses

auc= pROC::auc(roc.glmModel)
auc

plot(roc.glmModel)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))

p1 = predict(stck, mod_fit) #use predict to implement the MARS model stored
#in mod_fit on the raster stack of our predictors
plot(p1,main="random forest Predictive Map")

###test the impact of indicdual predictors
require(randomForest)
m1=randomForest(pb~.,data=training) # randomforest
partialPlot(m1,training,roughness1,pb)
#rf model, training data,x,y/response variable
p2= predict(stck,m1) # use preidct to implement the rf model stored
# in mod_fit on the raster stack of our predictors
plot(p2,main="RF Predictive Map")
m2=randomForest(pb~.,data=pa) # all data
p3 = predict(stck,m2)#
plot(p3,main="RF Predictive Map-all")














