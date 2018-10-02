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
mod_fit=train(pb~.,data=training,trControl=train_control,method="gbm")

summary(mod_fit)
## importance of the different predictors
varImp(mod_fit)

## test the model
p1=predict(mod_fit, newdata=testing) #predict on the test data

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

#raster predictors
require(raster)
datafiles = Sys.glob("*.tif") #Or whatever identifies your files

datafiles #list of predictors
stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
  tempraster = raster(datafiles[i])
  stck = stack(stck,tempraster)
}
p1= predict(stck,mod_fit)
plot(p1,main="GBM Predictive Map")


## USE THE WHOLE DATASET
pb=as.factor(pa$pb)# pa is the unsplit dataframe
mod_fit2=train(pb~.,data=pa,trControl=train_control,method="gbm")

summary(mod_fit2)


##raster
p2=predict(stck,mod_fit2) 
plot(p2,main="GBM Predictive all- Map")

#partial dependence plotss
library(gbm)
boost = gbm(pb~. , data=training,
            distribution = 'gaussian',
            n.trees=5000,
            interaction.depth = 4)
summary(boost)
plot(boost,i='slope')