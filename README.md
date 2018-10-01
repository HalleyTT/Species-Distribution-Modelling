# Species-Distribution-Modelling
Glm
m1 = glm(pb ~., data=training) #base package
 
Using caret package 
mod_fit=train(pb~.,data=training,trControl=train_control,method="glm",family="binomial")
 
importance of the different predictors
glm variable importance



## test the model
p1=predict(mod_fit, newdata=testing) #predict on the test data




#test model fit-auc
 

build an SDM
## read in all predictors-since they are all significant
#use predict to implement the GLM model stored in mod_fit on the raster stack of our predictors
 
use the basic GLM to predict implement the GLM model stored in mod_fit on the raster stack of our predictors
