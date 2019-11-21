library(dplyr)
  

library(rattle)
library(RColorBrewer)
library(ids) # random id generator

set.seed(123)

#turn all character variables into factors
autism_data2 <- autism_data %>% 
  select(Age, Gender, Ethnicity, Family, Used_app_before, App_result, Age_Group, Relation, ASD) %>%
  mutate(Age = as.numeric(Age),
         Gender = factor(Gender),
         Ethnicity = factor(Ethnicity),
         Family = factor(Family),
         Used_app_before = factor(Used_app_before),
         Relation = factor(Relation),
         ASD = factor(ASD))

#quick look at the data
head(autism_data2)

#Split data into training and test 70-30 split
train <- sample(nrow(autism_data2), 0.7*nrow(autism_data2), replace = FALSE)
TrainSet <- autism_data2[train,]
TestSet <- autism_data2[-train,]
summary(TrainSet)
summary(TestSet)

fit <- rpart(ASD ~ Age+ Gender+ Ethnicity+ Family+ Used_app_before+ App_result+ Age_Group+ Relation,
data = TrainSet,
method = "class")
#this formula showed that the decision tree only mapped the app_result for yes over 6.5
#and no for less than 6.5
#If i had more time I would further explore this variable


#removed app results to fit
fit <- rpart(ASD ~ Age+ Gender+ Ethnicity+ Family+ Used_app_before+ Age_Group+ Relation,
             data = TrainSet,
             method = "class")

fancyRpartPlot(fit)


#added controls to the fit
tree_fit <- rpart(ASD ~ Age+ Gender+ Ethnicity+ Family+ Used_app_before+ Age_Group+ Relation,
                        data = TrainSet,
                        method = "class",
                  control = rpart.control(minsplit = 25, cp = 0.01))

fancyRpartPlot(tree_fit)



#refined controls by manipulating minsplit
tree_fit2 <- rpart(ASD ~ Age+ Gender+ Ethnicity+ Family+ Used_app_before+ Age_Group+ Relation,
                  data = TrainSet,
                  method = "class",
                  control = rpart.control(minsplit = 30, cp = 0.01))

fancyRpartPlot(tree_fit2)


#looked at conditional partitioning
tree_fit3 <- ctree(ASD ~ Age+ Gender+ Ethnicity+ Family+ Used_app_before+ Age_Group+ Relation,
data = TrainSet)


#tree ROC

tree_roc <- tree_fit3 %>%
  predict(newdata = TestSet) %>%
  prediction(TestSet$ASD) %>%
  performance("tpr","fpr")



#did not work 
tree_roc <- tree_fit2 %>%
  predict(newdata = TestSet) %>%
  prediction(TestSet$ASD) %>%
  performance("tpr","fpr")

#test the model and look at the AUC. the closer to one the stronger
TestSet$predict.score <- predict(tree_fit2, TestSet)[,2]
pred <- prediction(TestSet$predict.score, TestSet$ASD)
pfm <- performance(pred, "tpr", "fpr")
auc <- performance(pred, "auc")

plot(pfm)



#here is how far i've gotten (3 hours in-finished exploratory analysis and started decision tree)





#this is an ability to interact with the decision tree 
new.fit <- prp(tree_fit,snip=TRUE)$obj
fancyRpartPlot(new_fit)

