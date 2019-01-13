library(DMwR)
library(xgboost)
library(Matrix)
data(algae)
algae=algae[-manyNAs(algae),]
clean.algae=knnImputation(algae,k=10)
clean.algae_num=clean.algae[,4:11]

seosan_one <- model.matrix(~season-1,clean.algae)
size_one <- model.matrix(~size-1,clean.algae)
speed_one <- model.matrix(~speed-1,clean.algae)
clean.algae_num=cbind(clean.algae_num,seosan_one,size_one,speed_one)
clean.algae_matrix=data.matrix(clean.algae_num)
label_a1=clean.algae$a1

numberOfTrainingSamples <- round(length(label_a1) * .75)
train_data <- clean.algae_matrix[1:numberOfTrainingSamples,]
train_labels <- label_a1[1:numberOfTrainingSamples]
test_data <- clean.algae_matrix[-(1:numberOfTrainingSamples),]
test_labels <- label_a1[-(1:numberOfTrainingSamples)]

dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

model <- xgboost(data = dtrain,    
                 nround = 3,
                 max.depth = 2,
                 lambda=0.01,
                 feature_selector="shuffle",
                 objective = "reg:linear")  
pred <- predict(model, dtest)
mse=mean(((test_labels-pred)^2))
mean_mse=mean(((test_labels-mean(train_labels))^2))
nmse=(sum((test_labels-pred)^2))/(sum((test_labels-mean(train_labels))^2))
print(nmse)#nround = 3,
          #max.depth = 2,
          #lambda=0.01
          #0.436559

xgb.plot.tree(model = model)
xgb.plot.multi.trees(feature_names = names(clean.algae_matrix), 
                     model = model)

importance_matrix <- xgb.importance(names(clean.algae_matrix), model = model)
xgb.plot.importance(importance_matrix)



################################################
test.algae_num=test.algae[,4:11]
seosan_one <- model.matrix(~season-1,test.algae)
size_one <- model.matrix(~size-1,test.algae)
speed_one <- model.matrix(~speed-1,test.algae)
test.algae_num=cbind(test.algae_num,seosan_one,size_one,speed_one)
test.algae_matrix=data.matrix(test.algae_num)
pred2 <- predict(model,test.algae_matrix)

label=read.table('Sols.txt')
r1=label[1]
print(sum((r1-pred2)^2))
print(sum((r1-mean(clean.algae$a1))^2))
####################################################





