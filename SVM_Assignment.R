##### Salary Data #####

salary_train <- read.csv(file.choose())
salary_test <- read.csv(file.choose())
sum(is.na(salary_train))
sum(is.na(salary_test))
summary(salary_train)
summary(salary_test)
str(salary_train)


### Discretizing - Training data ###

salary_train_n <- names(which(sapply(salary_train[,-14], is.factor)))

#install.packages("CatEncoders")
library(CatEncoders)

for (i in salary_train_n){
  encode <- LabelEncoder.fit(salary_train[,i])
  salary_train[,i] <- transform(encode,salary_train[,i])
}

str(salary_train)

### Discretizing - Training data ###

salary_test_n <- names(which(sapply(salary_test[,-14], is.factor)))

for (i in salary_test_n){
  encode <- LabelEncoder.fit(salary_test[,i])
  salary_test[,i] <- transform(encode,salary_test[,i])
}

str(salary_train)


### Normalising - Training data ###

norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}

salary_train_n <- as.data.frame(lapply(salary_train[1:13], norm))
summary(salary_train_n)
salary_train_n <- cbind(salary_train_n,salary_train$Salary)
colnames(salary_train_n)[14] <- "Salary"

### Normalising - Testing data ###

salary_test_n <- as.data.frame(lapply(salary_test[1:13], norm))
summary(salary_test_n)
salary_test_n <- cbind(salary_test_n,salary_test$Salary)
colnames(salary_test_n)[14] <- "Salary"


### Model Building ###

library(kernlab)
library(caret)

# Vanilla
model_vanilla <- ksvm(salary_train_n$Salary~.,data = salary_train_n,kernel = "vanilladot")
pred_model_vanilla <- predict(model_vanilla,newdata=salary_test_n[,-14])
mean(pred_model_vanilla==salary_test_n$Salary)
table(pred_model_vanilla,salary_test_n$Salary)

#Rbfdot
model_rbfdot <- ksvm(salary_train_n$Salary~.,data = salary_train_n,kernel="rbfdot")
pred_model_rbf <- predict(model_rbfdot,newdata=salary_test_n[,-14])
mean(pred_model_rbf==salary_test_n$Salary)

#polydot
model_polydot <- ksvm(salary_train_n$Salary~.,data = salary_train_n,kernel ="polydot")
pred_model_polydot <- predict(model_polydot,newdata=salary_train_n[,-14])
mean(pred_model_polydot==salary_test_n$Salary)



##### Forest Fires #####

fire_forest <- read.csv(file.choose())
ff <- fire_forest[,-c(1:2)]
summary(ff)
str(ff)

# Normalize

ff_norm <- as.data.frame(lapply(ff[,c(1:9)],FUN=norm))
ff_norm <- cbind(ff_norm,ff[,c(10:29)])

# Splitting

library(caTools)

split <- sample.split(ff_norm, SplitRatio = 0.7)
ff_train <- subset(ff_norm, split ==TRUE)
ff_test  <- subset(ff_norm, split == FALSE)
table(ff_train$size_category)
table(ff_test$size_category)

# Model building

# Vanilla
model_vanilla_ff <- ksvm(ff_train$size_category~.,data=ff_train,kernel="vanilladot")
pred_vanilla_ff <- predict(model_vanilla_ff,ff_test[,-29])
mean(pred_vanilla_ff==ff_test$size_category) #88.19

#Rbfdot
model_rbfdot_ff <- ksvm(ff_train$size_category~.,data=ff_train,kernel="rbfdot")
pred_rbf_ff <- predict(model_rbfdot_ff,ff_test[,-29])
mean(pred_rbf_ff==ff_test$size_category) #78.26

#Polydot
model_polydot_ff <- ksvm(ff_train$size_category~.,data=ff_train,kernel="polydot")
pred_polydot_ff <- predict(model_polydot_ff,ff_test[,-29])
mean(pred_polydot_ff==ff_test$size_category) #88.19