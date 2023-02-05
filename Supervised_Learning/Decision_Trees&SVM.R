setwd ("C:/Users/Study/OneDrive/Desktop/DU/Statastical_Learning/Home_assignment_2")
getwd()
library(readxl)
install.packages("ISLR")
library(ISLR)
df <- read_excel("C:/Users/Study/OneDrive/Desktop/DU/Statastical_Learning/Home_assignment_2/Data_Cortex_Nuclear.xls")
View(df)
#1a
dim(df)
str(df)
df_2 <- df
#handling missing values
#replacing null values by mean for only numerical columns 

library(dplyr)
library(tidyr)
df_2 <- df %>%
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
sum(is.na(df_2)) == 0
is.na(df_2)

# columns Genotype, Treastment, Behavior, class are categorical values

df_2$Genotype <- as.factor(df_2$Genotype)
df_2$Treatment <- as.factor(df_2$Treatment)
df_2$Behavior <- as.factor(df_2$Behavior)
df_2$class <- as.factor(df_2$class)
str(df_2)

# mouse id is not required for any analysis. So lets drop it 

df_2 <- select (df_2,-c(MouseID))
typeof(df_2)
df_2 <- as.data.frame(df_2)
View(df_2)
summary(df_2)

#Part 1

set.seed(1000)
train <- sample(nrow(data), nrow(data)* 0.7)

#a)

library(tree)
tree.genotype <- tree(Genotype ~ . - Treatment - Behavior - class, df_2, 
                      subset = train)
summary(tree.genotype)
tree.genotype.pred <- predict(tree.genotype,
                              df_2[-train,],
                              type = "class")
with(df_2[-train,], mean(tree.genotype.pred != Genotype))


#Lets prune

cv.genotype <- cv.tree(tree.genotype,
                       FUN = prune.misclass)
cv.genotype
plot(cv.genotype)

# as per plot 12 seems reasonable
prune.genotype <- prune.misclass(tree.genotype,
                                 best = 12)
plot(prune.genotype); text(prune.genotype, pretty = 0)

#checking prediction with pruned tree
prune.genotype.pred <- predict(prune.genotype,
                               df_2[-train,],
                               type="class")
mean(prune.genotype.pred != df_2[-train,]$Genotype)


#treatment

library(tree)
tree.treatment <- update(tree.genotype, Treatment~.)

summary(tree.treatment)
tree.treatment.predict <- predict(tree.treatment,
                                  df_2[-train,],
                                  type = "class")
mean(tree.treatment.predict != df_2[-train,]$Treatment)


cv.treatment <- cv.tree(tree.treatment, 
                        FUN = prune.misclass) 
plot(cv.treatment)

#plot says 10 clusters

prune.treatment <- prune.tree(tree.treatment,
                              best = 10)
prune.treatment.pred <- predict(prune.treatment,
                                df_2[-train,],
                                type = "class")
mean(prune.treatment.pred != df_2[-train,]$Treatment)


table(df_2[-train,]$Treatment)
table(df_2[train,]$Treatment)

#behavior
tree.behavior <- update(tree.treatment,
                        Behavior~.)
summary(tree.behavior)

#misclasification error
tree.behavior.pred <- predict(tree.behavior,
                              df_2[-train,],
                              type = "class")

mean(tree.behavior.pred != df_2[-train,]$Behavior)

#class

tree.class <- update(tree.behavior,
                     class~.)
summary(tree.class)

tree.class.pred <- predict(tree.class,
                           df_2[-train,],
                           type = "class")
mean(tree.class.pred != df_2[-train,]$class)
with(data[-train,], table(tree.class.pred, class))

#prune

cv.class <- cv.tree(tree.class,
                    FUN = prune.misclass)
plot(cv.class)
#as per plot cluster 8 , 8 == 36% and 10== 34% error

prune.class <- prune.tree(tree.class,
                          best = 8)
prune.class.pred <- predict(prune.class,
                            df_2[-train,],
                            type = "class")
mean(prune.class.pred != df_2[-train,]$class)


#with svm

require(e1071)
svm.fit <- svm(Genotype ~ . - Treatment - Behavior - class, data=df_2, 
               subset = train)

summary(svm.fit)

#test and train
#genotype
data.train <- df_2[train,]
data.test <- df_2[-train,]
paste("Train accuracy"  ,mean(predict(svm.fit) == data.train$Genotype))

paste("Test accuracy", mean(predict(svm.fit, data.test) ==data.test$Genotype))

#treatment
svm.fit <- update(svm.fit, Treatment ~.)
paste("Train accuracy"  ,mean(predict(svm.fit) == data.train$Treatment))
paste("Test accuracy", mean(predict(svm.fit, data.test) == data.test$Treatment))

#behavior
svm.fit <- update(svm.fit, Behavior ~.)
paste("Train accuracy"  ,mean(predict(svm.fit) == data.train$Behavior))
paste("Test accuracy", mean(predict(svm.fit, data.test) == data.test$Behavior))

table(predict(svm.fit, data.test), data.test$class)


#####################################################
#b

Y <- df_2[,c(78:81)]
X <- df_2[,-c(78:81)]
pr.out <- prcomp(X, scale = TRUE)

head(Y)
var_explained <- pr.out$sdev / sum(pr.out$sdev)
cum_var_explained <- cumsum(var_explained)
cum_var_explained > 0.9

#model

pca_x <- data.frame(pr.out$x)[1:51]
pca_data <- data.frame(pca_x, Y)
pca_data.test <- pca_data[-train,]
# Create models
pcatree.Genotype <- tree(Genotype ~ . - Behavior - Treatment - class, data = pca_data, subset = train)
pcatree.Behavior <- update(pcatree.Genotype, Behavior ~ . )
pcatree.Treatment <- update(pcatree.Genotype, Treatment ~ . )
pcatree.class <- update(pcatree.Genotype, class ~ . )

# Train accuracies
print("Train Accuracy: Geno, Beh, Trea, class")

summary(pcatree.Genotype)
summary(pcatree.Behavior)
summary(pcatree.Treatment)
summary(pcatree.class)

# Get train Accuracy
print("Test Accuracy: Geno, Beh, Trea, class")

mean(predict(pcatree.Genotype, newdata = pca_data.test, type = "class") == pca_data.test$Genotype)
mean(predict(pcatree.Behavior, newdata = pca_data.test, type = "class") == pca_data.test$Behavior)
mean(predict(pcatree.Treatment, newdata = pca_data.test, type = "class") == pca_data.test$Treatment)
mean(predict(pcatree.class, newdata = pca_data.test, type = "class") == pca_data.test$class)


#SVM

pcasvm.Genotype <- svm(Genotype ~ . - Behavior - Treatment - class, data = pca_data, subset = train)
pcasvm.Behavior <- update(pcasvm.Genotype, Behavior ~ . )
pcasvm.Treatment <- update(pcasvm.Genotype, Treatment ~ . )
pcasvm.class <- update(pcasvm.Genotype, class ~ . )

# Train accuracies
print("Train Accuracy: Geno, Beh, Trea, class")

mean(predict(pcasvm.Genotype) == pca_data[train,]$Genotype)

mean(predict(pcasvm.Behavior) == pca_data[train,]$Behavior)
mean(predict(pcasvm.Treatment) == pca_data[train,]$Treatment)
mean(predict(pcasvm.class) == pca_data[train,]$class)

# Get train Accuracy
print("Test Accuracy: Geno, Beh, Trea, class")
mean(predict(pcasvm.Genotype, pca_data.test) == pca_data.test$Genotype)
mean(predict(pcasvm.Behavior, newdata = pca_data.test) == pca_data.test$Behavior)
mean(predict(pcasvm.Treatment, newdata = pca_data.test) == pca_data.test$Treatment)
mean(predict(pcasvm.class, newdata = pca_data.test) == pca_data.test$class)

