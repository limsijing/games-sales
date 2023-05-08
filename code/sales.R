

## Video Games Sales
sales <- read.csv("~/Downloads/Video_Game_Sales_as_of_Jan_2017.csv")  
head(sales) 

####### EDA ######
sales <- sales %>% drop_na()# left with 7191 observations, no duplicates
# response variable 
sales <- sales %>% mutate(total_sales = NA_Sales + EU_Sales + JP_Sales + Other_Sales) %>%
  filter(total_sales <= Global_Sales) %>%
  select(-c(total_sales))
# Unnecessary columns
sales <- sales %>% dplyr::select(-c(Critic_Count, User_Count, Rating))
# Variable transformation
sales <- sales %>% 
  mutate(top15_publisher = str_detect(Publisher, "Sony|Nintendo|Microsoft|NetEase|Activision|Electronic Arts|
                            Take-Two Interactive|Bandai Namco|Square Enix|Nexon|Netmarble|Ubisoft|Konami|Sega")) %>%
  mutate(top15_publisher = if_else(top15_publisher==TRUE, 1, 0)) %>%
  mutate(top15_publisher = as.factor(top15_publisher))
table(sales$top15_publisher)
sales <- sales %>% 
  mutate(platform.PS = str_detect(Platform, "PS")) %>%
  mutate(platform.PS = if_else(platform.PS == T, 1, 0)) %>%
  mutate(platform.Nintendo = str_detect(Platform, "DS|DC|GBA|GC")) %>%
  mutate(platform.Nintendo = if_else(platform.Nintendo == T, 1, 0)) %>%
  mutate(platform.Wii = str_detect(Platform, "Wii")) %>%
  mutate(platform.Wii = if_else(platform.Wii == T, 1, 0)) %>%
  mutate(platform.Xbox = str_detect(Platform, "X")) %>%
  mutate(platform.Xbox = if_else(platform.Xbox == T, 1, 0))
# OHE
library(fastDummies)
sales <- dummy_cols(sales, select_columns = "Genre")

## Name
sales %>% count(Name) %>% arrange(desc(n))
## Platform
table(sales$Platform)
## Year
table(sales$Year_of_Release)
## Genre
table(sales$Genre)
## Publisher
table(sales$Publisher)
table(sales$Critic_Score)
table(sales$Critic_Count)
table(sales$User_Score)
table(sales$User_Count)

### Preliminary Analysis ###
library(ggplot2)
## Publisher and Sales
sales1$Publisher <- as.factor(sales1$Publisher)
summary(sales1$Publisher)  
# top 5 companies
sales %>% 
  filter(Publisher %in% c("Activision", "Microsoft Game Studios", "Nintendo",
                          "Sony Computer Entertainment", "Electronic Arts")) %>%
  ggplot() +
  geom_boxplot(aes(x = Publisher, y = Global_Sales, fill = Publisher)) +
  labs(title = "Global sales by publisher", y = "Global sales")
# removing anomaly  
sales %>% filter(Global_Sales < 80) %>%
  filter(Publisher %in% c("Activision", "Microsoft Game Studios", "Nintendo",
                          "Sony Computer Entertainment", "Electronic Arts")) %>%
  ggplot() +
  geom_boxplot(aes(x = Publisher, y = Global_Sales, fill = Publisher)) +
  labs(title = "Global sales by publisher", y = "Global sales")
# comparison with companies not in top15
sales %>% filter(Global_Sales < 80) %>%
  filter(Publisher %in% c("Capcom", "Microsoft Game Studios", "Nintendo",
                          "Sony Computer Entertainment", "Atari")) %>%
  ggplot() +
  geom_boxplot(aes(x = Publisher, y = Global_Sales, fill = Publisher)) +
  labs(title = "Global sales by publisher", y = "Global sales")

## Game
sales1$Name <- as.factor(sales1$Name)
summary(sales1$Name)
sales %>% group_by(Name) %>% arrange(desc(Global_Sales)) %>% head(5) %>%
  ggplot() +
  geom_col(aes(x = Name, y = Global_Sales)) +
  coord_flip() +
  labs(title = "Sales for each game", y = "Global sales (in millions)", x = "Game name")

## Critic score and sales
ggplot(sales, aes(x = Critic_Score, y = Global_Sales)) +
  geom_point() +
  labs(title = "Global sales by critic score", y = "Global sales", x = "Critic score")
sales %>% filter(Global_Sales < 80) %>%
  ggplot(aes(x = Critic_Score, y = Global_Sales)) +
  geom_point(position = "jitter") +
  geom_smooth(span=0.6) +
  labs(title = "Global sales by critic score", y = "Global sales", x = "Critic score")

## User score and sales
sales %>% filter(Global_Sales < 80) %>%
  ggplot(aes(x = User_Score, y = Global_Sales)) +
  geom_point(position = "jitter") +
  geom_smooth(span=0.6) +
  labs(title = "Global sales by user score", y = "Global sales", x = "User score")

## Genre 
sales1$Genre <- as.factor(sales1$Genre)
summary(sales1$Genre)
sales %>% filter(Global_Sales < 80) %>%
  ggplot() +
  geom_boxplot(aes(x = Genre, y = Global_Sales, fill = Genre)) +
  labs(title = "Global sales by genre", y = "Global sales")

## Platform
sales1$Platform <- as.factor(sales1$Platform)
summary(sales1$Platform)
# Nintendo: 3DS, DC, DS, GC, GBA
# Sony: PS, PS2, PS3, PS4, PSP, PSV
# Wii: Wii, WiiU
# Micrsoft: X, X360, XOne

sales <- sales %>% select(-c(Name, NA_Sales, EU_Sales, JP_Sales, Other_Sales,
                             Platform, Genre, Publisher))
sales <- sales %>% filter(Global_Sales < 80)
sales$Year_of_Release <- as.factor(sales$Year_of_Release)
median(sales$Global_Sales)
sales <- sales %>% 
  mutate(median = 1*(Global_Sales > median(Global_Sales))) %>%
  mutate(median = as.factor(median))
sales <- sales %>% rename(Genre_Role = `Genre_Role-Playing`)

## Checking for collinearity
library(car)
sales <- sales %>%
  mutate_if(is.character, as.factor)
model <- lm(total_sales~.-Platform - Genre - Publisher, data = sales)
summary(model)
vif(model)

# Best subset selection
library(leaps)
sales2$Year_of_Release <- as.factor(sales2$Year_of_Release)
regfit.full <- regsubsets(Global_Sales~.-Name-Platform-Publisher-NA_Sales-EU_Sales-JP_Sales-Other_Sales-Critic_Count-User_Count-Rating,
                          data = sales2, nvmax = 14)
reg.summary <- summary(regfit.full)
par(mfrow = c(1,3))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",
     type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adj
  usted RSq",type="l")
which.max(reg.summary$adjr2)
points(14,reg.summary$adjr2[14], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type="l")
which.min(reg.summary$cp)
points(14,reg.summary$cp[14],col="red",cex=2,pch=20)
which.min(reg.summary$bic)


## Train Test split
set.seed(1)
num_training = floor(5185 * 0.8)
train = sample(1:nrow(sales), num_training)
sales.train = sales[train,]
sales.test = sales[-train,]

print(c(nrow(sales.train), nrow(sales.test)))

######### MODELLING #########
# GAM
library(gam)
## Smoothing splines
gam_sales <- sales %>% select(-c(Platform, Genre, Publisher)) %>%
  select(Global_Sales, Critic_Score, User_Score, top15_publisher,
         platform.PS, platform.Nintendo, platform.Wii, platform.Xbox,
         Genre_Action, Genre_Adventure, Genre_Shooter, Genre_Simulation, Genre_Sports)
set.seed(1)
num_training = floor(5185 * 0.8)
train = sample(1:nrow(gam_sales), num_training)
gam_sales.train = gam_sales[train,]
gam_sales.test = gam_sales[-train,]
gam.fit1 <- gam(Global_Sales ~ s(Critic_Score,4) + s(User_Score, 4) + top15_publisher +
                  platform.PS + platform.Nintendo + platform.Wii + platform.Xbox + 
                  Genre_Action + Genre_Adventure +Genre_Shooter + Genre_Simulation + Genre_Sports, data = gam_sales.train)
par(mfrow = c(3, 4))
plot(gam.fit1, se = T, col = "blue")
summary(gam.fit1) ## non linear fit in numeric variables
gam.preds <- predict(gam.fit1, gam_sales.test)
sqrt(mean((gam.preds - gam_sales.test$Global_Sales)^2)) # 1.52243

## Natural splines
gam.fit2 <- gam(Global_Sales ~ ns(Critic_Score,4) + ns(User_Score, 4) + top15_publisher +
                  platform.PS + platform.Nintendo + platform.Wii + platform.Xbox + 
                  Genre_Action + Genre_Adventure +Genre_Shooter + Genre_Simulation + Genre_Sports, data = gam_sales.train)
par(mfrow = c(3, 4))
plot(gam.fit2, se = T, col = "red")
summary(gam.fit2) ## non linear fit in numeric variables
gam.preds <- predict(gam.fit2, gam_sales.test)
sqrt(mean((gam.preds - gam_sales.test$Global_Sales)^2)) # 1.506428

## Natural splines performed better
set.seed(1)
k = 10
fold.error = rep(0,k)
n = length(gam_sales$Global_Sales)
index = sample(n)
foldbreaks = c(0, floor(n/k * 1:k))
cv.error.k = matrix(NA, 10, 10, dimnames = list(NULL, paste(1:10)))
for(i in 1:10) {
  for (j in 1:10) {
    for(fold in 1:k) {
      curval = index[(1+foldbreaks[fold]):(foldbreaks[fold+1])]
      cv.gam.fit = gam(Global_Sales ~ ns(Critic_Score,4) + ns(User_Score, 4) + top15_publisher +
                         platform.PS + platform.Nintendo + platform.Wii + platform.Xbox + 
                         Genre_Action + Genre_Adventure +Genre_Shooter + Genre_Simulation + Genre_Sports, 
                       data = gam_sales.train)
      fold.error[fold] = mean((gam_sales$Global_Sales - predict(cv.gam.fit, gam_sales))[curval]^2)
    }
    cv.error.k[i, j] = mean(fold.error)
  }
}
cv.error.k
which.min(cv.error.k)

# degree of freedom: 5
gam.fit3 <- gam(Global_Sales ~ ns(Critic_Score,5) + ns(User_Score, 5) + top15_publisher +
                  platform.PS + platform.Nintendo + platform.Wii + platform.Xbox + 
                  Genre_Action + Genre_Adventure +Genre_Shooter + Genre_Simulation + Genre_Sports, 
                data = gam_sales.train)
par(mfrow = c(3, 4))
plot(gam.fit3, se = T, col = "red")
summary(gam.fit3) ## non linear fit in numeric variables
gam.preds <- predict(gam.fit3, gam_sales.test)
sqrt(mean((gam.preds - gam_sales.test$Global_Sales)^2)) #1.500648

# Polynomial regression
poly.fit <- lm(Global_Sales ~ poly(Critic_Score,2) + poly(User_Score, 2) + top15_publisher +
                 platform.PS + platform.Nintendo + platform.Wii + platform.Xbox + 
                 Genre_Action + Genre_Adventure +Genre_Shooter + Genre_Simulation + Genre_Sports, data = gam_sales.train)
poly.pred <- predict(poly.fit, gam_sales.test)
sqrt(mean((poly.pred - gam_sales.test$Global_Sales)^2)) # 1.55

# Random forest
library(randomForest)
set.seed(2)
cv <- c()
count <- 1
for(i in seq(3, 18, 3)) {
  rf <- randomForest(Global_Sales ~.-Platform-Genre-Publisher-median, data = sales.train,
                     mtry = i, importance = T)
  yhat.rf <- predict(rf, sales.test)
  cv[count] <- mean((yhat.rf - sales.test$Global_Sales)^2)
  count <- count + 1
}
## mtry: 9
rf <- randomForest(Global_Sales ~.-Platform-Genre-Publisher-median, data = sales.train,
                   mtry = 9, importance = T)
yhat.rf <- predict(rf, sales.test)
mean((yhat.rf - sales.test$Global_Sales)^2) #2.3903
varImpPlot(rf)

## classification for random forest
set.seed(2)
cv <- c()
count <- 1
for(i in seq(3, 18, 3)) {
  rf <- randomForest(median ~.-Platform-Genre-Publisher-Global_Sales, data = sales.train,
                     mtry = i, importance = T)
  rf.pred <- predict(rf, sales.test)
  print(table(predict = rf.pred, truth = sales.test$median))
}
# mtry = 3, CV = 69.95%
rf.class <- randomForest(median ~.-Platform-Genre-Publisher-Global_Sales, data = sales.train,
                         mtry = 3, importance = T)
rf.pred <- predict(rf.class, sales.test)
print(table(predict = rf.pred, truth = sales.test$median))
confusionMatrix(table(rf.pred, sales.test$median))
## Accuracy: 0.7338

# XGBoost
library(xgboost)
library(caret)
set.seed(2)
sales.xtrain = sales.train %>%
  dplyr::select(-c(Platform, Genre, Publisher, median))
sales.ytrain = sales.train %>% select(Global_Sales)
  
sales.xtest = sales.test %>%
  dplyr::select(-c(Platform, Genre, Publisher, median))
sales.ytest = sales.test %>% select(Global_Sales)

xgb.tuning_grid <- expand.grid(nrounds = c(30, 50 ,100, 125),
                               max_depth = c(3,5),
                               eta = c(0.3, 0.5),
                               colsample_bytree = c(0.8, 1),
                               gamma = c(1,3),
                               min_child_weight = 1,
                               subsample = c(0.8 ,1))
train_control <- trainControl(method = "cv", number = 5, search = "random")
xgb.RandomSearchCV <- train(Global_Sales~.-Publisher-Platform-Genre-median,
                            sales.train, method = "xgbTree",
                            trControl = train_control, tuneGrid = xgb.tuning_grid, 
                            verbosity = 0)
## refitting XGB
xgb.RandomSearchCV$bestTune
xgb.tuning_grid <- expand.grid(nrounds = 30,
                               max_depth = 3,
                               eta = 0.3,
                               colsample_bytree = 0.8,
                               gamma = 3,
                               min_child_weight = 1,
                               subsample = 1)
train_control <- trainControl(method = "cv", number = 5, search = "random")
xgb.RandomSearchCV <- train(Global_Sales~.,
                            sales.xtrain, method = "xgbTree",
                            trControl = train_control, tuneGrid = xgb.tuning_grid, 
                            verbosity = 0)
xgb.pred <- predict(xgb.RandomSearchCV, sales.xtest)
mean((xgb.pred - sales.xtest$Global_Sales)^2)

caret_imp <- varImp(xgb.RandomSearchCV)
plot(caret_imp)

# SVM
# linear
library(e1071)
library(ROCR)
library(caret)
set.seed(123)
linear.tune <- tune(svm, median~.-Platform-Genre-Publisher-Global_Sales, data = sales.train, kernel = "linear", 
                    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 50)),
                    tunecontrol = tune.control(cross = 5))
summary(linear.tune)
## cost = 1, CV error = 0.2878
best.linear <- linear.tune$best.model
summary(best.linear)
linear.ypred <- predict(best.linear, sales.test)
table(predict = linear.ypred, truth = sales.test$median)
confusionMatrix(table(linear.ypred, sales.test$median))
## Accuracy: 0.7252

# polynomial
set.seed(123)
polynomial.tune <- tune(svm, median~.-Platform-Genre-Publisher-Global_Sales, 
                        data = sales.train, kernel = "polynomial", 
                        ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 50),
                                      degree = c(2,3,4,5)),
                        tunecontrol = tune.control(cross = 5))
summary(polynomial.tune)
poly.ypred <- predict(polynomial.tune$best.model, sales.test)
table(predict = poly.ypred, truth = sales.test$median)
confusionMatrix(table(linear.ypred, sales.test$median))
## Accuracy: 0.7252
## cost = 50, degree = 2, CV error = 0.28737

# radial
radial.tune <- tune(svm, median~.-Platform-Genre-Publisher-Global_Sales,
                    data = sales.train, kernel = "radial", 
                    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20),
                                  gamma = c(1,2,3,4,5)),
                    tunecontrol = tune.control(cross = 5))
summary(radial.tune)
## cost = 1, gamma = 1, CV error = 0.3199
radial.svm <- svm(median~.-Platform-Genre-Publisher-Global_Sales, data = sales.train,
                  kernel = "radial", cost = 1, gamma = 1, scale = T)
radial.ypred <- predict(radial.svm, sales.test)
confusionMatrix(table(radial.ypred, sales.test$median))

## polynomial kernel performed the best
