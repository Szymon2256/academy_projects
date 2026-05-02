library(tree)
library(randomForest)
library(xgboost)
library(ISLR2)
data(College)
summary(College)
View(College)

New_College <- College[College$Grad.Rate <=100,]
New_College <- New_College[New_College$PhD <=100,]
summary(New_College)
View(New_College)
New_College$Grad.Rate
New_College$Private=ifelse(New_College$Private=="Yes", 1, 0)

#Tworzymy czynnik sprawdzajacy szkoly z najlepszą zdawalnością
Best <- factor(ifelse(New_College$Grad.Rate < 80, "No", "Yes"))
Graduation <- data.frame(New_College, Best)
View(Graduation)

#budujemy model
tree.college <- tree(Best ~. -Grad.Rate, Graduation)
summary(tree.college)

#wizualizacja drzewa 
plot(tree.college)
text(tree.college, pretty = 0)

tree.college
#------------------------------------------------------------------------
#Drzewo regresyjne
#Tworzymy zbior treningowy w naszych danych
set.seed(2)
train = sample(1:nrow(New_College),nrow(New_College)*0.8)

# Sadzimy drzewo
tree.grad <- tree(Grad.Rate ~. , New_College, subset = train)
summary(tree.grad)

plot(tree.grad)
text(tree.grad, pretty = 0)

#sprawdzamy czy za pomoca walidacji krzyzowej jest sens to drzewo przycinać
cv.grad = cv.tree(tree.grad)
cv.grad
plot(cv.grad$size, cv.grad$dev, type = "b", xlab = "Rozmiar", ylab = "Dewiancja")

#Rysujemy przycięte drzewo, parametr wybraliśmy za pomocą walidacji krzyżowej
prune.grad=prune.tree(tree.grad ,best=5)
plot(prune.grad)
text(prune.grad , pretty =0)

#predykcja na zbiorze testowym
y = predict(tree.grad, newdata = New_College[-train,])
Grad.test = New_College[-train, "Grad.Rate"]
plot(y, Grad.test)
abline(0,1)
mean((y-Grad.test)^2)

#------------------------------------------------------------------
#Bagging (parametr mtry równy liczbie obserwacji)
set.seed(2)
bag.grad <- randomForest(Grad.Rate ~., data = New_College, subset = train,
                           mtry = 18, importance = TRUE)
bag.grad

y.bag <- predict(bag.grad, newdata = New_College[-train,])
plot(y.bag, Grad.test)
abline(0,1)
mean((y.bag-Grad.test)^2)

#Random Forest
set.seed(2)
rf.grad <- randomForest(Grad.Rate ~., data = New_College, subset = train,
                          mtry = 3, ntree = 2000, importance = TRUE)
y.rf <-  predict(rf.grad, newdata = New_College[-train,])
mean((y.rf - Grad.test)^2)

importance(rf.grad)
varImpPlot(rf.grad)

#--------------------------------------------------------------------
#XGBoost


d_train = New_College[train, ]
d_test = New_College[-train, ]

train_x = data.matrix(d_train[, -18])
train_y = d_train[,18]

test_x = data.matrix(d_test[, -18])
test_y = d_test[, 18]

xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

model = xgb.train(data = xgb_train, max.depth = 3, watchlist=list(train=xgb_train, test=xgb_test), nrounds = 50)

final_model = xgboost(data = xgb_train, max.depth = 3, nrounds = 15, verbose = 0)

pred_y = predict(final_model, xgb_test)

plot(pred_y, test_y)
abline(0,1)

mean((test_y - pred_y)^2)
