getwd()
water <- read.csv("water_potability.csv", stringsAsFactors = FALSE)
str(water)

#sprawdzamy wszystkie cechy
summary(water)

#zamiana danych brakujacych na srednie
ph_mean <- mean(water$ph, na.rm = TRUE)
sulfate_mean <- mean(water$Sulfate, na.rm = TRUE)
Trihalomethanes_mean <- mean(water$Trihalomethanes, na.rm = TRUE)

water$ph <- ifelse(is.na(water$ph), ph_mean, water$ph)
water$Sulfate <- ifelse(is.na(water$Sulfate), sulfate_mean, water$Sulfate)
water$Trihalomethanes <- ifelse(is.na(water$Trihalomethanes), Trihalomethanes_mean, water$Trihalomethanes)

str(water)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
#normalizujemy dane 
water_n <- as.data.frame(lapply(water[1:9], normalize))
summary(water_n)

#tworzymy test i train set dla znormalizowanych danych
#tworzymy wektor z losowymi wartosciami aby losowo wybrac rozne zbiorniki wodne
?sample
train_sample <- sample(3276,2620)
water_n_train <- water_n[train_sample, ]
water_n_test <- water_n[-train_sample, ]
str(water_n_train)

#tworzymy wektory z etykietami dla zbiorow
water_train_labels <- water[train_sample, 10]
water_test_labels <- water[-train_sample, 10]
table(water_test_labels)
table(water_train_labels)

#knn dla normalizowanych danych
library(class)
water_n_test_pred <- knn(train = water_n_train, test = water_n_test, cl = water_train_labels, k=57)

summary(water_n_test_pred)
table(water_n_test_pred)
#robimy tabelke krzyzowa
library(gmodels)
CrossTable(x= water_test_labels, y= water_n_test_pred, prop.chisq=FALSE)

#standaryzujemy dane
water_s <- as.data.frame(lapply(water[1:9], scale))
str(water_s)

#tworzymy test i train set dla standaryzowanych danych
train_sample <- sample(3276,2620)
water_s_train <- water_s[train_sample, ]
water_s_test <- water_s[-train_sample, ]
str(water_s_train)

#knn dla standaryzowanych danych
water_s_test_pred <- knn(train = water_s_train, test = water_s_test, cl = water_train_labels, k=56)
CrossTable(x= water_test_labels, y= water_s_test_pred, prop.chisq=FALSE)

water_train <- water[train_sample, ]
water_test <- water[-train_sample, ]

#drzewo decyzyjne
library(C50)
water_train$Potability <- as.factor(water_train$Potability)
water_model <- C5.0(water_train[-10], water_train$Potability)

summary(water_model)

#model drzewa decyzyjnego
water_pred <- predict(water_model, water_test)
CrossTable(water_test$Potability, water_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual potability', 'predicted_potability'))

#sprawdzamy czy adaboost cos poprawia
water_boost10 <- C5.0(water_train[-10], water_train$Potability, trials = 10)
summary(water_boost10)

water_boost_pred10 <- predict(water_boost10, water_test)
CrossTable(water_test$Potability, water_boost_pred10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual potability', 'predicted_potability'))
