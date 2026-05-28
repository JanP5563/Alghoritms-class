
library(foreign)
my_data <- read.arff("~/Desktop/website+phishing/PhishingData.arff")

set.seed(123)

index_train <- sample(1:nrow(my_data), 0.8 * nrow(my_data))

train <- my_data[index_train, ]
test <- my_data[-index_train, ]

train_num <- as.data.frame(lapply(train, function(x) as.numeric(as.character(x))))
test_num <- as.data.frame(lapply(test, function(x) as.numeric(as.character(x))))

kol_min <- apply(train_num, 2, min)
kol_mean <- apply(train_num, 2, mean)
kol_med <- apply(train_num, 2, median)
kol_max <- apply(train_num, 2, max)
kol_var <- apply(train_num, 2, var)

summary <- data.frame(
  Min = kol_min,
  Mean = kol_mean,
  Median = kol_med,
  Max = kol_max,
  Variance = kol_var
)

print(summary)


covariance_matrix <- cov(train_num)
covariance_matrix

correlation_matrix <- cor(train_num)
correlation_matrix

if(!require(corrplot)) install.packages("corrplot")
library(corrplot)

round(correlation_matrix, 2)

corrplot(correlation_matrix, 
         method = "color",       
         type = "upper",         
         addCoef.col = "black",  
         tl.col = "black",       
         tl.srt = 45,           
         number.cex = 0.7,       
         diag = FALSE)           


if(!require(ggplot2)) install.packages("ggplot2")
if(!require(reshape2)) install.packages("reshape2")
library(ggplot2)
library(reshape2)


melted_cov <- melt(covariance_matrix)

ggplot(data = melted_cov, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "darkred", high = "darkblue", mid = "white", 
                       midpoint = 0, name="Covariance") + 
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  coord_fixed() +
  ggtitle("Covariance Matrix Plot (Covplot)")



#SVM
library(e1071)

train_num$Result <- as.factor(train_num$Result)
test_num$Result <- as.factor(test_num$Result)
model_svm <- svm(Result ~ ., data = train_num, kernel = "radial", scale = TRUE)
summary(model_svm)
#SVM


test_features <- test_num[, names(test_num) != "Result"]

predictions <- predict(model_svm, test_features)

confusion_matrix <- table(Realne = test_num$Result, Przewidziane = predictions)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat(paste("Accuracy:", round(accuracy * 100, 2), "%\n"))

library(ggplot2)

confusion_df <- as.data.frame(confusion_matrix)

ggplot(data = confusion_df, aes(x = Przewidziane, y = Realne, fill = Freq)) +
  geom_tile(color = "white") +
  
  scale_fill_gradient(low = "aliceblue", high = "navyblue", name = "Count") +
  geom_text(aes(label = Freq), color = "black", size = 5, fontface = "bold") +
  theme_minimal() +
  labs(title = "SVM Confusion Matrix Visualization",
       x = "Predicted Class (Model Decision)",
       y = "Actual Class (Ground Truth)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
