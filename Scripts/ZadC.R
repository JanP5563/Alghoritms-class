setwd("C:/Jan/Studia/IV SEMESTR/Algorithms/Projekt/Scripts")
# Project, Question B

install.packages("FNN") # for knn regression
library(FNN)
install.packages("modelsummary")
library(modelsummary)

# Improving readibility in plots
options(scipen = 999)

# Reading and describing the data --------------------------------------
# Numeric data
path = "../data/sggw_b.csv"
data <- read.csv(path)
str(data)
data_c <- data
data_c$Price... <- data_c$Price.../1000000
colnames(data_c)[colnames(data_c) == "Price..."] <- "Price (in mln.)"
datasummary(All(data_c) ~ Mean + SD + Min + Max, data = data, output = "latex")
head(data, 6)
variance <- sapply(data[,], var)


# Histograms and barplots

par(mfrow = c(2, 2))
hist(data$Area..ft2., 
     breaks = 12, 
     main = "Area", 
     xlab = "Area  (ft^2)", 
     ylab = "Number of houses", 
     col = "#3498DB")

columns <- c("Bedrooms", "Bathrooms", "Stories")

for (col in columns) {
  barplot(table(data[[col]]), 
        main = col, 
        xlab = col, 
        ylab = "Number of houses", 
        col = "#3498DB")
}

par(mfrow = c(1, 1))
hist(data$Price/1000000, 
     main = "Price", 
     xlab = "Price (in millions)", 
     ylab = "Number of houses", 
     col = "#3498DB")


# Binary data
columns_bin <- c("Metro","Basement", "Aircon", "ParkingSpace", "PopularArea", "Furnished")
percents <- colMeans(data[, columns_bin]) * 100

# Changing the margins, so they can accomodate the text
par(mar = c(8, 4, 4, 2) + 0.1)
x_coords <- barplot(percents, 
                    main = "% of houses having the amenity",
                    ylab = "% of houses", 
                    col = "#3498DB",
                    ylim = c(0, 100),
                    names.arg = "")

# Our custom text, for better readibility
text(x = x_coords, 
     y = -4,                        
     labels = columns_bin,          
     srt = 45,                      
     adj = 1,                       
     xpd = TRUE)                  

# Reset the margins
par(mar = c(5, 4, 4, 2) + 0.1)





# ----------------------------------------------------------
# Analysis, with kNN for regression
# ----------------------------------------------------------

# Dividing the data

set.seed(123)
indexes <- sample(1:nrow(data), size = round(0.8 * nrow(data)))
cols <- setdiff(colnames(data), "Price")

Y_train <- data$Price...[indexes]
Y_test <- data$Price...[-indexes]

X_train_unscaled <- data[, cols][indexes, ]
X_test_unscaled <- data[, cols][-indexes, ]

# Scaling the data

scaled_matrix_train <- scale(X_train_unscaled)
avg_train <- attr(scaled_matrix_train, "scaled:center")
var_train <- attr(scaled_matrix_train, "scaled:scale")
X_train <- as.data.frame(scaled_matrix_train)

X_test <- as.data.frame(scale(
  X_test_unscaled, 
  center = avg_train, 
  scale = var_train
))


# Regression for different k-values

min_rmse <- Inf
min_k <- NA
n <- 15 # number of k values checked
rmse_values <- numeric(n)

for (i in 1:n) {
  prediction <- knn.reg(train = X_train, test = X_test, y = Y_train, k = i)$pred
  rmse <- sqrt(mean((Y_test - prediction)^2))
  rmse_values[i] <- rmse
  
  if(rmse < min_rmse){
    min_rmse <- rmse
    min_k <- i
  }
  cat("For k =", i, "RMSE is:", round(rmse/10^6, 3), "mln \n")
}

plot(1:n, rmse_values/10^6, type = "b", pch = 19, col = "#3498DB",
     xlab = "k value", ylab = "RMSE (in mln.)",
     main = "RMSE dependence on the k parameter")

points(min_k, min_rmse/10^6, col = "red", pch = 19, cex = 1.5)

cat("Min rmse is", min_rmse, "for k:", min_k)




avg_price <- sum(Y_train)/length(Y_train)
cat("RMSE is", round(rmse/10^6, 3), "mln")
cat("Average price is:", round(avg_price/10^6, 4), "mln")
cat("RMSE is", round(rmse/avg_price*100, 1), "percents of the average price")

# k=1 was overfitting, k=2 is perfect, the lowest rmse score,
# and then, after k=3 and beyond, the rmse was rising constantly, was underfit

# Obliczamy przewidywania dla najlepszego k znalezionego w pętli
best_prediction <- knn.reg(train = X_train, test = X_test, y = Y_train, k = min_k)$pred


# 1. R^2
ss_res <- sum((Y_test - best_prediction)^2)
ss_tot <- sum((Y_test - mean(Y_test))^2)
r_squared <- 1 - (ss_res / ss_tot)

# 2. MAE
mae <- mean(abs(Y_test - best_prediction))

# 3. MAPE
mape <- mean(abs((Y_test - best_prediction) / Y_test)) * 100

cat("R^2:", round(r_squared, 3), "\n")
cat("MAE:", round(mae/10^6, 3), "mln\n")
cat("Average percentage error (MAPE):", round(mape, 1), "%\n")
