# Question 4: Housing Market Analysis (Regression)
# -------------------------------------------------

# Part A: Summary stats & baseline plot
df <- read.csv("data/sggw_b.csv")

# fix column names because they have weird characters
names(df)[1] <- "Area"
names(df)[11] <- "Price"

print("--- Data Summary ---")
summary(df[, c("Area", "Price")])

# basic scatter plot - using lightblue to match Jan's plots
plot(df$Area, df$Price,
     xlab = "Area (sq ft)",
     ylab = "Price ($)",
     main = "House Price vs. Living Area",
     col = "lightblue",
     pch = 19)


# Part B: Linear Regression
model_lin <- lm(Price ~ Area, data = df)

print("--- Linear Model Summary ---")
summary(model_lin)

# add the straight linear trendline (orange)
abline(model_lin, col = "#E67E22", lwd = 2)


# Part C: Polynomial Regression (Degree 2)
model_poly <- lm(Price ~ poly(Area, 2, raw = TRUE), data = df)

print("--- Polynomial Model Summary ---")
summary(model_poly)

# generate a smooth grid to plot the curve nicely
grid_range <- seq(min(df$Area), max(df$Area), length.out = 200)
preds <- predict(model_poly, newdata = data.frame(Area = grid_range))

# add the blue polynomial curve line over the plot
lines(grid_range, preds, col = "#3498DB", lwd = 2)