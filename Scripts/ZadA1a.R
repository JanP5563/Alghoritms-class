setwd("C:\\Users\\rysio\\Desktop\\Informatyka\\R\\DataMining\\Project\\Plots")
# subpoint a)
# --------- Defining data --------------
# x <-> Hours and y <-> Results
x <- c(25, 35, 40, 50, 55, 60, 70, 89, 92, 100)
y <- c(0, 0, 0, 1, 0, 1, 1, 1, 1, 1)

# Defining colors
# --- series 1 ---
col_points <- "#34495E"      # charcoal
col_curve <- "#3498DB"       # light blue
col_threshold <- "#E67E22"   # warm orange
col_grid_05 <- "#001733"     # black with blue accent

# Number of elements
n <- 200

# ---------- Regression ----------------
# Fit logistic regression model
m <- glm(y ~ x, family = binomial)

# Print to summerize model
print(summary(m))
print(exp(coef(m)))

# Create dense x sequence for smooth curve
x_seq <- seq(min(x), max(x), length.out = n)

# Predict probabilities directly
P_smooth <- predict(m, newdata = data.frame(x = x_seq), type = "response")

png('../Plots/plot1Aa.png', width = 1500, height = 1500, res = 150, type = "cairo", bg="white")
# ---------- Plot -------------------
plot(x, y, pch = 16, cex = 1.5, col = col_points,
     xlab = "Hours", ylab = "Probability",
     main = "Logistic Regression for Ex. 1A a)",
     cex.main = 1.8,
     ylim = c(0, 1),
     xlim = c(min(x), max(x)))
grid()

# Coef to extract model coeficients
coefs <- coef(m)

# Calc for x for y = 0.5
x_05 <- -coefs[1] / coefs[2]

lines(x_seq, P_smooth, col = col_curve, lwd = 2)
# Y axis
abline(h = 0.5, col=col_grid_05, lwd = 1, lty = 2)
axis(2, at = 0.5, cex.axis = 0.8)
# X axis
abline(v = x_05, col=col_grid_05, lwd = 1, lty = 2)
axis(1, at = round(x_05, digits = 2), cex.axis = 0.8, padj = -0.4)

points(x_05, 0.5, pch = 16, cex = 1.2, col = col_threshold)
dev.off()