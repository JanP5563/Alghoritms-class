setwd("C:\\Users\\rysio\\Desktop\\Informatyka\\R\\DataMining\\Project\\Plots")
# --------- Defining data --------------
# x <-> Hours and y <-> Results
x <- c(0.1, 0.5, 1, 1.5, 2.05, 2.5, 3, 5)
y <- c(0.23, 0.65, 0.9, 0.8, 0.7, 0.6, 0.5, 0.25)

model1 <- nls(y ~ (d * x) / (1 + e * x^2), 
             start = list(d = 1, e = 1))
print(sigma(model1))
print(sqrt(mean(residuals(model1)^2)))

model2 <- nls(y ~ (d * x) * exp(-b * x), 
              start = list(b = 0.1, d = 1))
print(sigma(model2))
print(sqrt(mean(residuals(model2)^2)))

model3 <- nls(y ~ a + b*x + c*x^2 + d*x^3, 
              start = list(a = 1, b = 1, c = 1, d = 1))
print(sigma(model3))
print(sqrt(mean(residuals(model3)^2)))

# ---------- Plot -------------------
png('plotA2c.png', width = 1500, height = 1500, res = 150, type = "cairo", bg="white")

plot(x, y, pch = 19, cex = 1.2, xlab = "x", ylab = "y", cex.lab = 1.5, col = "#222222",
     main = "Models from sub-point 2a) fitted with NonLinear Least-Squares", cex.main = 1.8)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "solid")
# graph1
cf <- coef(model1)
curve((cf["d"] * x) / (1 + cf["e"] * x^2),
      from = min(x), to = max(x),
      add = TRUE, col = "#0072B2", lwd = 2, n = 500)
# graph2
cf <- coef(model2)
curve((cf["d"] * x) * exp(-cf["b"] * x),
      from = min(x), to = max(x),
      add = TRUE, col = "#D55E00", lwd = 2, n = 500)
# graph3
cf <- coef(model3)
curve(cf["a"] + cf["b"]*x + cf["c"]*x^2 + cf["d"]*x^3,
      from = min(x), to = max(x),
      add = TRUE, col = "#009E73", lwd = 2, n = 500)

# points
points(x, y, col = "#222222", pch = 19, cex = 1.2)

legend("topright", 
       legend = c("f1(x)", "f2(x)", "f3(x)"), 
       col = c("#0072B2", "#D55E00", "#009E73"),
       cex = 1.5,
       lty = c(1, 1, 1), # Corresponds to line types
       pch = NA,         # Disables points in the legend
       lwd = 2)          # Matches line thickness

dev.off()