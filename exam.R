scores <- c(55, 60, 65, 67, 70, 72, 75, 76, 80, 82, 85, 88, 90, 92, 95, 
            96, 98, 100, 102, 105, 107, 110, 112, 115, 118, 120, 122, 
            125, 127, 130)

# Perform Kolmogorov-Smirnov test
ks.result <- ks.test(scores, "pnorm", mean = mean(scores), sd = sd(scores))

# Display the result
print(ks.result)
plot(ecdf(scores), main = "Empirical vs Theoretical CDF", col = "blue")
curve(pnorm(x, mean = mean(scores), sd = sd(scores)), 
      add = TRUE, col = "red", lwd = 2)
legend("bottomright", legend = c("Empirical CDF", "Theoretical CDF"),
       col = c("blue", "red"), lwd = 2)