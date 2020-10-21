# --------------- Load add-on packages ---------------
library("AER")

# --------------- Load data ---------------
data("CASchools")

# Me dice que tipo de objeto es CASchools
class(CASchools)

# Describe las primeras líneas del argumento
head(CASchools)

# --------------- New variables ---------------
CASchools$str <- CASchools$students / CASchools$teachers
CASchools$testscr <- (CASchools$read + CASchools$math) / 2

# --------------- Estadísticas descriptivas ---------------

# Mean
mean_str <- mean(CASchools$str)
mean_testscr <- mean(CASchools$testscr)

# Standard deviations
sd_str <- sd(CASchools$str)
sd_testscr <- sd(CASchools$testscr)

# Set up a vector of percentiles and compute the quantiles
quantiles <- c(0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)
quant_str <- quantile(CASchools$str, quantiles)
quant_testscr <- quantile(CASchools$testscr, quantiles)

# Gather everything in a dataframe
summary_report <- data.frame(mean = c(mean_str, mean_testscr), 
                             standard_deviation = c(sd_str, sd_testscr), 
                             quantile = rbind(quant_str, quant_testscr))
summary_report

# Gráfico de nube de puntos
plot(x = CASchools$str, y = CASchools$testscr, 
     main = "Scatterplot of STR and TESTSCR",
     xlab = "students / teachers", 
     ylab = "test score")

# Correlación
cor(x = CASchools$str, y = CASchools$testscr)

# --------------- Estimación de b1hat y b0hat ---------------

# Command attach: allows to use the variables contained in CASchools directly
attach(CASchools)

# Compute beta1_hat
beta_1 <- sum( (str - mean(str)) * (testscr - mean(testscr)) / 
               sum( (str - mean(str))^2 ) )

# Compute beta0_hat
beta_0 <- mean(testscr) - beta_1 * mean(str)

# --------------- Parte automática ---------------

# Estimate the model and assign the result lo linear_model
linear_model <- lm(formula = testscr ~ str, 
                   data = CASchools)
linear_model
summary(linear_model)

# --------------- Data plot ---------------

plot(x = str, y = testscr, 
     main = "Scatterplot of STR and TESTSCR", 
     xlab = "students / teachers", 
     ylab = "test score", 
     xlim = c(10, 30), 
     ylim = c(600, 720))

# Add the regression line
abline(linear_model)

# --------------- Parte manual ---------------

# Compute R^2 manually
SSR <- sum(linear_model$residuals^2)
SST <- sum((testscr - mean(testscr))^2)
R2 <- 1 - SSR/SST

# Compute Root MSE
n <- nrow(CASchools)
MSE <- sqrt(SSR / (n - 2))
# Entre más grande sea, la precisión del modelo es menor

# --------------- Prueba de hipótesis ---------------

# Print the summary of the coefficients to the console
summary(linear_model)$coefficients

# Determine residual degress of freedom
linear_model$df.residual

# p - value (dos colas)
2 * pt(q = -4.751327, df = 418)

# However since n is sufficiently large one could just as well use the standard
# normal density to compute p-value:
2 * pnorm(q = -4.751327)

# --------------- Results plot ---------------

# Plot the standard normal on the support [-6, 6]
t <- seq(-6, 6, 0.01)
plot(x = t, 
     y = dnorm(x = t, mean = 0, sd = 1), 
     type = "l", 
     col = "steelblue", 
     lwd = 2, 
     yaxs = "i", 
     axes = F, 
     ylab = "", 
     main = expression(
       "Calculating the p-value of a Two-side Test when" ~ t^calc ~ " = -4.75"
     ), 
     cex.lab = 0.7, 
     cex.main = 1)

tcalc <- -4.75
axis(side = 1, 
     at = c(0, -1.96, 1.96, -tcalc, tcalc), 
     cex.axis = 0.7)

# Shade the critical regions using polygon():
# Critical region in left tail
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96), 
        y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0), 
        col = "orange")
# Critical region in right tail
polygon(x = c(1.96, seq(1.96, 6, 0.01), 6), 
        y = c(0, dnorm(seq(1.96, 6, 0.01)), 0), 
        col = "orange")

# Add arrows and texts indicating critical regions and the p-value
arrows(x0 = -3.5, y0 = 0.2, x1 = -2.5, y1 = 0.02, length = 0.1)
arrows(x0 = 3.5, y0 = 0.2, x1 = 2.5, y1 = 0.02, length = 0.1)
arrows(x0 = -5, y0 = 0.16, x1 = -4.75, y1 = 0, length = 0.1)
arrows(x0 = 5, y0 = 0.16, x1 = 4.75, y1 = 0, length = 0.1)
text(x = -3.5, y = 0.22, 
     labels = expression("0.025" ~ " = " ~ over(alpha, 2)), 
     cex = 0.7)
text(x = 3.5, y = 0.22, 
     labels = expression("0.025" ~ " = " ~ over(alpha, 2)), 
     cex = 0.7)
text(x = -5, y = 0.18, 
     labels = expression(paste("-| ", t[calc], " |")), 
     cex = 0.7)
text(x = 5, y = 0.18, 
     labels = expression(paste("| ", t[calc], " |")), 
     cex = 0.7)

# Add ticks indicating critical values at the 0.05-level, t^calc and -t^calc
rug(x = c(-1.96, 1.96), 
    ticksize = 0.145, 
    lwd = 2, 
    col = "darkred")
rug(x = c(-tcalc, tcalc), 
    ticksize = -0.0451, 
    lwd = 2, 
    col = "darkred")

# --------------- Regresión con variable binaria ---------------

CASchools$dummy_tam <- CASchools$str < 20
head(CASchools)

# Plot the data
plot(x = CASchools$dummy_tam, y = CASchools$testscr, 
     # Provide the data to be plotted
     pch = 20, # Use filled circles as plot symbols
     cex = 0.5, # Set size of plot symbols to 0.5
     col = "steelblue", # Set the symbols color to "steelblue"
     # Set title and axis names
     xlab = expression(dummy_tam[i]), 
     ylab = "Test score", 
     main = "Dummy Regression")

# Estimate the dummy regression model
dummy_model <- lm(formula = testscr ~ dummy_tam, data = CASchools)
summary(dummy_model)

# Add group specific predictions to the plot
points(x = CASchools$dummy_tam, 
       y = predict(dummy_model), 
       col = "red", 
       pch = 20)

# Confidence intervals for coefficients in the dummy regression model
confint(dummy_model)
