# --------------- Load add-on packages ---------------
library(readxl)

# --------------- Load data ---------------
db_file_path <- "Escuela Colombiana de Ingeniería/Ciencias Económicas/Econometría/Databases/caschool/caschool.xlsx"
caschool <- read_xlsx(path = db_file_path)

# --------------- Gráfico de nube de puntos ---------------
scatter.smooth(caschool$str, caschool$testscr)

# --------------- Regresión lineal automática ---------------
aut_regress <- lm(formula = testscr ~ str, data = caschool)
summary(regress)

# Only coefficients
print(regress)

# --------------- Gráfico de nube de puntos (restringiendo los límites del eje y) ---------------
plot(caschool$str, caschool$testscr)

# Add OLS regression line
abline(regress)

# --------------- Regresión lineal manual ---------------

# Extract variables as a vectors
testscr_vector <- caschool$testscr
str_vector <- caschool$str

# Regression with vectors
vector_regression <- lm(testscr_vector ~ str_vector, data = caschool)
summary(vector_regression)

# Obtain predict values and residuals
testscr_hat <- fitted(vector_regression)
u_hat <- resid(vector_regression)

# Table results
cbind(testscr_vector, str_vector, testscr_hat, u_hat) # all
cbind(testscr_vector, str_vector, testscr_hat, u_hat)[1:20,]
