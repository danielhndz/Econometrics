# --------------- Load add-on packages ---------------
library("haven")
library("AER")

# --------------- Load data ---------------
db_file_path <- "Escuela Colombiana de Ingeniería/Ciencias Económicas/Econometría/Databases/wooldridge_3e/statafiles/CEOSAL2.DTA"
CEOSAL2 <- read_dta(file = db_file_path)

# --------------- Taller 1 - C2.2 ---------------

# Sueldo promedio
avg_salary <- mean(CEOSAL2$salary)
avg_salary

# Antigüedad promedio
avg_comten <- mean(CEOSAL2$comten)
avg_comten

# CEOs que están en su primer año
new_ceos <- sum(CEOSAL2$ceoten == 0)
new_ceos

# Mayor antiguedad entre los CEOs
max_ceoten <- max(CEOSAL2$ceoten)
max_ceoten

# Regresión log(salary) - ceoten
regression <- lm(formula = lsalary ~ ceoten, data = CEOSAL2)
summary(regression)

# --------------- Regresión lineal manual ---------------

# Extract variables as a vectors
lsalary_vector <- CEOSAL2$lsalary
ceoten_vector <- CEOSAL2$ceoten

# Regression with vectors
vector_regression <- lm(formula = lsalary_vector ~ ceoten_vector, data = CEOSAL2)
summary(vector_regression)

# Obtain predict values and residuals
lsalary_hat <- fitted(vector_regression)
lsalary_hat

u_hat <- resid(vector_regression)
u_hat
