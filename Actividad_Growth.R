# --------------- Load add-on packages ---------------
library("readxl")

# --------------- Load data ---------------
db_file_path <- "Escuela Colombiana de Ingeniería/Ciencias Económicas/Econometría/Databases/growth/growth.xlsx"
growth <- read_xlsx(db_file_path)

# --------------- Actividad ---------------

# Gráfico de nube de puntos
summary(growth)

# Gráfico de nube de puntos con línea de tendencia
scatter.smooth(x = growth$growth, y = growth$tradeshare)

# Regresión lineal
growth_regress <- lm(formula = growth ~ tradeshare, data = growth)

# Gráfico de nube de puntos sin línea de tendencia
plot(x = growth$tradeshare, y = growth$growth)

# Add OLS regression line
abline(growth_regress)
print(growth_regress)
summary(growth_regress)

# --------------- Regresión con variable dummy ---------------

reg1 <- lm(growth ~ oil, data = growth)
summary(reg1)
# ¿Adjusted R-squared < 0?
# /.../Econometría/2020 - 2/R2(ajustado)_negativo.pdf

# i. Modelo múltiple
reg2 <- lm(formula = growth ~ oil + rgdp60 + tradeshare + yearsschool + 
             rev_coups + assasinations, 
           data = growth)
summary(reg2)
