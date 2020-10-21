# Escuela Colombiana de Ingeniería Julio Garavito
# Econometría 1 (ECM1) - Grupo 1
# Daniel Alejandro Clavijo
# Daniel Felipe Hernández


# Segundo corte - Taller 2


# 1.
library(haven)
eaef21 <- read_dta("Escuela Colombiana de Ingeniería/Ciencias Económicas/Econometría/Databases/eaef21/eaef21.dta")
model <- lm(S ~ ASVABC + SM + SF + MALE, eaef21)
summary(model)

# 2.
model <- lm(EARNINGS ~ S + AGE + I(AGE^2) + EDUCBA + EDUCMAST + EDUCPHD, 
            eaef21)
# 20 - 25
new_data <- data.frame(S=c(mean(eaef21$S), mean(eaef21$S)), 
                       AGE = c(20, 25), 
                       EDUCBA = c(mean(eaef21$EDUCBA), mean(eaef21$EDUCBA)), 
                       EDUCMAST = c(mean(eaef21$EDUCMAST), mean(eaef21$EDUCMAST)), 
                       EDUCPHD = c(mean(eaef21$EDUCPHD), mean(eaef21$EDUCPHD)))
Y_hat <- predict(model, newdata = new_data)
diff(Y_hat)
# 30 - 35
new_data <- data.frame(S=c(mean(eaef21$S), mean(eaef21$S)), 
                       AGE = c(30, 35), 
                       EDUCBA = c(mean(eaef21$EDUCBA), mean(eaef21$EDUCBA)), 
                       EDUCMAST = c(mean(eaef21$EDUCMAST), mean(eaef21$EDUCMAST)), 
                       EDUCPHD = c(mean(eaef21$EDUCPHD), mean(eaef21$EDUCPHD)))
Y_hat <- predict(model, newdata = new_data)
diff(Y_hat)
# 37 - 45
new_data <- data.frame(S=c(mean(eaef21$S), mean(eaef21$S)), 
                       AGE = c(37, 45), 
                       EDUCBA = c(mean(eaef21$EDUCBA), mean(eaef21$EDUCBA)), 
                       EDUCMAST = c(mean(eaef21$EDUCMAST), mean(eaef21$EDUCMAST)), 
                       EDUCPHD = c(mean(eaef21$EDUCPHD), mean(eaef21$EDUCPHD)))
Y_hat <- predict(model, newdata = new_data)
diff(Y_hat)

# 3.
model <- lm(EARNINGS ~ S + AGE + I(AGE^2) + I(AGE^3) + I(AGE^4) + EDUCBA + EDUCMAST + EDUCPHD, 
            eaef21)
# 20 - 25
new_data <- data.frame(S=c(mean(eaef21$S), mean(eaef21$S)), 
                       AGE = c(20, 25), 
                       EDUCBA = c(mean(eaef21$EDUCBA), mean(eaef21$EDUCBA)), 
                       EDUCMAST = c(mean(eaef21$EDUCMAST), mean(eaef21$EDUCMAST)), 
                       EDUCPHD = c(mean(eaef21$EDUCPHD), mean(eaef21$EDUCPHD)))
Y_hat <- predict(model, newdata = new_data)
diff(Y_hat)
# 30 - 35
new_data <- data.frame(S=c(mean(eaef21$S), mean(eaef21$S)), 
                       AGE = c(30, 35), 
                       EDUCBA = c(mean(eaef21$EDUCBA), mean(eaef21$EDUCBA)), 
                       EDUCMAST = c(mean(eaef21$EDUCMAST), mean(eaef21$EDUCMAST)), 
                       EDUCPHD = c(mean(eaef21$EDUCPHD), mean(eaef21$EDUCPHD)))
Y_hat <- predict(model, newdata = new_data)
diff(Y_hat)
# 37 - 45
new_data <- data.frame(S=c(mean(eaef21$S), mean(eaef21$S)), 
                       AGE = c(37, 45), 
                       EDUCBA = c(mean(eaef21$EDUCBA), mean(eaef21$EDUCBA)), 
                       EDUCMAST = c(mean(eaef21$EDUCMAST), mean(eaef21$EDUCMAST)), 
                       EDUCPHD = c(mean(eaef21$EDUCPHD), mean(eaef21$EDUCPHD)))
Y_hat <- predict(model, newdata = new_data)
diff(Y_hat)

# 4.
library(AER)
model <- lm(EARNINGS ~ poly(AGE, degree = 4, raw = TRUE), eaef21)
summary(model)
model <- lm(EARNINGS ~ poly(AGE, degree = 3, raw = TRUE), eaef21)
summary(model)
model <- lm(EARNINGS ~ poly(AGE, degree = 2, raw = TRUE), eaef21)
summary(model)
# Prueba t con errores estándar robustos (simple)
coeftest(model, vcov. = vcovHC, type = "HC1")

# Prueba f con errores estándar robustos (conjunta)
# matriz de coeficientes de la hipótesis alterna
# (al menos uno distinto de cero)
R <- rbind(c(0, 0, 1, 0, 0),
           c(0, 0, 0, 1, 0), 
           c(0, 0, 0, 0, 1))
# test
linearHypothesis(model,
                 hypothesis.matrix = R, 
                 white.adjust = "hc1")
model <- lm(EARNINGS ~ AGE, eaef21)
summary(model)

# 9.
model  <- lm(log(EARNINGS) ~ S + EXP + ETHBLACK + ETHHISP, eaef21)
summary(model)
