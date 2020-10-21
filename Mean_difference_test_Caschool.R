# --------------- Load add-on packages ---------------
library(readxl)

# --------------- Load data ---------------
db_file_path <- "Escuela Colombiana de Ingeniería/Ciencias Económicas/Econometría/Databases/caschool/caschool.xlsx"
caschool <- read_xlsx(path = db_file_path)

# --------------- Pregunta empírica ---------------
# Al reducir el tamaño de las clases, ¿mejora el rendimiento académico?

# --------------- Estadísticas descriptivas ---------------

# Promedio STR y TESTSCR
avg_str <- mean(caschool$str)
avg_testscr <- mean(caschool$testscr)

# Compute sample standard deviations of STR and TESTSCR
sd_str <- sd(caschool$str)
sd_testscr <- sd(caschool$testscr)

# Set up a vector of percentiles and compute the quantiles
quantiles <- c(0.1, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)
quant_str <- quantile(caschool$str, quantiles)
quant_testscr <- quantile(caschool$testscr, quantiles)

# Gather everything in a dataframe
summary_report <- data.frame(average = c(avg_str, avg_testscr), 
                             standard_deviation = c(sd_str, sd_testscr), 
                             quantiles = rbind(quant_str, quant_testscr))

# --------------- Print results ---------------

# Print the summary
summary_report

# Gráfico de nube de puntos
plot(caschool$testscr ~ caschool$str, 
     data = caschool, 
     main = "Scatterplot of TESTSCR and STR", 
     xlab = "STR (X)", 
     ylab = "TESTSCR (Y)")

# Correlación
cor(caschool$testscr, caschool$str)

# --------------- Clasificación por tamaño ---------------

# Crear variable dummy
caschool$small_course = caschool$str < 20

# Gráfico de TESTSCR by groups
plot(caschool$small_course, caschool$testscr, # Provide the data to be plotted
     pch = 20, # Use filled circles as plot symbols
     cex = 0.5, # Set size of plot symbols to 0.5
     col = "steelblue", # Set the symbols color to "Steelblue"
     # Set title and axis names
     xlab = expression(small_course[i]), 
     ylab = "TESTSCR", 
     main = "Dummy Regression")

# --------------- Prueba de diferencia de medias a dos colas ---------------

t.test(caschool$testscr ~ caschool$small_course)

# Regla de decisión:
# t = |-4.0426| > 1.96
# Se rechaza la hipótesis nula y se acepta la alterna
# Concluyendo que si existen diferencias estadísticamente significativas entre 
# los grupos pequeños y los grupos grandes
