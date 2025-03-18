# ==============================================================================
# Tesis: Contraste de Bondad de Ajuste de los Modelos Ridge y Lasso
# Autor: Felipe Augusto Pucha Vargas
# Tutor: Luis Cornelio Castillo Cabay
# Universidad Central del Ecuador - 2025
# ==============================================================================

# ------------------------------------------------------------------------------
# Bloque 1: Configuración y Carga de Librerías
# ------------------------------------------------------------------------------
library(glmnet)    # Para Ridge y Lasso
library(tidyverse) # Para manipulación y visualización de datos
library(caret)     # Para división de datos y validación cruzada
library(MASS)      # Para la función stepAIC en selección de variables
library(ggplot2)   # Para visualización de gráficos
library(lmtest)    # Para pruebas de Durbin-Watson y Breusch-Pagan
library(car)       # Para calcular VIF
library(nortest)   # Para pruebas de normalidad
library(corrplot)  # Para visualizar la matriz de correlación

# ------------------------------------------------------------------------------
# Bloque 2: Carga de Datos y Preparación de la Matriz de Variables
# ------------------------------------------------------------------------------
ruta_csv <- "C:/Users/felip/Desktop/Tesis/Metodologia/Base de datos/BaseModelo_estandarizada.csv"
datos <- read.csv(ruta_csv, sep = ",", header = TRUE)

# Seleccionar la variable dependiente y las variables predictoras
vars_indep <- c("DIR", "HUM", "IUV", "LLU", "PRE", "RS", "TMP", "VEL")
X <- as.matrix(datos[, vars_indep])
y <- datos$NO2Suelo  # Variable respuesta

# ------------------------------------------------------------------------------
# Bloque 3: División de Datos en Conjunto de Entrenamiento y Prueba
# ------------------------------------------------------------------------------
set.seed(123)  # Para asegurar la reproducibilidad
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Crear el data frame de entrenamiento para métodos clásicos
train <- datos[train_index, ]

# ------------------------------------------------------------------------------
# Bloque 4: Validación Cruzada para Selección de Lambda Óptimo (Ridge y Lasso)
# ------------------------------------------------------------------------------
# Ridge (penalización L2)
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 10)
lambda_ridge_opt <- cv_ridge$lambda.min

# Lasso (penalización L1)
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10)
lambda_lasso_opt <- cv_lasso$lambda.min

print(paste("Lambda óptimo para Ridge:", lambda_ridge_opt))
print(paste("Lambda óptimo para Lasso:", lambda_lasso_opt))

# ------------------------------------------------------------------------------
# Bloque 5: Ajuste de Modelos Finales y Evaluación del Desempeño
# ------------------------------------------------------------------------------
modelo_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_ridge_opt)
modelo_lasso <- glmnet(X_train, y_train, alpha = 1, lambda = lambda_lasso_opt)

# Predicciones en el conjunto de prueba
pred_ridge_test <- predict(modelo_ridge, newx = X_test, s = lambda_ridge_opt)
pred_lasso_test <- predict(modelo_lasso, newx = X_test, s = lambda_lasso_opt)

# Evaluación del desempeño (MSE)
mse_ridge <- mean((y_test - pred_ridge_test)^2)
mse_lasso <- mean((y_test - pred_lasso_test)^2)
print(paste("MSE en prueba para Ridge:", mse_ridge))
print(paste("MSE en prueba para Lasso:", mse_lasso))

# ------------------------------------------------------------------------------
# Bloque 6: Visualización de Trayectorias de Coeficientes
# ------------------------------------------------------------------------------
lambdas <- 10^seq(-4, 2, length = 100)
ridge_fit <- glmnet(X_train, y_train, alpha = 0, lambda = lambdas)
lasso_fit <- glmnet(X_train, y_train, alpha = 1, lambda = lambdas)
par(mfrow = c(1, 2))
plot(ridge_fit, xvar = "lambda", label = TRUE, 
     main = "Trayectoria de Coeficientes - Ridge", 
     xlab = expression("Log(" * lambda * ")"), ylab = "Coeficientes")
abline(v = log(lambda_ridge_opt), col = "red", lty = 2)
plot(lasso_fit, xvar = "lambda", label = TRUE, 
     main = "Trayectoria de Coeficientes - Lasso", 
     xlab = expression("Log(" * lambda * ")"), ylab = "Coeficientes")
abline(v = log(lambda_lasso_opt), col = "red", lty = 2)

# Extraer y mostrar coeficientes en los lambda óptimos
coef_ridge_opt <- predict(ridge_fit, s = lambda_ridge_opt, type = "coefficients")
coef_lasso_opt <- predict(lasso_fit, s = lambda_lasso_opt, type = "coefficients")
print("Coeficientes Ridge en el lambda óptimo:")
print(coef_ridge_opt)
print("Coeficientes Lasso en el lambda óptimo:")
print(coef_lasso_opt)

# ------------------------------------------------------------------------------
# Bloque 7: Evaluación de Supuestos del Modelo Clásico (OLS)
# ------------------------------------------------------------------------------
file_path <- "C:/Users/felip/Desktop/Tesis/Secciones/Contraste de Bondad de Ajuste/Base de datos simplificada/BaseModelo_preprocessed.csv"
df <- read.csv(file_path)
modelo <- lm(NO2Suelo ~ ., data = df)
summary(modelo)

# 1. Linealidad: Gráfico de residuos vs predicciones
residuos <- residuals(modelo)
predicciones <- fitted(modelo)
ggplot(data = data.frame(predicciones, residuos), aes(x = predicciones, y = residuos)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, col = "red") +
  ggtitle("Residuos vs Predicciones") +
  xlab("Predicciones") +
  ylab("Residuos")

# 2. Independencia: Test de Durbin-Watson
dw_test_result <- dwtest(modelo)
print(dw_test_result)

# 3. Homoscedasticidad: Test de Breusch-Pagan
bp_test_result <- bptest(modelo)
print(bp_test_result)

# 4. Normalidad: Test de Shapiro-Wilk y gráfico Q-Q
sw_test_result <- shapiro.test(residuos)
print(sw_test_result)
qqnorm(residuos, main = "Gráfico Q-Q Normal", xlab = "Cuantiles teóricos", ylab = "Cuantiles de la muestra")
qqline(residuos, col = "red")

# 5. Multicolinealidad: Cálculo de VIF
vif_values <- vif(modelo)
print("Valores de VIF:")
print(vif_values)

# 6. Matriz de correlación y visualización
cor_matrix <- cor(df)
corrplot(cor_matrix, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200), tl.cex = 0.8)

# ------------------------------------------------------------------------------
# Bloque 8: Definición de Funciones para Cálculo de Métricas de Desempeño
# ------------------------------------------------------------------------------
rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2))
}
mae <- function(obs, pred) {
  mean(abs(obs - pred))
}
r2_ajustado <- function(obs, pred, p) {
  n <- length(obs)
  rss <- sum((obs - pred)^2)
  tss <- sum((obs - mean(obs))^2)
  r2 <- 1 - rss/tss
  1 - ((1 - r2) * (n - 1))/(n - p - 1)
}

# Métricas para el Modelo Ridge
pred_ridge_test <- predict(modelo_ridge, newx = X_test, s = lambda_ridge_opt)
rmse_ridge <- rmse(y_test, pred_ridge_test)
mae_ridge <- mae(y_test, pred_ridge_test)
r2adj_ridge <- r2_ajustado(y_test, pred_ridge_test, p = length(vars_indep))
cat("Ridge - RMSE:", rmse_ridge, "\n")
cat("Ridge - MAE:", mae_ridge, "\n")
cat("Ridge - R^2 ajustado:", r2adj_ridge, "\n")

# Cálculo de métricas de complejidad para Ridge
pred_ridge_train <- predict(modelo_ridge, newx = X_train, s = lambda_ridge_opt)
RSS_ridge <- sum((y_train - pred_ridge_train)^2)
n_train <- length(y_train)
p <- ncol(X_train)
I_p <- diag(p)
inv_ridge <- solve(t(X_train) %*% X_train + lambda_ridge_opt * I_p)
Hat_ridge <- X_train %*% inv_ridge %*% t(X_train)
df_ridge <- sum(diag(Hat_ridge))
sigma2_ridge <- RSS_ridge / n_train
AIC_ridge <- n_train * log(sigma2_ridge) + 2 * df_ridge
BIC_ridge <- n_train * log(sigma2_ridge) + log(n_train) * df_ridge
cat("Ridge - AIC:", AIC_ridge, "\n")
cat("Ridge - BIC:", BIC_ridge, "\n")

# Métricas para el Modelo Lasso
pred_lasso_test <- predict(modelo_lasso, newx = X_test, s = lambda_lasso_opt)
rmse_lasso <- rmse(y_test, pred_lasso_test)
mae_lasso <- mae(y_test, pred_lasso_test)
r2adj_lasso <- r2_ajustado(y_test, pred_lasso_test, p = length(vars_indep))
cat("Lasso - RMSE:", rmse_lasso, "\n")
cat("Lasso - MAE:", mae_lasso, "\n")
cat("Lasso - R^2 ajustado:", r2adj_lasso, "\n")

# Cálculo de métricas de complejidad para Lasso
pred_lasso_train <- predict(modelo_lasso, newx = X_train, s = lambda_lasso_opt)
RSS_lasso <- sum((y_train - pred_lasso_train)^2)
coef_lasso <- as.vector(coef_lasso_opt)
df_lasso <- sum(coef_lasso != 0) - 1  
sigma2_lasso <- RSS_lasso / n_train
AIC_lasso <- n_train * log(sigma2_lasso) + 2 * df_lasso
BIC_lasso <- n_train * log(sigma2_lasso) + log(n_train) * df_lasso
cat("Lasso - AIC:", AIC_lasso, "\n")
cat("Lasso - BIC:", BIC_lasso, "\n")

# ------------------------------------------------------------------------------
# Bloque 9: Selección de Variables (Métodos Clásicos y Bootstrap)
# ------------------------------------------------------------------------------
# Definir fórmula completa para métodos clásicos
full_formula <- as.formula(paste("NO2Suelo ~", paste(vars_indep, collapse = " + ")))

# Ajuste de modelos clásicos en el conjunto de entrenamiento
full_model <- lm(full_formula, data = train)
null_model <- lm(NO2Suelo ~ 1, data = train)

# Forward Selection: desde modelo nulo a modelo completo
forward_model <- stepAIC(null_model, scope = list(lower = null_model, upper = full_model),
                         direction = "forward", trace = FALSE)
cat("\nVariables seleccionadas en Forward Selection:\n")
print(names(coef(forward_model))[-1])
cat("AIC del modelo Forward:", AIC(forward_model), "\n")

# Backward Selection: a partir del modelo completo eliminando variables
backward_model <- stepAIC(full_model, direction = "backward", trace = FALSE)
cat("\nVariables seleccionadas en Backward Selection:\n")
print(names(coef(backward_model))[-1])
cat("AIC del modelo Backward:", AIC(backward_model), "\n")

# Ajuste de Lasso en el conjunto de entrenamiento
X_train <- as.matrix(train[, vars_indep])
y_train <- train$NO2Suelo
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, lambda = lambdas, nfolds = 10)
lambda_lasso_opt <- cv_lasso$lambda.min
lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = lambda_lasso_opt)
lasso_coef <- predict(lasso_model, s = lambda_lasso_opt, type = "coefficients")
lasso_selected <- setdiff(rownames(lasso_coef)[lasso_coef[,1] != 0], "(Intercept)")

# Ajuste de Ridge en el conjunto de entrenamiento
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, lambda = lambdas, nfolds = 10)
lambda_ridge_opt <- cv_ridge$lambda.min
ridge_model <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_ridge_opt)
ridge_coef <- predict(ridge_model, s = lambda_ridge_opt, type = "coefficients")
umbral <- 1e-3  # Umbral para considerar la variable "seleccionada" en Ridge
ridge_selected <- setdiff(rownames(ridge_coef)[abs(ridge_coef[,1]) > umbral], "(Intercept)")

cat("\nComparación de Variables Seleccionadas:\n")
cat("Forward Selection: ", paste(names(coef(forward_model))[-1], collapse = ", "), "\n")
cat("Backward Selection:", paste(names(coef(backward_model))[-1], collapse = ", "), "\n")
cat("Lasso Selection:   ", paste(lasso_selected, collapse = ", "), "\n")
cat("Ridge Selection:   ", paste(ridge_selected, collapse = ", "), "\n")
summary(forward_model)
summary(backward_model)

# Obtener coeficientes y p-valores para Forward Selection
summary_forward <- summary(forward_model)
coef_forward <- summary_forward$coefficients
cat("\nCoeficientes y p-valores de Forward Selection:\n")
print(coef_forward)

# Obtener coeficientes y p-valores para Backward Selection
summary_backward <- summary(backward_model)
coef_backward <- summary_backward$coefficients
cat("\nCoeficientes y p-valores de Backward Selection:\n")
print(coef_backward)

# Extraer coeficientes de Ridge y Lasso en el lambda óptimo
ridge_coefs <- as.matrix(coef(ridge_model, s = lambda_ridge_opt))
lasso_coefs <- as.matrix(coef(lasso_model, s = lambda_lasso_opt))
cat("\nCoeficientes del modelo Ridge en el lambda óptimo:\n")
print(ridge_coefs)
cat("\nCoeficientes del modelo Lasso en el lambda óptimo:\n")
print(lasso_coefs)

# Estimar p-valores para Ridge y Lasso (mediante ajuste de modelos lm() con variables seleccionadas)
ridge_model_lm <- lm(NO2Suelo ~ ., data = train[, c(ridge_selected, "NO2Suelo")])
lasso_model_lm <- lm(NO2Suelo ~ ., data = train[, c(lasso_selected, "NO2Suelo")])
summary_ridge <- summary(ridge_model_lm)
summary_lasso <- summary(lasso_model_lm)
coef_ridge_lm <- summary_ridge$coefficients
coef_lasso_lm <- summary_lasso$coefficients
cat("\nCoeficientes y p-valores del modelo Ridge:\n")
print(coef_ridge_lm)
cat("\nCoeficientes y p-valores del modelo Lasso:\n")
print(coef_lasso_lm)

# ------------------------------------------------------------------------------
# Bloque 10: Evaluación de Estabilidad mediante Bootstrap y Comparación de Selección
# ------------------------------------------------------------------------------
set.seed(123)  # Para reproducibilidad
R <- 100  # Número de replicaciones bootstrap
forward_sel_list <- list()
backward_sel_list <- list()
lasso_sel_list <- list()
ridge_sel_list <- list()

for(i in 1:R) {
  indices <- sample(seq_len(nrow(train)), replace = TRUE)
  boot_sample <- train[indices, ]
  
  # Forward Selection en la muestra bootstrap
  null_model_boot <- lm(NO2Suelo ~ 1, data = boot_sample)
  full_model_boot <- lm(full_formula, data = boot_sample)
  forward_model_boot <- stepAIC(null_model_boot, scope = list(lower = null_model_boot, upper = full_model_boot),
                                direction = "forward", trace = FALSE)
  forward_sel_list[[i]] <- names(coef(forward_model_boot))[-1]
  
  # Backward Selection en la muestra bootstrap
  backward_model_boot <- stepAIC(full_model_boot, direction = "backward", trace = FALSE)
  backward_sel_list[[i]] <- names(coef(backward_model_boot))[-1]
  
  # Lasso Selection en la muestra bootstrap
  X_boot <- as.matrix(boot_sample[, vars_indep])
  y_boot <- boot_sample$NO2Suelo
  cv_lasso_boot <- cv.glmnet(X_boot, y_boot, alpha = 1, lambda = lambdas, nfolds = 5)
  lambda_lasso_boot <- cv_lasso_boot$lambda.min
  lasso_model_boot <- glmnet(X_boot, y_boot, alpha = 1, lambda = lambda_lasso_boot, max_iter = 10000)
  lasso_coef_boot <- coef(lasso_model_boot)
  sel_vars_lasso <- setdiff(rownames(lasso_coef_boot)[lasso_coef_boot[,1] != 0], "(Intercept)")
  lasso_sel_list[[i]] <- sel_vars_lasso
  
  # Ridge Selection en la muestra bootstrap
  cv_ridge_boot <- cv.glmnet(X_boot, y_boot, alpha = 0, lambda = lambdas, nfolds = 5)
  lambda_ridge_boot <- cv_ridge_boot$lambda.min
  ridge_model_boot <- glmnet(X_boot, y_boot, alpha = 0, lambda = lambda_ridge_boot, max_iter = 10000)
  ridge_coef_boot <- coef(ridge_model_boot)
  sel_vars_ridge <- setdiff(rownames(ridge_coef_boot)[abs(ridge_coef_boot[,1]) > umbral], "(Intercept)")
  ridge_sel_list[[i]] <- sel_vars_ridge
}

# Función para contar la frecuencia de selección de cada variable
count_selection <- function(selection_list, variables) {
  sapply(variables, function(v) {
    mean(sapply(selection_list, function(sel) v %in% sel))
  })
}

all_vars <- vars_indep
forward_freq <- count_selection(forward_sel_list, all_vars)
backward_freq <- count_selection(backward_sel_list, all_vars)
lasso_freq <- count_selection(lasso_sel_list, all_vars)
ridge_freq <- count_selection(ridge_sel_list, all_vars)

selection_freq <- data.frame(
  Variable = all_vars,
  Forward = round(forward_freq * 100, 1),
  Backward = round(backward_freq * 100, 1),
  Lasso = round(lasso_freq * 100, 1),
  Ridge = round(ridge_freq * 100, 1)
)

cat("\nFrecuencia de selección (en %) a través de Bootstrap:\n")
print(selection_freq)
