# ========================================
# Cargar librerías necesarias
# ========================================
library(readxl)    # Para leer archivos Excel
library(openxlsx)  # Para guardar archivos Excel
library(MASS)      # Para la transformación Box-Cox
library(glmnet)    # Para Ridge y Lasso
library(tidyverse) # Para manipulación de datos
library(caret)     # Para partición y validación cruzada

# ========================================
# Definir rutas de archivos (Relativas para GitHub)
# ========================================
ruta_entrada <- "data/BaseExtendida.xlsx"
ruta_intermedia <- "data/BaseExtendida_BoxCox.xlsx"
ruta_final_xlsx <- "data/BaseExtendida_Normalizada.xlsx"
ruta_final_csv <- "data/BaseExtendida_Normalizada.csv"

# ========================================
# Paso 1: Carga, limpieza y transformación Box‑Cox
# ========================================
df <- read_excel(ruta_entrada)

# Eliminar filas con valores NA o negativos (se conservan filas con ceros)
df_clean <- df[apply(df, 1, function(x) all(!is.na(x) & as.numeric(x) >= 0)), ]
cat("\n🔹 Filas después de eliminar NA y negativos:", nrow(df_clean), "\n")

# Identificar las columnas numéricas
cols_num <- names(df_clean)[sapply(df_clean, is.numeric)]

# Aplicar Box‑Cox a cada columna numérica cuyos valores sean estrictamente > 0
for (col_name in cols_num) {
  if (all(df_clean[[col_name]] > 0, na.rm = TRUE)) {
    tryCatch({
      # Calcular el mejor lambda en el rango -2 a 2
      bc_result  <- boxcox(lm(df_clean[[col_name]] ~ 1), lambda = seq(-2, 2, 0.1))
      best_lambda <- bc_result$x[which.max(bc_result$y)]
      
      # Aplicar la transformación
      if (best_lambda == 0) {
        df_clean[[col_name]] <- log(df_clean[[col_name]])
      } else {
        df_clean[[col_name]] <- (df_clean[[col_name]]^best_lambda - 1) / best_lambda
      }
      cat("✅ Columna", col_name, "transformada con Box‑Cox. Lambda:", best_lambda, "\n")
    }, error = function(e) {
      cat("⚠️ Error en Box‑Cox para la columna", col_name, ":", e$message, "\n")
    })
  } else {
    cat("⚠️ Columna", col_name, "no transformada con Box‑Cox (contiene ceros)\n")
  }
}

# Guardar la base intermedia con Box‑Cox aplicada
write.xlsx(df_clean, ruta_intermedia, rowNames = FALSE)

# ========================================
# Paso 2: Normalización Z-score
# ========================================
df_boxcox <- read_excel(ruta_intermedia)

# Verificar si la base tiene datos antes de aplicar Z-score
if (nrow(df_boxcox) == 0) {
  stop("⚠️ Error: La base intermedia está vacía. Revisa si Box-Cox eliminó demasiadas filas.")
}

cols_num2 <- names(df_boxcox)[sapply(df_boxcox, is.numeric)]

# Aplicar Z-score solo si la columna tiene varianza distinta de 0
for (col_name in cols_num2) {
  std_dev <- sd(df_boxcox[[col_name]], na.rm = TRUE)
  if (std_dev > 1e-6) {  # Evita dividir por valores extremadamente pequeños
    df_boxcox[[col_name]] <- scale(df_boxcox[[col_name]])
  } else {
    cat("⚠️ Columna", col_name, "tiene varianza muy baja y no se normaliza.\n")
  }
}

# Guardar la base final en Excel y CSV
write.xlsx(df_boxcox, ruta_final_xlsx, rowNames = FALSE)
write.csv(df_boxcox, ruta_final_csv, row.names = FALSE)

cat("✅ Archivo final guardado en:", ruta_final_xlsx, "\n")

# ========================================
# Paso 3: División en conjunto de entrenamiento (70%) y prueba (30%)
# ========================================
set.seed(123)
train_index <- createDataPartition(df_boxcox$NO2Suelo, p = 0.7, list = FALSE)
train_data <- df_boxcox[train_index, ]
test_data <- df_boxcox[-train_index, ]

# Separar variables predictoras y respuesta
X_train <- as.matrix(train_data[, !names(train_data) %in% "NO2Suelo"])
y_train <- train_data$NO2Suelo
X_test <- as.matrix(test_data[, !names(test_data) %in% "NO2Suelo"])
y_test <- test_data$NO2Suelo

# ========================================
# Paso 4: Selección del lambda óptimo con Validación Cruzada K-Fold
# ========================================
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 10) # Ridge (L2)
lambda_ridge_opt <- cv_ridge$lambda.min 

cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 10) # Lasso (L1)
lambda_lasso_opt <- cv_lasso$lambda.min 

cat("\n🔹 Lambda óptimo (K-Fold) para Ridge:", lambda_ridge_opt, "\n")
cat("🔹 Lambda óptimo (K-Fold) para Lasso:", lambda_lasso_opt, "\n")

# ========================================
# Paso 5: Ajuste de modelos finales con los lambdas óptimos
# ========================================
modelo_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = lambda_ridge_opt)
modelo_lasso <- glmnet(X_train, y_train, alpha = 1, lambda = lambda_lasso_opt)

# ========================================
# Paso 6: Extracción de coeficientes en los valores óptimos de lambda
# ========================================
coef_ridge_opt <- coef(modelo_ridge, s = lambda_ridge_opt)
coef_lasso_opt <- coef(modelo_lasso, s = lambda_lasso_opt)

# Mostrar coeficientes de Ridge
cat("\n🔹 Coeficientes Ridge en el lambda óptimo:\n")
print(coef_ridge_opt)

# Mostrar coeficientes de Lasso
cat("\n🔹 Coeficientes Lasso en el lambda óptimo:\n")
print(coef_lasso_opt)

# ========================================
# Guardar coeficientes en archivos
# ========================================
ridge_coef_df <- as.data.frame(as.matrix(coef_ridge_opt))
lasso_coef_df <- as.data.frame(as.matrix(coef_lasso_opt))

write.xlsx(ridge_coef_df, "data/Coeficientes_Ridge.xlsx", rowNames = TRUE)
write.xlsx(lasso_coef_df, "data/Coeficientes_Lasso.xlsx", rowNames = TRUE)

cat("\n✅ Coeficientes guardados correctamente en Excel.\n")
