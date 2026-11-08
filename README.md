# Contraste de Bondad de Ajuste de los Modelos Ridge y Lasso

Este repositorio contiene el código en **R** utilizado para el desarrollo de la tesis de grado titulada **"Contraste de bondad de ajuste de los modelos Ridge y Lasso"**, presentada en la Carrera de Ingeniería Matemática de la Universidad Central del Ecuador (UCE).

## 👤 Autor
* **Felipe Augusto Pucha Vargas**
* **Tutor:** MSc. Luis Cornelio Castillo Cabay
* **Institución:** Universidad Central del Ecuador - Facultad de Ciencias

## 📂 Estructura del Repositorio

El código está dividido en dos scripts principales para facilitar su lectura y ejecución:

* `01_Analisis_Principal.R`: Script principal que realiza el análisis comparativo entre Ridge y Lasso. Incluye análisis exploratorio, validación cruzada, diagnóstico de supuestos (OLS), evaluación de métricas (RMSE, MAE, AIC, BIC), selección clásica de variables (Forward/Backward) y evaluación de estabilidad mediante Bootstrap.
* `02_Modelo_Extendido.R`: Script correspondiente a la sección de "Trabajos Futuros". Implementa un modelo extendido con variables de interacción, aplicando previamente una transformación Box-Cox y normalización Z-score.
* `data/`: Carpeta que contiene los conjuntos de datos (archivos `.csv` y `.xlsx`) necesarios para ejecutar los scripts.

## 🛠️ Requisitos e Instalación

Para ejecutar este código, necesitas tener instalado **R** (versión 4.0 o superior) y **RStudio**. 

Las librerías necesarias se pueden instalar ejecutando el siguiente comando en la consola de R:

```R
install.packages(c("glmnet", "tidyverse", "caret", "MASS", "ggplot2", 
                   "lmtest", "car", "nortest", "corrplot", "readxl", "openxlsx"))
