# Contraste de Bondad de Ajuste de los Modelos Ridge y Lasso

Este repositorio contiene el código en **R** utilizado para el desarrollo de la tesis de grado titulada **"Contraste de bondad de ajuste de los modelos Ridge y Lasso"**, presentada en la Carrera de Ingeniería Matemática de la Universidad Central del Ecuador (UCE).

## 👤 Autor
* **Felipe Augusto Pucha Vargas**
* **Tutor:** MSc. Luis Cornelio Castillo Cabay
* **Institución:** Universidad Central del Ecuador - Facultad de Ciencias

## 📂 Estructura del Repositorio

El repositorio cuenta con los siguientes archivos:

* `Bondad de ajuste Ridge y Lasso`: Script principal que realiza el análisis comparativo entre Ridge y Lasso (análisis exploratorio, validación cruzada, diagnóstico de supuestos, métricas y Bootstrap).
* `Modelo extendido`: Script correspondiente a la sección de "Trabajos Futuros". Implementa un modelo con variables de interacción, transformación Box-Cox y normalización.
* `BaseModelo_estandarizada.csv`: Conjunto de datos base utilizado para el análisis principal.
* `README.md`: Este archivo de instrucciones.

## 🛠️ Requisitos e Instalación

Para ejecutar este código, necesitas tener instalado **R** (versión 4.0 o superior) y **RStudio**. 

Las librerías necesarias se pueden instalar ejecutando el siguiente comando en la consola de R:

```R
install.packages(c("glmnet", "tidyverse", "caret", "MASS", "ggplot2", 
                   "lmtest", "car", "nortest", "corrplot", "readxl", "openxlsx"))
