# Contraste de Bondad de Ajuste de los Modelos Ridge y Lasso

Este repositorio contiene el código en **R** y los conjuntos de datos utilizados para el desarrollo de la tesis de grado titulada **"Contraste de bondad de ajuste de los modelos Ridge y Lasso"**, presentada en la Carrera de Ingeniería Matemática de la Universidad Central del Ecuador (UCE).

## 👤 Autor
* **Felipe Augusto Pucha Vargas**
* **Tutor:** MSc. Luis Cornelio Castillo Cabay
* **Institución:** Universidad Central del Ecuador - Facultad de Ciencias

## 📂 Estructura del Repositorio

Todos los archivos se encuentran en la raíz del proyecto y están divididos de la siguiente manera:

**Scripts de R:**
* `Bondad_de_ajuste_Ridge_y_Lasso.R`: Script principal. Realiza el análisis exploratorio, la validación cruzada para la selección de \(\lambda\), la evaluación de supuestos del modelo clásico (OLS), el cálculo de métricas predictivas y la estabilidad mediante Bootstrap.
* `Modelo_extendido.R`: Script correspondiente a la sección de "Trabajos Futuros". Implementa un modelo con variables de interacción, aplicando previamente una transformación Box-Cox y normalización Z-score.

**Bases de Datos:**
* `BaseModelo_estandarizada.csv`: Conjunto de datos principal utilizado para los modelos Ridge y Lasso.
* `BaseModelo_preprocessed.csv`: Conjunto de datos simplificado para el diagnóstico de supuestos del modelo OLS.
* `BaseExtendida.xlsx`: Base de datos que incluye la variable de satélite y las interacciones para el modelo extendido.

## 🛠️ Requisitos e Instalación

Para ejecutar este código, necesitas tener instalado **R** (versión 4.0 o superior) y **RStudio**. 

Las librerías necesarias se pueden instalar ejecutando el siguiente comando en la consola de R:

```R
install.packages(c("glmnet", "tidyverse", "caret", "MASS", "ggplot2", 
                   "lmtest", "car", "nortest", "corrplot", "readxl", "openxlsx"))
