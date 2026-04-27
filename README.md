# Diabetes Health Indicators — Shiny App

## Descripción

Aplicación web interactiva desarrollada en R con Shiny para explorar los indicadores de salud relacionados con la diabetes en la población adulta de Estados Unidos. Permite filtrar por condición diabética y rango de IMC, actualizando todas las visualizaciones en tiempo real.

**Proyecto 2 — Herramientas y Visualización de Datos**
Fundación Universitaria Los Libertadores

---

## Dataset

- **Fuente:** Kaggle / CDC (Centers for Disease Control and Prevention)
- **URL:** https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset
- **Nombre:** Diabetes Health Indicators Dataset — BRFSS 2015
- **Descripción:** Encuesta telefónica aplicada por el CDC a adultos en EE.UU. durante 2015. Contiene 253,680 registros y 22 variables relacionadas con condiciones de salud, hábitos de vida e indicadores clínicos. La variable objetivo (`Diabetes_012`) clasifica a cada persona en: sin diabetes (0), prediabetes (1) o diabetes (2).

---

## Hallazgos Principales

1. **Subdiagnóstico de prediabetes:** Solo el 1.8% de los encuestados reporta prediabetes, lo que sugiere un alto nivel de subdiagnóstico en la población general estadounidense.

2. **BMI como factor de riesgo central:** La mediana del IMC en personas con diabetes (~31) es significativamente mayor que en personas sin diabetes (~27), confirmando la obesidad como factor de riesgo principal.

3. **Relación BMI — salud percibida:** Existe una tendencia positiva clara entre mayor IMC y peor salud general autopercibida en los tres grupos, con los casos de diabetes concentrados en la zona de mayor riesgo.

4. **Prevalencia creciente con la edad:** La proporción de personas con diabetes aumenta sostenidamente con la edad. A partir del grupo 60–64 años, los casos superan el 25% de los encuestados en ese rango.

5. **Hipertensión como comorbilidad dominante:** La correlación más fuerte con diabetes es la salud general percibida (0.33) seguida de hipertensión (0.30) y BMI (0.22). La actividad física muestra correlación negativa consistente.

---

## Visualizaciones Implementadas

1. **Gráfico de barras comparativo** — Distribución de encuestados por condición diabética (sin diabetes, prediabetes, diabetes) con conteos absolutos y porcentajes.

2. **Diagrama de caja (boxplot)** — Distribución del Índice de Masa Corporal (BMI) por grupo, con indicación de mediana, rango intercuartílico y media.

3. **Scatter plot** — Relación entre IMC y salud general autopercibida, con línea de regresión lineal e intervalo de confianza al 95% por grupo.

4. **Barras apiladas al 100%** — Composición de la condición diabética según grupo de edad (13 rangos etarios), mostrando la evolución de la prevalencia a lo largo del ciclo de vida.

5. **Mapa de calor (heatmap)** — Matriz de correlación de Pearson entre 13 indicadores de salud, con paleta divergente azul–blanco–rojo.

---

## Tecnologías Utilizadas

- **Framework:** Shiny
- **Lenguaje:** R 4.5.3
- **Bibliotecas:**
  - `shiny` — framework de aplicaciones web reactivas
  - `shinythemes` — tema visual flatly
  - `ggplot2` — visualizaciones estáticas
  - `dplyr` — manipulación de datos
  - `tidyr` — transformación de datos
  - `reshape2` — reestructuración para heatmap
  - `scales` — formato de ejes y etiquetas

---

## Instalación y Ejecución Local

### Requisitos Previos

- R >= 4.0.0
- RStudio (recomendado)

### Instrucciones

```bash
# Clonar repositorio
git clone https://github.com/TU_USUARIO/shiny-diabetes.git
cd shiny-diabetes
```

```r
# Instalar dependencias
install.packages(c("shiny", "shinythemes", "ggplot2", "dplyr",
                   "tidyr", "reshape2", "scales"))

# Ejecutar aplicación
shiny::runApp()
```

El dataset debe estar ubicado en `data/diabetes_012_health_indicators_BRFSS2015.csv`.

---

## Despliegue

**URL en producción:** https://aczino23lol.shinyapps.io/shiny-diabetes/

Desplegado en **shinyapps.io** (free tier) mediante el paquete `rsconnect`.

---

## Autores

- Carlos Muñoz
- [Juan Camilo]

**Curso:** Herramientas y Visualización de Datos
**Institución:** Fundación Universitaria Los Libertadores
**Año:** 2026
