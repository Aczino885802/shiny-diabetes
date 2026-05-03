# Diabetes Health Indicators — Shiny App

## Descripción

Aplicación web interactiva desarrollada en R con Shiny y Plotly para explorar los indicadores de salud relacionados con la diabetes en la población adulta de Estados Unidos. La app permite filtrar por condición diabética actualizando todas las visualizaciones en tiempo real, e incluye un bloque de **Hallazgos** dinámico debajo de cada gráfico que se recalcula automáticamente según la selección del usuario, proporcionando interpretaciones analíticas concretas calculadas directamente desde el dataset.

**Proyecto 2 — Herramientas y Visualización de Datos**
Fundación Universitaria Los Libertadores

---

## Dataset

- **Fuente:** Kaggle / CDC (Centers for Disease Control and Prevention)
- **URL:** https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset
- **Nombre:** Diabetes Health Indicators Dataset — BRFSS 2015
- **Descripción:** Encuesta telefónica aplicada por el CDC a adultos en EE.UU. durante 2015. Contiene 253,680 registros y 22 variables relacionadas con condiciones de salud, hábitos de vida, indicadores clínicos y nivel socioeconómico. La variable objetivo (`Diabetes_012`) clasifica a cada persona en: sin diabetes (0), prediabetes (1) o diabetes (2).

---

## Hallazgos Principales

1. **Hallazgo 1 — Acceso a salud condicionado por ingreso:** En el nivel de ingreso más bajo (<$10k anuales), el 30.4% de las personas no fue al médico por costo, mientras que en el nivel más alto (>$75k) solo el 4% reporta esta barrera. Aunque la cobertura de seguro va del 76% al 96% según el ingreso, tener seguro no equivale a poder pagar la atención. Este patrón explica parte del subdiagnóstico de prediabetes en la población vulnerable.

2. **Hallazgo 2 — BMI como factor de riesgo central:** La mediana del IMC en personas con diabetes (~31, zona de obesidad según OMS) es significativamente mayor que en personas sin diabetes (~27, zona de sobrepeso). Aplicando los puntos de corte clínicos de la OMS, la distribución del peso se desplaza progresivamente hacia la derecha conforme la condición se agrava, confirmando la obesidad como factor de riesgo principal.

3. **Hallazgo 3 — Hipertensión como comorbilidad dominante:** El 75.3% de las personas con diabetes también presenta hipertensión, frente al 37.1% en personas sin diabetes. La actividad física muestra el patrón inverso: 77.9% en sin diabetes vs 63.1% en diabetes. La diabetes no aparece aislada — viene acompañada de un paquete de comorbilidades cardiometabólicas que deben tratarse en conjunto.

4. **Hallazgo 4 — Prevalencia creciente con la edad:** La proporción de personas con diabetes aumenta sostenidamente con la edad. Entre los 50 y los 70 años la prevalencia casi se duplica, lo que define una franja crítica para programas de tamizaje preventivo. A partir de los 70 años, más del 25% de los encuestados reporta diabetes.

5. **Hallazgo 5 — Correlaciones clave entre indicadores:** Diabetes correlaciona principalmente con salud general percibida (0.30), hipertensión (0.27) y BMI (0.22). La actividad física muestra correlación negativa consistente (−0.12). Ningún factor por sí solo explica la diabetes — es el resultado de múltiples variables interconectadas, lo que confirma la necesidad de modelos multivariados para predecir el riesgo.

---

## Visualizaciones Implementadas

1. **Gráfico de barras + línea (Acceso a salud por ingreso):** cobertura de seguro de salud (`AnyHealthcare`) cruzada con el porcentaje de personas que no fueron al médico por costo (`NoDocbcCost`), agrupadas por los 8 niveles de ingreso del dataset.

2. **Histograma superpuesto con cortes OMS (Distribución del BMI):** distribución del IMC por condición diabética con líneas verticales que marcan los puntos de corte clínicos de la OMS (sobrepeso ≥ 25 y obesidad ≥ 30).

3. **Gráfico radar (Perfil de factores de riesgo):** prevalencia de 5 factores de riesgo (hipertensión, colesterol alto, tabaquismo, actividad física, enfermedad cardíaca) representada como polígono cerrado por condición diabética.

4. **Mapa de calor (Prevalencia por edad):** porcentaje de cada condición diabética en 13 grupos etarios, con paleta secuencial blanco–rojo que comunica la intensidad del fenómeno.

5. **Heatmap de correlaciones (Matriz Pearson):** matriz de correlación entre 9 indicadores de salud con paleta divergente rojo–blanco–azul (escala −1 a 1), permitiendo identificar relaciones positivas y negativas.

---

## Tecnologías Utilizadas

- **Framework:** Shiny (R)
- **Lenguaje:** R 4.x
- **Bibliotecas:**
  - `shiny` — framework de aplicaciones web interactivas
  - `shinythemes` — temas visuales (Flatly)
  - `plotly` — visualizaciones interactivas
  - `dplyr` — manipulación de datos
  - `tidyr` — transformación de datos
  - `ggplot2` — gráficos estáticos
  - `reshape2` — reestructuración de datos
  - `scales` — formateo de ejes y etiquetas

---

## Instalación y Ejecución Local

### Requisitos Previos

- R >= 4.0
- RStudio (recomendado)

### Instrucciones

```bash
# Clonar repositorio
git clone https://github.com/Aczino885802/shiny-diabetes.git
cd shiny-diabetes
```

Desde RStudio, instalar las dependencias:

```r
install.packages(c("shiny", "shinythemes", "plotly", "dplyr",
                   "tidyr", "ggplot2", "reshape2", "scales"))
```

Ejecutar la aplicación:

```r
shiny::runApp()
```

El dataset debe estar ubicado en `data/diabetes_012_health_indicators_BRFSS2015.csv`.

---

## Despliegue

**URL en producción:** https://aczino23lol.shinyapps.io/shiny-diabetes/

Desplegado en **shinyapps.io** mediante el paquete `rsconnect`. Para redesplegar después de cambios:

```r
rsconnect::deployApp()
```

---

## Autores

- Carlos Andrés Muñoz Arias
- Juan Camilo Girata Arango

**Curso:** Herramientas y Visualización de Datos
**Institución:** Fundación Universitaria Los Libertadores
**Año:** 2026