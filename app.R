# ============================================================
# Diabetes Health Indicators — BRFSS 2015
# Herramientas y Visualización de Datos — Proyecto 2
# Shiny App | R
# ============================================================

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)

# ── Datos ────────────────────────────────────────────────────
df_raw <- read.csv("data/diabetes_012_health_indicators_BRFSS2015.csv")

df_raw$Diabetes_Label <- factor(
  df_raw$Diabetes_012,
  levels = c(0, 1, 2),
  labels = c("Sin Diabetes", "Prediabetes", "Diabetes")
)

# Paleta cualitativa accesible — igual que D3.js
PAL <- c("Sin Diabetes" = "#4DAF4A",
         "Prediabetes"  = "#FF7F00",
         "Diabetes"     = "#E41A1C")

# ── UI ───────────────────────────────────────────────────────
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, sans-serif; }

    .titulo-app {
      padding: 18px 24px 14px;
      border-bottom: 3px solid #E41A1C;
      margin-bottom: 22px;
    }
    .titulo-app h3 {
      margin: 0 0 4px;
      font-size: 20px;
      font-weight: 700;
      color: #1a1a2e;
    }
    .titulo-app p {
      margin: 0;
      font-size: 12px;
      color: #6c757d;
    }

    .panel-control {
      background: #f8f9fa;
      border: 1px solid #dee2e6;
      border-radius: 6px;
      padding: 16px;
    }
    .panel-control h5 {
      font-size: 11px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.8px;
      color: #718096;
      margin-bottom: 14px;
    }

    .insight {
      background: #fffbeb;
      border-left: 4px solid #f6ad55;
      padding: 9px 13px;
      margin-bottom: 14px;
      font-size: 13px;
      color: #3d3d3d;
      border-radius: 0 4px 4px 0;
    }
    .insight strong { color: #c05621; }

    .nav-tabs > li > a {
      font-size: 13px;
      font-weight: 600;
      color: #495057;
    }
    .nav-tabs > li.active > a {
      color: #E41A1C !important;
      border-top: 3px solid #E41A1C !important;
    }

    .fuente {
      font-size: 11px;
      color: #adb5bd;
      margin-top: 6px;
      text-align: right;
    }
  "))),
  
  # Encabezado
  div(class = "titulo-app",
      h3("Indicadores de Salud y Riesgo de Diabetes — EE.UU. 2015"),
      p("Behavioral Risk Factor Surveillance System (BRFSS) · CDC · n = 253,680 · 22 variables")
  ),
  
  fluidRow(
    # ── Panel de control ──────────────────────────────────────
    column(3,
           div(class = "panel-control",
               h5("Filtros"),
               
               checkboxGroupInput(
                 "sel_grupo", "Condicion diabetica:",
                 choices  = c("Sin Diabetes", "Prediabetes", "Diabetes"),
                 selected = c("Sin Diabetes", "Prediabetes", "Diabetes")
               ),
               
               hr(style = "margin: 12px 0;"),
               
               p(style = "font-size:11px; color:#868e96; margin:0;",
                 "Los filtros aplican a todas las visualizaciones excepto el mapa de correlaciones.")
           )
    ),
    
    # ── Visualizaciones ───────────────────────────────────────
    column(9,
           tabsetPanel(
             
             # Viz 1 — Barras: Comparacion entre grupos
             tabPanel("Comparacion por grupo",
                      br(),
                      div(class = "insight",
                          strong("Hallazgo:"),
                          " El 84.2% de los encuestados no tiene diabetes. La prediabetes
             esta muy subrepresentada (1.8%), lo que puede reflejar
             subdiagnostico en la poblacion general."
                      ),
                      plotOutput("viz_barras", height = "400px"),
                      p(class = "fuente", "Fuente: CDC BRFSS 2015 — Kaggle")
             ),
             
             # Viz 2 — Boxplot: Distribucion BMI
             tabPanel("Distribucion de BMI",
                      br(),
                      div(class = "insight",
                          strong("Hallazgo:"),
                          " La mediana del BMI en personas con diabetes (~31) es
             significativamente mayor que en personas sin diabetes (~27).
             El sobrepeso y la obesidad emergen como factores de riesgo centrales."
                      ),
                      plotOutput("viz_boxplot", height = "400px"),
                      p(class = "fuente", "Fuente: CDC BRFSS 2015 — Kaggle")
             ),
             
             # Viz 3 — Barras agrupadas: Factores de riesgo
             tabPanel("Factores de riesgo",
                      br(),
                      div(class = "insight",
                          strong("Hallazgo:"),
                          " La hipertension (75.3%) y el colesterol alto (67%) son los factores
             mas prevalentes en personas con diabetes. La actividad fisica es
             notablemente menor en ese grupo."
                      ),
                      plotOutput("viz_factores", height = "420px"),
                      p(class = "fuente", "Fuente: CDC BRFSS 2015 — Kaggle")
             ),
             
             # Viz 4 — Barras apiladas: Prevalencia por edad
             tabPanel("Prevalencia por edad",
                      br(),
                      div(class = "insight",
                          strong("Hallazgo:"),
                          " La proporcion de personas con diabetes crece sostenidamente
             con la edad. A partir del grupo 60-64 anos, los casos superan
             el 25% de los encuestados."
                      ),
                      plotOutput("viz_edad", height = "420px"),
                      p(class = "fuente", "Fuente: CDC BRFSS 2015 — Kaggle")
             ),
             
             # Viz 5 — Heatmap: Correlaciones
             tabPanel("Mapa de correlaciones",
                      br(),
                      div(class = "insight",
                          strong("Hallazgo:"),
                          " Diabetes correlaciona principalmente con hipertension (0.27),
             salud general percibida (0.30) y BMI (0.22). La actividad
             fisica muestra correlacion negativa consistente."
                      ),
                      plotOutput("viz_heatmap", height = "480px"),
                      p(class = "fuente", "Fuente: CDC BRFSS 2015 — Kaggle")
             )
             
           ) # fin tabsetPanel
    )
  ) # fin fluidRow
)

# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Dataset filtrado reactivo
  df <- reactive({
    df_raw %>%
      filter(Diabetes_Label %in% input$sel_grupo)
  })
  
  # ── Viz 1: Barras comparativas ────────────────────────────
  output$viz_barras <- renderPlot({
    datos <- df() %>%
      count(Diabetes_Label) %>%
      mutate(
        pct   = n / sum(n),
        label = paste0(format(n, big.mark = ","), "\n", percent(pct, accuracy = 0.1))
      )
    
    ggplot(datos, aes(x = reorder(Diabetes_Label, -n), y = n, fill = Diabetes_Label)) +
      geom_col(width = 0.55, show.legend = FALSE) +
      geom_text(aes(label = label), vjust = -0.35, size = 3.8, lineheight = 1.3,
                fontface = "bold", color = "#2d2d2d") +
      scale_fill_manual(values = PAL) +
      scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
      labs(
        title    = "Distribucion de encuestados segun condicion diabetica",
        subtitle = "La mayoria de encuestados no reporta diabetes; la prediabetes aparece subrepresentada",
        x = NULL, y = "Numero de personas"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title         = element_text(face = "bold", size = 14, color = "#1a1a2e"),
        plot.subtitle      = element_text(size = 11, color = "#6c757d", margin = margin(b = 12)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(face = "bold", size = 12),
        axis.text.y        = element_text(color = "#6c757d"),
        plot.margin        = margin(10, 20, 10, 10)
      )
  })
  
  # ── Viz 2: Boxplot BMI ────────────────────────────────────
  output$viz_boxplot <- renderPlot({
    ggplot(df(), aes(x = Diabetes_Label, y = BMI, fill = Diabetes_Label)) +
      geom_boxplot(
        outlier.shape = 16, outlier.alpha = 0.08,
        outlier.size  = 0.6, width = 0.45, show.legend = FALSE
      ) +
      stat_summary(fun = mean, geom = "point",
                   shape = 23, size = 3.5, fill = "white", color = "#2d2d2d") +
      scale_fill_manual(values = PAL) +
      scale_y_continuous(limits = c(10, 70), breaks = seq(10, 70, 10)) +
      labs(
        title    = "Distribucion del IMC (BMI) segun condicion diabetica",
        subtitle = "El rombo blanco indica la media. Las cajas muestran el rango intercuartilico (Q1-Q3).",
        x = NULL, y = "Indice de Masa Corporal (BMI)"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title         = element_text(face = "bold", size = 14, color = "#1a1a2e"),
        plot.subtitle      = element_text(size = 11, color = "#6c757d", margin = margin(b = 12)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(face = "bold", size = 12),
        plot.margin        = margin(10, 20, 10, 10)
      )
  })
  
  # ── Viz 3: Barras agrupadas factores de riesgo ────────────
  output$viz_factores <- renderPlot({
    factores    <- c("HighBP","HighChol","Smoker","PhysActivity","HeartDiseaseorAttack")
    etiquetas   <- c("Hipertension","Col. alto","Fumador","Act. fisica","Enf. cardiaca")
    
    datos_long <- df() %>%
      group_by(Diabetes_Label) %>%
      summarise(across(all_of(factores), mean), .groups = "drop") %>%
      pivot_longer(cols = all_of(factores),
                   names_to = "Factor", values_to = "Proporcion") %>%
      mutate(Factor = factor(Factor, levels = factores, labels = etiquetas))
    
    ggplot(datos_long, aes(x = Factor, y = Proporcion, fill = Diabetes_Label)) +
      geom_col(position = "dodge", width = 0.7) +
      geom_text(aes(label = percent(Proporcion, accuracy = 1)),
                position = position_dodge(width = 0.7),
                vjust = -0.4, size = 3.2, fontface = "bold") +
      scale_fill_manual(values = PAL, name = "Condicion") +
      scale_y_continuous(labels = percent, limits = c(0, 1),
                         expand = expansion(mult = c(0, 0.1))) +
      labs(
        title    = "Prevalencia de factores de riesgo por condicion diabetica",
        subtitle = "Porcentaje de personas con cada factor de riesgo presente",
        x = NULL, y = "Proporcion"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title         = element_text(face = "bold", size = 14, color = "#1a1a2e"),
        plot.subtitle      = element_text(size = 11, color = "#6c757d", margin = margin(b = 12)),
        legend.position    = "top",
        legend.title       = element_text(face = "bold", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(angle = 20, hjust = 1),
        plot.margin        = margin(10, 20, 10, 10)
      )
  })
  
  # ── Viz 4: Barras apiladas por edad ──────────────────────
  output$viz_edad <- renderPlot({
    etiq_edad <- c(
      "1"="18-24","2"="25-29","3"="30-34","4"="35-39",
      "5"="40-44","6"="45-49","7"="50-54","8"="55-59",
      "9"="60-64","10"="65-69","11"="70-74","12"="75-79","13"="80+"
    )
    
    datos_edad <- df_raw %>%
      filter(Diabetes_Label %in% input$sel_grupo) %>%
      mutate(GrupoEdad = factor(Age, levels = 1:13, labels = etiq_edad)) %>%
      count(GrupoEdad, Diabetes_Label) %>%
      group_by(GrupoEdad) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
    
    ggplot(datos_edad, aes(x = GrupoEdad, y = pct, fill = Diabetes_Label)) +
      geom_col(width = 0.75, position = "stack") +
      scale_fill_manual(values = PAL, name = "Condicion") +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(
        title    = "Composicion de la condicion diabetica segun grupo de edad",
        subtitle = "Barras apiladas al 100% — La prevalencia de diabetes aumenta con la edad",
        x = "Grupo de edad (anos)", y = "Proporcion de encuestados"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title         = element_text(face = "bold", size = 14, color = "#1a1a2e"),
        plot.subtitle      = element_text(size = 11, color = "#6c757d", margin = margin(b = 12)),
        legend.position    = "top",
        legend.title       = element_text(face = "bold", size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.x        = element_text(angle = 35, hjust = 1, size = 10),
        plot.margin        = margin(10, 20, 10, 10)
      )
  })
  
  # ── Viz 5: Heatmap correlaciones ──────────────────────────
  output$viz_heatmap <- renderPlot({
    vars <- c("Diabetes_012","HighBP","HighChol","BMI",
              "Smoker","PhysActivity","GenHlth","Age","Income")
    labs_vars <- c("Diabetes","Hipertension","Col. alto","BMI",
                   "Fumador","Act. fisica","Salud gral.","Edad","Ingreso")
    
    mat <- df_raw %>%
      select(all_of(vars)) %>%
      cor(use = "complete.obs")
    
    colnames(mat) <- labs_vars
    rownames(mat) <- labs_vars
    
    mat_long <- melt(mat)
    mat_long$txt_color <- ifelse(abs(mat_long$value) > 0.25, "white", "#2d2d2d")
    
    ggplot(mat_long, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white", linewidth = 0.6) +
      geom_text(aes(label = sprintf("%.2f", value), color = txt_color),
                size = 3.0, fontface = "bold") +
      scale_color_identity() +
      scale_fill_gradient2(
        low = "#2166AC", mid = "#f7f7f7", high = "#D6604D",
        midpoint = 0, limits = c(-1, 1),
        name = "Pearson r",
        guide = guide_colorbar(barheight = 10, barwidth = 1.2)
      ) +
      labs(
        title    = "Matriz de correlacion de Pearson entre indicadores de salud",
        subtitle = "Paleta divergente: rojo = correlacion positiva · azul = correlacion negativa",
        x = NULL, y = NULL
      ) +
      coord_fixed() +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_text(face = "bold", size = 14, color = "#1a1a2e"),
        plot.subtitle = element_text(size = 11, color = "#6c757d", margin = margin(b = 12)),
        axis.text.x   = element_text(angle = 40, hjust = 1, size = 10),
        axis.text.y   = element_text(size = 10),
        panel.grid    = element_blank(),
        legend.title  = element_text(face = "bold", size = 10),
        plot.margin   = margin(10, 20, 10, 10)
      )
  })
}

# ── Lanzar ───────────────────────────────────────────────────
shinyApp(ui = ui, server = server)