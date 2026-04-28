# ============================================================
# Diabetes Health Indicators — BRFSS 2015
# Herramientas y Visualización de Datos — Proyecto 2
# Shiny App | R + Plotly
# ============================================================

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(scales)
library(plotly)

df_raw <- read.csv("data/diabetes_012_health_indicators_BRFSS2015.csv")
df_raw$Diabetes_Label <- factor(df_raw$Diabetes_012, levels=c(0,1,2),
                                labels=c("Sin Diabetes","Prediabetes","Diabetes"))

PAL <- c("Sin Diabetes"="#4DAF4A", "Prediabetes"="#FF7F00", "Diabetes"="#E41A1C")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, sans-serif; }
    .titulo-app { padding: 18px 24px 14px; border-bottom: 3px solid #E41A1C; margin-bottom: 22px; }
    .titulo-app h3 { margin: 0 0 4px; font-size: 20px; font-weight: 700; color: #1a1a2e; }
    .titulo-app p { margin: 0; font-size: 12px; color: #6c757d; }
    .panel-control { background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px; padding: 16px; }
    .panel-control h5 { font-size: 11px; font-weight: 700; text-transform: uppercase; letter-spacing: 0.8px; color: #718096; margin-bottom: 14px; }
    .insight { background: #fffbeb; border-left: 4px solid #f6ad55; padding: 9px 13px; margin-bottom: 14px; font-size: 13px; color: #3d3d3d; border-radius: 0 4px 4px 0; }
    .insight strong { color: #c05621; }
    .nav-tabs > li > a { font-size: 13px; font-weight: 600; color: #495057; }
    .nav-tabs > li.active > a { color: #E41A1C !important; border-top: 3px solid #E41A1C !important; }
    .fuente { font-size: 11px; color: #adb5bd; margin-top: 6px; text-align: right; }
  "))),
  
  div(class="titulo-app",
      h3("Indicadores de Salud y Riesgo de Diabetes — EE.UU. 2015"),
      p("Behavioral Risk Factor Surveillance System (BRFSS) · CDC · n = 253,680 · 22 variables")),
  
  fluidRow(
    column(3,
           div(class="panel-control",
               h5("Filtros"),
               checkboxGroupInput("sel_grupo", "Condicion diabetica:",
                                  choices=c("Sin Diabetes","Prediabetes","Diabetes"),
                                  selected=c("Sin Diabetes","Prediabetes","Diabetes")),
               hr(style="margin: 12px 0;"),
               p(style="font-size:11px; color:#868e96; margin:0;",
                 "Los filtros aplican a todas las visualizaciones excepto el mapa de correlaciones."))),
    column(9,
           tabsetPanel(
             tabPanel("Comparacion por grupo", br(),
                      div(class="insight", strong("Hallazgo:"), " El 84.2% no tiene diabetes. La prediabetes esta muy subrepresentada (1.8%)."),
                      plotlyOutput("viz_barras", height="400px"),
                      p(class="fuente", "Fuente: CDC BRFSS 2015 — Kaggle")),
             tabPanel("Distribucion de BMI", br(),
                      div(class="insight", strong("Hallazgo:"), " La mediana del BMI en personas con diabetes (~31) es significativamente mayor que en personas sin diabetes (~27)."),
                      plotlyOutput("viz_boxplot", height="400px"),
                      p(class="fuente", "Fuente: CDC BRFSS 2015 — Kaggle")),
             tabPanel("Factores de riesgo", br(),
                      div(class="insight", strong("Hallazgo:"), " La hipertension (75.3%) y el colesterol alto (67%) son los factores mas prevalentes en personas con diabetes."),
                      plotlyOutput("viz_factores", height="420px"),
                      p(class="fuente", "Fuente: CDC BRFSS 2015 — Kaggle")),
             tabPanel("Prevalencia por edad", br(),
                      div(class="insight", strong("Hallazgo:"), " La proporcion de personas con diabetes crece sostenidamente con la edad, superando el 25% a partir de los 60-64 anos."),
                      plotlyOutput("viz_edad", height="420px"),
                      p(class="fuente", "Fuente: CDC BRFSS 2015 — Kaggle")),
             tabPanel("Mapa de correlaciones", br(),
                      div(class="insight", strong("Hallazgo:"), " Diabetes correlaciona con hipertension (0.27), salud general (0.30) y BMI (0.22). La actividad fisica muestra correlacion negativa."),
                      plotlyOutput("viz_heatmap", height="500px"),
                      p(class="fuente", "Fuente: CDC BRFSS 2015 — Kaggle"))
           ))
  )
)

server <- function(input, output, session) {
  
  df <- reactive({
    df_raw %>% filter(Diabetes_Label %in% input$sel_grupo)
  })
  
  # Viz 1: Barras
  output$viz_barras <- renderPlotly({
    datos <- df() %>%
      count(Diabetes_Label) %>%
      mutate(pct = n / sum(n),
             label = paste0(format(n, big.mark=","), "<br>(", percent(pct, accuracy=0.1), ")"),
             color = PAL[as.character(Diabetes_Label)])
    
    plot_ly(data=datos,
            x=~reorder(as.character(Diabetes_Label), -n),
            y=~n,
            type="bar",
            marker=list(color=~color),
            text=~label,
            textposition="outside",
            hovertemplate=~paste0(Diabetes_Label,"<br>n = ",format(n,big.mark=","),"<br>",percent(pct,accuracy=0.1),"<extra></extra>"),
            showlegend=FALSE) %>%
      layout(xaxis=list(title="", showgrid=FALSE),
             yaxis=list(title="Numero de personas", gridcolor="#e2e8f0", tickformat=","),
             plot_bgcolor="white", paper_bgcolor="white",
             font=list(family="Helvetica Neue, Helvetica, Arial, sans-serif"),
             margin=list(t=20,b=50))
  })
  
  # Viz 2: Boxplot
  output$viz_boxplot <- renderPlotly({
    grupos <- intersect(c("Sin Diabetes","Prediabetes","Diabetes"), input$sel_grupo)
    fig <- plot_ly(type="box")
    for (g in grupos) {
      vals <- df() %>% filter(as.character(Diabetes_Label)==g) %>% pull(BMI)
      fig <- add_trace(fig, y=vals, name=g,
                       fillcolor=PAL[g], line=list(color="#1a1a2e", width=1.5),
                       marker=list(opacity=0), boxmean=TRUE, type="box",
                       hovertemplate=paste0(g,"<br>Mediana: %{median:.1f}<br>Q1: %{q1:.1f} · Q3: %{q3:.1f}<extra></extra>"))
    }
    fig %>% layout(showlegend=FALSE,
                   xaxis=list(title="", showgrid=FALSE),
                   yaxis=list(title="Indice de Masa Corporal (BMI)", gridcolor="#e2e8f0", range=c(10,70)),
                   plot_bgcolor="white", paper_bgcolor="white",
                   font=list(family="Helvetica Neue, Helvetica, Arial, sans-serif"),
                   margin=list(t=20,b=50))
  })
  
  # Viz 3: Factores de riesgo
  output$viz_factores <- renderPlotly({
    factores  <- c("HighBP","HighChol","Smoker","PhysActivity","HeartDiseaseorAttack")
    etiquetas <- c("Hipertension","Col. alto","Fumador","Act. fisica","Enf. cardiaca")
    grupos    <- intersect(c("Sin Diabetes","Prediabetes","Diabetes"), input$sel_grupo)
    
    # Calcular proporciones reales por grupo
    datos_f <- df() %>%
      group_by(Diabetes_Label) %>%
      summarise(across(all_of(factores), mean, .names="{.col}"), .groups="drop") %>%
      pivot_longer(cols=all_of(factores), names_to="Factor", values_to="Proporcion") %>%
      mutate(Factor=factor(Factor, levels=factores, labels=etiquetas),
             Diabetes_Label=as.character(Diabetes_Label))
    
    fig <- plot_ly(type="bar")
    for (g in grupos) {
      sub <- datos_f %>% filter(Diabetes_Label==g)
      fig <- add_trace(fig,
                       x=sub$Factor,
                       y=sub$Proporcion,
                       name=g,
                       type="bar",
                       marker=list(color=PAL[g]),
                       text=percent(sub$Proporcion, accuracy=0.1),
                       textposition="outside",
                       hovertemplate=paste0(g,"<br>%{x}: %{text}<extra></extra>"))
    }
    fig %>% layout(barmode="group",
                   xaxis=list(title="", showgrid=FALSE),
                   yaxis=list(title="Proporcion", gridcolor="#e2e8f0",
                              tickformat=".0%", range=c(0,1.05)),
                   legend=list(orientation="h", y=1.08, x=0),
                   plot_bgcolor="white", paper_bgcolor="white",
                   font=list(family="Helvetica Neue, Helvetica, Arial, sans-serif"),
                   margin=list(t=60,b=60))
  })
  
  # Viz 4: Prevalencia por edad
  output$viz_edad <- renderPlotly({
    etiq_edad <- c("1"="18-24","2"="25-29","3"="30-34","4"="35-39","5"="40-44","6"="45-49",
                   "7"="50-54","8"="55-59","9"="60-64","10"="65-69","11"="70-74","12"="75-79","13"="80+")
    grupos <- intersect(c("Sin Diabetes","Prediabetes","Diabetes"), input$sel_grupo)
    
    datos_edad <- df_raw %>%
      filter(as.character(Diabetes_Label) %in% grupos) %>%
      mutate(GrupoEdad=factor(Age, levels=1:13, labels=etiq_edad)) %>%
      count(GrupoEdad, Diabetes_Label) %>%
      group_by(GrupoEdad) %>%
      mutate(pct=n/sum(n)) %>%
      ungroup() %>%
      mutate(Diabetes_Label=as.character(Diabetes_Label))
    
    fig <- plot_ly(type="bar")
    for (g in grupos) {
      sub <- datos_edad %>% filter(Diabetes_Label==g)
      fig <- add_trace(fig,
                       x=sub$GrupoEdad,
                       y=sub$pct,
                       name=g,
                       type="bar",
                       marker=list(color=PAL[g]),
                       hovertemplate=paste0(g,"<br>Edad: %{x}<br>%{y:.1%}<extra></extra>"))
    }
    fig %>% layout(barmode="stack",
                   xaxis=list(title="Grupo de edad (anos)", showgrid=FALSE),
                   yaxis=list(title="Proporcion", gridcolor="#e2e8f0",
                              tickformat=".0%", range=c(0,1.02)),
                   legend=list(orientation="h", y=1.08, x=0),
                   plot_bgcolor="white", paper_bgcolor="white",
                   font=list(family="Helvetica Neue, Helvetica, Arial, sans-serif"),
                   margin=list(t=60,b=60))
  })
  
  # Viz 5: Heatmap
  output$viz_heatmap <- renderPlotly({
    vars      <- c("Diabetes_012","HighBP","HighChol","BMI","Smoker","PhysActivity","GenHlth","Age","Income")
    labs_vars <- c("Diabetes","Hipertension","Col. alto","BMI","Fumador","Act. fisica","Salud gral.","Edad","Ingreso")
    
    mat <- df_raw %>% select(all_of(vars)) %>% cor(use="complete.obs")
    colnames(mat) <- labs_vars
    rownames(mat) <- labs_vars
    
    plot_ly(x=labs_vars, y=labs_vars, z=mat,
            type="heatmap",
            colorscale=list(c(0,"#2166AC"), c(0.5,"#f7f7f7"), c(1,"#D6604D")),
            zmin=-1, zmax=1,
            text=round(mat,2), texttemplate="%{text}",
            hovertemplate="%{y} x %{x}<br>r = %{z:.3f}<extra></extra>",
            colorbar=list(title="Pearson r")) %>%
      layout(xaxis=list(tickangle=-40, showgrid=FALSE),
             yaxis=list(autorange="reversed", showgrid=FALSE),
             plot_bgcolor="white", paper_bgcolor="white",
             font=list(family="Helvetica Neue, Helvetica, Arial, sans-serif", size=11),
             margin=list(t=20,b=120,l=100))
  })
}

shinyApp(ui=ui, server=server)