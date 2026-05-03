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
    .ctitle { font-size: 14px; font-weight: 700; color: #1a1a2e; margin-bottom: 2px; margin-top: 6px; }
    .csub { font-size: 12px; color: #718096; margin-bottom: 8px; }
    .hallazgos {
      background: #fffbeb;
      border-left: 4px solid #f6ad55;
      padding: 14px 18px;
      border-radius: 0 6px 6px 0;
      margin-top: 16px;
      margin-bottom: 8px;
      font-size: 13.5px;
      color: #2d3748;
      line-height: 1.65;
    }
    .hallazgos b { color: #c05621; }
    .hallazgos .titulo-h {
      font-size: 12px;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.8px;
      color: #c05621;
      margin-bottom: 8px;
      display: block;
    }
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
                 "Los filtros aplican a todas las visualizaciones excepto el mapa de correlaciones y el de acceso por ingreso."))),
    column(9,
           tabsetPanel(
             tabPanel("Acceso a salud por ingreso", br(),
                      div(class="ctitle", "Acceso a atencion medica segun nivel de ingreso"),
                      div(class="csub", "Cobertura de salud y costo como barrera para ir al medico"),
                      plotlyOutput("viz_acceso", height="460px"),
                      p(class="fuente", "Fuente: CDC BRFSS 2015 — Kaggle"),
                      uiOutput("hallazgos_acceso")),
             
             tabPanel("Distribucion de BMI", br(),
                      div(class="ctitle", "Distribucion del peso corporal segun condicion diabetica"),
                      div(class="csub", "Histograma con lineas que marcan sobrepeso (25) y obesidad (30) segun la OMS"),
                      plotlyOutput("viz_bmi", height="460px"),
                      p(class="fuente", "Fuente: CDC BRFSS 2015 — Kaggle · Cortes IMC: OMS"),
                      uiOutput("hallazgos_bmi")),
             
             tabPanel("Factores de riesgo", br(),
                      div(class="ctitle", "Perfil de factores de riesgo por condicion diabetica"),
                      div(class="csub", "Cuanto afecta cada factor a las personas segun su condicion"),
                      plotlyOutput("viz_factores", height="480px"),
                      p(class="fuente", "Fuente: CDC BRFSS 2015 — Kaggle"),
                      uiOutput("hallazgos_factores")),
             
             tabPanel("Prevalencia por edad", br(),
                      div(class="ctitle", "Prevalencia diabetica segun grupo de edad"),
                      div(class="csub", "Mapa de calor — mas rojo = mayor porcentaje en esa edad"),
                      plotlyOutput("viz_edad", height="380px"),
                      p(class="fuente", "Fuente: CDC BRFSS 2015 — Kaggle"),
                      uiOutput("hallazgos_edad")),
             
             tabPanel("Mapa de correlaciones", br(),
                      div(class="ctitle", "Como se relacionan los indicadores de salud entre si"),
                      div(class="csub", "Rojo: van de la mano · Azul: cuando uno sube, el otro baja"),
                      plotlyOutput("viz_heatmap", height="500px"),
                      p(class="fuente", "Fuente: CDC BRFSS 2015 — Kaggle"),
                      uiOutput("hallazgos_corr"))
           ))
  )
)

server <- function(input, output, session) {
  
  df <- reactive({
    df_raw %>% filter(Diabetes_Label %in% input$sel_grupo)
  })
  
  # ── Helper para bloque de hallazgos ────────────────────────
  hallazgos_box <- function(html) {
    HTML(paste0(
      '<div class="hallazgos"><span class="titulo-h">Hallazgos</span>',
      html, '</div>'
    ))
  }
  
  # ── Tab 1: Acceso a salud por ingreso ──────────────────────
  output$viz_acceso <- renderPlotly({
    income_labels <- c("1"="<$10k","2"="$10-15k","3"="$15-20k","4"="$20-25k",
                       "5"="$25-35k","6"="$35-50k","7"="$50-75k","8"=">$75k")
    d <- df_raw %>% mutate(Ingreso = income_labels[as.character(Income)]) %>%
      filter(!is.na(Ingreso))
    
    agrup <- d %>% group_by(Ingreso) %>%
      summarise(cobertura = mean(AnyHealthcare)*100,
                no_fue = mean(NoDocbcCost)*100,
                n = n(), .groups="drop")
    agrup$Ingreso <- factor(agrup$Ingreso, levels=income_labels)
    agrup <- agrup %>% arrange(Ingreso)
    
    plot_ly() %>%
      add_trace(x=agrup$Ingreso, y=round(agrup$cobertura,1),
                type="bar", name="Tiene seguro de salud",
                marker=list(color="#4DAF4A"),
                text=paste0(round(agrup$cobertura,1),"%"),
                textposition="outside", textfont=list(size=10),
                hovertemplate="<b>Ingreso: %{x}</b><br>Con seguro: %{y:.1f}%<extra></extra>") %>%
      add_trace(x=agrup$Ingreso, y=round(agrup$no_fue,1),
                type="scatter", mode="lines+markers",
                name="No fue al medico por costo",
                line=list(color="#E41A1C", width=3),
                marker=list(color="#E41A1C", size=10, line=list(color="white", width=2)),
                hovertemplate="<b>Ingreso: %{x}</b><br>No fue por costo: %{y:.1f}%<extra></extra>") %>%
      layout(yaxis=list(title="Porcentaje (%)", range=c(0,110), gridcolor="#e2e8f0"),
             xaxis=list(title="Nivel de ingreso anual (USD)", showgrid=FALSE),
             legend=list(orientation="h", y=1.08, x=0),
             plot_bgcolor="white", paper_bgcolor="white",
             font=list(family="Helvetica Neue, Helvetica, Arial, sans-serif"),
             margin=list(t=60,b=60), bargap=0.35)
  })
  
  output$hallazgos_acceso <- renderUI({
    income_labels <- c("1"="<$10k","2"="$10-15k","3"="$15-20k","4"="$20-25k",
                       "5"="$25-35k","6"="$35-50k","7"="$50-75k","8"=">$75k")
    d <- df_raw %>% mutate(Ingreso = income_labels[as.character(Income)]) %>%
      filter(!is.na(Ingreso))
    agrup <- d %>% group_by(Ingreso) %>%
      summarise(cobertura = mean(AnyHealthcare)*100,
                no_fue = mean(NoDocbcCost)*100, .groups="drop")
    agrup$Ingreso <- factor(agrup$Ingreso, levels=income_labels)
    agrup <- agrup %>% arrange(Ingreso)
    
    cob_min  <- agrup$cobertura[1]
    cob_max  <- agrup$cobertura[nrow(agrup)]
    cost_min <- agrup$no_fue[1]
    cost_max <- agrup$no_fue[nrow(agrup)]
    cob_diff <- cob_max - cob_min
    cost_ratio <- cost_min / max(cost_max, 0.1)
    
    prev_pre  <- mean(df_raw$Diabetes_012 == 1) * 100
    prev_diab <- mean(df_raw$Diabetes_012 == 2) * 100
    
    hallazgos_box(sprintf(
      "El grafico muestra dos curvas que se mueven en direcciones opuestas. La cobertura de seguro sube de <b>%.1f%%</b> en el nivel mas bajo a <b>%.1f%%</b> en el mas alto, una mejora de <b>%.1f puntos</b>. Al mismo tiempo, el porcentaje de personas que dejaron de ir al medico por costo cae de <b>%.1f%%</b> a <b>%.1f%%</b> — es decir, en los niveles mas pobres es <b>%.1f veces mas alto</b> que en los mas ricos.<br><br>
      Lo logico aqui es preguntarse: si en el nivel de ingreso mas bajo el %.1f%% ya tiene seguro, &iquest;por que casi 1 de cada 3 personas (el %.1f%%) no fue al medico por dinero? La respuesta esta en que <b>tener seguro no es lo mismo que poder pagar la atencion</b>. Los seguros tienen copagos, deducibles y medicamentos no cubiertos. Para alguien que gana menos de $10.000 al ano, esos gastos pueden ser una decision real entre ir al medico o pagar otras necesidades basicas.<br><br>
      Esto conecta con algo importante del dataset: la prediabetes solo aparece en <b>%.1f%%</b> de los registros, mientras que la diabetes ya diagnosticada esta en <b>%.1f%%</b>. La prediabetes es una etapa silenciosa que solo se detecta con un examen de sangre — un examen al que la poblacion vulnerable no esta llegando. La diabetes en cambio si aparece mas porque cuando da sintomas, la gente termina yendo a urgencias. <b>Lo que vemos no es que haya poca prediabetes, sino que no se esta detectando en quienes mas la tienen.</b>",
      cob_min, cob_max, cob_diff, cost_min, cost_max, cost_ratio,
      cob_min, cost_min, prev_pre, prev_diab
    ))
  })
  
  # ── Tab 2: Histograma BMI con cortes OMS ───────────────────
  output$viz_bmi <- renderPlotly({
    grupos <- intersect(c("Sin Diabetes","Prediabetes","Diabetes"), input$sel_grupo)
    if (length(grupos) == 0) return(plot_ly() %>% layout(title="Selecciona al menos un grupo"))
    
    fig <- plot_ly()
    for (g in grupos) {
      vals <- df() %>% filter(as.character(Diabetes_Label)==g) %>% pull(BMI)
      vals <- vals[!is.na(vals)]
      fig <- add_trace(fig, x=vals, type="histogram", name=g,
                       marker=list(color=PAL[g]), opacity=0.55,
                       xbins=list(start=12, end=60, size=1),
                       histnorm="percent",
                       hovertemplate=paste0("<b>",g,"</b><br>IMC: %{x}<br>%{y:.1f}% del grupo<extra></extra>"))
    }
    fig %>% layout(barmode="overlay",
                   xaxis=list(title="Indice de Masa Corporal (BMI)", range=c(12,60), showgrid=FALSE),
                   yaxis=list(title="% dentro del grupo", gridcolor="#e2e8f0"),
                   shapes=list(
                     list(type="line", x0=25, x1=25, y0=0, y1=1, yref="paper",
                          line=list(color="#4a5568", width=1.5, dash="dash")),
                     list(type="line", x0=30, x1=30, y0=0, y1=1, yref="paper",
                          line=list(color="#1a1a2e", width=1.5, dash="dash"))
                   ),
                   annotations=list(
                     list(x=25, y=1.02, yref="paper", text="Sobrepeso (OMS &ge; 25)",
                          showarrow=FALSE, font=list(size=11, color="#4a5568")),
                     list(x=30, y=1.02, yref="paper", text="Obesidad (OMS &ge; 30)",
                          showarrow=FALSE, font=list(size=11, color="#1a1a2e"))
                   ),
                   legend=list(orientation="h", y=1.08, x=0),
                   plot_bgcolor="white", paper_bgcolor="white",
                   font=list(family="Helvetica Neue, Helvetica, Arial, sans-serif"),
                   margin=list(t=60,b=60))
  })
  
  output$hallazgos_bmi <- renderUI({
    grupos <- intersect(c("Sin Diabetes","Prediabetes","Diabetes"), input$sel_grupo)
    if (length(grupos) == 0) {
      return(hallazgos_box("Selecciona al menos un grupo en el filtro lateral para ver los hallazgos."))
    }
    
    stats <- list()
    for (g in grupos) {
      vals <- df() %>% filter(as.character(Diabetes_Label)==g) %>% pull(BMI)
      vals <- vals[!is.na(vals)]
      stats[[g]] <- list(
        mediana = median(vals),
        pct_obesidad = mean(vals >= 30) * 100,
        pct_sobrepeso = mean(vals >= 25 & vals < 30) * 100,
        pct_normopeso = mean(vals < 25) * 100
      )
    }
    
    n_sel <- length(grupos)
    
    if (n_sel == 1) {
      g <- grupos[1]; s <- stats[[g]]
      zona <- if (s$mediana >= 30) "obesidad" else if (s$mediana >= 25) "sobrepeso" else "peso normal"
      txt <- sprintf("En el grupo <b>%s</b>, el peso tipico (mediana) es de <b>IMC %.1f</b>, lo que cae en la zona de <b>%s</b>. Distribuyendo a las personas segun los cortes de la OMS:<br>
      &bull; <b>%.1f%%</b> tiene peso normal (IMC menor a 25).<br>
      &bull; <b>%.1f%%</b> tiene sobrepeso (IMC entre 25 y 30).<br>
      &bull; <b>%.1f%%</b> tiene obesidad (IMC mayor o igual a 30).<br><br>
      Lo que estos numeros dicen sobre este grupo: la mayoria de las personas <b>no</b> tienen un peso saludable segun la OMS. Para entender por que esto importa, activa los otros grupos en el filtro y compara como cambian estos porcentajes — esa comparacion es la que revela el rol del peso en la diabetes.",
                     g, s$mediana, zona, s$pct_normopeso, s$pct_sobrepeso, s$pct_obesidad)
    } else if (n_sel == 2) {
      g1 <- grupos[1]; g2 <- grupos[2]
      s1 <- stats[[g1]]; s2 <- stats[[g2]]
      if (s1$mediana > s2$mediana) { mayor <- g1; menor <- g2; sm <- s1; sn <- s2 }
      else { mayor <- g2; menor <- g1; sm <- s2; sn <- s1 }
      diff_med <- abs(s1$mediana - s2$mediana)
      diff_obs <- sm$pct_obesidad - sn$pct_obesidad
      txt <- sprintf("La mediana del IMC en <b>%s</b> (%.1f) supera a la de <b>%s</b> (%.1f) por <b>%.1f puntos</b>. Pero el dato mas revelador esta en la proporcion de obesidad: <b>%.1f%%</b> en %s contra <b>%.1f%%</b> en %s — una diferencia de <b>%.1f puntos</b>.<br><br>
      Lo que esto significa logicamente: no es solo que un grupo \"pese un poco mas\" que el otro. Es que la <b>proporcion</b> de personas en zona de obesidad cambia de forma sustancial entre los dos grupos. Cuando una distribucion completa se desplaza asi, no estamos hablando de casos individuales — estamos viendo un patron estructural que afecta a todo el grupo.<br><br>
      Si %s es el grupo con condicion diabetica mas severa, este patron confirma que el peso no es solo una consecuencia de la enfermedad: es un factor que la antecede y la sostiene.",
                     mayor, sm$mediana, menor, sn$mediana, diff_med, sm$pct_obesidad, mayor, sn$pct_obesidad, menor, diff_obs, mayor)
    } else {
      s_sd <- stats[["Sin Diabetes"]]; s_pd <- stats[["Prediabetes"]]; s_d <- stats[["Diabetes"]]
      ratio_obs <- s_d$pct_obesidad / max(s_sd$pct_obesidad, 0.1)
      txt <- sprintf("Las medianas de IMC se ordenan de forma escalonada: <b>%.1f</b> en Sin Diabetes, <b>%.1f</b> en Prediabetes y <b>%.1f</b> en Diabetes. La progresion no es casual: cada paso hacia una condicion mas grave viene con un peso tipico mayor.<br><br>
      Lo mas contundente esta en la proporcion de obesidad de cada grupo:<br>
      &bull; Sin Diabetes: <b>%.1f%%</b> tienen obesidad.<br>
      &bull; Prediabetes: <b>%.1f%%</b>.<br>
      &bull; Diabetes: <b>%.1f%%</b>.<br><br>
      Es decir, en el grupo con diabetes, casi <b>%.1f veces mas</b> personas estan en obesidad respecto al grupo sin diabetes. Cuando un factor cambia tanto entre grupos, no es coincidencia: <b>el peso no acompana a la diabetes, la antecede</b>.<br><br>
      Esto tiene una implicacion practica: si el peso aumenta el riesgo de manera tan clara, entonces la prevencion se vuelve concreta — bajar de IMC no es un consejo generico, es la intervencion mas directa con la evidencia que muestra este dataset.",
                     s_sd$mediana, s_pd$mediana, s_d$mediana,
                     s_sd$pct_obesidad, s_pd$pct_obesidad, s_d$pct_obesidad, ratio_obs)
    }
    hallazgos_box(txt)
  })
  
  # ── Tab 3: Radar de factores de riesgo ─────────────────────
  output$viz_factores <- renderPlotly({
    grupos <- intersect(c("Sin Diabetes","Prediabetes","Diabetes"), input$sel_grupo)
    if (length(grupos) == 0) return(plot_ly() %>% layout(title="Selecciona al menos un grupo"))
    
    fcols   <- c("HighBP","HighChol","Smoker","PhysActivity","HeartDiseaseorAttack")
    flabels <- c("Hipertension","Col. alto","Fumador","Act. fisica","Enf. cardiaca")
    
    fig <- plot_ly(type="scatterpolar", mode="lines")
    for (g in grupos) {
      sub <- df() %>% filter(as.character(Diabetes_Label)==g)
      vals <- sapply(fcols, function(c) round(mean(sub[[c]], na.rm=TRUE)*100, 1))
      vals_closed   <- c(vals, vals[1])
      labels_closed <- c(flabels, flabels[1])
      fig <- add_trace(fig, r=vals_closed, theta=labels_closed,
                       fill="toself", name=g, type="scatterpolar",
                       line=list(color=PAL[g], width=2),
                       fillcolor=paste0(PAL[g], "59"),
                       hovertemplate=paste0("<b>",g,"</b><br>%{theta}: %{r:.1f}%<extra></extra>"))
    }
    fig %>% layout(
      polar=list(radialaxis=list(visible=TRUE, range=c(0,100), ticksuffix="%",
                                 gridcolor="#e2e8f0", linecolor="#e2e8f0"),
                 angularaxis=list(gridcolor="#e2e8f0", linecolor="#e2e8f0"),
                 bgcolor="white"),
      paper_bgcolor="white",
      font=list(family="Helvetica Neue, Helvetica, Arial, sans-serif", size=12),
      legend=list(orientation="h", y=-0.1, x=0.5, xanchor="center"),
      margin=list(l=80, r=80, t=20, b=20))
  })
  
  output$hallazgos_factores <- renderUI({
    grupos <- intersect(c("Sin Diabetes","Prediabetes","Diabetes"), input$sel_grupo)
    if (length(grupos) == 0) {
      return(hallazgos_box("Selecciona al menos un grupo en el filtro lateral para ver los hallazgos."))
    }
    
    fcols   <- c("HighBP","HighChol","Smoker","PhysActivity","HeartDiseaseorAttack")
    flabels <- c("Hipertension","Col. alto","Fumador","Act. fisica","Enf. cardiaca")
    factores_data <- list()
    for (g in grupos) {
      sub <- df() %>% filter(as.character(Diabetes_Label)==g)
      vals <- sapply(fcols, function(c) round(mean(sub[[c]], na.rm=TRUE)*100, 1))
      names(vals) <- flabels
      factores_data[[g]] <- vals
    }
    
    n_sel <- length(grupos)
    
    if (n_sel == 1) {
      g <- grupos[1]; d_g <- factores_data[[g]]
      ord <- sort(d_g, decreasing=TRUE)
      mayor <- names(ord)[1]; menor <- names(ord)[length(ord)]
      ord_str <- paste(sapply(seq_along(ord), function(i)
        sprintf("<b>%s</b> (%.1f%%)", names(ord)[i], ord[i])), collapse=" &middot; ")
      txt <- sprintf("En el grupo <b>%s</b>, los cinco factores se ordenan asi de mayor a menor prevalencia:<br>%s<br><br>
      El factor mas extendido es <b>%s</b> (%.1f%%) y el menos comun es <b>%s</b> (%.1f%%). Pero ver un solo grupo no nos dice mucho — un porcentaje aislado no permite saber si es alto o bajo. Por ejemplo, un %.1f%% en %s solo cobra sentido cuando lo comparamos con los otros grupos.<br><br>
      Activa los otros grupos en el filtro para que el contraste revele que factores son <b>distintivos</b> de cada condicion y cuales son comunes a todos.",
                     g, ord_str, mayor, ord[1], menor, ord[length(ord)], ord[1], mayor)
    } else if (n_sel == 2) {
      g1 <- grupos[1]; g2 <- grupos[2]
      d1 <- factores_data[[g1]]; d2 <- factores_data[[g2]]
      diferencias <- d1 - d2
      ord_dif <- sort(abs(diferencias), decreasing=TRUE)
      mayor_dif <- names(ord_dif)[1]
      menor_dif <- names(ord_dif)[length(ord_dif)]
      val_dif <- abs(diferencias[mayor_dif])
      val_dif_min <- abs(diferencias[menor_dif])
      seg_dif <- names(ord_dif)[2]
      txt <- sprintf("Comparando los dos grupos, las diferencias mas grandes estan en:<br>
      &bull; <b>%s</b>: %.1f%% en %s vs %.1f%% en %s — diferencia de <b>%.1f puntos</b>.<br>
      &bull; <b>%s</b>: %.1f%% vs %.1f%% — diferencia de %.1f puntos.<br><br>
      Y la diferencia <b>mas pequena</b> esta en <b>%s</b> (%.1f%% vs %.1f%%, solo %.1f puntos). Eso significa que en ese factor los dos grupos son muy parecidos — no es lo que los diferencia.<br><br>
      Lo logico que se desprende: <b>%s</b> es el factor que mas distingue a estos dos grupos. Si %s es el grupo con condicion mas severa, entonces controlar %s deberia ser una prioridad para evitar que personas pasen del grupo mas leve al mas grave.",
                     mayor_dif, d1[mayor_dif], g1, d2[mayor_dif], g2, val_dif,
                     seg_dif, d1[seg_dif], d2[seg_dif], abs(diferencias[seg_dif]),
                     menor_dif, d1[menor_dif], d2[menor_dif], val_dif_min,
                     mayor_dif, g1, mayor_dif)
    } else {
      d_sd <- factores_data[["Sin Diabetes"]]
      d_d  <- factores_data[["Diabetes"]]
      d_pd <- factores_data[["Prediabetes"]]
      ratio_hta  <- d_d["Hipertension"] / max(d_sd["Hipertension"], 0.1)
      ratio_chol <- d_d["Col. alto"]    / max(d_sd["Col. alto"], 0.1)
      txt <- sprintf("Mirando los tres grupos juntos, dos factores muestran un patron claro de escalada de Sin Diabetes &rarr; Prediabetes &rarr; Diabetes:<br>
      &bull; <b>Hipertension</b>: %.1f%% &rarr; %.1f%% &rarr; <b>%.1f%%</b>. En el grupo con diabetes es <b>%.1f veces</b> mas frecuente que en el grupo sano.<br>
      &bull; <b>Colesterol alto</b>: %.1f%% &rarr; %.1f%% &rarr; <b>%.1f%%</b>. La diabetes lo multiplica por <b>%.1f</b>.<br><br>
      Y un factor se mueve <b>en sentido opuesto</b>: la actividad fisica pasa de <b>%.1f%%</b> en sin diabetes, a %.1f%% en prediabetes, a solo <b>%.1f%%</b> en el grupo con diabetes. Es la unica variable donde el grupo mas sano tiene <b>mas</b> que los enfermos.<br><br>
      La lectura logica es directa: la diabetes no aparece sola, viene en un paquete. Cuando alguien tiene diabetes, las probabilidades de que tambien tenga hipertension y colesterol alto son <b>3 a 4 veces mas altas</b> que en una persona sana. Y al mismo tiempo, hace menos ejercicio. Esto explica por que los protocolos medicos para diabetes incluyen siempre control de presion, control de lipidos y prescripcion de actividad fisica — no se tratan como cosas separadas porque <b>los datos muestran que no lo son</b>.",
                     d_sd["Hipertension"], d_pd["Hipertension"], d_d["Hipertension"], ratio_hta,
                     d_sd["Col. alto"], d_pd["Col. alto"], d_d["Col. alto"], ratio_chol,
                     d_sd["Act. fisica"], d_pd["Act. fisica"], d_d["Act. fisica"])
    }
    hallazgos_box(txt)
  })
  
  # ── Tab 4: Heatmap Edad x Condicion ────────────────────────
  output$viz_edad <- renderPlotly({
    grupos <- intersect(c("Sin Diabetes","Prediabetes","Diabetes"), input$sel_grupo)
    if (length(grupos) == 0) return(plot_ly() %>% layout(title="Selecciona al menos un grupo"))
    
    etiq_edad <- c("1"="18-24","2"="25-29","3"="30-34","4"="35-39","5"="40-44","6"="45-49",
                   "7"="50-54","8"="55-59","9"="60-64","10"="65-69","11"="70-74","12"="75-79","13"="80+")
    
    de <- df_raw %>% filter(as.character(Diabetes_Label) %in% grupos) %>%
      mutate(GrupoEdad = etiq_edad[as.character(Age)])
    
    pivot <- de %>% count(GrupoEdad, Diabetes_Label) %>%
      group_by(GrupoEdad) %>%
      mutate(pct = n/sum(n)*100) %>% ungroup()
    
    edades <- as.character(etiq_edad)
    z_data <- matrix(0, nrow=length(grupos), ncol=length(edades))
    rownames(z_data) <- grupos; colnames(z_data) <- edades
    
    for (i in seq_along(grupos)) {
      for (j in seq_along(edades)) {
        v <- pivot %>% filter(GrupoEdad==edades[j], as.character(Diabetes_Label)==grupos[i]) %>% pull(pct)
        z_data[i,j] <- ifelse(length(v)>0, v, 0)
      }
    }
    
    text_data <- matrix(sprintf("%.1f%%", z_data), nrow=length(grupos))
    
    plot_ly(z=z_data, x=edades, y=grupos,
            type="heatmap",
            text=text_data, texttemplate="%{text}",
            textfont=list(size=11, color="#1a1a2e"),
            colorscale=list(c(0, "#f7fafc"), c(0.3, "#fed7d7"),
                            c(0.6, "#fc8181"), c(1, "#c53030")),
            zmin=0, zmax=100,
            colorbar=list(title="% del grupo", thickness=14, ticksuffix="%"),
            hovertemplate="<b>%{y}</b><br>Edad: %{x}<br>%{z:.1f}%<extra></extra>") %>%
      layout(xaxis=list(title="Grupo de edad (anos)", tickangle=-30),
             yaxis=list(autorange="reversed"),
             plot_bgcolor="white", paper_bgcolor="white",
             font=list(family="Helvetica Neue, Helvetica, Arial, sans-serif", size=11),
             margin=list(l=120, r=20, t=20, b=70))
  })
  
  output$hallazgos_edad <- renderUI({
    grupos <- intersect(c("Sin Diabetes","Prediabetes","Diabetes"), input$sel_grupo)
    if (length(grupos) == 0) {
      return(hallazgos_box("Selecciona al menos un grupo en el filtro lateral para ver los hallazgos."))
    }
    
    etiq_edad <- c("1"="18-24","2"="25-29","3"="30-34","4"="35-39","5"="40-44","6"="45-49",
                   "7"="50-54","8"="55-59","9"="60-64","10"="65-69","11"="70-74","12"="75-79","13"="80+")
    de <- df_raw %>% filter(as.character(Diabetes_Label) %in% grupos) %>%
      mutate(GrupoEdad = etiq_edad[as.character(Age)])
    pivot <- de %>% count(GrupoEdad, Diabetes_Label) %>%
      group_by(GrupoEdad) %>%
      mutate(pct = n/sum(n)*100) %>% ungroup()
    
    pcts_edad <- list()
    for (g in grupos) {
      pcts_edad[[g]] <- setNames(
        sapply(etiq_edad, function(e) {
          v <- pivot %>% filter(GrupoEdad==e, as.character(Diabetes_Label)==g) %>% pull(pct)
          ifelse(length(v)>0, v, 0)
        }),
        etiq_edad
      )
    }
    
    n_sel <- length(grupos)
    
    if (n_sel == 1) {
      g <- grupos[1]
      txt <- sprintf("Con un solo grupo activo (<b>%s</b>), todas las celdas marcan 100%% — porque dentro de los datos filtrados, este grupo es el unico que existe. El mapa solo cobra sentido cuando hay al menos dos condiciones para comparar entre edades.<br><br>
      Activa otro grupo en el filtro para que el mapa pueda mostrar la <b>proporcion</b> de cada condicion en cada edad. Esa proporcion es la que revela el patron importante: como el riesgo cambia con los anos.", g)
    } else if (n_sel == 2) {
      g1 <- grupos[1]; g2 <- grupos[2]
      joven_1 <- pcts_edad[[g1]]["18-24"]; mayor_1 <- pcts_edad[[g1]]["80+"]
      joven_2 <- pcts_edad[[g2]]["18-24"]; mayor_2 <- pcts_edad[[g2]]["80+"]
      cambio_1 <- mayor_1 - joven_1; cambio_2 <- mayor_2 - joven_2
      txt <- sprintf("Los datos muestran como cambia la proporcion de cada grupo a lo largo de las edades:<br>
      &bull; <b>%s</b>: pasa de <b>%.1f%%</b> en 18-24 anos a <b>%.1f%%</b> en 80+. Cambio: <b>%+.1f puntos</b>.<br>
      &bull; <b>%s</b>: pasa de <b>%.1f%%</b> en 18-24 a <b>%.1f%%</b> en 80+. Cambio: <b>%+.1f puntos</b>.<br><br>
      Lo que esto dice logicamente: cuando un grupo aumenta con la edad y el otro disminuye, no es porque las personas \"cambien de grupo\" individualmente — es porque la composicion de cada generacion es diferente. Las generaciones mayores acumularon mas anos de exposicion a factores de riesgo (peso, sedentarismo, presion alta), y eso se refleja en la proporcion de quienes hoy tienen una u otra condicion.<br><br>
      Esto es importante para sistemas de salud: la edad no es solo un dato demografico, es <b>un predictor estructural</b> que permite anticipar donde poner los recursos.",
                     g1, joven_1, mayor_1, cambio_1, g2, joven_2, mayor_2, cambio_2)
    } else {
      d_24 <- pcts_edad[["Diabetes"]]["18-24"]
      d_50 <- pcts_edad[["Diabetes"]]["50-54"]
      d_65 <- pcts_edad[["Diabetes"]]["65-69"]
      d_80 <- pcts_edad[["Diabetes"]]["80+"]
      sd_24 <- pcts_edad[["Sin Diabetes"]]["18-24"]
      sd_80 <- pcts_edad[["Sin Diabetes"]]["80+"]
      ratio_d <- d_80 / max(d_24, 0.1)
      txt <- sprintf("Lo primero que salta en el mapa es que la diabetes pasa de practicamente <b>%.1f%%</b> en personas de 18-24 anos a <b>%.1f%%</b> en mayores de 80. Eso es un crecimiento de <b>%.0f veces</b>. Y en paralelo, la fila de Sin Diabetes baja de <b>%.1f%%</b> a <b>%.1f%%</b>.<br><br>
      Pero el dato mas util no son los extremos, son los puntos intermedios: a los 50-54 anos la diabetes ya esta en <b>%.1f%%</b>, y a los 65-69 sube a <b>%.1f%%</b>. Es decir, entre los 50 y los 70 anos la prevalencia <b>casi se duplica</b>. Esa franja de 20 anos es donde el riesgo se acelera mas rapido.<br><br>
      Logicamente esto significa que las campanas de tamizaje no deberian empezar a los 65 (cuando ya muchos estan enfermos), sino alrededor de los 45-50, antes de que la curva se acelere. <b>El grafico no solo dice que el riesgo crece con la edad — dice exactamente cuando empieza a crecer rapido</b>, y esa informacion es la que permite actuar a tiempo.",
                     d_24, d_80, ratio_d, sd_24, sd_80, d_50, d_65)
    }
    hallazgos_box(txt)
  })
  
  # ── Tab 5: Heatmap correlaciones ───────────────────────────
  output$viz_heatmap <- renderPlotly({
    vars      <- c("Diabetes_012","HighBP","HighChol","BMI","Smoker","PhysActivity","GenHlth","Age","Income")
    labs_vars <- c("Diabetes","Hipertension","Col. alto","BMI","Fumador","Act. fisica","Salud gral.","Edad","Ingreso")
    mat <- df_raw %>% select(all_of(vars)) %>% cor(use="complete.obs")
    colnames(mat) <- labs_vars; rownames(mat) <- labs_vars
    
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
  
  output$hallazgos_corr <- renderUI({
    vars <- c("Diabetes_012","HighBP","HighChol","BMI","Smoker","PhysActivity","GenHlth","Age","Income")
    labs_vars <- c("Diabetes","Hipertension","Col. alto","BMI","Fumador","Act. fisica","Salud gral.","Edad","Ingreso")
    mat <- df_raw %>% select(all_of(vars)) %>% cor(use="complete.obs")
    colnames(mat) <- labs_vars; rownames(mat) <- labs_vars
    
    fila_diab <- mat["Diabetes", ]; fila_diab <- fila_diab[names(fila_diab) != "Diabetes"]
    top_pos <- sort(fila_diab, decreasing=TRUE)[1:3]
    top_neg <- sort(fila_diab)[1:2]
    income_genhlth <- mat["Ingreso", "Salud gral."]
    edad_hta <- mat["Edad", "Hipertension"]
    bmi_hta <- mat["BMI", "Hipertension"]
    
    txt <- sprintf("El mapa cruza 9 variables del dataset y mide que tan fuerte van juntas. Los valores van de -1 (azul oscuro: cuando una sube, la otra baja) a 1 (rojo oscuro: suben juntas). Cero (blanco) significa que no se relacionan.<br><br>
    Lo primero que hay que notar: <b>ninguna correlacion con Diabetes pasa de 0.30</b>. Las tres mas altas son <b>%s</b> (%.2f), <b>%s</b> (%.2f) y <b>%s</b> (%.2f). Esto significa que <b>ningun factor por si solo explica la diabetes</b> — no hay una variable magica. Si fuera tan simple, ya habria una sola prueba para detectarla. La diabetes es el resultado de varios factores actuando juntos.<br><br>
    Y eso lleva al segundo hallazgo: las correlaciones <b>entre los factores</b> son a veces mas fuertes que con la diabetes misma. Por ejemplo, BMI con Hipertension correlaciona <b>%.2f</b>, y Edad con Hipertension <b>%.2f</b>. Esto explica el patron de \"comorbilidad\" que vimos en el radar — las enfermedades vienen en paquete porque <b>los factores de riesgo se refuerzan entre si</b>.<br><br>
    El dato sociopolitico esta en la esquina opuesta: <b>Ingreso vs Salud general</b> da <b>%.2f</b> (negativa). Es decir, a menor ingreso, peor salud percibida. No es una correlacion gigante, pero es consistente y conecta con lo que vimos en el tab 1 — el dinero condiciona el acceso a la salud, y eso se traduce en como la gente se siente.<br><br>
    <b>Conclusion logica:</b> el dataset no muestra una causa unica de diabetes, sino una <b>red de variables interconectadas</b>. Cualquier modelo predictivo o intervencion de salud publica que ignore esta interconexion va a fallar — porque atacar una sola variable deja a las otras compensandola.",
                   names(top_pos)[1], top_pos[1], names(top_pos)[2], top_pos[2], names(top_pos)[3], top_pos[3],
                   bmi_hta, edad_hta, income_genhlth)
    
    hallazgos_box(txt)
  })
}

shinyApp(ui=ui, server=server)