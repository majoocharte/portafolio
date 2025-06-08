# LIBRERÍAS
#----
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")}
library(plotly)
library(shiny)
library(shinydashboard)
library(deSolve)
library(ggplot2)
#----

# FUNCIONES

color_general <- "#333333"

# set y get de multipliers
# ----
mask_types <- c(
  rep("Sin cubrebocas", 3),
  rep("Cubrebocas de tela, delgado o flojo", 8),
  rep("Cubrebocas de tela, grueso y ajustado", 20),
  rep("Cubrebocas quirúrgico", 20),
  rep("Cubrebocas con filtro insertado", 15),
  rep("N95, KN95, o FFP2", 20),
  rep("N95 sellado", 8),
  rep("Respirador P100", 5)
)

vac_types <- c(
  rep("Pfizer BioNTech", 8),
  rep("Moderna", 8),
  rep("AstraZeneca", 6),
  rep("Johnson & Johnson", 4),
  rep("Sputnik V", 2),
  rep("No vaccine", 2)
)


# Vaccine effectiveness mapping
vaccine_effectiveness_map <- list(
  "Pfizer BioNTech" = c(1, 0.8, 0.25),
  "Moderna" = c(1, 0.8, 0.25),
  "AstraZeneca" = c(1, 1, 0.3),
  "Johnson & Johnson" = c(1, 0.95, 0.95),  # Single-dose vaccine
  "Sputnik V" = c(1, 0.8, 0.25),
  "No vaccine" = c(1, 1, 1)  # No reduction
)

# Function: Get vaccination effectiveness
get_vaccination_effectiveness <- function(vaccine_type, num_doses) {
  effectiveness_values <- vaccine_effectiveness_map[[vaccine_type]]
  effectiveness_values[num_doses]
}

# Function: Get mask multiplier
get_mask_multiplier <- function(mask_type, for_self = TRUE) {
  multipliers <- list(
    "Sin cubrebocas" = 1,
    "Cubrebocas de tela, delgado o flojo" = ifelse(for_self, 1, 0.5),
    "Cubrebocas de tela, grueso y ajustado" = ifelse(for_self, 2/3, 1/3),
    "Cubrebocas quirúrgico" = ifelse(for_self, 0.5, 0.25),
    "Cubrebocas con filtro insertado" = ifelse(for_self, 0.5, 0.25),
    "N95, KN95, o FFP2" = ifelse(for_self, 1/3, 1/6),
    "N95 sellado" = ifelse(for_self, 1/8, 1/16),
    "Respirador P100" = ifelse(for_self, 1/20, 1/30)
  )
  if (!mask_type %in% names(multipliers)) {
    stop("Invalid mask type provided.")
  }
  multipliers[[mask_type]]
}

# Function: Get speaking volume multiplier
get_speaking_volume_multiplier <- function(speaking_volume) {
  volume_multipliers <- list(
    "Bajo" = 0.2,
    "Normal" = 1,
    "Alto" = 5
  )
  if (!speaking_volume %in% names(volume_multipliers)) {
    stop("Invalid speaking volume provided.")
  }
  volume_multipliers[[speaking_volume]]
}

# Function: Get distance multiplier
get_distance_multiplier <- function(distance) {
  if (distance >= 3) {
    0.25  
  } else if (distance >= 2) {
    0.5
  } else if (distance >= 0.3) {
    1  
  } else if (distance > 0) {
    5
  } else {
    stop("Distance must be a positive number.")
  }
}

# Function: Get ventilation multiplier
get_ventilation_multiplier <- function(ventilation) {
  multipliers <- list(
    "Espacios bien ventilados" = 0.2,
    "Espacios con ventilación moderada" = 0.5,
    "Espacios poco ventilados" = 1
  )
  if (!ventilation %in% names(multipliers)) {
    stop("Invalid ventilation level provided.")
  }
  multipliers[[ventilation]]
}
# ---- 

# Para mask effectiveness / SE USA EN TODO MENOS LA DE MASK EFFECTIVENESS
# ----
# Function: Calculate total mask effectiveness
calculate_total_mask_effectiveness <- function(my_mask, other_masks) {
  my_mask_multiplier <- get_mask_multiplier(my_mask, for_self = TRUE)
  total_multiplier <- my_mask_multiplier
  for (mask in other_masks) {
    total_multiplier <- total_multiplier * get_mask_multiplier(mask, for_self = FALSE)
  }
  1 - total_multiplier
}
# ----

# cubrebocas 
# ----
get_cubrebocas_multiplier <- function(cubrebocas_tipo, for_self = TRUE) {
  multipliers <- list(
    "Sin cubrebocas" = 1,
    "Cubrebocas de tela, delgado o flojo" = ifelse(for_self, 1, 0.5),
    "Cubrebocas de tela, grueso y ajustado" = ifelse(for_self, 2/3, 1/3),
    "Cubrebocas quirúrgico" = ifelse(for_self, 0.5, 0.25),
    "Cubrebocas con filtro insertado" = ifelse(for_self, 0.5, 0.25),
    "N95, KN95 o FFP2" = ifelse(for_self, 1/3, 1/6),
    "N95 sellado" = ifelse(for_self, 1/8, 1/16),
    "Respirador P100" = ifelse(for_self, 1/20, 1/30)
  )
  return(multipliers[[cubrebocas_tipo]])
}

calculate_total_cubrebocas_effectiveness <- function(my_cubrebocas, other_cubrebocas) {
  my_cubrebocas_multiplier <- get_cubrebocas_multiplier(my_cubrebocas, for_self = TRUE)
  total_multiplier <- my_cubrebocas_multiplier
  for (cubrebocas in other_cubrebocas) {
    their_cubrebocas_multiplier <- get_cubrebocas_multiplier(cubrebocas, for_self = FALSE)
    total_multiplier <- total_multiplier * their_cubrebocas_multiplier
  }
  cubrebocas_effectiveness <- 1 - total_multiplier
  return(cubrebocas_effectiveness)
}

# ----


# Para el SIR seccionado: 
# función "calculate_covid_prob_yes_vacc_yes_mask"
# ----
calculate_covid_prob_yes_vacc_yes_mask <- function(infected_vector, susceptibles_vector) {
  #infected 
  last_7 <- tail(na.omit(infected_vector), 7)
  infected_lastweek <- sum(last_7)
  
  #susceptibles
  s_sin_na <- na.omit(susceptibles_vector)
  s_sin_na_ni_ceros <- s_sin_na[s_sin_na != 0]
  susceptibles <- tail(s_sin_na_ni_ceros,1)
  
  # other people
  num_people <- sample(1:50, 1)
  
  #mask
  my_mask <- sample(mask_types, 1)
  other_masks <- sample(mask_types, num_people)
  
  speaking_volume <- sample(c("Bajo", "Normal", "Alto"), 1)
  distance <- sample(0.1:3, 1)
  duration_hours <- sample(0:500, 1)/60
  ventilation <- sample(c("Espacios bien ventilados", "Espacios con ventilación moderada", "Espacios poco ventilados"), 1)
  
  if (susceptibles > 0) {
    local_prevalence <- infected_lastweek / susceptibles
  } else {
    print("susceptibles 0 o negativo")
    local_prevalence <- 0.1
  }
  if (local_prevalence== 0){
    local_prevalence <- 1
  }
  
  total_mask_effectiveness <- calculate_total_mask_effectiveness(my_mask, other_masks)
  
  # Get multipliers
  ventilation_multiplier <- get_ventilation_multiplier(ventilation)
  distance_multiplier <- get_distance_multiplier(distance)
  speaking_volume_multiplier <- get_speaking_volume_multiplier(speaking_volume)
  
  #vaccines
  vaccine_type <- sample(vac_types, 1)
  num_doses <- sample(c(1,2,2,3,3,3), 1) 
  vaccination_effectiveness <- get_vaccination_effectiveness(vaccine_type, num_doses)
  
  # Total risk in microCOVIDs
  person_risk <- sample(1:10, 1) * num_people * local_prevalence * duration_hours 
  print("person risk")
  print(person_risk)
  
  if (any(is.na(c(speaking_volume_multiplier, distance_multiplier, ventilation_multiplier, total_mask_effectiveness, vaccination_effectiveness)))) {
    cat("NA detected in the following terms:\n")
    if (is.na(speaking_volume_multiplier)) cat(" - speaking_volume_multiplier\n")
    if (is.na(distance_multiplier)) cat(" - distance_multiplier\n")
    if (is.na(ventilation_multiplier)) cat(" - ventilation_multiplier\n")
    if (is.na(total_mask_effectiveness)) cat(" - total_mask_effectiveness\n")
    if (is.na(vaccination_effectiveness)) cat(" - vaccination_effectiveness\n")
  } else {
    risk_reduction <- speaking_volume_multiplier * distance_multiplier * ventilation_multiplier * total_mask_effectiveness * vaccination_effectiveness
    print("Risk reduction:")
    print(risk_reduction)
  }
  
  microcovids <- person_risk * risk_reduction
  print("microcovids")
  print(microcovids)
  
  if (microcovids == 0){
    print("local prevalance")
    print(local_prevalence)
    print("infectados")
    print(infected_lastweek)
    print("susceptibles")
    print(susceptibles)
  }
  
  probability <- microcovids/1000000
  
  if (probability >= 1){
    probability <- 1
  }
  if (probability <= 0){
    probability <- 0
  }
  print("probability")
  #print(probability)
  return(probability)  
}

# ----

# función "calculate_covid_prob_no_vacc_yes_mask"
# ----
calculate_covid_prob_no_vacc_yes_mask <- function(infected_vector, susceptibles_vector) {
  #infected 
  last_7 <- tail(na.omit(infected_vector), 7)
  infected_lastweek <- sum(last_7)
  
  #susceptibles
  s_sin_na <- na.omit(susceptibles_vector)
  s_sin_na_ni_ceros <- s_sin_na[s_sin_na != 0]
  susceptibles <- tail(s_sin_na_ni_ceros,1)
  
  # other people
  num_people <- sample(1:50, 1)
  
  #mask
  my_mask <- sample(mask_types, 1)
  other_masks <- sample(mask_types, num_people)
  
  speaking_volume <- sample(c("Bajo", "Normal", "Alto"), 1)
  distance <- sample(0.1:3, 1)
  duration_hours <- sample(0:500, 1)/60
  ventilation <- sample(c("Espacios bien ventilados", "Espacios con ventilación moderada", "Espacios poco ventilados"), 1)
  
  if (susceptibles > 0) {
    local_prevalence <- infected_lastweek / susceptibles
  } else {
    print("susceptibles 0 o negativo")
    local_prevalence <- 0.1
  }
  if (local_prevalence== 0){
    local_prevalence <- 1
  }
  
  total_mask_effectiveness <- calculate_total_mask_effectiveness(my_mask, other_masks)
  
  # Get multipliers
  ventilation_multiplier <- get_ventilation_multiplier(ventilation)
  distance_multiplier <- get_distance_multiplier(distance)
  speaking_volume_multiplier <- get_speaking_volume_multiplier(speaking_volume)
  
  # Total risk in microCOVIDs
  person_risk <- sample(1:20, 1) * num_people * local_prevalence * duration_hours 
  #print("person risk")
  #print(person_risk)
  
  risk_reduction <- speaking_volume_multiplier * distance_multiplier * ventilation_multiplier * total_mask_effectiveness
  #print("risk reduction")
  #print(risk_reduction)
  
  microcovids <- person_risk * risk_reduction
  print("microcovids")
  print(microcovids)
  
  if (microcovids == 0){
    print("local prevalance")
    print(local_prevalence)
    print("infectados")
    print(infected_lastweek)
    print("susceptibles")
    print(susceptibles)
  }
  
  probability <- microcovids/1000000
  
  if (probability >= 1){
    probability <- 1
  }
  if (probability <= 0){
    probability <- 0
  }
  print("probability")
  print(probability)
  return(probability)  
}
# ----

# función "calculate_covid_prob_no_vacc_no_mask"
# ----
calculate_covid_prob_no_vacc_no_mask <- function(infected_vector, susceptibles_vector) {
  
  #infected 
  last_7 <- tail(na.omit(infected_vector), 7)
  infected_lastweek <- sum(last_7)
  #susceptibles
  s_sin_na <- na.omit(susceptibles_vector)
  s_sin_na_ni_ceros <- s_sin_na[s_sin_na != 0]
  susceptibles <- tail(s_sin_na_ni_ceros,1)
  print("susceptibles")
  print(susceptibles)
  
  # other people
  num_people <- sample(1:50, 1)
  
  speaking_volume <- sample(c("Bajo", "Normal", "Alto"), 1)
  distance <- sample(0.1:1.5, 1)
  duration_hours <- sample(0:10, 1)
  ventilation <- sample(c("Espacios bien ventilados", "Espacios con ventilación moderada", "Espacios poco ventilados"), 1)
  
  if (susceptibles > 0) {
    local_prevalence <- infected_lastweek / susceptibles
  } else {
    print("susceptibles 0 o negativo")
    local_prevalence <- 0.1
  }
  if (local_prevalence== 0){
    local_prevalence <- 1
  }
  
  # Get multipliers
  ventilation_multiplier <- get_ventilation_multiplier(ventilation)
  distance_multiplier <- get_distance_multiplier(distance)
  speaking_volume_multiplier <- get_speaking_volume_multiplier(speaking_volume)
  
  # Total risk in microCOVIDs
  person_risk <- sample(1:35, 1) * num_people * local_prevalence * duration_hours 
  print("person risk")
  print(person_risk)
  
  risk_reduction <- speaking_volume_multiplier * distance_multiplier * ventilation_multiplier 
  print("risk reduction")
  print(risk_reduction)
  
  microcovids <- person_risk * risk_reduction
  print("microcovids")
  print(microcovids)
  
  if (microcovids == 0){
    print("local prevalance")
    print(local_prevalence)
    print("infectados")
    print(infected_lastweek)
    print("susceptibles")
    print(susceptibles)
  }
  
  probability <- microcovids/1000000
  
  if (probability >= 1){
    probability <- 1
  }
  if (probability <= 0){
    probability <- 0
  }
  print("probability")
  print(probability)
  return(probability)  
}
ed.sol <- function(t, state, parms, u, N, B, yL) {
  with(as.list(state), {
    dxdt <- rep(0, length(state))
    dxdt[1] <- u * N - B * I * (S / N) - u * S
    dxdt[2] <- B * I * (S / N) - yL * I - u * I
    dxdt[3] <- yL * I - u * R
    return(list(dxdt))
  })
}
# ----


# INTERFAZ
ui <- dashboardPage( 
  # definir pestañas 
  # ----
  skin = "purple",
  dashboardHeader(title = "Modelo SIR"), # título del shiny
  dashboardSidebar( # manejar la sidebar del shiny
    sidebarMenu( # manejar los elementos del menú lateral
      # agregar cada ítem
      menuItem(" Inicio", tabName= "inicio", icon = icon("home")),
      menuItem(" Modelo SIR", tabName = "pestaña5", icon = icon("chart-line")),
      menuItem(" Our World in Data", tabName = "pestaña1", icon = icon("globe")),
      menuItem(" Efectividad de tu Cubrebocas", tabName = "pestaña2", icon = icon("mask-face")),
      menuItem(" Probabilidad Individual", tabName = "pestaña3", icon = icon("chart-bar")),
      menuItem(" Todos como tú", tabName = "pestaña4", icon = icon("user")),
      menuItem(" Modelo SIR Estocástico", tabName = "pestaña6", icon = icon("chart-line")),
      menuItem(" Comparativa", tabName = "pestaña7", icon = icon("pie-chart")), 
      menuItem(" Conclusiones", tabName = "pestaña8", icon = icon("lightbulb")), 
      menuItem(" Referencias", tabName = "pestaña9", icon = icon("clipboard-list"))
    )
  ),
  #----  
  
  # definir el cuerpo de la app
  # ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$style(HTML("
        .typewriter {
          font-family: 'Courier New', Courier, monospace;
          overflow: hidden;
          border-right: .15em solid black;
          white-space: nowrap;
          animation: typing 4s steps(30, end), blink-caret .75s step-end infinite;
        }
        
        @keyframes typing {
          from { width: 0 }
          to { width: 100% }
        }

        @keyframes blink-caret {
          from, to { border-color: transparent }
          50% { border-color: black }
        }
      "))
    ),
  # ----
  
    # manejar las tabs
    tabItems(

      # pestaña de inicio
      #----
      # pestaña de inicio
      tabItem(tabName = "inicio", div( class = "typewriter", h2("Simulador de Epidemia: Modelo SIR",
                                     align = "center", 
                                     style = "font-size: 36px; font-weight: bold;")),
              h4(strong("Comparativa de modelos con ecuaciones diferenciales y modelos estocásticos "), align = "center"),
              p("En esta página web encontrará el modelo SIR con diversas modificaciones modelando la enfermedad infecciosa del SARS-COV-2, es decir, la pandemia del covid-19; con el objetivo de visualizar como las acciones que toma la población ante un virus pueden cambiar el curso de una pandemia.",align = "center"),
              tags$img(
                src = "homepage.png", 
                height = "350px", 
                width = "800px", 
                alt = "COVID-19", 
                style = "display: block; margin-left: auto; margin-right: auto;"
              )     
      ),  
      #----
      
      # primera pestaña / OWID
      # ----     
      # primera pestaña
      tabItem(
        tabName = "pestaña1",
        # Título principal
        h2("Our World in Data", 
           align = "center", 
           style = "font-size: 36px; font-weight: bold;"),
        
        # Subtítulo
        h5(strong("¿Si una imagen dice más que mil palabras, una gráfica dirá más que un millón de datos?"), 
           align = "center", 
           style = "font-size: 18px;"),
        
        # Párrafo introductorio
        p("Si se observan los distintos gráficos de la página web", em("Our World in Data"), "se observará que aunque durante la pandemia del covid-19 fuera el mismo virus afectando a la misma raza humana... las tasas de infección, mortalidad, propagación e inclusive vacunación fueron distintas alrededor del mundo... ¿Por qué?"),
        p("Estas diferencias pueden explicarse por una combinación de factores biológicos, sociales, económicos, culturales y políticos."),
        
        # Lista de factores
        tags$ul(class = "custom-list",
                tags$li("Factores biológicos como el estado de salud preexistente, en donde poblaciones con mayores tasas de enfermedades crónicas, como diabetes, obesidad o hipertensión, fueron más vulnerables."),
                tags$li("Factores Sociales y Culturales: Algunos de ellos son, la densidad poblacional, ya que en regiones más densamente pobladas como grandes ciudades, se enfrentó una propagación más rápida del virus. Estilos de vida y prácticas culturales, con costumbres como la cercanía física al interactuar o la frecuencia de reuniones sociales influyendo en la transmisión. La desigualdad social, donde las comunidades más vulnerables con acceso limitado a servicios de salud, sufrieron más gravemente."),
                tags$li("Factores Económicos: Países con sistemas de salud bien financiados pudieron diagnosticar, tratar y prevenir de manera más eficaz, mientras que países con sistemas deficientes no. La capacidad económica de cada país afectó la rapidez con la que los gobiernos adquirieron vacunas y aplicaron medidas de mitigación como cuarentenas o distribución de mascarillas."),
                tags$li("Factores Políticos: La respuesta de los gobiernos a la pandemia varió enormemente, algunos implementaron restricciones estrictas y efectivas mientras que otros minimizaron la gravedad del virus. Además, el uso de mensajes claros y estrategias de información influyeron en la adherencia de la población a las medidas preventivas."),
                tags$li("Factores de Vacunación: Las vacunas llegaron de manera desigual a los países, siendo los más ricos los primeros en acceder a ellas; sin embargo, las tasas de vacunación estuvieron afectadas por la confianza en las vacunas y por movimientos antivacunas en algunas regiones.")
        ),
        
        # Conclusión
        br(),
        strong("Estas diferencias reflejan cómo un evento global puede tener impactos significativamente distintos dependiendo del contexto local. Entender estos factores es esencial para prepararse mejor ante futuras pandemias."),
        br(), br(),
        
        # Espacio adicional
        br(),
        
        # Iframe para integrar página web
        tags$iframe(
          src = "https://ourworldindata.org/coronavirus",
          height = "600px",
          width = "100%",
          style = "border:2px solid #f0f4f4;",  # borde
          scrolling = "auto"
        )
      ),
      
      # ---- 
      
      # segunda pestaña / CUBREBOCAS
      #----
      tabItem(tabName = "pestaña2", div( class = "typewriter", h2("Efectividad del uso del cubrebocas",
                                                                  align = "center", 
                                                                  style = "font-size: 36px; font-weight: bold;")),
              br(),
              fluidRow(
                box(
                  title = "Impacto del Cubrebocas en la Propagación del Virus",
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  width = 12,
                  p("El SARS-CoV-2 se propaga principalmente a través de gotículas respiratorias y aerosoles. Los cubrebocas reducen la expulsión de partículas infectadas al hablar, toser o estornudar, mientras que limitan la inhalación de partículas contaminadas. Cuando una gran parte de la población usa cubrebocas, el efecto protector se amplifica reduciendo significativamente las tasas de transmisión."),
                  p("En algunos países, el uso de cubrebocas fue obligatorio en espacios públicos y cerrados desde el principio, mientras que otros tardaron en implementarlo o no lo exigieron. Al inicio de la pandemia algunos lugares enfrentaron escasez de cubrebocas, lo que limitó su adopción especialmente en países de bajos ingresos.")
                )
              ),
              p(strong(em("¿Y en tu caso, qué tan fuerte era tu protección?")), 
                style = "font-size: 16px;"),
              p("En una interacción normal, ingresa el tipo de cubrebocas que usabas tú y el que usaban las otras personas a tu alrededor."), br(),
              
              div(class = "custom-panel",
                  fluidPage(
                    titlePanel("Calculadora de efectividad de cubrebocas"),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("my_cubrebocas", "Selecciona tu cubrebocas:", 
                                    choices = c("Sin cubrebocas", 
                                                "Cubrebocas de tela, delgado o flojo", 
                                                "Cubrebocas de tela, grueso y ajustado", 
                                                "Cubrebocas quirúrgico", 
                                                "Cubrebocas con filtro insertado", 
                                                "N95, KN95 o FFP2", 
                                                "N95 sellado", 
                                                "Respirador P100")),
                        numericInput("num_people", "¿Con cuántas personas interactúas?", value = 1, min = 1),
                        uiOutput("other_cubrebocas_ui"),
                        actionButton("calculate", "Calcular efectividad")
                      ),
                      
                      mainPanel(
                        fluidRow(
                          # Primera columna
                          column(
                            width = 6,  # Ajusta al 50% del ancho
                            p("Tu cubrebocas es este:", style = "font-size: 16px; font-weight: bold;"),  # Texto más grande y en negritas
                            uiOutput("imagen_dinamica")  # Imagen del cubrebocas
                          ),
                          # Segunda columna
                          column(
                            width = 6,  # Ajusta al 50% del ancho
                            uiOutput("result", style = "font-size: 16px; font-weight: bold;"),  # Resultado más grande y en negritas
                            uiOutput("riesgo_dinamica")
                          )
                        )
                      )
                    )
                  )
              )
      ),
      
      #----
      
      # tercera pestaña / p indv
      # ----
      tabItem(tabName = "pestaña3", h2("Probabilidad Individual de infección",
                                       align = "center", 
                                       style = "font-size: 36px; font-weight: bold;"),
              br(),
                box(
                  title = "Cálculo del Riesgo de una Actividad",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  p(HTML("<b>Para calcular el riesgo de una actividad:</b>")),
                  p("Dado el número de casos activos en el área, se calcula la probabilidad de que una persona con la que convives esté infectada. 
    Después, se ajusta con el número de personas con la que se esté realizando una actividad, mientras más personas estén involucradas mayor será el riesgo. 
    Finalmente, se toma en cuenta el tiempo que la actividad se lleve a cabo, cuanto más tiempo se exponga mayor será el riesgo."),
                  br(),
                  p(HTML("<b>Para ajustarlo con las medidas de prevención:</b>")),
                  p("Para ello se ajusta el tipo de espacio en que se lleva a cabo la actividad. Si es una actividad en el interior o con poca ventilación, 
    la probabilidad de transmisión sube, mientras que hacerlo en espacios abiertos reduce el riesgo. 
    También se ajusta la distancia física entre las personas, pues la cercanía aumenta la exposición. Continuando, se considera el uso del cubrebocas, pues estos reducen la cantidad de partículas virales en el aire. 
    Y por último, consideramos si estás vacunado, lo que reduce el riesgo considerablemente.")
                ),
              fluidRow(
                column(width = 7,
                       p(HTML("<span style='font-size: 16px;'>Para calcular qué tan probable es que una persona se infecte realizando cierta actividad, el modelo calcula el riesgo de que esta produzca un contagio y luego se ajusta con las medidas de prevención que se tomen; basado en el modelo de </span>"), 
                         a(HTML("<b>microcovid.org</b>"), href = "https://www.microcovid.org", style = "font-size: 16px;"))),
                column(width = 5,
                       box( title = "Microcovid", status = "primary", collapsible = FALSE, solidHeader = TRUE, width = "100%",
                            withMathJax(), p("$$1 
                                             microcovid = \\frac{1}{1,000,000}$$"))
                )),
              fluidRow(
                column(
                  width = 2 # Offset izquierdo para centrar
                ),
                column(
                  width = 4,
                  p("Seleccione los Datos", style = "font-size: 16px; font-weight: bold;"),
                  selectInput("local_prevalence", "Riesgo local",
                              choices = c("Riesgo bajo", "Riesgo medio", "Riesgo alto", "Riesgo extremo")),
                  
                  selectInput("vaccine_type", "Tipo de Vacuna:",
                              choices = names(vaccine_effectiveness_map)),
                  
                  numericInput("num_doses", "Número de Dosis:", value = 2, min = 1, step = 1, max = 3),
                  
                  selectInput("my_cubrebocas", "Selecciona tu cubrebocas:", 
                              choices = c("Sin cubrebocas", 
                                          "Cubrebocas de tela, delgado o flojo", 
                                          "Cubrebocas de tela, grueso y ajustado", 
                                          "Cubrebocas quirúrgico", 
                                          "Cubrebocas con filtro insertado", 
                                          "N95, KN95 o FFP2", 
                                          "N95 sellado", 
                                          "Respirador P100")),
                  
                  sliderInput("num_people2", "¿Con cuántas personas interactúas?", value = 1, min = 1, max = 10),
                  uiOutput("other_cubrebocas_ui2"),
                  
                  selectInput("speaking_volume", "Volumen al Hablar:",
                              choices = c("Bajo", "Normal", "Alto")),
                  
                  sliderInput("distance", "Distancia (metros):",
                              min = 0.1, max = 10, value = 2, step = 0.1),
                  
                  sliderInput("duration_hours", "Duración (horas):",
                              min = 0, max = 24, value = 1, step = 0.5),
                  
                  selectInput("ventilation", "Ventilación:",
                              choices = c("Espacios bien ventilados", "Espacios con ventilación moderada", "Espacios poco ventilados")),
                  
                  actionButton("calculate_btn", "Calcular Riesgo", icon = icon("calculator"))
                ),
                
                column(
                  width = 4,
                  p("Riesgo Calculado:", style = "font-size: 16px; font-weight: bold;"),
                  uiOutput("microcovid_result"),
                  uiOutput("microcovid_dinamica")
                ),
                column(
                  width = 2 # Offset derecho para centrar
                )
              )
              
              
      ),#tabItem3
      # ----
      
      # cuarta pestaña / todos como tú
      #----
      
      tabItem(tabName = "pestaña4", div( class = "typewriter", h2("Todos como tú",
                                                                  align = "center", 
                                                                  style = "font-size: 36px; font-weight: bold;")),
              tabItem(tabName = "pestaña4",
                      
                      br(),fluidRow(
                        box(
                          title = "Información del modelo",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = TRUE,
                          strong("Para observar cómo hubiera sido la pandemia si toda la población tuviera el mismo comportamiento, ingresa una probabilidad de infección. Esta probabilidad será la misma para todos los individuos."),
                          br(), br(),
                          "Ajusta el resto de los parámetros para ver qué pasa... notarás que si la probabilidad de infección y la probabilidad de muerte a causa de la enfermedad fueran altas, la pandemia hubiera acabado demasiado rápido, pues la población viva desaparecería velozmente, dejando al virus sin huéspedes. En consecuencia, el virus también desaparecería, pues necesitan organismos vivos para replicarse y sobrevivir.",
                          br(), br(),
                          "En esta simulación, la probabilidad de que una persona infectada del virus muera se mantiene constante a lo largo de todo el modelo y es la misma para todos los individuos, obviando la capacidad de los hospitales, el efecto de las enfermedades crónicas en la probabilidad de recuperarse, la calidad del sistema de salud en sus comunidades, etc.",
                          br(), br(),
                          "Además, en este modelo las personas infectadas permanecen infectadas 7 días, y en cada uno enfrentan la posibilidad de morir. Si lograron sobrevivir, se pasarán al grupo de recuperados, en donde tendrán una inmunidad de 60 días antes de ser susceptibles de nuevo."
                        )
                      ), 
                      fluidRow(
                        box(
                          title = "Nota adicional",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = TRUE,
                          "La virulencia es una medida de la gravedad o el daño que un patógeno (como un virus, bacteria o parásito) causa en su hospedador. En términos simples, se refiere a qué tan dañino o agresivo es un microorganismo al causar una enfermedad. Un patógeno altamente virulento puede provocar síntomas graves o incluso la muerte, mientras que uno de baja virulencia puede causar síntomas leves o ser asintomático. La virulencia depende de factores como la capacidad del patógeno para invadir tejidos, evadir el sistema inmunológico o liberar toxinas.",
                          br(), br(),
                          "La relación entre la virulencia de un virus y su capacidad de propagación en una población es clave en la epidemiología. Según la hipótesis del equilibrio entre virulencia y transmisión, los patógenos enfrentan un dilema evolutivo: causar enfermedades graves (alta virulencia) puede reducir la transmisión, ya que los hospedadores mueren rápidamente, mientras que una virulencia baja permite que los hospedadores permanezcan activos por más tiempo, favoreciendo la propagación. Así, los patógenos muy letales suelen propagarse menos, mientras que los menos mortales pueden difundirse más ampliamente.",
                          br(), br(),
                          "Sin embargo, esta relación no siempre es sencilla y depende de factores como la inmunidad del hospedador, la velocidad de replicación del patógeno y las condiciones ambientales. Aunque en general los patógenos muy virulentos tienen menor transmisión, hay excepciones según las características específicas de cada caso. Comprender este equilibrio es esencial para predecir brotes y diseñar estrategias de salud pública que controlen la propagación de enfermedades infecciosas."
                        )
                      ), 
                      fluidRow(
                        column(
                          width = 3,  # Mitad de la fila para los sliders
                          h4("Parámetros del modelo", style = "color: #2C3E50; font-weight: bold;"),
                          numericInput("prob_s_i", "Probabilidad de infección", 
                                      min = 0.001, max = 1, value = 0.1, step = 0.01),
                          sliderInput("prob_i_m", "Probabilidad de muerte", 
                                      min = 0, max = 1, value = 0.01, step = 0.01),
                          sliderInput("dias", "Número de días de simulación:", 
                                      min = 1, max = 1000, value = 300, step = 100)
                        ),
                        column(
                          width = 9,
                          h4("Gráfica de Simulación modelo SIR", style = "color: #2C3E50; font-weight: bold;"),
                          tags$div(
                            plotlyOutput("graficaTodosComoTu", height = "400px"),
                            style = "border: 2px solid #2C3E50; padding: 0px;"  
                          )
                        )
                      )
              )
      ),#tabItem4
      #----
      
      # quinta pestaña  / SIR
      # ----
      tabItem(tabName = "pestaña5", div( class = "typewriter", h2("Modelo SIR como un sistema de ecuaciones diferenciales",
                                                                   align = "center", 
                                                                   style = "font-size: 36px; font-weight: bold;")),
              
              # Box: Introducción al Modelo SIR
              br(),box(
                title = "Información del modelo SIR",
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                collapsed = TRUE,
                width = 12,
                p("El modelo SIR es un modelo matemático utilizado para describir la propagación de enfermedades infecciosas en una población. Es uno de los modelos más básicos y ampliamente utilizados en epidemiología. Se denomina SIR porque divide la población en tres grupos:"),
                p(strong("S: Susceptibles")),
                p("Representa la fracción de la población que es vulnerable a la enfermedad, es decir, las personas que pueden contraerla si entran en contacto con un individuo infectado."),
                p(strong("I: Infectados")),
                p("Representa la fracción de la población que está infectada y puede transmitir la enfermedad a los susceptibles."),
                p(strong("R: Recuperados o removidos")),
                p("Representa la fracción de la población que ya no puede contraer ni transmitir la enfermedad, ya sea porque se han recuperado y adquirido inmunidad o porque han fallecido."),
                br(),
                p("El modelo SIR se basa en un sistema de ecuaciones diferenciales ordinarias que describe cómo las tres categorías cambian con el tiempo."),
                tags$img(src = "ecuaciones.png", height = "300px", style = "display: block; margin-left: auto; margin-right: auto;")
              ),
              
              # Box: Dinámica del Modelo SIR
              box(
                title = "Dinámica del Modelo",
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE, 
                collapsed = TRUE,
                width = 12,
                p("El modelo SIR se basa en un sistema de ecuaciones diferenciales ordinarias que describe cómo las tres categorías cambian con el tiempo:"),
                
                p(strong("Tasa de cambio de los susceptibles")),
                tags$img(src = "ecuacions.png", height = "80px", style = "display: block; margin-left: auto; margin-right: auto;"),
                p("β es la tasa de contacto o transmisión, que indica la probabilidad de que un susceptible se infecte al entrar en contacto con un infectado."),
                
                p(strong("Tasa de cambio de los infectados")),
                tags$img(src = "ecuacioni.png", height = "80px", style = "display: block; margin-left: auto; margin-right: auto;"),
                p("La primera parte representa el crecimiento de infectados debido al contacto entre susceptibles e infectados. La segunda parte representa la disminución de infectados debido a la recuperación o fallecimiento."),
                
                p(strong("Tasa de cambio de los recuperados")),
                tags$img(src = "ecuacionr.png", height = "80px", style = "display: block; margin-left: auto; margin-right: auto;"),
                p("γ es la tasa de recuperación o el inverso del período infeccioso promedio.")
              ),
              
              # Box: Suposiciones del Modelo SIR
              box(
                title = "Suposiciones del Modelo SIR",
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE, 
                collapsed = TRUE,
                width = 12,
                p("La población total N = S + I + R es constante y no cambia a pesar de los nacimientos o muertes que ocurran."),
                p("Todos los individuos en la población tienen la misma probabilidad de entrar en contacto con otros."),
                p("La enfermedad otorga inmunidad permanente una vez que una persona se recupera."),
                p("No hay demora en el tiempo de infección o recuperación.")
              ),
              
              # Box: R0 - Número básico de reproducción
              box(
                title = "R0",
                status = "primary", 
                solidHeader = TRUE, 
                collapsible = TRUE, 
                width = 12,
                p("R0, es el número básico de reproducción, un valor importante que se calcula como R0 = beta/gamma. Representa el número promedio de nuevas infecciones que un individuo infectado puede causar en una población completamente susceptible."),
                p("Si R0 es mayor a uno, la enfermedad se propaga. Si R0 es menor a uno, la enfermedad eventualmente desaparecerá.")
              ),
              
              
              #contenido
              sidebarLayout(
                sidebarPanel(
                  tags$div(  # Estilo del panel lateral
                    h3("Parámetros del Modelo", style = "color: #2C3E50; font-weight: bold;"),  # Título estilizado
                    sliderInput("S", "Susceptibles inciales", min = 100, max = 1000000, value = 100000, step = 50000),
                    sliderInput("I", "Infectados iniciales", min = 100, max = 700000, value = 300, step = 100),
                    sliderInput("R", "Removidos iniciales", min = 1, max = 70000, value = 30, step = 100),
                    sliderInput("t", "Días a modelar", min = 0, max = 500, value = 150, step = 1),
                    sliderInput("L", "Días que el paciente es contagioso", min = 0, max = 100, value = 10, step = 1),
                    sliderInput("u", "u", min = 0, max = .1, value = 0.001, step = 0.00001),
                    sliderInput("B", "Tasa de transmisibilidad B", min = 0, max = 100, value = 0.5, step = .5)
                  )
                ),
                mainPanel(# Título estilizado
                  tags$div(
                    h3("Gráfica del Modelo SIR", style = "color: #2C3E50; font-weight: bold;"),
                    plotlyOutput("plotSIR"),
                    br(),
                    uiOutput("rotexto")
                  )
                )
              )
      ), 
      #----
      
      # sexta pestaña 
      # -----
      tabItem(tabName = "pestaña6" , div( class = "typewriter", h2("Modelo SIR estocástico",
                                                                    align = "center", 
                                                                    style = "font-size: 36px; font-weight: bold;")),
              br(),fluidRow(
                box(
                  title = "Información del modelo",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE,
                  "Contemplando que la población necesitó un tiempo para empezar a usar el cubrebocas, y que las vacunas tardaron en desarrollarse, en esta adaptación del modelo SIR se crean tres etapas. En la primera no se toma ninguna medida de prevención, en la segunda se usan cubrebocas y en la tercera la población está vacunada.",
                  br(), br(),
                  "En esta simulación, la probabilidad de que una persona infectada del virus muera se mantiene constante a lo largo de todo el modelo y es la misma para todos los individuos, obviando la capacidad de los hospitales, el efecto de las enfermedades crónicas en la probabilidad de recuperarse, la calidad del sistema de salud en sus comunidades, etc.",
                  br(), br(),
                  "Además, en este modelo las personas infectadas permanecen infectadas 7 días, y en cada uno enfrentan la posibilidad de morir. Si lograron sobrevivir, se pasarán al grupo de recuperados, en donde tendrán una inmunidad de 60 días antes de ser susceptibles de nuevo.",
                  br(), br(),
                  strong("En esta simulación, se modela por separado la probabilidad de infección de cada persona, contemplando que todos hacen una actividad distinta en su día.")
                )
              ),
              fluidRow(
                column(
                  width = 3,  # Mitad de la fila para los sliders
                  h4("Parámetros del modelo", style = "color: #2C3E50; font-weight: bold;"),
                  sliderInput("i_t_sin_medidas", 
                              "Días a modelar sin medidas preventivas",
                              min = 1, max = 500, value = 100, step = 50),
                  sliderInput("i_t_con_cubrebocas", 
                              "Días a modelar con el uso del cubrebocas",
                              min = 1, max = 500, value = 100, step = 50),
                  sliderInput("i_t_con_vacunas", 
                              "Días a modelar con vacunas",
                              min = 1, max = 500, value = 100, step = 50),
                  sliderInput("prob_i_m", 
                              "Probabilidad de muerte",
                              min = 0.001, max = 0.1, value = 0.005, step = 0.001)
                ),
                column(
                  width = 9,
                  h4("Gráfica de Simulación modelo SIR", style = "color: #2C3E50; font-weight: bold;"),
                  tags$div(
                    plotlyOutput("graficaSimulacion", height = "400px"),
                    style = "border: 2px solid #2C3E50; padding: 0px;"  
                  )
                )
                
              ) # fluid row
      ), # tab item
      
      # ----

      #séptima pestaña sir comp 
      #----      
      tabItem( tabName = "pestaña7", div( class = "typewriter", h2("Comparación de modelos",
                                                                   align = "center", 
                                                                   style = "font-size: 36px; font-weight: bold;")),
          # primera columna (izquierda)
          fluidRow(
          column( width = 4, 
            # primera box
            fluidRow( h4("\b \b \b Parámetros del Modelo", style = "color: #2C3E50; font-size: 18px; font-weight: bold;"),
              box( 
                title = "Condiciones compartidas", collapsible = TRUE, status = "primary",
                solidHeader = TRUE, width = 12,
                sliderInput("i_t_sin_medidasC", 
                            "Días a modelar sin medidas preventivas",
                            min = 1, max = 500, value = 100, step = 50),
                sliderInput("i_t_con_cubrebocasC", 
                            "Días a modelar con el uso del cubrebocas",
                            min = 1, max = 500, value = 100, step = 50),
                sliderInput("i_t_con_vacunasC", 
                            "Días a modelar con vacunas",
                            min = 1, max = 500, value = 100, step = 50),
                sliderInput("prob_i_mC", 
                            "Probabilidad de muerte",
                            min = 0.001, max = 0.1, value = 0.005, step = 0.001)
              )
            ),
            # segunda box
            fluidRow(
              box(
                title = "Condiciones Modelo ODE", status = "primary", collapsible = TRUE,
                solidHeader = TRUE, width = 12,
                sliderInput("gamma1",
                            "Beta Etapa 1",
                            min = 0, max = 1, step = 0.01, value = 0.5),
                sliderInput("gamma2",
                            "Beta Etapa 2",
                            min = 0, max = 1, step = 0.01, value = 0.5),
                sliderInput("gamma3",
                            "Beta Etapa 3",
                            min = 0, max = 1, step = 0.01, value = 0.5)
              )
            )
          ),#columna izquierda
          
          # segunda columna (derecha)
          column( width = 8, offset = 0,
                  h4("Modelo SIR Estocástico", style = "color: #2C3E50; font-weight: bold;"),
                  tags$div(
                    plotlyOutput("graficaSimulacionE", height = "250px", width = "100%"),
                    style = "border: 2px solid #2C3E50; padding: 0px;"
                    ),
                    p(""), br(),
                
                  h4("Modelo SIR con ODE", style = "color: #2C3E50; font-weight: bold;"),
                  tags$div(
                    plotlyOutput("graficaSimulacionODE", height = "250px", width = "100%"),
                    style = "border: 2px solid #2C3E50; padding: 0px;"
                    ),
                  p("")
              ),
      )#todo a un fluidRow
      ), #tabItem
      
      #----
      
      
      # pestaña 8 de conclusiones
      #----
      tabItem(tabName = "pestaña8", div( class = "typewriter", h2("Conclusiones",
                                                                align = "center", 
                                                                style = "font-size: 36px; font-weight: bold;")),
              h4(strong("Comparativa de modelos con ecuaciones diferenciales y modelos estocásticos "), align = "center"),
              p("Ambos modelos son efectivos para modelar enfermedades infecciosas. Sin embargo, la decisión de cuál usar dependerá de la finalidad con la que se usan los modelos. El modelo con ecuaciones diferenciales es últil pues es fácil de entender y tiene posibilidades infinitas para modelar las ecuaciones. Por el otro lado, el modelo estocástico es útil para modelar todos los casos especificos de cada persona en una sola gráfica, tomando en cuenta como factores como el disntanciamiento social, el uso del cubrebocas, la ventilación de los espacios, etc, tienen un impacto en la transmición del virus; este modelo también tiene posibilidades infinitas de mejora. ",align = "center"),
              tags$img(
                src = "homepage.png", 
                height = "350px", 
                width = "800px", 
                alt = "COVID-19", 
                style = "display: block; margin-left: auto; margin-right: auto;"
              )     
      ), 
      #----
      
      # pestaña 9 referencias 
      # ----
      
      tabItem(tabName = "pestaña9", div( class = "typewriter", h2("Referencias",
                                                                  align = "center", 
                                                                  style = "font-size: 36px; font-weight: bold;")),

                  h3("Referencias utilizadas para el proyecto COVID-19"),
                  p("Este proyecto incluye diversas referencias sobre análisis y datos relacionados con la pandemia."),
                  p("A continuación, se presentan las fuentes utilizadas:"), br(), 

                  tags$ul(
                    tags$li(a("Our World in Data", href = "https://ourworldindata.org/coronavirus", target = "_blank")),
                    tags$li(a("Información de la Organización Mundial de la Salud (OMS)", href = "https://www.who.int/", target = "_blank")),
                    tags$li(a("Centro para el Control y la Prevención de Enfermedades (CDC): R0", href = "https://www.cdc.gov/coronavirus/2019-ncov/science/science-briefs/transmission.html", target = "_blank")),
                    tags$li(a("Modelo SIR - Diversos Artículos de la Revista Nature", href = "https://www.nature.com/articles/s41586-020-2521-4", target = "_blank")),
                    tags$li(a("MicroCOVID Project", href = "https://www.microcovid.org/", target = "_blank"))

              )
      )
      #----
    ) #tabItems
  ) #dashboardBody 
) #dashboardPage


server <- function(input, output) {
  # pestaña 2 / cubrebocas
  #----

  # Dinámicamente generar inputs para los cubrebocas de otras personas
  output$other_cubrebocas_ui <- renderUI({
    num_people <- input$num_people
    lapply(1:num_people, function(i) {
      selectInput(paste0("other_cubrebocas_", i), 
                  paste("Selecciona el cubrebocas de la persona ", i, ":"), 
                  choices = c("Sin cubrebocas", 
                              "Cubrebocas de tela, delgado o flojo", 
                              "Cubrebocas de tela, grueso y ajustado", 
                              "Cubrebocas quirúrgico", 
                              "Cubrebocas con filtro insertado", 
                              "N95, KN95 o FFP2", 
                              "N95 sellado", 
                              "Respirador P100"))
    })
  })
  
  # Imagen dinámica
  output$imagen_dinamica <- renderUI({
    img_src <- switch(input$my_cubrebocas,
                      "Sin cubrebocas" = "nomask.png",
                      "Cubrebocas de tela, delgado o flojo" = "cloth.png",
                      "Cubrebocas de tela, grueso y ajustado" = "thickcloth.png",
                      "Cubrebocas quirúrgico" = "surgical.png",
                      "Cubrebocas con filtro insertado" = "filterinsert.png",
                      "N95, KN95 o FFP2" = "kn95.png",
                      "N95 sellado" = "n95sealed.png",
                      "Respirador P100" = "p100.png"
    )
    tags$img(src = img_src, height = "300px")
  })
  
  # Calcular efectividad total
  observeEvent(input$calculate, {
    req(input$my_cubrebocas)
    num_people <- input$num_people
    other_cubrebocas <- sapply(1:num_people, function(i) input[[paste0("other_cubrebocas_", i)]])
    total_effectiveness <- calculate_total_cubrebocas_effectiveness(input$my_cubrebocas, other_cubrebocas)
    porcentaje <- round(total_effectiveness * 100, 2)
    
    output$result <- renderText({
      HTML(paste("<b>La efectividad total del uso de cubrebocas es:</b>",
                 round(total_effectiveness * 100, 2), "%"))
    })
    
    output$riesgo_dinamica <- renderUI({
      percentage <- round(total_effectiveness * 100, 2)
      img_src <- if (percentage <= 20) {
        "1-removebg-preview.png"
      } else if (percentage <= 40) {
        "2-removebg-preview.png"
      } else if (percentage <= 60) {
        "3-removebg-preview.png"
      } else if (percentage <= 80) {
        "4-removebg-preview.png"
      } else {
        "5-removebg-preview.png"
      }
      tags$img(src = img_src, height = "300px")
    })
  })
  
  # Calcular efectividad total
  observeEvent(input$calculate, {
    req(input$my_cubrebocas)
    num_people <- input$num_people
    other_cubrebocas <- sapply(1:num_people, function(i) input[[paste0("other_cubrebocas_", i)]])
    total_effectiveness <- calculate_total_cubrebocas_effectiveness(input$my_cubrebocas, other_cubrebocas)
    
    output$result <- renderText({
      paste("La efectividad total del uso de cubrebocas es:", round(total_effectiveness * 100, 2), "%")
    })
  })
  
  #----
  
  # pestaña 3 / prob indv
  #----
  
  # seleccionar cubrebocas
  output$other_cubrebocas_ui2 <- renderUI({
    req(input$num_people2)  # Verifica que num_people tenga un valor válido
    lapply(1:input$num_people2, function(i) {
      selectInput(
        paste0("other_cubrebocas_", i),
        paste("Selecciona el cubrebocas de la persona", i, ":"),
        choices = c(
          "Sin cubrebocas",
          "Cubrebocas de tela, delgado o flojo",
          "Cubrebocas de tela, grueso y ajustado",
          "Cubrebocas quirúrgico",
          "Cubrebocas con filtro insertado",
          "N95, KN95 o FFP2",
          "N95 sellado",
          "Respirador P100"
        )
      )
    })
  })
  
  
  # calcular microcovid 
  observeEvent(input$calculate_btn, {
    
    # datitos 
    local_prevalence <- if (input$local_prevalence == "Riesgo bajo") {
      80 / 10000
    } else if (input$local_prevalence == "Riesgo medio") {
      150 / 10000
    } else if (input$local_prevalence == "Riesgo alto") {
      325 / 10000
    } else if (input$local_prevalence == "Riesgo extremo") {
      750 / 10000
    } else {
      NA  # Valor por defecto en caso de que no coincida con ninguna categoría
    }
    
    vaccine_type = input$vaccine_type
    num_doses = input$num_doses
    my_mask = c(input$my_cubrebocas)
    speaking_volume = input$speaking_volume
    distance = input$distance
    duration_hours = input$duration_hours
    ventilation = input$ventilation
    
    # Vector para el cubrebocas de las demás personas
    other_masks <- sapply(1:input$num_people2, function(i) {
      input_id <- paste0("other_cubrebocas_", i)
      if (!is.null(input[[input_id]])) {
        input[[input_id]]
      } else {
        NA  # Valor por defecto si el ID no existe
      }
    })
    

    # calcular

    vaccination_effectiveness <- get_vaccination_effectiveness(vaccine_type, num_doses)
      
    total_mask_effectiveness <- calculate_total_cubrebocas_effectiveness(my_mask, other_masks)
    print(total_mask_effectiveness)
    
    ventilation_multiplier <- get_ventilation_multiplier(ventilation)
    distance_multiplier <- get_distance_multiplier(distance)
    speaking_volume_multiplier <- get_speaking_volume_multiplier(speaking_volume)
      
    num_people <- length(other_masks)
      
      
    total_risk_microcovids <- 1.25 * (1 + local_prevalence) * local_prevalence * speaking_volume_multiplier * distance_multiplier * ventilation_multiplier * duration_hours * num_people * (1 - total_mask_effectiveness) * 1e6 * vaccination_effectiveness


    # Mostrar el resultado
    output$microcovid_result <- renderUI({
      HTML(paste0(
        "En esta actividad te expones a: <b>", 
        round(total_risk_microcovids, 6), 
        " microCOVIDs</b>, lo que te da una probabilidad de <b>", 
        (100*(round(total_risk_microcovids / 1000000, 8))), 
        "%</b>."
      ))
    })
    
    output$microcovid_dinamica <- renderUI({
      risk_microcovids <- round(total_risk_microcovids, 2)

      img_src <- if (risk_microcovids >= 100000) { # más de 100k
        # Muy peligroso
        "1-removebg-preview.png"
      } else if (risk_microcovids >= 10000) { # más de 10k
        # Peligroso
        "2-removebg-preview.png"
      } else if (risk_microcovids >= 1000) { # más de 1k
        # Moderado
        "3-removebg-preview.png"
      } else if (risk_microcovids >= 100) { # más de 100
        # Bajo
        "4-removebg-preview.png"
      } else {                              # menos de 100
        # Muy bajo
        "5-removebg-preview.png"
      }
      
      tags$img(src = img_src, height = "300px")
    })
    
  })
  
  #----

  # pestaña 4 / todos como tú
  #----
  output$graficaTodosComoTu <- renderPlotly({
    # Configuración inicial
    t <- input$dias
    prob_s_i <- input$prob_s_i
    prob_i_m <- input$prob_i_m
    
    
    valor_inicial_i <- 1
    valor_inicial_r <- 0
    
    # Parámetros adicionales
    dias_para_R <- 7  # Días antes de pasar de I a R
    dias_para_S <- 60  # Días de inmunidad temporal antes de regresar a S
    
    # Inicializar vectores
    # ----

    tiempo <- 1:t
    
    
    S <- numeric(length(tiempo))  # Vector para S
    I <- numeric(length(tiempo))  # Vector para I
    R <- numeric(length(tiempo))  # Vector para R
    M <- numeric(length(tiempo))  # Vector para Muertes
    
    
    S[1] <- 100000 # Asignar el valor inicial a S
    I[1] <- 1  # Asignar el valor inicial a I
    R[1] <- 0  # Asignar el valor inicial a R
    M[1] <- 0  # Asignar el valor inicial a M
    

    # Registrar las nuevas infecciones
    infecciones <- rep(0, length(tiempo) + dias_para_R)
    
    # Registrar recuperados día a día
    recuperados_a_susceptibles <- rep(0, length(tiempo) + dias_para_S)
    
    # Simulación de la dinámica
    for (t in 2:t) {
      print("sin medidas")
      print(t)
      # 1. Conversión de S a I (asegurando no exceder S disponible)
      # Número de susceptibles en el paso t-1
      N_susceptibles <- S[t - 1]
      
      # Generar probabilidades individuales usando directamente la función global
      probabilidades_individuales <- rep(prob_s_i, N_susceptibles)
      
      # Determinar si cada persona se infecta, basándose en su probabilidad individual
      conversiones_por_persona <- rbinom(N_susceptibles, size = 1, prob = probabilidades_individuales)
      
      # Sumar las infecciones totales
      conversiones_totales <- sum(conversiones_por_persona)
      
      # Asegurar que no se infecten más personas de las que hay en S
      conversiones_totales <- min(conversiones_totales, S[t - 1])
      
      # Actualizar S e I
      S[t] <- S[t - 1] - conversiones_totales
      I[t] <- I[t - 1] + conversiones_totales
      
      # Registrar infecciones para futuras recuperaciones
      if (t + dias_para_R <= length(infecciones)) {
        infecciones[t + dias_para_R] <- conversiones_totales
      }
      
      
      # 2. Muertes directas desde I 
      muertes <- rbinom(1, I[t], prob_i_m)
      muertes <- min(muertes, I[t])  # Garantizar que no haya más muertes que infectados
      I[t] <- I[t] - muertes
      M[t] <- M[t - 1] + muertes
      
      # 3. Mover de I a R después de X días (asegurando no exceder I disponible)
      if (t > dias_para_R) {
        nuevos_R <- infecciones[t]  # Recuperaciones del día actual
        nuevos_R <- min(nuevos_R, I[t])  # Garantizar que no haya más recuperaciones que infectados
        I[t] <- I[t] - nuevos_R
        R[t] <- R[t - 1] + nuevos_R
        
        # Registrar recuperados para regresar a S después de 60 días
        recuperados_a_susceptibles[t + dias_para_S] <- nuevos_R
      } else {
        R[t] <- R[t - 1]
      }
      
      # 4. Mover de R a S después del período de inmunidad
      if (t > dias_para_S) {
        retornan_a_S <- recuperados_a_susceptibles[t]  # Recuperados que completaron 60 días de inmunidad
        retornan_a_S <- min(retornan_a_S, R[t])  # Garantizar que no haya más retornos que recuperados
        R[t] <- R[t] - retornan_a_S
        S[t] <- S[t] + retornan_a_S
      }
    }
    
    
    # Crear un data frame con los resultados
    # ----
    resultados <- data.frame(
      Día = tiempo,
      Susceptibles = S,
      Infectados = I,
      Recuperados = R,
      Muertes = M
    )
    
    # Calcular la población total
    resultados$Total <- resultados$Susceptibles + resultados$Infectados + resultados$Recuperados 
    
    # Crear el gráfico interactivo
    fig <- plot_ly(resultados, x = ~Día) %>%
      add_lines(y = ~Susceptibles, name = "Susceptibles", line = list(color = "#ff0000", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br><b>Susceptibles:</b> <b>", Susceptibles, "</b>",
                              "<br>Infectados:", Infectados,
                              "<br>Recuperados:", Recuperados,
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Infectados, name = "Infectados", line = list(color = "#00ff00", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br><b>Infectados:</b> <b>", Infectados, "</b>",
                              "<br>Recuperados:", Recuperados,
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Recuperados, name = "Recuperados", line = list(color = "#0000ff", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br>Infectados:", Infectados,
                              "<br><b>Recuperados:</b> <b>", Recuperados, "</b>",
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Muertes, name = "Muertes", line = list(color = "#a01cec", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br>Infectados:", Infectados,
                              "<br>Recuperados:", Recuperados,
                              "<br><b>Muertes:</b> <b>", Muertes, "</b>",
                              "<br>Población Viva:", Total)) %>%
      
      layout(
        xaxis = list(title = "Día"),
        yaxis = list(title = "Cantidad"),
        legend = list(title = list(text = "Estado"))
      )
    # ----
    # Mostrar la gráfica
    fig
  })
  #----
  
  # pestaña 5 / modelo SIR
  # ----
  output$plotSIR <- renderPlotly({
    S <- input$S
    I <- input$I
    R <- input$R
    t_input <- input$t
    t <- seq(0, t_input, 1)
    L <- input$L
    TDN <- input$TDN
    u <- input$u
    B <- input$B
    yL <- 1 / L
    N <- S + I + R
    
    init <- c(S = S, I = I, R = R)
    
    miOutput <- as.data.frame(ode(y = init, times = t, func = ed.sol, parms = NULL, u = u, N = N, B = B, yL = yL))
    
    resultados <- data.frame(
      Día = miOutput$time,
      Susceptibles = miOutput$S,
      Infectados = miOutput$I,
      Recuperados = miOutput$R,
      Total = miOutput$S + miOutput$I + miOutput$R
    )
    
    
    fig <- plot_ly(resultados, x = ~Día) %>%
      add_lines(y = ~Susceptibles, name = "Susceptibles", line = list(color = "#ff0000", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br><b>Susceptibles:</b> <b>", Susceptibles, "</b>",
                              "<br>Infectados:", Infectados,
                              "<br>Recuperados:", Recuperados, 
                              "<br>Total:", Total)) %>%
      
      add_lines(y = ~Infectados, name = "Infectados", line = list(color = "#00ff00", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br><b>Infectados:</b> <b>", Infectados, "</b>",
                              "<br>Recuperados:", Recuperados, 
                              "<br>Total:", Total)) %>%
      
      add_lines(y = ~Recuperados, name = "Recuperados", line = list(color = "#0000ff", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br>Infectados:", Infectados,
                              "<br><b>Recuperados:</b> <b>", Recuperados, "</b>",
                              "<br>Total:", Total )) %>%

      layout(
        xaxis = list(
          title = list(text = "Día", font = list(color = color_general)),
          tickfont = list(color = color_general)
        ),
        yaxis = list(
          title = list(text = "Cantidad", font = list(color = color_general)),
          tickfont = list(color = color_general)
        ),
        legend = list(
          title = list(text = "Estado", font = list(color = color_general)),
          font = list(color = color_general)
        )
      )
    
  })
  
  output$rotexto <- renderUI({
    B <- input$B
    yL <- 1 / input$L
    
    r0 <- B / yL
    HTML(paste("<b>R0 tiene un valor de:</b>", round(r0, 2)))
  })
  # ----
  
  # pestaña 6 / sir secciones estocástico
  # ----
  
  output$graficaSimulacion <- renderPlotly({
    
    i_t_sin_medidas <- input$i_t_sin_medidas
    i_t_con_cubrebocas <- input$i_t_con_cubrebocas
    i_t_con_vacunas <- input$i_t_con_vacunas
    prob_i_m <- input$prob_i_m
    
    
    # Inicializar vectores
    # ----
    tiempo_sin_medidas <- i_t_sin_medidas 
    tiempo_con_cubrebocas <- tiempo_sin_medidas + i_t_con_cubrebocas
    tiempo_con_vacunas <- tiempo_con_cubrebocas + i_t_con_vacunas
    
    tiempo_modelado <- i_t_sin_medidas + i_t_con_cubrebocas + i_t_con_vacunas
    tiempo <- 1:tiempo_modelado  
    
    
    S <- numeric(length(tiempo))  # Vector para S
    I <- numeric(length(tiempo))  # Vector para I
    R <- numeric(length(tiempo))  # Vector para R
    M <- numeric(length(tiempo))  # Vector para Muertes
    
    S[1] <- 100000  # Asignar el valor inicial a S
    I[1] <- 1  # Asignar el valor inicial a I
    R[1] <- 0  # Asignar el valor inicial a R
    M[1] <- 0  # Asignar el valor inicial a M
    
    # Parámetros adicionales
    dias_para_R <- 7  # Días antes de pasar de I a R
    dias_para_S <- 60  # Días de inmunidad temporal antes de regresar a S
    
    
    # Registrar las nuevas infecciones
    infecciones <- rep(0, length(tiempo) + dias_para_R)
    
    # Registrar recuperados día a día
    recuperados_a_susceptibles <- rep(0, length(tiempo) + dias_para_S)
    
    # ----
    
    
    # Simulación de la dinámica
    # ----
    for (t in 2:tiempo_sin_medidas) {
      print("sin medidas")
      print(t)
      # 1. Conversión de S a I (asegurando no exceder S disponible)
      # Número de susceptibles en el paso t-1
      N_susceptibles <- S[t - 1]
      
      # Generar probabilidades individuales usando directamente la función global
      probabilidades_individuales <- rep(calculate_covid_prob_no_vacc_no_mask(I, S), N_susceptibles)
      
      # Determinar si cada persona se infecta, basándose en su probabilidad individual
      conversiones_por_persona <- rbinom(N_susceptibles, size = 1, prob = probabilidades_individuales)
      
      # Sumar las infecciones totales
      conversiones_totales <- sum(conversiones_por_persona)
      
      # Asegurar que no se infecten más personas de las que hay en S
      conversiones_totales <- min(conversiones_totales, S[t - 1])
      
      # Actualizar S e I
      S[t] <- S[t - 1] - conversiones_totales
      I[t] <- I[t - 1] + conversiones_totales
      
      # Registrar infecciones para futuras recuperaciones
      if (t + dias_para_R <= length(infecciones)) {
        infecciones[t + dias_para_R] <- conversiones_totales
      }
      
      
      # 2. Muertes directas desde I 
      muertes <- rbinom(1, I[t], prob_i_m)
      muertes <- min(muertes, I[t])  # Garantizar que no haya más muertes que infectados
      I[t] <- I[t] - muertes
      M[t] <- M[t - 1] + muertes
      
      # 3. Mover de I a R después de X días (asegurando no exceder I disponible)
      if (t > dias_para_R) {
        nuevos_R <- infecciones[t]  # Recuperaciones del día actual
        nuevos_R <- min(nuevos_R, I[t])  # Garantizar que no haya más recuperaciones que infectados
        I[t] <- I[t] - nuevos_R
        R[t] <- R[t - 1] + nuevos_R
        
        # Registrar recuperados para regresar a S después de 60 días
        recuperados_a_susceptibles[t + dias_para_S] <- nuevos_R
      } else {
        R[t] <- R[t - 1]
      }
      
      # 4. Mover de R a S después del período de inmunidad
      if (t > dias_para_S) {
        retornan_a_S <- recuperados_a_susceptibles[t]  # Recuperados que completaron 60 días de inmunidad
        retornan_a_S <- min(retornan_a_S, R[t])  # Garantizar que no haya más retornos que recuperados
        R[t] <- R[t] - retornan_a_S
        S[t] <- S[t] + retornan_a_S
      }
    }
    
    for (t in (tiempo_sin_medidas + 1):tiempo_con_cubrebocas){
      print("con cubrebocas")
      print(t)
      # 1. Conversión de S a I (asegurando no exceder S disponible)
      # Número de susceptibles en el paso t-1
      N_susceptibles <- S[t - 1]
      
      # Generar probabilidades individuales usando directamente la función global
      probabilidades_individuales <- rep(calculate_covid_prob_no_vacc_yes_mask(I, S), N_susceptibles)
      
      # Determinar si cada persona se infecta, basándose en su probabilidad individual
      conversiones_por_persona <- rbinom(N_susceptibles, size = 1, prob = probabilidades_individuales)
      
      # Sumar las infecciones totales
      conversiones_totales <- sum(conversiones_por_persona)
      
      # Asegurar que no se infecten más personas de las que hay en S
      conversiones_totales <- min(conversiones_totales, S[t - 1])
      
      # Actualizar S e I
      S[t] <- S[t - 1] - conversiones_totales
      I[t] <- I[t - 1] + conversiones_totales
      
      # Registrar infecciones para futuras recuperaciones
      if (t + dias_para_R <= length(infecciones)) {
        infecciones[t + dias_para_R] <- conversiones_totales
      }
      
      # 2. Muertes directas desde I 
      muertes <- rbinom(1, I[t], prob_i_m)
      muertes <- min(muertes, I[t])  # Garantizar que no haya más muertes que infectados
      I[t] <- I[t] - muertes
      M[t] <- M[t - 1] + muertes
      
      # 3. Mover de I a R después de X días (asegurando no exceder I disponible)
      if (t > dias_para_R) {
        nuevos_R <- infecciones[t]  # Recuperaciones del día actual
        nuevos_R <- min(nuevos_R, I[t])  # Garantizar que no haya más recuperaciones que infectados
        I[t] <- I[t] - nuevos_R
        R[t] <- R[t - 1] + nuevos_R
        
        # Registrar recuperados para regresar a S después de 60 días
        recuperados_a_susceptibles[t + dias_para_S] <- nuevos_R
      } else {
        R[t] <- R[t - 1]
      }
      
      # 4. Mover de R a S después del período de inmunidad
      if (t > dias_para_S) {
        retornan_a_S <- recuperados_a_susceptibles[t]  # Recuperados que completaron 60 días de inmunidad
        retornan_a_S <- min(retornan_a_S, R[t])  # Garantizar que no haya más retornos que recuperados
        R[t] <- R[t] - retornan_a_S
        S[t] <- S[t] + retornan_a_S
      }
    }
    
    for (t in (tiempo_con_cubrebocas + 1):tiempo_con_vacunas){
      print("con vacunas")
      print(t)
      # 1. Conversión de S a I (asegurando no exceder S disponible)
      # Número de susceptibles en el paso t-1
      N_susceptibles <- S[t - 1]
      
      # Generar probabilidades individuales usando directamente la función global
      probabilidades_individuales <- rep(calculate_covid_prob_yes_vacc_yes_mask(I, S), N_susceptibles)
      
      # Determinar si cada persona se infecta, basándose en su probabilidad individual
      conversiones_por_persona <- rbinom(N_susceptibles, size = 1, prob = probabilidades_individuales)
      
      # Sumar las infecciones totales
      conversiones_totales <- sum(conversiones_por_persona)
      
      # Asegurar que no se infecten más personas de las que hay en S
      conversiones_totales <- min(conversiones_totales, S[t - 1])
      
      # Actualizar S e I
      S[t] <- S[t - 1] - conversiones_totales
      I[t] <- I[t - 1] + conversiones_totales
      
      # Registrar infecciones para futuras recuperaciones
      if (t + dias_para_R <= length(infecciones)) {
        infecciones[t + dias_para_R] <- conversiones_totales
      }
      
      # 2. Muertes directas desde I 
      muertes <- rbinom(1, I[t], prob_i_m)
      muertes <- min(muertes, I[t])  # Garantizar que no haya más muertes que infectados
      I[t] <- I[t] - muertes
      M[t] <- M[t - 1] + muertes
      
      # 3. Mover de I a R después de X días (asegurando no exceder I disponible)
      if (t > dias_para_R) {
        nuevos_R <- infecciones[t]  # Recuperaciones del día actual
        nuevos_R <- min(nuevos_R, I[t])  # Garantizar que no haya más recuperaciones que infectados
        I[t] <- I[t] - nuevos_R
        R[t] <- R[t - 1] + nuevos_R
        
        # Registrar recuperados para regresar a S después de 60 días
        recuperados_a_susceptibles[t + dias_para_S] <- nuevos_R
      } else {
        R[t] <- R[t - 1]
      }
      
      # 4. Mover de R a S después del período de inmunidad
      if (t > dias_para_S) {
        retornan_a_S <- recuperados_a_susceptibles[t]  # Recuperados que completaron 60 días de inmunidad
        retornan_a_S <- min(retornan_a_S, R[t])  # Garantizar que no haya más retornos que recuperados
        R[t] <- R[t] - retornan_a_S
        S[t] <- S[t] + retornan_a_S
      }
    }
    
    # ----
    
    # Crear un data frame con los resultados y calcular la problación viva 
    # ----
    resultados <- data.frame(
      Día = tiempo,
      Susceptibles = S,
      Infectados = I,
      Recuperados = R,
      Muertes = M,
      Total = S + I + R
    )
    # ----
    
    
    # Crear el gráfico interactivo
    # ----
    fig <- plot_ly(resultados, x = ~Día) %>%
      add_lines(y = ~Susceptibles, name = "Susceptibles", line = list(color = "#ff0000", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br><b>Susceptibles:</b> <b>", Susceptibles, "</b>",
                              "<br>Infectados:", Infectados,
                              "<br>Recuperados:", Recuperados,
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Infectados, name = "Infectados", line = list(color = "#00ff00", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br><b>Infectados:</b> <b>", Infectados, "</b>",
                              "<br>Recuperados:", Recuperados,
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Recuperados, name = "Recuperados", line = list(color = "#0000ff", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br>Infectados:", Infectados,
                              "<br><b>Recuperados:</b> <b>", Recuperados, "</b>",
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Muertes, name = "Muertes", line = list(color = "#a01cec", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br>Infectados:", Infectados,
                              "<br>Recuperados:", Recuperados,
                              "<br><b>Muertes:</b> <b>", Muertes, "</b>",
                              "<br>Población Viva:", Total)) %>%
      
      # Añadir líneas verticales para los cambios de sección
      add_segments(x = tiempo_sin_medidas, xend = tiempo_sin_medidas, y = 0, yend = max(resultados$Total),
                   line = list(color = "#333333", dash = "dot"), name = "Inicio del uso de cubrebocas") %>%
      add_segments(x = tiempo_con_cubrebocas, xend = tiempo_con_cubrebocas, y = 0, yend = max(resultados$Total),
                   line = list(color = "#999999", dash = "dot"), name = "Inicio de la vacunación") %>%
      
      
      layout(
        xaxis = list(
          title = list(text = "Día", font = list(color = color_general)),
          tickfont = list(color = color_general)
        ),
        yaxis = list(
          title = list(text = "Cantidad", font = list(color = color_general)),
          tickfont = list(color = color_general)
        ),
        legend = list(
          title = list(text = "Estado", font = list(color = color_general)),
          font = list(color = color_general)
        )
      )
    # ----
  })
  
  # ----
  
  # pestaña 7 / sirs comparación
  
  # sir estocastico
  # ---- 

  output$graficaSimulacionE <- renderPlotly({
    
    i_t_sin_medidas <- max(1, input$i_t_sin_medidasC)
    i_t_con_cubrebocas <- max(1, input$i_t_con_cubrebocasC)
    i_t_con_vacunas <- max(1, input$i_t_con_vacunasC)
    
    prob_i_m <- input$prob_i_mC

    
    # Inicializar vectores
    # ----
    tiempo_sin_medidas <- i_t_sin_medidas 
    tiempo_con_cubrebocas <- tiempo_sin_medidas + i_t_con_cubrebocas
    tiempo_con_vacunas <- tiempo_con_cubrebocas + i_t_con_vacunas
    
    tiempo_modelado <- i_t_sin_medidas + i_t_con_cubrebocas + i_t_con_vacunas
    tiempo <- 1:tiempo_modelado  
    
    
    S <- numeric(length(tiempo))  # Vector para S
    I <- numeric(length(tiempo))  # Vector para I
    R <- numeric(length(tiempo))  # Vector para R
    M <- numeric(length(tiempo))  # Vector para Muertes
    
    S[1] <- 100000  # Asignar el valor inicial a S
    I[1] <- 1  # Asignar el valor inicial a I
    R[1] <- 0  # Asignar el valor inicial a R
    M[1] <- 0  # Asignar el valor inicial a M
    
    # Parámetros adicionales
    dias_para_R <- 7  # Días antes de pasar de I a R
    dias_para_S <- 60  # Días de inmunidad temporal antes de regresar a S
    
    
    # Registrar las nuevas infecciones
    infecciones <- rep(0, length(tiempo) + dias_para_R)
    
    # Registrar recuperados día a día
    recuperados_a_susceptibles <- rep(0, length(tiempo) + dias_para_S)
    
    # ----
    
    
    # Simulación de la dinámica
    # ----
    for (t in 2:tiempo_sin_medidas) {
      print("sin medidas")
      print(t)
      # 1. Conversión de S a I (asegurando no exceder S disponible)
      # Número de susceptibles en el paso t-1
      N_susceptibles <- S[t - 1]
      
      # Generar probabilidades individuales usando directamente la función global
      probabilidades_individuales <- rep(calculate_covid_prob_no_vacc_no_mask(I, S), N_susceptibles)
      
      # Determinar si cada persona se infecta, basándose en su probabilidad individual
      conversiones_por_persona <- rbinom(N_susceptibles, size = 1, prob = probabilidades_individuales)
      
      # Sumar las infecciones totales
      conversiones_totales <- sum(conversiones_por_persona)
      
      # Asegurar que no se infecten más personas de las que hay en S
      conversiones_totales <- min(conversiones_totales, S[t - 1])
      
      # Actualizar S e I
      S[t] <- S[t - 1] - conversiones_totales
      I[t] <- I[t - 1] + conversiones_totales
      
      # Registrar infecciones para futuras recuperaciones
      if (t + dias_para_R <= length(infecciones)) {
        infecciones[t + dias_para_R] <- conversiones_totales
      }
      
      
      # 2. Muertes directas desde I 
      muertes <- rbinom(1, I[t], prob_i_m)
      muertes <- min(muertes, I[t])  # Garantizar que no haya más muertes que infectados
      I[t] <- I[t] - muertes
      M[t] <- M[t - 1] + muertes
      
      # 3. Mover de I a R después de X días (asegurando no exceder I disponible)
      if (t > dias_para_R) {
        nuevos_R <- infecciones[t]  # Recuperaciones del día actual
        nuevos_R <- min(nuevos_R, I[t])  # Garantizar que no haya más recuperaciones que infectados
        I[t] <- I[t] - nuevos_R
        R[t] <- R[t - 1] + nuevos_R
        
        # Registrar recuperados para regresar a S después de 60 días
        recuperados_a_susceptibles[t + dias_para_S] <- nuevos_R
      } else {
        R[t] <- R[t - 1]
      }
      
      # 4. Mover de R a S después del período de inmunidad
      if (t > dias_para_S) {
        retornan_a_S <- recuperados_a_susceptibles[t]  # Recuperados que completaron 60 días de inmunidad
        retornan_a_S <- min(retornan_a_S, R[t])  # Garantizar que no haya más retornos que recuperados
        R[t] <- R[t] - retornan_a_S
        S[t] <- S[t] + retornan_a_S
      }
    }
    
    for (t in (tiempo_sin_medidas + 1):tiempo_con_cubrebocas){
      print("con cubrebocas")
      print(t)
      # 1. Conversión de S a I (asegurando no exceder S disponible)
      # Número de susceptibles en el paso t-1
      N_susceptibles <- S[t - 1]
      
      # Generar probabilidades individuales usando directamente la función global
      probabilidades_individuales <- rep(calculate_covid_prob_no_vacc_yes_mask(I, S), N_susceptibles)
      
      # Determinar si cada persona se infecta, basándose en su probabilidad individual
      conversiones_por_persona <- rbinom(N_susceptibles, size = 1, prob = probabilidades_individuales)
      
      # Sumar las infecciones totales
      conversiones_totales <- sum(conversiones_por_persona)
      
      # Asegurar que no se infecten más personas de las que hay en S
      conversiones_totales <- min(conversiones_totales, S[t - 1])
      
      # Actualizar S e I
      S[t] <- S[t - 1] - conversiones_totales
      I[t] <- I[t - 1] + conversiones_totales
      
      # Registrar infecciones para futuras recuperaciones
      if (t + dias_para_R <= length(infecciones)) {
        infecciones[t + dias_para_R] <- conversiones_totales
      }
      
      # 2. Muertes directas desde I 
      muertes <- rbinom(1, I[t], prob_i_m)
      muertes <- min(muertes, I[t])  # Garantizar que no haya más muertes que infectados
      I[t] <- I[t] - muertes
      M[t] <- M[t - 1] + muertes
      
      # 3. Mover de I a R después de X días (asegurando no exceder I disponible)
      if (t > dias_para_R) {
        nuevos_R <- infecciones[t]  # Recuperaciones del día actual
        nuevos_R <- min(nuevos_R, I[t])  # Garantizar que no haya más recuperaciones que infectados
        I[t] <- I[t] - nuevos_R
        R[t] <- R[t - 1] + nuevos_R
        
        # Registrar recuperados para regresar a S después de 60 días
        recuperados_a_susceptibles[t + dias_para_S] <- nuevos_R
      } else {
        R[t] <- R[t - 1]
      }
      
      # 4. Mover de R a S después del período de inmunidad
      if (t > dias_para_S) {
        retornan_a_S <- recuperados_a_susceptibles[t]  # Recuperados que completaron 60 días de inmunidad
        retornan_a_S <- min(retornan_a_S, R[t])  # Garantizar que no haya más retornos que recuperados
        R[t] <- R[t] - retornan_a_S
        S[t] <- S[t] + retornan_a_S
      }
    }
    
    for (t in (tiempo_con_cubrebocas + 1):tiempo_con_vacunas){
      print("con vacunas")
      print(t)
      # 1. Conversión de S a I (asegurando no exceder S disponible)
      # Número de susceptibles en el paso t-1
      N_susceptibles <- S[t - 1]
      
      # Generar probabilidades individuales usando directamente la función global
      probabilidades_individuales <- rep(calculate_covid_prob_yes_vacc_yes_mask(I, S), N_susceptibles)
      
      # Determinar si cada persona se infecta, basándose en su probabilidad individual
      conversiones_por_persona <- rbinom(N_susceptibles, size = 1, prob = probabilidades_individuales)
      
      # Sumar las infecciones totales
      conversiones_totales <- sum(conversiones_por_persona)
      
      # Asegurar que no se infecten más personas de las que hay en S
      conversiones_totales <- min(conversiones_totales, S[t - 1])
      
      # Actualizar S e I
      S[t] <- S[t - 1] - conversiones_totales
      I[t] <- I[t - 1] + conversiones_totales
      
      # Registrar infecciones para futuras recuperaciones
      if (t + dias_para_R <= length(infecciones)) {
        infecciones[t + dias_para_R] <- conversiones_totales
      }
      
      # 2. Muertes directas desde I 
      muertes <- rbinom(1, I[t], prob_i_m)
      muertes <- min(muertes, I[t])  # Garantizar que no haya más muertes que infectados
      I[t] <- I[t] - muertes
      M[t] <- M[t - 1] + muertes
      
      # 3. Mover de I a R después de X días (asegurando no exceder I disponible)
      if (t > dias_para_R) {
        nuevos_R <- infecciones[t]  # Recuperaciones del día actual
        nuevos_R <- min(nuevos_R, I[t])  # Garantizar que no haya más recuperaciones que infectados
        I[t] <- I[t] - nuevos_R
        R[t] <- R[t - 1] + nuevos_R
        
        # Registrar recuperados para regresar a S después de 60 días
        recuperados_a_susceptibles[t + dias_para_S] <- nuevos_R
      } else {
        R[t] <- R[t - 1]
      }
      
      # 4. Mover de R a S después del período de inmunidad
      if (t > dias_para_S) {
        retornan_a_S <- recuperados_a_susceptibles[t]  # Recuperados que completaron 60 días de inmunidad
        retornan_a_S <- min(retornan_a_S, R[t])  # Garantizar que no haya más retornos que recuperados
        R[t] <- R[t] - retornan_a_S
        S[t] <- S[t] + retornan_a_S
      }
    }
    
    # ----
    
    # Crear un data frame con los resultados y calcular la problación viva 
    # ----
    resultados <- data.frame(
      Día = tiempo,
      Susceptibles = S,
      Infectados = I,
      Recuperados = R,
      Muertes = M,
      Total = S + I + R
    )
    # ----
    
    
    # Crear el gráfico interactivo
    # ----
    fig <- plot_ly(resultados, x = ~Día) %>%
      add_lines(y = ~Susceptibles, name = "Susceptibles", line = list(color = "#ff0000", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br><b>Susceptibles:</b> <b>", Susceptibles, "</b>",
                              "<br>Infectados:", Infectados,
                              "<br>Recuperados:", Recuperados,
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Infectados, name = "Infectados", line = list(color = "#00ff00", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br><b>Infectados:</b> <b>", Infectados, "</b>",
                              "<br>Recuperados:", Recuperados,
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Recuperados, name = "Recuperados", line = list(color = "#0000ff", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br>Infectados:", Infectados,
                              "<br><b>Recuperados:</b> <b>", Recuperados, "</b>",
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Muertes, name = "Muertes", line = list(color = "#a01cec", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br>Infectados:", Infectados,
                              "<br>Recuperados:", Recuperados,
                              "<br><b>Muertes:</b> <b>", Muertes, "</b>",
                              "<br>Población Viva:", Total)) %>%
      
      # Añadir líneas verticales para los cambios de sección
      add_segments(x = tiempo_sin_medidas, xend = tiempo_sin_medidas, y = 0, yend = max(resultados$Total),
                   line = list(color = "#333333", dash = "dot"), name = "Inicio del uso de cubrebocas") %>%
      add_segments(x = tiempo_con_cubrebocas, xend = tiempo_con_cubrebocas, y = 0, yend = max(resultados$Total),
                   line = list(color = "#999999", dash = "dot"), name = "Inicio de la vacunación") %>%
      
      
      layout(
        xaxis = list(
          title = list(text = "Día", font = list(color = color_general)),
          tickfont = list(color = color_general)
        ),
        yaxis = list(
          title = list(text = "Cantidad", font = list(color = color_general)),
          tickfont = list(color = color_general)
        ),
        legend = list(
          title = list(text = "Estado", font = list(color = color_general)),
          font = list(color = color_general)
        )
      )
    # ----
  })
  

  
  
  
  #sir ode
  # ----
  
  # sir ode 
  # ----
  output$graficaSimulacionODE <- renderPlotly({
    
    
    # Definición del sistema de ecuaciones diferenciales
    ed.solC <- function(t, state, parms) {
      with(as.list(c(state, parms)), {
        dxdt <- numeric(4)
        dxdt[1] <- -beta * I * (S / N) + rho * R               # dS/dt
        dxdt[2] <- beta * I * (S / N) - I / 7 - d * I          # dI/dt
        dxdt[3] <- I / 7 - rho * R                             # dR/dt
        dxdt[4] <- d * I                                       # dM/dt
        return(list(dxdt))
      })
    }
    
    # Parámetros obtenidos de la interfaz
    i_t_sin_medidasC <- max(1, input$i_t_sin_medidasC)
    i_t_con_cubrebocasC <- max(1, input$i_t_con_cubrebocasC)
    i_t_con_vacunasC <- max(1, input$i_t_con_vacunasC)
    
    # Tiempos acumulados por etapa
    tiempo_sin_medidasC <- seq(0, i_t_sin_medidasC - 1, by = 1)
    tiempo_con_cubrebocasC <- seq(i_t_sin_medidasC, i_t_sin_medidasC + i_t_con_cubrebocasC - 1, by = 1)
    tiempo_con_vacunasC <- seq(i_t_sin_medidasC + i_t_con_cubrebocasC, i_t_sin_medidasC + i_t_con_cubrebocasC + i_t_con_vacunasC - 1, by = 1)
    
    # Parámetros fijos
    beta1 <- max(0.001, input$gamma1)
    beta2 <- max(0.001, input$gamma2)
    beta3 <- max(0.001, input$gamma3)
    
    rho <- 1 / 60
    d <- input$prob_i_mC
    S <- 100000
    I <- 1
    R <- 0
    M <- 0
    N <- S + I + R + M
    
    # Etapa 1: Sin medidas
    init <- c(S = S, I = I, R = R, M = M)
    parms <- c(beta = beta1, rho = rho, d = d, N = N)
    resultadosODE1 <- ode(y = init, times = tiempo_sin_medidasC, func = ed.solC, parms = parms)
    resultadosODE1 <- as.data.frame(resultadosODE1)
    
    # Etapa 2: Uso de cubrebocas
    init <- c(S = resultadosODE1[nrow(resultadosODE1), "S"],
              I = resultadosODE1[nrow(resultadosODE1), "I"],
              R = resultadosODE1[nrow(resultadosODE1), "R"],
              M = resultadosODE1[nrow(resultadosODE1), "M"])
    parms <- c(beta = beta2, rho = rho, d = d, N = N)
    resultadosODE2 <- ode(y = init, times = tiempo_con_cubrebocasC, func = ed.solC, parms = parms)
    resultadosODE2 <- as.data.frame(resultadosODE2)
    
    # Etapa 3: Vacunación
    init <- c(S = resultadosODE2[nrow(resultadosODE2), "S"],
              I = resultadosODE2[nrow(resultadosODE2), "I"],
              R = resultadosODE2[nrow(resultadosODE2), "R"],
              M = resultadosODE2[nrow(resultadosODE2), "M"])
    parms <- c(beta = beta3, rho = rho, d = d, N = N)
    resultadosODE3 <- ode(y = init, times = tiempo_con_vacunasC, func = ed.solC, parms = parms)
    resultadosODE3 <- as.data.frame(resultadosODE3)
    
    # Combinar resultados
    resultados_combinados <- rbind(resultadosODE1, resultadosODE2, resultadosODE3)
    
    resultados <- data.frame(
      Día = resultados_combinados$time,
      Susceptibles = resultados_combinados$S,
      Infectados = resultados_combinados$I,
      Recuperados = resultados_combinados$R,
      Muertes = resultados_combinados$M,
      Total = resultados_combinados$S + resultados_combinados$I + resultados_combinados$R
    )
    
    
    fig <- plot_ly(resultados, x = ~Día) %>%
      add_lines(y = ~Susceptibles, name = "Susceptibles", line = list(color = "#ff0000", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br><b>Susceptibles:</b> <b>", Susceptibles, "</b>",
                              "<br>Infectados:", Infectados,
                              "<br>Recuperados:", Recuperados,
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Infectados, name = "Infectados", line = list(color = "#00ff00", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br><b>Infectados:</b> <b>", Infectados, "</b>",
                              "<br>Recuperados:", Recuperados,
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Recuperados, name = "Recuperados", line = list(color = "#0000ff", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br>Infectados:", Infectados,
                              "<br><b>Recuperados:</b> <b>", Recuperados, "</b>",
                              "<br>Muertes:", Muertes, 
                              "<br>Población Viva:", Total)) %>%
      
      add_lines(y = ~Muertes, name = "Muertes", line = list(color = "#a01cec", width = 1.5),
                hoverinfo = "text",
                text = ~paste("Día:", Día,
                              "<br>Susceptibles:", Susceptibles,
                              "<br>Infectados:", Infectados,
                              "<br>Recuperados:", Recuperados,
                              "<br><b>Muertes:</b> <b>", Muertes, "</b>",
                              "<br>Población Viva:", Total)) %>%
      
      # Añadir líneas verticales para los cambios de sección
      add_segments(x = i_t_sin_medidasC, xend = i_t_sin_medidasC, y = 0, yend = max(resultados$Total),
                   line = list(color = "#333333", dash = "dot"), name = "Inicio del uso de cubrebocas") %>%
      add_segments(x = (i_t_sin_medidasC + i_t_con_cubrebocasC), xend = (i_t_sin_medidasC + i_t_con_cubrebocasC), y = 0, yend = max(resultados$Total),
                   line = list(color = "#999999", dash = "dot"), name = "Inicio de la vacunación") %>%
      
      
      layout(
        xaxis = list(
          title = list(text = "Día", font = list(color = color_general)),
          tickfont = list(color = color_general)
        ),
        yaxis = list(
          title = list(text = "Cantidad", font = list(color = color_general)),
          tickfont = list(color = color_general)
        ),
        legend = list(
          title = list(text = "Estado", font = list(color = color_general)),
          font = list(color = color_general)
        )
      )
    
  })    
  # ----
  
}

shinyApp(ui, server)