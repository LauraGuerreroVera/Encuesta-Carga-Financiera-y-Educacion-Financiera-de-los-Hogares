library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)


datos <- read_excel("datos.xlsx")    
año <- read_excel("año.xlsx")


    class(datos$OTROS_PRESTAMOS)
    sum(is.na(datos$OTROS_PRESTAMOS))
    datos$OTROS_PRESTAMOS[is.na(datos$OTROS_PRESTAMOS)] = "NO APLICA"
    table(datos$OTROS_PRESTAMOS) 
    datos$OTROS_PRESTAMOS <- as.factor(datos$OTROS_PRESTAMOS)
    levels(datos$OTROS_PRESTAMOS)
    
    sum(is.na(datos$OTROS_PRESTAMOS))
    datos = as.data.frame(datos)
    OP = datos %>% 
      group_by(OTROS_PRESTAMOS) %>% 
      summarise(conteo = n())
    
    class(datos$DEPTO)
    sum(is.na(datos$DEPTO))
    datos$DEPTO[is.na(datos$DEPTO)] = "NO APLICA"
    table(datos$DEPTO)
    datos$DEPTO <- as.factor(datos$DEPTO)
    levels(datos$DEPTO)
    
    class(datos$EDAD)
    datos$EDAD <- as.numeric(as.character(datos$EDAD))
    datos$EDAD <- as.factor(datos$EDAD)
    table(datos$EDAD)
    
    sum(is.na(datos$EDAD))
    datos = as.data.frame(datos)
    E = datos %>% 
      group_by(EDAD) %>% 
      summarise(conteo = n()) 
    
    AÑOS <- factor(año$AÑOS)
    levels(AÑOS) <- c(2017, 2018)
    AÑOS <- table(AÑOS)
    barplot(AÑOS)
    
    
    
    sum(is.na(datos$DEPTO))
    datos = as.data.frame(datos)
    DEP = datos %>% 
      group_by(DEPTO) %>% 
      summarise(conteo = n()) 
    
    
    
    class(datos$SEXO)
    sum(is.na(datos$SEXO))
    datos$SEXO[is.na(datos$SEXO)] = "NO APLICA"
    table(datos$SEXO)
    datos$SEXO <- as.factor(datos$SEXO)
    levels(datos$SEXO)
    
    sum(is.na(datos$SEXO))
    datos = as.data.frame(datos)
    S = datos %>% 
      group_by(SEXO) %>% 
      summarise(conteo = n(),
                percentaje = n()/nrow(datos)) 
    
    
    class(datos$CREDITO_HIP)
    sum(is.na(datos$CREDITO_HIP))
    datos$CREDITO_HIP[is.na(datos$CREDITO_HIP)] = "NO APLICA"
    table(datos$CREDITO_HIP)
    datos$CREDITO_HIP <- as.factor(datos$CREDITO_HIP)
    levels(datos$CREDITO_HIP)
    
    sum(is.na(datos$CREDITO_HIP))
    datos = as.data.frame(datos)
    CH = datos %>% 
      group_by(CREDITO_HIP) %>% 
      summarise(conteo = n()) 
    
    
    class(datos$GASTO_TOTAL_INGRESO)
    sum(is.na(datos$GASTO_TOTAL_INGRESO))
    datos$GASTO_TOTAL_INGRESO[is.na(datos$GASTO_TOTAL_INGRESO)] = "NO APLICA"
    table(datos$GASTO_TOTAL_INGRESO)
    datos$GASTO_TOTAL_INGRESO <- as.factor(datos$GASTO_TOTAL_INGRESO)
    levels(datos$GASTO_TOTAL_INGRESO)
    
    sum(is.na(datos$GASTO_TOTAL_INGRESO))
    datos = as.data.frame(datos)
    GTI = datos %>% 
      group_by(GASTO_TOTAL_INGRESO) %>% 
      summarise(conteo = n()) 
    
    class(datos$TARJETA_CREDITO)
    sum(is.na(datos$TARJETA_CREDITO))
    datos$TARJETA_CREDITO[is.na(datos$TARJETA_CREDITO)] = "NO APLICA"
    table(datos$TARJETA_CREDITO)
    datos$TARJETA_CREDITO <- as.factor(datos$TARJETA_CREDITO)
    levels(datos$TARJETA_CREDITO)
    
    sum(is.na(datos$TARJETA_CREDITO))
    datos = as.data.frame(datos)
    TC = datos %>% 
      group_by(TARJETA_CREDITO) %>% 
      summarise(conteo = n()) 
    
    class(datos$OTROS_PRESTAMOS)
    sum(is.na(datos$OTROS_PRESTAMOS))
    datos$OTROS_PRESTAMOS[is.na(datos$OTROS_PRESTAMOS)] = "NO APLICA"
    table(datos$OTROS_PRESTAMOS)
    datos$OTROS_PRESTAMOS <- as.factor(datos$OTROS_PRESTAMOS)
    levels(datos$OTROS_PRESTAMOS)
    
    sum(is.na(datos$OTROS_PRESTAMOS))
    datos = as.data.frame(datos)
    OP = datos %>% 
      group_by(OTROS_PRESTAMOS) %>% 
      summarise(conteo = n()) 
    
    class(datos$TIENDA_BARRIO)
    sum(is.na(datos$TIENDA_BARRIO))
    datos$TIENDA_BARRIO[is.na(datos$TIENDA_BARRIO)] = "NO APLICA"
    table(datos$TIENDA_BARRIO)
    datos$TIENDA_BARRIO <- as.factor(datos$TIENDA_BARRIO)
    levels(datos$TIENDA_BARRIO)
    
    sum(is.na(datos$TIENDA_BARRIO))
    datos = as.data.frame(datos)
    TB = datos %>% 
      group_by(TIENDA_BARRIO) %>% 
      summarise(conteo = n()) 
    
    class(datos$CREDITO_EDUCATIVO)
    sum(is.na(datos$CREDITO_EDUCATIVO))
    datos$CREDITO_EDUCATIVO[is.na(datos$CREDITO_EDUCATIVO)] = "NO APLICA"
    table(datos$CREDITO_EDUCATIVO)
    datos$CREDITO_EDUCATIVO <- as.factor(datos$CREDITO_EDUCATIVO)
    levels(datos$CREDITO_EDUCATIVO)
    
    sum(is.na(datos$CREDITO_EDUCATIVO))
    datos = as.data.frame(datos)
    CE = datos %>% 
      group_by(CREDITO_EDUCATIVO) %>% 
      summarise(conteo = n()) 
    
    class(datos$ACCIONES)
    sum(is.na(datos$ACCIONES))
    datos$ACCIONES[is.na(datos$ACCIONES)] = "NO APLICA"
    table(datos$ACCIONES)
    datos$ACCIONES <- as.factor(datos$ACCIONES)
    levels(datos$ACCIONES)
    
    sum(is.na(datos$ACCIONES))
    datos = as.data.frame(datos)
    A = datos %>% 
      group_by(ACCIONES) %>% 
      summarise(conteo = n()) 
    
    class(datos$CDT)
    sum(is.na(datos$CDT))
    datos$CDT[is.na(datos$CDT)] = "NO APLICA"
    table(datos$CDT)
    datos$CDT <- as.factor(datos$CDT)
    levels(datos$CDT)
    
    sum(is.na(datos$CDT))
    datos = as.data.frame(datos)
    C = datos %>% 
      group_by(CDT) %>% 
      summarise(conteo = n()) 
    
    class(datos$CTA_AHORROS)
    sum(is.na(datos$CTA_AHORROS))
    datos$CTA_AHORROS[is.na(datos$CTA_AHORROS)] = "NO APLICA"
    table(datos$CTA_AHORROS)
    datos$CTA_AHORROS <- as.factor(datos$CTA_AHORROS)
    levels(datos$CTA_AHORROS)
    
    sum(is.na(datos$CTA_AHORROS))
    datos = as.data.frame(datos)
    CA = datos %>% 
      group_by(CTA_AHORROS) %>% 
      summarise(conteo = n()) 
    
    class(datos$DEUDAS)
    sum(is.na(datos$DEUDAS))
    datos$DEUDAS[is.na(datos$DEUDAS)] = "NO APLICA"
    table(datos$DEUDAS)
    datos$DEUDAS <- as.factor(datos$DEUDAS)
    levels(datos$DEUDAS)
    
    sum(is.na(datos$DEUDAS))
    datos = as.data.frame(datos)
    D = datos %>% 
      group_by(DEUDAS) %>% 
      summarise(conteo = n()) 
    
    class(datos$NIVEL_ENDEUDAMIENTO)
    sum(is.na(datos$NIVEL_ENDEUDAMIENTO))
    datos$NIVEL_ENDEUDAMIENTO[is.na(datos$NIVEL_ENDEUDAMIENTO)] = "NO APLICA"
    table(datos$NIVEL_ENDEUDAMIENTO)
    datos$NIVEL_ENDEUDAMIENTO <- as.factor(datos$NIVEL_ENDEUDAMIENTO)
    levels(datos$NIVEL_ENDEUDAMIENTO)
    
    sum(is.na(datos$NIVEL_ENDEUDAMIENTO))
    datos = as.data.frame(datos)
    NE = datos %>% 
      group_by(NIVEL_ENDEUDAMIENTO) %>% 
      summarise(conteo = n()) 
    
    class(datos$COMPRAS_TARJETA_CREDITO)
    sum(is.na(datos$COMPRAS_TARJETA_CREDITO))
    datos$COMPRAS_TARJETA_CREDITO[is.na(datos$COMPRAS_TARJETA_CREDITO)] ="NO APLICA"
    table(datos$COMPRAS_TARJETA_CREDITO)
    datos$COMPRAS_TARJETA_CREDITO <- as.factor(datos$COMPRAS_TARJETA_CREDITO)
    levels(datos$COMPRAS_TARJETA_CREDITO)
    
    sum(is.na(datos$COMPRAS_TARJETA_CREDITO))
    datos = as.data.frame(datos)
    CTC = datos %>% 
      group_by(COMPRAS_TARJETA_CREDITO) %>% 
      summarise(conteo = n()) 
    
    class(datos$AUMENTO_COMPRAS_CREDITO)
    sum(is.na(datos$AUMENTO_COMPRAS_CREDITO))
    datos$AUMENTO_COMPRAS_CREDITO[is.na(datos$AUMENTO_COMPRAS_CREDITO)] = "NO APLICA"
    table(datos$AUMENTO_COMPRAS_CREDITO)
    datos$AUMENTO_COMPRAS_CREDITO <- as.factor(datos$AUMENTO_COMPRAS_CREDITO)
    levels(datos$AUMENTO_COMPRAS_CREDITO)
    
    sum(is.na(datos$AUMENTO_COMPRAS_CREDITO))
    datos = as.data.frame(datos)
    ACC = datos %>% 
      group_by(AUMENTO_COMPRAS_CREDITO) %>% 
      summarise(conteo = n()) 
    
    class(datos$FRECUENCIA_COMPRAS_CREDITO)
    sum(is.na(datos$FRECUENCIA_COMPRAS_CREDITO))
    datos$FRECUENCIA_COMPRAS_CREDITO[is.na(datos$FRECUENCIA_COMPRAS_CREDITO)] = "NO APLICA"
    table(datos$FRECUENCIA_COMPRAS_CREDITO) 
    datos$FRECUENCIA_COMPRAS_CREDITO <- as.factor(datos$FRECUENCIA_COMPRAS_CREDITO)
    levels(datos$FRECUENCIA_COMPRAS_CREDITO)
    
    sum(is.na(datos$FRECUENCIA_COMPRAS_CREDITO))
    datos = as.data.frame(datos)
    FCC = datos %>% 
      group_by(FRECUENCIA_COMPRAS_CREDITO) %>% 
      summarise(conteo = n()) 
    
     
    count.data <- data.frame(
  class = c("Femenino", "Masculino"),
  n = c(14544, 11727),
  prop = c(55.37, 44.64)
)
count.data <- count.data %>% 
  arrange(desc(class)) %>% 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

mycolsexo <- c("cyan", "blue")


table(datos$CREDITO_HIP)
credito.data <- data.frame(
  class =c("No", "No informa", "No sabe", "Si"),
  n = c(19560, 69, 120, 6522),
  propcredito = c(74.5, 0.25, 0.45, 24.8)
)
credito.data <- credito.data %>% 
  arrange(desc(class)) %>% 
  mutate(lab.ypos = cumsum(propcredito) - 0.5*propcredito)
mycols <- c("green", "yellow", "blue", "red")



ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Laura Marcela Guerrero Vera - PROYECTO DE APRENDIZAJE - DASHBOARD SOCIODEMOGRÁFICO", titleWidth = 1000),
    dashboardSidebar(
        
        sidebarMenu(
          
          menuItem("Identificación", tabName = "identificacion"),
          menuItem("Activos-Deuda Hipotecaria", tabName = "Activos"),
          menuItem("Consumo", tabName = "consumo", menuSubItem("Gasto total del ingreso", tabName = "gasto")),
          menuItem("Deuda no hipotecaria", tabName = "deuda no",
                   menuSubItem("Tarjeta de crédito/Libre inversión", tabName = "tarjeta"),
                   menuSubItem("Crédito educativo/tienda del barrio", tabName = "barrio"),
                   menuSubItem("TC como medio de pago", tabName = "medio"),
                   menuSubItem("Aumento/Frecuencia", tabName = "aumento")),
          menuItem("Activos financieros y reales", tabName = "activos"),
          menuItem("Carga financiera/restricciones al credito", tabName = "carga"),
          menuItem("Summary", tabName = "sum")
          
        )
       
    ),
    
    dashboardBody(
      
      
       tags$head(
         tags$img(src="https://upload.wikimedia.org/wikipedia/commons/d/d9/LogoEntidad2.png", height=150, width=100)
       ) ,
      
      
      infoBox("","Encuesta educación y carga financiera de los hogares", color="aqua", icon = icon("bar-chart")),
      valueBox(62838, "Personas encuestadas (año 2018)", icon = icon("users"), color="aqua"),
      valueBox(26271, "Filtro: Propietario de vivienda", icon = icon("tags"), color="aqua"),
      tags$br(),
      
      tabItems(
        tabItem(tabName = "sum",
                fluidRow(
                  box(width = 12,
                  tabsetPanel(type = "tab",
                    tabPanel("summary", verbatimTextOutput("sum")))
                ))
          
        ),
        tabItem(tabName = "identificacion", icon = icon("car"),
                h1("Identificación de los encuestados"),
                fluidRow(
                  box(title = "AÑO DE LA ENCUESTA", status = "primary", solidHeader = TRUE, width = 7, hight = 455,
                      " El Banco de la República conjuntamente con el DANE realizaron la encuesta a 
                      los hogares y las personas mayores de edad que dentro de los mismos poseen 
                      servicios financieros, con el objeto de construir indicadores de carga 
                      financiera para medir el nivel de endeudamiento, la capacidad de pago y el 
                      conocimiento que tienen los hogares acerca de las principales variables monetarias y 
                      financieras que afectan las decisiones de consumo, ahorro e inversión de la población.",
                      plotOutput("GRAFICO19")),
                  
                  box( title = "CIUDADES ENCUESTADAS", status = "primary", solidHeader = TRUE, width = 5, hight = 455,
                       "La población Colombiana encuestada habita en las ciudades de Bogotá, Medellin y Cali",
                  
                  img(src="https://1.bp.blogspot.com/-xf0xCX3d7UU/Xw4fIoz9sCI/AAAAAAAALeU/Hw5yHRqwgksfqrJ9Ttr5VRGRbiLYz0uvQCLcBGAsYHQ/s2048/MC2.png", heigth = 800, width = 300 
                      )),
                  
                  
                  box(
                    title = "DEPARTAMENTO", solidHeader = TRUE, width =  12, status = "primary", 
                    "Para Bogotá del total de personas encuestadas el 54% son mujeres, 
                    para Medellin el 56%, y para Cali el 56%. Para Bogotá  del total de 
                    personas encuestadas el 45% son hombres, para Medellin el 44%, y para Cali el 44%.",
                    
                    plotOutput("GRAFICO1")),
                  
                  box(
                    title = "GÉNERO", solidHeader = TRUE, width = 6, height = 500 , status = "primary", 
                    "El 55.4% de los encuestados son mujeres y el 45.6% son hombres. ",
                    
                    plotOutput("GRAFICO2")),
                  box(
                    title = "EDAD", height = 500, width = 6, status = "primary", solidHeader = TRUE, 
                    "El 25.3% de las personas encuestadas están entre un rango de edad de 0-32 años, el 26% entre 33 y 50 años, el 25.3% entre un rango de 51 y 63 años, y el 23.4% entre 64 y 106 años.",
                    plotOutput("GRAFICO3"))
                  

                )),
        tabItem(tabName = "Activos",
                
                fluidRow(
                  box(
                    title = "CRÉDITO HIPOTECARIO", width =12, length = 1,
                    status = "primary", solidHeader = TRUE, 
                    " Del total de personas propietarias de vivienda, el 24.8% tienen crédito hipotecario.",
                    plotOutput("GRAFICO5"))
                )
        ),
        tabItem(tabName = "gasto",
                
                fluidRow(
                  box(
                    title = "GASTO TOTAL DEL INGRESO", width = 12, length =1, 
                    status = "primary", solidHeader = TRUE, 
                    "Esta pregunta fue respondida por 26264 personas. Del total de personas encuestadas, el 82.7% gasta la totalidad de su ingreso.",
                    plotOutput("GRAFICO6"))
                )
        ),
        tabItem(tabName = "tarjeta",
                h1("Tarjeta de crédito"),
                fluidRow(
                  
                  box(
                    title = "TARJETA DE CRÉDITO", width = 12, status = "primary", 
                    solidHeader = TRUE, 
                    "Esta pregunta fue respondida por el 99.4% de la poblaciÓn encuestada. Del total de personas encuestadas, 
                    el 75.6% no tienen tarjetas de crédito.",
                    plotOutput("GRAFICO7")),
                  box(
                    title = "CRÉDITO DE LIBRE INVERSIÓN", width = 12, status = "primary", solidHeader = T, 
                    "Del 94% de personas encuestadas que respondieron esta pregunta, el 11.4% tiene créditos de libre inversión",
                    plotOutput("GRAFICO8")
                  )
                  
                  
                )),
        tabItem(tabName = "barrio",
                fluidRow(
                  box(
                    title = "CRÉDITOS EDUCATIVOS", width = 12, height = 500, 
                    status = "primary", solidHeader = TRUE, 
                    " 151 personas del total de encuestadas no respondieron si tienen créditos educativos.",
                    plotOutput("GRAFICO10")),
                  box(
                    title = "PRESTAMOS EN LA TIENDA DEL BARRIO", width = 12, height = 500, 
                    status = "primary", solidHeader = TRUE, 
                    "Esta pregunta fue respondida por el 99.4% de la poblaciÓn encuestada. Del total de personas encuestadas, el 96% no tienen créditos en la tienda del barrio.",
                    plotOutput("GRAFICO9"))
                  
                )),
        tabItem(tabName = "medio",
                fluidRow(
                  box(
                    title = "COMPRAS CON TARJETA DE CRÉDITO", width = 12, status = "primary", 
                    solidHeader = TRUE, 
                    "Esta pregunta no fue respondida por 20011 personas del total de encuestadas",
                    plotOutput("GRAFICO16"))
                )),
        tabItem(tabName = "aumento",
                fluidRow(
                  
                  
                  box(
                    title = "AUMENTO DE COMPRAS CON TARJETAS DE CRÉDITO", height = 555, width = 12, status = "primary", solidHeader = TRUE, 
                    "21482 personas no reespondieron si consideran que el uso de tarjetas de crédito ha aumentado desde el momento que accedieron a él.",
                    plotOutput("GRAFICO17")),
                  
                  box(
                    title = "FRECUENCIA DE COMPRAS CON TARJETA DE CREDITO", height = 555, width = 12, status = "primary", 
                    solidHeader = TRUE, 
                    " 21482 personas no respondieron con qué frecuencia utilizan las tarjetas de crédito como medio de compra de bienes y servicios.",
                    plotOutput("GRAFICO18"))
                  
                  
                )),
        tabItem(tabName = "activos",
                fluidRow(
                  box(
                    title = "CUENTA DE AHORROS", width = 12, status = "primary", 
                    solidHeader = TRUE, "Esta pregunta fue respondida por el 99.4% de las personas encuestadas, de las cuales el 29% tienen cuenta de ahorros.",
                    plotOutput("GRAFICO13")),
                  box(
                    title = "CDT", height = 555, status = "primary", solidHeader = TRUE, 
                    "Esta pregunta fue respondida por el 99.4% de las personas encuestadas, de las cuales el 97.2% no tienen CDT.",
                    plotOutput("GRAFICO12")),
                  
                  box(
                    title = "ACCIONES", height = 555, status = "primary", 
                    solidHeader = TRUE, 
                    " Del 99.4% de personas que respondieron esta pregunta, el 98% no tienen acciones",
                    plotOutput("GRAFICO11"))
                  
                )),
        tabItem(tabName = "carga",
                fluidRow(
                  box(
                    title = "DEUDAS / INGRESO", width = 12, height = 500, status = "primary", solidHeader = TRUE, 
                    "Esta pregunta no fue respondida por 151 personas del total de encuestadas",
                    plotOutput("GRAFICO14")),
                  box(
                    title = "NIVEL DE ENDEUDAMIENTO", width = 12, height = 500, status = "primary", solidHeader = TRUE, 
                    "Esta pregunta no fue respondida por 15949 personas del total de encuestadas",
                    plotOutput("GRAFICO15"))
                  
                  
                ))
        )
      
      
    )
    )
  )


server <- function(input, output){
  
  output$sum <- renderPrint({
    summary(datos)
  
  })
  
  output$GRAFICO1 <- renderPlot({
    ggplot(datos, aes(DEPTO, fill=SEXO)) +
      geom_bar(position = "dodge", color="black") + 
      theme_minimal()+ 
      scale_y_continuous(breaks = c(0, 3000, 5000))
    
  })
  output$GRAFICO3 <- renderPlot({
    ggplot(E, aes(x=EDAD, y =conteo, group=1)) + 
      geom_line(color="midnightblue") + geom_point(color="royalblue3") + theme_test() + xlab("Edad") + 
      ylab("Cantidad de personas") + geom_text(aes(label=conteo), vjust=-0.6, size=3.9) + 
      theme(legend.position="top") + guides(fill = guide_legend(nrow = 1)) 
    
  })
  
  output$GRAFICO2 <- renderPlot({
    ggplot(count.data, aes(x=2, y=prop, fill = class)) +
      geom_bar(stat = "identity", color = "gray", size = 1) + 
      coord_polar(theta = "y", start = 0) + 
      geom_text(aes(y=lab.ypos, label = percent(prop/100)), 
                color = "black", size=5) +
      scale_fill_manual(values = mycolsexo) +
      theme_void() + xlim(0.6, 2.5)
    
  })
  
  output$GRAFICO5 <- renderPlot({
    ggplot(CH, aes(x=CREDITO_HIP, y=conteo)) + geom_bar(stat="identity", fill=(c("darkblue")))+ 
      facet_grid(CREDITO_HIP ~., scales = "free_y") + theme_test() + 
      geom_text(aes(label=conteo), vjust=-0.3, size=6.0) + 
      scale_y_continuous(limits = c(0, 25000)) + 
      ylab("Cantidad de personas") + xlab("Categorias") + 
      theme(strip.background = element_rect(fill="paleturquoise1"))
    
  })
  
  output$GRAFICO6 <- renderPlot({
    ggplot(data = GTI, aes(x = GASTO_TOTAL_INGRESO, y=conteo), fill=supp) + 
      geom_bar(stat = "identity", fill=(c("skyblue3")), colour="darkblue") + 
      coord_flip() + geom_text(aes(label=conteo),vjust=2, size=5.5) + 
      theme_test() + xlab("Gasto total del ingreso") + ylab("Cantidad de personas")
    
  })
  
  output$GRAFICO7 <- renderPlot({
    ggplot(TC, aes(x=TARJETA_CREDITO, y=conteo)) + geom_bar(stat="identity", 
                                                            fill=(c("dodgerblue")), color="darkblue") + 
      facet_grid(TARJETA_CREDITO ~., scales = "free_y") + theme_test() + 
      geom_text(aes(label=conteo), vjust=-0.04, size=6.5) + 
      scale_y_continuous(limits = c(0, 22000)) + 
      ylab("Cantidad de personas") + xlab("Categorias") + 
      theme(strip.background = element_rect(fill="lightcyan1"))
  })
  output$GRAFICO8 <- renderPlot({
    ggplot(OP, aes(x=OTROS_PRESTAMOS, y =conteo, group=1)) + 
      geom_line(color="darkblue") + geom_point(color="black") + 
      theme_minimal() + xlab("Categorias") + 
      ylab("Cantidad de personas") + 
      geom_text(aes(label=conteo), vjust=-0.5, size=4.5) +
      scale_y_continuous(breaks=NULL)
  })
  
  output$GRAFICO9 <- renderPlot({
    ggplot(data=TB, aes(x=TIENDA_BARRIO, y=conteo), fill=supp) +
      geom_bar(stat="identity", fill=(c("palevioletred4"))) +
      geom_text(aes(label=conteo), vjust=-0.3, size=4.5)+
      theme_test() + xlab("") + ylab("Cantidad de personas") + 
      scale_y_continuous(limits = c(0, 28000))
    
  })
  
  output$GRAFICO10 <- renderPlot({
    ggplot(CE, aes(x=CREDITO_EDUCATIVO, y =conteo, group=1)) + 
      geom_line(color="darkred") + geom_point(color="firebrick4") + 
      theme_minimal() + xlab("Categorias") + 
      ylab("Cantidad de personas") + geom_text(aes(label=conteo), vjust=-0.5, size=4.5) + 
      theme_test() 
  })
  
  output$GRAFICO11 <- renderPlot({
    ggplot(A, aes(x=ACCIONES, y =conteo, group=1)) + 
      geom_line(color="navyblue") + geom_point(color="darkblue") + 
      theme_minimal() + 
      xlab("Categorias") + 
      ylab("Cantidad de personas") + 
      scale_y_continuous(limits = c(0, 30000)) + 
      geom_text(aes(label=conteo), vjust=-0.5, size=4.5) + 
      scale_y_continuous(breaks=NULL)
    
    
  }) 
  
  output$GRAFICO12 <- renderPlot({
    ggplot(C, aes(x=CDT, y =conteo, group=1)) + 
      geom_line(color="navyblue") + geom_point(color="darkblue") + 
      theme_minimal() + xlab("Categorias") + 
      ylab("Cantidad de personas") + 
      geom_text(aes(label=conteo), vjust=-0.5, size=4.5) + 
      scale_y_continuous(limits = c(0, 30000)) + 
      scale_y_continuous(breaks=NULL)
    
  })
  
  output$GRAFICO13 <- renderPlot({
    ggplot(CA, aes(x=CTA_AHORROS, y =conteo, group=1)) + 
      geom_line(color="darkblue") + geom_point(color="black") + 
      theme_minimal() + xlab("Categorias") + 
      ylab("Cantidad de personas") + 
      geom_text(aes(label=conteo), vjust=-0.5, size=4.5) +
      scale_y_continuous(breaks=NULL)
    
  })
  
  output$GRAFICO14 <- renderPlot({
    ggplot(data = D, aes(x = DEUDAS, y=conteo), fill=supp) + 
      geom_bar(stat = "identity", fill=(c("goldenrod3")), color="goldenrod4") + 
      coord_flip() + geom_text(aes(label=conteo), vjust=-0.5, size=5.5) + 
      theme_minimal() + ylab("Cantidad de personas") + xlab("Deudas/Ingreso") 
    
  })
  
  output$GRAFICO15 <- renderPlot({
    
    ggplot(NE, aes(x=NIVEL_ENDEUDAMIENTO, y=conteo)) + 
      geom_bar(stat = "identity", fill=(c("mediumblue")), color="navy") + 
      coord_flip() + facet_wrap(NIVEL_ENDEUDAMIENTO~.) + theme_test() + 
      geom_text(aes(label=conteo), vjust=-0.7, size=4.5) + 
      theme(strip.background = element_rect(fill="blanchedalmond")) + xlab("Nivel de endeudamiento")
    
  })
  
  output$GRAFICO16 <- renderPlot({
    
    
    ggplot(data=CTC, aes(x=COMPRAS_TARJETA_CREDITO, y=conteo), fill=supp) +
      geom_bar(stat="identity", fill=(c("palevioletred4"))) +
      geom_text(aes(label=conteo), vjust=-0.3, size=4.5)+
      theme_test() + xlab("") + ylab("Cantidad de personas") + 
      scale_y_continuous(limits = c(0, 28000))
    
  })
  
  output$GRAFICO17 <- renderPlot({
    ggplot(data = ACC, aes(x = AUMENTO_COMPRAS_CREDITO, y=conteo), fill=supp) + 
      geom_bar(stat = "identity", fill=(c("skyblue2")), color=("darkblue")) + 
      coord_flip() + geom_text(aes(label=conteo),vjust=-0.3, size=4.5) + 
      theme_test() + xlab("Aumento de compras con tarjeta de crédito") + ylab("Cantidad de personas")
    
  })
  
  output$GRAFICO18 <- renderPlot({
    ggplot(data = FCC, aes(x = FRECUENCIA_COMPRAS_CREDITO, y=conteo), fill=supp) + 
      geom_bar(stat = "identity", fill=(c("steelblue")), color=("darkblue")) + 
      coord_flip() + geom_text(aes(label=conteo),vjust=-0.3, size=4.5) + 
      theme_test() + xlab("")
    
    
  })
  
  output$GRAFICO19 <- renderPlot({
    barplot(AÑOS, names = colnames(año$AÑOS), 
            ylab = "Cantidad de personas encuestadas", ylim = c(0, 90000), 
            col = c("cadetblue3", "royalblue3"), 
            legend.text=c("28144 hogares encuestados", 
                          "62838 hogares encuestados"))
    
    
    
  })
}

shinyApp(ui, server)




