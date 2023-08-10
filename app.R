library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(forecast) 

idlink <- "1exgR0qDLWPOytp0mVyLOjwFDIDjOfVJB"
datos <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", idlink), fileEncoding = "Latin1")
datos[is.na(datos)] <- 0
Dels <- c("Aborto", "Abuso de confianza", "Abuso sexual", "Acoso sexual", 
          "Allanamiento de morada", "Amenazas", "Contra el medio ambiente",
          "Corrupción de menores","Daño a la propiedad","Delitos cometidos por servidores públicos"
          ,"Despojo","Electorales","Evasión de presos","Extorsión","Falsedad",
          "Falsificación","Feminicidio","Fraude","Homicidio","Hostigamiento sexual"
          ,"Incesto","Incumplimiento de asistencia familiar","Lesiones","Narcomenudeo",
          "Delitos contra patrimonio","Delitos contra familia","Delitos contra sociedad"
          ,"Delitos del fuero común","Delitos que atentana la libertad personal"
          ,"Delitos contralibertad y seguridad sexual","Delitos contra vida e integridad corporal"
          ,"Rapto","Robo","Secuestro","Tráfico de menores","Trata de personas","Violación equiparada",
          "Violación","Violencia de genero","Violencia familiar")
valores_ag <- aggregate(cbind(Enero, Febrero, Marzo, Abril, Mayo, Junio, Julio, Agosto, Septiembre, Octubre, Noviembre, Diciembre) ~ Tipo.de.delito + Año,data = datos, FUN = sum)
datos_modelo <- aggregate(. ~ Tipo.de.delito + Año, data = valores_ag, sum)
datos_modelo$Tipo.de.delito <- replace(datos_modelo$Tipo.de.delito, !is.na(datos_modelo$Tipo.de.delito) ,Dels)
datos_modelo <- datos_modelo %>% pivot_longer(cols = c(Enero:Diciembre), names_to = "Mes", values_to = "Delitos") %>% mutate(Mes = match(Mes, Mes))
datos_modelo <- subset(datos_modelo, !(Año >= 2023 & Mes > 6))

ui <- dashboardPage(
  dashboardHeader(title = "Crime rate"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  withSpinner(
                    plotOutput("plot1", height = 400)
                  )
                ),
                
                box(
                  h2("Crime"),
                  selectInput("crimes", "Select a crime", choices = Dels, 1
                  ),
                  box(
                    h2("Prediccion que se hizo este mes"),
                    h2(textOutput("result"))
                  )
                ),
              ),
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$crimes, {
    output$plot1 <- renderPlot({withSpinner(plotOutput("plot1", height=400))})
    data = input$crimes
    train <- subset(datos_modelo, Tipo.de.delito == data)
    train2 <- select(train, Delitos)
    crimets <- ts(data = train2, start = c(2015, 1),end = c(2023, 5), frequency = 12)
    fit <- auto.arima(crimets)
    reslt = forecast(fit,1)
    output$result <- renderText(reslt$mean)
    output$plot1 <- renderPlot({
      plot(forecast(fit,12), xlab ="Año", ylab ="Casos", main ="Incremento proyectado a 1 año", col.main ="black")
    })
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
