library(shiny)
library(plotly)
library(deSolve)
library(DT)

############################
# Integrantes del Proyecto
###########################

#Cristan Javier Da Camara
#Camilo Andres Sandoval
#Kenneth Leonel Triana 

##########################################
#DATOS DE LOS CSV
##########################################
###Cargarmos archivos###
datos <- read.csv("../Desktop/Covid/total_cases1.csv")

recuperados = read.csv("../Desktop/Covid/RecuperadosCovid1.csv")

muertes = read.csv("../Desktop/Covid/total_deaths1.csv")

###Cargarmos archivos###

###########################
#Funciones
###########################


##############################
#Funcion bezier para interpolar
###############################
bezierCurve <- function(x, y, n=10)
{
  outx <- NULL
  outy <- NULL
  
  i <- 1
  for (t in seq(0, 1, length.out=n))
  {
    b <- bez(x, y, t)
    outx[i] <- b$x
    outy[i] <- b$y
    
    i <- i+1
  }
  
  return (list(x=outx, y=outy))
}

bez <- function(x, y, t)
{
  outx <- 0
  outy <- 0
  n <- length(x)-1
  for (i in 0:n)
  {
    outx <- outx + choose(n, i)*((1-t)^(n-i))*t^i*x[i+1]
    outy <- outy + choose(n, i)*((1-t)^(n-i))*t^i*y[i+1]
  }
  
  return (list(x=outx, y=outy))
}


############################
#Funcion Sir
############################
sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}


beta=1.46
gamma=0.5
sol1=ode(y = c(S = 0.9699, I = 2.255e-4, R = 0),
         times = seq(0, 132),
         func = sir_equations,
         parms = c(beta, gamma))


sol2=ode(y = c(S = 0.9699, I = 2.255e-4, R = 0),
         times = seq(0, 132),
         func = sir_equations,
         parms = c(beta, gamma),method="rk4")

##############
#ERRORES
##############
errorAbs=abs(sol1-sol2)

errorRelativOde =  (errorAbs /sol1)

errorRelativoRk4 = (errorAbs /sol2)


###Interfaz shiny###
if (interactive()) {
  ui <- fluidPage(
    ##Primera pestaña del shiny
    tabsetPanel(tabPanel("Graficas",
                         headerPanel('Coronavirus Proyecto'),
                         #Seleccion de paises a mostrar
                         sidebarPanel(
                           selectInput('xcol','escoja una opcion ', names(datos)),
                           selectInput('ycol','escoja una opcion', names(datos)),
                           selected = names(datos)[[2]]),
                         navlistPanel(
                           tabPanel("Contagiados,        Tabla 1"),
                           tabPanel("Recuperados,        Tabla 2"),
                           tabPanel("Muertes,            Tabla 3")
                         ),
                         #Muestra las graficas de los contagiados, recuperados y muertos
                         tabPanel("Tasa de contagiados",plotlyOutput('plot')),
                         tabPanel("Recuperados",plotlyOutput('plot1')),
                         tabPanel("Muertos",plotlyOutput('plot2'))
                         
    ),
    #Se muestra el modelo SIR para Colombia
    tabPanel("SIR", 
             headerPanel("MODELO SIR Colombia"),
             sliderInput(inputId = "time_values", label = "Tiempo", value = c(0, 38), min = 0, max = 60),
             sliderInput(inputId = "beta", label ="Beta", value = 1.36, min = 0.00, max = 1.5, step = 0.01),
             sliderInput(inputId = "gamma", label ="Gamma", value = 0.5, min = 0.00, max = 2.2, step = 0.1),
             tabPanel("Muertos", plotOutput("distplot"))
             
             
             
    ),
    tabPanel("SIR RK4", 
             headerPanel("MODELO SIR Colombia"),
             sliderInput(inputId = "time_values1", label = "Tiempo", value = c(0, 38), min = 0, max = 60),
             sliderInput(inputId = "beta1", label ="Beta", value = 1.36, min = 0.00, max = 1.5, step = 0.01),
             sliderInput(inputId = "gamma1", label ="Gamma", value = 0.5, min = 0.00, max = 2.2, step = 0.1),
             tabPanel("Muertos", plotOutput("distplot2"))
             
             
             
    ),
    tabPanel("Interpolacion", 
              headerPanel("Tablas con Interpolacion Bezier"),
              tabPanel( "contagiados",plotOutput("distplot4")),
              tabPanel( "contagiados",plotOutput("distplot5")),
              tabPanel( "contagiados",plotOutput("distplot6"))
             
             )
    
    
    ))
  
  server <- function(input, output) {
    #Se guarda la informacion del csv
    x <- reactive({
      datos[,input$xcol]
    })
    
    y <- reactive({
      datos[,input$ycol]
    })
    
    x1 <- reactive({
      recuperados[,input$xcol]
    })
    
    y1 <- reactive({
      recuperados[,input$ycol]
    })
    
    
    x2 <- reactive({
      muertes[,input$xcol]
    })
    
    y2 <- reactive({
      muertes[,input$ycol]
    })
    #Se guarda la informacion del csv    
    
    
    #Colocamos tablas en shiny
    output$plot <- renderPlotly(
      plot1 <- plot_ly(
        x = x(),
        y = y(), 
        type = 'scatter',
        mode = 'lines')%>% 
        layout(title = "Contagiados")
    )
    
    output$plot1 <- renderPlotly(
      plot1 <- plot_ly(
        x = x1(),
        y = y1(), 
        type = 'scatter',
        mode = 'lines')%>% 
        layout(title = "Recuperados")
    )
    
    
    output$plot2 <- renderPlotly(
      plot1 <- plot_ly(
        x = x2(),
        y = y2(), 
        type = 'scatter',
        mode = 'lines')%>% 
        layout(title = "Muertes")
    )
    #Colocamos tablas en shiny
    
    #Ecuacion del SIR
    sir_equations <- function(time, variables, parameters) {
      with(as.list(c(variables, parameters)), {
        dS <- -beta * I * S
        dI <-  beta * I * S - gamma * I
        dR <-  gamma * I
        return(list(c(dS, dI, dR)))
      })
    }
    #Solucion de ecuacion por ODE
    sir_values_1 <- reactive({
      req(input$time_values, input$beta, input$gamma)
      ode(y = c(S = 0.9699, I = 2.255e-4, R = 0),
          times = seq(input$time_values[1], input$time_values[2]),
          func = sir_equations,
          parms = c(beta = input$beta, gamma = input$gamma))
    })
    sir_values_2 <- reactive({
      req(input$time_values1, input$beta1, input$gamma1)
      ode(y = c(S = 0.9699, I = 2.255e-4, R = 0),
          times = seq(input$time_values1[1], input$time_values1[2]),
          func = sir_equations,
          parms = c(beta = input$beta1, gamma = input$gamma1),method="rk4")
      
    })
    
    #Se muestra la grafica para la solucion de ecuacion por ODE
    output$distplot <- renderPlot({
      val <- as.data.frame(sir_values_1())
      
      with(val, {
        plot(time, S, type = "l", col = "blue",
             xlab = "Tiempo (dias)", ylab = "Porcentaje de personas")
        lines(time, I, col = "red")
        lines(time, R, col = "green")
      })
      
      legend("right", c("susceptibles", "infectados", "recuperados"),
             col = c("blue", "red", "green"), lty = 1, bty = "n")
      
    })
    
    #Se muestra la grafica para la solucion de ecuacion por Rk4 
    output$distplot2 <- renderPlot({
      val1 <- as.data.frame(sir_values_2())
      
      with(val1, {
        plot(time, S, type = "l", col = "blue",
             xlab = "Tiempo (dias)", ylab = "Porcentaje de personas")
        lines(time, I, col = "red")
        lines(time, R, col = "green")
      })
      
      legend("right", c("susceptibles", "infectados", "recuperados"),
             col = c("blue", "red", "green"), lty = 1, bty = "n")
    })
    
    
    
   #################################
   # Renderplots de la interpolacion
   ##################################
    
    output$distplot4 <- renderPlot({
      #Cogemos los datos del csv y solo cogemos la informacion de Colombia (contagiados)
      contagiados = datos[ ,-c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16)]
      tiempo = c( 0:132)
      #Hacenos un plot vacio para que muestra la interpolacion por el metodo de bezier
      plot( tiempo, contagiados, "o", pch=20, col = "white", main = " Contagiados")
      #Utilizamos la funcion Beziercurve  utilizada en el reto N2 
      points(bezierCurve(tiempo,contagiados,20), type="l", col="blue")
      
      
    })
    
    output$distplot5 <- renderPlot({
      #Cogemos los datos del csv y solo cogemos la informacion de Colombia (recuperados)
      recover = recuperados[ ,-c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16)]
      tiempo = c( 0:132) 
      #Hacenos un plot vacio para que muestra la interpolacion por el metodo de bezier
      plot( tiempo, recover, "o", pch=20, col = "white",main = "Recuperados")
      #Utilizamos la funcion Beziercurve  utilizada en el reto N2 
      points(bezierCurve(tiempo,recover,20), type="l", col="Green")
      
    })
    
    output$distplot6 <- renderPlot({
      #Cogemos los datos del csv y solo cogemos la informacion de Colombia (muertos)
      muertos = muertes[ ,-c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16)]
      tiempo = c( 0:132)
      #Hacenos un plot vacio para que muestra la interpolacion por el metodo de bezier
      plot( tiempo, muertos, "o", pch=20, col = "white", main = "Muertes")
      #Utilizamos la funcion Beziercurve  utilizada en el reto N2 
      points(bezierCurve(tiempo,muertos,20), type="l", col="red")
      
    })
    
    
    
    
    
    
  }
  shinyApp(ui,server)
}

###Interfaz shiny###

##########################################
# datos De informacion Para el modelo Sir
###########################################
#Recuperados argentina = 1837/5776
#http://www.scielo.org.co/scielo.php?script=sci_arttext&pid=S0124-00642020000101101
#https://www.covidvisualizer.com/
