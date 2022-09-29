library(shiny)
library(shinydashboard)
library(colourpicker)
library(data.table)
library(DT)
library(car)

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "DASHBOARD"),
    dashboardSidebar(
            sidebarMenu(
                    fileInput("csv_input", "Select CSV File to Import", accept = ".csv"),
                    checkboxInput(inputId = "header", 
                                  label = "Baris pertama merupakan nama kolom", 
                                  value = TRUE),
                    selectInput(inputId = "Sep", label = "Choose Separator",
                                choices = c("semicolon (;)"=";",
                                            "comma (,)"=",",
                                            "pipe (|)"="|",
                                            "space ( )"=" ")),
                    selectInput(inputId = "var1",
                                label = "Pilih Variabel Numerik:",
                                choices = NULL), 
                    selectInput(inputId = "num", label = "Pilih Jenis Visualisasi", 
                                c("Histogram", "Boxplot", "Density", "scatter plot","QQ-Plot")),
                    conditionalPanel(
                        condition = "input.num == 'scatter plot'",
                        selectInput("var2",
                                    label = "Pilih Variabel Numerik ke-2:",
                                    choices = NULL,
                                    multiple = FALSE,
                                    selected = NULL))
                )
        ),
    
    dashboardBody(
       tabsetPanel(
         tabPanel("Data Table", 
                  dataTableOutput(outputId = "tabel"), style= "height:500px; overflow-y: scroll;overflow-x: scroll;"),
          tabPanel(
               title = "Plot",
               colourInput("warna", label = "Pilih warna chart",
                           value = "lightblue", returnName = T,
                           allowTransparent = T),
               plotOutput(outputId = "plot"),
               conditionalPanel(
                 condition = "input.num == 'Histogram'",
                 fluidRow(
                   column(4,
                          sliderInput("slider", "Pilih Break Points", 
                                      1, 100, 12, width = "420px")),
                   column(3,
                          selectInput("freqProp", "Pilih Jenis Data",
                                      c("Frekuensi", "Proporsi")),
                          conditionalPanel(
                            condition = "input.freqProp == 'Proporsi'",
                            checkboxInput("histDen", "Density Plot", value=T)
                          )
                   )
                 )
               ),
               conditionalPanel(
                 condition = "input.num == 'Density'",
                 checkboxInput("fillDen", "Fill Density Plot"),
                 checkboxInput("banwidth", "Atur Bandwidth"),
                 conditionalPanel(
                   condition = "input.banwidth == true",
                   sliderInput("sliderDen", "Pilih Banwidth", 
                               1, 100, 50, width = "420px")
                 )
               )
           ),
          tabPanel("Statistics",
                   verbatimTextOutput(outputId = "summary")),
          tabPanel(title ="Tentang Aplikasi",
                   textOutput(outputId = "note"))
       )
    )
)

server <- function(input, output, session){
  csv_input <- reactive({
    req(input$csv_input)
    csv_input <- read.csv(input$csv_input$datapath, 
                          header = input$header,
                          sep = input$Sep)
})
  var <- reactive({
    data <- csv_input()[[input$var1]]
  })
  
  vartu <- reactive({
    data <- csv_input()[[input$var2]]
  })
  
  varti <- reactive({
    data <- csv_input()[[input$var3]]
  })
  
  observe({
    updateSelectInput(session = session, inputId = "var1",
                      choices = colnames(csv_input())[sapply(csv_input(), is.numeric)])
    updateSliderInput(session = session, inputId = "slider", label = "Pilih Break Points", 
                      min = 1, max = nrow(csv_input()), value = sqrt(nrow(csv_input()))+1)
    updateSelectInput(session = session, inputId = "var2",
                      choices = colnames(csv_input())[sapply(csv_input(), is.numeric)])
    updateSelectInput(session = session, inputId = "var3",
                      choices = colnames(csv_input())[sapply(csv_input(), is.numeric)])
  })
  
  output$tabel <- renderDataTable({
    datatable(csv_input(), 
                  options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    })
  
  output$summary <- renderPrint(
    stargazer::stargazer(
      csv_input(),
      type = "text",
      title = "Descriptive statistics",
      digits = 1,
      out = "table1.txt")
  )
  output$note <- renderText("STADEX atau Statistical Data Exploration merupakan aplikasi yang berguna untuk melakukan eksplorasi data dan memberikan informasi terkait statistika deskriptif seperti mean, median, max, min, percentil, dan standar deviasi sehingga membantu dalam memahami karakteristik dari data. Statistical Data Exploration juga dapat melakukan uji normalitas untuk melihat kenormalan dari data. Statistical Data Exploration hanya terbatas pada data bertipe numerik.
")
  
  output$plot <- renderPlot({
    if(input$num=="Histogram"){
      if(input$freqProp=="Frekuensi"){
        p <- hist(var(), xlab = input$var1, main = paste("Histogram of", input$var1), 
                  col = input$warna, breaks=input$slider)
      } else {
        p <- hist(var(), xlab = input$var1, main = paste("Histogram of", input$var1), 
                  col = input$warna, breaks=input$slider, freq=F)
      }
      if(input$histDen){
        p <- lines(density(var()), col = "black", lwd = 2)
      }
    } else if(input$num=="Boxplot"){
      p <- boxplot(var(), xlab = input$var1, main = paste("Boxplot of", input$var1), col = input$warna)
    } else if(input$num=="Density"){
      if(input$banwidth){
        p <- plot(density(var(), bw = input$sliderDen), xlab = input$var1,  lwd = 2,
                  main = paste("Density plot of", input$var1), col = input$warna)
        if(input$fillDen){
          p <- polygon(density(var(), bw = input$sliderDen), col = input$warna)
        }
      } else {
        p <- plot(density(var()), xlab = input$var1,  lwd = 2,
                  main = paste("Density plot of", input$var1), col = input$warna)
        if(input$fillDen){
          p <- polygon(density(var()), col = input$warna)
        }
      }
    } else if(input$num=="scatter plot"){
      p <- plot(var(), vartu(), main = paste("Scatter Plot of", input$var1, "vs", input$var2), col =input$warna,
                xlab = input$var1, ylab = input$var2)}
    else if(input$num=="QQ-Plot"){
      qqnorm(var(),main = paste("QQ-Plot of", input$var1, "vs", input$var3), col =input$warna,
                xlab = input$var1, ylab = input$var3) 
      qqline(var(), col = "red")}
    
  })
}

shinyApp(ui, server)

