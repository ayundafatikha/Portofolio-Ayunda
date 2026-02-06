# Library
library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyverse)
library(readxl)
library(dashboardthemes)
library(shinydashboardPlus)
library(e1071)
library(rlang)

# Set working directory (sesuaikan lokasi dataset)
setwd("D:/SMT 5/Data Mining/FP Ayunda")

# Load dataset
df <- read.csv("data_bikebuyers.csv"); df

# Train Naive Bayes model
naive_bayes_model <- naiveBayes(Purchased.Bike ~ Age + Cars + Income + Children + Commute.Distance, data = df)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Bike Buyers Dashboard Prediction and Visualization", titleWidth = 200),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Pendahuluan", tabName = "Pendahuluan", icon = icon("clipboard")),
      menuItem("Dataset", tabName = "Dataset", icon = icon("table")),
      menuItem("Visualisasi", tabName = "Visualisasi", icon = icon("chart-bar")),
      menuItem("Prediksi", tabName = "Prediksi", icon = icon("chart-line")),
      menuItem("Authors", tabName = "Authors", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Pendahuluan",
              box(title = "Pendahuluan", status = "primary", width = 12,
                  p("Dashboard ini dirancang untuk menganalisis data pembelian sepeda berdasarkan faktor-faktor seperti usia, jumlah mobil, pendapatan, jumlah anak, dan jarak komuter. Diharapkan dengan adanya dashboard ini memudahkan individu untuk mengetahui visualisasi dan juga prediksi dari pembelian sepeda berdasarkan variabel yang dipilih."))
      ),
      tabItem(tabName = "Dataset",
              tabBox(id = "data_view", width = 12,
                     tabPanel("Data", dataTableOutput("dfb")),
                     tabPanel("Struktur", verbatimTextOutput("structure")),
                     tabPanel("Summary", verbatimTextOutput("summary"))
              )
      ),
      tabItem(tabName = "Visualisasi",
              fluidRow(
                box(title = "Visualisasi Data", status = "primary", width = 12,
                    selectInput("plot_var", "Pilih Variabel untuk Visualisasi:", 
                                choices = names(df), selected = "Age"),
                    selectInput("plot_type", "Jenis Plot:", 
                                choices = c("Histogram", "Boxplot", "Pie Chart", "Bar Chart")),
                    plotOutput("data_plot"))
              )
      ),
      tabItem(tabName = "Prediksi",
              fluidRow(
                box(title = "Input Prediksi", status = "primary", width = 12,
                    numericInput("age", "Umur:", 30, min = 0, max = 100),
                    selectInput("cars", "Jumlah Mobil:", choices = c("0", "1", "2", "3", "4")),
                    numericInput("income", "Pendapatan:", 50000, min = 0),
                    numericInput("children", "Jumlah Anak:", 1, min = 0),
                    selectInput("commute", "Jarak Komuter:", choices = c("0-1 Miles", "1-2 Miles", "2-5 Miles", "5-10 Miles", "10+ Miles")),
                    actionButton("predict_btn", "Prediksi"),
                    verbatimTextOutput("prediction_result"))
              )
      ),
      tabItem(tabName = "Authors",
              fluidRow(
                box(title = "Authors", status = "primary", width = 12,
                    imageOutput("ayunda", height = 330, width=290),
                    p("Nama : Ayunda Fatikha"),
                    p("NRP : 5003221023"),
                    p("Data Mining dan Visualisasi C"),
                    p("Departemen Statistika, Fakultas Sains dan Analitika Data"),
                    p("Institut Teknologi Sepuluh Nopember")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$structure <- renderPrint({
    str(df)
  })
  output$summary <- renderPrint({
    summary(df)
  })
  output$dfb <- renderDataTable({
    datatable(df, options = list(scrollX = TRUE))
  })
  output$ayunda <- renderImage({
    list(src="www/ayunda.jpg", deleteFile=FALSE, height=330, width=290)
  }, deleteFile = FALSE)
  
  # Visualisasi
  output$data_plot <- renderPlot({
    req(input$plot_var, input$plot_type)
    plot_var <- input$plot_var
    
    if (input$plot_type == "Histogram") {
      ggplot(df, aes_string(x = plot_var)) +
        geom_histogram(binwidth = (max(df[[plot_var]], na.rm = TRUE) - min(df[[plot_var]], na.rm = TRUE)) / 30, 
                       fill = "blue", color = "black") +
        labs(title = paste("Histogram:", plot_var), x = plot_var, y = "Frequency")
      
    } else if (input$plot_type == "Boxplot") {
      ggplot(df, aes_string(y = plot_var)) +
        geom_boxplot(fill = "orange") +
        labs(title = paste("Boxplot:", plot_var), y = plot_var)
      
    } else if (input$plot_type == "Pie Chart") {
      df_grouped <- df %>% count(!!sym(plot_var))
      ggplot(df_grouped, aes(x = "", y = n, fill = !!sym(plot_var))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = paste("Pie Chart:", plot_var)) +
        theme_void()
      
    } else if (input$plot_type == "Bar Chart") {
      df_grouped <- df %>% count(!!sym(plot_var))
      ggplot(df_grouped, aes(x = !!sym(plot_var), y = n, fill = !!sym(plot_var))) +
        geom_bar(stat = "identity") +
        labs(title = paste("Bar Chart:", plot_var), x = plot_var, y = "Count") +
        theme_minimal()
    }
  })
  
  # Prediksi
  observeEvent(input$predict_btn, {
    new_data <- data.frame(
      Age = input$age,
      Cars = input$cars,
      Income = input$income,
      Children = input$children,
      Commute.Distance = input$commute
    )
    prediction <- predict(naive_bayes_model, new_data)
    output$prediction_result <- renderText({
      paste("Hasil Prediksi: ", ifelse(prediction == "Yes", "Membeli Sepeda", "Tidak Membeli Sepeda"))
    })
  })
}

# Jalankan Aplikasi
shinyApp(ui, server)

getwd
