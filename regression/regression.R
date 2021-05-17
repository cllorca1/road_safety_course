library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(plotly)
library(htmltools)
library(here)

dataset = read.csv(paste(here(),"dataset.csv", sep = "/"))
error = data.frame() 
iteration = 0
  
summary(lm(dataset, formula = Crashes ~ AADT + L))

ui = dashboardPage(
  dashboardHeader(title = "Regression models for crashes"),
  dashboardSidebar(
    sliderInput(label = "Linear model Intercept",
                inputId = "a",
                value = -18,
                min = -30,
                max = 30,
                step = 1),
    sliderInput(label = "Linear model AADT/10,000 coefficient",
                inputId = "b",
                value = 0,
                min = 0,
                max = 10,
                step = 0.01),
    sliderInput(label = "Linear model L coefficient",
                inputId = "c",
                value = 1,
                min = 0,
                max = 2,
                step = 0.001),
    sliderInput(label = "Poisson exp(beta0)",
                inputId = "beta0",
                value = -5,
                min = -20,
                max = 20,
                step = 0.1),
    sliderInput(label = "Poisson exp(beta1) AADT",
                inputId = "beta1",
                value =0,
                min = 0,
                max = 2,
                step = 0.01),
    sliderInput(label = "Poisson exp(beta2) L",
                inputId = "beta2",
                value =1,
                min = 0,
                max = 2,
                step = 0.01)
  ),
  dashboardBody(
    tabBox(
      tabPanel(
        title = "Linear model results y-y_est",
        plotlyOutput("result", width = "100%")
      ), 
      tabPanel(
        title = "Linear model equation y-aadt",
        sliderInput(label = "Select a first segment length",
                    inputId = "my_l",
                    value = 25,
                    min = 0,
                    max = 100,
                    step = 5),
        sliderInput(label = "Select a second segment length",
                    inputId = "my_l2",
                    value = 50,
                    min = 0,
                    max = 100,
                    step = 5),
        plotlyOutput("result_aadt_eq", width = "100%")
      ),
      tabPanel(
        title = "Poisson log-linear model results y-y_est",
        plotlyOutput("result_poisson", width = "100%")
      ), 
      tabPanel(
        title = "Poisson log-linear equation y-aadt",
        sliderInput(label = "Select a first segment length",
                    inputId = "my_l_poisson",
                    value = 25,
                    min = 0,
                    max = 100,
                    step = 5),
        sliderInput(label = "Select a second segment length",
                    inputId = "my_l2_poisson",
                    value = 50,
                    min = 0,
                    max = 100,
                    step = 5),
        plotlyOutput("result_aadt_eq_poisson", width = "100%")
      ),
      width = 1200, height = 1200)
  )
)


server = function(input, output, session){
  
  output$result = renderPlotly({
    a = input$a
    b = input$b
    c = input$c
    
    dataset  = dataset %>% mutate(Crashes_est = a + b * AADT/10000 + c * L)
    
    ggplot(dataset, aes(x = Crashes, y = Crashes_est)) + geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") + 
      xlab("Observed") +
      ylab("Estimated")
    ggplotly(height = 800)
  })
  
  
  output$result_aadt_eq = renderPlotly({
    a = input$a
    b = input$b
    c = input$c
    
    dataset  =dataset %>% mutate(Crash_est = a + b * AADT/10000 + c * input$my_l) %>% mutate(l = input$my_l)
    dataset_2  =dataset %>% mutate(Crash_est = a + b * AADT/10000 + c * input$my_l2) %>% mutate(l = input$my_l2)
    
    ggplot(dataset %>% bind_rows(dataset_2),
           aes(x = AADT, y = Crash_est, group = l, color = as.factor(l))) + geom_path() +
      xlab("AADT") +
      ylab("Estimated number of crashes") + 
      geom_hline(yintercept = 0, color = "red")
    ggplotly(height = 800)
  })
  
  output$result_poisson = renderPlotly({
    beta0 = input$beta0
    beta1 = input$beta1
    beta2 = input$beta2
    
    dataset  = dataset %>% mutate(Crashes_est = exp(beta0) * AADT ^ beta1 * L ^ beta2)
    
    
    ggplot(dataset, aes(x = Crashes, y = Crashes_est)) + geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") + 
      xlab("Observed") +
      ylab("Estimated")
    ggplotly(height = 800)
  })
  
  output$result_aadt_eq_poisson = renderPlotly({
    beta0 = input$beta0
    beta1 = input$beta1
    beta2 = input$beta2
    
    dataset = dataset %>% arrange(AADT)
    
    dataset  =dataset %>%
      mutate(Crashes_est = exp(beta0) * AADT ^ beta1 * input$my_l_poisson ^ beta2) %>%
      mutate(l = input$my_l_poisson)
    dataset_2  =dataset %>%
      mutate(Crashes_est = exp(beta0) * AADT ^ beta1 * input$my_l2_poisson ^ beta2) %>%
      mutate(l = input$my_l2_poisson)
    
    ggplot(dataset %>% bind_rows(dataset_2),
           aes(x = AADT, y = Crashes_est, group = l, color = as.factor(l))) + geom_path() +
      xlab("AADT") +
      ylab("Estimated number of crashes") + 
      geom_hline(yintercept = 0, color = "red")
    ggplotly(height = 800)
  })
  
  
  
}

shinyApp(ui, server)
