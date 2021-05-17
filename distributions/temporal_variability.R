library(ggplot2)
library(dplyr)
library(shiny)
library(shinydashboard)
library(plotly)
library(htmltools)

ui = dashboardPage(
  dashboardHeader(title = "Looking at crash temporal variations"),
  dashboardSidebar(
    sliderInput(label = "Average number of crashes per month (1)",
                inputId = "mean_1",
                value = 10,
                min = 0.01,
                max = 100,
                step = 1),
    sliderInput(label = "Average number of crashes per month (2)",
                inputId = "mean_2",
                value = 10,
                min = 0.01,
                max = 100,
                step = 1),
    sliderInput(label =  "Interval to calculate the mean",
                inputId = "interval",
                value = 12,
                min = 1,
                max = 12*10,
                step = 1),
    sliderInput(label =  "NB_size parameter",
                inputId = "size_parameter",
                value = 0.5,
                min = 0.1,
                max = 10,
                step = 0.1)
  ),
  dashboardBody(
    tabBox(
      tabPanel(
        title = "Temporal series",
        plotlyOutput("result", width = "100%")
      ),
      tabPanel(
        title = "Histograms",
        plotlyOutput("histogram", width = "100%")
      )
    )
  )
)


server = function(input, output){
  
  output$result = renderPlotly({
    size_parameter = input$size_parameter
    mean_1 = input$mean_1
    mean_2 = input$mean_2
    sample_size = 120
    
    my_data = data.frame(i = 1:sample_size, rnd = runif(sample_size)) 
    my_data$y = qnbinom(p = my_data$rnd,mu = mean_1, size = size_parameter)
    
    my_data_2 = data.frame(i = 1:sample_size, rnd = runif(sample_size)) 
    my_data_2$y = qnbinom(p = my_data_2$rnd, mu = mean_2, size = size_parameter)
    
    
    interval = input$interval
    breaks = seq(0,sample_size,interval)
    
    my_data$bin = cut(my_data$i, breaks) 
    my_data_b = my_data %>% group_by(bin) %>% mutate(y = mean(y))
    
    my_data_2$bin = cut(my_data_2$i, breaks) 
    my_data_2b = my_data_2 %>% group_by(bin) %>% mutate(y = mean(y))
    
    
    my_data = my_data %>% mutate(type = "raw data - monthly") %>% mutate(location = "1")
    my_data_b = my_data_b %>% mutate(type = "mean") %>% mutate(location = "1")
    
    my_data_2 = my_data_2 %>% mutate(type = "raw data - monthly") %>% mutate(location = "2")
    my_data_2b = my_data_2b %>% mutate(type = "mean") %>% mutate(location = "2")
    
    
    my_data = my_data %>% bind_rows(my_data_2) %>% bind_rows(my_data_b) %>% bind_rows(my_data_2b)
    
    ggplot(my_data, aes(x = i, y = y, group = type, color = type)) + geom_point() +
      geom_line() +
      scale_color_manual(values = c("#D96A6A","#7A95DE")) + 
      ylim(0,max(my_data$y)*1.2) + facet_grid(.~location) + xlab("Time (month)") + ylab("Number of crashes/month")
    ggplotly(height = 800, width = 1600) 
  })
  
  output$histogram = renderPlotly({
    size_parameter = input$size_parameter
    mean_1 = input$mean_1
    mean_2 = input$mean_2
    sample_size = 120
    
    my_data = data.frame(i = 1:sample_size, rnd = runif(sample_size)) 
    my_data$y = qnbinom(p = my_data$rnd,mu = mean_1, size = size_parameter)
    
    my_data_2 = data.frame(i = 1:sample_size, rnd = runif(sample_size)) 
    my_data_2$y = qnbinom(p = my_data_2$rnd, mu = mean_2, size = size_parameter)
    
    my_data = my_data %>% mutate(type = "raw data") %>% mutate(location = "1")
    my_data_2 = my_data_2 %>% mutate(type = "raw data") %>% mutate(location = "2")
   
    my_data = my_data %>% bind_rows(my_data_2)
    
    ggplot(my_data, aes(x = y, fill = location, alpha = 0.2)) + geom_density() +
      xlab("Number of crashes/month") + 
      scale_fill_manual(values = c("#85A2D1", "#294065")) + 
      ylab("Frequency")
    ggplotly(height = 800, width = 1600) 
  })
  
}

shinyApp(ui, server)
