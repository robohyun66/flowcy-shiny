#library(shiny)
library(plotly)
library(dplyr)
library(plotly)
library(devtools)
library(tidyr)
library(data.table)
library(ggplot2)
library(gganimate)
library(randomcoloR)
source("viz.R")
source("data.R")

ui <- fluidPage(
    fluidRow(
        column(6,
               plotlyOutput(outputId = "row_selected")
        ),
        column(4,
               div(DT::dataTableOutput("table"), style = "font-size:60%; width: 10%")
        )      
    ),
    fluidRow(
        column(6,
               plotlyOutput("p") 
               ),
        column(4,
               verbatimTextOutput("event")
        )
        
    )
)

server <- function(input, output, session) {
    output$event <- renderPrint({
      # get click information
        d <- event_data("plotly_click")
        if(is.null(d))
            "The selected cluster number is Here"
        else{
            d$key
        }
        
    })
    output$table = DT::renderDataTable({
        beta_index_first = 0
        beta_index_second = 0
        selected = event_data("plotly_click")
        if(!is.null(selected)){
            cluster_num = as.numeric(selected$key)
            alpha = t(round(as.data.frame(res$alpha)[cluster_num,],2))
            beta_index_first = (cluster_num-1)*3 + 1
            beta_index_second = beta_index_first + 2
            if(beta_index_first != 0){
                beta = round(as.data.frame(res$beta)[,beta_index_first:beta_index_second],2)
            }
            cbind(alpha,beta)
        }
    })
    output$row_selected <- renderPlotly({
      # prob_plot 
      res$prob = res$pie
      prob_table = res %>% make_prob_table()
      prob_plot <- prob_table %>% make_prob_plot()
      # cov_plot
      covariates <- res %>% make_covariates_table()
      covariates_plot <- covariates %>% make_covariates_plot()
    
       subplot(covariates_plot,prob_plot,nrows = 2)
    })
    
    observeEvent(input$table_rows_selected, {
      s = input$table_rows_selected
      covariates = res$X[,s]
      covariates = as.data.frame(covariates)
      covariates = setDT(covariates, keep.rownames = "Time")[]
      covariates <- covariates%>%
        gather(var,val,-c(1))
      colnames(covariates) = c("Time","Trace","Value")
      plotlyProxy("row_selected",session)%>%
        plotlyProxyInvoke("addTraces",list(x = covariates$Time,y = covariates$Value, line = list(color = "red"))
          
        )
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
