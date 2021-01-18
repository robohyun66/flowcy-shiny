library(shiny)
library(plotly)
# code to for pie_plot(this is the same as in viz.R)
myColor <- randomcoloR::distinctColorPalette(k = numclust + 1)
key = highlight_key(pie_table,~Cluster)
pie_plot <- plot_ly(key)%>%
  group_by(Cluster)%>%
  add_lines(x = ~Time , y = ~Prob,color = ~Cluster, colors = myColor[2:numclust + 1] ,)%>%
  add_segments(x = ~Time, xend = ~Time, y = 0, yend = 1, frame = ~Time,showlegend = FALSE, color = myColor[1],showlegend = FALSE)
pie_plot <- pie_plot%>%layout(
  xaxis = list(
    type = "date",
    tickformat = "%d%M%Y",showticklabels = FALSE,showgrid = FALSE),
  yaxis = list(showgrid = FALSE,title = c("Cluster Probabilities"),titlefont = list(size = 8)),
  showlegend = FALSE,
  title = ""
)
pie_plot <- pie_plot%>%highlight(
  on = "plotly_click", 
  off = "plotly_doubleclick",
  selectize = FALSE, 
  dynamic = FALSE, 
  persistent = FALSE,
  showlegend = F
)

# code for cv_plot
cv_plot <- plot_ly(covariates)%>%
  group_by(Trace)%>%
  add_lines(x = ~Time , y = ~Value,color = "grey50",opacity = 0.1)%>%
  add_segments(x = ~Time, xend = ~Time, y = -6, yend = 6, frame = ~Time,showlegend = FALSE, color = myColor[1],showlegend = FALSE)

# modify the x-axis  
cv_plot <- cv_plot%>%layout(
  xaxis = list(
    type = "date",
    tickformat = "%d%M%Y",showticklabels = FALSE,showgrid = FALSE),
  yaxis = list(showgrid = FALSE, title = c("EnvironmenTal Covariates"),titlefont = list(size = 8)),
  showlegend = FALSE
)
cv_plot

ui <- fluidPage(
    fluidRow(
        column(6,
               plotlyOutput(outputId = "p")
        ),
        column(4,
               div(DT::dataTableOutput("table"), style = "font-size:60%; width: 10%")
        )      
    ),
    fluidRow(
        column(6,
               plotlyOutput("row_selected") 
               ),
        column(4,
               verbatimTextOutput("event")
        )
        
    )
)

server <- function(input, output, session) {
    output$p <- renderPlotly({
      # A plotly object 
        pie_plot
      #  s = input$table_rows_selected
      #  if(is.null(s)) s = dim(res$X)[2]
      #  covariates = res$X[,s]
      #  covariates = as.data.frame(covariates)
      #  covariates = setDT(covariates, keep.rownames = "Time")[]
      #  covariates <- covariates%>%
      #      gather(var,val,-c(1))
      #  colnames(covariates) = c("Time","Trace","Value")
      #  cv_plot = cv_plot%>%add_data(covariates)%>%group_by(Trace)%>%add_lines(x = ~Time , y = ~Value,mode = "lines")
      #  subplot(pie_plot,cv_plot,nrows = 2,shareX = TRUE)
    })
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
            alpha = t(as.data.frame(res$alpha)[cluster_num,])
            beta_index_first = (cluster_num-1)*3 + 1
            beta_index_second = beta_index_first + 2
            if(beta_index_first != 0){
                beta = as.data.frame(res$beta)[,beta_index_first:beta_index_second]
            }
            cbind(alpha,beta)
        }
    })
    output$row_selected <- renderPlotly({
        s = input$table_rows_selected
        if(is.null(s)){
            s = dim(res$X)[2]
            cv_plot
        }else{
            covariates = res$X[,s]
            covariates = as.data.frame(covariates)
            covariates = setDT(covariates, keep.rownames = "Time")[]
            covariates <- covariates%>%
                gather(var,val,-c(1))
            colnames(covariates) = c("Time","Trace","Value")
            cv_plot = cv_plot%>%add_data(covariates)%>%group_by(Trace)%>%add_lines(x = ~Time , y = ~Value,mode = "lines")
            cv_plot
        }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
