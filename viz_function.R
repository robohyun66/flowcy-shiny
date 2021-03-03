## contain helper functions needed in the process of loading,extracting and
## processing data

# List of color. It is initialized outside of function defintion to ensure that functions share same set of colors
myColor <- randomcoloR::distinctColorPalette(k = (res$numclust) + 1)
## SH: Fill in.

##' @param res Flowmix object.
##'
##' @return plot: a plotly object of the 3+1 plots.
##'
default_view <- function(res){

  ## For back-compatability.
  if(exists('pie', where=res)) res$prob = pie


  ## Form three tables.
  prob_table <- res %>% make_prob_table()
  covariates <- res %>% make_covariates_table()
  mn_table <- res %>% make_mn_table()
  combined_table <- full_join(mn_table,
                              prob_table,
                              by = c("Time", "Cluster"))

  ## prob_plot <- prob_table %>% make_prob_plot()
  ## covariates_plot <- covariates %>% make_covariates_plot()

  ## "combined_table = cbind(prob_table, mn_table)
  ## clustered_data_table <- cbind(pie_table, mn_table)[c(-7,-8)]
  ## SH: This code is bad for a few reasons (1) it's not readable, and (2)
  ## it's not good practice to manipulate data tables by column or row
  ## numbers. It's more reliable to use column names.
  ## Instead, you should do something like this
  ## "combined_table %>% select(-Time, -Cluster)"
  ## I chose to use dplyr::full_join().


  ## SH: convert_ylist_2d() is .
  table_list = convert_ylist_2d(ylist)
  myColor <- randomcoloR::distinctColorPalette(k = numclust + 1)


  ## SH: You should continue from here. I've wrapped a lot of this functionality
  ## into `make_prob_plot` and `make_covariate_plot`; you should use these
  ## functions whenever possible. For instance, the whole block that starts with
  ## the comment "# create highlight key, which highlights by cluster" should be
  ## replaced by calls to `make_prob_plot()`.

  # Plot probability
  clustered_data_table$Cluster <- sapply(clustered_data_table$Cluster,as.character)

  key <- highlight_key(clustered_data_table,~Cluster)

  # create highlight key, which highlights by cluster
  pie_plot <- plot_ly(key)%>%
    group_by(Cluster)%>%
    add_lines(x = ~Time , y = ~Prob,color = ~Cluster, colors = myColor[2:numclust + 1] ,)%>%
    add_segments(x = ~Time, xend = ~Time, y = 0, yend = 1, frame = ~Time,showlegend = FALSE, color = myColor[1],showlegend = FALSE)

  # modify the x-axis
  pie_plot <- pie_plot%>%layout(
    xaxis = list(
      type = "date",
      tickformat = "%d%M%Y",showticklabels = FALSE,showgrid = FALSE),
    yaxis = list(showgrid = FALSE,title = c("Cluster Probabilities"),titlefont = list(size = 8)),
    showlegend = FALSE,
    title = ""
  )
  pie_plot%>%highlight(
    on = "plotly_click",
    off = "plotly_doubleclick",
    selectize = FALSE,
    dynamic = FALSE,
    persistent = FALSE,
    showlegend = F
  )

  # 3 scatterplots
  p1 <- plot_ly(as.data.frame(table_list[1]),
                x = ~diam_mid,
                y = ~chl_small,
                size = ~ counts_1,
                type = 'scatter',
                frame = ~Time,
                mode = 'markers',
                sizes = c(0,1000),
                opacity = 0.5,
                alpha = 0.3,
                showlegend = FALSE,
                marker = list(
                  color = "blue"
                )
  ) %>%
    add_trace(data = key,
              x = ~diam_mid_pie,
              y = ~chl_small_pie,
              text = ~Cluster,
              textposition = 'middle right',
              frame = ~Time,
              showlegend = FALSE,
              size = ~Prob*200,
              marker = list(
                color = "red"
              ),
              hoverinfo = 'text',
              hovertext = ~paste("Cluster", Cluster),
              mode = 'markers',
              textfont = list(size = 14),
              opacity = 0.9,
              alpha = 1
    )%>% layout(yaxis = list(showgrid = FALSE,title = c("chl_small"),titlefont = list(size = 8)), xaxis = list(showgrid = FALSE,title = c("diam_mid"),titlefont = list(size = 8)))
  p2 <- plot_ly(as.data.frame(table_list[2]),
                y = ~pe,
                x = ~chl_small,
                size = ~ counts_2,
                type = 'scatter',
                mode = 'markers',
                frame = ~Time,
                alpha = 0.3,
                opacity=0.5,
                sizes = c(0,1000),
                showlegend = FALSE,
                marker = list(
                  color = "blue"
                )
  )%>%
    add_trace(data = key,
              y = ~pe_pie,
              x = ~chl_small_pie,
              text = ~Cluster,
              textposition = 'middle right',
              frame = ~Time,
              showlegend = FALSE,
              size = ~Prob*200,
              hoverinfo = 'text',
              hovertext = ~paste("Cluster",Cluster),
              mode = 'markers',
              textfont = list(size = 14),
              opacity = 0.9,
              alpha = 1,
              marker = list(
                color = "red"
              )
    )%>%layout(yaxis = list(showgrid = FALSE,title = c("pe"),titlefont = list(size = 8)), xaxis = list(showgrid = FALSE,title = c("chl_small"),titlefont = list(size = 8)))

  p3<-plot_ly(as.data.frame(table_list[3]),
              y= ~diam_mid,
              x = ~pe,
              size = ~ counts_3,
              frame = ~Time,
              type = 'scatter',
              mode = 'markers',
              opacity=0.5,
              sizes = c(0,1000),
              alpha = 0.3,
              showlegend = FALSE,
              marker = list(
                color = "blue"
              )
  )%>%
    add_trace(data = key,
              y = ~diam_mid_pie,
              x = ~pe_pie,
              text = ~Cluster,
              textposition = 'middle right',
              frame = ~Time,
              size = ~Prob*200,
              hoverinfo = 'text',
              hovertext = ~paste("Cluster",Cluster),
              mode = 'markers',
              textfont = list(size = 14),
              opacity = 0.9,
              alpha = 1,
              marker = list(color = "red")
    )%>%layout(yaxis = list(showgrid = FALSE,title = c("diam_mid"),titlefont = list(size = 8)), xaxis = list(showgrid = FALSE,title = c("pe"),titlefont = list(size = 8)))
  scatterplot <- subplot(p1,p2,p3,titleX = TRUE,titleY = TRUE)%>%animation_opts(
    frame = 5,transition = 0, easing = "elastic", redraw = FALSE,mode = "immediate"
  )


  plot <- subplot(scatterplot,
                  covariates_plot,
                  prob_plot,
                  nrows = 3,
                  margin = 0.05,
                  shareX = FALSE,
                  titleY = TRUE,
                  titleX = TRUE)%>%animation_opts(
    frame = 5,transition = 0, easing = "elastic", redraw = FALSE,mode = "immediate"
  )
  return(plot)

}

reorder_clust <- function(res){
  
  ## Create an order
  
  ## Here's a suggestion for the 1d plots... since the coloring (and
  ## likewise numbering) of the clusters is arbitrary, what if we come up
  ## with some standard rule for labeling/coloring.  For example, it could
  ## simply be from largest diameter (averaged across all time) to smallest
  ## diameter.  Another natural choice for the ordering would be to order
  ## them from largest to smallest pi values (again averaged over all time).
  ## This way, your 1d-all-models.pdf will have (for the most part)
  ## consistent coloring/labeling in all the tiny plots.  Actually, the
  ## "largest pi" approach would work for the 3d plots as well.
  
  ## ord = res$prob %>% colSums %>% order(decreasing=TRUE)
  ## ord = res$mn %>% colSums %>% order(decreasing=TRUE)
  ord = res$mn[,1,] %>% colSums() %>% order(decreasing=TRUE)
  
  ## Reorder mean
  res$mn = res$mn[,,ord, drop=FALSE]
  
  ## Reorder sigma
  res$sigma = res$sigma[ord,,,drop=FALSE]
  
  ## Reorder prob
  res$prob = res$prob[,ord, drop=FALSE]
  
  ## Reorder the alpha coefficients
  res$alpha = res$alpha[ord,, drop=FALSE] ## Also rename the row names
  rownames(res$alpha) = paste0("clust-", 1:res$numclust)
  
  ## Reorder the beta coefficients
  res$beta = res$beta[ord]
  names(res$beta) = paste0("clust-", 1:res$numclust)
  
  return(res)
}

## ## 1. Loading probabilities from res$pie
## ##' @param res: A list containing the resulting data from model
## ##' @param numclust: An integer indicating number of clusters in model
## ##' @return pie_table : data.frame with TT rows and 3 columns
## form_pie_table <- function(res){
##   numclust = res$numclust
##   pie_table <- data.frame(res$pie)
##   pie_table <- setDT(pie_table, keep.rownames = "Time")[]
##   colnames(pie_table) <- c("Time", paste0("cluster_", 1:numclust))
##   ## Reshape it to 3 columns as described above
##   pie_table <- pie_table%>%
##     gather(var,val,-c(1))%>%
##     separate(var,c("type","cluster"))%>%
##     .[-2]
##   colnames(pie_table) <- c("Time", "Cluster","Prob")
##   return(pie_table)
## }




##' Reshape cluster probability table to long format.
##'
##' @param res Flowmix object.
##'
##' @return Long format matrix.
make_prob_table <- function(res){
  prob = res$prob
  numclust = ncol(prob)
  prob_table <- data.frame(prob)
  prob_table <- setDT(prob_table, keep.rownames = "Time")[]
  colnames(prob_table) <- c("Time", paste0("cluster_", 1:numclust))
  prob_table <- prob_table %>%
    gather(var, val, -c(1)) %>%
    separate(var,c("type", "cluster")) %>%
    .[-2] %>%
    setNames(c("Time", "Cluster", "Prob")) %>%
    as_tibble()
}



##' Makes plotly object from a long format table of cluster probabilities,
##' created using \code{make_prob_table()}.
##'
##' @param prob_table Prob table.
##'
##' @return A plotly object
make_prob_plot <- function(prob_table){
  numclust = prob_table %>% ncol()
  myColor <- randomcoloR::distinctColorPalette(k = numclust + 1)
  key = highlight_key(prob_table, ~Cluster)
  prob_plot <- plot_ly(key)%>%
    group_by(Cluster)%>%
    add_lines(x = ~Time , y = ~Prob,color = ~Cluster, colors = myColor[2:numclust + 1] ,)%>%
    add_segments(x = ~Time, xend = ~Time, y = 0, yend = 1, frame = ~Time,
                 showlegend = FALSE, color = myColor[1],showlegend = FALSE) %>%
    layout(xaxis = list(type = "date", tickformat = "%d%M%Y", showticklabels = FALSE, showgrid = FALSE),
           yaxis = list(showgrid = FALSE,title = c("Cluster Probabilities"),titlefont = list(size = 8)),
           showlegend = FALSE, title = "") %>%
    highlight(on = "plotly_click",
              off = "plotly_doubleclick",
              selectize = FALSE,
              dynamic = FALSE,
              persistent = FALSE,
              showlegend = F)
}




##' Make n x p covariates table into long format.
##'
##' @param res: A list containing the resulting data from model
##'
##' @return Data frame with np rows, and three columns -- time, covariate name
##'   and value. The three columns are named "Time", "Trace", and "Value".
make_covariates_table <- function(res){
  covariates = res$X
  covariates = as.data.frame(covariates)
  covariates = setDT(covariates, keep.rownames = "Time")[]
  covariates <- covariates %>%
    gather(var,val,-c(1))
  colnames(covariates) = c("Time","Trace","Value")
  # Removed for now
 # covariates$Time = lubridate::as_date(covariates$Time)
  return(covariates)
}





##' From the output of \code{covariates_table()}, create a plotly object.
##'
##' @param covariates Long format of covariates table.
##'
##' @return A plotly object.
make_covariates_plot <- function(covariates){
  covariates %>%
    plot_ly() %>%
    group_by(Trace) %>%
    add_lines(x = ~Time , y = ~Value,color = "grey50", opacity = 0.1) %>%
    add_segments(x = ~Time, xend = ~Time, y = -6, yend = 6, frame = ~Time,
                 showlegend = FALSE, color = myColor[1], showlegend = FALSE) %>%
    layout(xaxis = list(
               type = "date",
               tickformat = "%d%M%Y",showticklabels = FALSE,showgrid = FALSE),
           yaxis = list(showgrid = FALSE, title = c("EnvironmenTal Covariates"),titlefont = list(size = 8)),
           showlegend = FALSE)
}



##' SH: fill this in.
##'
##' @param res Flowmix object.
##'
##' @return Data frame with TT rows and 3 columns. SH: this is inaccurate.
make_mn_table <- function(res){
  res = reorder_clust(res)
  numclust = res$numclust
  TT = length(ylist)
  Time <- names(ylist)
  mn = res$mn
  dfs = lapply(1:numclust, function(iclust){
    one_table = tibble(diam_mid_pie = mn[,1,iclust], ##SH: why is this called pie???
                       chl_small_pie = mn[,2,iclust],##SH: why is this called pie???
                       pe_pie = mn[,3,iclust],##SH: why is this called pie???
                       Time = Time, Cluster = rep(iclust, TT))
    colnames(one_table)[1:3] <- c("diam_mid_pie","chl_small_pie","pe_pie")
    one_table
  })
  mn_table <- bind_rows(dfs)
  mn_table$Cluster <- sapply(mn_table$Cluster, toString)
  mn_table <- mn_table %>% as_tibble()
  return(mn_table)
}





##' Reshaping ylist into a new list of 3 entries each containing table of data
##' with respect to 2 of the dimensions. This function specifically applies to
##' the case when number of dimensions = 3
##'
##' @param ylist: A list containing the resulting data from ylist in res
##' @param table_list: A list of table (dimension choose 2) data frames. The deafult ordering will be...(to be determined)\
##'
convert_ylist_2d <- function(ylist){

  ## Setup
  alltimes = names(ylist)
  TT = length(ylist)
  tablist = list()
  dimslist = list(c(1,2), c(2,3), c(3,1))

  ## Obtain all the tables once.
  for(ii in 1:length(dimslist)){

    dims = dimslist[[ii]]

    ## Form the table
    all_2d_tables = lapply(1:TT, function(tt){
      y = ylist[[tt]][,dims]
      counts = countslist[[tt]]
      collapse_3d_to_2d(y, counts, dims=1:2)
    })
    combined_2d_table = do.call(rbind, all_2d_tables) %>% as_tibble()

    ## Add time
    reptimes = sapply(all_2d_tables, nrow)
    times = rep(alltimes, reptimes)
    combined_2d_table[,"Time"] = times
    newname = paste0("counts_", ii)
    combined_2d_table <- rename(combined_2d_table, !!newname:=counts)
    tablist[[ii]] = combined_2d_table
  }
  diam_chl_table <- tablist[[1]]
  pe_chl_table <- tablist[[2]]
  diam_pe_table <- tablist[[3]]
  return(list(diam_chl_table,pe_chl_table,diam_pe_table))
}
