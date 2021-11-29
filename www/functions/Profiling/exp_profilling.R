

exp_profilling <- function(exp_data){
  ################################################
  ####                                        ####
  #### PLOT: total expressed lipid by samples ####
  ####                                        ####
  ################################################
  require(tidyr)
  require(plotly)
  require(dplyr)
  ## Count total expressed lipids ##
  num.lip <- exp_data %>%
    gather(sample_name,value,-feature) %>%
    mutate(is.expr = !is.na(value)) %>% 
    group_by(sample_name) %>% 
    summarise(expr.count = sum(is.expr)) %>% 
    arrange(sample_name)
  num.lip$sample_name <- factor(num.lip$sample_name,levels = unique(num.lip$sample_name)[order(num.lip$expr.count, decreasing = TRUE)])
  
  ## plot barplot ##
  i.expr.lip <- plot_ly(num.lip,x=~sample_name,y=~expr.count,
                        type="bar",color = ~sample_name,hoverinfo="text",
                        text=~paste("Sample name :",sample_name,"\nCount :",expr.count)) %>%
    layout(legend = list(title=list(text="Sample"),
                         font = list(family = 'arial')), 
           xaxis = list(#title="Sample",
                        titlefont = list(family = 'arial'),
                        tickfont = list(family = 'arial'), 
                        automargin = T, 
                        showgrid = FALSE), 
           yaxis = list(title="Number of Expressed Lipids",
                        titlefont = list(family = 'arial'), 
                        tickfont = list(family = 'arial'), 
                        automargin = T, 
                        showgrid = FALSE))
  #############################################
  ####                                     ####
  #### PLOT: total lipid amount of samples ####
  ####                                     ####
  #############################################
  tot.lip <- exp_data %>%
    gather(sample_name,value,-feature) %>% 
    group_by(sample_name) %>%
    summarise(lipid_amount=sum(value, na.rm=T)) 
  tot.lip$sample_name <- factor(tot.lip$sample_name,levels = unique(tot.lip$sample_name)[order(tot.lip$lipid_amount, decreasing = TRUE)])
  
  ## plot barplot ##
  i.p.amount <- plot_ly(tot.lip,x=~sample_name,y=~lipid_amount,
                        type="bar",color = ~sample_name,hoverinfo="text",
                        text=~paste("Sample name :",sample_name,"\nAmount :",round(lipid_amount,3))) %>%
    layout(legend = list(title=list(text="Sample"),
                         font = list(family = 'arial')), 
           xaxis = list(#title="Sample",
                        titlefont = list(family = 'arial'),
                        tickfont = list(family = 'arial'), 
                        automargin = T, 
                        showgrid = FALSE), 
           yaxis = list(title="Lipid Amount",
                        titlefont = list(family = 'arial'), 
                        tickfont = list(family = 'arial'), 
                        automargin = T, 
                        showgrid = FALSE)) 
  
  #################################################
  ####                                         ####
  #### PLOT: hist of quantification per sample ####
  ####                                         ####
  #################################################
  p.hist.data <- exp_data %>%
    gather(sample_name,value,-feature)
  p.hist.data <- na.omit(p.hist.data)
  length_sample <- length(unique(p.hist.data$sample_name))
  for (i in 1:length_sample) {
    if(i==1){
      density_sample <- p.hist.data[which(p.hist.data$sample_name==unique(p.hist.data$sample_name)[i]),]
      density_data_x <- data.frame(density(log10(density_sample$value))$x)
      density_data_y <- data.frame(density(log10(density_sample$value))$y)
    }else{
      density_sample <- p.hist.data[which(p.hist.data$sample_name==unique(p.hist.data$sample_name)[i]),]
      density_data_x <- data.frame(density_data_x,density(log10(density_sample$value))$x)
      density_data_y <- data.frame(density_data_y,density(log10(density_sample$value))$y)
    }
  }
  colnames(density_data_x) <- paste0("x",1:length_sample)
  colnames(density_data_y) <- paste0("y",1:length_sample)
  density_data <- cbind(density_data_x,density_data_y)
  ## plot densityplot ##
  for (i in 1:length_sample) {
    if(i==1){
      p.hist.value <- plot_ly(data = density_data,
                              x=~x1,
                              y=~y1,
                              type = 'scatter',
                              mode = 'lines',
                              name = unique(p.hist.data$sample_name)[1],
                              hoverinfo="text",
                              text=~paste("Sample name :",
                                          unique(p.hist.data$sample_name)[1],
                                          "\nlog10(expression) :",
                                          round(x1,3),
                                          "\nDensity :",
                                          round(y1,3)))  
    }else{
      text = paste0("p.hist.value %>%add_trace(x = ~x",
                    i,
                    ",",
                    "y = ~y",i,
                    ",name = unique(p.hist.data$sample_name)[",i,
                    "],text=~paste(\"Sample name :\",unique(p.hist.data$sample_name)[",i,
                    "],\"\nlog10(expression) :\",round(x",i,
                    ",3),\"\nDensity :\",round(y",i,",3)))")
      p.hist.value <- eval(parse(text=text))
    }
  }
  p.hist.value <- p.hist.value %>%
    layout(legend=list(title=list(text="Sample")),
           xaxis=list(title = "log10(expression)",zeroline= FALSE,showgrid = FALSE),
           yaxis=list(title = "Density",showgrid = FALSE))
  
  return(list(i.expr.lip = i.expr.lip,
              i.p.amount = i.p.amount,
              p.hist.value = p.hist.value))
}


