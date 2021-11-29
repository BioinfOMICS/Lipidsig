DE_sub_char_plot_2 <- function(DE_split_char_table_all, DE_split_char_index, group_info,
                               char_var='Category', split_var, split_class,
                               insert_ref_group=NULL, ref_group=NULL){
  
  require(plotly)
  require(data.table)
  
  #### Plot ####
  
  CTRL.RES <- DE_split_char_table_all %>% 
    dplyr::select(1:2, sig, mean_ctrl, sd_ctrl) %>% 
    mutate(Group = 'Ctrl')
  colnames(CTRL.RES) <- c('Split_category', 'Category', 'Significant', 'Mean', 'SD', 'Group')
  
  EXP.RES <- DE_split_char_table_all %>% 
    dplyr::select(1:2, sig, mean_exp, sd_exp) %>% 
    mutate(Group = 'Exp')
  colnames(EXP.RES) <- c('Split_category', 'Category', 'Significant', 'Mean', 'SD', 'Group')
  
  
  ## Fig.1 bar chart
  barTab <- rbindlist(l = list(CTRL.RES, EXP.RES), use.names = T, fill = T)
  if(sum(is.na(suppressWarnings(as.numeric(barTab$Category))))==0){
    barTab$Category <- as.factor(as.numeric(barTab$Category))
  }
  
  splitTab <- barTab %>% filter(Split_category == split_class)
  #splitTab$Category <- factor(x = splitTab$Category, levels = sort(unique(as.character(splitTab$Category))))
  splitTab <- splitTab %>% group_by(Category) %>% mutate(max_error_bar = max(Mean+SD)) %>% ungroup()
  splitTab$post_hoc_pvalue = NA
  for(i in 1:nrow(splitTab)){
    post_hoc_pvalue_data <- DE_split_char_table_all[which(DE_split_char_table_all[,1]==split_class),]
    splitTab$post_hoc_pvalue[i] <- post_hoc_pvalue_data$post_hoc_pvalue[which(splitTab$Category[i]==post_hoc_pvalue_data[,2])]
  }
  if(!is.null(insert_ref_group) & !is.null(ref_group)){
    exp_raw_name <- ref_group[-which(insert_ref_group==ref_group)]
    splitTab$Group[which(splitTab$Group=='Ctrl')] <-  insert_ref_group
    splitTab$Group[which(splitTab$Group=='Exp')] <-  exp_raw_name
    splitTab$Group <- factor(splitTab$Group, levels = c(insert_ref_group, exp_raw_name))
    
  }
  splitTab_sig <- splitTab %>% filter(Significant=='yes') %>%mutate(pvalue_text = ifelse(post_hoc_pvalue<=0.001 ,"***",
                                                                                         ifelse(post_hoc_pvalue<=0.01 ,"**",
                                                                                                ifelse(post_hoc_pvalue<=0.05 ,"*",""))))
  
  barPlot <- ggplot(data=splitTab, aes(x=Category, y=Mean,fill=Group)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values=c('lightslateblue','sienna2')) +
    geom_errorbar(aes(ymin=Mean, ymax=Mean+SD),color="gray39", width=.9,position=position_dodge()) +
    geom_text(data=splitTab_sig, aes(x=Category, y=max_error_bar+5 , label = pvalue_text),color="red") +
    theme_minimal() + 
    labs(x = char_var)
  barggplotly <- ggplotly(barPlot)
  for (i in 1:length(unique(barPlot$data$Group))) {
    n <- length(unique(barPlot$data$Group))
    data <- barPlot$data[which(barPlot$data$Group==unique(barPlot$data$Group)[i]),]
    barggplotly$x$data[[i]]$text <- paste0("Category :",data$Category,
                                           "\nMean :",round(data$Mean,3),
                                           "\nSD :",round(data$SD,3),
                                           "\nGroup :",data$Group)
    barggplotly$x$data[[i+n]]$text <- paste0("Category :",data$Category,
                                             "\nMean :",round(data$Mean,3),
                                             "\nSD :",round(data$SD,3),
                                             "\nGroup :",data$Group)
  }
  for (i in 1:length(barggplotly$x$data)) {
    text = str_split(barggplotly$x$data[[i]]$hovertext,"<br />max_error_bar")
    hovertext =list()
    if(length(text)>0){
      for (j in 1:length(text)) {
        hovertext[[j]] <- paste(text[[j]][1],"Significant : YES")
      }
      barggplotly$x$data[[i]]$hovertext <-hovertext
    }
  }
  barPlot <- barggplotly
  
  ## Fig.1-1 bar chart sqrt scale
  
  barPlot_sqrt <- ggplot(data=splitTab, aes(x=Category, y=Mean,fill=Group)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values=c('lightslateblue','sienna2')) +
    geom_errorbar(aes(ymin=Mean, ymax=Mean+SD),color="gray39", width=.9,position=position_dodge()) +
    geom_text(data=splitTab_sig, aes(x=Category, y=max_error_bar+5 , label = pvalue_text),color="red") +
    scale_y_sqrt() +
    theme_minimal() + 
    labs(x = char_var)
  bar_ggplotly_sqrt <- ggplotly(barPlot_sqrt)
  for (i in 1:length(unique(barPlot_sqrt$data$Group))) {
    n <- length(unique(barPlot_sqrt$data$Group))
    data <- barPlot_sqrt$data[which(barPlot_sqrt$data$Group==unique(barPlot_sqrt$data$Group)[i]),]
    bar_ggplotly_sqrt$x$data[[i]]$text <- paste0("Category :",data$Category,
                                                 "\nMean :",round(data$Mean,3),
                                                 "\nSD :",round(data$SD,3),
                                                 "\nGroup :",data$Group)
    bar_ggplotly_sqrt$x$data[[i+n]]$text <- paste0("Category :",data$Category,
                                                   "\nMean :",round(data$Mean,3),
                                                   "\nSD :",round(data$SD,3),
                                                   "\nGroup :",data$Group)
  }
  for (i in 1:length(bar_ggplotly_sqrt$x$data)) {
    text = str_split(bar_ggplotly_sqrt$x$data[[i]]$hovertext,"<br />max_error_bar")
    hovertext =list()
    if(length(text)>0){
      for (j in 1:length(text)) {
        hovertext[[j]] <- paste(text[[j]][1],"Significant : YES")
      }
      bar_ggplotly_sqrt$x$data[[i]]$hovertext <-hovertext
    }
  }
  barPlot_sqrt <- bar_ggplotly_sqrt
  
  ## Fig.3 trend plot
  linePlot <- ggplot(data=splitTab, aes(x=Category, y=Mean,group=Group,color=Group)) +
    geom_line(stat="identity",position=position_dodge(0.05))+
    #geom_point()+
    scale_color_manual(values=c('lightslateblue','sienna2')) +
    geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD),color="gray39",
                  position=position_dodge(0.05)) +
    geom_text(data=splitTab_sig,aes(x=Category, y=max_error_bar+5, label = pvalue_text),color="red") +
    theme_minimal() + 
    labs(x = char_var)
  lineggplotly <- ggplotly(linePlot)
  for(i in 1:length(lineggplotly$x$data)){
    if(!is.null(lineggplotly$x$data[i]$name)){
      lineggplotly$x$data[i]$name = gsub("\\(","",str_split(lineggplotly$x$data[i]$name,",")[[1]][1])
    }
  }
  for (i in 1:length(unique(linePlot$data$Group))) {
    n <- length(unique(linePlot$data$Group))
    data <- linePlot$data[which(linePlot$data$Group==unique(linePlot$data$Group)[i]),]
    lineggplotly$x$data[[i]]$text <- paste0("Category :",data$Category,
                                            "\nMean :",round(data$Mean,3),
                                            "\nSD :",round(data$SD,3),
                                            "\nGroup :",data$Group)
    if(sum(grepl("\\*",lineggplotly$x$data[[i+n]]$text))==0){
      lineggplotly$x$data[[i+n]]$text <- paste0("Category :",data$Category,
                                                "\nMean :",round(data$Mean,3),
                                                "\nSD :",round(data$SD,3),
                                                "\nGroup :",data$Group)
    }
  }
  for (i in 1:length(lineggplotly$x$data)) {
    text = str_split(lineggplotly$x$data[[i]]$hovertext,"<br />max_error_bar")
    hovertext =list()
    if(length(text)>0){
      for (j in 1:length(text)) {
        hovertext[[j]] <- paste(text[[j]][1],"Significant : YES")
      }
      lineggplotly$x$data[[i]]$hovertext <-hovertext
    }
  }
  linePlot <- lineggplotly
  
  ## Fig.3_1 trend plot sqrt scale
  linePlot_sqrt <- ggplot(data=splitTab, aes(x=Category, y=Mean,group=Group,color=Group)) +
    geom_line(stat="identity",position=position_dodge(0.05))+
    #geom_point()+
    scale_color_manual(values=c('lightslateblue','sienna2')) +
    geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD),color="gray39",
                  position=position_dodge(0.05)) +
    geom_text(data=splitTab_sig,aes(x=Category, y=max_error_bar+5, label = pvalue_text),color="red") +
    scale_y_sqrt() +
    theme_minimal() + 
    labs(x = char_var)
  line_ggplotly_sqrt <- ggplotly(linePlot_sqrt)
  for(i in 1:length(line_ggplotly_sqrt$x$data)){
    if(!is.null(line_ggplotly_sqrt$x$data[i]$name)){
      line_ggplotly_sqrt$x$data[i]$name = gsub("\\(","",str_split(line_ggplotly_sqrt$x$data[i]$name,",")[[1]][1])
    }
  }
  for (i in 1:length(unique(linePlot_sqrt$data$Group))) {
    n <- length(unique(linePlot_sqrt$data$Group))
    data <- linePlot_sqrt$data[which(linePlot_sqrt$data$Group==unique(linePlot_sqrt$data$Group)[i]),]
    line_ggplotly_sqrt$x$data[[i]]$text <- paste0("Category :",data$Category,
                                                  "\nMean :",round(data$Mean,3),
                                                  "\nSD :",round(data$SD,3),
                                                  "\nGroup :",data$Group)
    if(sum(grepl("\\*",line_ggplotly_sqrt$x$data[[i+n]]$text))==0){
      line_ggplotly_sqrt$x$data[[i+n]]$text <- paste0("Category :",data$Category,
                                                      "\nMean :",round(data$Mean,3),
                                                      "\nSD :",round(data$SD,3),
                                                      "\nGroup :",data$Group)
    }
  }
  for (i in 1:length(line_ggplotly_sqrt$x$data)) {
    text = str_split(line_ggplotly_sqrt$x$data[[i]]$hovertext,"<br />max_error_bar")
    hovertext =list()
    if(length(text)>0){
      for (j in 1:length(text)) {
        hovertext[[j]] <- paste(text[[j]][1],"Significant : YES")
      }
      line_ggplotly_sqrt$x$data[[i]]$hovertext <-hovertext
    }
  }
  linePlot_sqrt <- line_ggplotly_sqrt
  
  ## Fig.2 box plot
  colnames(DE_split_char_index)[1] <- 'Split_category'
  
  boxTab <- DE_split_char_index %>% 
    filter(Split_category == split_class) %>%
    dplyr::select(-Split_category) %>%
    column_to_rownames(var = char_var) %>% 
    t() %>% as.data.frame() %>%
    merge(group_info, by.x = 0, by.y = 'sample_name')
  colnames(boxTab)[2] <- 'Category'
  if(!is.null(insert_ref_group) & !is.null(ref_group)){
    exp_raw_name <- ref_group[-which(insert_ref_group==ref_group)]
    boxTab$group[which(boxTab$group=='ctrl')] <-  insert_ref_group
    boxTab$group[which(boxTab$group=='exp')] <-  exp_raw_name
  }
  t.test.pvalue <- tryCatch({t.test(Category ~ group, data = boxTab, var.equal = T)["p.value"]},
                            warning = function(w) {NA},error = function(e){NA})
  if(!is.na(t.test.pvalue)){
  # t.test.pvalue <- tryCatch({t.test(Category ~ group, data = boxTab)["p.value"]},
  #                           warning = function(w) {NULL},error = function(e){NULL})
  # if(!is.null(t.test.pvalue)){
    if(t.test.pvalue<=0.05){
      group_name <- c(unique(boxTab$group)[1],paste0(unique(boxTab$group)[1],"0"),unique(boxTab$group)[2])
      group_max <- max(boxTab$Category) %>%unique()
      group_min <- min(boxTab$Category) %>%unique()
      if(t.test.pvalue<=0.05 &t.test.pvalue>0.01){
        t.text <- c('', '*','')
      }else if (t.test.pvalue<=0.01 & t.test.pvalue>0.001){
        t.text <- c('', '**','')
      }else{
        t.text <- c('', '***','')
      }
      boxTab_1 <- boxTab %>%
        filter(group==unique(boxTab$group)[1])
      boxTab_2 <- boxTab %>%
        filter(group==unique(boxTab$group)[2])
      boxPlot <- plot_ly() %>%
        add_bars(x = group_name,
                 y = rep(group_max+0.15,3),
                 opacity=1,
                 showlegend = F,
                 marker=list(line = list(color='rgba(0,0,0,0'),
                             color = 'rgba(0,0,0,0'),
                 textfont = list(color = 'red'),
                 text =  t.text ,
                 hoverinfo = 'none',
                 textposition = 'outside',
                 legendgroup = "1") %>%
        add_lines(x = c(rep(paste(unique(boxTab$group)[1]),2),rep(paste(unique(boxTab$group)[2]),2)),
                  y = c(group_max+0.1,group_max+0.15,group_max+0.15,group_max+0.1),
                  showlegend = F,
                  line = list(color = 'black'),
                  legendgroup = "1",
                  hoverinfo = 'none') %>%
        add_boxplot(data = boxTab_1,x = ~group, y = ~Category, 
                    color = I('lightslateblue'),
                    name = unique(boxTab$group)[1],
                    boxpoints = "all", jitter = 0.85,  pointpos = 0,
                    marker = list(size = 5, opacity = 0.8)) %>%
        add_boxplot(data = boxTab_2,x = ~group, y = ~Category, 
                    color = I('sienna2'),
                    name = unique(boxTab$group)[2],
                    boxpoints = "all", jitter = 0.85,  pointpos = 0,
                    marker = list(size = 5, opacity = 0.8)) %>%
        layout(title = split_class,
               xaxis = list(title = 'Group',
                            tickmode = 'array',
                            tickvals =  c(unique(boxTab$group)[1], '', unique(boxTab$group)[2]),
                            ticktext = c(unique(boxTab$group)[1], '', unique(boxTab$group)[2]),
                            titlefont = list(size = 16), 
                            tickfont = list(size = 14)),
               yaxis = list(title = paste0(char_var, ' index'), 
                            titlefont = list(size = 16), 
                            tickfont = list(size = 14),
                            range = c( group_min, group_max+0.5)
                            #type = "log", 
                            #exponentformat = 'e'
               ), 
               legend = list(font = list(size = 14), 
                             y = 0.5),
               margin = list(l=70, r=70, b=80, t = 60))
    }else{
      boxPlot <- plot_ly(data = boxTab, 
                         x = ~group, 
                         y = ~Category, 
                         type = 'box', 
                         color = ~group,
                         colors = c('lightslateblue','sienna2'),
                         boxpoints = 'all', 
                         jitter = 0.85, 
                         pointpos = 0,
                         marker = list(size = 5, opacity = 0.8)) %>% 
        layout(title = split_class,
               #titlefont = list(size = 20),
               xaxis = list(title = 'Group', 
                            titlefont = list(size = 16), 
                            tickfont = list(size = 14)),
               yaxis = list(title = paste0(char_var, ' index'), 
                            titlefont = list(size = 16), 
                            tickfont = list(size = 14)#, 
                            #type = "log", 
                            #exponentformat = 'e'
               ), 
               legend = list(font = list(size = 14), 
                             y = 0.5),
               margin = list(l=70, r=70, b=80, t = 60)) 
    }
  }else{
    boxPlot <- plot_ly(data = boxTab, 
                       x = ~group, 
                       y = ~Category, 
                       type = 'box', 
                       color = ~group,
                       colors = c('lightslateblue','sienna2'),
                       boxpoints = 'all', 
                       jitter = 0.85, 
                       pointpos = 0,
                       marker = list(size = 5, opacity = 0.8)) %>% 
      layout(title = split_class,
             #titlefont = list(size = 20),
             xaxis = list(title = 'Group', 
                          titlefont = list(size = 16), 
                          tickfont = list(size = 14)),
             yaxis = list(title = paste0(char_var, ' index'), 
                          titlefont = list(size = 16), 
                          tickfont = list(size = 14)#, 
                          #type = "log", 
                          #exponentformat = 'e'
             ), 
             legend = list(font = list(size = 14), 
                           y = 0.5),
             margin = list(l=70, r=70, b=80, t = 60)) 
  }
  
  return(list(barPlot, linePlot, boxPlot,barPlot_sqrt, linePlot_sqrt))
  
} #function