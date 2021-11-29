DE_char_2 <- function(exp_data,  data_transform=T, #lipid_char_table, 
                      group_info, paired=F, sig_pvalue = 0.05, sig_FC = 2,
                      insert_ref_group=NULL, ref_group=NULL, char_var='Category'){
  require(tidyverse)
  require(stringr)
  require(data.table)
  require(plotly)
  #source('/media/md1200/analysis/Script/lipid_web/Species2Char.R')
  
  #char_exp_data <- Species2Char(exp_data, lipid_char_table, char_var)
  char_exp_data <- exp_data
  
  var <- char_exp_data[[1]]
  char_name <- colnames(char_exp_data)[1]
  
  if(data_transform==T){
    Char_Transform_table <- cbind(char_exp_data[1],log10(char_exp_data[-1]))
  }else{
    Char_Transform_table <- char_exp_data
  }
  
  colnames(Char_Transform_table)[1] <- 'feature'
  
  #two-way ANOVA
  if(length(unique(Char_Transform_table[[1]]))>1){
    two_way_anova <- Char_Transform_table %>% gather(-feature, key='sample_name', value='value') %>% 
      left_join(group_info[c(1,3)], by='sample_name') %>% 
      mutate(feature=as.factor(feature))
    two_way_anova <- tryCatch({aov(value ~ group+feature+group:feature, data=two_way_anova)},error = function(e){NULL})
    if(is.null(two_way_anova)){
      anova_pvalue <- NA
    }else{
      if(nrow(summary(two_way_anova)[[1]])!=4){
        anova_pvalue <- NA
      }else{
        anova_pvalue <- summary(two_way_anova)[[1]][[3,"Pr(>F)"]]
      }
    }
  }else{
    anova_pvalue <- NA
  }
  
  
  group1 <- group_info %>% filter(group=='ctrl') %>% 
    arrange(pair) %>% .$sample_name
  group2 <- group_info %>% filter(group=='exp') %>% 
    arrange(pair) %>% .$sample_name
  
  var_name <- character(length(var))
  mean_ctrl <- numeric(length(var))
  sd_ctrl <- numeric(length(var))
  mean_exp <- numeric(length(var))
  sd_exp <- numeric(length(var))
  
  FC <- numeric(length(var))
  pvalue <- numeric(length(var))
  
  
  #t.test
  for(a in 1:length(var)){
    
    var_name[a] <- var[a]
    mean_ctrl[a] <- mean(unlist(char_exp_data[a,group1]), na.rm=T)
    mean_exp[a] <- mean(unlist(char_exp_data[a,group2]), na.rm=T)
    
    sd_ctrl[a] <- sd(unlist(char_exp_data[a,group1]), na.rm=T)
    sd_exp[a] <- sd(unlist(char_exp_data[a,group2]), na.rm=T)
    
    FC[a] <- mean(unlist(char_exp_data[a,group2]), na.rm=T)/
      mean(unlist(char_exp_data[a,group1]), na.rm=T)
    
    if(paired==T){
      pvalue[a] <- tryCatch(
        t.test(unlist(Char_Transform_table[a,group1]), unlist(Char_Transform_table[a,group2]), paired = T, var.equal = T)$p.value,
        error=function(e){NA}
      )
      
    }else{
      pvalue[a] <- tryCatch(
        t.test(unlist(Char_Transform_table[a,group1]), unlist(Char_Transform_table[a,group2]), paired = F, var.equal = T)$p.value,
        error=function(e){NA}
      )
    }
    
  }
  Result_table <- data.frame(var_name=var_name, method='two-way anova', anova_pvalue=anova_pvalue,
                             post_hoc_test='t.test', mean_ctrl=mean_ctrl,
                             sd_ctrl=sd_ctrl, mean_exp=mean_exp,
                             sd_exp=sd_exp, FC=FC, log2FC=log2(FC),
                             post_hoc_pvalue=pvalue)
  Result_table <- Result_table %>% mutate(sig=ifelse(post_hoc_pvalue<sig_pvalue & abs(log2FC)>log2(sig_FC),'yes', 'no'))
  Result_table <- Result_table %>% mutate(sig=ifelse(is.na(sig), 'no',sig ))
  Result_table[is.na(Result_table)] <- NA
  colnames(Result_table)[1] <- char_name
  
  #Combined char into one
  if(sum(is.na(suppressWarnings(as.numeric(var))))==0){
    
    char_exp_data[[1]] <- as.numeric(char_exp_data[[1]])
    
    Combined_char_data <- map2(char_exp_data[1], char_exp_data[-1], ~sum(.x*.y, na.rm = T)/sum(.y, na.rm = T)) %>% 
      as.data.frame() %>% mutate(Combine_name=str_c(char_name, '_index')) %>% 
      dplyr::select(Combine_name, everything())
    colnames(Combined_char_data) <- colnames(char_exp_data)
    
    
    mean_ctrl <- mean(unlist(Combined_char_data[,group1]), na.rm=T)
    mean_exp <- mean(unlist(Combined_char_data[,group2]), na.rm=T)
    
    sd_ctrl <- sd(unlist(Combined_char_data[,group1]), na.rm=T)
    sd_exp <- sd(unlist(Combined_char_data[,group2]), na.rm=T)
    
    FC <- mean_exp/mean_ctrl
    
    if(data_transform==T){
      Combined_char_transform_table <- cbind(Combined_char_data[1],log10(Combined_char_data[-1]))
    }else{
      Combined_char_transform_table <- Combined_char_data
    }
    
    
    if(paired==T){
      pvalue <- tryCatch(
        t.test(unlist(Combined_char_transform_table[,group1]), unlist(Combined_char_transform_table[,group2]), paired = T, var.equal = T)$p.value,
        error=function(e){NA}
      )
      
    }else{
      pvalue <- tryCatch(
        t.test(unlist(Combined_char_transform_table[,group1]), unlist(Combined_char_transform_table[,group2]), paired = F, var.equal = T)$p.value,
        error=function(e){NA}
      )
    }
    
    Combine_char_result_table <- data.frame(var_name=Combined_char_data[1,1], 
                                            method='t.test', mean_ctrl=mean_ctrl,
                                            sd_ctrl=sd_ctrl, mean_exp=mean_exp,
                                            sd_exp=sd_exp,FC=FC, log2FC=log2(FC),
                                            p_value=pvalue)
    Combine_char_result_table <- Combine_char_result_table %>% mutate(sig=ifelse(p_value<sig_pvalue & abs(log2FC)>log2(sig_FC),'yes', 'no'))
    Combine_char_result_table <- Combine_char_result_table %>% mutate(sig=ifelse(is.na(sig), 'no',sig ))
    Combine_char_result_table[is.na(Combine_char_result_table)] <- NA
    
    colnames(Combine_char_result_table)[1] <- colnames(Combined_char_data)[1]
    
  }else{
    Combined_char_data <- data.frame()
    Combine_char_result_table <- data.frame()
    
  }
  
  #### Plot ####
  CHAR <- colnames(Result_table[1])
  
  CTRL.RES <- Result_table %>% 
    dplyr::select(1, sig, mean_ctrl, sd_ctrl) %>% 
    mutate(Group = 'Ctrl')
  colnames(CTRL.RES) <- c('Category', 'Significant', 'Mean', 'SD', 'Group')
  
  EXP.RES <- Result_table %>% 
    dplyr::select(1, sig, mean_exp, sd_exp) %>% 
    mutate(Group = 'Exp')
  colnames(EXP.RES) <- c('Category', 'Significant', 'Mean', 'SD', 'Group')
  
  
  ## Fig.1 bar chart
  barTab <- rbindlist(l = list(CTRL.RES, EXP.RES), use.names = T, fill = T)
  barTab <- barTab %>%  group_by(Category) %>% mutate(max_error_bar = max(Mean+SD)) %>% ungroup()  
  barTab$post_hoc_pvalue = NA
  for(i in 1:nrow(barTab)){
    barTab$post_hoc_pvalue[i] <- Result_table$post_hoc_pvalue[which(barTab$Category[i]==Result_table[,1])]
  }
  if(!is.null(insert_ref_group) & !is.null(ref_group)){
    exp_raw_name <- ref_group[-which(insert_ref_group==ref_group)]
    barTab$Group[which(barTab$Group=='Ctrl')] <-  insert_ref_group
    barTab$Group[which(barTab$Group=='Exp')] <-  exp_raw_name
    barTab$Group <- factor(barTab$Group, levels = c(insert_ref_group, exp_raw_name))
  }
  if(sum(is.na(suppressWarnings(as.numeric(barTab$Category))))==0){
    barTab$Category <- as.factor(as.numeric(barTab$Category))
  }
  barTab_sig <- barTab %>% filter(Significant=='yes') %>%mutate(pvalue_text = ifelse(post_hoc_pvalue<=0.001 ,"***",
                                                                                     ifelse(post_hoc_pvalue<=0.01 ,"**",
                                                                                            ifelse(post_hoc_pvalue<=0.05 ,"*",""))))
  
  barPlot <- ggplot(data=barTab, aes(x=Category, y=Mean,fill=Group)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values=c('lightslateblue','sienna2')) +
    geom_errorbar(aes(ymin=Mean, ymax=Mean+SD),color="gray39", width=.9,position=position_dodge()) +
    geom_text(data=barTab_sig,aes(x=Category, y=max_error_bar+5, label = pvalue_text),color="red") +
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
  
  barPlot_sqrt <- ggplot(data=barTab, aes(x=Category, y=Mean,fill=Group)) +
    geom_bar(stat="identity",position="dodge") +
    scale_fill_manual(values=c('lightslateblue','sienna2')) +
    geom_errorbar(aes(ymin=Mean, ymax=Mean+SD),color="gray39", width=.9,position=position_dodge()) +
    geom_text(data=barTab_sig,aes(x=Category, y=max_error_bar+5, label = pvalue_text),color="red") +
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
  
  if(nrow(Combined_char_data) > 0 & nrow(Combine_char_result_table) > 0){
    
    ## Fig.3 trend plot
    linePlot <- ggplot(data=barTab, aes(x=Category, y=Mean,group=Group,color=Group)) +
      geom_line(stat="identity",position=position_dodge(0.05))+
      #geom_point()+
      scale_color_manual(values=c('lightslateblue','sienna2')) +
      geom_errorbar(aes(ymin=Mean, ymax=Mean+SD),color="gray39",
                    position=position_dodge(0.05)) +
      geom_text(data=barTab_sig,aes(x=Category, y=max_error_bar+5, label = pvalue_text),color="red") +
      theme_minimal() + 
      labs(x = char_var)
    lineggplotly <- ggplotly(linePlot)
    
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
    linePlot_sqrt <- ggplot(data=barTab, aes(x=Category, y=Mean,group=Group,color=Group)) +
      geom_line(stat="identity",position=position_dodge(0.05))+
      #geom_point()+
      scale_color_manual(values=c('lightslateblue','sienna2')) +
      geom_errorbar(aes(ymin=Mean, ymax=Mean+SD),color="gray39",
                    position=position_dodge(0.05)) +
      geom_text(data=barTab_sig,aes(x=Category, y=max_error_bar+5, label = pvalue_text),color="red") +
      scale_y_sqrt() +
      theme_minimal() + 
      labs(x = char_var)
    line_sqrt_ggplot <- ggplotly(linePlot_sqrt)
    
    for (i in 1:length(unique(linePlot_sqrt$data$Group))) {
      n <- length(unique(linePlot_sqrt$data$Group))
      data <- linePlot_sqrt$data[which(linePlot_sqrt$data$Group==unique(linePlot_sqrt$data$Group)[i]),]
      line_sqrt_ggplot$x$data[[i]]$text <- paste0("Category :",data$Category,
                                                  "\nMean :",round(data$Mean,3),
                                                  "\nSD :",round(data$SD,3),
                                                  "\nGroup :",data$Group)
      if(sum(grepl("\\*",line_sqrt_ggplot$x$data[[i+n]]$text))==0){
        line_sqrt_ggplot$x$data[[i+n]]$text <- paste0("Category :",data$Category,
                                                      "\nMean :",round(data$Mean,3),
                                                      "\nSD :",round(data$SD,3),
                                                      "\nGroup :",data$Group)
      }
    }
    for (i in 1:length(line_sqrt_ggplot$x$data)) {
      text = str_split(line_sqrt_ggplot$x$data[[i]]$hovertext,"<br />max_error_bar")
      hovertext =list()
      if(length(text)>0){
        for (j in 1:length(text)) {
          hovertext[[j]] <- paste(text[[j]][1],"Significant : YES")
        }
        line_sqrt_ggplot$x$data[[i]]$hovertext <-hovertext
      }
    }
    linePlot_sqrt <- line_sqrt_ggplot
    
    ## Fig.2 box plot
    boxTab <- Combined_char_data %>%
      column_to_rownames(var = CHAR) %>% 
      t() %>% as.data.frame() %>%
      merge(group_info, by.x = 0, by.y = 'sample_name')
    colnames(boxTab)[2] <- 'Category'
    
    if(!is.null(insert_ref_group) & !is.null(ref_group)){
      exp_raw_name <- ref_group[-which(insert_ref_group==ref_group)]
      boxTab$group[which(boxTab$group=='ctrl')] <-  insert_ref_group
      boxTab$group[which(boxTab$group=='exp')] <-  exp_raw_name
    }
    
    if(Combine_char_result_table$sig=='yes'){
      group_name <- c(unique(boxTab$group)[1],paste0(unique(boxTab$group)[1],"0"),unique(boxTab$group)[2])
      group_max <- max(boxTab$Category) %>%unique()
      group_min <- min(boxTab$Category) %>%unique()
      if(Combine_char_result_table$p_value<=0.05 &Combine_char_result_table$p_value>0.01){
        t.text <- c('', '*','')
      }else if (Combine_char_result_table$p_value<=0.01 & Combine_char_result_table$p_value>0.001){
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
                 marker=list(line = list(color='rgba(0,0,0,0)'),
                             color = 'rgba(0,0,0,0)'),
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
        layout(xaxis = list(title = 'Group',
                            tickmode = 'array',
                            tickvals =  c(unique(boxTab$group)[1], '', unique(boxTab$group)[2]),
                            ticktext = c(unique(boxTab$group)[1], '', unique(boxTab$group)[2]),
                            titlefont = list(size = 16), 
                            tickfont = list(size = 14)),
               yaxis = list(#title = paste0(char_var, ' index'), 
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
        layout(#title = gene,
          #titlefont = list(size = 20),
          xaxis = list(title = 'Group', 
                       titlefont = list(size = 16), 
                       tickfont = list(size = 14)),
          yaxis = list(title = paste0(CHAR, ' index'), 
                       titlefont = list(size = 16), 
                       tickfont = list(size = 14)#, 
                       #type = "log", 
                       #exponentformat = 'e'
          ), 
          legend = list(font = list(size = 14), 
                        y = 0.5),
          margin = list(l=70, r=70, b=80, t = 60))
    }
    
    
    return(list(DE_char_exp_data = char_exp_data, 
                DE_char_table_all = Result_table, 
                DE_char_combined_table = Combined_char_data, 
                DE_char_combine_result_table = Combine_char_result_table, 
                DE_char_barplot = barPlot,
                DE_char_barplot_sqrt = barPlot_sqrt,
                DE_char_boxplot = boxPlot, 
                DE_char_trendplot = linePlot,
                DE_char_trendplot_sqrt = linePlot_sqrt))
    
  }else{
    
    return(list(DE_char_exp_data = char_exp_data, 
                DE_char_table_all = Result_table, 
                DE_char_combined_table = Combined_char_data, 
                DE_char_combine_result_table = Combine_char_result_table, 
                DE_char_barplot = barPlot,
                DE_char_barplot_sqrt = barPlot_sqrt))
    
  }
  
}