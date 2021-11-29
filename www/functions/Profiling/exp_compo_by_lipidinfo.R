

exp_compo_by_lipidinfo <- function(exp_data, lipid_char_table, char_var){
  
  require(Hmisc)
  require(plotly)
  require(tidyr)
  require(stringr)
  FA_col <- grep("FA_",colnames(lipid_char_table),value = TRUE)
  if(length(FA_col)>0){
    max_comma <- 0
    for(i in 1:length(FA_col)){
      col <- FA_col[i]
      comma_count <- max(str_count(lipid_char_table[,col], ','),na.rm = T)
      if(comma_count>0){
        lipid_char_table <- separate(lipid_char_table,col,c(col,paste0(col,"_",1:comma_count))) 
      }
      if(comma_count>max_comma){max_comma <- comma_count}
    }
    lipid_char_table <- lipid_char_table%>%gather(lipid.category, lipid.category.value,-feature)
    if(max_comma>0){
      for (i in 1:max_comma) {
        select_name <- paste0("_",i)
        lipid_char_table <-lipid_char_table[-intersect(grep(select_name,lipid_char_table[,"lipid.category"]),which(is.na(lipid_char_table$lipid.category.value))),]
      }
      for(i in 1:length(FA_col)){
        col <- FA_col[i]
        lipid_char_table[grep(col,lipid_char_table[,"lipid.category"]),"lipid.category"]<-col
      }
    }
    
  }else{
    lipid_char_table <- lipid_char_table%>%gather(lipid.category, lipid.category.value,-feature) 
  }
  #######################################################
  ####                                                 ####
  #### PLOT: barplot of expression of each lipid class ####
  ####                                                 ####
  #########################################################
  
  
  p.barplot.p <-merge(exp_data %>%gather(sample_name,value,-feature),
                      lipid_char_table,
                      by="feature") %>%
    filter(lipid.category == char_var & !is.na(lipid.category.value)) %>%
    group_by(sample_name, lipid.category, lipid.category.value) %>% 
    summarise(value=sum(value,na.rm=T)) %>%
    ungroup()%>%
    group_by(sample_name) %>%
    mutate(weight = 100/sum(value)) %>%
    mutate(value=value*weight)%>%
    ungroup %>%
    plot_ly(x=~lipid.category.value,
            y=~value,
            color = ~sample_name,
            type="bar",
            hoverinfo="text",
            text=~paste(capitalize(char_var)," :",lipid.category.value,
                        "\nSample name :",sample_name,
                        "\nLipid Expression :",round(value,3))) %>%
    layout(legend = list(title=list(text="Sample name")),
           xaxis = list(title=capitalize(char_var)),
           yaxis = list(title='Lipid Expression',nticks=15),
           title = paste0('Lipid ', capitalize(char_var)))
  
  ######################################
  ####                              ####
  #### PLOT: lipid composition plot ####
  ####                              ####
  ######################################
  
  p.compos <- merge(exp_data %>%gather(sample_name,value,-feature),
                    lipid_char_table,
                    by="feature") %>%
    filter(lipid.category == char_var & !is.na(lipid.category.value)) %>% 
    group_by(sample_name, lipid.category, lipid.category.value) %>% 
    summarise(value=sum(value,na.rm=T)) %>%
    ungroup()%>%
    group_by(sample_name) %>%
    mutate(weight = 100/sum(value)) %>%
    mutate(value=value*weight)%>%
    ungroup %>%
    plot_ly(x=~value,
            y=~sample_name,
            color = ~lipid.category.value,
            type="bar",
            hoverinfo="text",
            orientation = "h",
            text=~paste(capitalize(char_var),":",lipid.category.value,
                        "\nSample name :",sample_name,
                        "\nLipid Expression :",round(value,3))) %>%
    layout(barmode = 'stack',
           legend = list(title=list(text=capitalize(char_var))),
           xaxis = list(title="%",nticks=15),
           yaxis = list(title=''),
           title = paste0('Lipid ', capitalize(char_var), ' Composition'))
  return(list(p.barplot.p = p.barplot.p,
              p.compos = p.compos))
} 






