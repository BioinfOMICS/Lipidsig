Enrichment <- function(DE_species_table_sig, lipid_char_table, char_var, sig_pvalue = 0.05){
  
  require(tidyverse)
  require(plotly)
  require(ggthemes)
  
  lipo.fisher <- function(sig.mat, lipo.category){
    
    sig.class <- sig.mat %>%
      group_by(characteristic)%>%
      summarise(sig.count=n())%>%
      left_join(lipo.category, by='characteristic')%>%
      mutate(p.value=NA, significant='NO')
    
    n.sig <- sig.class%>%
      summarise(sum=sum(sig.count)) %>% 
      .$sum
    
    n.lipid <- lipo.category %>% 
      summarise(sum=sum(total.count)) %>% 
      .$sum
    
    for (i in 1:nrow(sig.class)){
      
      sig.class$p.value[i]=fisher.test(matrix(c(sig.class$sig.count[i], 
                                                sig.class$total.count[i], 
                                                n.sig-sig.class$sig.count[i],
                                                n.lipid-sig.class$total.count[i]), nrow=2), alternative = 'greater')$p.value
    }
    sig.class$m.log.p=-log10(sig.class$p.value)
    sig.class$significant[sig.class$p.value < sig_pvalue]='YES'
    
    return(sig.class) 
  } #function
  
  
  lipo.category <- lipid_char_table %>% 
    dplyr::select(feature, 'characteristic' = all_of(char_var)) %>% 
    group_by(characteristic) %>% 
    summarise(total.count = n())
  
  char.tab <- lipid_char_table %>% 
    dplyr::select(feature, 'characteristic' = all_of(char_var))
  
  sig_lipid.all <- DE_species_table_sig %>% 
    left_join(char.tab, by = 'feature') %>% 
    filter(!is.na(characteristic)) %>%
    mutate(condition = 'UP & DOWN')
  
  sig_lipid.up <- DE_species_table_sig %>% 
    left_join(char.tab, by = 'feature') %>% 
    filter(!is.na(characteristic)) %>%
    filter(log2FC > 0) %>%
    mutate(condition = 'UP')
  
  sig_lipid.down <- DE_species_table_sig %>% 
    left_join(char.tab, by = 'feature') %>% 
    filter(!is.na(characteristic)) %>%
    filter(log2FC < 0) %>% 
    mutate(condition = 'DOWN')
  
  #### enrichment analysis by category ####
  sig_class.all <- lipo.fisher(sig_lipid.all, lipo.category) %>% mutate(condition='UP & DOWN')
  if(nrow(sig_lipid.up) > 0){
    sig_class.up <- lipo.fisher(sig_lipid.up, lipo.category) %>% mutate(condition='UP')
  }else{
    sig_class.up <- NULL
  }
  if(nrow(sig_lipid.down) > 0){
    sig_class.down <- lipo.fisher(sig_lipid.down, lipo.category) %>% mutate(condition='DOWN')
  }else{
    sig_class.down <- NULL
  }
  
  sig_class <- bind_rows(sig_class.all,sig_class.up,sig_class.down)
  sig_class$characteristic <- factor(sig_class$characteristic, sort(unique(sig_class$characteristic)))
  
  plot.tab <- sig_class %>% 
    filter(condition != 'UP & DOWN') %>% 
    mutate(mlogP = ifelse(condition == 'DOWN', -(m.log.p), m.log.p), 
           significance = ifelse(significant == 'YES' & condition == 'UP', 'UP', 
                                 ifelse(significant == 'YES' & condition == 'DOWN', 'DOWN', 'non-significant')))
  
  if(max(plot.tab$m.log.p) == 0){
    
    in.sig.class <- NULL
    
  }else{
    
    if(length(unique(plot.tab$condition)) == 2){
      
      x.max <- max(ceiling(plot.tab$m.log.p))
      x.label <- as.character(c(x.max:1, 0, 1:x.max))
      plot.tab <- plot.tab %>% group_by(characteristic) %>% mutate(rank=mlogP[which(abs(mlogP)==max(abs(mlogP)))])
      p.sig.class <- ggplot(plot.tab, 
                            aes(x=mlogP, y=reorder(characteristic, rank,max), fill=significance)) + 
        geom_col() + 
        geom_vline(xintercept = 0, color = '#444444') + 
        theme_hc() + 
        scale_fill_manual(breaks=c('DOWN', 'UP', 'non-significant'), values =c('#4169E1', '#FF6347', '#666666'))+ 
        #geom_text(aes(label = as.character(sig.count)), vjust=-.3, size=3) + 
        theme(axis.title = element_text(size=14), 
              axis.text.y=element_text(size=12), 
              axis.text.x=element_text(size=12)) + 
        labs(x='-log10(p-value)', y=char_var) + 
        scale_x_continuous(breaks = -x.max:x.max, 
                           labels = x.label)
      
    }else if(length(unique(plot.tab$condition)) == 1 & unique(plot.tab$condition) == 'UP'){
      
      x.max <- max(ceiling(plot.tab$m.log.p))
      x.label <- as.character(0:x.max)
      
      p.sig.class <- ggplot(plot.tab, 
                            aes(x=mlogP, y=reorder(characteristic, mlogP), fill=significance)) + 
        geom_col() + 
        #geom_vline(xintercept = 0, color = '#444444') + 
        theme_hc() + 
        scale_fill_manual(breaks=c('DOWN', 'UP', 'non-significant'), values =c('#4169E1', '#FF6347', '#666666'))+ 
        #geom_text(aes(label = as.character(sig.count)), vjust=-.3, size=3) + 
        theme(axis.title = element_text(size=14), 
              axis.text.y=element_text(size=12), 
              axis.text.x=element_text(size=12)) + 
        labs(x='-log10(p-value)', y=char_var) + 
        scale_x_continuous(breaks = 0:x.max, 
                           labels = x.label)
      
    }else if(length(unique(plot.tab$condition)) == 1 & unique(plot.tab$condition) == 'DOWN'){
      
      x.max <- max(ceiling(plot.tab$m.log.p))
      x.label <- as.character(x.max:0)
      
      p.sig.class <- ggplot(plot.tab, 
                            aes(x=mlogP, y=reorder(characteristic, mlogP), fill=significance)) + 
        geom_col() + 
        #geom_vline(xintercept = 0, color = '#444444') + 
        theme_hc() + 
        scale_fill_manual(breaks=c('DOWN', 'UP', 'non-significant'), values =c('#4169E1', '#FF6347', '#666666'))+ 
        #geom_text(aes(label = as.character(sig.count)), vjust=-.3, size=3) + 
        theme(axis.title = element_text(size=14), 
              axis.text.y=element_text(size=12), 
              axis.text.x=element_text(size=12)) + 
        labs(x='-log10(p-value)', y=char_var) + 
        scale_x_continuous(breaks = -x.max:0, 
                           labels = x.label)
      
    }
    
    
    # p.sig.class <- ggplot(sig_class,
    #                       aes(x=characteristic, y=m.log.p, fill=significant)) + 
    #   geom_col() +
    #   facet_wrap(~condition, ncol=1) +
    #   geom_abline(slope = 0, intercept = -log10(sig_pvalue), color='red', size=.3) +
    #   geom_text(aes(label = as.character(sig.count)), vjust=-.3, size=3) +
    #   theme_hc()+
    #   theme(axis.title = element_text(size=14), 
    #         axis.text.y=element_text(size=12), 
    #         axis.text.x=element_text(size=12)) +
    #   scale_fill_manual(breaks=c('NO','YES'), values =c('#666666', '#FF6347'))+
    #   labs(y='-log10(p-value)', x=char_var)
    
    p.sig.class$data$p.value <- round(p.sig.class$data$p.value,3)
    p.sig.class$data$mlogP <- round(p.sig.class$data$mlogP,3)
    in.sig.class <- ggplotly(p.sig.class)
    for (i in 1:length(in.sig.class$x$data)){
      in.sig.class$x$data[[i]]$text = gsub("reorder\\(characteristic, rank, max\\)",char_var,in.sig.class$x$data[[i]]$text)
      in.sig.class$x$data[[i]]$text = gsub("reorder\\(characteristic, mlogP\\)",char_var,in.sig.class$x$data[[i]]$text)
      in.sig.class$x$data[[i]]$text <- str_replace(string = in.sig.class$x$data[[i]]$text, pattern = '-', replacement = '')
      in.sig.class$x$data[[i]]$text <- str_replace(string = in.sig.class$x$data[[i]]$text, pattern = 'mlogP', replacement = '-log10(p-value)')
      if (!is.null(in.sig.class$x$data[[i]]$hovertext)){
        in.sig.class$x$data[[i]]$hovertext =  gsub("\\(","",in.sig.class$x$data[[i]]$hovertext)
        in.sig.class$x$data[[i]]$hovertext =  gsub("\\)","",in.sig.class$x$data[[i]]$hovertext)
        in.sig.class$x$data[[i]]$hovertext = gsub("as.charactersig.count","sig.count ",in.sig.class$x$data[[i]]$hovertext)
        in.sig.class$x$data[[i]]$hovertext = gsub("\\reorder(characteristic, mlogP)",char_var,in.sig.class$x$data[[i]]$hovertext)
        in.sig.class$x$data[[i]]$hovertext <- str_replace(in.sig.class$x$data[[i]]$hovertext, pattern = '-', replacement = '')
        in.sig.class$x$data[[i]]$hovertext <- str_replace(in.sig.class$x$data[[i]]$hovertext, pattern = 'mlogP', replacement = '-log10(p-value)')
      }
    }
    
  }
  
  
  final.tab <- sig_class %>% 
    filter(condition != 'UP & DOWN') %>% 
    dplyr::select(condition, characteristic, sig.count, total.count, p.value, m.log.p, significant)
  
  final.tab$m.log.p <- round(final.tab$m.log.p, 3)
  #colnames(final.tab)[2:7] <- c(char_var, 'significant lipids', 'total lipids', 'p-value', '-log10(p-value)', 'significance')
  
  return(list(enrich_char_table = final.tab,
              enrich_char_barplot = in.sig.class))
  
} #function


