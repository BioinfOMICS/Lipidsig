# Sig_lipid_feature (DE_species_table_sig, lipid_info_table, lipid_var)

Sig_lipid_feature <- function(DE_species_table_sig, lipid_char_table, char_var, sig_FC = 2){
  
  require(tidyverse)
  require(plotly)
  require(ggpubr)
  require(hwordcloud)
  
  char <- lipid_char_table %>% 
    dplyr::select(feature, all_of(char_var))
  colnames(char)[2] <- 'characteristic'
  
  plot.tab <- DE_species_table_sig %>% 
    left_join(char, by = 'feature') %>% 
    filter(!is.na(characteristic)) %>%
    arrange(characteristic)
  plot.tab$characteristic <- factor(plot.tab$characteristic, sort(unique(plot.tab$characteristic)))
  #plot.order <- sort(unique(plot.tab$characteristic))
  
  #### bar plot ####
  sig_class.log2FC <- plot.tab %>%
    group_by(characteristic)%>%
    summarise(log2FC.mean=mean(log2FC),
              log2FC.sd=sd(log2FC),
              log2FC.direction=log2FC.mean/abs(log2FC.mean),
              significant='NO')%>%
    mutate(log2FC.meansd=log2FC.mean+log2FC.sd*log2FC.direction,
           significant=replace(x = significant, list = abs(log2FC.mean) > log2(sig_FC), values = 'YES'))
  
  p.log2fc.sig <- ggplot(sig_class.log2FC,
                         aes(x=reorder(characteristic,-log2FC.mean), y=log2FC.mean, fill=significant)) + 
    geom_col() + 
    geom_errorbar(aes(min=log2FC.mean, max=log2FC.meansd)) + 
    theme_hc() +
    scale_fill_manual(breaks=c('NO','YES'), values =c('#666666', '#FF6347'))+
    labs(fill=paste0('> ', sig_FC, ' FC'),
         y='log2(Fold Change)',
         x=char_var,
         title='Significant lipids') + # (without infinite fold change)
    theme(plot.title=element_text(size=16, hjust = 0.5), 
          axis.title=element_text(size=14), 
          axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1,size=12),#, angle = 30),
          axis.text.y=element_text(size=12))
  
  p.log2fc.sig$data$log2FC.mean <- round(p.log2fc.sig$data$log2FC.mean,3)
  p.log2fc.sig$data$log2FC.sd <- round(p.log2fc.sig$data$log2FC.sd,3)
  p.log2fc.sig$data$log2FC.meansd <- round(p.log2fc.sig$data$log2FC.meansd,3)
  in.log2fc.sig.ggplotly <- ggplotly(p.log2fc.sig)
  
  for(i in 1:length(in.log2fc.sig.ggplotly$x$data)){
    in.log2fc.sig.ggplotly$x$data[[i]]$text =paste0(char_var,' :',p.log2fc.sig$data$characteristic,
                                                    '\nmean(log2FC) :',round(p.log2fc.sig$data$log2FC.mean,3),
                                                    '\nsignificant :',p.log2fc.sig$data$significant)
  }
  
  in.log2fc.sig <- in.log2fc.sig.ggplotly
  
  #### lolipop chart ####
  sig.dotchart <- plot.tab %>%
    group_by(characteristic)%>%
    mutate(log2FC.mean=mean(log2FC))
  sig.dotchart$characteristic <- fct_reorder(sig.dotchart$characteristic,sig.dotchart$log2FC.mean, min)
  
  
  p.sig.dotchart <- ggdotchart(sig.dotchart, combine = T, 
                               x = "characteristic", y = "log2FC", 
                               rotate = TRUE,  
                               color = "white",
                               sorting = "none",
                               add = "segments",
                               #add.params = list(color = "lightgray", size = 0.5), 
                               dot.size = 3,
                               legend.title = char_var, 
                               xlab = " ",
                               ylab = "log2(Fold change)", 
                               legend = "right",
                               #order = plot.order,
                               #orientation = "horizontal",
                               ggtheme = theme_pubr()) +
    geom_point(aes(text=paste("Characteristic :",characteristic,"<br>","log2FC : ",round(log2FC,2)),
                   color = characteristic,size=3)) + 
    guides(size = FALSE)
  in.sig.dotchart <- ggplotly(p.sig.dotchart,tooltip = "text")
  
  
  #### word cloud ####
  wc.tab <- plot.tab %>%
    group_by(characteristic) %>%
    summarise(freqs = n())
  
  wc <- hwordcloud(text = wc.tab$characteristic, 
                   size = wc.tab$freqs, 
                   theme = "gridlight")
  
  
  return(list(barPlot = in.log2fc.sig, 
              lolipop = in.sig.dotchart, 
              word = wc))
  
} #function