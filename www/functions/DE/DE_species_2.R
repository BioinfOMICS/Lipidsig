

DE_species_2 <- function(exp_data, data_transform = T, group_info, paired = F, test = 't.test', 
                         adjust_p_method = 'BH', sig_stat = 'p.adj', sig_pvalue = 0.05, sig_FC = 2){
  
  require(tidyverse)
  require(ggthemes)
  require(plotly)
  require(ggpubr)
  
  
  colnames(exp_data)[1] <- 'feature'
  
  if(paired == T){
    exp_data_ga <- exp_data %>% 
      gather(sample_name, value, -1) %>% 
      left_join(group_info, by = 'sample_name') %>% 
      arrange(group, pair, feature)
  }else if(paired == F){
    exp_data_ga <- exp_data %>% 
      gather(sample_name, value, -1) %>% 
      left_join(group_info, by = 'sample_name')
  }
  
  
  if(data_transform == T & test == 't.test'){
    
    exp_data_trans_ga <- exp_data_ga
    exp_data_trans_ga[,3] <- log10(exp_data_trans_ga[,3])
    
    exp_data_tab <- exp_data_ga %>% 
      group_by(feature, group) %>%
      summarise(value = list(value)) %>%
      spread(group, value) %>%
      group_by(feature) %>%
      mutate(mean_ctrl = mean(unlist(ctrl), na.rm = T),
             mean_exp = mean(unlist(exp), na.rm = T), 
             method = 't-test',
             FC = mean_exp/mean_ctrl, 
             log2FC = log2(FC))
    
    exp_data_p <- exp_data_trans_ga %>% 
      group_by(feature, group) %>%
      summarise(value = list(value)) %>%
      spread(group, value) %>%
      group_by(feature) %>%
      mutate(p_value = tryCatch(t.test(unlist(ctrl), unlist(exp), paired = paired, var.equal = T)$p.value,
                                error=function(e){NA}), 
             m_log10_p_value = -log10(p_value))
    #statistic = t.test(unlist(ctrl), unlist(exp))$statistic)
    exp_data_p$p_adj = p.adjust(exp_data_p$p_value, method = adjust_p_method)
    exp_data_p$m_log10_p_adj = -log10(exp_data_p$p_adj)
    
    exp_data_stat <- exp_data_tab[-2:-3] %>% 
      right_join(exp_data_p[-2:-3], by = 'feature') %>% 
      mutate(sig_p = ifelse(p_value < sig_pvalue & abs(log2FC) > log2(sig_FC), 'yes', 'no'), 
             sig_p_adj = ifelse(p_adj < sig_pvalue & abs(log2FC) > log2(sig_FC), 'yes', 'no'))
    
    
  }else if(data_transform == F & test == 't.test'){
    
    exp_data_p <- exp_data_ga %>% 
      group_by(feature, group) %>%
      summarise(value = list(value)) %>%
      spread(group, value) %>%
      group_by(feature) %>%
      mutate(mean_ctrl = mean(unlist(ctrl), na.rm = T),
             mean_exp = mean(unlist(exp), na.rm = T), 
             method = 't-test',
             FC = mean_exp/mean_ctrl,
             log2FC = log2(FC),
             p_value = tryCatch(t.test(unlist(ctrl), unlist(exp), paired = paired, var.equal = T)$p.value,
                                error=function(e){NA}), 
             m_log10_p_value = -log10(p_value))
    exp_data_p$p_adj = p.adjust(exp_data_p$p_value, method = adjust_p_method)
    exp_data_p$m_log10_p_adj = -log10(exp_data_p$p_adj)
    
    exp_data_stat <- exp_data_p[-2:-3] %>% 
      mutate(sig_p = ifelse(p_value < sig_pvalue & abs(log2FC) > log2(sig_FC), 'yes', 'no'), 
             sig_p_adj = ifelse(p_adj < sig_pvalue & abs(log2FC) > log2(sig_FC), 'yes', 'no'))
    
  }else if(data_transform == T & test == 'wilcox.test'){
    
    exp_data_trans_ga <- exp_data_ga
    exp_data_trans_ga[,3] <- log10(exp_data_trans_ga[,3])
    
    exp_data_tab <- exp_data_ga %>% 
      group_by(feature, group) %>%
      summarise(value = list(value)) %>%
      spread(group, value) %>%
      group_by(feature) %>%
      mutate(mean_ctrl = mean(unlist(ctrl), na.rm = T),
             mean_exp = mean(unlist(exp), na.rm = T), 
             method = 'wilcoxon test',
             FC = mean_exp/mean_ctrl, 
             log2FC = log2(FC))
    
    exp_data_p <- exp_data_trans_ga %>% 
      group_by(feature, group) %>%
      summarise(value = list(value)) %>%
      spread(group, value) %>%
      group_by(feature) %>%
      mutate(p_value = tryCatch(wilcox.test(unlist(ctrl), unlist(exp), paired = paired)$p.value,
                                error=function(e){NA}), 
             m_log10_p_value = -log10(p_value))
    #statistic = wilcox.test(unlist(ctrl), unlist(exp))$statistic, 
    exp_data_p$p_adj = p.adjust(exp_data_p$p_value, method = adjust_p_method)
    exp_data_p$m_log10_p_adj = -log10(exp_data_p$p_adj)
    
    exp_data_stat <- exp_data_tab[-2:-3] %>% 
      right_join(exp_data_p[-2:-3], by = 'feature') %>% 
      mutate(sig_p = ifelse(p_value < sig_pvalue & abs(log2FC) > log2(sig_FC), 'yes', 'no'), 
             sig_p_adj = ifelse(p_adj < sig_pvalue & abs(log2FC) > log2(sig_FC), 'yes', 'no'))
    
    
  }else if(data_transform == F & test == 'wilcox.test'){
    
    exp_data_p <- exp_data_ga %>% 
      group_by(feature, group) %>%
      summarise(value = list(value)) %>%
      spread(group, value) %>%
      group_by(feature) %>%
      mutate(mean_ctrl = mean(unlist(ctrl), na.rm = T),
             mean_exp = mean(unlist(exp), na.rm = T), 
             method = 'wilcoxon test',
             FC = mean_exp/mean_ctrl,
             log2FC = log2(FC),
             p_value = tryCatch(wilcox.test(unlist(ctrl), unlist(exp), paired = paired)$p.value,
                                error=function(e){NA}), 
             m_log10_p_value = -log10(p_value))
    exp_data_p$p_adj = p.adjust(exp_data_p$p_value, method = adjust_p_method)
    exp_data_p$m_log10_p_adj = -log10(exp_data_p$p_adj)
    
    exp_data_stat <- exp_data_p[-2:-3] %>%
      mutate(sig_p = ifelse(p_value < sig_pvalue & abs(log2(FC)) > log2(sig_FC), 'yes', 'no'),
             sig_p_adj = ifelse(p_adj < sig_pvalue & abs(log2(FC)) > log2(sig_FC), 'yes', 'no'))
    
  }
  
  exp_data_stat$mean_ctrl <- round(exp_data_stat$mean_ctrl, 5)
  exp_data_stat$mean_exp <- round(exp_data_stat$mean_exp, 5)
  exp_data_stat$FC <- round(exp_data_stat$FC, 3)
  exp_data_stat$log2FC <- round(exp_data_stat$log2FC, 3)
  exp_data_stat$m_log10_p_value <- round(exp_data_stat$m_log10_p_value, 3)
  exp_data_stat$m_log10_p_adj <- round(exp_data_stat$m_log10_p_adj, 3)
  
  #### Significant lipid ####
  if(sig_stat == 'p'){
    sig.diff.exp <- exp_data_stat %>%
      filter(sig_p == 'yes') %>% 
      distinct(feature, .keep_all = T)
  }else if(sig_stat == 'p.adj'){
    sig.diff.exp <- exp_data_stat %>%
      filter(sig_p_adj == 'yes') %>% 
      distinct(feature, .keep_all = T)
  }
  
  if(nrow(sig.diff.exp) > 0){
    
    #### lolipop chart ####
    if(sum(is.finite(sig.diff.exp$log2FC)) == 0){ ##all sig. lipid is infinite
      
      #sig.diff.infinite <- sig.diff.exp
      sig.diff.exp$log2FC <- ifelse(sig.diff.exp$log2FC > 0, 5, -5)
      
      p.dotchart <- ggdotchart(sig.diff.exp, x = "feature", y = "log2FC", 
                               rotate = TRUE,  
                               color = "white",
                               #palette = "Red-Blue", 
                               sorting = "descending",
                               add = "segments",
                               #add.params = list(color = "lightgray", size = 0.5), 
                               #group = "group",
                               dot.size = 2.5,
                               legend.title = "log2(FC)", 
                               xlab = " ",
                               ylab = "log2(Fold Change)", 
                               legend = "right", 
                               #orientation = "horizontal",
                               ggtheme = theme_pubr()) #+
      p.dotchart <- if(sig_stat == "p"){
        p.dotchart + geom_point(aes(text=paste("feature :",feature,"<br>","log2(FC) : ",round(log2FC,2),"<br>","-log10(p-value) :",m_log10_p_value),color = m_log10_p_value,size=2.5)) + 
          guides(size = FALSE) + labs(colour="-log10(p-value)") +
          scale_colour_gradient2(low="steelblue", mid="white", high="red", midpoint=0) +
          scale_y_continuous(breaks = c(-5, -2, -1, 0, 1, 2, 5), labels = c('-Inf', -2, -1, 0, 1, 2, 'Inf'), limits = c(-6, 6))
      }else{
        p.dotchart+geom_point(aes(text=paste("feature:",feature,"<br>","log2(FC): ",
                                             round(log2FC,2),"<br>","-log10(padj) :",m_log10_p_adj),color = m_log10_p_adj,size=2.5)) + 
          guides(size = FALSE) + labs(colour="-log10(padj)") +
          scale_colour_gradient2(low="steelblue", mid="white", high="red", midpoint=0) +
          scale_y_continuous(breaks = c(-5, -2, -1, 0, 1, 2, 5), labels = c('-Inf', -2, -1, 0, 1, 2, 'Inf'), limits = c(-6, 6))
      }
      
      # geom_point(aes(text=paste("feature :",feature,"<br>","log2FC : ",round(log2FC,2)), color = log2FC, size=2.5)) + 
      # guides(size = FALSE) + 
      # scale_colour_gradient2(low="steelblue", mid="white", high="red", midpoint=0) +
      # gradient_color('PuOr') +
      # scale_y_continuous(breaks = c(-5, -2, -1, 0, 1, 2, 5), labels = c('-Inf', -2, -1, 0, 1, 2, 'Inf'), limits = c(-6, 6))
      
      #in.dotchart <- ggplotly(p.dotchart,tooltip = "text")
      
      
      if(nrow(sig.diff.exp) < 26){
        in.dotchart <- ggplotly(p.dotchart,
                                #width = 800,
                                height = 500,
                                #margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
                                tooltip = "text")
      }else{
        in.dotchart <- ggplotly(p.dotchart,
                                #width = 800,
                                height = nrow(sig.diff.exp)*20,
                                #margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
                                tooltip = "text")
      }
      
      i=NULL
      for(i in 1:length(in.dotchart$x$data)){
        in.dotchart$x$data[[i]]$text <- in.dotchart$x$data[[i]]$text %>% 
          str_replace_all(pattern = 'log2(FC): 5', replacement = 'log2(FC): Inf') %>% 
          str_replace_all(pattern = 'log2(FC):  5', replacement = 'log2(FC):  Inf') %>% 
          str_replace_all(pattern = 'log2(FC): -5', replacement = 'log2(FC): -Inf') %>% 
          str_replace_all("Inf\\..*", "Inf")
      }
      
      
    }else if(sum(is.infinite(sig.diff.exp$log2FC)) == 0){ ##all sig. lipid is finite
      
      p.dotchart <- ggdotchart(sig.diff.exp, x = "feature", y = "log2FC", 
                               rotate = TRUE,  
                               color = "white",
                               sorting = "descending",
                               add = "segments",
                               #add.params = list(color = "lightgray", size = 0.5), 
                               #group = "group",
                               dot.size = 2.5,
                               legend.title = "log2(FC)", 
                               xlab = " ",
                               ylab = "log2(Fold Change)", 
                               legend = "right", 
                               #orientation = "horizontal",
                               ggtheme = theme_pubr()) #+
      p.dotchart <- if(sig_stat == "p"){
        p.dotchart + geom_point(aes(text=paste("feature :",feature,"<br>","log2(FC) : ",round(log2FC,2),"<br>",
                                               "-log10(p-value) :",m_log10_p_value),color = m_log10_p_value,size=2.5)) + 
          guides(size = FALSE) + labs(colour="-log10(p-value)") +
          scale_colour_gradient2(low="steelblue", mid="white", high="red", midpoint=0)
      }else{
        p.dotchart + geom_point(aes(text=paste("feature :",feature,"<br>","log2(FC) : ",round(log2FC,2),"<br>",
                                               "-log10(padj) :",m_log10_p_adj),color = m_log10_p_adj,size=2.5)) + 
          guides(size = FALSE) + labs(colour="-log10(padj)") +
          scale_colour_gradient2(low="steelblue", mid="white", high="red", midpoint=0)
      }
      
      # geom_point(aes(text=paste("feature :",feature,"<br>","log2FC : ",round(log2FC,2)),
      #                color = log2FC,size=2.5)) + guides(size = FALSE) +
      # scale_colour_gradient2(low="steelblue", mid="white", high="red", midpoint=0)
      #gradient_color('PuOr')#+ geom_hline(yintercept = 0, linetype = 1, color = "lightgray")
      
      #in.dotchart <- ggplotly(p.dotchart,tooltip = "text")
      
      if(nrow(sig.diff.exp) < 26){
        in.dotchart <- ggplotly(p.dotchart,
                                #width = 800,
                                height = 500,
                                #margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
                                tooltip = "text")
      }else{
        in.dotchart <- ggplotly(p.dotchart,
                                #width = 800,
                                height = nrow(sig.diff.exp)*20,
                                #margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
                                tooltip = "text")
      }
      
    }else{
      
      rm.inf <- sig.diff.exp %>% filter(is.finite(log2FC))
      INF <- ceiling(max(rm.inf$abs.log2FC, na.rm = T) * 1.5)
      
      #sig.diff.mix <- sig.diff.exp
      sig.diff.exp$log2FC <- ifelse(is.infinite(sig.diff.exp$log2FC) & sig.diff.exp$log2FC > 0, INF, 
                                    ifelse(is.infinite(sig.diff.exp$log2FC) & sig.diff.exp$log2FC < 0, -INF, sig.diff.exp$log2FC))
      
      p.dotchart <- ggdotchart(sig.diff.exp, x = "feature", y = "log2FC", 
                               rotate = TRUE,  
                               color = "white",
                               sorting = "descending",
                               add = "segments",
                               #add.params = list(color = "lightgray", size = 0.5), 
                               #group = "group",
                               dot.size = 2.5,
                               legend.title = "log2(FC)", 
                               xlab = " ",
                               ylab = "log2(Fold Change)", 
                               legend = "right", 
                               #orientation = "horizontal",
                               ggtheme = theme_pubr()) #+ 
      p.dotchart <- if(sig_stat == "p"){
        p.dotchart + geom_point(aes(text=paste("feature :",feature,"<br>","log2(FC) : ",round(log2FC,2),"<br>",
                                               "-log10(p-value) :",m_log10_p_value),color = m_log10_p_value,size=2.5))+ 
          guides(size = FALSE) + labs(colour="-log10(p-value)") +
          scale_colour_gradient2(low="steelblue", mid="white", high="red", midpoint=0) + 
          scale_y_continuous(breaks = c(-INF, -2, -1, 0, 1, 2, INF), labels = c('-Inf', -2, -1, 0, 1, 2, 'Inf'), limits = c(-(INF+1), (INF+1)))
      }else{
        p.dotchart + geom_point(aes(text=paste("feature :",feature,"<br>","log2(FC) : ",round(log2FC,2),"<br>",
                                               "-log10(padj) :",m_log10_p_adj),color = m_log10_p_adj,size=2.5)) + 
          guides(size = FALSE) + labs(colour="-log10(padj)") +
          scale_colour_gradient2(low="steelblue", mid="white", high="red", midpoint=0) + 
          scale_y_continuous(breaks = c(-INF, -2, -1, 0, 1, 2, INF), labels = c('-Inf', -2, -1, 0, 1, 2, 'Inf'), limits = c(-(INF+1), (INF+1)))
      }
      
      
      # geom_point(aes(text=paste("feature :",feature,"<br>","log2FC : ",round(log2FC,2)),
      #                color = log2FC,size=2.5)) + guides(size = FALSE) +
      # scale_colour_gradient2(low="steelblue", mid="white", high="red", midpoint=0) +
      # # gradient_color('PuOr') +
      # scale_y_continuous(breaks = c(-INF, -2, -1, 0, 1, 2, INF), labels = c('-Inf', -2, -1, 0, 1, 2, 'Inf'), limits = c(-(INF+1), (INF+1)))
      
      #in.dotchart <- ggplotly(p.dotchart,tooltip = "text")
      
      if(nrow(sig.diff.exp) < 26){
        in.dotchart <- ggplotly(p.dotchart,
                                #width = 800,
                                height = 500,
                                #margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
                                tooltip = "text")
      }else{
        in.dotchart <- ggplotly(p.dotchart,
                                #width = 800,
                                height = nrow(sig.diff.exp)*20,
                                #margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
                                tooltip = "text")
      }
      
      
      i=NULL
      for(i in 1:length(in.dotchart$x$data)){
        in.dotchart$x$data[[i]]$text <- in.dotchart$x$data[[i]]$text %>% 
          str_replace_all(pattern = paste0('log2(FC): ', INF), replacement = 'log2(FC): Inf') %>% 
          str_replace_all(pattern = paste0('log2(FC):  ', INF), replacement = 'log2(FC):  Inf') %>% 
          str_replace_all(pattern = paste0('log2(FC): -', INF), replacement = 'log2(FC): -Inf') %>% 
          str_replace_all("Inf\\..*", "Inf")
      }
      
    }
    
  }else{
    
    in.dotchart = NULL
    
  } #if(nrow(sig.diff.exp) > 0)
  
  return(list(DE_species_table_all = exp_data_stat, 
              DE_species_table_sig = sig.diff.exp, 
              DE_species_dotchart_sig = in.dotchart))
  
} #function


