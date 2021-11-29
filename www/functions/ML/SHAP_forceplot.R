

SHAP_forceplot <- function(shap_score, topN_feature=10, 
                           cluster_method = "ward.D", group_num = 10, 
                           zoom_in_loc=NULL,zoom_in_group=NULL){
  f_plot <- function (shapobs, id = "sorted_id", zoom_in_location = NULL, 
                      y_parent_limit = NULL, y_zoomin_limit = NULL, zoom_in = TRUE, 
                      zoom_in_group = NULL, x_rank='sorted_id') 
  {
    shapobs_long <- melt.data.table(shapobs, measure.vars = colnames(shapobs)[!colnames(shapobs) %in% 
                                                                                c(id, "group", "ID")])
    shapobs_long <- as.data.frame(shapobs_long)
    if (dim(shapobs)[2] - 1 <= 12) {
      p <- plot_ly(shapobs_long,x=shapobs_long[,which(colnames(shapobs_long)==x_rank)],y = ~value,
                   type='bar',color = ~variable,hoverinfo="text",
                   colors = RColorBrewer::brewer.pal(dim(shapobs)[2] -1, "Paired"),
                   text=~paste("Sorted ID :",shapobs_long[,which(colnames(shapobs_long)==x_rank)],
                               "\nShapley values by feature :",round(value,3),"\nFeature :",variable)) %>%
        layout(barmode = "relative",
               title = 'SHAP force plot',
               xaxis = list(title ="Sorted ID"),
               yaxis = list(title ="Shapley values by feature:\n (Contribution to the base value)"),
               legend = list(title=list(text="Feature"),y=0.5,x=1.1,font = list(size = 9)))  
    }
    else {
      p <- plot_ly(shapobs_long,x=shapobs_long[,which(colnames(shapobs_long)==x_rank)],y = ~value,
                   type='bar',color = ~variable,hoverinfo="text",
                   text=~paste("Sorted ID :",shapobs_long[,which(colnames(shapobs_long)==x_rank)],
                               "\nShapley values by feature :",round(value,3),"\nFeature :",variable)) %>%
        layout(barmode = "relative",
               title = 'SHAP force plot',
               xaxis = list(title ="Sorted ID"),
               yaxis = list(title ="Shapley values by feature:\n (Contribution to the base value)"),
               legend = list(title=list(text="Feature"),y=0.5,x=1.1,font = list(size = 9)))  
    }
    if (!is.null(y_parent_limit) & !is.null(y_zoomin_limit)) {
      warning("Just notice that when parent limit is set, the zoom in axis limit won't work, it seems to be a problem of ggforce.\n")
    }
    #   if (zoom_in & is.null(zoom_in_group)) {
    #     x_mid <- if (is.null(zoom_in_location)) 
    #       shapobs[, .N] * 0.6
    #     else zoom_in_location
    #     x_interval <- stats::median(c(50, 150, floor(shapobs[, 
    #                                                          .N] * 0.1)))
    #     message("Data has N = ", shapobs[, .N], " | zoom in length is ", 
    #             x_interval, " at location ", x_mid, ".\n")
    #     #p <- p + ggforce::facet_zoom(xlim = c(x_mid, x_mid + 
    #     #                                        x_interval), ylim = y_zoomin_limit, horizontal = F, 
    #     #                             zoom.size = 0.6) + theme(zoom.y = element_blank(), 
    #     #                                                      validate = FALSE)
    #     p <- p + ggforce::facet_zoom(xlim = c(zoom_in_loc[1],zoom_in_loc[2]), 
    #                                  ylim = y_zoomin_limit, horizontal = F, 
    #                                  zoom.size = 0.6) + theme(zoom.y = element_blank(), 
    #                                                           validate = FALSE)
    #   }
    #   else if (zoom_in) {
    #     message("Data has N = ", shapobs[, .N], " | zoom in at cluster ", 
    #             zoom_in_group, " with N = ", shapobs[group == zoom_in_group, 
    #                                                  .N], ".\n")
    #     p <- p + ggforce::facet_zoom(x = group == zoom_in_group, 
    #                                  ylim = y_zoomin_limit, horizontal = F, zoom.size = 0.6) + 
    #       theme(zoom.y = element_blank(), validate = FALSE)
    #   }
    #   else {
    #     message("Data has N = ", shapobs[, .N], " | no zoom in.\n")
    #   }
    return(p)
  }
  if(topN_feature>10){topN_feature <- 10}else{topN_feature <- topN_feature}
  
  plot_data <- shap.prep.stack.data(shap_contrib = shap_score, 
                                    top_n = topN_feature,cluster_method=cluster_method, n_groups = group_num)
  if(is.null(zoom_in_loc)&&is.null(zoom_in_group)){zoom_in <- F}else{zoom_in <- T}
  f_plot <- f_plot(plot_data,zoom_in = zoom_in, 
                   x_rank='sorted_id', 
                   zoom_in_location=zoom_in_loc,
                   zoom_in_group=zoom_in_group)
  plot_data <- plot_data %>% as.data.frame() %>% 
    dplyr::select(ID, group, sorted_id,everything())
  
  colnames(plot_data)[1:3] <- c('Sample ID', 'Group', 'Sorted ID')
  
  return(list(plot_data,f_plot))
}

