library(gridExtra)   # a helper for arranging individual ggplot objects
library(ggthemes)    # has a clean theme for ggplot2
library(viridis)     # best. color. palette. evar.

super_cor_plot <- function(cor_mat, 
                           bw = F, 
                           text_size = 10, 
                           base_size = 15, 
                           title = "Correlation plot", 
                           subset_x = NULL, 
                           subset_y = NULL, 
                           xlabs = F, 
                           ylabs = T){
  id_levels <- factor(row.names(cor_mat))
  tmp_df <- as.data.frame(cor_mat)
  tmp_df$id <- factor(row.names(tmp_df), levels = id_levels) 
  browser()
  #tmp_df <- melt(tmp_df)
  tmp_df <- tmp_df %>% pivot_longer(-id)
  #tmp_df$id <- factor(tmp_df$id, levels=rev(levels(tmp_df$id)))
  tmp_df$name <- factor(tmp_df$name, levels = rev(levels(tmp_df$id)))
  if(!is.null(subset_x) & length(subset_x) != 0){
    tmp_df <- tmp_df %>% filter(id %in% subset_x)
    tmp_df <- tmp_df %>% filter(!(name %in% subset_x))
  }
  if(!is.null(subset_y) & length(subset_y) != 0){
    tmp_df <- tmp_df %>% filter(name %in% subset_y)
  }
  stopifnot(nrow(tmp_df) > 0)
  super_heat_map(tmp_df, 
                 title = title, 
                 tile_border_size = 0, 
                 xlabs = xlabs,
                 ylabs = ylabs,
                 text_size = text_size, 
                 base_size = base_size, 
                 bw = bw)
  
}
super_heat_map <- function(melted_df, 
                           title = "", 
                           scale_name = "", 
                           bw = T, 
                           xlabs = T, 
                           ylabs = T, 
                           text_size = 6, 
                           base_size = 11, 
                           tile_border_size = .1){
  gg <- ggplot(melted_df, aes(x = id, y = name, fill = value))
  if(tile_border_size <= 0){
    gg <- gg + geom_tile()
  }
  else{
    gg <- gg + geom_tile(color = "white", size = tile_border_size)
  }
  if(bw){
    gg <- gg + scale_fill_gradient(high = "black", low = "white")
  }else{
    gg <- gg + scale_fill_viridis(name = scale_name)
  }
  
  gg <- gg + coord_equal()
  #gg <- gg + facet_wrap(~type, ncol=2)
  gg <- gg + labs(x = NULL, y = NULL, title = title)
  gg <- gg + theme_tufte(base_size = base_size, base_family = "serif")
  gg <- gg + theme(axis.ticks = element_blank())
  if(xlabs){
    gg <- gg + theme(axis.text.x = element_text(size = text_size, angle = -60, hjust = 0))
  }
  else{
    gg <- gg + theme(axis.text.x = element_blank())
  }
  if(ylabs){
    gg <- gg + theme(axis.text.y = element_text(size = text_size))
  }
  else{
    gg <- gg + theme(axis.text.y = element_blank())
  }
  gg <- gg + theme(panel.border = element_blank())
  gg <- gg + theme(plot.title = element_text(hjust = 0))
  gg <- gg + theme(strip.text = element_text(hjust = 0))
  gg <- gg + theme(panel.spacing.x = unit(0.5, "cm"))
  gg <- gg + theme(panel.spacing.y = unit(0.5, "cm"))
  gg <- gg + theme(legend.title = element_text(size = text_size))
  gg <- gg + theme(legend.title.align = 1)
  gg <- gg + theme(legend.text = element_text(size = text_size))
  gg <- gg + theme(legend.position = "right")
  gg <- gg + theme(legend.key.size = unit(0.5, "cm"))
  gg <- gg + theme(legend.key.width = unit(.1, "cm"))
  return(gg)
}