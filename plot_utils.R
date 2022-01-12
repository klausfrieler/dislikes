library(tidyverse)

mean_value_plot <- function(data, type = "style", max_val = 7, weighted = F, scale = T){
  y_intercept <- 3
  #data <- data %>% filter(type == !!type)
  if(scale || weighted){
    data <- data %>% mutate(across(starts_with("DS"), ~{scale(.x) %>% as.vector()})) 
    y_intercept <- 0
  }
  data <- data %>%  
    mutate(order_var = DS_displeasure * norm_style_count) %>% 
    mutate(style = fct_reorder(style, order_var, mean)) %>% 
    select(starts_with("DS"), 
           style, 
           norm_style_count, 
           type, 
           degree) %>% 
    pivot_longer(-c(style, norm_style_count, type, degree)) 
  data <- data %>%   
    group_by(style, type, degree, name, norm_style_count) %>% 
    summarise(m = mean(value), 
              w_m = mean(value * norm_style_count), 
              s = se(value), 
              w_s = se(value * norm_style_count), .groups = "drop") %>% 
    mutate(g = degree) %>% 
    mutate(name = str_remove(name, "^DS_") %>% 
             str_replace_all("_", " ") %>% 
             str_to_title()) %>% 
    #mutate(g = sprintf("%s_%s", type, degree)) %>% 
    filter(type == !!type,  
           abs(m) < max_val) 
  
  if(type == "style"){
    data <- data %>% filter(style != "NA")
  }
  x_label <- "Mean"
  if(weighted){
    data <- data %>% mutate(m = w_m, s = w_s) 
    x_label <- "Weighted Mean"
  }
  q <- data %>% ggplot(aes(x = style, 
                           y = m, 
                           colour = degree, 
                           #linetype = type, 
                           #shape = g
                           )) 
  q <- q + geom_point(aes(size = (norm_style_count)^2))
  q <- q + geom_errorbar(aes(ymin = m - 1.96*s, ymax = m + 1.96*s), width = .5)
  q <- q + facet_wrap( ~ name) 
  q <- q + geom_line(aes(group = g)) 
  q <- q + geom_hline(aes(yintercept = y_intercept))  
  q <- q + coord_flip() 
  q <- q + theme_bw() 
  q <- q + theme(legend.position = "right", legend.text = element_text(size = 12))
  #q <- q + theme(legend.position = c(.94, .1), legend.text = element_text(size = 12))
  q <- q + guides(size = "none")
  q <- q + guides(shape = "none")
  q <- q + labs(x = "", y = x_label, title = sprintf("Type: %s %s", str_to_title(type), ifelse(weighted, "(weighted)", "")))
  q <- q + scale_color_manual(values = c("black", "red"), name = "")#stringr::str_to_title(type))
  q
}

distribution_by_style_and_var <- function(data, var_name, title = var_name){
  if(is.null(title)){
    title <- new_factor_map[new_factor_map$new_factor == var_name,]$new_name
  }
  data <- data %>%  
    select(all_of(var_name), style) %>% 
    pivot_longer(-style) %>% 
    mutate(style = fct_reorder(style, value, median)) 
  q <- data %>% ggplot(aes(x = value, y = ..count..)) 
  q <- q + geom_histogram(color = "black", fill = "indianred") 
  q <- q + facet_wrap(~style, scales = "free_y") 
  q <- q + labs(title = title)
  q <- q + theme_bw()
  q
}

distribution_by_participant <- function(data, p_id, var_name, title = var_name){
  if(is.null(title)){
    title <- new_factor_map[new_factor_map$new_factor == var_name,]$new_name
  }
  data <- data %>%  
    select(all_of(var_name), style) %>% 
    pivot_longer(-style) %>% 
    mutate(style = fct_reorder(style, value, median)) 
  q <- data %>% ggplot(aes(x = value, y = ..count..)) 
  q <- q + geom_histogram(color = "black", fill = "indianred") 
  q <- q + facet_wrap(~style, scales = "free_y") 
  q <- q + labs(title = title)
  q <- q + theme_bw()
  q
}

panels_plot <- function(data, vars, group_var, base_size = 14){
  layout <- theme_bw(base_size = base_size) +
    theme(plot.margin = unit(c(.33, .33, .33, .33), "cm"), 
          strip.background = element_rect(fill = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
    )
  if(is.null(group_var) || nchar(group_var) == 0){
    data <- data %>%
      dplyr::select(all_of(c(vars))) %>% set_names(fashion_subscale_names(names(.)))
    q <-   data %>% GGally::ggpairs(
      aes(alpha = 0.0001
      ),
      #method = "spearman",
      showStrips = T,
      progress = T
    ) 
      
  }
  else{
    data <- data %>%
      dplyr::select(all_of(c(vars, group_var))) 
  q <-  data %>% GGally::ggpairs(
      aes(color = {{group_var}}, alpha = 0.001, fill = {{group_var}}),
      #method = "spearman",
      showStrips = T,
      progress = T
    ) 
      
  }
  
  q <- q +   
    scale_color_manual(values = c("lightblue4", "darkgoldenrod4", "lightblue4", "darkgoldenrod4")) +
    scale_fill_manual(values = c("lightblue4", "darkgoldenrod4", "lightblue4", "darkgoldenrod4")) +
    labs(
      title = "Correlations",
      x = "",
      y = "",
      color = "Legend"
    ) +
    layout +
    theme(axis.title.y = element_text(size = 10),
          axis.title.x = element_text(size = 10))
  q
}