library(tidyverse)

mean_value_plot <- function(data, type = "style", max_val = 2, weighted = F){
  data <- data %>%  
    mutate(style = fct_reorder(style, MR_displeasure, mean)) %>% 
    select(starts_with("MR"), 
           style, 
           norm_style_count, 
           type, 
           degree) %>% 
    pivot_longer(-c(style, norm_style_count, type, degree)) %>% 
    group_by(style, type, degree, name, norm_style_count) %>% 
    summarise(m = mean(value), w_m = mean(value * norm_style_count), .groups = "drop") %>% 
    mutate(g = degree) %>% 
    mutate(name = str_remove(name, "^MR_") %>% 
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
    data <- data %>% mutate(m = w_m) 
    x_label <- "Weighted Mean"
  }
  q <- data %>% ggplot(aes(x = style, 
                           y = m, 
                           colour = degree, 
                           #linetype = type, 
                           #shape = g
                           )) 
  q <- q + geom_point(aes(size = (norm_style_count)^2))
  q <- q + facet_wrap( ~ name) 
  q <- q + geom_line(aes(group = g)) 
  q <- q + geom_hline(aes(yintercept = 0))  
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