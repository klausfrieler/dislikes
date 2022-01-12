tidy_assoc_stats <- function(as){
  if(class(as) != "assocstats"){
    tryCatch({
      as <- vcd::assocstats(as)
    }, 
    error = function(e){
      browser()
      })  
    }
  as$chisq_tests %>% 
    as_tibble() %>% 
    set_names(c("statistic", "df", "p_value")) %>% 
    mutate(test = c("llr", "chisq")) %>% 
    pivot_wider(names_from = test, values_from = everything()) %>% select(-starts_with("test_")) %>%  
    
    bind_cols(tibble(phi = as$phi, contingency = as$contingency, cramers_v = as$cramer))
}

get_prop_table <- function(data, 
                           type = "style", 
                           degree = "strong", 
                           group = "gender", 
                           target_var = "style",
                           keep_levels = T,
                           output_format = "list"){
  tmp <- data
  if(nchar(type) > 0 ){
    tmp <- tmp %>% filter(type == !!type)
  }
  if(nchar(degree) > 0){
    tmp <- tmp %>% filter(degree == !!degree)
  }
  if(group == target_var){
    browser()
    return(NULL)
  }
  
  orig_target_var <- target_var
  orig_group <- group
  if(group == "lpa_class" || target_var == "lpa_class"){
    # tmp <- add_lpa_class(tmp, type, degree)
    # if(target_var == "lpa_class"){
    #   target_var <- sprintf("lpa_class_%s_%s", type, degree)    
    # }
    # if(group == "lpa_class"){
    #   group <- sprintf("lpa_class_%s_%s", type, degree)    
    # }
  }
  
  if(group == "gender"){
    tmp <- tmp %>% filter(gender != "D")
  }
  #browser()
  group_levels <- data[[group]] %>% as.factor() %>% levels()
  if(length(group_levels) == 0 || keep_levels == F){
    group_levels <- tmp[[group]] %>% as.factor() %>% levels()
  }
  target_levels <- data[[target_var]] %>% as.factor() %>% levels()
  if(length(target_levels) == 0 || keep_levels == F){
    target_levels <- tmp[[target_var]] %>% as.factor() %>% levels()
  }
  tryCatch(tab <- table(factor(tmp[[group]], group_levels), 
                        factor(tmp[[target_var]], target_levels)), 
           error = function(e){
             browser()
           })
  #browser()
  prop_tab_1 <- tab %>% prop.table(1) %>% round(2)
  prop_tab_2 <- tab %>% prop.table(2) %>% round(2)
  metadata <- tibble(type = type, degree = degree, group  = orig_group, target_var = orig_target_var)
  #browser()
  if(output_format == "list"){
    list(metadata = metadata,
         tab = tab, 
         prop_tab_row = prop_tab_1, 
         prop_tab_col = prop_tab_2, 
         chisq = vcd::assocstats(tab[, colSums(tab)!= 0]) %>% tidy_assoc_stats())
    
  }
  else if (output_format == "df"){
    prop_tab <- prop_tab_1
    tab_type <- "row_wise"
    if(str_detect(group, "lpa_class")){
      prop_tab <- prop_tab_2 
      tab_type <- "col_wise"
    }
    # prop_tab <- prop_tab %>% 
    #   t() %>% 
    #   as.data.frame() %>% 
    #   pivot_wider(id_cols = Var2, names_from = Var1, values_from = Freq) %>% 
    #   rename(group_values = Var2)
    prop_tab <- prop_tab %>% 
      t() %>% 
      as.data.frame() %>% 
      rename(group_value = Var1, target_value = Var2, freq = Freq) %>% 
      mutate(tab_type = tab_type)
    #browser()
    bind_cols(metadata = metadata, 
              chisq = vcd::assocstats(tab[, colSums(tab) != 0]) %>% tidy_assoc_stats(), 
              prop_tab)  %>% 
      mutate(id = sprintf("%s_%s_%s_%s", 
                          substr(type, 1,3), 
                          substr(degree, 1, 3), 
                          substr(group, 1, 3), 
                          substr(target_var, 1, 3))) %>% 
      select(id, everything())
  }
  else if (output_format == "plot"){
    ggassoc(tab, x_lab = group, y_lab = target_var, subtitle = sprintf("%s x %s (%s, %s)", group, target_var, type, degree))
  }
  else {
    stop(printf("Unrecognized output format: %s", output_format))
  }
}

get_all_contingency_tables <- function(data, add_lpa_class = T, output_format = "df", outdir = "plots"){
  types <- unique(data$type)
  degrees <- unique(data$degree)
  groups <- c("gender", "age_group", "lpa_class")
  target_vars <- c("style", "lpa_class")
  if(add_lpa_class){
    data <- map_dfr(types, function(ty){
      map_dfr(degrees, function(deg){
        add_lpa_class(data, ty, deg)  
      })
    })
    
  }
  
  map <- purrr::map
  map(types, function(ty){
    map(degrees, function(deg){
      map(groups, function(g){
        map(target_vars, function(tv){
          messagef("Testing type = '%s', degree = '%s', group = '%s', target = '%s'", ty, deg, g, tv)
          if(tv != g){
            q <- get_prop_table(data, 
                                type = ty, 
                                degree = deg, 
                                group = g, 
                                target_var = tv, 
                                output_format = output_format)
            if(output_format == "plot"){
              ggsave(sprintf("%s/%s_%s_%s_%s.png", outdir, ty, deg, g, tv), dpi = 500)
              return(NULL)
            }
            q
          }
        }) %>% set_names(target_vars)
      }) %>% set_names(groups)
    }) %>% set_names(degrees)
  }) %>% set_names(types)
}

get_chisq_stats <- function(data, group_var, target_var){
  types <- unique(data$type)
  degrees <- unique(data$degree)
  map_dfr(types, function(ty){
    map_dfr(degrees, function(deg){
      q <- get_prop_table(data, 
                          type = ty, 
                          degree = deg, 
                          group = group_var, 
                          target_var = target_var, 
                          output_format = "list")
      tibble(type = ty, degree = deg, group = group_var, target = target_var, 
             statistic = q$chisq$statistic_chisq,
             df = q$chisq$df_chisq,
             p_value = q$chisq$p_value_chisq,
             cramers_v = q$chisq$cramers_v 
      ) 
    })
    }) %>% 
    mutate(p_value_adj = p.adjust(p_value),
           sig = get_sig_stars(p_value_adj))

}

bootstrap_chisq <- function(data, cond_var, var, size = 1000){
  conditions <- unique(data[[cond_var]])
  data <- data %>% select(all_of(c(cond_var, var)))
  map_dfr(1:size, function(x){
    bs <- 
      map_dfr(conditions, function(cond){
        data %>% 
          filter(!!sym(cond_var) == cond) %>% 
          mutate(!!sym(var) := sample(!!sym(var), nrow(.), replace = T))
      })
    table(bs[[cond_var]], bs[[var]]) %>% tidy_assoc_stats()
  })  
}