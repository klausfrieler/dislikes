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
    mutate(p_value_adj = (p_value),
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


get_style_stats <- function(data, for_report = F){
  style_stats <- data %>% 
    freq2_table(full_type, style ) %>% 
    arrange(full_type, desc(freq)) %>%
    pivot_wider(id_cols = style, 
                values_from = c(n, freq), 
                names_from = full_type, 
                values_fill = 0) %>% 
    mutate(
      total_artist = n_artist_strong + n_artist_slight, 
      total_style = n_style_slight + n_style_strong,
      total_artist_col = sum(total_artist),
      total_style_col = sum(total_style),
      total1 = total_artist + total_style,
      total_freq_artist = round(100*total_artist/total_artist_col, 1), 
      total_freq_style = round(100*total_style/total_style_col, 1),
      total = total_artist + total_style) %>%
    select(-c(total1, total_artist_col, total_style_col)) %>% 
    arrange(desc(total)) 
  if(for_report){
    colSums <- style_stats %>% select(-1) %>% colSums() %>% t() %>% as_tibble() %>% mutate(style = "Sum") %>% 
      select(style, everything())
    style_stats <- bind_rows(style_stats, colSums) %>% 
      mutate(across(starts_with("freq_"), to_perc))
    
    style_stats <- style_stats %>% 
    set_names(c("Style", 
                "Count (Artist/Slight)", 
                "Count (Artist/Strong)", 
                "Count (Style/Slight)", 
                "Count (Style/Strong)", 
                "Perc. (Artist/Slight)", 
                "Perc. (Artist/Strong)", 
                "Perc. (Style/Slight)", 
                "Perc. (Style/Strong)",
                "Count (Artist)",
                "Count (Style,)",
                "Perc. (Artist)",
                "Perc. (Style)",
                "Count"))
  } else{
    style_stats <- style_stats %>% mutate(across(starts_with("freq_"), to_perc))
  }       

  style_stats 
}

get_binary_style_judgments <- function(data, type  = "style", degree = "", threshold = 3, alpha = .01, with_mixed = T){
  data <- data %>%  filter(style != "NA") 
  if(nchar(type) > 0){
    data <- data %>% filter(type == !!type)
  }
  if(nchar(degree) > 0){
    data <- data %>% filter(degree == !!degree)
  }
  
  data <- data %>% 
    select(style, starts_with("DS")) %>% 
    pivot_longer(-style) %>% 
    mutate(value = factor(value > threshold, labels = c("no", "yes")))
  styles <- unique(data$style)
  vars <- unique(data$name)

  map_dfr(styles, function(st){
    data <- data %>% filter(style == st)
    map_dfr(vars, function(v){
      data <- data %>% filter(name == v)
      tab <- sort(table(data$value), decreasing = T) 
      chisq <- suppressWarnings(tab %>% chisq.test())
      applies <- names(tab)[1]
      if(with_mixed && chisq$p.value >= alpha){
        applies <- "mixed"
      }
      #browser()
      tibble(style = st, 
             var = v, 
             applies = applies, 
             maj_n = tab[1], 
             min_n = tab[2], 
             n = sum(tab), 
             log2_BF = log2(tab[1]/tab[2]))
    })
  }) %>% mutate(var = fashion_subscale_names(var))
}
get_all_binary_style_judgements <- function(data, threshold = 3, alpha = .01, with_mixed = T){
    map_dfr(unique(data$type), function(ty){
      map_dfr(unique(data$degree), function(deg){
        get_binary_style_judgments(data, 
                                   type = ty, 
                                   degree = deg, 
                                   alpha = alpha, 
                                   threshold = threshold, 
                                   with_mixed = with_mixed) %>% mutate(type = ty, 
                                                                       degree = deg)
      })
    }) 
  
}

binary_style_judgments_plot <- function(data, threshold = 3, alpha = .01, with_mixed = T, only_agreement = F){
  pallette <- c("grey", "black", "red")#
  if(!with_mixed){
    pallette <- c("black", "red")#
  }
  plot_data <- get_all_binary_style_judgements(data, threshold, alpha, with_mixed) 
  if(only_agreement){
    plot_data <- plot_data %>% 
      group_by(style, var) %>% 
      summarise(div = length(table(applies)), 
                applies = names(table(applies))[which.max(table(applies))] ) %>% 
      filter(div == 1) %>% 
      mutate(Applies = factor(applies))
  }
  else {
    plot_data <- plot_data %>% 
      mutate(Applies = factor(applies), type = str_to_title(type), degree = str_to_title(degree))
  }
  q <- plot_data %>% ggplot(aes(x = style, y = var, fill = Applies)) 
  q <- q + geom_tile() 
  q <- q + coord_flip() 
  if(!only_agreement){
    q <- q + facet_grid(type ~degree)
  }
  q <- q + theme_bw()
  q <- q + theme(panel.grid.major =  element_blank(), panel.grid.minor = element_blank(), 
                 strip.background = element_rect(fill = "white"), 
                 axis.text.x = element_text(angle = 45, hjust = 1))
  q <- q + scale_fill_manual(values = pallette)
  q <- q + labs(x = "", y = "")
  q
}

get_kripp_by_style <- function(data, type  = "style", degree = "strong", vars = NULL ){
  data <- data %>%  filter(style != "NA") 
  if(nchar(type) > 0){
    data <- data %>% filter(type == !!type)
  }
  if(nchar(degree) > 0){
    data <- data %>% filter(degree == !!degree)
  }
  styles <- unique(data$style)
  if(is.null(vars) || all(nchar(vars) == 0)){
    vars <- names(data %>% select(starts_with("DS")))
  }
  map_dfr(styles, function(st){
    tmp <- data %>% filter(style == st) %>% select(all_of(vars)) %>% as.matrix()
    ka <- tryCatch({
      tmp  %>% irr::kripp.alpha(method = "interval")},
      error = function(e){
        messagef("No kripp.alpha for %s!", st)
        ka <- list(value = NA)
      })
    tibble(type = type, degree = degree, style = st, ka = ka$value, n_raters = nrow(tmp))
  })
}

get_all_kripp <- function(data){
  type_ka <- data %>% get_kripp_by_style(degree = "") %>% mutate(full_type = "type")
  degree_ka <- data %>% get_kripp_by_style(type = "") %>% mutate(full_type = "deg")
  full_ka <- data %>% get_kripp_by_style(type = "", degree = "") %>% mutate(full_type = "full")
  map_dfr(unique(data$type), function(ty){
    map_dfr(unique(data$degree), function(deg){
      get_kripp_by_style(data, ty, deg) %>% mutate(full_type = sprintf("%s_%s", ty, deg))
    })
  }) %>% 
    select(type, degree, style, full_type, n_raters, ka) %>% 
    bind_rows(type_ka, degree_ka, full_ka)
}

kripp_alpha_style_judgments_plot <- function(all_kas){
  pallette <- c("grey", "black", "red")#
  plot_data <- all_kas %>% 
    filter(str_detect(full_type, "_"), !is.na(ka)) %>% 
    mutate(full_type = fct_reorder(fashion_subscale_names(full_type) %>% str_replace( " ", "/") 
                                   , ka, mean) %>% fct_rev(), 
           style =  fct_reorder(style, ka, mean),
           label = sprintf("%.2f (%d)", ka, n_raters),#
           label_color = factor(ka >.0),
           `Krippendorff's Alpha` = ka) 
  #browser()
  q <- plot_data %>% ggplot(aes(x = style, y = full_type, fill = `Krippendorff's Alpha`)) 
  q <- q + geom_tile() 
  q <- q + geom_text(aes(label = label, color = label_color)) 
  q <- q + coord_flip() 
  q <- q + theme_bw()
  q <- q + theme(panel.grid.major =  element_blank(), panel.grid.minor = element_blank(), 
                 strip.background = element_rect(fill = "white"))
  q <- q + viridis::scale_fill_viridis()
  q <- q + scale_color_manual(values = c("white", "black"), guide = "none")
  q <- q + labs(x = "", y = "")
  q
}

report_chisq_test <- function(tab, in_brackets = F, effsize = c("cramer"), test = c("Pearson")){
  stats <- tab %>% vcd::assocstats()
  eff_size_labels <- c("cramer" =  "Cramer's V")
  test_label <- c("X^2" = "Chi^2")
  ret <- sprintf("Chi-squared(%.0f) = %.2f, p = %.3f, %s = %.2f", 
          stats$chisq_tests[,"df"][["Pearson"]], 
          stats$chisq_tests[,"X^2"][["Pearson"]],
          stats$chisq_tests[,"P(> X^2)"][["Pearson"]],
          eff_size_labels[effsize],
          stats[[effsize]]
          )
  if(in_brackets){
    ret <- sprintf("(%s)", ret)
  }
  ret
}