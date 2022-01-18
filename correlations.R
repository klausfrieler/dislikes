library(tidyverse)

cmp_all_correlations <- function(data, method = "pearson", prefix = "DS"){
  cor_full <- data %>% 
    select(starts_with(prefix)) %>% 
    correlation::correlation(method = method) %>% 
    as.data.frame() %>% 
    mutate(p_adj = p.adjust(p), type = "full")

  cor_sub <- 
    map_dfr(unique(data$full_type), function(ft){
    data %>% 
      filter(full_type == ft) %>% 
      select(starts_with(prefix)) %>% 
      correlation::correlation(method = method) %>% 
      as.data.frame() %>% 
      mutate(p_adj = p.adjust(p), type = ft)
    
  })

 cor_all <- bind_rows(cor_full, cor_sub) %>% 
   as_tibble() 
 names(cor_all)[names(cor_all) == "rho"] <- "r"
 names(cor_all)[names(cor_all) == "tau"] <- "r"
 names(cor_all)[names(cor_all) == "S"] <- "t"
 names(cor_all)[names(cor_all) == "z"] <- "t"
 cor_all$df_error <- NULL
 cor_all <- cor_all %>% 
   select(var1 = Parameter1, var2 = Parameter2, r, -starts_with("CI"), -t, -Method, p, p_adj, type) %>% 
   mutate(cor_id = sprintf("%s - %s", var1, var2) %>% fashion_subscale_names()) 
   
 cor_all
}

get_sig_cors <- function(data,method = "pearson", alpha = .002, prefix = "DS"){
  cmp_all_correlations(data, method = method, prefix = prefix) %>% 
    filter(p_adj < alpha) %>% 
    group_by(cor_id) %>% 
    mutate(n_cond_cor = n()) %>% 
    ungroup()
  
} 
get_top_cors <- function(data, method = "pearson", alpha = .002, prefix = "DS", for_report = F){
  sig_cor <- get_sig_cors(data, method, alpha, prefix = prefix)
  top_cor <- sig_cor %>% filter(n_cond_cor == 5) 
  if(for_report){
    # cond_cor <-     top_cor %>% 
    #   pivot_wider(id_cols = c(var1, var2, cor_id, n_cond_cor), 
    #               names_from = type, 
    #               values_from = r, 
    #               names_prefix = "r") 
    top_cor <- top_cor %>% 
      group_by(cor_id) %>% 
      mutate(cor_range = sprintf("%.3f-%.3f", min(r), max(r)), cor_mean = round(mean(r),3)) %>% ungroup() %>% 
      arrange(desc(cor_mean)) %>%  
      distinct(cor_id, cor_mean, cor_range) %>% 
      set_names("Subscale Pair", "Range", "Mean")
  }
  top_cor
}
get_all_jennrich_tests <- function(data){
  full_types <- unique(data$full_type)
  map_dfr(full_types, function(ft1){
    map_dfr(full_types, function(ft2){
      if(ft2 < ft1 ){
        return(NULL)
      }
      tmp1 <- data %>% filter(full_type == ft1) %>%  select(starts_with("DS")) %>%  as.matrix()
      tmp2 <- data %>% filter(full_type == ft2) %>%  select(starts_with("DS")) %>%  as.matrix()
      ctj <- suppressWarnings(psych::cortest.jennrich(tmp1, tmp2))
      #browser()
      tibble(full_type1 = ft1, full_type2 = ft2, statistic = ctj$chi2, p = ctj$prob)
    })
  })
}

pbcor_glance <- function(pbtest){
  tibble(estimate = pbtest$cor, 
         statistic = pbtest$test, 
         p.value = pbtest$p.value, 
         parameter =  pbtest$n, 
         conf.low = pbtest$cor_ci[1],
         conf.high = pbtest$cor_ci[2],
         method = "pbcor",
         alternative = "two.sided"
  )
}

get_all_correlations <- function(data, 
                                 target_var = "arousal_mean", 
                                 predictors = c(fa_acoustic_preds, "tempo"), 
                                 save = F, 
                                 robust = T, 
                                 label_size = 4){
  map_dfr(predictors, function(p){
    if(robust){
      ct <- 
        WRS2::pbcor(data[[target_var]], data[[p]], ci = T) %>% 
        pbcor_glance()
    }
    else{
      ct <- 
        cor.test(data[[target_var]], data[[p]]) %>% 
        broom::glance()}
    ct <- ct %>%
      mutate(feature = p, target = target_var) %>% 
      select(-alternative, -method)
  })
}

get_all_correlations_by_group <-function(data, target_var = "DS_too_emotional", predictor_var = "DS_too_complex"){
  full_types <- unique(data$full_type)
  styles <- unique(data$style)
  map_dfr(full_types, function(ft){
    map_dfr(styles, function(st){
      get_all_correlations(master %>% filter(style == st), 
                           target_var = target_var, 
                           predictors = predictor_var) %>% 
        mutate(style = st, full_type = ft)
    })    
  }) %>% mutate(p_adj = p.adjust(p.value)) 
}