variable_threshold_analysis <- function(data){
  var_thresh_analysis <- 
    map_dfr(1:7, function(thresh){
      map_dfr(num_vars, function(x){ 
        n_ids <- data %>% 
          filter(!!sym(x) >= thresh) %>% 
          count(p_id) %>% 
          nrow()
        base_n <- data %>% 
          filter(!is.na(!!sym(x))) %>% 
          nrow() 
        tibble(var = x, 
               thresh = thresh, 
               n_ids = n_ids, 
               base_n = base_n,
               f_ids = round(n_ids/base_n, 2))}) %>% 
        arrange(f_ids)
    }) 
  var_thresh_analysis
} 

get_var_stats <- function(x){
  x <- x[!is.na(x)]
  med <- median(x, na.rm = T)
  value <- "too extreme"
  if(med != min(x) && med != max(x)){
    value <- "okay"
  }
  not_extreme <- length(x[x != med ])
  tibble(median = med, 
         n = length(x),
         n_extreme = length(x[x == med]), r_extreme = length(x[x == med])/length(x), 
         n_not_extreme = length(x[x != med]), r_not_extreme = length(x[x != med])/length(x), 
         value = value)
}

check_concept_polarities <- function(data, var_map){
  concepts <- var_map %>% count(concept) %>% filter(n == 2) %>% pull(concept)
  map_dfr(concepts, function(conc){
    tmp <- var_map %>% filter(concept == conc) %>% pull(label)
    data %>% 
      select(all_of(tmp)) %>% 
      corrr::correlate() %>% 
      pivot_longer(-term) %>% 
      mutate(concept = conc) %>% filter(term != name)
  })
}

check_rater_extremity <- function(data){
  data <- data %>% select(p_id, type, degree, all_of(num_vars))
  classify_rating <- Vectorize(function(x){
    if(is.na(x)) NA
    else if(x < 3) "low"
    else if(x > 3) "high"
    else if(x == 3) "medium"
  })
  stats <- data %>% pivot_longer(-c(p_id, type, degree)) %>% mutate(rating_class = classify_rating(value))
  browser()
  stats %>% 
    group_by(p_id, type, degree) %>% 
    summarise(n = sum(!is.na(value)), 
              high_rate = sum(rating_class == "high", na.rm = T)/n,
              low_rate = sum(rating_class == "low", na.rm = T)/n, 
              l = log(high_rate/low_rate + .01), 
              .groups = "drop")  
  
}

check_rater_polarities <- function(data, var_map){
  polarities <- var_map %>% count(concept) %>% filter(n > 1) %>% pull(concept)
  n_ratings <- data %>% count(p_id)
  
  map_dfr(polarities, function(pol){
    vars <- var_map %>% filter(concept == pol) %>% pull(label)
    stopifnot(length(vars) == 2)
    paradox_raters <- data %>% filter(!!sym(vars[1]) == !!sym(vars[2])) %>% pull(p_id)
    tibble(concept = pol, 
           var1 = vars[1], 
           var2 = vars[2], 
           p_id = paradox_raters)
  }) %>% group_by(p_id) %>% 
    mutate(n_paradoxes = n()) %>% 
    ungroup() %>% 
    left_join(n_ratings, by = "p_id") %>% 
    mutate(r_paradox = n_paradoxes/n/length(polarities))
}

check_polarities <- function(data, var_map){
  polarities <- var_map %>% count(concept) %>% filter(n > 1) %>% pull(concept)
  map_dfr(polarities, function(pol){
    vars <- var_map %>% filter(concept == pol) %>% pull(label)
    stopifnot(length(vars) == 2)
    n_paradox <- data %>% filter(!!sym(vars[1]) == !!sym(vars[2])) %>% nrow()
    n_okay <- data %>% filter(!!sym(vars[1]) != !!sym(vars[2]))%>% nrow()
    mean_diff <- data %>% mutate(d = abs(!!sym(vars[1]) - !!sym(vars[2]))) %>% pull(d) %>% mean(na.rm = T)  
    total <- n_paradox + n_okay
    tibble(concept = pol, 
           var1 = vars[1], 
           var2 = vars[2], 
           mean_abs_diff = mean_diff,
           n_paradox = n_paradox,
           n_okay = n_okay, 
           total = total, 
           r_paradox = n_paradox/total,
           r_okay = n_okay/total)
    
  })
}

check_original_factor_solution <- function(data, 
                                           exclude_vars = NULL,
                                           imputation_type = c("mice", "no", "median"),
                                           n_factors = 8,
                                           with_optimal = T){
  set.seed(666)
  orig_data <- data
  data <-  data %>% 
    select(-starts_with("PC")) %>% 
    select(-starts_with("reaction")) %>% 
    select(-contains("style"))%>% 
    select(-all_of(exclude_vars))
  if(imputation_type[1] == "mice"){
    impute <- mice::mice(data = data, m = 1, method = "pmm", maxit = 10, seed = 500)
    data <- complete(impute, 1) %>% as_tibble()  
  }
  if(imputation_type[1] == "median"){
    data <- impute_all_vars(data)
  }
  print(nrow(data %>% select(where(is.numeric)) %>% select(-starts_with(c("reaction")))))
  print(length(data %>% select(where(is.numeric)) %>% select(-starts_with(c("reaction")))))
  efa <- get_efa(data %>% select(where(is.numeric)) %>% select(-starts_with(c("reaction"))), 
                 n_factors = n_factors, with_optimal = with_optimal, with_panels = F, rotate = "oblimin") 
  n_factors <- dim(efa$scores)[2]
  combined <- bind_cols(orig_data, efa$scores %>% as.data.frame()) %>% select(starts_with("TC"), starts_with("PC"))
  #browser()
  corr <- corrr::correlate(combined)
  fac_names <- sprintf("TC%d", 1:n_factors)
  factor_map <- 
    map_dfr(fac_names, function(x){
      #tmp <- corr %>% corrr::focus(!!sym(x)) %>% filter(!!sym(x) == max(!!sym(x)))
      #browser()
      corr %>% corrr::focus(!!sym(x)) %>% 
        top_n(2) %>% 
        arrange(desc(!!sym(x))) %>%
        mutate(rank = 1:n(), new_factor = x)  %>% 
        rename(old_factor = term, corr = !!sym(x))  
    }) %>% 
    select(new_factor, old_factor, corr, rank)  
  browser() 
  top2 <- factor_map  %>% 
    select(new_factor, old_factor, rank) %>% 
    pivot_wider(id_cols = new_factor, names_from = rank, values_from = old_factor, names_prefix = "r") %>%
    mutate(old_factors = sprintf("%s;%s", r1, r2)) %>% select(new_factor, old_factors)
  efa_ana <- analyse_efa(data, efa) %>% left_join(top2, by = "new_factor")
  list(efa = efa, 
       data = bind_cols(orig_data, efa$scores %>% as.data.frame()), 
       factor_map = factor_map,
       compare_df = combined, 
       efa_ana = efa_ana)
}

analyse_efa <- function(data, efa, prefix = "TC"){
  n_factors <- dim(efa$scores)[2]
  tmp <- bind_cols(data, efa$scores %>% as.data.frame()) %>% 
    select(-contains("style")) 
  cor_mat <-  tmp %>% select(where(is.numeric)) %>% corrr::correlate()
  fac_names <- sprintf("%s%d", prefix, 1:n_factors)
  factor_map <- 
    map_dfr(fac_names, function(x){
      #browser()
      cor_mat %>% corrr::focus(!!sym(x)) %>% 
        filter(!str_detect(term, prefix)) %>% 
        top_n(10) %>% 
        arrange(desc(!!sym(x))) %>%
        mutate(rank = 1:n(), new_factor = x)  %>% 
        rename(corr = !!sym(x))  
    }) %>% 
    select(new_factor, term, corr, rank)  
  factor_map
}


simulate_degenerate_vars <-function(x, y, range = 1:5, low = 1:2){
  #browser()
  x[is.na(x)] <- median(x, na.rm = T)
  y[is.na(y)] <- median(y, na.rm = T)
  x.low_end <-  x[x <= max(low)]
  x.high_end <- x[x > max(low)]
  y.low_end <-  y[y <= max(low)]
  y.high_end <- y[y > max(low)]
  #browser()
  print(tibble(x = x, y = y) %>% correlation::correlation())
  print(tibble(x = x, y = y) %>% filter(x <= max(low), y <= max(low)) %>% correlation::correlation())
  print(tibble(x = x, y = y) %>% filter(x > max(low), y > max(low)) %>% correlation::correlation())
  x_low_sim <- sample(unique(x.low_end), size = length(x.low_end), prob = prop.table(table(c(y.low_end))), replace = T)
  x_high_sim <- sample(x.high_end, length(x.high_end), replace = T)
  y_low_sim <- sample(unique(y.low_end), size = length(y.low_end), prob = prop.table(table(c(x.low_end))), replace = T)
  y_high_sim <- sample(y.high_end, length(y.high_end), replace = T)
  ret <- list(x = c(x_low_sim, x_high_sim), y = c(y_low_sim, y_high_sim))
  #browser()
  orig_ct <- cor(x, y)
  sim_ct <- cor(ret$x, ret$y)
  list(orig_ct = orig_ct, sim_ct = sim_ct)
}