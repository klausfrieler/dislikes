freq_table <- function(x, prop_var) {
  prop_var  <- enquo(prop_var)
  tmp <- x %>% 
    group_by( !!prop_var) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n /sum(n)) %>% 
    ungroup
  tmp
  #tmp %>% ggplot(aes_string(x = rlang::quo_text(prop_var), y= "freq")) + geom_col()
}

freq2_table <- function(x, group_var, prop_var) {
  group_var <- enquo(group_var)
  prop_var  <- enquo(prop_var)
  tmp <- x %>% 
    group_by(!!group_var, !!prop_var) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n /sum(n)) %>% 
    ungroup
  tmp
  #tmp %>% ggplot(aes_string(x = rlang::quo_text(prop_var), y= "freq")) + geom_col()
}

impute_mice <- function(data){
  impute <- mice::mice(data = data, m = 1, method = "pmm", maxit = 10, seed = 500)
  complete(impute, 1) %>% as_tibble()  
  
}

impute_median <- function(data, vars, method = "median"){
  for(v in vars){
    data[[v]][is.na(data[[v]])] <- median(data[[v]][!is.na(data[[v]])])
  }
  data
}

impute_all_vars <- function(data){
  impute_median(data, names(data)[str_detect(names(data), "[.]")])
}

get_sig_stars <- Vectorize(function(p_val){
  if(is.na(p_val)) return(NA)
  if(p_val <.001) return("***")
  if(p_val <.01) return("**")
  if(p_val <.05) return("*")
  return("")
  
})

comp_cor_mat_entries <- function(data){
  cor_mat <-  data %>% corrr::correlate()
  vars <- unique(cor_mat$term)
  map_dfr(vars, function(v1){
    map_dfr(vars, function(v2){
      if(v1 >= v2){
        return(NULL)
      }
      #browser()
      var_cor <- cor_mat %>% filter(term == v1) %>% pull(all_of(v2))
      row1 <- cor_mat %>% filter(term == v1) %>% select(-term, -all_of(c(v1, v2))) %>% t() %>% as.vector()
      row2 <- cor_mat %>% filter(term == v2) %>% select(-term, -all_of(c(v1, v2))) %>% t() %>% as.vector()
      cos_sim <- sum(row1 * row2)/(sqrt(sum(row1^2)))/sqrt(sum(row2^2))
      euclid_d <- sqrt(sum((row1 - row2)^2))
      max_d <- max(abs((row1 - row2)))
      tibble(var1 = v1, var2 = v2, var_cor = var_cor, cos_sim = cos_sim, d = euclid_d, max_d = max_d)
    })
  })
}
# analyse_lpa_classes <- function(data){
#   lpa_style_strong <- add_lpa_class(master, "style", "strong") %>% rename(lpa_class = lpa_class_style_strong)
#   lpa_style_slight <- add_lpa_class(master, "style", "slight") %>% rename(lpa_class = lpa_class_style_sligth)
#   lpa_artist_strong <- add_lpa_class(master, "artist", "strong") %>% rename(lpa_class = lpa_class_artist_strong)
#   lpa_artist_slight <- add_lpa_class(master, "artist", "slight") %>% rename(lpa_class = lpa_class_artist_sligth)
#   lpa_style_strong_means <- lpa_style_strong %>% 
#     pivot_longer(cols = starts_with("PC")) %>% 
#     group_by(lpa_class, name) %>% summarise(m = mean(value))
#   lpa_style_slight_means <- lpa_style_slight %>% 
#     pivot_longer(cols = starts_with("PC")) %>% 
#     group_by(lpa_class, name) %>% summarise(m = mean(value))
#   lpa_artist_strong_means <- lpa_artist_strong %>% 
#     pivot_longer(cols = starts_with("PC")) %>% 
#     group_by(lpa_class, name) %>% summarise(m = mean(value))
#   lpa_artist_slight_means <- lpa_artist_slight %>% 
#     pivot_longer(cols = starts_with("PC")) %>% 
#     group_by(lpa_class, name) %>% summarise(m = mean(value))
# }
fashion_subscale_names <- function(subscale_names){
  str_remove(subscale_names, "^DS_") %>% 
    str_replace_all("_", " ") %>% 
    str_to_title()
}
scale_definition_from_keys <- function(key_file = "data/keys_df.xlsx", sheet = "keys_v3"){
  key_tmp <- readxl::read_xlsx(key_file, sheet = sheet)
  factor_names <- key_tmp %>% select(-1) %>% names()
  map_dfr(factor_names, function(fn){
    #browser()
    items <- key_tmp %>%  filter(!!sym(fn) > 0)  %>% pull(rowname) %>% sort() 
    tibble(subscale = fn, n_items = length(items), items = items %>% paste(collapse = ", "))
  }) %>% mutate(subscale = fashion_subscale_names(subscale)) %>% 
    set_names(c("Subscale", "No. Items", "Items"))
}


get_optimal_factors <- function(data, plot = F){
  
  ev <- eigen(cor(data, use = "pairwise.complete.obs")) # get eigenvalues
  ap <- parallel(subject = nrow(data), var = ncol(data),
                 rep = 100, cent = .05)
  nS <- tryCatch(nScree(x = ev$values, aparallel = ap$eigen$qevpea), error = function(x) {
    list(Components = data.frame(noc = 1, naf = 1, nparallel = 1, nkaiser = 1))
  })
  if(plot){
    plotnScree(nS)
  }
  #print(nS$Components)
  return(nS$Components)
}

get_efa <- function(data, 
                    quest_type = "" , 
                    n_factors = 3, 
                    rotate = "oblimin",
                    method = c("principal", "fa"),
                    with_optimal = T, 
                    with_panels = F, 
                    only_factors = F){
  messagef("extracting efa for %s",quest_type)
  tmp <- 
    data %>% 
    select(where(is.numeric)) 
  if(any(str_detect("PC_", names(tmp)))){
    messagef("Warning: data already contains factors")
  }
  #browser()
  if(quest_type != ""){
    tmp <- tmp %>% select(starts_with(quest_type))
    stopifnot(nrow(tmp) > 0)
  }
  #browser()
  if(with_optimal){
    of <- get_optimal_factors(tmp, plot = F)
    n_factors <- of$nkaiser 
  }
  if(with_panels){
    psych::pairs.panels(tmp)
  }
  if(only_factors){
    return(n_factors)
  }
  #browser()
  kmo <- psych::KMO(tmp)
  bart <- psych::cortest.bartlett(tmp)
  #print(kmo)
  #print(bart)
  if(method[1] == "princpal"){
    psych::principal(tmp, n_factors, rotate = rotate)
  }
  else{
    psych::fa(tmp, n_factors, rotate = rotate)
    
  }
}