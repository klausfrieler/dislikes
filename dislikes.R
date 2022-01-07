library(tidyverse)
library(tidyLPA)
library(nFactors)
library(psych)
source("ggassoc.R")
source("super_heatmap.R")
source("plot_utils.R")

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

factor_map <- c("PC_overload" = "PC_too_avant", 
                "PC_deficiency" = "PC_too_boring", 
                "PC_displeasure" = "PC_displeasure",
                "PC_complexity" = "PC_too_complex",
                "PC_simplicity"= "PC_too_mainstream",
                "PC_social" = "PC_social_rej",
                "PC_soft" = "PC_too_fem", 
                "PC_energetic" = "PC_too_masc")

lpa_class_labels <- tribble(~type, ~degree, ~class1, ~class2, 
                            "style",  "strong", "highbrow", "lowbrow",
                            "style",  "slight",  "lowbrow", "highbrow",
                            "artist", "strong", "highbrow", "lowbrow", 
                            "artist", "slight", "lowbrow", "highbrow"
)
education_labels <- c("in school", 
                      "without degree", 
                      "primary", "o-levels", 
                      "a-levels", "a-levels", 
                      "bachelor", 
                      "master", 
                      "promotion", 
                      "other")
job_labels <- c("employed", "self-employed", "student", "unemployed", "retired", "other")
bad_vars <- c("emo.too_expressive", 
              "lyrics.too_complex", 
              "lyrics.too_realistic", 
              "music.disliked_instruments", 
              "music.too_chaotic", 
              "music.too_complex", 
              "music.too_dissonant", 
              "music.too_fast", 
              "music.too_loud", 
              "music.too_mainstream", 
              "music.too_melodious", 
              "music.too_much_change", 
              "music.too_niche", 
              "music.too_rhythmic", 
              "music.too_slow", 
              "music.too_soft", 
              "music.too_unrhythmic", 
              "music.too_variable", 
              "social.bad_experiences", 
              "social.not_peer_approved", 
              "social.too_often_heard")
bad_vars2 <- setdiff(bad_vars, c("music.disliked_instruments", 
                                 "music.too_dissonant", 
                                 "music.too_mainstream", 
                                 "music.too_niche", 
                                 "music.too_unrhythmic", 
                                 "music.too_loud"))
                                 
style_labels <- c("12" = "Schlager",
                  "14" = "Traditional",
                  "10" = "HipHop",
                  "9"  = "Pop",
                  "8"  = "Metal",
                  "4"  = "Techno",
                  "11" = "Rock",
                  "3"  = "EDM",
                  "6"  = "Jazz",
                  "2"  = "Country",
                  "15" = "Reggae",
                  "16" = "NA",
                  "7"  = "Classical",
                  "5"  = "House",
                  "1"  = "Blues",
                  "13" = "World")
gender_labels <- c("F", "M", "D")

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
  if(method == "princpal"){
    psych::principal(tmp, n_factors, rotate = rotate)
  }
  else{
    psych::fa(tmp, n_factors, rotate = rotate)
    
  }
}

read_var_map <- function(fname = "data/var_map.csv"){
  read.csv(fname, sep = ";", header =T, stringsAsFactors = F) %>% 
    as_tibble() %>% 
    mutate(code = str_replace(code, "^_", ""))
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
read_data <- function(fname = "data/dislikes_data_merrill.xlsx"){
  data <- readxl::read_xlsx(fname)
  data$p_id <- as.character(data$p_id)
  data$job <- as.character(data$job)
  data$grade <- as.character(data$grade)
  data$education <- as.character(data$education)
  data$age <- as.character(data$age)
  data$style <- style_labels[as.character(data$style)]
  data$gender <- gender_labels[as.integer(data$gender)]
  data$age_group <- factor(data$age <= 30, labels = c(">30", "<=30"))
  data %>% filter(gender != "D")
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
add_style_counts <- function(data){
  data %>% 
    group_by(type, degree, style) %>% 
    mutate(style_count = n())  %>% 
    ungroup() %>% 
    group_by(type, degree) %>% 
    mutate(norm_style_count = style_count/n()) %>% 
    ungroup()
}

setup_workspace <- function(reload = T, add_to_global_env = F){
  if(!reload){
    master <- readRDS("data/master.rds")
  }
  else{
    master <- read_data()
  }
  if(add_to_global_env)assign("master_raw", master, globalenv())
  before <- n_distinct(master$p_id)
  master <- filter_bad_raters(master)
  after <- n_distinct(master$p_id)
  messagef("Excluded %d bad raters (%d -> %d)", before -after, before, after)
  var_map <- read_var_map()
  labels <- var_map$label
  names(labels) <- var_map$code
  names(master)[names(master) %in% names(labels)] <- labels
  #browser()
  master$education <- education_labels[as.numeric(master$education)]
  master$job <- job_labels[as.numeric(master$job)]
  master <- add_alternate_factors(master, NULL)   
  master_f5 <- add_alternate_factors(master, bad_vars2)
  browser()  
  master <- add_style_counts(master)
  master_f5 <- add_style_counts(master_f5)
  if(add_to_global_env){
    assign("master", master, globalenv())
    assign("master_f5", master_f5, globalenv())
  }
  return(master)
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
get_rating_stats <- function(ratings, thresh = .9){
  #vector of raw ratings with values 1-5
  #browser()
  if(length(ratings) == 0 || all(is.na(ratings))){
    return(NA)
  }
  ratings_f <- factor(ratings, levels = 1:5)
  tab <- table(ratings_f)
  ratings_g <- fct_collapse(ratings_f, extreme_low = c("1"), extreme_high = c("5"), med = c("2", "3", "4"))
  tab_g  <- table(ratings_g) %>% prop.table()
  if(tab_g[["extreme_low"]] >= thresh){
    return("polarizer_low")
  }
  if(tab_g[["extreme_high"]] >= thresh){
    return("polarizer_high")
  }
  if((tab_g[["extreme_high"]] + tab_g[["extreme_low"]]) >= thresh){
    return("polarizer_high")
  }
  "moderato"
}

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
  
  } 
  var_thresh_analysis
filter_bad_raters <- function(data){
  bad_ids <- data %>%  
    select(p_id, type, degree, where(is.numeric), -starts_with("PC")) %>% 
    pivot_longer(-c(p_id, type, degree)) %>% 
    group_by(p_id, type, degree) %>% 
    summarise(rating_type = get_rating_stats(value)) %>% 
    ungroup() %>% 
    group_by(p_id) %>% 
    summarise(rt = sum(str_detect(rating_type, "pola")), n = n()) %>% 
    filter(rt == n, rt > 0, n > 2) %>% 
    pull(p_id)
  data %>% filter(!(p_id %in% bad_ids))
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

impute_mice <- function(data){
  impute <- mice::mice(data = data, m = 1, method = "pmm", maxit = 10, seed = 500)
  complete(impute, 1) %>% as_tibble()  
  
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

add_alternate_factors <- function(data, 
                                  exclude_vars = bad_vars2,  
                                  method = "principal",
                                  imputation_type = c("mice", "no", "median")){
  set.seed(666)
  data <-  data %>% 
    select(-starts_with("PC")) %>% 
    select(-all_of(exclude_vars)) 
  if(imputation_type[1] == "mice"){
    impute <- mice::mice(data = data, m = 1, method = "pmm", maxit = 10, seed = 500)
    data <- complete(impute, 1) %>% as_tibble()  
  }
  if(imputation_type[1] == "median"){
    data <- impute_all_vars(data)
  }
  efa <- get_efa(data %>% 
                   select(where(is.numeric)) %>% 
                   select(-starts_with("reaction"), -contains("style")),
                 method = method,
                 n_factors = 6, with_optimal = T, with_panels = F, rotate = "oblimin") 
  #browser()
  n_factors <- dim(efa$scores)[2]
  if(n_factors == 4){
    factor_names <- c("PC_too_simple", "PC_displeasure", "PC_too_little_impact", "PC_social")
  }
  else if(n_factors == 5){
    #efa_ana <- analyse_efa(data, efa)
    #browser()
    factor_names <- c("PC_too_simple",  "PC_too_niche", "PC_displeasure", "PC_social", "PC_too_little_impact")
  }
  else if(n_factors == 8){
    tmp <- readxl::read_xlsx("efa_ana.xlsx") %>% 
      distinct(new_factor, new_name)  
    #factor_order <- (efa$loadings %>% dimnames())[[2]]
    #factor_names <- tmp$new_name
    #names(factor_names) <- tmp$new_factor
    #factor_names <- factor_names[factor_order] %>% as.vector()
    efa_ana <- analyse_efa(data, efa, "MR")
    factor_names <- dimnames(efa$scores)[[2]]
    # factor_names <- c("PC_too_avantgarde", 
    #                   "PC_no_expression", 
    #                   "PC_complexity",
    #                   "PC_displeasure", 
    #                   "PC_social", 
    #                   "PC_too_commercial", 
    #                   "PC_too_whimpy", 
    #                   "PC_too_karneval")
    browser()
  }
  else{
    messagef("Factor names only for 4, 5 or 8 factors, you got: %s", n_factors)
    return(bind_cols(data, efa$scores %>%  as_tibble())) 
  }
  bind_cols(data, 
            efa$scores %>% 
              as_tibble() %>% 
              set_names(factor_names))  
}
add_cluster_factors <- function(data, nclusters = 8){
  keys <- data %>% 
    select(all_of(num_vars)) %>% 
    impute_mice() %>% 
    cor() %>% 
    ICLUST(nclusters = nclusters, plot = F) %>% 
    cluster2keys() %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble()
  factors <- 
    map_dfc(names(keys %>% select(-rowname)), function(k){
    rn <- keys %>% filter(!!sym(k) != 0) %>% pull(rowname)
    browser()
    tibble(!!sym(k) := data[rn] %>% rowSums(na.rm = T)/length(rn))
  }) 
  bind_cols(data, factors)
}

tidy_assoc_stats <- function(as){
  as$chisq_tests %>% 
    as_tibble() %>% 
    set_names(c("statistic", "df", "p_value")) %>% 
    mutate(test = c("llr", "chisq")) %>% 
    pivot_wider(names_from = test, values_from = everything()) %>% select(-starts_with("test_")) %>%  
  
  bind_cols(tibble(phi = as$phi, contingency = as$contingency, cramers_v = as$cramer))
}

add_lpa_class <- function(data, type = "style", degree = "strong", use_same_name = F, n_classes = 2){
  if(use_same_name){
    var_name <- "lpa_class"
  }
  else{
    var_name <- sprintf("lpa_class_%s_%s", type, degree)
  }
  #browser()
  if(var_name %in% names(data)){
    messagef("Found %s in data", var_name)
    return(data)
  }
  
  if(!is.null(type) & nchar(type) > 0){
    data  <-  data %>% filter(type == !!type) 
    
  }
  if(!is.null(degree) & nchar(degree) > 0){
    data  <-  data %>% filter(degree == !!degree) 
    
  }
  labels  <- lpa_class_labels %>% filter(type == !!type, degree == !!degree) 
  lpa <- data %>%  select(starts_with("MR")) %>% estimate_profiles(n_classes)
  plot_profiles(lpa, add_line = T, rawdata = F) + coord_flip()
  browser()
  first_PC <- names(data)[str_detect(names(data), "^MR_")][1] 
  tmp <- bind_cols(data, get_data(lpa) %>% select(Class))
  # tmp <- get_data(lpa) %>% left_join(data %>% 
  #                                      select(starts_with(first_PC), 
  #                                             gender, p_id, style, type, degree, age_group, education, job), 
  #                                    by = c("first_PC", "gender", "p_id", type, degree, age_group)) 
  if(nrow(labels) > 0){
    tmp <- tmp %>% 
      mutate(lpa_class = factor(Class, labels = c(labels$class1, labels$class2)))
  }
  else{
    tmp <- tmp %>% 
      mutate(lpa_class = factor(Class))
    
  }
  check_stats <- tmp %>% group_by(lpa_class) %>% summarise(across(starts_with("MR_"), mean))
  check_stats_t <- check_stats %>% 
    pivot_longer(-lpa_class) %>% 
    pivot_wider(id_cols = name, names_from = lpa_class, values_from = value, names_prefix = "lpa_class") 
  messagef("Type = %s, degree = %s", type, degree)
  print(check_stats_t)
  #if(check_stats$PC_complexity[1] > check_stats$PC_complexity[2]){
  #  tmp <- tmp %>% mutate(lpa_class = factor(lpa_class, labels = levels(lpa_class)[c(2,1)]))
  #}
    
  tmp %>% rename(!!var_name := lpa_class) %>% select( -Class)
}

add_all_lpa_classes <- function(data){
  map_dfr(data %>% group_split(type, degree), function(x){
    add_lpa_class(x, type = unique(x$type), degree = unique(x$degree), use_same_name = T)
  })
}

get_sig_stars <- Vectorize(function(p_val){
  if(is.na(p_val)) return(NA)
  if(p_val <.001) return("***")
  if(p_val <.01) return("**")
  if(p_val <.05) return("*")
  return("")
  
})

get_prop_table <- function(data, 
                           type = "style", 
                           degree = "strong", 
                           group = "gender", 
                           target_var = "style",
                           keep_levels = T,
                           output_format = "list"){

  tmp <- data %>% filter(type == !!type, degree == !!degree)
  if(group == target_var){
    browser()
    return(NULL)
  }
  
  orig_target_var <- target_var
  orig_group <- group
  if(group == "lpa_class" || target_var == "lpa_class"){
    tmp <- add_lpa_class(tmp, type, degree)
    if(target_var == "lpa_class"){
      target_var <- sprintf("lpa_class_%s_%s", type, degree)    
    }
    if(group == "lpa_class"){
      group <- sprintf("lpa_class_%s_%s", type, degree)    
    }
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
    ggassoc(tab, x_lab = group, y_lab = target_var)
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

add_scores_from_key <- function(data, key_file = "keys_df.xlsx", sheet = "key2s", impute_method = c("no", "mice", "median")){
  orig_data <- data
  data <- data %>% select(all_of(num_vars))
  if(impute_method[1] == "mice"){
    data <- impute_mice(data )
  }
  if(impute_method[1] == "median"){
    data <- impute_median(data, vars = num_vars)
  }
  key_tmp <- readxl::read_xlsx(key_file, sheet = sheet)
  keys_cleaned <- key_tmp %>% select(-1) %>% as.matrix()
  var_names <- key_tmp %>% pull(1)
  factor_names <- key_tmp %>% select(-1) %>% names()
  row.names(keys_cleaned) <- var_names
  item_scores <- scoreItems(keys_cleaned, data)
  bind_cols(orig_data, item_scores$scores %>% as.data.frame() %>% as_tibble() %>% set_names(factor_names)) 
}
scale_definition_from_keys <- function(key_file = "keys_df.xlsx", sheet = "keys2"){
  key_tmp <- readxl::read_xlsx(key_file, sheet = sheet)
  factor_names <- key_tmp %>% select(-1) %>% names()
  map_dfr(factor_names, function(fn){
    #browser()
    items <- key_tmp %>%  filter(!!sym(fn) > 0)  %>% pull(rowname) %>% sort() 
    tibble(subscale = fn, n_items = length(items), items = items %>% paste(collapse = ";"))
  })
}

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
