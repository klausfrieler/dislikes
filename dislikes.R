library(tidyverse)
library(tidyLPA)
library(nFactors)
library(psych)
source("ggassoc.R")
source("super_heatmap.R")
source("utils.R")
source("plot_utils.R")
source("contingencies.R")

messagef <- function(...) message(sprintf(...))
printf <- function(...) print(sprintf(...))

lpa_class_labels <- tribble(~type, ~degree, ~class1, ~class2, 
                            "style",  "strong", "highbrow", "lowbrow",
                            "style",  "slight", "highbrow", "lowbrow",
                            "artist", "strong", "highbrow", "lowbrow", 
                            "artist", "slight", "lowbrow",  "highbrow"
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
              "music.too_chaotic", 
              "music.too_complex", 
              "music.too_fast", 
              "music.too_melodious", 
              "music.too_much_change", 
              "music.too_rhythmic", 
              "music.too_slow", 
              "music.too_soft", 
              "music.too_variable", 
              "social.bad_experiences", 
              "social.not_peer_approved", 
              "social.too_often_heard")

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



read_var_map <- function(fname = "data/var_map.csv"){
  read.csv(fname, sep = ";", header =T, stringsAsFactors = F) %>% 
    as_tibble() %>% 
    filter(nchar(code) > 0) %>% 
    mutate(code = str_replace(code, "^_", ""))
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

add_style_counts <- function(data){
  data %>% 
    group_by(type, degree, style) %>% 
    mutate(style_count = n())  %>% 
    ungroup() %>% 
    group_by(type, degree) %>% 
    mutate(norm_style_count = style_count/n()) %>% 
    ungroup()
}
var_map <- read_var_map()
num_vars <- var_map %>% filter(type == 1) %>% pull(label) 

setup_workspace <- function(reload = T, add_to_global_env = F){

  if(!reload){
    master <- readRDS("data/master.rds")
    if(add_to_global_env){
      assign("master", master, globalenv())
    }
    return(master)
  }
  
  master <- read_data()
  if(add_to_global_env)assign("master_raw", master, globalenv())
  before <- n_distinct(master$p_id)
  master <- filter_bad_raters(master)
  after <- n_distinct(master$p_id)
  messagef("Excluded %d bad raters (%d -> %d)", before -after, before, after)
  var_map <- read_var_map()
  names(master)[names(master) %in% var_map$code] <- var_map$label
  #browser()
  master$education <- education_labels[as.numeric(master$education)]
  master$job <- job_labels[as.numeric(master$job)]
  master <- master %>% 
    mutate(age = as.numeric(age), 
           grade = as.numeric(grade)) %>% 
    mutate(higher_ed = education %in% c("bachelor", "master", "promotion"), 
           at_least_a_level = education %in% c("bachelor", "master", "promotion", "a-levels"), 
           other_ed = !at_least_a_level)
  master <- master %>% group_by(p_id) %>% mutate(n_conditions = n()) %>% ungroup() 
  master <- master %>% mutate(full_type = sprintf("%s_%s", type, degree))
  master <- add_scores_from_key(master, key_file = "data/keys_df.xlsx", sheet = "keys_v3", impute_method = "mice")
  master <- add_style_counts(master)
  master <- add_all_lpa_classes(master)
  master <- add_total_lpa_class(master)
  if(add_to_global_env){
    saveRDS(master, "data/master.RDS")
    assign("master", master, globalenv())
  }
  return(master)
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


add_lpa_class <- function(data, type = "style", degree = "strong", use_same_name = F, n_classes = 2, with_diagnostics = F){
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
  #browser()
  if(!is.null(type) & nchar(type) > 0){
    data  <-  data %>% filter(type == !!type) 
    
  }
  if(!is.null(degree) & nchar(degree) > 0){
    data  <-  data %>% filter(degree == !!degree) 
    
  }
  labels  <- lpa_class_labels %>% filter(type == !!type, degree == !!degree) 
  lpa <- data %>%  select(starts_with("DS")) %>% estimate_profiles(n_classes)
  if(with_diagnostics)plot_profiles(lpa, add_line = T, rawdata = F) + coord_flip()
  #browser()
  first_PC <- names(data)[str_detect(names(data), "^DS_")][1] 
  tmp <- bind_cols(data, get_data(lpa) %>% select(Class))
  if(nrow(labels) > 0){
    tmp <- tmp %>% 
      mutate(lpa_class = factor(Class, labels = c(labels$class1, labels$class2)))
  }
  else{
    tmp <- tmp %>% 
      mutate(lpa_class = factor(Class))
    
  }
  if(with_diagnostics){
    check_stats <- tmp %>% group_by(lpa_class) %>% summarise(across(starts_with("DS_"), mean))
    check_stats_t <- check_stats %>% 
      pivot_longer(-lpa_class) %>% 
      pivot_wider(id_cols = name, names_from = lpa_class, values_from = value, names_prefix = "lpa_class") 
    messagef("Type = %s, degree = %s", type, degree)
    print(check_stats_t)
}
  tmp %>% rename(!!var_name := lpa_class) %>% select( -Class)
}

add_all_lpa_classes <- function(data){
  map_dfr(data %>% group_split(type, degree), function(x){
    add_lpa_class(x, type = unique(x$type), degree = unique(x$degree), use_same_name = T)
  })
}

add_total_lpa_class <- function(data){
  lpa_classes <- c("lowbrow", "mainly_lowbrow", "mixed", "mainly_highbrow", "highbrow")
  get_class <- function(lpa_classes){
    highbrowness <- mean(lpa_classes == "highbrow")
    if(highbrowness == 0) return("lowbrow")
    if(highbrowness == 1) return("highbrow")
    if(highbrowness > .5) return("mainly_highbrow")
    if(highbrowness < .5) return("mainly_lowbrow")
    return("mixed")
  }
  data %>% 
    group_by(p_id) %>% 
    mutate(total_lpa_class = get_class(lpa_class)) %>% 
    mutate(total_lpa_class = factor(total_lpa_class, levels = lpa_classes)) %>% ungroup()
}

add_scores_from_key <- function(data, 
                                key_file = "data/keys_df.xlsx", 
                                sheet = "keys_v3", 
                                impute_method = c("no", "mice", "median")){
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

