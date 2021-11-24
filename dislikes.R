library(tidyverse)
library(tidyLPA)
library(nFactors)
library(psych)
source("ggassoc.R")

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

lpa_class_labels <- tribble(~type, ~degree, ~low, ~hi, 
                            "style",  "strong", "lowbrow", "highbrow",
                            "style",  "slight",  "small deficits", "tolerable",
                            "artist", "strong", "highbrow", "lowbrow", 
                            "artist", "slight", "tolerable", "lowbrow"
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
              "music.too_off", 
              "music.too_rhythmic", 
              "music.too_slow", 
              "music.too_soft", 
              "music.too_unrhythmic", 
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

get_efa <- function(data, quest_type = "" , n_factors = 3, rotate = "varimax", with_optimal = T, with_panels = F, only_factors = F){
  messagef("extracting efa for %s",quest_type)
  tmp <- 
    data %>% 
    select(where(is.numeric)) %>% 
    select(-starts_with("PC")) 
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
  psych::principal(tmp, n_factors, rotate = rotate)
}

read_var_map <- function(fname = "data/var_map.csv"){
  var_map <- read.csv(fname, sep = ";", header =T, stringsAsFactors = F) %>% 
    as_tibble() %>% 
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

setup_workspace <- function(){
  master <- read_data()
  var_map <- read_var_map()
  labels <- var_map$label
  names(labels) <- var_map$code
  names(master)[names(master) %in% names(labels)] <- labels
  browser()
  master$education <- education_labels[as.numeric(master$education)]
  master$job <- job_labels[as.numeric(master$job)]
  master_alt <- add_alternate_factors(master)
  assign("master", master, globalenv())
  assign("master_alt", master_alt, globalenv())
}

add_alternate_factors <- function(data){
  set.seed(666)
  data <-  data %>% select(-starts_with("PC")) %>% select(-all_of(bad_vars)) %>% select(-starts_with(c("reaction")))
  impute <- mice::mice(data = data, m = 1, method = "pmm", maxit = 10, seed = 500)
  data <- complete(impute, 1) %>% as_tibble()  
  efa <- get_efa(data, n_factors = 6, with_optimal = T, with_panels = F, rotate = "oblimin") 
  browser()
  bind_cols(data, 
            efa$scores %>% 
              as_tibble() %>% 
              set_names(c("PC_too_simple", "PC_displeasure", "PC_too_little_impact", "PC_social")))  
}

tidy_assoc_stats <- function(as){
  as$chisq_tests %>% 
    as_tibble() %>% 
    set_names(c("statistic", "df", "p_value")) %>% 
    mutate(test = c("llr", "chisq")) %>% 
    pivot_wider(names_from = test, values_from = everything()) %>% select(-starts_with("test_")) %>%  
  
  bind_cols(tibble(phi = as$phi, contingency = as$contingency, cramers_v = as$cramer))
}

add_lpa_class <- function(data, type = "style", degree = "strong"){
  var_name <- sprintf("lpa_class_%s_%s", type, degree)
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
  lpa <- data %>%  select(starts_with("PC")) %>% estimate_profiles(2)
  first_PC <- names(data)[str_detect(names(data), "^PC_")][1] 
  tmp <- get_data(lpa) %>% left_join(data %>% 
                                       select(starts_with(first_PC), 
                                              gender, p_id, style, type, degree, age_group), 
                                     by = first_PC) %>% 
    mutate(lpa_class = factor(CPROB1 > .5, labels = c(labels$low, labels$hi)))
  check_stats <- tmp %>% group_by(lpa_class) %>% summarise(across(starts_with("PC_"), mean))
  print(check_stats)
  #if(check_stats$PC_complexity[1] > check_stats$PC_complexity[2]){
  #  tmp <- tmp %>% mutate(lpa_class = factor(lpa_class, labels = levels(lpa_class)[c(2,1)]))
  #}
    
  tmp %>% rename(!!var_name := lpa_class) %>% select(-model_number, -classes_number, -Class, -CPROB1, -CPROB2)
}

get_sig_stars <- Vectorize(function(p_val){
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

get_all_contingency_tables <- function(data, output_format = "df"){
  types <- unique(data$type)
  degrees <- unique(data$degree)
  groups <- c("gender", "age_group", "lpa_class")
  target_vars <- c("style", "lpa_class")
  data <- map_dfr(types, function(ty){
    map_dfr(degrees, function(deg){
      add_lpa_class(data, ty, deg)  
    })
  })

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
              ggsave(sprintf("%s_%s_%s_%s.png", ty, deg, g, tv), dpi = 500)
              return(NULL)
            }
            q
        }
      }) %>% set_names(target_vars)
    }) %>% set_names(groups)
  }) %>% set_names(degrees)
  }) %>% set_names(types)
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
