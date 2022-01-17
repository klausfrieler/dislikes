library(tidyverse)
library(coin)

get_manova <- function(data, 
                       scale_by_id = T, 
                       var_names = "DS_", 
                       predictors = c("type", "degree", "style", "age_group", "gender", "p_id"),
                       exclude_preds = "",
                       tidy_up = F){
  vars <- data %>% select(contains(var_names)) %>% names()
  if(scale_by_id){
    predictors <- setdiff(predictors, "p_id")
  }
  predictors <- setdiff(predictors, exclude_preds)
  predictor_terms <- paste(predictors, collapse = " + ")
  man_form <- sprintf("cbind(%s) ~ style + type +  degree", paste(vars, collapse = ",")) %>% as.formula()
  if(scale_by_id){
    for(v in vars){
      data <- data %>% group_by(p_id) %>% mutate(!!sym(v) := scale(!!sym(v))) %>% ungroup()
    }
  } 
  #browser()
  man_form <- sprintf("cbind(%s) ~ %s", paste(vars, collapse = ","), predictor_terms) %>% as.formula()
  
  man_model <- manova(man_form, data = data) 
  if(tidy_up){
    man_model_tidy <- man_model %>% 
      broom::tidy() %>%
      mutate(response = "Overall")  %>% 
      filter(term != "Residuals")
    man_model <- summary.aov(man_model)
    ret <- 
      map_dfr(1:length(vars), function(i){
      man_model[[i]] %>% broom::tidy() %>% mutate(response = vars[i]) %>% filter(term != "Residuals")
    }) 
    ret <- ret %>% 
      bind_rows(man_model_tidy) %>% 
      mutate(sig = get_sig_stars(p.value)) %>% 
      select(response, everything())
  }
  else{ret <- man_model}
  ret
}

get_independence_test <- function(data, 
                                  type = "style",
                                  degree = "strong",
                                  var_names = "DS_", 
                                  predictors = c("style | degree"),
                                  nresample = 1000){
  vars <- data %>% select(contains(var_names)) %>% names()
  cit_form <- sprintf("%s ~ %s", paste(vars, collapse = " + "), predictors) %>% as.formula()
  if(nchar(type) > 0){
    data <- data %>% mutate(across(where(is.character), as.factor)) %>% filter(type == !!type)
  }
  if(nchar(degree) > 0){
    data <- data %>% mutate(across(where(is.character), as.factor)) %>% filter(degree == !!degree)
    
  }
  cit <- coin::independence_test( cit_form, 
                                  distribution = coin::approximate(nresample = nresample), 
                                  teststat = "maximum", 
                                  data = data) 
  list(cit = cit, 
       model = as.character(cit_form),
       type = type,
       degree = degree,
       vars = vars,
       predictors = predictors,
       pval_global = coin::pvalue(cit), 
       raw_single_pvals = coin::pvalue(cit, method = "unadjusted"),
       single_pvals = coin::pvalue(cit, method = "single-step")) 
}