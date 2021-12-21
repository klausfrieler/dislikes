library(tidyverse)

get_manova <- function(data, 
                       scale_by_id = T, 
                       var_names = "MR", predictors = c("type", "degree", "style", "age_group", "gender", "p_id"),
                       tidy_up = F){
  vars <- data %>% select(contains(var_names)) %>% names()
  if(scale_by_id){
    predictors <- setdiff(predictors, "p_id")
  }
  predictor_terms <- paste(predictors, collapse = " + ")
  man_form <- sprintf("cbind(%s) ~ type + style + degree + p_id", paste(vars, collapse = ",")) %>% as.formula()
  if(scale_by_id){
    for(v in vars){
      data <- data %>% group_by(p_id) %>% mutate(!!sym(v) := scale(!!sym(v))) %>% ungroup()
    }
  } 
  man_form <- sprintf("cbind(%s) ~ %s", paste(vars, collapse = ","), predictor_terms) %>% as.formula()
  
  man_model <- manova(man_form, data = data) 
  if(tidy_up){
    man_model <- summary.aov(man_model)
    ret <- 
      map_dfr(1:length(vars), function(i){
      man_model[[i]] %>% broom::tidy() %>% mutate(response = vars[i]) %>% filter(term != "Residuals")
    }) 
    ret <- ret %>% 
      mutate(sig = get_sig_stars(p.value)) %>% 
      select(response, everything())
  }
  else{ret <- man_model}
  ret
}