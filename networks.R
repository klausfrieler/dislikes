library(DT)
library(sjPlot)
library(pcalg)
library(networkD3)
library(igraph)
library(network)
library(sna)

get_pc_graph <- function(data, 
                         alpha = .05,   
                         charge = -220,
                         linkDistance = 200,
                         fontSize = 16,
                         opacityNoHover = .75){
  #require(Rgraphviz)
  #browser()
  red <- data
  cor_data <- cor(red, use = "pairwise.complete.obs")
  suffStat <- list(C = cor_data, n = nrow(red))
  pc.fit <- pcalg::pc(suffStat, indepTest = pcalg::gaussCItest, p = ncol(red), alpha = alpha)
  labels <- names(red)
  #browser()
  #adjm <- wgtMatrix(getGraph(pc.fit), transpose = FALSE)
  #ig_network <- graph_from_adjacency_matrix(adjm, mode = "directed", weighted = T)
  ig_network <- graph_from_graphnel(getGraph(pc.fit), name = TRUE, weight = TRUE, unlist.attrs = TRUE)
  d3_network <- igraph_to_networkD3(ig_network)
  d3_network$nodes$group <- 1
  d3_network$nodes$name <- labels
  for(i in 1:nrow(d3_network$links)){
    d3_network$links[i,]$value <-  50*abs(cor_data[d3_network$links[i,]$source + 1,  d3_network$links[i,]$target + 1])
  }
  browser()
  sn <- forceNetwork(
    Links = d3_network$links, 
    Nodes = d3_network$nodes, 
    Source = 'source', 
    Target = 'target', 
    Value = "value",
    NodeID = 'name', 
    Group = "group", 
    fontSize = fontSize,
    opacityNoHover = opacityNoHover,
    bounded = F,
    zoom = T,
    charge = charge,
    linkDistance = linkDistance,
    arrows = TRUE,
    #Nodesize = "size",
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
  )
  q <- pcalg::iplotPC(pc.fit, labels = labels)
  #plot(pc.fit@graph, main = "", nodeAttrs = nAttrs, attrs = attrs)
  #sn
}

get_stable_cor_network2 <- function(data, 
                                   alpha = .05,   
                                   charge = -220,
                                   linkDistance = 200,
                                   fontSize = 16,
                                   opacityNoHover = .75){
  #require(Rgraphviz)
  #browser()
  set.seed(666)
  red <- master
  top_cor <- get_top_cors(master)
  cor_data <- top_cor %>% 
    group_by(var1, var2, cor_id) %>% 
    summarise(r = mean(r), .groups = "drop")
  var_levels <- levels(factor(c(cor_data$var1, cor_data$var2)))
  group1 <- c("DS_too_simple", "DS_too_mainstream", "DS_social_incongruence", "DS_not_authentic")
  group2 <- c("DS_displeasure", "DS_no_impact")
  group3 <- setdiff(var_levels, union(group1, group2)) 
  group_labels <- rep(1, length(var_levels))
  group_labels[var_levels %in% group1] <- 1
  group_labels[var_levels %in% group2] <- 2
  group_labels[var_levels %in% group3] <- 3
  
  d3_network <- list(nodes = data.frame(name = fashion_subscale_names(var_levels), 
                                        group = group_labels),
                     links = data.frame(source = factor(cor_data$var1, levels = var_levels) %>% 
                                          as.integer() %>% 
                                          magrittr::subtract(1),
                                        target = factor(cor_data$var2, levels = var_levels) %>% 
                                          as.integer() %>% 
                                          magrittr::subtract(1),
                                        value = cor_data$r))
  sn <- forceNetwork(
    Links = d3_network$links, 
    Nodes = d3_network$nodes, 
    Source = 'source', 
    Target = 'target', 
    Value = "value",
    NodeID = 'name', 
    Group = "group", 
    fontSize = fontSize,
    opacityNoHover = opacityNoHover,
    bounded = F,
    zoom = T,
    charge = charge,
    linkDistance = linkDistance,
    arrows = TRUE,
    #Nodesize = "size",
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
  )
  sn <- sankeyNetwork(
    Links = d3_network$links, 
    Nodes = d3_network$nodes, 
    Source = 'source', 
    Target = 'target', 
    Value = "value",
    NodeID = 'name', 
    fontSize = fontSize,
    #Nodesize = "size",
    colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")
  )
  DS_var <- union(top_cor$var1, top_cor$var2 )
  cor_mat <- matrix(0, 
                    ncol = length(DS_var), 
                    nrow = length(DS_var), 
                    dimnames = list(DS_var, DS_var))
  for(i in 1:nrow(top_cor)){
    cor_mat[top_cor[i,]$var1, top_cor[i,]$var2] <- top_cor[i,]$r
    cor_mat[top_cor[i,]$var2, top_cor[i,]$var1] <- top_cor[i,]$r
  }
  dimnames(cor_mat) <- lapply(dimnames(cor_mat), fashion_subscale_names)
  sn <- chordNetwork(Data = cor_mat, 
                     width = 750, 
                     height = 750,
                     labels = fashion_subscale_names(DS_var)) 
  sn <- simpleNetwork(d3_network$links)
  cor_mat[cor_mat> 0 ] <- 1
  sn <- network(cor_mat, directed = TRUE)
  network.vertex.names(sn) = fashion_subscale_names(DS_var)
  sn <- GGally::ggnet2(sn, node.size = 6, node.color = "red", edge.size = 1, edge.color = "blue")
  sn
}

get_stable_cor_network <- function(data, directed = TRUE){
  #seed <- sample(1:1000,1)
  #printf("Seed = %d", seed)
  set.seed(872)
  top_cor <- get_top_cors(data)
  
  cor_data <- top_cor %>% 
    group_by(var1, var2, cor_id) %>% 
    summarise(r = mean(r), .groups = "drop")
  DS_vars <- levels(factor(c(cor_data$var1, cor_data$var2)))
  group1 <- c("DS_too_simple", "DS_too_mainstream", "DS_social_incongruence", "DS_not_authentic", "DS_too_emotional")
  group2 <- c("DS_displeasure", "DS_no_impact")
  group3 <- setdiff(DS_vars, union(group1, group2)) 
  node_labels <- c("Reaction", "Reaction", "Highbrow", "Social", "Lowbrow", "Highbrow", "Highbrow", "Lowbrow", "Highbrow")
  node_types <- c("Reaction", "Reaction", rep("Descriptive", 7))
  cor_mat <- matrix(0, 
                    ncol = length(DS_vars), 
                    nrow = length(DS_vars), 
                    dimnames = list(DS_vars, DS_vars))
  for(i in 1:nrow(top_cor)){
    cor_mat[top_cor[i,]$var1, top_cor[i,]$var2] <- top_cor[i,]$r
    cor_mat[top_cor[i,]$var2, top_cor[i,]$var1] <- top_cor[i,]$r
  }
  #browser()
  if(directed){
    save_val <- cor_mat["DS_social_incongruence", "DS_displeasure"] 
    cor_mat["DS_displeasure", ] <- 0 
    cor_mat["DS_no_impact", ] <- 0 
    cor_mat["DS_social_incongruence", ] <- 0 
    cor_mat["DS_social_incongruence", "DS_displeasure"] <- save_val 
    
  }
  dimnames(cor_mat) <- lapply(dimnames(cor_mat), fashion_subscale_names)
  #cor_mat[cor_mat > 0 ] <- 1
  #cor_mat <- cor_mat * as.integer(lower.tri(cor_mat))
  sn <- network::network(cor_mat, directed = directed)
  network.vertex.names(sn) = fashion_subscale_names(DS_vars)
  sn %v% "group" <- node_labels
  sn %v% "main_type" <- node_types

  sn <- GGally::ggnet2(sn, 
                       mode = "fruchtermanreingold",
                       node.size = 10, 
                       label = TRUE,
                       color = "group",
                       shape = "main_type",
                       shape.legend = "",
                       color.legend = "",
                       palette = "Set2",
                       arrow.size = 12, 
                       arrow.gap = 0.025,
                       #size = "degree",
                       edge.size = 1, 
                       edge.color = "darkgrey")
  sn
}
