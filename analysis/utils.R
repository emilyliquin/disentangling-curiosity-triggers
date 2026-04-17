plot_partr2 <- function(dataset) {
  
  # get full model r2
  full_model_r2 <- dataset[dataset$term == "Full","estimate"] %>% unlist()
  names(full_model_r2) <- NULL
  
  # do some renaming
  dataset$term <- str_replace_all(dataset$term, "Entropy_theta_z", "Global uncertainty")
  dataset$term <- str_replace_all(dataset$term, "RPE_MAP_abs_z", "Surprise")
  dataset$term <- str_replace_all(dataset$term, "Entropy_z_z", "Local learning potential")
  dataset$term <- str_replace_all(dataset$term, "EIG_theta_z", "Global learning potential")
  
  dataset$term <- str_replace_all(dataset$term, "scale\\(Entropy_theta\\)", "Global uncertainty")
  dataset$term <- str_replace_all(dataset$term, "scale\\(RPE_MAP_abs\\)", "Surprise")
  dataset$term <- str_replace_all(dataset$term, "scale\\(Entropy_z\\)", "Local learning potential")
  dataset$term <- str_replace_all(dataset$term, "scale\\(EIG_theta\\)", "Global learning potential")
  
  # split terms into two columns
  split_terms <- str_split(as.character(dataset$term), "\\+")
  dataset$term1 <- split_terms %>% map(~ .[[1]]) %>% unlist()
  dataset$term2 <- split_terms %>% map(~ ifelse(length(.) == 1, NA, .[[2]])) %>% 
    unlist()
  
  dataset$term2 <- ifelse(is.na(dataset$term2), dataset$term1, dataset$term2)
  
  dataset <- as.data.frame(dataset)
  dataset[dataset$term1 == "Local learning potential" & dataset$term2 == "Global learning potential",
          c("term1", "term2")] <- c("Global learning potential", "Local learning potential")
  
  # remove full r2
  dataset <- dataset %>% filter(term1 != "Full")
  
  # relevel factors

  dataset$term1 <- case_when(dataset$term1 == "Local learning potential" ~ "Local LP",
                             dataset$term1 == "Global learning potential" ~ "Global LP",
                             dataset$term1 == "Surprise" ~ "Surprise",
                             dataset$term1 == "Global uncertainty" ~ "Global\nuncertainty")
  
  dataset$term1 <- factor(dataset$term1, 
                          levels = c("Surprise",
                                     "Global\nuncertainty",
                                     "Global LP",
                                     "Local LP"))

  
  dataset$term2 <- case_when(dataset$term2 == "Local learning potential" ~ "Local LP",
                             dataset$term2 == "Global learning potential" ~ "Global LP",
                             dataset$term2 == "Surprise" ~ "Surprise",
                             dataset$term2 == "Global uncertainty" ~ "Global\nuncertainty")
  
  dataset$term2 <- factor(dataset$term2, 
                          levels = c("Surprise",
                                     "Global\nuncertainty",
                                     "Global LP",
                                     "Local LP"))
  
  
  # add highlight for diagonals
  dataset$highlight2 <- case_when(dataset$term1 == 
                                    dataset$term2  ~ "yes",
                                  .default = "no")
  
  
  max_break <- ifelse(abs(full_model_r2-round(full_model_r2/0.05)*.05) > 0.2, 
                      round(full_model_r2/0.05)*.05, 
                      round(full_model_r2/0.05)*.05 - 0.05)
  
  max_break <- ifelse(max_break < 0, 0, max_break)
  
  plot_breaks <- c(seq(0, max_break, by = 0.05), full_model_r2)
  
  plot_labels <- c(seq(0, max_break, by = 0.05), sprintf("%.2f (Model R²)", 
                                                                    round(full_model_r2, 2)))
  
  n_labels <- length(seq(0, max_break, by = 0.05))
  
  if(full_model_r2 > 0.5) {
    plot_breaks <- c(seq(0, max_break, by = 0.1), full_model_r2)
    plot_labels <- c(seq(0, max_break, by = 0.1), sprintf("%.2f (Model R²)", 
                                                          round(full_model_r2, 2)))
    n_labels <- length(seq(0, max_break, by = 0.1))
    
  }
  
  
  p <- ggplot(dataset, 
              aes(x = term1, y = term2, fill = estimate)) +
    geom_tile() +
    geom_tile(aes(linewidth = highlight2), fill = NA, color = "black") +
    geom_text(aes(label = sprintf('%.2f',estimate)), size = 6, size.unit = "pt") +
    theme_minimal(base_size = 7) +
    theme(panel.grid = element_blank(),
          legend.position = "right",
          plot.title = element_text(size = rel(1)))+ 
    theme(axis.text.x = element_text(angle = 45, hjust=1))+
    scale_fill_distiller(palette = "YlOrRd", direction = 1,
                         limits = c(0, full_model_r2 + full_model_r2/10),
                         breaks = plot_breaks,
                         labels = plot_labels) +
    scale_linewidth_manual(values = c("yes" = 0.5, "no" = 0), guide = "none") + 
    labs(fill = bquote(part ~ R^2)) +
    guides(fill = guide_colourbar(ticks = TRUE,
                                  ticks.colour = c(rep("white", n_labels), "black"),
                                  ticks.linewidth = unit(c(rep(0.2, n_labels), 0.8), "pt"),
                                  barwidth = unit(0.25, "cm"), 
                                  barheight = unit(1, "cm"))) +
    xlab("") + ylab("Predictor") + 
    labs(x = NULL) + coord_cartesian(expand = FALSE)
  
  return(p)
}



plot_partr2_s3 <- function(dataset) {
  
  # get full model r2
  full_model_r2 <- dataset[dataset$term == "Full","estimate"] %>% unlist()
  names(full_model_r2) <- NULL
  
  # do some renaming
  dataset$term <- str_replace_all(dataset$term, "Entropy_theta_z", "Global uncertainty")
  dataset$term <- str_replace_all(dataset$term, "RPE_MAP_abs_z", "Surprise")
  dataset$term <- str_replace_all(dataset$term, "Entropy_z_z", "Local learning potential")
  dataset$term <- str_replace_all(dataset$term, "EIG_theta_z", "Global learning potential")
  
  dataset$term <- str_replace_all(dataset$term, "scale\\(Entropy_theta\\)", "Global uncertainty")
  dataset$term <- str_replace_all(dataset$term, "scale\\(RPE_MAP_abs\\)", "Surprise")
  dataset$term <- str_replace_all(dataset$term, "scale\\(Entropy_z\\)", "Local learning potential")
  dataset$term <- str_replace_all(dataset$term, "scale\\(EIG_theta\\)", "Global learning potential")
  
  # split terms into two columns
  split_terms <- str_split(as.character(dataset$term), "\\+")
  dataset$term1 <- split_terms %>% map(~ .[[1]]) %>% unlist()
  dataset$term2 <- split_terms %>% map(~ ifelse(length(.) == 1, NA, .[[2]])) %>% 
    unlist()
  
  dataset$term2 <- ifelse(is.na(dataset$term2), dataset$term1, dataset$term2)
  
  dataset <- as.data.frame(dataset)
  dataset[dataset$term1 == "Local learning potential" & dataset$term2 == "Global learning potential",
          c("term1", "term2")] <- c("Global learning potential", "Local learning potential")
  
  # remove full r2
  dataset <- dataset %>% filter(term1 != "Full")
  
  # relevel factors
  
  dataset$term1 <- case_when(dataset$term1 == "Local learning potential" ~ "Local LP",
                             dataset$term1 == "Global learning potential" ~ "Global LP",
                             dataset$term1 == "Surprise" ~ "Surprise",
                             dataset$term1 == "Global uncertainty" ~ "Global\nuncertainty")
  
  dataset$term1 <- factor(dataset$term1, 
                          levels = c("Surprise",
                                     "Global\nuncertainty",
                                     "Global LP",
                                     "Local LP"))
  
  
  dataset$term2 <- case_when(dataset$term2 == "Local learning potential" ~ "Local LP",
                             dataset$term2 == "Global learning potential" ~ "Global LP",
                             dataset$term2 == "Surprise" ~ "Surprise",
                             dataset$term2 == "Global uncertainty" ~ "Global\nuncertainty")
  
  dataset$term2 <- factor(dataset$term2, 
                          levels = c("Surprise",
                                     "Global\nuncertainty",
                                     "Global LP",
                                     "Local LP"))
  
  
  # add highlight for diagonals
  dataset$highlight2 <- case_when(dataset$term1 == 
                                    dataset$term2  ~ "yes",
                                  .default = "no")
  
  
  max_break <- ifelse(abs(full_model_r2-round(full_model_r2/0.05)*.05) > 0.2, 
                      round(full_model_r2/0.05)*.05, 
                      round(full_model_r2/0.05)*.05 - 0.05)
  
  max_break <- ifelse(max_break < 0, 0, max_break)
  
  plot_breaks <- c(seq(0, max_break, by = 0.05), full_model_r2)
  
  plot_labels <- c(seq(0, max_break, by = 0.05), sprintf("%.2f\n(Model R²)", 
                                                         round(full_model_r2, 2)))
  
  n_labels <- length(seq(0, max_break, by = 0.05))
  
  if(full_model_r2 > 0.5) {
    plot_breaks <- c(seq(0, max_break, by = 0.1), full_model_r2)
    plot_labels <- c(seq(0, max_break, by = 0.1), sprintf("%.2f\n(Model R²)", 
                                                          round(full_model_r2, 2)))
    n_labels <- length(seq(0, max_break, by = 0.1))
    
  }
  
  
  p <- ggplot(dataset, 
              aes(x = term1, y = term2, fill = estimate)) +
    geom_tile() +
    geom_tile(aes(linewidth = highlight2), fill = NA, color = "black") +
    geom_text(aes(label = sprintf('%.2f',estimate)), size = 6, size.unit = "pt") +
    theme_minimal(base_size = 7) +
    theme(panel.grid = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = rel(1)))+ 
    theme(axis.text.x = element_text(angle = 45, hjust=1))+
    scale_fill_distiller(palette = "YlOrRd", direction = 1,
                         limits = c(0, full_model_r2 + full_model_r2/10),
                         breaks = plot_breaks,
                         labels = plot_labels) +
    scale_linewidth_manual(values = c("yes" = 0.5, "no" = 0), guide = "none") + 
    labs(fill = bquote(part ~ R^2)) +
    guides(fill = guide_colourbar(ticks = TRUE,
                                  ticks.colour = c(rep("white", n_labels), "black"),
                                  ticks.linewidth = unit(c(rep(0.2, n_labels), 0.8), "pt"),
                                  barwidth = unit(2.25, "cm"), 
                                  barheight = unit(0.25, "cm"))) +
    xlab("Predictor") + ylab("Predictor")
  
  return(p)
}

