plot_missing_parameters <- function(data, use_counts) {
  
  # create missing pattern dataframe
  missing_patterns <- data.frame(is.na(data)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  
  # compute number of rows of the original dataframe
  # which here corresponds to the sum of count in missing_patterns
  total_num_rows <- sum(missing_patterns$count)
  
  # create variable to identify complete cases
  # create variable corresponding to ranking of missing patterns by frequency
  msp_treated <- missing_patterns %>%
    mutate(complete_cases = rowSums(missing_patterns == TRUE) == 0) %>%
    rowid_to_column(var = "Y")
  
  # compute ranking of complete cases among missing patterns
  index_complete_cases <- msp_treated[msp_treated$complete_cases == TRUE, ]$Y
  
  # create top plot
  top_plot <- msp_treated %>%
    pivot_longer(cols = -c("Y", "count", "complete_cases"), names_to = "variable", values_to = "bool") %>%
    group_by(variable) %>%
    summarize(countmissing = sum(bool * count)) %>%
    mutate(percentmissing = countmissing / total_num_rows)
  
  # set y aesthetics of top plot depending on use_counts value
  if(use_counts == TRUE){
    top_plot  <- top_plot %>%
      ggplot(aes(y = countmissing))
  }
  else{
    top_plot  <- top_plot %>%
      ggplot(aes(y = 100 * percentmissing))
  }
  
  # keep creating top plot
  top_plot <- top_plot +
    geom_bar(aes(x = reorder(variable, -countmissing)), stat="identity", fill="#6495ED", alpha=0.6) +
    xlab("") +
    ylab(ifelse(use_counts == TRUE, "num rows \n missing", "% rows \n missing")) +
    labs(title = "Missing value patterns") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 20, hjust = 1))
  
  # create main plot
  main_plot <- msp_treated %>%
    pivot_longer(cols = -c("Y", "count", "complete_cases"), names_to = "variable", values_to = "bool") %>%
    group_by(variable) %>%
    mutate(countmissing = sum(bool * count)) %>%
    mutate(percentmissing = countmissing / total_num_rows) %>%
    ggplot(aes(x = reorder(variable, -countmissing), y = reorder(Y, -Y), fill= bool, alpha = complete_cases)) + 
    geom_tile(color = "gray") +
    annotate(geom = "text", y = nrow(missing_patterns)  + 1 - index_complete_cases, x = ncol(missing_patterns) / 2, 
             label="complete cases", hjust = 0.5) +
    xlab("variable") +
    ylab("missing pattern") +
    scale_fill_manual(values = c("#cbcbcb", "#7f6fa1")) +
    scale_alpha_manual(values = c(0.6,1)) +
    theme_classic() +
    theme(legend.position="none",
          axis.text.x = element_text(angle = 20, hjust = 1))
  
  # create right plot
  right_plot <- msp_treated %>%
    mutate(percentmissing = count / total_num_rows)
  
  # set y aesthetics of right plot depending on use_counts value
  if(use_counts == TRUE){
    right_plot <- right_plot %>%
      ggplot(aes(y = count, alpha = complete_cases))
  }
  else{
    right_plot <- right_plot %>%
      ggplot(aes(y = 100 * percentmissing, alpha = complete_cases))
  }
  
  # keep creating right plot
  right_plot <- right_plot  +
    geom_bar(aes(x= reorder(Y, -Y)), stat="identity", fill="#6495ED") +
    xlab("") +
    ylab(ifelse(use_counts == TRUE, "num rows", "% rows")) +
    scale_alpha_manual(values = c(0.6,1)) +
    coord_flip() +
    theme_bw() +
    theme(legend.position="none") +
    theme(panel.grid.major.y = element_blank())
  
  # set y limits of top plot depending on use_counts value
  if(!use_counts){
    top_plot <- top_plot + ylim(0,100)
    right_plot <- right_plot + ylim(0, 100)
  }
  else{
    top_plot <- top_plot + expand_limits(y=c(0, NA))
    right_plot <- right_plot + expand_limits(y=c(0, NA))
  }
  
  
  # put together the three plots with patchwork
  design <- "
    11111#
    222223
    222223
    222223
    222223"
  
  top_plot + main_plot + right_plot + plot_layout(design = design)
}