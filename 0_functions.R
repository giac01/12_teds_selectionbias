rename_from_labels <- function(df) {
  # Get current column names
  old_names <- names(df)
  
  # Create vector of new names based on labels
  new_names <- sapply(df, function(column) {
    # Get the label attribute, if it exists
    label <- attr(column, "label")
    
    # If no label exists, keep the original name
    if (is.null(label)) return(NULL)
    
    return(label)
  })
  
  # Filter out NULLs (columns without labels)
  valid_idx <- !sapply(new_names, is.null)
  
  # Only rename columns that have labels
  if (any(valid_idx)) {
    names(df)[valid_idx] <- unlist(new_names[valid_idx])
  }
  
  names(df) = paste0(old_names, ": ", names(df))
  
  return(df)
}


set_most_frequent_ref <- function(x) {
  most_frequent_level <- names(sort(table(x), decreasing = TRUE)[1])
  relevel(x, ref = most_frequent_level)
}


calc_auc = function(x){
  outcome_name    = all.vars(formula(x))[1]
  
  outcome_values  = x$data[[outcome_name]]
  predicted_probs = predict(x, type = "response")
  
  roc_obj         = pROC::roc(outcome_values, predicted_probs)
  
  plot(roc_obj)
  
  auc             = pROC::auc(roc_obj)
  
  return(auc)
  
}

var_to_label = function(var, df0 = df){
  df0 = select(df0, all_of(var))
  out = sapply(1:ncol(df0), function(i) attr(df0[[i]], "label"))
  return(out)
}

df_to_label  = function(df){
  sapply(1:ncol(df), function(i) attr(df[[i]], "label"))
}

cleanvar = function(x){
  return(as.numeric(na.omit(unlist(x))))
}

# df2 = original_dataset
# df1 = attritioned_datasets[[10]]

# x = rbinom(100, 4, .2)
# y = rbinom(100, 4, .8)
# 
# x[13] = NA
# y[95] = NA
 
.hellinger_discrete = function(x, y){
  all_outcomes = as.numeric(stats::na.omit(base::unique(c(x,y))))
  
  x_count = sapply(all_outcomes, function(i) length(which(x==i)))
  y_count = sapply(all_outcomes, function(i) length(which(y==i)))
  
  x_prob  = x_count/sum(x_count)
  y_prob  = y_count/sum(y_count)
  
  x_prob_sqrt = sqrt(x_prob)
  y_prob_sqrt = sqrt(y_prob)
  
  if (round(sum(x_prob),4)!=1 | round(sum(y_prob),4)!=1) stop("ERROR")
  
  h = sqrt(sum((x_prob_sqrt-y_prob_sqrt)^2))/sqrt(2)
  
  return(h)
}

.safe_hellinger <- function(x, y) {
  tryCatch(
    {
      statip::hellinger(x, y) * sqrt(2)
    },
    error = function(e) NA,
    warning = function(w) NA
  )
}

compare_hellinger = function(df1, df2){
  
  # Figure out if each column should be treated as discerete or continuous 
  
  n_categories = apply(df2, 2, function(x) length(unique(x)))
  
  var_type     = ifelse(n_categories<15, "discrete", "continuous")
  
  out = vector() # vector of H values for each column in df1/df2 
  
  for(i in 1:ncol(df1)){
    # print(var_type[i])
    if (var_type[i]=="continuous"){
      out[i] =.safe_hellinger(
        cleanvar(df1[,i]),
        cleanvar(df2[,i])
      )
    }
    
    if (var_type[i]=="discrete"){
      out[i] = .hellinger_discrete(
        cleanvar(df1[,i]),
        cleanvar(df2[,i])
      )
    }
  }
  
  return(out)
  # return(
  #   sapply(1:ncol(df1), function(i) 
  #     .safe_hellinger(
  #       cleanvar(df1[,i]),
  #       cleanvar(df2[,i])
  #     )
  #   )
  # )
}

calc_smd = function(x,y){
  md        = mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
  # pooled_sd = sqrt((sd(x, na.rm = TRUE)^2 + sd(y, na.rm = TRUE)^2)/2)
  sd_y      = sd(y, na.rm = TRUE)
  return(md / sd_y)
}

calc_md = function(x,y){
  md        = mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
  return(md)
}

compare_smd = function(df1, df2){
  return(
    sapply(1:ncol(df1), function(i) 
      calc_smd(
        cleanvar(df1[,i]),
        cleanvar(df2[,i])
      )
    )
  )
}


compare_md = function(df1, df2){
  return(
    sapply(1:ncol(df1), function(i) 
      calc_md(
        cleanvar(df1[,i]),
        cleanvar(df2[,i])
      )
    )
  )
}

compare_correlation = function(df1, df2){
  df1_cor = cor(df1, use = "pairwise.complete.obs")
  df2_cor = cor(df2, use = "pairwise.complete.obs")
  
  residuals = df1_cor - df2_cor
  residuals = residuals[lower.tri(residuals)]
  
  return(residuals)
  
}

.boot_compare_df = function(df1, df2){
  boot_select = sample(nrow(df1), nrow(df1), replace = TRUE)
  
  df1 = df1[boot_select,]
  df2 = df2[boot_select,]
  
  out = list(
    compare_md(df1, df2),
    compare_smd(df1, df2),
    compare_hellinger(df1, df2),
    compare_correlation(df1, df2),
    calc_srmr2(df1, df2)
  )
  
  names(out) = c("md", "smd", "h", "cor_resid", "srmr")
  
  return(out)
}


.mean_qi_pd = function(x, .width = .95){
  out = ggdist::mean_qi(x, .width = .width, na.rm = TRUE)
  pd  = as.numeric(bayestestR::p_direction(x))
  out$pd = pd
  return(out)
}

compare_df = function(df1, df2, B = 10){
  boot_results = list()
  for (i in 1:B){
    boot_results[[i]] = .boot_compare_df(df1, df2)
  }  
  
  md_df   = do.call(rbind, lapply(boot_results, function(x) t(as.data.frame(x$md))))
  smd_df  = do.call(rbind, lapply(boot_results, function(x) t(as.data.frame(x$smd))))
  h_df    = do.call(rbind, lapply(boot_results, function(x) t(as.data.frame(x$h))))
  cor_df  = do.call(rbind, lapply(boot_results, function(x) t(as.data.frame(x$cor_resid))))
  srmr_df = do.call(rbind, lapply(boot_results, function(x) t(as.data.frame(x$srmr))))
  

  bootstrap_iter = list(md_df, smd_df, h_df, cor_df, srmr_df)
  names(bootstrap_iter) = c("md", "smd", "h", "cor_resid", "srmr")
  
  bootstrap_summary = lapply(bootstrap_iter, function(df)
    apply(df,2, function(xx).mean_qi_pd(xx))
  )
  
  out = list(bootstrap_summary, bootstrap_iter)
  
  names(out) = c("bootstrap_summary", "bootstrap_iter")
  
  return(out)
}
# 
# df1 = original_dataset
# df2 = attritioned_datasets[[10]]

.square <- function(x) {
  return(x^2)
}

calc_rmse = function(df1, df2){
  df1_cor = cor(df1, use = "pairwise.complete.obs")
  df2_cor = cor(df2, use = "pairwise.complete.obs")

  residuals = df1_cor - df2_cor

  residuals = residuals[lower.tri(residuals)]

  rmsr = sqrt(mean(.square(residuals)))

  return(rmsr)
}

calc_srmr = function(df1, df2){
  df1_cov = cov(df1, use = "pairwise.complete.obs")
  df2_cov = cov(df2, use = "pairwise.complete.obs")
  
  residuals = df1_cov - df2_cov
  
  df1_inv_sqrt_var = diag(1/sqrt(diag(df1_cov)))
  df2_inv_sqrt_var = diag(1/sqrt(diag(df2_cov))) # For some reason this isn't used for calculations 
  
  residuals = residuals    %*% df1_inv_sqrt_var 
  residuals = t(residuals) %*% df1_inv_sqrt_var
  residuals = t(residuals)
  
  residuals = residuals^2
  
  srmr = sqrt(mean(residuals[lower.tri(residuals, diag = TRUE)]))
  
  return(srmr)
}

calc_srmr2 = function(df1, df2){
  df1_cov = cov(df1, use = "pairwise.complete.obs")
  df2_cov = cov(df2, use = "pairwise.complete.obs")
  
  p = ncol(df1_cov)
  
  r = vector()
  
  for (i in 1:p){
    for (j in 1:i){
      x = (df1_cov[i,j]-df2_cov[i,j])/sqrt(df1_cov[i,i]*df1_cov[j,j])
      x = x^2
      r = c(r, x)
    }
  }
  
  if (p*(p+1)/2 != length(r)) stop("error")
  
  srmr = sqrt(mean(r))
  return(srmr)
}


# source("https://raw.githubusercontent.com/cran/glvmfit/4b4a3d28af5dbbfbeed4875cf811366aa76461a4/R/srmr.R")
# 
# srmr(S = cov(df2, use = "pairwise.complete.obs"), 
#      Sigma = cov(df1, use = "pairwise.complete.obs")
#      )
# 
# calc_srmr(df2, df1)
# calc_srmr(df1, df2)
# calc_srmr2(df1, df2) 
# 
# calc_rmse(df1, df2)
# 
# 1/sd(df1[[12]], na.rm = TRUE)

# x = compare_df(original_dataset, attritioned_datasets[[10]], B = 10)
# 
# 
# 
# Hmisc::summaryM()
# 
# rstatix::get_summary_stats
# 
# # hellinger_calc()
# 
# ggdist::mean_qi(bootstrap_iter$md )

# list(c(1,233,123,NA,213,2)) %>% cleanvar()
# 
# original_dataset[,1] %>% cleanvar()



library(ggplot2)
library(dplyr)

# Function to create ggplot of lower triangular correlation matrix
plot_lower_triangular_matrix <- function(
    data, 
    value_col    = "y", 
    pd_col       = "pd", 
    pd_threshold = 0.95,
    show_values  = TRUE,
    text_size    = 3, 
    # rq2y         = rqy2,
    # x_var        = x_var,
    # y_var        = y_var,
    title        = "Residual Correlation Matrix"
) {
  
  data$x_var = x_var
  data$y_var = y_var
  
  # Extract unique variable names
  # all_vars <- unique(c(data[["x_var"]], data[["y_var"]]))
  # n_vars <- length(all_vars)
  
  # Create a data frame for plotting
  plot_data <- data.frame()
  
  # Add correlation values for lower triangle
  for (i in 1:nrow(data)) {
    # x_var <- data[[x_var_col]][i]
    # y_var <- data[[y_var_col]][i]
    value <- data[[value_col]][i]
    pd_value <- data[[pd_col]][i]
    
    # Find positions
    # x_pos <- which(all_vars == x_var[i])
    # y_pos <- which(all_vars == y_var[i])
    
    x_pos <- which(data$x_var[i] == rq2y)
    y_pos <- which(data$y_var[i] == rq2y)
    
    # Only include lower triangle (where row > col)
    # if (x_pos > y_pos) {
      # Only color if pd value meets threshold, otherwise set to NA for no color
    fill_value <- ifelse(pd_value >= pd_threshold, value, NA)
      
      plot_data <- rbind(plot_data, data.frame(
        x = y_var[i],
        y = x_var[i],
        value = value,
        fill_value = fill_value,
        pd = pd_value
      ))
    # }
  }
  
  # Skip adding diagonal values - we want only lower triangle without diagonal
  
  # Set factor levels to control ordering
  plot_data$x <- factor(plot_data$x, levels = rq2y, labels = rq2y_labels)
  plot_data$y <- factor(plot_data$y, levels = rev(rq2y), labels = rev(rq2y_labels))
  
  # Format numbers without leading zero
  plot_data$formatted_value <- ifelse(plot_data$value >= 0,
                                      sub("^0", "", sprintf("%.3f", plot_data$value)),
                                      sub("^-0", "-", sprintf("%.3f", plot_data$value)))
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = x, y = y, fill = fill_value)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "darkgreen", 
                         midpoint = 0, name = "Correlation", 
                         limits = c(-0.1, 0.1), na.value = "white") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(hjust = 1),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    coord_equal() +
    labs(title = title)
  
  # Add correlation values as text if requested
  if (show_values) {
    p <- p + geom_text(aes(label = formatted_value), 
                       size = text_size, color = "black")
  }
  
  return(p)
}



# # Example usage:
# debug(plot_lower_triangular_matrix)
# 
# plot_lower_triangular_matrix(x)

