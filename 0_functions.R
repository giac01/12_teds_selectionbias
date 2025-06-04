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


calc_auc = function(x, printplot = FALSE){
  outcome_name    = all.vars(formula(x))[1]
  
  # Get the complete cases from the original data
  complete_cases <- complete.cases(x$data[, c(outcome_name, all.vars(formula(x))[-1])])
  
  # Extract outcome values for complete cases only
  outcome_values <- x$data[[outcome_name]][complete_cases]
  predicted_probs <- predict(x, type = "response")
  
  roc_obj         = pROC::roc(outcome_values, predicted_probs, quiet = TRUE)
  
  if (printplot) plot(roc_obj)
  
  auc             = pROC::auc(roc_obj, quiet = TRUE)
  
  return(auc)
  
}

var_to_label = function(var, df0 = df){
  # df0 = select(df0, all_of(var))
  df0 = df0[var]
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
  out      = ggdist::mean_qi(x, .width = .width, na.rm = TRUE)
  pd       = as.numeric(bayestestR::p_direction(x))
  pval     = bayestestR::pd_to_p(pd)
  out$pd   = pd
  out$pval = pval
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


# 
# library(ggplot2)
# library(dplyr)

# Function to create ggplot of lower triangular correlation matrix
plot_lower_triangular_matrix <- function(
    data, 
    p_threshold = 0.05,
    show_values  = TRUE,
    text_size    = 3,
    title = "plot title",
    caption = "plot subtitle"

) {
  
  rq2y <- get("rq2y", envir = parent.frame())
  rq2y_labels_short <- get("rq2y_labels_short", envir = parent.frame())
  
  data$x_var = get("x_var", envir = parent.frame())
  data$y_var = get("y_var", envir = parent.frame())
  
  # Extract unique variable names
  # all_vars <- unique(c(data[["x_var"]], data[["y_var"]]))
  # n_vars <- length(all_vars)
  
  # Create a data frame for plotting
  plot_data <- data.frame()
  
  # Add correlation values for lower triangle
  for (i in 1:nrow(data)) {
    value   <- data[["y"]][i]
    p_value <- data[["pval_adj"]][i]
    

    fill_value <- ifelse(p_value < p_threshold, value, NA)
      
      plot_data <- rbind(plot_data, data.frame(
        x = y_var[i],
        y = x_var[i],
        value = value,
        fill_value = fill_value
      ))
    # }
  }
  
  # Skip adding diagonal values - we want only lower triangle without diagonal
  
  # Set factor levels to control ordering
  plot_data$x <- factor(plot_data$x, levels = rq2y,      labels = rq2y_labels_short)
  plot_data$y <- factor(plot_data$y, levels = rev(rq2y), labels = rev(rq2y_labels_short))
  
  plot_data$x <- droplevels(plot_data$x)
  plot_data$y <- droplevels(plot_data$y)
  
  # Format numbers without leading zero
  plot_data$formatted_value <- ifelse(plot_data$value >= 0,
                                      sub("^0", "", sprintf("%.3f", plot_data$value)),
                                      sub("^-0", "-", sprintf("%.3f", plot_data$value)))
  
  
  if (length(which(!is.na(plot_data$fill_value)))==0) { plot_data$fill_value = 0 }
  
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
    labs(
      title = title, 
      caption = caption
    ) +
    theme(legend.position = "none")
  
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

calc_ace = function(
    data = NULL,
    var  = c("bvocab")
    
    
    ){
  
  if (length(var)!=1) stop("length(var) should equal 1")
  
  data_mz = data %>%
    filter(x3zygos=="MZ")
  
  data_dz = data %>%
    filter(x3zygos=="DZ same sex")
  
  ace_results  = vector()
  
  mz_cor = cor(
    as.numeric(data_mz[[paste0(var,"1")]]),
    as.numeric(data_mz[[paste0(var,"2")]]),
    use = "pairwise.complete.obs"
  )

  dz_cor = cor(
    as.numeric(data_dz[[paste0(var,"1")]]),
    as.numeric(data_dz[[paste0(var,"2")]]),
    use = "pairwise.complete.obs"
  )
  
  a = 2*(mz_cor - dz_cor)
  
  c = 2*dz_cor - mz_cor
  
  e = 1 - mz_cor
  
  ace_results = c(a, c, e)
  
  names(ace_results) = c("a", "c", "e")
    
  return(ace_results)
}



# var = "bvocab"

compare_ace = function(
    var = var,
    B = 10
    ){

  boot_results_original = list() # bootstrapped results for non-attrition dataset
  boot_results = list()          # bootstrapped results for 
  
  original_dataset_twin1     = get("original_dataset_twin1",     envir = parent.frame())
  attritioned_datasets_twin1 = get("attritioned_datasets_twin1", envir = parent.frame())
  
  
  if (length(var)>1) stop("length(var) should equal 1")
  if (nrow(original_dataset_twin1)!=nrow(attritioned_datasets_twin1[[1]])) stop("nrows should match")

  for (i in 1:B){
    
    boot_select = sample(nrow(original_dataset_twin1), nrow(original_dataset_twin1), replace = TRUE)
    
    boot_results_original[[i]] = calc_ace(original_dataset_twin1[boot_select,], var = var)
    
    boot_results[[i]] = list()
    
    for (j in seq_along(rq1y)){
      
      boot_results[[i]][[j]] = calc_ace(data=attritioned_datasets_twin1[[j]][boot_select,], var = var)
      
    }
    
    names(boot_results[[i]]) = rq1y

  }

  boot_results_original_df = data.frame(do.call("rbind", boot_results_original))
  
  
  boot_results_df <- lapply(rq1y, function(selection_group) {
    lapply(boot_results, function(x) x[[selection_group]])
  })
  
  boot_results_df = lapply(boot_results_df, function(x) data.frame(do.call('rbind', x)))
  
  names(boot_results_df) = rq1y
  
  # Compare changes in ace model parameters
  
  boot_results_differences = list()
  
  for(i in seq_along(rq1y)){
    boot_results_differences[[i]] = as.matrix(boot_results_original_df) - as.matrix(boot_results_df[[i]])
  }
  
  boot_results_differences_summary = lapply(boot_results_differences, function(x)
    apply(x,2,.mean_qi_pd)
  )
  
  names(boot_results_differences_summary) = rq1y
  
  out = list(
    boot_results_differences_summary,
    boot_results_differences,
    boot_results_df,
    boot_results_original
  )
  
  names(out) = c("boot_results_differences_summary", "boot_results_differences", "boot_results_df")

  return(out)
}


glm_model_comparison = function(full_model) {
  
  # Get drop1 results
  drop_results    = drop1(full_model, test = "LRT")
  drop_results_df = data.frame(drop_results)
  
  # Get variable names
  vars_to_test    = all.vars(formula(full_model))[-1]
  outcome         = all.vars(formula(full_model))[1]
  
  # Fit Intercept-Only Model
  
  used_rows       = as.numeric(names(residuals(full_model)))
  
  if(length(used_rows)!=nobs(full_model)) stop("sample size does not match between comparisons A")
  
  subset_data     = full_model$data[used_rows, ]
  
  null_model      = update(
    full_model, 
    . ~ 1,
    data = subset_data
    )
  
  if (nobs(null_model)!=nobs(full_model)) stop("sample size does not match between comparisons B")
  
  # Initialize results
  comparison_df = data.frame(
    # Model             = c("Full", paste0("Drop_", vars_to_test)),
    Variables_Dropped = c("None", vars_to_test),
    Variables_full    = c("None", var_to_label(vars_to_test)),
    outcome           = rep(outcome, length(vars_to_test)+1),
    nobs              = integer(length(vars_to_test) + 1),
    
    logLik            = numeric(length(vars_to_test) + 1),
    AIC               = numeric(length(vars_to_test) + 1),
    BIC               = numeric(length(vars_to_test) + 1),
    AUC               = numeric(length(vars_to_test) + 1),
    mcfad_r2          = numeric(length(vars_to_test) + 1),

    Delta_logLik      = numeric(length(vars_to_test) + 1),
    Delta_AIC         = numeric(length(vars_to_test) + 1),
    Delta_BIC         = numeric(length(vars_to_test) + 1),
    Delta_AUC         = numeric(length(vars_to_test) + 1),
    Delta_mcfad_r2    = numeric(length(vars_to_test) + 1),

    stringsAsFactors  = FALSE
  )
  
  # Full model metrics
  comparison_df$nobs[1]         = nobs(full_model)
  comparison_df$logLik[1]       = as.numeric(logLik(full_model))
  comparison_df$AIC[1]          = AIC(full_model)
  comparison_df$BIC[1]          = BIC(full_model)
  comparison_df$AUC[1]          = calc_auc(full_model)
  comparison_df$mcfad_r2[1]     = 1 - logLik(full_model)/logLik(null_model)
  

  
  # Reduced models
  for(i in seq_along(vars_to_test)) {
    var                                 = vars_to_test[i]
    reduced_formula                     = update(formula(full_model), paste("~ . -", var))
    reduced_model                       = glm(reduced_formula, data = full_model$model, family = full_model$family) # full_model$model should only include the complete case data (removing rows with missing predictor values)
    comparison_df$nobs[i + 1]           = nobs(reduced_model)
    
    if(nobs(reduced_model)!=nobs(full_model)) stop("non match nobs!")
    
    comparison_df$logLik[i + 1]         = as.numeric(logLik(reduced_model))
    comparison_df$AIC[i + 1]            = AIC(reduced_model)
    comparison_df$BIC[i + 1]            = BIC(reduced_model)
    comparison_df$AUC[i + 1]            = calc_auc(reduced_model)
    comparison_df$mcfad_r2[i + 1]       = 1 - logLik(reduced_model)/logLik(null_model)
    
    comparison_df$Delta_logLik[i + 1]   = as.numeric(logLik(full_model)) - as.numeric(logLik(reduced_model))
    comparison_df$Delta_AIC[i + 1]      = AIC(reduced_model) - AIC(full_model)
    comparison_df$Delta_BIC[i + 1]      = BIC(reduced_model) - BIC(full_model)
    comparison_df$Delta_AUC[i + 1]      = comparison_df$AUC[i + 1] - comparison_df$AUC[1]
    comparison_df$Delta_mcfad_r2[i + 1] = comparison_df$mcfad_r2[i + 1] - comparison_df$mcfad_r2[1]
    
  }
  
  comparison_df$df    = 0
  comparison_df$df    = drop_results_df$Df[match(comparison_df$Variables_Dropped, rownames(drop_results_df))]
  comparison_df$LRT   = 0
  comparison_df$LRT   = drop_results_df$LRT[match(comparison_df$Variables_Dropped, rownames(drop_results_df))]
  comparison_df$LRT_p = drop_results_df$Pr..Chi.[match(comparison_df$Variables_Dropped, rownames(drop_results_df))]
  
  
  # Sort by Delta_AIC (most important variables have largest positive Delta_AIC)
  comparison_df = comparison_df[order(comparison_df$LRT_p, decreasing = TRUE), ]
  
  return(comparison_df)
}
