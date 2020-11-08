library(tidyverse)
load("datensaetze.Rdata")
load("port_syn_list.Rdata")
load("einkommen_syn_list.Rdata")
load("transf_syn_list.Rdata")

#helper function
#Bei einigen Variablen tritt das Problem auf, dass einzelne Kategorien so selten auftreten,
#dass sie im Trainingsdatensatz vorhanden sind im Testdatensatz aber nicht.
#aber nicht. Beispielsweise gibt es die Ausprägung von Medu = none im Datensatz port nur drei mal.
#Diese Funktion findet Variablen mit dieser Charakteristik.
detect_new_levels <- function(train,test) {
  fac_col <- colnames(dplyr::select_if(train,is.factor))
  res_log <- rep(FALSE,length(fac_col))
  res <- list()
  i <- 1
  for (col in fac_col) {
    org_lev <- levels(droplevels(train[[col]]))
    test_lev <- levels(droplevels(test[[col]]))
    new_lev <- setdiff(test_lev,org_lev)
    if(length(new_lev)>0){
      res_log[i] <- TRUE
      res[[col]] <- new_lev
    }
    i <- i+1
  }
  return(list(
    res_log = res_log,
    res_list = res
  ))
}


#main function
#Trainiert die logistischen Regressionsmodelle. Zusätzlich wird Kreuzvalidierung angewandt.
cv_fun <- function(data_o, syn_list, target, folds = 5,boot70 = FALSE){
  # tibble for model metrics:
  metrics_tbl <- tibble(method = "INIT",sig_o = 0,
                        sig_syn = 0, diff = list(1:3),
                        vorz = list(c(T,T,F)))
  set.seed(1)
  # create folds
  ind <- caret::createFolds(data_o[[target]], k = folds)
  train_o_list <- lapply(1:folds, function(i){
    data_o[-c(ind[[i]]),]
  })
  list_res <- lapply(names(syn_list), function(method_name){
    data_syn <- syn_list[[method_name]]
      res <- sapply(1:folds, function(i){
        train_o <- train_o_list[[i]]
        train_syn <- data_syn[-c(ind[[i]]),]
        #bootstrap unbalanced data
        if(boot70){
          train_o <- train_o %>%
            filter(G3 == "0") %>%
            sample_n(200,replace = TRUE) %>%
            bind_rows(filter(train_o,G3 == "1"))
          train_syn <- train_syn %>%
            filter(G3 == "0") %>%
            sample_n(200,replace = TRUE) %>%
            bind_rows(filter(train_syn,G3 == "1"))
        }
        test <- data_o[ind[[i]],]
        # deal with unseen levels in test data set
        detect_o <- detect_new_levels(train_o,test)
        detect_syn <- detect_new_levels(train_syn,test)
        if(!all(detect_o$res_log)){
          for(var in names(detect_o$res_list)){
            test <- test[!c(test[[var]] %in% detect_o$res_list[[var]]),]
          }
        }
        if(!all(detect_syn$res_log)){
          for(var in names(detect_syn$res_list)){
            test <- test[!c(test[[var]] %in% detect_syn$res_list[[var]]),]
          }
        }
        # fit models + auc
        mod_o <- glm(as.formula(paste0(target, "~.")), family = binomial, data = train_o)
        mod_syn <- glm(as.formula(paste0(target, "~.")), family = binomial, data = train_syn)
        pred_o <- predict(mod_o,newdata = test, type = "response")
        pred_syn <- predict(mod_syn,newdata = test, type = "response")
        auc_o <- ModelMetrics::auc(actual = test[[target]],predicted = pred_o)
        auc_syn <- ModelMetrics::auc(actual = test[[target]],predicted = pred_syn)
        # extraction model metrics:
        tidy_mod_o <- broom::tidy(mod_o)
        tidy_mod_syn <- broom::tidy(mod_syn)
        sig_o <- mean(tidy_mod_o$p.value <= 0.05)
        sig_syn <- mean(tidy_mod_syn$p.value <= 0.05)
        diff_vec <- tidy_mod_syn$estimate / tidy_mod_o$estimate
        vorzeichen <- sign(tidy_mod_syn$estimate) == sign(tidy_mod_o$estimate)
        metrics_tbl <<- metrics_tbl %>%
          bind_rows(tibble(method = method_name,
                           sig_o = sig_o,sig_syn = sig_syn,
                           diff = list(diff_vec),
                           vorz = list(vorzeichen)))
        
        return(c(auc_o, auc_syn))
      })
      list("auc-Matrix" = res , "auc-Werte" = rowMeans(res))
  })
  list_res$metrics <- metrics_tbl%>%
    filter(method != "INIT")
  return(list_res)
}

# Ergebnisse für die einzelnen Datenbeispielse
blood_cv <- cv_fun(transf, syn_list_trans, "march2007")

einkommen_s$sex <- as.factor(einkommen_s$sex)
einkommen_s$income <- as.factor(einkommen_s$income)
syn_list_eink$sample$sex <- as.factor(syn_list_eink$sample$sex)
syn_list_eink$sample$income <- as.factor(syn_list_eink$sample$income)
eink_cv <- cv_fun(einkommen_s, syn_list_eink, "income")

port_cv <- cv_fun(port, syn_list_port, "G3",boot70 = T)

# helper function to aggregate the data from metrics
aggr_metrics <- function(metrics_tbl){
  len_coef <- length(metrics_tbl$diff[[1]])
  diff_tbl <- paste0("diff_",seq(len_coef)) %>% 
    purrr::map_dfc(setNames,object = list(numeric(dim(metrics_tbl)[1])))
  vorz_tbl <- paste0("vorz_",seq(len_coef)) %>% 
    purrr::map_dfc(setNames,object = list(logical(dim(metrics_tbl)[1])))
  new_metrics <- metrics_tbl %>%
    dplyr::select(method,sig_o,sig_syn) %>%
    bind_cols(diff_tbl) %>%
    bind_cols(vorz_tbl)
  for (i in seq(dim(metrics_tbl)[1])) {
    for (j in seq(len_coef)) {
      new_metrics[[paste0("diff_",j)]][i] <- metrics_tbl$diff[[i]][j]
      new_metrics[[paste0("vorz_",j)]][i] <- metrics_tbl$vorz[[i]][j]
    }
  }
  new_metrics1 <- new_metrics %>%
    group_by(method) %>% 
    summarise(sig_o = mean(sig_o),
              sig_syn = mean(sig_syn)) %>%
    ungroup()
  new_metrics2 <- new_metrics %>%
    group_by(method) %>% 
    summarise_at(paste0("diff_",seq(len_coef)),
                 mean) %>%
    ungroup()
  new_metrics3 <- new_metrics %>%
    group_by(method) %>% 
    summarise_at(paste0("vorz_",seq(len_coef)),
                 sum) %>%
    ungroup()
  new_metrics <- new_metrics1 %>%
    left_join(new_metrics2, by = "method") %>%
    left_join(new_metrics3, by = "method")
  return(new_metrics)
}

#Ergebnisse in schönerer Form
blood_agg <- aggr_metrics(blood_cv$metrics)
eink_agg <- aggr_metrics(eink_cv$metrics)
port_agg <- aggr_metrics(port_cv$metrics)

# spalten samplen um beispiele zu extrahieren, weil bei den zwei letzten Datenbeispiele
#sonst zu unübersichtlich, wenn man alle Koeffizienten etc. vergleicht.
samp_cols_helper <- function(agg_df,n_samp,n_skip) {
  helper_length <- (dim(agg_df)[2]-n_skip)/2
  set.seed(9)
  samp_ind <- sample(seq(helper_length),n_samp)
  agg_df[,c(seq(n_skip),n_skip+samp_ind,n_skip+helper_length+samp_ind)]
}
port_agg_short <- samp_cols_helper(port_agg,4,3)
eink_agg_short <- samp_cols_helper(eink_agg,4,3)

