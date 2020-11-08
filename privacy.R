#######################################################################
                    # Funktionen zu disclosure risk
#####################################################################
#Packete
library(tidyverse)
library(ggpubr)
#Datensätze
load("datensaetze.Rdata")
load("port_syn_list.Rdata")
load("einkommen_syn_list.Rdata")
load("transf_syn_list.Rdata")

# Zeilenvergleich mit k=1 nächste Nachbarn zwischen dem Originaldatensazt und dem synthetischen
privacy_compare <- function(data, data_syn) {
  data <- mutate_if(data, is.numeric, scale)
  data_syn <- mutate_if(data_syn, is.numeric, scale)
  data_dummy <- fastDummies::dummy_cols(data, remove_first_dummy = T,
                           remove_selected_columns = T)
  data_syn_dummy <- fastDummies::dummy_cols(data_syn, remove_first_dummy = T,
                                            remove_selected_columns = T)
  dist_mat <- fields::rdist(data_syn_dummy,data_dummy)
  distances <- apply(dist_mat, 1, min)
  return(list(mean_min_dist = mean(distances),
              distances = distances,
              summary = summary(distances)))
}

#Ergebnisse der Zeilenvergleiche
#Für den Einkommensdatensatz
erg_eink <- lapply(syn_list_eink, function(x){
  privacy_compare(einkommen_s, x)
})
#Für den Schulleistungsdatensatz
erg_port <- lapply(syn_list_port, function(x){
  privacy_compare(port, x)
})
#Für den Bluttransfusionsdatensatz
erg_trans <- lapply(syn_list_trans, function(x){
  privacy_compare(transf, x)
})


#CAP Funktion
#Evaluierung der Güte der Vorhersage einer zufällig gewählten Zielvariable 
#mit unterschiedlichen bekannten Schlüsselvariablen (gute Güte ist hierbei schlecht)
cap_fun <- function(keys, target, data, data_syn){
  data <- mutate_if(data, is.character, as.factor)
  data_syn <- mutate_if(data_syn, is.character, as.factor)
  target <- colnames(data)[target]
  data_keys <- dplyr::select(data, keys)
  data_syn_keys <- dplyr::select(data_syn, keys)
  if(any(sapply(colnames(data_keys),function(name){class(data_keys[[name]])}) == "factor")){
    if(length(keys) == 1){
      data_dummy <- fastDummies::dummy_cols(data_keys, remove_first_dummy = T,
                                            remove_selected_columns = F)
      data_syn_dummy <- fastDummies::dummy_cols(data_syn_keys, remove_first_dummy = T,
                                                remove_selected_columns = F)
      data_dummy <- data_dummy[,-1]
      data_syn_dummy <- data_syn_dummy[,-1]
    } else {
      data_dummy <- fastDummies::dummy_cols(data_keys, remove_first_dummy = T,
                                             remove_selected_columns = T)
      data_syn_dummy <- fastDummies::dummy_cols(data_syn_keys, remove_first_dummy = T,
                                              remove_selected_columns = T)
    }
  } else {
    data_dummy <- data_keys
    data_syn_dummy <- data_syn_keys
  }
  # distance matrix (ith row distance of the ith synthesised row 
  # to all the rows of the original data set)
  dist_mat <- fields::rdist(data_syn_dummy,data_dummy)
  # colsums minus dist_mat
  dist_mat_temp <- matrix(rep(colSums(dist_mat),dim(dist_mat)[1]),
                          nrow = dim(dist_mat)[1],byrow = TRUE) - 
    dist_mat
  # invers zum Abstand gewichtete diskrete verteilung pro spalte (originale Zeile)
  # äquivalent zu cols of dist_mat / colsum, dann 1-dist_mat, dann auf colsum = 1 normalisieren.
  dist_mat <- t(t(dist_mat_temp)/colSums(dist_mat_temp))
  if(is.factor(data[[target]])){
    target_orig_num <- as.numeric(data[[target]])
    target_syn_num <- as.numeric(data_syn[[target]])
    # majority vote nach dem wahrscheinlichkeitsvektor
    pred_tbl <- as_tibble(dist_mat) %>%
      mutate(cats = target_syn_num) %>%
      group_by(cats) %>%
      summarise_all(sum)
    pred_tar <- as.matrix(pred_tbl[,(2:(dim(pred_tbl)[2]))]) %>%
      apply(2,which.max)
    accuracy <- mean(pred_tar == target_orig_num)
    return(c(accuracy = accuracy))
  } else {
    pred_tar <- as.vector(data_syn[[target]] %*% dist_mat)
    mse_pred <- ModelMetrics::mse(actual =  data[[target]],predicted = pred_tar)
    return(c(mse = mse_pred))
  }
} 

#CAP Funktion für alle synthetischen Datensätze eines Datenbeispiels mit jeweils
#zehn zufällig gewählten Szenarien
cap_loop_fun <- function(n_vec, data, data_syn_list, rep_num = 10){
  set.seed(6)
  col_num <- seq(dim(data)[2])
  res <- tibble(method = rep("",length(data_syn_list)*length(n_vec)*rep_num),
                n = 0, keys = "",
                target = "", mse = 0)
  key_tar_tbl <- tibble(n = rep(n_vec,each = rep_num),
                        tar = sample(col_num,rep_num*length(n_vec),replace = TRUE),
                        key = "",
                        id = 1:(rep_num*length(n_vec))) %>% 
    rowwise(id) %>%
    mutate(key = str_c(sample(col_num[col_num != tar],n),collapse = ","))
  #return(key_tar_tbl)
  j = 1
  for (name in names(data_syn_list)) {
    for (i in n_vec) {
      for (k in seq(rep_num)) {
        tar <- filter(key_tar_tbl,n == i)$tar[k] 
        key <- as.numeric(str_split(filter(key_tar_tbl,n == i)$key[k]
                                    ,",",simplify = TRUE))
        #print(list(tar,key))
        mse_temp <- cap_fun(keys = key,target = tar,data = data,
                            data_syn = data_syn_list[[name]])
        res[j,] <- list(method = name,n = i,
                        keys = str_c(key,collapse = ","),
                        target = as.character(tar),
                        mse = mse_temp)
        j <- j+1
      }
    }
  }
  return(res)
}


#Ergebnisse für den Bluttransfusionsdatensatz
theme_set(theme_classic()+
            theme(panel.grid.major.y = element_line(color = "lightgrey")))
col_pal_trans <- colorRampPalette(c("#f50519","#d1931f","#fce621"))

cap_df_transf <- cap_loop_fun(c(1,2,3),transf,syn_list_trans)
my_labels <- c(
  "1" = "Anzahl Keys: 1",
  "2" = "Anzahl Keys: 2",
  "3" = "Anzahl Keys: 3",
  "5" = "Anzahl Keys: 5",
  "10" = "Anzahl Keys: 10",
  "15" = "Anzahl Keys: 15"
)

#dazugehörige Visulaisierungen
my_labeller <- as_labeller(my_labels)

transf_mse_plot <- cap_df_transf %>%
  filter(target != "4") %>%
  ggplot(aes(x = method,y = mse,col = target))+
  geom_jitter(width = 0,height = 0.01,alpha = 0.6)+
  facet_wrap(~factor(n),labeller = my_labeller)+
  scale_color_manual(values = c(col_pal_trans(3)[1:2],"gold"),
                     labels = names(transf)[1:3])+
  theme(panel.border = element_rect(color = "black",fill = NA))

transf_acc_plot <- cap_df_transf %>%
  filter(target == "4") %>%
  ggplot(aes(x = method,y = mse,col = target))+
  geom_jitter(width = 0,height = 0,alpha = 0.6)+
  facet_wrap(~factor(n),labeller = my_labeller)+
  scale_color_manual(values = c(col_pal_trans(3)[1:2],"gold"),
                     labels = "march2007")+
  labs(y = "accuracy")+
  theme(panel.border = element_rect(color = "black",fill = NA))

ggarrange(transf_acc_plot,transf_mse_plot,nrow = 2,ncol = 1)

# Ergebnisse für den Einkommensdatensatz und deren Visualisierung
col_pal_einkommen_plus <- colorRampPalette(c("#05eefa","#1f0399","#B43ED1",
                                             "#D69FED", "#329c9a", "#51E0C3",
                                             "#057bfa"))
cap_df_eink <- cap_loop_fun(c(1,3,5),einkommen_s,syn_list_eink)

eink_mse_plot <- cap_df_eink %>%
  filter((target %in% c(1,3,8))) %>%
  ggplot(aes(x = method,y = mse,col = target))+
  geom_jitter(width = 0,height = 5,alpha = 0.8)+
  facet_wrap(~factor(n),labeller = my_labeller)+
  scale_color_manual(values = col_pal_einkommen_plus(3),
                     labels = c("age","education","hours per week"))+
  theme(panel.border = element_rect(color = "black",fill = NA))

eink_acc_plot <- cap_df_eink %>%
  filter(!(target %in% c(1,3,8))) %>%
  mutate(target = factor(target,levels = paste(c(2,4,5,6,7,9,10))))%>%
  ggplot(aes(x = method,y = mse,col = target))+
  geom_jitter(width = 0,height = 0.02,alpha = 0.8)+
  stat_summary(aes(y = mse,group=1), fun=mean,
               colour="#E61983", geom="line",group=1,
               alpha = 0.4,size = 1.2)+
  facet_wrap(~factor(n),labeller = my_labeller)+
  scale_color_manual(values = col_pal_einkommen_plus(7),
                     labels = names(einkommen_s)[c(2,4,5,6,7,9,10)])+
  labs(y = "accuracy")+
  theme(panel.border = element_rect(color = "black",fill = NA))

ggarrange(eink_acc_plot,eink_mse_plot,nrow = 2,ncol = 1)

#Ergebnisse für den Schulleistungsdatensatz und deren Visualisierung
col_pal_port <- colorRampPalette(c("#33821f","#2fd106", "#5dfc9d"))
cap_df_port <- cap_loop_fun(c(1,3,5,10,15),port,syn_list_port)

port_mse_plot <- cap_df_port %>%
  filter((target %in% c(3,30))) %>%
  ggplot(aes(x = method,y = mse,col = target))+
  geom_jitter(width = 0,height = 3,alpha = 0.8)+
  facet_wrap(~factor(n),labeller = my_labeller)+
  scale_color_manual(values = col_pal_port(3)[1:2],
                     labels = c("age","absences"))+
  theme(panel.border = element_rect(color = "black",fill = NA))

port_acc_plot <- cap_df_port %>%
  filter(!(target %in% c(3,30))) %>%
  mutate(target = factor(target,levels = paste(c(1,2,4:29,31))))%>%
  ggplot(aes(x = method,y = mse,col = target))+
  stat_summary(aes(y = mse,group=1), fun=mean,
               colour="#E61983", geom="line",group=1,
               alpha = 0.4,size = 1.2)+
  geom_jitter(width = 0,height = 0,alpha = 0.8)+
  facet_wrap(~factor(n),labeller = my_labeller)+
  scale_color_manual(values = col_pal_port(24))+
  labs(y = "accuracy")+
  theme(panel.border = element_rect(color = "black",fill = NA),
        legend.position = "none")

ggarrange(port_acc_plot,port_mse_plot,nrow = 2,ncol = 1,heights = c(2,1))


