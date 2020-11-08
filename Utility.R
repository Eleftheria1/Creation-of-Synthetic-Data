#######################################################################
        # Evaluierung des Nutzens der synthetischen Datensätze
#####################################################################
# Verteilungsvergleich: boxplots/ hist für cont und barplots für categorical

#Lade Datensätze und benötigte Packete
load("datensaetze.Rdata")
load("port_syn_list.Rdata")
load("einkommen_syn_list.Rdata")
load("transf_syn_list.Rdata")

library(tidyverse)
library(ggpubr)
#Erstelle theme
my_grey <-c("#A8A8A8")
theme_set(theme_classic()+
            theme(panel.grid.major.y = element_line(color = "lightgrey")))

############################################################################
                          # blood transfusion
############################################################################
col_pal_trans <- colorRampPalette(c("#f50519","#d1931f","#fce621"))

transf_march_plot <- dplyr::select(transf,march2007) %>% mutate(Method = "observed")
for (name in names(syn_list_trans)) {
  transf_march_plot <- transf_march_plot %>%
    bind_rows(mutate(dplyr::select(syn_list_trans[[name]],march2007),Method = name))
}

#Für die kategorische Variable march2007 Histogramme:
# Non-parametric
np_transf_march <- transf_march_plot %>%
  filter(!(Method %in% c("norm","normrank"))) %>%
  mutate(Method = relevel(factor(Method),"observed")) %>%
  ggplot(aes(x = march2007,fill = Method)) +
    geom_bar(position = "dodge")+
    scale_fill_manual(values = c(my_grey,col_pal_trans(3)))+
    labs(y = "Anzahl",title = "Nicht parametrische Modelle")

# Parametric
p_transf_march <- transf_march_plot %>%
  filter(!(Method %in% c("cart","rf","sample"))) %>%
  mutate(Method = relevel(factor(Method),"observed")) %>%
  ggplot(aes(x = march2007,fill = Method)) +
    geom_bar(position = "dodge")+
    scale_fill_manual(values = c(my_grey,col_pal_trans(2)))+
    labs(y = "Anzahl",title = "Parametrische Modelle")

ggarrange(np_transf_march,p_transf_march)

#Für die metrischen Variablen Dichtefunktionen
transf_c_plot <- dplyr::select(transf,First,Last,Frequency) %>% mutate(Method = "observed")
for (name in names(syn_list_trans)) {
  transf_c_plot <- transf_c_plot %>%
    bind_rows(mutate(dplyr::select(syn_list_trans[[name]],First,Last,Frequency),Method = name))
}
transf_c_plot <- pivot_longer(transf_c_plot,-Method,names_to = "var")


mybw <- 0.25
transf_cont_plots <- list()
# nicht parametrisch
for (varb in c("First","Last","Frequency")) {
  titl1 <- ifelse(varb == "Last","Nicht parametrische Modelle","")
  titl2 <- ifelse(varb == "Last","Parametrische Modelle","")
  transf_cont_plots[[paste0(varb,"np")]] <- 
    transf_c_plot %>%
    filter(var == varb) %>%
    filter(!(Method %in% c("norm","normrank")))%>%
    mutate(Method = relevel(factor(Method),"observed"))%>%
    ggplot(aes(x = value,col = Method,fill = Method))+
    geom_density(bw = mybw,size = 0.8,alpha = 0.5)+
    scale_color_manual(values = c(my_grey,col_pal_trans(3)))+
    scale_fill_manual(values = c(my_grey,NA,NA,NA))+
    scale_y_continuous(breaks = seq(0,1,0.2))+
    xlim(0,NA)+
    labs(x = varb,title = titl1,
         caption = paste("bandwidth =",mybw))
  
 #parametrisch
  transf_cont_plots[[paste0(varb,"p")]] <- 
    transf_c_plot %>%
    filter(var == varb) %>%
    filter(!(Method %in% c("cart","rf","sample")))%>%
    mutate(Method = relevel(factor(Method),"observed"))%>%
    ggplot(aes(x = value,col = Method,fill = Method))+
    geom_density(bw = mybw,size = 0.8,alpha = 0.5)+
    scale_color_manual(values = c(my_grey,col_pal_trans(2)))+
    scale_fill_manual(values = c(my_grey,NA,NA))+
    scale_y_continuous(breaks = seq(0,1,0.2))+
    labs(x = varb,title = titl2,
         caption = paste("bandwidth =",mybw))+
    xlim(0,NA)
}

#zusammen
ggarrange(ggarrange(transf_cont_plots[[1]],
                    transf_cont_plots[[3]],
                    transf_cont_plots[[5]],
                    common.legend = TRUE,legend = "right",
                    ncol = 3,nrow = 1),
          ggarrange(transf_cont_plots[[2]],
                    transf_cont_plots[[4]],
                    transf_cont_plots[[6]],
                    common.legend = TRUE,legend = "right",
                    ncol = 3,nrow = 1),
          nrow = 2,ncol = 1
          )

# korrelationsmatrizen (differenzen) visualisiert
#cart
corrplot::corrplot((cor(dplyr::select(transf,-march2007))-
                      cor(dplyr::select(syn_list_trans$cart,-march2007))),
                   method = "ellipse",type = "lower",addCoef.col = NULL,
                   tl.col = "black",diag = F)
#random forest
corrplot::corrplot((cor(dplyr::select(transf,-march2007))-
                      cor(dplyr::select(syn_list_trans$rf,-march2007))),
                   method = "ellipse",type = "lower",addCoef.col = NULL,
                   tl.col = "black",diag = F,
                   title = "Methode: rf")
#sample
corrplot::corrplot((cor(dplyr::select(transf,-march2007))-
                      cor(dplyr::select(syn_list_trans$sample,-march2007))),
                   method = "ellipse",type = "lower",addCoef.col = NULL,
                   tl.col = "black",diag = F,
                   title = "Methode: Sample")
#norm
corrplot::corrplot((cor(dplyr::select(transf,-march2007))-
                      cor(dplyr::select(syn_list_trans$norm,-march2007))),
                   method = "ellipse",type = "lower",addCoef.col = NULL,
                   tl.col = "black",diag = F,
                   title = "Methode: Norm")
#normrank
corrplot::corrplot((cor(dplyr::select(transf,-march2007))-
                      cor(dplyr::select(syn_list_trans$normrank,-march2007))),
                   method = "ellipse",type = "lower",addCoef.col = NULL,
                   tl.col = "black",diag = F,
                   title = "Methode: Normrank")

#auf kategorische variable bedingte summary statistiken
#original
transf %>%
  dplyr::group_by(march2007)%>%
  summarise(mean_first = mean(First),
            mean_last = mean(Last),
            mean_freq = mean(Frequency),
            median_first = median(First),
            median_last = median(Last),
            median_freq = median(Frequency)) %>%
  ungroup()
#cart
syn_list_trans$cart %>%
  dplyr::group_by(march2007)%>%
  summarise(mean_first = mean(First),
            mean_last = mean(Last),
            mean_freq = mean(Frequency),
            median_first = median(First),
            median_last = median(Last),
            median_freq = median(Frequency)) %>%
  ungroup()
#random forest
syn_list_trans$rf %>%
  dplyr::group_by(march2007)%>%
  summarise(mean_first = mean(First),
            mean_last = mean(Last),
            mean_freq = mean(Frequency),
            median_first = median(First),
            median_last = median(Last),
            median_freq = median(Frequency)) %>%
  ungroup()
#sample
syn_list_trans$sample %>%
  dplyr::group_by(march2007)%>%
  summarise(mean_first = mean(First),
            mean_last = mean(Last),
            mean_freq = mean(Frequency),
            median_first = median(First),
            median_last = median(Last),
            median_freq = median(Frequency)) %>%
  ungroup()
#norm
syn_list_trans$norm %>%
  dplyr::group_by(march2007)%>%
  summarise(mean_first = mean(First),
            mean_last = mean(Last),
            mean_freq = mean(Frequency),
            median_first = median(First),
            median_last = median(Last),
            median_freq = median(Frequency)) %>%
  ungroup()
#normrank
syn_list_trans$normrank %>%
  dplyr::group_by(march2007)%>%
  summarise(mean_first = mean(First),
            mean_last = mean(Last),
            mean_freq = mean(Frequency),
            median_first = median(First),
            median_last = median(Last),
            median_freq = median(Frequency)) %>%
  ungroup()


############################################################################
                                # einkommen
############################################################################
col_pal_einkommen <- colorRampPalette(c("#05eefa","#1f0399", "#329c9a", "#ceeef5","#057bfa" ))


# Für die metrischen Variablen Dichtefunktionen
eink_c_plot <- dplyr::select(einkommen_s,age,education,hours.per.week) %>% mutate(Method = "observed")
for (name in names(syn_list_eink)) {
  eink_c_plot <- eink_c_plot %>%
    bind_rows(mutate(dplyr::select(syn_list_eink[[name]],age,education,hours.per.week),Method = name))
}
eink_c_plot <- pivot_longer(eink_c_plot,-Method,names_to = "var")
mybw_eink <- c("age" = 3,"education" = 1,"hours.per.week" = 4)

eink_cont_plots <- list()
for (varb in c("age","education","hours.per.week")) {
  titl1 <- ifelse(varb == "education","Nicht parametrische Modelle","")
  titl2 <- ifelse(varb == "education","Parametrische Modelle","")
  
  # nicht parametrisch
  eink_cont_plots[[paste0(varb,"np")]] <- 
    eink_c_plot %>%
    filter(var == varb) %>%
    filter(!(Method %in% c("polyreg")))%>%
    mutate(Method = relevel(factor(Method),"observed"))%>%
    ggplot(aes(x = value,col = Method,fill = Method))+
    geom_density(bw = mybw_eink[varb],size = 0.8,alpha = 0.5)+
    scale_color_manual(values = c(my_grey,col_pal_einkommen(3)))+
    scale_fill_manual(values = c(my_grey,NA,NA,NA))+
    scale_y_continuous(breaks = seq(0,1,0.2))+
    xlim(0,NA)+
    labs(x = varb,title = titl1,
         caption = paste("bandwidth =",mybw_eink[varb]))
  #parametrisch
  eink_cont_plots[[paste0(varb,"p")]] <- 
    eink_c_plot %>%
    filter(var == varb) %>%
    filter(!(Method %in% c("cart","rf","sample")))%>%
    mutate(Method = relevel(factor(Method),"observed"))%>%
    ggplot(aes(x = value,col = Method,fill = Method))+
    geom_density(bw = mybw_eink[varb],size = 0.8,alpha = 0.5)+
    scale_color_manual(values = c(my_grey,col_pal_einkommen(1)))+
    scale_fill_manual(values = c(my_grey,NA,NA))+
    scale_y_continuous(breaks = seq(0,1,0.2))+
    labs(x = varb,title = titl2,
         caption = paste("bandwidth =",mybw_eink[varb]))+
    xlim(0,NA)
}

#Zusammengefasst
ggarrange(ggarrange(eink_cont_plots[[1]],
                    eink_cont_plots[[3]],
                    eink_cont_plots[[5]],
                    common.legend = TRUE,legend = "right",
                    ncol = 3,nrow = 1),
          ggarrange(eink_cont_plots[[2]],
                    eink_cont_plots[[4]],
                    eink_cont_plots[[6]],
                    common.legend = TRUE,legend = "right",
                    ncol = 3,nrow = 1),
          nrow = 2,ncol = 1
)

#Für die kategoriellen Variablen Histogramme
eink_cat_plot <- dplyr::select(einkommen_s,-age,-education,-hours.per.week) %>% mutate(Method = "observed")
for (name in names(syn_list_eink)) {
  eink_cat_plot <- eink_cat_plot %>%
    bind_rows(mutate(dplyr::select(syn_list_eink[[name]],-age,-education,-hours.per.week),Method = name))
}
eink_cat_plot <- pivot_longer(eink_cat_plot,-Method,names_to = "var")


# nicht parametrisch
eink_cat_plot %>%
  filter(!(Method %in% c("polyreg")))%>%
 #filter(var %in% c("income","sex","marital.status")) %>%
  mutate(value = if_else(value == "<=50K","\U02264 50K",value))%>%
  mutate(Method = relevel(factor(Method),"observed"))%>%
  ggplot(aes(x = factor(value),fill = Method)) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c(my_grey,col_pal_einkommen(3)))+
  labs(y = "Anzahl",x = "",title = "Nicht parametrische Modelle")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))+
  facet_wrap(~var,scales = "free",nrow = 2,ncol = 4,strip.position = "bottom")

# parametrisch
eink_cat_plot %>%
  filter(!(Method %in% c("cart","rf","sample")))%>%
  mutate(value = if_else(value == "<=50K","\U02264 50K",value))%>%
  mutate(Method = relevel(factor(Method),"observed"))%>%
  ggplot(aes(x = factor(value),fill = Method)) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c(my_grey,col_pal_einkommen(3)))+
  labs(y = "Anzahl",title = "Parametrische Modelle",x = "")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))+
  facet_wrap(~var,scales = "free",nrow = 2,ncol = 4,strip.position = "bottom")

# corr matrix differenzen
#cart
corrplot::corrplot((cor(dplyr::select(einkommen_s,age,education,hours.per.week))-
                      cor(dplyr::select(syn_list_eink$cart,age,education,hours.per.week))),
                   method = "ellipse",type = "lower",addCoef.col = NULL,
                   tl.col = "black",diag = F,
                   title = "Methode: Cart")
#random forest
corrplot::corrplot((cor(dplyr::select(einkommen_s,age,education,hours.per.week))-
                      cor(dplyr::select(syn_list_eink$rf,age,education,hours.per.week))),
                   method = "ellipse",type = "lower",addCoef.col = NULL,
                   tl.col = "black",diag = F,
                   title = "Methode: rf")
#sample
corrplot::corrplot((cor(dplyr::select(einkommen_s,age,education,hours.per.week))-
                      cor(dplyr::select(syn_list_eink$sample,age,education,hours.per.week))),
                   method = "ellipse",type = "lower",addCoef.col = NULL,
                   tl.col = "black",diag = F,
                   title = "Methode: Sample")
#polyreg
corrplot::corrplot((cor(dplyr::select(einkommen_s,age,education,hours.per.week))-
                      cor(dplyr::select(syn_list_eink$polyreg,age,education,hours.per.week))),
                   method = "ellipse",type = "lower",addCoef.col = NULL,
                   tl.col = "black",diag = F,
                   title = "Methode: Polyreg")


# kreuztabelle
catvars_eink <- names(einkommen_s)[-c(1,3,8)]

create_big_contingency <- function(df,cat_var_names) {
  kreuztab_list <- list()
  for (varb in cat_var_names) {
    temp_list <- sapply(cat_var_names,function(varb2){
      table(df[[varb]],df[[varb2]])
    },simplify = F)
    temp_mat <- temp_list[[1]]
    for (i in seq(temp_list)[-1]) {
      temp_mat <- cbind(temp_mat,temp_list[[i]])
    }
    kreuztab_list[[varb]] <- temp_mat
  }
  final_mat <- kreuztab_list[[1]]
  for (j in seq(kreuztab_list)[-1]) {
    final_mat <- rbind(final_mat,kreuztab_list[[j]])
  }
  return(final_mat)
}
#Original
cont_table_eink <- create_big_contingency(einkommen_s,catvars_eink)
#cart
cont_table_eink_cart <- create_big_contingency(syn_list_eink$cart,catvars_eink)
#sample
cont_table_eink_sample <- create_big_contingency(syn_list_eink$sample,catvars_eink)
#random forest
cont_table_eink_rf <- create_big_contingency(syn_list_eink$rf,catvars_eink)
#polyreg
cont_table_eink_polyreg <- create_big_contingency(syn_list_eink$polyreg,catvars_eink)

# verhältnisse
#cart
verhaeltnis_cart <- (cont_table_eink_cart)/(cont_table_eink)
verhaeltnis_cart[is.nan(verhaeltnis_cart)|is.infinite(verhaeltnis_cart)] <- 
  cont_table_eink_cart[is.nan(verhaeltnis_cart)|is.infinite(verhaeltnis_cart)]+1

#sample
verhaeltnis_sample <- (cont_table_eink_sample)/(cont_table_eink)
verhaeltnis_sample[is.nan(verhaeltnis_sample)|is.infinite(verhaeltnis_sample)] <- 
  cont_table_eink_sample[is.nan(verhaeltnis_sample)|is.infinite(verhaeltnis_sample)]+1

#random forest
verhaeltnis_rf <- (cont_table_eink_rf)/(cont_table_eink)
verhaeltnis_rf[is.nan(verhaeltnis_rf)|is.infinite(verhaeltnis_rf)] <- 
  cont_table_eink_rf[is.nan(verhaeltnis_rf)|is.infinite(verhaeltnis_rf)]+1

#polyreg
verhaeltnis_polyreg <- (cont_table_eink_polyreg)/(cont_table_eink)
verhaeltnis_polyreg[is.nan(verhaeltnis_polyreg)|is.infinite(verhaeltnis_polyreg)] <- 
  cont_table_eink_polyreg[is.nan(verhaeltnis_polyreg)|is.infinite(verhaeltnis_polyreg)]+1

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
lower_tri_cart <- get_lower_tri(verhaeltnis_cart)
lower_tri_rf <- get_lower_tri(verhaeltnis_rf)
lower_tri_sample <- get_lower_tri(verhaeltnis_sample)
lower_tri_polyreg <- get_lower_tri(verhaeltnis_polyreg)

colnames_kreuz_einkommen <- colnames(lower_tri_cart)
colnames_kreuz_einkommen[c(13,17,21)]<- c("Other1","Other2","Other3")
colnames(lower_tri_cart) <- colnames_kreuz_einkommen
rownames(lower_tri_cart) <- colnames_kreuz_einkommen
colnames(lower_tri_rf) <- colnames_kreuz_einkommen
rownames(lower_tri_rf) <- colnames_kreuz_einkommen
colnames(lower_tri_sample) <- colnames_kreuz_einkommen
rownames(lower_tri_sample) <- colnames_kreuz_einkommen
colnames(lower_tri_polyreg) <- colnames_kreuz_einkommen
rownames(lower_tri_polyreg) <- colnames_kreuz_einkommen

#Visualisierung des unteren Dreiecks der Verhältniskontingenztabelle
plot_lower_tri <- function(lower_tri,col_pal,col_pal_num,
                           col_pal_sel,method_name,txt_size = 10,
                           scale_lims = NULL,axis_txt = TRUE,
                           low_col = NULL,mid_col = "white",high_col = NULL) {
  if(is.null(scale_lims)){scale_lims <- c(NA,NA)}
  melted_cormat <- reshape2::melt(lower_tri, na.rm = TRUE)
  if(axis_txt){
    ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "lightgrey")+
      scale_fill_gradient2(low = ifelse(is.null(low_col),col_pal(col_pal_num)[col_pal_sel[1]],low_col),
                           high = ifelse(is.null(high_col),col_pal(col_pal_num)[col_pal_sel[2]],high_col),
                           mid = ifelse(is.null(mid_col),col_pal(col_pal_num)[col_pal_sel[3]],mid_col),
                           limit = scale_lims,
                           midpoint = 1,  space = "Lab", name = "Verhältnis") +
      labs(x = "", y = "", title = paste("Method:",method_name)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = txt_size, hjust = 1))+
      theme(panel.grid.major.x = element_line(color = "lightgrey"), panel.grid.major.y = element_blank())+
      coord_fixed()
  } else {
    ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = ifelse(is.null(low_col),col_pal(col_pal_num)[col_pal_sel[1]],low_col),
                           high = ifelse(is.null(high_col),col_pal(col_pal_num)[col_pal_sel[2]],high_col),
                           mid = ifelse(is.null(mid_col),col_pal(col_pal_num)[col_pal_sel[3]],mid_col),
                           limit = scale_lims,
                           midpoint = 1,  space = "Lab", name = "Verhältnis") +
      labs(x = "", y = "", title = paste("Method:",method_name)) +
      theme(axis.text = element_blank(),axis.ticks = element_blank())+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank())+
      coord_fixed()
  }
}

#cart
plot_lower_tri(lower_tri_cart,col_pal = col_pal_einkommen,col_pal_num = 5,
               col_pal_sel = c(2,3,4),method_name = "cart",scale_lims = c(NA,4))
#random forest
plot_lower_tri(lower_tri_rf,col_pal = col_pal_einkommen,col_pal_num = 5,
               col_pal_sel = c(2,3,4),method_name = "rf",scale_lims = c(NA,4))
#sample
plot_lower_tri(lower_tri_sample,col_pal = col_pal_einkommen,col_pal_num = 5,
               col_pal_sel = c(2,3,4),method_name = "sample",scale_lims = c(NA,4))
#polyreg
plot_lower_tri(lower_tri_polyreg,col_pal = col_pal_einkommen,col_pal_num = 5,
               col_pal_sel = c(2,3,4),method_name = "polyreg",scale_lims = c(NA,4))


############################################################################
                              # Schulleistungen
############################################################################
col_pal_port <- colorRampPalette(c("#33821f","#2fd106", "#5dfc9d"))

#Für die metrischen Variablen Dichtefunktionen
port_c_plot <- dplyr::select(port,age,absences) %>% mutate(Method = "observed")
for (name in names(syn_list_port)) {
  port_c_plot <- port_c_plot %>%
    bind_rows(mutate(dplyr::select(syn_list_port[[name]],age,absences),Method = name))
}
port_c_plot <- pivot_longer(port_c_plot,-Method,names_to = "var")

mybw_port <- c("age"= 0.5,"absences"=1)
port_cont_plots <- list()

for (varb in c("age","absences")) {
  titl1 <- ifelse(varb == "age","Nicht parametrische Modelle","")
  titl2 <- ifelse(varb == "age","Parametrische Modelle","")
  port_cont_plots[[paste0(varb,"np")]] <- 
    port_c_plot %>%
    filter(var == varb) %>%
    filter(!(Method %in% c("polyreg")))%>%
    mutate(Method = relevel(factor(Method),"observed"))%>%
    ggplot(aes(x = value,col = Method,fill = Method))+
    geom_density(bw = mybw_port[varb],size = 0.8,alpha = 0.5)+
    scale_color_manual(values = c(my_grey,col_pal_port(3)))+
    scale_fill_manual(values = c(my_grey,NA,NA,NA))+
    scale_y_continuous(breaks = seq(0,1,0.2))+
    labs(x = varb,title = titl1,
         caption = paste("bandwidth =",mybw_port[varb]))
  
  port_cont_plots[[paste0(varb,"p")]] <- 
    port_c_plot %>%
    filter(var == varb) %>%
    filter(!(Method %in% c("cart","rf","sample")))%>%
    mutate(Method = relevel(factor(Method),"observed"))%>%
    ggplot(aes(x = value,col = Method,fill = Method))+
    geom_density(bw = mybw_port[varb],size = 0.8,alpha = 0.5)+
    scale_color_manual(values = c(my_grey,col_pal_port(1)))+
    scale_fill_manual(values = c(my_grey,NA,NA))+
    scale_y_continuous(breaks = seq(0,1,0.2))+
    labs(x = varb,title = titl2,
         caption = paste("bandwidth =",mybw_port[varb]))
}


ggarrange(ggarrange(port_cont_plots[[1]],
                    port_cont_plots[[3]],
                    common.legend = TRUE,legend = "right",
                    ncol = 2,nrow = 1),
          ggarrange(port_cont_plots[[2]],
                    port_cont_plots[[4]],
                    common.legend = TRUE,legend = "right",
                    ncol = 2,nrow = 1),
          nrow = 2,ncol = 1)

#Für die kategorischen Variablen Histogramme
port_cat_plot <- dplyr::select(port,-age,-absences) %>% mutate(Method = "observed")
for (name in names(syn_list_port)) {
  port_cat_plot <- port_cat_plot %>%
    bind_rows(mutate(dplyr::select(syn_list_port[[name]],-age,-absences),Method = name))
}
port_cat_plot <- pivot_longer(port_cat_plot,-Method,names_to = "var")

#Zunächst alle Variablen -> sehr unübersichtlich -> später fokussieren auf ein paar wenige

#non parametric 
port_cat_plot %>%
  filter(!(Method %in% c("polyreg")))%>%
  mutate(Method = relevel(factor(Method),"observed"))%>%
  ggplot(aes(x = factor(value),fill = Method)) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c(my_grey,col_pal_port(3)))+
  labs(y = "Anzahl",x = "",title = "Nicht parametrische Modelle")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))+
  facet_wrap(~var,scales = "free",nrow = 6,ncol = 5,strip.position = "bottom")

#parametric 
port_cat_plot %>%
  filter(!(Method %in% c("cart","rf","sample")))%>%
  mutate(Method = relevel(factor(Method),"observed"))%>%
  ggplot(aes(x = factor(value),fill = Method)) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c(my_grey,col_pal_port(3)))+
  labs(y = "Anzahl",title = "Parametrische Modelle",x = "")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))+
  facet_wrap(~var,scales = "free",nrow = 6,ncol = 5,strip.position = "bottom")

#non parametric Ausgewählte
port_cat_plot %>%
  filter(!(Method %in% c("polyreg")))%>%
  filter(var %in% c("address","Fedu","health","traveltime","Walc","activities")) %>%
  mutate(Method = relevel(factor(Method),"observed"))%>%
  ggplot(aes(x = factor(value),fill = Method)) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c(my_grey,col_pal_port(3)))+
  labs(y = "Anzahl",x = "",title = "Nicht parametrische Modelle")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))+
  facet_wrap(~var,scales = "free",nrow = 2,ncol = 3,strip.position = "bottom")

#parametric Ausgewählte
port_cat_plot %>%
  filter(!(Method %in% c("cart","rf","sample")))%>%
  filter(var %in% c("address","Fedu","health","traveltime","Walc","activities")) %>%
  mutate(Method = relevel(factor(Method),"observed"))%>%
  ggplot(aes(x = factor(value),fill = Method)) +
  geom_bar(position = "dodge")+
  scale_fill_manual(values = c(my_grey,col_pal_port(3)))+
  labs(y = "Anzahl",title = "Parametrische Modelle",x = "")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))+
  facet_wrap(~var,scales = "free",nrow = 2,ncol = 3,strip.position = "bottom")

# tabelle der differenz der korrelationsmatrizen
#cart
cor(dplyr::select(port,age,absences))-
  cor(dplyr::select(syn_list_port$cart,age,absences))
#random forest
cor(dplyr::select(port,age,absences))-
  cor(dplyr::select(syn_list_port$rf,age,absences))
#sample
cor(dplyr::select(port,age,absences))-
  cor(dplyr::select(syn_list_port$sample,age,absences))
#polyreg
cor(dplyr::select(port,age,absences))-
  cor(dplyr::select(syn_list_port$polyreg,age,absences))




# kreuztabellen
catvars_port <- names(port)[-c(3,30)]
#original
cont_table_port <- create_big_contingency(port,catvars_port)
#cart
cont_table_port_cart <- create_big_contingency(syn_list_port$cart,catvars_port)
#random forest
cont_table_port_rf <- create_big_contingency(syn_list_port$rf,catvars_port)
#sample
cont_table_port_sample <- create_big_contingency(syn_list_port$sample,catvars_port)
#polyreg
cont_table_port_polyreg <- create_big_contingency(syn_list_port$polyreg,catvars_port)

#Verhältnisse
#cart
verhaeltnis_cart_port <- (cont_table_port_cart)/(cont_table_port)
verhaeltnis_cart_port[is.nan(verhaeltnis_cart_port)|is.infinite(verhaeltnis_cart_port)] <- 
  cont_table_port_cart[is.nan(verhaeltnis_cart_port)|is.infinite(verhaeltnis_cart_port)]+1
#polyreg
verhaeltnis_polyreg_port <- (cont_table_port_polyreg)/(cont_table_port)
verhaeltnis_polyreg_port[is.nan(verhaeltnis_polyreg_port)|is.infinite(verhaeltnis_polyreg_port)] <- 
  cont_table_port_polyreg[is.nan(verhaeltnis_polyreg_port)|is.infinite(verhaeltnis_polyreg_port)]+1
#sample
verhaeltnis_sample_port <- (cont_table_port_sample)/(cont_table_port)
verhaeltnis_sample_port[is.nan(verhaeltnis_sample_port)|is.infinite(verhaeltnis_sample_port)] <- 
  cont_table_port_sample[is.nan(verhaeltnis_sample_port)|is.infinite(verhaeltnis_sample_port)]+1
#random forest
verhaeltnis_rf_port <- (cont_table_port_rf)/(cont_table_port)
verhaeltnis_rf_port[is.nan(verhaeltnis_rf_port)|is.infinite(verhaeltnis_rf_port)] <- 
  cont_table_port_rf[is.nan(verhaeltnis_rf_port)|is.infinite(verhaeltnis_rf_port)]+1

#lower triangle
#cart
lower_tri_cart_port <- get_lower_tri(verhaeltnis_cart_port)
#random forest
lower_tri_rf_port <- get_lower_tri(verhaeltnis_rf_port)
#sample
lower_tri_sample_port <- get_lower_tri(verhaeltnis_sample_port)
#polyreg
lower_tri_polyreg_port <- get_lower_tri(verhaeltnis_polyreg_port)

#colnames der Kreuztabellen
colnames_kreuz_port <- colnames(lower_tri_cart_port)
colnames_kreuz_port_num <- paste(colnames_kreuz_port,
                                 seq(colnames_kreuz_port),sep = "_")
colnames(lower_tri_cart_port) <- colnames_kreuz_port_num
rownames(lower_tri_cart_port) <- colnames_kreuz_port_num
colnames(lower_tri_rf_port) <- colnames_kreuz_port_num
rownames(lower_tri_rf_port) <- colnames_kreuz_port_num
colnames(lower_tri_sample_port) <- colnames_kreuz_port_num
rownames(lower_tri_sample_port) <- colnames_kreuz_port_num
colnames(lower_tri_polyreg_port) <- colnames_kreuz_port_num
rownames(lower_tri_polyreg_port) <- colnames_kreuz_port_num

#Visualisierung der unteren dreieckshälfte der Verhältniskreuztabellen
#cart
plot_lower_tri(lower_tri_cart_port,col_pal = col_pal_port,
               col_pal_num = 3,col_pal_sel = 1:3,
               scale_lims = c(NA,4),
               method_name = "cart",axis_txt = FALSE,
               low_col = "blue")
#random forest
plot_lower_tri(lower_tri_rf_port,col_pal = col_pal_port,
               col_pal_num = 3,col_pal_sel = 3:1,
               scale_lims = c(NA,4),
               method_name = "rf",axis_txt = FALSE,
               low_col = "blue")
#sample
plot_lower_tri(lower_tri_sample_port,col_pal = col_pal_port,
               col_pal_num = 3,col_pal_sel = 1:3,
               scale_lims = c(NA,4),
               method_name = "sample",axis_txt = FALSE,
               low_col = "blue")
#polyreg
plot_lower_tri(lower_tri_polyreg_port,col_pal = col_pal_port,
               col_pal_num = 3,col_pal_sel = 1:3,
               scale_lims = c(NA,4),
               method_name = "polyreg",axis_txt = FALSE,
               low_col = "blue")
