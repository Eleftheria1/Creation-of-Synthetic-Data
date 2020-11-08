#########################################################
#                   Sythetisierung
#########################################################
#Packete
library(synthpop)
library(tidyverse)
#Originaldatensätze
load("datensaetze.Rdata")

m <- 1
my_seed <- 8

# smoothing mit Gaußschem Kerndichteschätzer für metrische Variablen
smoothing_blood <- list(Last = "density", Frequency = "density", First = "density")
smoothing_eink <- list(age = "density", education = "density", hours.per.week = "density")
smoothing_edu <- list(age = "density", absences = "density")

#Zufällige Anordnung der Reihenfolge in der die Variablen synthetisiert werden sollen
set.seed(8)
visit_seq_blood <- sample(ncol(transf_org))
set.seed(21)
visit_seq_eink <- sample(ncol(einkommen_s))
visit_seq_edu <- sample(ncol(port))
visit_seq_edu[1] <- 3
visit_seq_edu[5]<- 8

#Listen für die synthetischen Datensätze 
#Listelemente: einzelne Synthetisierungsverfahren
syn_list_trans <- list()
syn_list_eink <- list()
syn_list_port <- list()

#######################################################################
#blood transfusion
#####################################################################
syn_list_trans$cart <- syn(transf_org, method = "cart", visit.sequence = visit_seq_blood,
                           m = m, smoothing = smoothing_blood, models = F, seed = my_seed)$syn
syn_list_trans$rf <- syn(transf_org, method = "rf", visit.sequence = visit_seq_blood,
                           m = m, models = F, seed = my_seed)$syn
syn_list_trans$sample <- syn(transf_org, method = "sample", visit.sequence = visit_seq_blood,
                           m = m, smoothing = smoothing_blood, models = F, seed = my_seed)$syn
syn_list_trans$norm <- syn(transf_org, method = c(rep("norm",3), "logreg"), visit.sequence = visit_seq_blood,
                           m = m, models = F, seed = my_seed)$syn
syn_list_trans$normrank <- syn(transf_org, method = c(rep("normrank",3), "logreg"), visit.sequence = visit_seq_blood,
                           m = m, smoothing = smoothing_blood, models = F, seed = my_seed)$syn

#Nachträglich ein paar Regeln die beachtet werden müssen
#Beispielsweise kann die letzte Blutspende nicht länger her sein als die erste etc.
syn_list_trans <- lapply(syn_list_trans, function(df){
  df %>%
    mutate(First = if_else(First < 0,0,First),
           Last = if_else(Last < 0,0,Last),
           Frequency = if_else(Frequency < 1,1,Frequency),
           Last = if_else(Last > First,First,Last),
           Frequency = if_else(First == Last,1,Frequency),
           Last = log(Last+1),
           Frequency = log(Frequency),
           First = log(First+1))
})

#Liste der synthetischen Datensätze für Bluttransfusionen speichern
#save(syn_list_trans,file = "transf_syn_list.Rdata")
#load("transf_syn_list.Rdata")

#######################################################################
#einkommen
#####################################################################
syn_list_eink$cart <- syn(einkommen_s, method = "cart", visit.sequence = visit_seq_eink,
                           m = m, smoothing = smoothing_eink, models = F, seed = my_seed)$syn
syn_list_eink$rf <- syn(einkommen_s, method = "rf", visit.sequence = visit_seq_eink,
                          m = m, models = F, seed = my_seed)$syn
syn_list_eink$sample <- syn(einkommen_s, method = "sample", visit.sequence = visit_seq_eink,
                          m = m, smoothing = smoothing_eink, models = F, seed = my_seed)$syn
syn_list_eink$polyreg <- syn(einkommen_s, method = c("norm","polyreg","norm",rep("polyreg",4),"norm",rep("polyreg",2)),
                             visit.sequence = visit_seq_eink,
                             m = m, models = F, seed = my_seed)$syn
#Nachträgliche Änderungen der numerischen Variable age, wie beispielsweise, dass
#eine Person unter 16 kein Einkommen haben kann
syn_list_eink <- lapply(syn_list_eink, function(df){
  df %>%
    mutate(
      age = as.numeric(age),
      age = if_else(age < 16,17,age),
      age = as.integer(age))
})

#Speichern der synthetischen Datensätze für das Einkommen
#save(syn_list_eink,file = "einkommen_syn_list.Rdata")
#load("einkommen_syn_list.Rdata")

#######################################################################
#Schulleistungen
#####################################################################

syn_list_port$cart <- syn(port, method = "cart", visit.sequence = visit_seq_edu,
                          m = m, smoothing = smoothing_edu, models = F, seed = my_seed)$syn
syn_list_port$rf <- syn(port, method = "rf", visit.sequence = visit_seq_edu,
                        m = m, models = F, seed = my_seed)$syn
syn_list_port$sample <- syn(port, method = "sample", visit.sequence = visit_seq_edu,
                            m = m, smoothing = smoothing_edu, models = F, seed = my_seed)$syn
syn_list_port$polyreg <- syn(port, method = c(rep("polyreg",2),"norm",rep("polyreg",26),"norm","polyreg"),
                             visit.sequence = visit_seq_edu,
                             m = m, models = F, seed = my_seed)$syn
#Nachträgliche Änderungen der metrischen Variablen, wie beispielsweise, dass die Fehltage
#nicht negativ sein können
syn_list_port <- lapply(syn_list_port, function(df){
  df %>%
    mutate(
      age = if_else(age < 15,15,age),
      absences = if_else(absences < 0,0,absences)
    )
})

#Speichern der synthetischen Datensätze für die Schulleistungen
#save(syn_list_port,file = "port_syn_list.Rdata")
#load("port_syn_list.Rdata")
