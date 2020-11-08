#######################################################################
                        # Datenaufbereitung und EDA
#####################################################################
#benötigte Packete
library(tidyverse)
library(GGally)
library(forcats)
#######################################################################
                              #blood transfusion
#####################################################################
#Daten einlesen und aufbereiten
transfusion <- read.table("transfusion.data", sep = ",", header = T)
transfusion <- dplyr::rename(transfusion, march2007 = whether.he.she.donated.blood.in.March.2007,
                      Last = Recency..months., Frequency = Frequency..times.,
                      Monetary =Monetary..c.c..blood.,
                      First = Time..months.)

unique(transfusion$Monetary/transfusion$Frequency) #250 c.c. pro donation 
#-> monetary und frequency 1 korrelation immer 250 fache -> monetary löschen

hist_trans <- sapply(1:5,function(i){
  ggplot(transfusion)+
    geom_histogram(aes_string(colnames(transfusion)[i]))
},simplify = FALSE)
ggpubr::ggarrange(plotlist = hist_trans)
# -> sehr rechtsschief -> Variablen logarithmieren

transf <- transfusion %>%
  dplyr::select(-Monetary)%>%
  dplyr::mutate(Last = log(Last+1),
         Frequency = log(Frequency),
         First = log(First+1),
         march2007 = as.factor(march2007)) #um einen Tag verschoben, damit log(0) nicht inf
transf_org <- transfusion %>%
  dplyr::select(-Monetary)%>%
  dplyr::mutate(march2007 = as.factor(march2007),
                Frequency = as.numeric(Frequency))

#unbalanced data Problematik durch sample beheben
set.seed(2)
transf <- transf %>%
  filter(march2007 == 0) %>%
  sample_frac(size = 0.55) %>%
  bind_rows(filter(transf,march2007 == 1))
transf_org <- transf_org %>%
  filter(march2007 == 0) %>%
  sample_frac(size = 0.55) %>%
  bind_rows(filter(transf_org,march2007 == 1))
  
#EDA
theme_set(theme_classic()+
          theme(panel.grid.major.y = element_line(color = "lightgrey")))

#Farbpalette
col_pal_trans <- colorRampPalette(c("#f50519","#d1931f","#fce621"))

#Boxplots
box_trans <- sapply(c(3,1,2),function(i){
  ggplot(transf,aes(x = factor(march2007)))+
    geom_boxplot(aes_string(y = colnames(transf)[i]),fill = col_pal_trans(3)[i])+
    scale_x_discrete(labels = c("Nein","Ja"))+
    ylim(0,5)+
    labs(x = c("Blutspende im März","","")[i])
},simplify = FALSE)
ggpubr::ggarrange(plotlist = box_trans,ncol = 3)

summary(transf)
#####################################################################
                            #einkommen
####################################################################
#daten einlesen und aufbereiten
einkommen =read.table("adult.csv",sep=",",header=TRUE)

einkommen %>%
  select_if(is.factor)%>%
  summary()

# native country ? in other, merge all but mexico to other
# race -> white black other
# occupation: other service in other
# marital status: separated,married, widowed, never married
# workclass, private, selfemp,gov,other

table(einkommen$marital.status,einkommen$relationship)
# überschneidende (redundante) infos und bei den infos die unterschiedlich sind kleine var
# ==> relationship raus und marital.status married zsmfassen

einkommen <- einkommen %>%
  mutate(
    workclass = as.character(workclass),
    workclass = case_when(
      workclass == "?" ~ "(Other)",
      workclass == "Never-worked" ~ "(Other)",
      workclass == "Without-pay" ~ "(Other)",
      workclass == "Self-emp-not-inc" ~ "Selfemp",
      workclass == "Self-emp-inc" ~ "Selfemp",
      workclass == "Federal-gov" ~ "Gov",
      workclass == "Local-gov" ~ "Gov",
      workclass == "State-gov" ~ "Gov",
      TRUE ~ workclass),
    workclass = as.factor(workclass),
    marital.status = as.character(marital.status),
    marital.status = case_when(
      marital.status == "Divorced" ~ "Separated",
      marital.status == "Married-AF-spouse" ~ "Married",
      marital.status == "Married-civ-spouse" ~ "Married",
      marital.status == "Married-spouse-absent" ~ "Married",
      TRUE ~ marital.status),
    marital.status = as.factor(marital.status),
    occupation = as.character(occupation),
    occupation = case_when(
      occupation == "Armed-Forces" ~ "Other",
      occupation == "Priv-house-serv" ~ "Other",
      occupation == "Protective-serv" ~ "Other",
      occupation == "Tech-support" ~ "Other",
      occupation == "Farming-fishing" ~ "Other",
      occupation == "Handlers-cleaners" ~ "Other",
      occupation == "Transport-moving" ~ "Other",
      occupation == "?" ~ "Other",
      occupation == "Other-service" ~ "Other",
      TRUE ~ occupation),
    occupation = as.factor(occupation),
    race = as.character(race),
    race = case_when(
      race == "Amer-Indian-Eskimo" ~ "Other",
      race == "Asian-Pac-Islander" ~ "Other",
      TRUE ~ race),
    race = as.factor(race),
    native.country = as.character(native.country),
    native.country = if_else(native.country == "United-States",native.country,"Other"),
    native.country = as.factor(native.country),
    education = as.character(education),
    education = case_when(
      education == "HS-grad" ~ "hs",
      education == "Doctorate" ~ "post_hs",
      education == "Prof-school" ~ "post_hs",
      education == "Assoc-acdm" ~ "post_hs",
      education == "Assoc-voc" ~ "post_hs",
      education == "Masters" ~ "post_hs",
      education == "Bachelors" ~ "post_hs",
      education == "Some-college" ~ "post_hs",
      TRUE ~ "lower_hs"),
    education = as.factor(education)
    )

hist_eink <- sapply(1:6,function(i){
  eink_num <- einkommen %>%
    select_if(is.numeric)
  ggplot(eink_num)+
    geom_histogram(aes_string(colnames(eink_num)[i]))
},simplify = FALSE)
ggpubr::ggarrange(plotlist = hist_eink)

# edu oder edu num? -> edu.num statt edu (quasi ordinal metrisch behandelt)
# delete fnlweigth as it was added by goverment afterwards
# capital gain/capital loss raus da low var huge density mass 0

einkommen <- einkommen %>%
  dplyr::select(-relationship,
         -education,
         -capital.gain,
         -capital.loss,
         -fnlwgt)

#unbalanced data Problematik durch sample lösen
set.seed(2)
einkommen_s <- einkommen %>%
  filter(income == "<=50K") %>%
  sample_n(250)%>%
  bind_rows(einkommen %>%
              filter(income == ">50K") %>%
              sample_n(250))

einkommen_s <- rename(einkommen_s, education = education.num)

#EDA
#Farbpalette
col_pal_einkommen <- colorRampPalette(c("#05eefa","#1f0399", "#329c9a", "#ceeef5","#057bfa" ))

#Boxplots für metrische Variablen
box_eink <- sapply(c(1,3,8),function(i){
  ggplot(einkommen_s,aes(x = factor(income)))+
    geom_boxplot(aes_string(y = colnames(einkommen_s)[i]),fill = col_pal_einkommen(8)[i])+
    scale_x_discrete(labels = c("Nein","Ja"))+
    labs(x = c("","","Einkommen über 50K", "", "", "", "", "")[i])
},simplify = FALSE)
ggpubr::ggarrange(plotlist = box_eink,ncol = 3)

#Histogramme für kategorielle Variablen
einkommen_s$income <- as.factor(einkommen_s$income)
hist_eink2 <- sapply(c(2,4,5,6,7,9),function(i){
  ggplot(einkommen_s, aes_string(x=colnames(einkommen_s)[i])) +
    geom_bar(aes(fill =relevel(income,"<=50K"), group=relevel(income,">50K")),position = "fill")+
    scale_y_continuous("Percentage (%)", limits = c(0, 1), breaks = seq(0, 1, by=0.2))+
    #labs(x = c("","","", "", "", "", "", "","")[i])+
    scale_fill_manual(name = "Income",values=col_pal_einkommen(8)[c(6,7)],labels = c("\U02264 50K","> 50K"))+
    theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))
},simplify = FALSE)
ggpubr::ggarrange(plotlist = hist_eink2, ncol = 3, nrow = 2,
                  common.legend = TRUE,legend = "bottom")




###############################################################
                          #Schulleistungen
###############################################################
#Daten einlesen und aufbereiten
portugal =read.table("student-mat.csv",sep=";",header=TRUE)
portugal$G3 <- as.factor(portugal$G3)
#Schulnoten in bestanden und durchgefallen kategorisieren
portugal$G3 <- portugal$G3 %>%
  fct_collapse("0" = c("0", "4", "5", "6", "7", "8", "9"), 
               "1" = c("10":"20"))

#write_csv(portugal, "C:/Users/Eleftheria/Desktop/BA/Datensätze/student/port_bin")
port_bin <- read_csv("port_bin")
#Entfernen der zwei Variablen G2 und G1, da sonst zu hohe accurancy bei der logistischen 
#Modellierung (so empfohlen von den Bereitstellern des Datensatzes)
port <- port_bin %>% dplyr::select(!G2) %>% dplyr::select(!G1)

sum(as.numeric(port$G3)-1)/length(port$G3)
#unbalanced (32.9%) deswegen bei crossvalidation im trainingsschritt auf 50 prozent hochbootstrappen 

#transform into factors the numerics where it is sensible
port <- port %>%
  mutate(Medu = factor(Medu,labels = c("none","primary","5th9th","secondary","higher")),
         Fedu = factor(Fedu,labels = c("none","primary","5th9th","secondary","higher")),
         G3 = factor(G3))
#character to factor
port <- port %>%
  mutate_if(is.character,factor)
port <- port %>%
  mutate(freetime = factor(freetime,labels =  c("very low","low","medium","high","very high")),
         goout = factor(goout,labels =  c("very low","low","medium","high","very high")),
         Dalc = factor(Dalc,labels =  c("very low","low","medium","high","very high")),
         Walc = factor(Walc,labels =  c("very low","low","medium","high","very high")),
         health = factor(health,labels =  c("very low","low","medium","high","very high")),
         studytime = factor(studytime,labels =  c("very low","low","high","very high")),
         traveltime = factor(traveltime,labels =  c("very low","low","high","very high")),
         failures = factor(failures,labels =  c("very low","low","high","very high")),
         famrel = factor(famrel,labels =  c("very bad","bad","medium","goog","excellent")))

col_pal_port <- colorRampPalette(c("#33821f","#2fd106", "#5dfc9d"))

# Histogramme für kategorielle Variablen zu viele zu unübersichtlich
# -> auswahl treffen
port_auswahl <- select(port, G3, higher, schoolsup, guardian, Medu, Fjob, romantic)
fac_col_port_auswahl <- port %>%
  select (higher, schoolsup, guardian, Medu, Fjob, romantic) %>%
  colnames()
hist_port_auswahl <- sapply(fac_col_port_auswahl,function(var_name){
  ggplot(port_auswahl, aes_string(x=var_name)) +
    geom_bar(aes(fill = G3, group=G3),position = "fill")+
    scale_y_continuous("Percentage (%)", limits = c(0, 1), breaks = seq(0, 1, by=0.2))+
    #labs(x = c("","","", "", "", "", "", "","")[i])+
    scale_fill_manual(name = "Final result",values=col_pal_port(2),
                      labels = c("Failed","Passed"))+
    theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))
},simplify = FALSE)
ggpubr::ggarrange(plotlist = hist_port_auswahl, ncol = 3, nrow = 2,
                  common.legend = TRUE,legend = "bottom")


#Boxplots für metrische Variablen
# boxplots: age, absence
box_port_age <- ggplot(port,aes(x = G3,y = age))+
  geom_boxplot(fill = col_pal_port(2)[1])+
  scale_x_discrete(labels = c("Failed","Passed"))+
  labs(x = "")
# box_port_age
box_port_abs <- ggplot(port,aes(x = G3,y = absences))+
  geom_boxplot(fill = col_pal_port(2)[2])+
  scale_x_discrete(labels = c("Failed","Passed"))+
  labs(x = "")
# box_port_absences
box_port_arr <- ggpubr::ggarrange(box_port_age,box_port_abs,label.x = "Final result")
ggpubr::annotate_figure(box_port_arr,bottom = "Final result")


########################################################################
#write data
#save(einkommen_s,port,transf,transf_org,file = "datensaetze.Rdata")
#load("datensaetze.Rdata")
