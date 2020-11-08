#Datensätze laden
load("datensaetze.Rdata")
#benötigte Packete
library(rpart)
library(rpart.plot)
library(tree)
library(plyr)
#nur Nomenklatur der Übersicht halber im Plot geändert
einkommen_s$income <- mapvalues(as.factor(einkommen_s$income),
                                from = c("<=50K", ">50K"), to = c("<50K", ">50K"))
#Baum fitten
fit <- rpart(sex ~. , data = einkommen_s, method = "class")
#gefitteten Baum visualisieren
prp(fit,
    type = 3,
    extra = 2, 
    clip.right.labs = F,
    box.palette = c("#6eb6f5","#050df7"),
    fallen.leaves = T,
    branch = 1,
    round = 0.3,
    split.col = "#1f0399",varlen = 0,
    branch.col = "#1f0399",col = "white",
    space = 0.6,
    clip.facs = T)

