m <- matrix(c(1,15,2,854,2,621),nrow=2,ncol=3,byrow=TRUE)
m
pourcentages.lignes(m)
pourcentages.colonnes(m)



t <- matrix(c(37,36,12,65,43,7),nrow=2,ncol=3,byrow=TRUE)
colnames(t) <- c("Sociologue", "Banquier", "Archéologue")
rownames(t) <- c("Avec brouette", "Sans brouette")
pourcentages.lignes(t)
pourcentages.colonnes(t)
pourcentages.theoriques(t,5)
effectifs.theoriques(t,5)
chisq.test(t)$expected
ecarts <- effectifs.theoriques(t,10)-t
sum((ecarts^2)/effectifs.theoriques(t,10))
chisq.test(t)

plot.binom <- function(nb=100,tirage=NULL,highl=FALSE) {
  par(cex=0.85)
  if (is.null(tirage))
    tmp.tab <- table(rbinom(nb,200,0.217))
  else
    tmp.tab <- table(tirage)
  plot(tmp.tab,xlab="Nombre de sociologues à brouette trouvés",ylab="Nombre d'occurrences",main=paste("Résultats pour",format(nb,scientific=FALSE),"expériences"))
  if (highl) segments(highl,0,highl,tmp.tab[as.character(highl)],col="red",lwd=2)
}

tirage <- rbinom(100,200,0.217)

pdf(file="/home/julien/doc/khi2/images/exp100.pdf")
plot.binom(tirage=tirage)
dev.off()

png(file="/home/julien/doc/khi2/images/exp100.png",width=600,height=600,pointsize=9)
plot.binom(tirage=tirage)
dev.off()

pdf(file="/home/julien/doc/khi2/images/exp1000_10000_100000.pdf",width=12,height=12)
par(mfrow=c(2,2),cex=0.85)
plot.binom(1000)
plot.binom(10000)
plot.binom(100000)
plot.binom(1000000,highl=37)
dev.off()

png(file="/home/julien/doc/khi2/images/exp1000_10000_100000.png",width=800,height=800,pointsize=9)
par(mfrow=c(2,2),cex=0.85)
plot.binom(1000)
plot.binom(10000)
plot.binom(100000)
plot.binom(1000000,highl=37)
dev.off()


t <- matrix(c(20,30,10,5),nrow=2,ncol=2,byrow=TRUE)
t
chisq.test(t)
t - chisq.test(t)$expected


####Exemples de chi deux HDV 2003

load("/home/julien/enquetes/hdv2003/data/hdv.rda") # df hdv

hdv$csego6 <- substring(as.character(hdv$csego),0,1)
hdv$csego6[hdv$csego6 %in% c("0","8","")] <- NA
hdv$cspere6 <- substring(as.character(hdv$cspere),0,1)
hdv$cspere6[hdv$cspere6 %in% c("0","8","")] <- NA

cs6names <- c("Agriculteurs","Indépendants","Cadres","Interm.","Employés","Ouvriers")
hdv$csego6 <- factor(hdv$csego6, labels=cs6names)
hdv$cspere6 <- factor(hdv$cspere6, labels=cs6names)

attach(hdv)




### Croisement fait d'être élevé seul par la mère par CS du père : p = 1.726e-08
t <- table(belevq2,cspere6)
t
summary(t)
pourcentages.colonnes(t)
chisq.test(t)$residuals



### Croisement entre la pratique du football et le sentiment d'appartenance à une classe sociale : p=~0.47
t <- table(lspor5,vclso)
t
summary(t)
#pourcentages.colonnes(t)
chisq.test(t,simulate.p.value=TRUE)$residuals
chisq.test(t,simulate.p.value=TRUE)


### Croisement préférer série / âge pour regroupements modalités

chires <- function(v1,v2) {
  t <- table(v1,v2)
  print(chisq.test(t))
  cat("\n--- RESIDUS ---\n")
  print(chisq.test(t)$residuals)
  cat("\n--- POURCENTAGES COLONNES ---\n")
  print(pourcentages.colonnes(t))
}


agecl <- cut(agee,breaks=c(16,25,35,45,55,65,100))
chires(ltelv4,agecl)
ageclb <- cut(agee,breaks=c(16,55,100))
chires(ltelv4,ageclb)


### Sensibilité aux effectifs

tab <-cbind(c(7,4,9),c(5,8,12))
tab
pourcentages.lignes(tab)
chisq.test(tab)

tab <-cbind(c(50,80,120),c(70,40,90))
pourcentages.lignes(tab)
chisq.test(tab)

tab <-cbind(c(10,20),c(20,10))
chisq.test(tab)
tab <-tab*10
chisq.test(tab)

### Résidus : Croisement appartenance CS et CS

table(csego6,vclso)
pourcentages.lignes(table(csego6,vclso))
chires(csego6,vclso)


library(vcd)
mosaicplot(table(csego,cspere),shade=TRUE)
mosaicplot(table(csego6,cspere6),shade=TRUE)
assocplot(table(csego6,cspere6))

### Exemple assocplot

## pdf(file="/home/julien/doc/khi2/images/assoc.pdf",pointsize=9)
## assoc(table(csego6,cspere6),shade=TRUE,gp_labels=gpar(fontsize=7),gp_varnames=gpar(fontsize=10,fontface="bold"),set_varnames=list(cspere6="CS du père",csego6="CS de l'enquêté"),legend_args=list(fontsize=9),margins=c(4,3,0,4))
## dev.off()

### Exemple mosaicplot

pdf(file="/home/julien/doc/khi2/images/mosaic.pdf",pointsize=9)
freqfam <- ffreq1
freqfam[freqfam==""] <- NA
freqfam <- factor(freqfam,labels=c("Au moins une fois\npar semaine","Une à trois fois\npar mois","Plusieurs fois\ndans l'année","Exceptionnellement","Jamais"))
t <- table(csego6,freqfam)
mosaic(t,shade=TRUE,gp_labels=gpar(fontsize=8),gp_varnames=gpar(fontsize=10,fontface="bold"),set_varnames=list(freqfam="Rencontres avec la famille",csego6="CS de l'enquêté"),legend_args=list(fontsize=9),margins=c(4,3,0,8),rot_labels=c(90,90,0,0),just_labels=c("left","right"),offset_varnames=c(5,0,0,4),pop=FALSE)
tab <- round(t/apply(t,1,sum)*100,1)
labeling_cells(text=tab, clip = FALSE,gp_text=gpar(fontsize=7))(t)
dev.off()

png(file="/home/julien/doc/khi2/images/mosaic.png",pointsize=9,width=700,height=700)
freqfam <- ffreq1
freqfam[freqfam==""] <- NA
freqfam <- factor(freqfam,labels=c("Au moins une fois\npar semaine","Une à trois fois\npar mois","Plusieurs fois\ndans l'année","Exceptionnellement","Jamais"))
t <- table(csego6,freqfam)
mosaic(t,shade=TRUE,gp_labels=gpar(fontsize=8),gp_varnames=gpar(fontsize=10,fontface="bold"),set_varnames=list(freqfam="Rencontres avec la famille",csego6="CS de l'enquêté"),legend_args=list(fontsize=9),margins=c(4,3,0,8),rot_labels=c(90,90,0,0),just_labels=c("left","right"),offset_varnames=c(5,0,0,4),pop=FALSE)
tab <- round(t/apply(t,1,sum)*100,1)
labeling_cells(text=tab, clip = FALSE,gp_text=gpar(fontsize=7))(t)
dev.off()


### Effectifs théoriques inférieurs à 5

t <- cbind(c(220,200,200),c(7,1,1))#,c(2,1,4))
chisq.test(t)
chisq.test(t,simulate.p.value=TRUE,B=100000)
fisher.test(t)

t
chisq.test(t)$expected
t-chisq.test(t)$expected
chisq.test(t)$residuals


### Variables cachées

hommes <- subset(hdv,sexee==1)
femmes <- subset(hdv,sexee==2)

chires(bjint,lois16)
chires(hommes$bjint,hommes$lois16)
chires(femmes$bjint,femmes$lois16)

table(lois16,bjint)
table(hommes$lois16,hommes$bjint)
table(femmes$lois16,femmes$bjint)

### V de Cramer

t <- rbind(c(20,20),c(20,20))
cramer.v(t)
t <- rbind(c(30,10),c(10,30))
cramer.v(t)
t <- rbind(c(40,0),c(0,40))
cramer.v(t)
t <- rbind(c(20,10),c(15,35),c(38,21))
cramer.v(t)
t <- t*10
cramer.v(t)
