library(dplyr)
library(ggplot2)



#############################################################################################################################
#####################################editar la tabla de datos################################################################
#############################################################################################################################

datos<-data.frame(mujeres_trabajadoras)
datos$ANIOP<-factor(datos$ANIOP,levels = levels(factor(datos$ANIOP)),labels = c("no_labora","si_labora"))
datos$ANIOA<-factor(datos$ANIOA,levels = levels(factor(datos$ANIOA)),labels = c("no_labora","si_labora"))
datos$RAZAN<-factor(datos$RAZAN,levels = levels(factor(datos$RAZAN)),labels = c("blanca","negra"))
datos$NIN1<-factor(datos$NIN1,levels = levels(factor(datos$NIN1)),labels = c("no_hay","si_hay"))
datos$NIN2<-factor(datos$NIN2,levels = levels(factor(datos$NIN2)),labels = c("no_hay","si_hay"))
datos

##########################################################################################################
###########################################respuesta pregunta 1###########################################
##########################################################################################################

tabla_0<-table(select(datos,c(ANIOA,ANIOP)))
tabla_0
datos_0<-as.data.frame(tabla_0)
datos_0

ggplot(data=datos_0, aes(x=ANIOP, y=Freq,fill=ANIOA)) + 
  geom_bar(stat="identity",position = position_dodge()) +geom_text(aes(label = Freq), vjust = 0, size = 3.5)+
  scale_fill_manual(values=c("darkgreen", "purple","black"))

tabla_0_prop<-prop.table(tabla_0)
tabla_0_prop
datos_0_prop<-as.data.frame(tabla_0_prop)

ggplot(data=datos_0_prop, aes(x=ANIOA, y=Freq, fill=ANIOP)) + 
  geom_bar(stat="identity",position = position_dodge2())+geom_text(aes(label = Freq), vjust = 0, size = 3.5)+
  ylab("Frecuencia relativa")+
  scale_fill_manual(values=c("darkgreen", "purple","black"))

#############################################################################################
####################respuesta pregunta 2#####################################################
#############################################################################################

#######tabla de estad?stica descriptiva del salario del esposo por grupo de mujeres######################
##################################################################################
aggregate(INGESP~ANIOA,FUN=summary,data=datos)

###################gr?fica box plot de del salario del espso por grupo de mujeres######
#################################################################################################
ggplot(data =datos , mapping = aes(x = ANIOA, y = INGESP)) +
  geom_boxplot()


##################################################################################################
######################################respuesta pregunta 3#######################################
##################################################################################################

#####tabla de estad?sica descriptiva#######
summary(select(datos,EDAD))

#####grafica  boxplot######
ggplot(data = datos, mapping = aes(x ="",y=EDAD)) +xlab("")+
  geom_boxplot()

normalidad<-ggplot(data = datos, aes(EDAD))+xlab("")+ylab("Conteos")
normalidad+geom_histogram(color="darkblue",fill="lightblue",bins =5)

#######tabla de estadistica descriptiva de la edad por grupo#######
aggregate(EDAD~ANIOA,FUN=summary,data=datos)
#####grafica de la edad por cada grupo###############
ggplot(data = datos, mapping = aes(ANIOA,y=EDAD))+geom_boxplot()


ggplot(datos)+ 
  geom_histogram(bins = 10, aes( EDAD)) + 
  facet_grid(ANIOA~., scales = 'free') +
  xlab("Edad") + 
  ylab("Frecuencia") + 
  ggtitle("Distribuci?n de la variable edad") +
  theme_minimal()

####################################################################################################
##########################Pregunta 4################################################################
####################################################################################################


############################################
#####convertir a factor la variable EDUC###
###########################################
table(datos$EDUC)
datos$EDUC<-factor(datos$EDUC)
datos$EDUC

##################################
#######tabla de proporciones #####
##################################

######varible ANIOA###############
ANIOA_d<-prop.table(table(select(datos,ANIOA)))
ANIOA_d
dato_ANIOA<-as.data.frame(ANIOA_d)
dato_ANIOA
dato_ANIOA<-rename(dato_ANIOA,ANIOA=Var1)
dato_ANIOA

ggplot(data=dato_ANIOA, aes(x=ANIOA, y=Freq)) + 
  geom_bar(stat="identity") +geom_text(aes(label = Freq), vjust = -0.3, size = 3.5)+ylab("Frecuencia relativa")+
  scale_fill_manual(values=c("darkgreen", "purple"))

ggplot(data=dato_ANIOA,aes(x="",y=Freq,fill=ANIOA))+
  geom_bar(stat = "identity", color="white")+
  geom_text(aes(label=Freq),position=position_stack(vjust=0.5),color="white",size=5)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("blue","steelblue","orange","gray","green"))+
  theme_void()+
  labs(title="Gr?fico de Pie")
###########variable EDUC#############

educ_d<-prop.table(table(select(datos,EDUC)))
educ_d

dato_educ<-as.data.frame(educ_d)

dato_educ<-rename(dato_educ,EDUC=Var1)

ggplot(data=dato_educ, aes(x=EDUC, y=Freq)) + 
  geom_bar(stat="identity") +geom_text(aes(label = Freq), vjust = -0.3, size = 3.5)+ylab("Frecuencia relativa")+
  scale_fill_manual(values=c("darkgreen", "purple"))


ggplot(data=dato_educ,aes(x="",y=Freq,fill=EDUC))+
  geom_bar(stat = "identity", color="white")+
  geom_text(aes(label=Freq),position=position_stack(vjust=0.9),color="black",size=2)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("blue","steelblue","orange","gray","green"))+
  theme_void()+
  labs(title="Gr?fico de Pie")


#############tabla de frecuencia cruzada EDUC y ANIOA###

tabla_1<-prop.table(table(select(datos,EDUC,ANIOA)))
tabla_1

###################conversi?n de tabla a data frame##############
#################################################################

datos_1<-as.data.frame(tabla_1)


ggplot(data=datos_1, aes(x=EDUC, y=Freq,fill=ANIOA)) + 
  geom_bar(stat="identity",position = position_dodge())+geom_text(aes(label = Freq), vjust =0, size = 3.5)+
  ylab("frecuencia relativa")+
  scale_fill_manual(values=c("darkgreen", "purple","black"))


###################################################################################
#######################pregunta 5############################################################
#############################################################################################

#################tabla cruazada ANIOA y NIN1####################
################################################################

tabla_02<-prop.table(table(select(datos,ANIOA,NIN1)))
tabla_02
datos_02<-as.data.frame(tabla_02)

##########grafica de la tabla cruazada ANIOA y NIN1############
###############################################################

ggplot(data=datos_02, aes(x=ANIOA, y=Freq,fill=NIN1)) +
  geom_bar(stat="identity",position = position_dodge()) +
  ylab("frecuencia relativa")+geom_text(aes(label = Freq), vjust =0, size = 3.5)+ 
  scale_fill_manual(values=c("darkgreen", "purple","black"))

########################tabla cruzada ANIOA y NIN2################
##################################################################

tabla_002<-prop.table(table(select(datos,ANIOA,NIN2)))  
tabla_002
datos_002<-as.data.frame(tabla_002)

###################grafica de latabla cruzada ANIOA y NIN2################################
##########################################################################################
ggplot(data=datos_002, aes(x=ANIOA, y=Freq,fill=NIN2)) +
  geom_bar(stat="identity",position = position_dodge())+geom_text(aes(label = Freq), vjust =0, size = 3.5)+
  ylab("frecuencia relativa")+
  scale_fill_manual(values=c("darkgreen", "purple","black"))

##############tabla cruzada ANIOA, NIN1 y NIN2###########################################
#########################################################################################

tabla_0002<-prop.table(table(select(datos,ANIOA,NIN1,NIN2))) 
tabla_0002
datos_0002<-as.data.frame(tabla_0002)

##############grafica de la tabla cruazada ANIOA, NIN1 y NIN2##############################
###########################################################################################


ggplot(data=datos_0002, aes(x=ANIOA, y=Freq,fill=NIN2)) +facet_grid(NIN1~.)+
  geom_bar(stat="identity",position = position_dodge())+geom_text(aes(label = Freq), vjust =0, size = 3.5)+
  ylab("frecuencia relativa")+
  scale_fill_manual(values=c("darkgreen", "purple","black"))


###################################################################################
#######################pregunta 6############################################################
#############################################################################################

tabla_3<-prop.table(table(select(datos,c(ANIOP,ANIOA,RAZAN))))
tabla_3
datos_3<-as.data.frame(tabla_3)
datos_3

ggplot(data=datos_3, aes(x=ANIOP, y=Freq,fill=ANIOA)) +facet_grid(RAZAN~.) +
  geom_bar(stat="identity",position = position_dodge()) +geom_text(aes(label = Freq), vjust =0, size = 2.5)+
  ylab("Frecuencia relativa")+
  scale_fill_manual(values=c("darkgreen", "purple","black"))

