library(readxl)
rm(list=ls())
Data <- read_excel("Data.xlsx", sheet = "DataLimpia")
#Data es ciertos datos demográficos o preguntas que puedan afectar la valuación
#contingente para reactores nucleares de potencia. Cada persona aparece 10 veces,
#una vez por cada pregunta que respondió en la seccion de la valuación.
Data2 <- read_excel("Data.xlsx", sheet = "RespuestasTotal")
Data2[sapply(Data2, is.character)] <- lapply(Data2[sapply(Data2, is.character)], 
                                             as.factor)
Data2 <- as.data.frame(Data2)
#Data2 va a servir principalmente para realizar gráficos para visualizar demográficas
# y las respuestas a los reactores nucleares de investigación. Cada persona
#aparece una vez. El as.data.frame es para convertir todo lo que es texto a factor
library(reshape2)
library(ggplot2)
l <- reshape(Data2, 
             varying = c("DiezKm", "CincuentaKm", "CienKm", "QuinientosKm"), 
             v.names = "BeneficioMinimo",
             timevar = "Distancia", 
             times = factor(c("10Km", "50Km", "100Km", "500Km"),
                            levels = c("10Km", "50Km", "100Km", "500Km")),
             new.row.names = 1:5732,
             direction = "long")
library(dplyr)
library("RColorBrewer")
longInv <- as.data.frame(l)
longInv <- longInv %>% 
  mutate(BeneficioMinimo = factor(BeneficioMinimo, levels = c("NO ACEPTARÍA", "375$", "750$", "1500$", "3000$", "6000$")))

ggplot(data = longInv, aes(x=Distancia, fill=BeneficioMinimo)) + 
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "Set2")

pred.data <- data.frame(Distancia = rep(0:600, times = 15),
                        Dinero = rep(seq.int(0,7000, 500), each = 601))


#modelo básico
modelo1 <- glm(RespuestaBinaria ~ Distancia + Dinero, binomial, data = Data)
summary(modelo1)

p1 <- predict(modelo1, pred.data)

pred.data1 <- pred.data %>% mutate(p_aceptar = exp(p1)/(1+exp(p1)))

ggplot(pred.data1, aes(Distancia, Dinero)) +
  #geom_raster(aes(fill = p_aceptar)) +
  geom_contour(aes(z=p_aceptar))+
  annotate(geom="text", x=550, y=6800, label="85%") + 
  annotate(geom="text",x=500,y=5300, label="80%") + 
  annotate(geom="text",x=400,y=4800, label="75%") +
  annotate(geom="text",x=300,y=4300, label="70%") + 
  annotate(geom="text",x=270,y=3500, label="65%") +
  annotate(geom="text",x=200,y=3000, label="60%") + 
  annotate(geom="text",x=180,y=2000, label="55%") + 
  annotate(geom="text",x=100,y=1800, label="50%") + 
  annotate(geom="text",x=60,y=1000, label="45%") +
  annotate(geom="text",x=30,y=250, label="40%") + 
  labs(title = "Probabilidad de estar de acuerdo con la construcción de una central \nnuclear de potencia",
       y = "Dinero [AR$]",
       x = "Distancia [km]")


#modelo log(distancia)
modelo2 <- glm(RespuestaBinaria ~ log(Distancia) + Dinero, binomial, data = Data)
summary(modelo2) #mejorcito

p2 <- predict(modelo2, pred.data)
pred.data2 <- pred.data %>% mutate(p_aceptar = exp(p2)/(1+exp(p2)))

ggplot(pred.data2, aes(Distancia, Dinero)) +
  #geom_raster(aes(fill = p_aceptar)) +
  geom_contour(aes(z=p_aceptar)) + 
  annotate(geom="text", x=475, y=5600, label="80%") + 
  annotate(geom="text",x=270,y=3500, label="70%") +
  annotate(geom="text",x=180,y=2000, label="60%") + 
  annotate(geom="text",x=100,y=900, label="50%") + 
  annotate(geom="text",x=40,y=400, label="40%") + 
  labs(title = "Probabilidad de estar de acuerdo con la construcción de una central nuclear de potencia",
       y = "Dinero [AR$]",
       x = "Distancia [km]")

modelo3 <- glm(RespuestaBinaria ~ log(Distancia) + log(Dinero), binomial, data=Data)
summary(modelo3)
p3 <- predict(modelo3, pred.data)
pred.data3 <- pred.data %>% mutate(p_aceptar = exp(p3)/(1+exp(p3)))


ggplot(pred.data3, aes(Distancia, Dinero)) +
  #geom_raster(aes(fill = p_aceptar)) +
  geom_contour(aes(z=p_aceptar)) +
  annotate(geom="text", x=500, y=6200, label="80%") + 
  annotate(geom="text",x=200,y=3500, label="70%") +
  annotate(geom="text",x=120,y=1700, label="60%") + 
  annotate(geom="text",x=80,y=900, label="50%") + 
  annotate(geom="text",x=40,y=640, label="45%") + 
  labs(title = "Probabilidad de estar de acuerdo con la construcción de una central nuclear de potencia",
       y = "Dinero [AR$]",
       x = "Distancia [km]")

modelo4 <- glm(RespuestaBinaria ~ Distancia + log(Dinero), binomial, data = Data)
summary(modelo4)
p4 <- predict(modelo4, pred.data)
pred.data4 <- pred.data %>% mutate(p_aceptar = exp(p4)/(1+exp(p4)))

ggplot(pred.data4, aes(Distancia, Dinero)) +
  #geom_raster(aes(fill = p_aceptar)) +
  geom_contour(aes(z=p_aceptar)) +
  annotate(geom="text", x=500, y=6200, label="80%") + 
  annotate(geom="text",x=340,y=3500, label="70%") +
  annotate(geom="text",x=235,y=1700, label="60%") + 
  annotate(geom="text",x=85,y=1300, label="50%") + 
  labs(title = "Probabilidad de estar de acuerdo con la construcción de una central nuclear de potencia",
       y = "Dinero [AR$]",
       x = "Distancia [km]")


modelo5 <- glm(RespuestaBinaria ~ Distancia + I(Distancia^2)+ Dinero, binomial, data = Data)
summary(modelo5) #casi igual al modelo1
#igual el coeficiente para distancia^2 siendo negativo es raro, genera una curva
#pero en teoria en distancias gigantescas bajaria la probabilidad de aceptar?

p5 <- predict(modelo5, pred.data)
pred.data5 <- pred.data %>% mutate(p_aceptar = exp(p5)/(1+exp(p5)))
ggplot(pred.data5, aes(Distancia, Dinero)) +
  #geom_raster(aes(fill = p_aceptar)) +
  geom_contour(aes(z=p_aceptar)) +
  annotate(geom="text", x=340, y=5500, label="85%") + 
  annotate(geom="text",x=340,y=3500, label="80%") +
  annotate(geom="text",x=340,y=1900, label="75%") + 
  annotate(geom="text",x=340,y=300, label="70%") + 
  labs(title = "Probabilidad de estar de acuerdo con la construcción de una central \nnuclear de potencia",
       y = "Dinero [AR$]",
       x = "Distancia [km]")

modelo6 <- glm(RespuestaBinaria ~ Distancia + Dinero + I(Dinero^2), binomial, data = Data)
summary(modelo6)
p6 <- predict(modelo6, pred.data)
pred.data6 <- pred.data %>% mutate(p_aceptar = exp(p6)/(1+exp(p6)))
ggplot(pred.data6, aes(Distancia, Dinero)) +
  #geom_raster(aes(fill = p_aceptar)) +
  geom_contour(aes(z=p_aceptar)) +
  annotate(geom="text", x=610, y=6500, label="85%") + 
  annotate(geom="text", x=500, y=5000, label="80%") + 
  annotate(geom="text",x=410,y=4000, label="75%") +
  annotate(geom="text",x=340,y=3500, label="70%") +
  annotate(geom="text",x=250,y=3000, label="65%") + 
  annotate(geom="text",x=210,y=2300, label="60%") + 
  annotate(geom="text",x=150,y=2000, label="55%") + 
  annotate(geom="text",x=85,y=1580, label="50%") + 
  annotate(geom="text",x=60,y=1100, label="45%") + 
  annotate(geom="text",x=40,y=500, label="40%") + 
  labs(title = "Probabilidad de estar de acuerdo con la construcción de una central \nnuclear de potencia",
       y = "Dinero [AR$]",
       x = "Distancia [km]")


modelo7 <- glm(RespuestaBinaria ~ Distancia + I(Distancia^2) + Dinero + I(Dinero^2), binomial, data = Data)
summary(modelo7)
p7 <- predict(modelo7, pred.data)
pred.data7 <- pred.data %>% mutate(p_aceptar = exp(p7)/(1+exp(p7)))
ggplot(pred.data7, aes(Distancia, Dinero)) +
  #geom_raster(aes(fill = p_aceptar)) +
  geom_contour(aes(z=p_aceptar)) +
  annotate(geom="text", x=340, y=5000, label="85%") + 
  annotate(geom="text",x=340,y=3000, label="80%") +
  annotate(geom="text",x=340,y=1600, label="75%") + 
  annotate(geom="text",x=340,y=650, label="70%") + 
  labs(title = "Probabilidad de estar de acuerdo con la construcción de una central \nnuclear de potencia",
       y = "Dinero [AR$]",
       x = "Distancia [km]")


#Modelos con otras variables

modelosexo <- glm(RespuestaBinaria ~ log(Distancia) + Dinero + Sexo, binomial, data = Data)
summary(modelosexo) 

modeloedad <- glm(RespuestaBinaria ~ log(Distancia) + Dinero + Edad, binomial, data = Data)
summary(modeloedad) 

modeloedad <- glm(RespuestaBinaria ~ log(Distancia) + Dinero + NivelEducativoAlcanzado, binomial, data = Data)
summary(modeloedad) 

modelolocacion <- glm(RespuestaBinaria ~ log(Distancia) + Dinero + Residencia, binomial, data = Data)
summary(modelolocacion) 

#Otros gráficos
levels(Data2$PercepcionInfoNuclearARG) <- c("Nada", "Poco", "Medianamente Informado", "Mucho")
ggplot(Data2, aes(PercepcionInfoNuclearARG)) + 
  geom_bar(fill = "turquoise4") +
  labs(title = "¿Estás informado sobre la actividad nuclear argentina?",
       y = "Cantidad") + 
  theme(axis.title.x=element_blank())

ggplot(Data2, aes(RiesgoSaludCentralCercana)) + 
  geom_bar(fill="turquoise4") + 
  labs(title = "¿Considerás que la presencia de una central nuclear \nen tu localidad implica un riesgo para tu salud?",
       x="1 es Ningún riesgo y 5 es Muy Riesgoso",
       y="Cantidad")

ggplot(Data2, aes(DeberiaExistirMasEducacion)) + 
  geom_bar(fill="turquoise4") + 
  labs(title = "¿Creés que debería existir una mayor comunicación y educación \nacerca de las características de éste tipo de instalaciones?",
       x="1 es Innecesario y 5 es Muy Necesario",
       y="Cantidad")

ggplot(Data2, aes(ValoracionAporteTrabajo)) + 
  geom_bar(fill="turquoise4") + 
  labs(title = "¿Valorarías el aporte en cuanto a puestos de trabajo que generaría \nla instalación de una planta nuclear en tu localidad?",
       y = "Cantidad",
       x = "1 es Poco y 5 es Mucho")

ggplot(data = Data2, aes(x=AutopercepcionConocimientoNuclear)) + 
  geom_bar(fill="turquoise4") + 
  labs(title = "¿Cuánto conocimiento acerca de las centrales nucleares y sus riesgos \ncreés tener?",
       y = "Cantidad",
       x = "1 es Poco y 5 es Mucho")

