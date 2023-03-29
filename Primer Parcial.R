library(tidyverse)
library(ggplot2)
library(janitor)
library(skimr)
library(GGally)
library(ggpubr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(treemap)
library(forcats)
library(hrbrthemes)
library(ggridges)


#Tratamiento del dataset
rain = read_csv("C:/Users/bruno/OneDrive/Carrera/Primer cuatrimestre 2023/Análisis Predictivo/archive (15)/weatherAUS.csv")


rain %>% head(5) %>% View()

glimpse(rain)
head(rain,10)

#Chequeo de duplicados
rain %>% group_by(Date,Location) %>% summarise(cantidad=n()) %>% filter(cantidad>1)  #No hay mas de un registro por dia por ciudad


#Missings


######################
summary(rain)
#######################

rain$missings = rowSums(is.na(rain))
total_missings = rain %>% group_by(missings) %>% summarise(cantidad=n(), porcentaje = round(n()/nrow(rain)*100,2))
acumulado = c(total_missings$porcentaje[1])
for (i in c(2:length(total_missings$porcentaje))) {
  acumulado[i] = acumulado[i-1] + total_missings$porcentaje[i]
}
acumulado = round(acumulado,1)
total_missings$frecuencia_acumulada = acumulado
total_missings$total_na = total_missings$cantidad * total_missings$missings

total_missings %>% View()

total_missings %>% ggplot(aes(x = missings, y = frecuencia_acumulada)) +
  geom_line(size = 1.5, color = "blue") +
  xlab("Cantidad de missings por fila") +
  ylab("Frecuencia acumulada (%)")

###########################################
delete = rain %>% filter(missings>=14)
colMeans(is.na(delete)) * 100  #de las filas que voy a eliminar, cómo se distribuyen los faltantes entre los diferentes features
cities_deleted = delete %>% group_by(Location) %>% summarise(cantidad=n())
years_deleted = delete %>% group_by(año = format(Date,"%Y")) %>% summarise(cantidad=n())
months_deleted = delete %>% group_by(mes = format(Date,"%m")) %>% summarise(cantidad=n())
days_deleted = delete %>% group_by(dia = format(Date,"%d")) %>% summarise(cantidad=n())


rain %>% dim()

cities_deleted %>% View()

##########################################
rain = rain %>% filter(missings<14) %>% select(-missings) #Elimino a los que tienen 14 missings o más
porcentaje_missings = colMeans(is.na(rain)) * 100  #hay cuatro columnas que tienen aproximadamente el 40% faltante


#########################################


#Outliers (variables numéricas)

boxplot(rain$MinTemp, rain$MaxTemp, rain$Temp9am, rain$Temp3pm, main="Temperaturas (°c)", names = c("Temperaturas Mínimas", "Temperaturas Máximas", "Temperatura 9am", "Temperatura 3pm"))
boxplot(rain$Rainfall, rain$Evaporation, main="Precipitaciones y evaporación (mm)", names = c("Precipitación", "Evaporación"))
boxplot(rain$Sunshine, main = "Horas de sol brillante durante el día")
boxplot(rain$WindGustSpeed, rain$WindSpeed9am, rain$WindSpeed3pm, main="Velocidad del viento (km/h)", names=c("Velocidad máxima", "Velocidad 9am", "Velocidad 3pm"))
boxplot(rain$Humidity9am, rain$Humidity3pm, main="Humedad (%)", names = c("Humedad 9am", "Humedad 3pm"))
boxplot(rain$Pressure9am, rain$Pressure3pm, main="Presión atmosférica (hpa)", names = c("Presión 9am", "Presión 3pm"))
boxplot(rain$Cloud9am, rain$Cloud3pm, main="Nubosidad (octas)", names = c("Nubosidad 9am", "Nubosidad 3pm"))


#Variables categoricas
funModeling::freq(rain$Location)

wind_data = rain %>% group_by(WindDir9am, WindDir3pm, WindGustDir) %>% summarise(cantidad=n()) %>% arrange(desc(cantidad)) %>%  head(100)
wind1 = rain %>% group_by(WindGustDir) %>% summarise(cantidad=n()) %>% arrange(desc(cantidad)) %>%  head(5)
wind2 = rain %>% group_by(WindDir9am) %>% summarise(cantidad=n()) %>% arrange(desc(cantidad)) %>%  head(5)
wind3 = rain %>% group_by(WindDir3pm) %>% summarise(cantidad=n()) %>% arrange(desc(cantidad)) %>%  head(5)
ggplot(wind1, aes(x = WindGustDir, y = cantidad)) + geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7), stat = "identity")
ggplot(wind2, aes(x = WindDir9am, y = cantidad)) + geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7), stat = "identity")
ggplot(wind3, aes(x = WindDir3pm, y = cantidad)) + geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7), stat = "identity")


tabla = addmargins(table(rain %>% select(RainToday, RainTomorrow), useNA = "ifany"))
tabla = tabla[1:3,1:3]
mosaicplot(tabla, color = c("red", "green", "blue"), main = "Rain Today vs. Rain Tomorrow", labeller = label_both) 

###########################################


##Análisis EDA sobre variables categóricas (relcionándolas con RainToday y Rain Tomorrow)

###¿En qué ciudades llueve más (cantidad de veces, no cantidad de mm)? ¿En cuáles se pronostica que hay más lluvias para el día siguiente? 


rain_today = rain %>% filter(!is.na(RainToday)) %>% group_by(Location) %>% summarise(rain_yes = sum(RainToday == 'Yes'), rain_no = sum(RainToday == "No"), cantidad=n(), porcentaje_si = (rain_yes/cantidad)*100, porcentaje_no = (rain_no/cantidad)*100 )
rain_today = rain_today %>% select(Location, porcentaje_si, porcentaje_no)

rain_today %>% arrange(desc(porcentaje_si)) %>% head(5) %>% 
  ggplot( aes(x=porcentaje_si, y=Location)) +
  geom_bar(stat="identity", fill="blue", alpha=.6, width=.4) +
  coord_flip() +
  xlab("% de días lluviosos") +
  ylab("Ubicacion")
  theme_bw()

rain_today %>% arrange(desc(porcentaje_no)) %>% head(5) %>% 
  ggplot( aes(x=porcentaje_no, y=Location)) +
  geom_bar(stat="identity", fill="red", alpha=.6, width=.4) +
  coord_flip() +
  xlab("% de días no lluviosos") +
  ylab("Ubicación")
  theme_bw()



rain_tomorrow = rain %>% filter(!is.na(RainTomorrow) & !is.na(RainToday)) %>% group_by(Location) %>% summarise(iguales = sum(RainToday==RainTomorrow), distintos = sum(RainToday!=RainTomorrow), cantidad=n(), porcentaje_iguales=(iguales/cantidad)*100, porcentaje_distintos=(distintos/cantidad)*100)
rain_tomorrow = rain_tomorrow %>% select(Location, porcentaje_iguales, porcentaje_distintos)

rain_tomorrow %>% arrange(desc(porcentaje_iguales)) %>% head(5) %>% 
  ggplot( aes(x=porcentaje_iguales, y=Location)) +
  geom_bar(stat="identity", fill="brown", alpha=.6, width=.4) +
  coord_flip() +
  ylab("Ubicación") +
  xlab("% de igualdad entre RainToday y RainTomorrow")
  theme_bw()

rain_tomorrow %>% arrange(desc(porcentaje_distintos)) %>% head(5) %>% 
  ggplot( aes(x=porcentaje_distintos, y=Location)) +
  geom_bar(stat="identity", fill="purple", alpha=.6, width=.4) +
  coord_flip() +
  ylab("Ubicación") +
  xlab("% de igualdad entre RainToday y RainTomorrow")
  theme_bw()

# ¿Hay independencia entre si hoy llueve a si lloverá mañanana?

tabla
chisq.test(tabla)




###¿Hay una época del año donde llueve más?

rain_today_year = rain %>% filter(!is.na(RainToday)) %>% group_by(año = substr(Date,1,4)) %>% summarise(cantidad=n(), dias_lluviosos = sum(RainToday=="Yes"), porcentaje_lluvia = (dias_lluviosos/cantidad)*100)
rain_today_month = rain %>% filter(!is.na(RainToday)) %>% group_by(mes = substr(Date,6,7)) %>% summarise(cantidad=n(), dias_lluviosos = sum(RainToday=="Yes"), porcentaje_lluvia = (dias_lluviosos/cantidad)*100)
rain_today_day = rain %>% filter(!is.na(RainToday)) %>% group_by(dia = substr(Date,9,10)) %>% summarise(cantidad=n(), dias_lluviosos = sum(RainToday=="Yes"), porcentaje_lluvia = (dias_lluviosos/cantidad)*100)


rain_today_year %>%
  arrange(porcentaje_lluvia) %>% 
  ggplot( aes(x=año, y=porcentaje_lluvia)) +
  geom_segment( aes(xend=año, yend=0)) +
  geom_point( size=4, color="light blue") +
  coord_flip() +
  theme_bw() +
  xlab("Año") +
  ylab("% de días lluviosos")


rain_today_month %>%
  arrange(porcentaje_lluvia) %>% 
  ggplot( aes(x=mes, y=porcentaje_lluvia)) +
  geom_segment( aes(xend=mes, yend=0)) +
  geom_point( size=4, color="light blue") +
  coord_flip() +
  theme_bw() +
  xlab("Mes")
  ylab("% de días lluviosos")


rain_today_day %>%
  arrange(porcentaje_lluvia) %>% 
  ggplot( aes(x=dia, y=porcentaje_lluvia)) +
  geom_segment( aes(xend=dia, yend=0)) +
  geom_point( size=4, color="light blue") +
  coord_flip() +
  theme_bw() +
  xlab("")


#¿Cambia la dirección del viento en aquellos meses donde llueve menos respecto a los que llueve más?

#Meses de verano

winds_summer = rain %>% filter(substr(Date,6,7) %in% c("01","02","03")) %>% group_by(WindGustDir) %>% summarise(cantidad=n()) %>% arrange(desc(cantidad)) %>% head(5)
winds_winter = rain %>% filter(substr(Date,6,7) %in% c("06","07","08")) %>% group_by(WindGustDir) %>% summarise(cantidad=n()) %>% arrange(desc(cantidad)) %>% head(5)


barplot(height=winds_summer$cantidad, names=winds_summer$WindGustDir, 
        col=rgb(0.8,0.1,0.1,0.6),
        xlab="Viento más fuerte registrado", 
        ylab="Cantidad de días", 
        main="Vientos más fuertes registrados en verano", 
        ylim=c(0,4000)
)

barplot(height=winds_winter$cantidad, names=winds_winter$WindGustDir, 
        col="blue",
        xlab="Viento más fuerte registrado", 
        ylab="Cantidad de días", 
        main="Vientos más fuertes registrados en invierno", 
        ylim=c(0,4000)
)



### Correlaciones

prueba = rain %>% select(-c(Date, Location, WindGustDir, WindDir3pm,WindDir9am, RainToday, RainTomorrow))

GGally::ggcorr(
  prueba, method=c("pairwise","spearman"),  
  label=T, hjust=1, label_size=2, layout.exp=10, size=3)


#Análisis para correlacion:

#1. Temperatura y evaporación 


rain %>% filter(!is.na(RainTomorrow), !is.na(Temp3pm), !is.na(Evaporation)) %>% ggplot() + geom_point(aes(Temp3pm, y=Evaporation, color = RainToday)) + geom_smooth(aes(Temp3pm, y=Evaporation))
rain %>% filter(!is.na(RainTomorrow), !is.na(Temp3pm), !is.na(Evaporation)) %>% ggplot() + geom_point(aes(Temp3pm, y=Evaporation, color = RainTomorrow)) + geom_smooth(aes(Temp3pm, y=Evaporation))
rain %>% filter(is.na(RainTomorrow), !is.na(Temp3pm), !is.na(Evaporation)) %>% ggplot() + geom_point(aes(Temp3pm, y=Evaporation)) + geom_smooth(aes(Temp3pm, y=Evaporation))

rain %>% filter(RainToday=="Yes", !is.na(Temp3pm), !is.na(Evaporation), !is.na(RainTomorrow)) %>% ggplot() + geom_point(aes(x= Temp3pm, y = Evaporation, color = RainTomorrow)) + geom_smooth(aes(x= Temp3pm, y = Evaporation)) +ggtitle("Temperatura (3pm) vs. Evaporación - Días lluviosos")
rain %>% filter(RainToday=="No", !is.na(Temp3pm), !is.na(Evaporation), !is.na(RainTomorrow)) %>% ggplot() + geom_point(aes(x= Temp3pm, y = Evaporation, color = RainTomorrow)) + geom_smooth(aes(x= Temp3pm, y = Evaporation)) +ggtitle("Temperatura (3pm) vs. Evaporación - Días no lluviosos")

#2. Sunshine y cloud

rain %>% filter(!is.na(Sunshine) & !is.na(Cloud3pm) & !is.na(RainToday)) %>% ggplot() + geom_point(aes(x=Sunshine, y=Cloud3pm, color = RainToday)) + geom_smooth(aes(x=Sunshine, y=Cloud3pm)) 
rain %>% filter(!is.na(Sunshine) & !is.na(Cloud3pm) & !is.na(RainTomorrow)) %>% ggplot() + geom_point(aes(x=Sunshine, y=Cloud3pm, color = RainTomorrow)) + geom_smooth(aes(x=Sunshine, y=Cloud3pm)) 
rain %>% filter(!is.na(Sunshine) & !is.na(Cloud3pm) & is.na(RainTomorrow)) %>% ggplot() + geom_point(aes(x=Sunshine, y=Cloud3pm)) + geom_smooth(aes(x=Sunshine, y=Cloud3pm)) 

#3 Sunshine y Humedad 3pm
rain %>% filter(RainToday=="Yes", !is.na(Humidity3pm), !is.na(Sunshine), !is.na(RainTomorrow)) %>% ggplot() + geom_point(aes(x= Humidity3pm, y = Sunshine, color = RainTomorrow)) + geom_smooth(aes(x= Humidity3pm, y = Sunshine)) +ggtitle("Humedad (3pm) vs. Horas de sol - Días lluviosos")
rain %>% filter(RainToday=="No", !is.na(Humidity3pm), !is.na(Sunshine), !is.na(RainTomorrow)) %>% ggplot() + geom_point(aes(x= Humidity3pm, y = Sunshine, color = RainTomorrow)) + geom_smooth(aes(x= Humidity3pm, y = Sunshine)) +ggtitle("Humedad (3pm) vs. Horas de sol - Días no lluviosos")



###De las veces que llueve, ¿cuál es la temperatura promedio? ¿Cuál es la velocidad promedio del viento? Conectar ambas variables (scatterplot)
## Mismo análisis que antes, pero para días en los que no llueve 


rain2 = rain %>% filter(RainToday == "Yes", !is.na(WindSpeed9am),!is.na(WindSpeed3pm) ,!is.na(MaxTemp), !is.na(MinTemp), !is.na(RainTomorrow), substr(Date,6,7) %in% c("07","08","09")) %>% mutate(Average_Temperature = (MaxTemp+MinTemp)/2, Average_Wind_Speed = (WindSpeed9am+WindSpeed3pm)/2)
rain2 %>% ggplot() + geom_point(aes(x=Average_Temperature, y = Average_Wind_Speed, color=RainTomorrow)) + xlab("Temperatura promedio (°c)") + ylab("Velocidad del viento promedio (km/h)") + ggtitle("Temperatura Promedio vs Veclodiad del viento promedio - Días lluviosos y de invierno") 


rain3 = rain %>% filter(RainToday == "No", !is.na(WindSpeed9am),!is.na(WindSpeed3pm) ,!is.na(MaxTemp), !is.na(MinTemp), !is.na(RainTomorrow), substr(Date,6,7) %in% c("01","02","03")) %>% mutate(Average_Temperature = (MaxTemp+MinTemp)/2, Average_Wind_Speed = (WindSpeed9am+WindSpeed3pm)/2)
rain3 %>% ggplot() + geom_point(aes(x=Average_Temperature, y = Average_Wind_Speed, color=RainTomorrow)) + xlab("Temperatura promedio (°c)") + ylab("Velocidad del viento promedio (km/h)") + ggtitle("Temperatura Promedio vs Veclodiad del viento promedio - Días no lluviosos y de verano") 


rain2 %>% ggplot(aes(x=Average_Temperature, y=Average_Wind_Speed) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis() +
  theme(
    legend.position='none'
  ) + xlab("Temperatura promedio (°c)") + ylab("Velocidad del viento promedio (km/h)") + ggtitle("Temperatura Promedio vs Veclodiad del viento promedio - Días lluviosos y de invierno") 


rain3 %>% ggplot(aes(x=Average_Temperature, y=Average_Wind_Speed) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis() +
  theme(
    legend.position='none'
  )+ xlab("Temperatura promedio (°c)") + ylab("Velocidad del viento promedio (km/h)") + ggtitle("Temperatura Promedio vs Veclodiad del viento promedio - Días no lluviosos y de verano") 

###### Humedad y velocidad del viento

rain4 = rain %>% filter(RainToday == "Yes", !is.na(WindSpeed9am),!is.na(WindSpeed3pm) ,!is.na(Humidity9am), !is.na(Humidity3pm)) %>% mutate(Average_Humidity = (Humidity3pm+Humidity9am)/2, Average_Wind_Speed = (WindSpeed9am+WindSpeed3pm)/2)

rain4 %>% ggplot(aes(x=Average_Humidity, y=Average_Wind_Speed) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis() +
  theme(
    legend.position='none'
  )+ xlab("Humedad Promedio (%)") + ylab("Velocidad del viento promedio (km/h)") + ggtitle("Humedad Promedio vs Velocidad promedio del viento promedio - Días no lluviosos") 

rain5 = rain %>% filter(RainToday == "No", !is.na(WindSpeed9am),!is.na(WindSpeed3pm) ,!is.na(Humidity9am), !is.na(Humidity3pm)) %>% mutate(Average_Humidity = (Humidity3pm+Humidity9am)/2, Average_Wind_Speed = (WindSpeed9am+WindSpeed3pm)/2)

rain5 %>% ggplot(aes(x=Average_Humidity, y=Average_Wind_Speed) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis() +
  theme(
    legend.position='none'
  )+ xlab("Temperatura promedio (°c)") + ylab("Velocidad del viento promedio (km/h)") + ggtitle("Temperatura Promedio vs Veclodiad del viento promedio - Días no lluviosos y de verano") 


#### Humidity vs Cloud (dependiendo si mañana lloverá)

rain6 = rain %>% filter(RainTomorrow == "No", !is.na(Cloud9am),!is.na(Cloud3pm) ,!is.na(Humidity9am), !is.na(Humidity3pm)) %>% mutate(Average_Humidity = (Humidity3pm+Humidity9am)/2, Average_Cloud = (Cloud9am+Cloud3pm)/2)

rain6 %>% ggplot(aes(x=Average_Humidity, y=Average_Cloud) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis() +
  theme(
    legend.position='none'
  )+ xlab("Humedad promedio (%)") + ylab("Nubosidad promedio (octas)") + ggtitle("Humedad Promedio vs Nubosidad promedio - Pronóstico sin lluvia para mañana") 

rain7 = rain %>% filter(RainTomorrow == "Yes", !is.na(Cloud9am),!is.na(Cloud3pm) ,!is.na(Humidity9am), !is.na(Humidity3pm)) %>% mutate(Average_Humidity = (Humidity3pm+Humidity9am)/2, Average_Cloud = (Cloud9am+Cloud3pm)/2)

rain7 %>% ggplot(aes(x=Average_Humidity, y=Average_Cloud) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis() +
  theme(
    legend.position='none'
  )+ xlab("Humedad promedio (%)") + ylab("Nubosidad promedio (octas)") + ggtitle("Humedad Promedio vs Nubosidad promedio - Pronóstico de lluvia para mañana") 
