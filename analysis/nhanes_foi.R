

test <- generate_foi(age = 0:100, 
                     sex = c("Male","Female"),
                     race = c("Non-Hispanic White", "Non-Hispanic Black", 
                              "Other Hispanic", "Mexican-American", 
                              "Other"),
                     period = c(1991:1994))

test3 <- summary_foi(age = c(0:100), 
                     sex = c("Male", "Female"), 
                     race = c("Non-Hispanic White", "Other"), 
                     period = c(1991:1992), percs = T)



plot_foi(age = c(0:100), 
         sex = c("Male", "Female"), 
         race = c("Non-Hispanic White", "Other"), 
         period = c(1991:2000), percs = F)


library(hpgenerator)

test4 <- plot_foi()

#Repo hpgenerator. Generate shiny app. 

#Guardar data frame. alpha y gamma. agregar columna country: USA. 

#summary_foi percs. Automatizarlo. 

#Convertir data frames a objetos de paquete de R. en DATA. 

#númerico period

#dataframe generate_foi

test1 <- generate_foi(period = c("1991", "2000"))



dim(test1)


test2 <- summary_foi(period = c("1991", "2000", "2010"), race = c("Mexican-American"))


plot_foi(period = c("1991", "1992", "1993", "1994", "1995"), race = c("Non-Hispanic White"))

#Modelo estadístico
#parámetros de jags
#evaluamos modelos muchas veces. Uncertainty propagation. Modelación con incertidumbre. 


#Hexamapas

#matriz. sacar point estimates.
#filas edad, columnas periodo y pasar a función de hexamapas. 

#plot_foi_hexamap

#unit tests. 

#1991 a 2030

#data meter df_gammas y df_alphas. Guardar como Rdata. 

#nhanes

#smoking 



