# Script para analisis de base de datos campaña VIH
# Hecho por: Frank Zela-Coila
# Fecha: 06-01-2024

# Paquetes
  library(tidyverse)
  library(openxlsx)
  library(compareGroups)
  library(Hmisc)

# Subir la base de datos
  df_filtro <- read.xlsx("data/DB_VIH.xlsx")

# Cuestionario de conocimientos 
# Cada respuesta correcta se puntúa con uno (1) y las incorrectas con cero (0)
  df1 <- df_filtro %>%
    mutate(
      Puntaje_p1 = ifelse(p1 == "F", 1, 0), # rpta correcta es FALSO se puntúa 1
      Puntaje_p2 = ifelse(p2 == "F", 1, 0), # rpta correcta es FALSO se puntúa 1
      Puntaje_p3 = ifelse(p3 == "F", 1, 0), # rpta correcta es FALSO se puntúa 1
      Puntaje_p4 = ifelse(p4 == "F", 1, 0), # rpta correcta es FALSO se puntúa 1
      Puntaje_p5 = ifelse(p5 == "V", 1, 0), # rpta correcta es VERDADERO se puntúa 1
      Puntaje_p6 = ifelse(p6 == "F", 1, 0), # rpta correcta es FALSO se puntúa 1
      Puntaje_p7 = ifelse(p7 == "V", 1, 0), # rpta correcta es VERDADERO se puntúa 1
      Puntaje_p8 = ifelse(p8 == "F", 1, 0), # rpta correcta es FALSO se puntúa 1
      Puntaje_p9 = ifelse(p9 == "F", 1, 0), # rpta correcta es FALSO se puntúa 1
      Puntaje_p10 = ifelse(p10 == "F", 1, 0) # rpta correcta es FALSO se puntúa 1
    )

# Se realiza la suma de los puntajes para tener un ponderado general
  df1$Suma_Total <- rowSums(df1[, c("Puntaje_p1", "Puntaje_p2", 
                                    "Puntaje_p3", "Puntaje_p4", 
                                    "Puntaje_p5", "Puntaje_p6", 
                                    "Puntaje_p7", "Puntaje_p8", 
                                    "Puntaje_p9", "Puntaje_p10")])

# Se categoriza de 0-5 = malo; 6-7 = regular; 8-10 = bueno
  df1 <- df1 %>%
    mutate(
      score_vih = ifelse(Suma_Total > 5, 
                         ifelse(Suma_Total > 7, 2,1), 0)) # se categoriza 0, 1 y2
  
  df1$score_vih1 <- factor(df1$score_vih,
                           levels = c(0,1,2),# el 0, 1 y 2, pasan a ser malo, regular y bueno
                           labels = c("Malo", "Regular", "Bueno")) 
  

# Verificar normalidad de varialbes numéricas
  hist(df1$Suma_Total) # se prefiere usar el método gráfico
  shapiro.test(df1$Suma_Total) # no se recomienda usar
  hist(df1$edad) 
  hist(df1$edad_start_vida_sexual)
  hist(df1$Suma_Total)


# Tablas----
  # Tabla 1. Características sociodemográficas y de
  # conocimientos sobre VIH de estudiantes universitarios 
  
  df2 <- df1 %>% 
    select(edad, sexo, area, religion, anio_estudios, lgbti, 
           start_vida_sexual, edad_start_vida_sexual, 
           metodo_anticonceptivo, metodo_usado, 
           metodo_usado_Condón,metodo_usado_vac_tri,
           metodo_usado_impl_subde,metodo_usado_diafragma,
           metodo_usado_diafragma, metodo_usado_DIU_o_T,
           metodo_usado_píldoras_anticonceptivas,
           metodo_usado_pastilla_día_siguiente,
           metodo_usado_otro,
           
           previous_vih_test,
           
           recursos_aprendizaje,
           recursos_aprendizaje_casa,
           recursos_aprendizaje_periodicos,
           recursos_aprendizaje_tv,
           recursos_aprendizaje_radio,
           recursos_aprendizaje_internet,
           recursos_aprendizaje_colegio,
           recursos_aprendizaje_universidad,
           recursos_aprendizaje_otro,
           
           aprender_vih,
           aprender_vih_casa,aprender_vih_internet,
           aprender_vih_colegio,aprender_vih_escuela,
           aprender_vih_universidad,aprender_vih_otro,
           
           Suma_Total, score_vih1)
  
  # Se juntan todas las religiones diferentes a la católica
  df2 <- df2 %>%
    mutate(
      religion = ifelse(religion == "Católico", "Católico", "Otro")
    )
  
  tabla1 <- createTable(compareGroups(data=df2,
                                      method = c(2, 3, 3, 3, 3, 3, 
                                                 3, 2, 
                                                 3, 3, 
                                                 3,3,
                                                 3,3,
                                                 3, 3,
                                                 3,
                                                 3,
                                                 3,
                                                 
                                                 3,
                                                 
                                                 3,
                                                 3,
                                                 3,
                                                 3,
                                                 3,
                                                 3,
                                                 3,
                                                 3,
                                                 3,
                                                 
                                                 3,
                                                 3,3,
                                                 3,3,
                                                 3,2,
                                                 3
                                      )))
  
  tabla1
  export2xls(tabla1, "tables/tabla1.xlsx")
  
 
  # Tabla 2. Conocimiento sobre VIH malo-regular y bueno en estudiantes
  # según variables sociodemográficas
  
  # Para tener solo dos opciones, se busca tener Malo-Regular y Bueno
  df2$score_vih2 <- factor(df1$score_vih,
                           levels = c(0,1,2),
                           labels = c(0,0,1)) # se junta regular y malo

  table(df2$score_vih2)
  
  df2$score_vih2 <- as.numeric(df2$score_vih2)
  
  tabla2 <- createTable(compareGroups(score_vih2 ~ edad + sexo+ 
                                        area+ religion + 
                                        anio_estudios+ lgbti+ 
                                        start_vida_sexual +
                                        previous_vih_test,
                                      data=df2,byrow = T,
                                      method = c(2,3, 
                                                 3,3,
                                                 3,3,3,
                                                 3,3)))
  
  tabla2
  export2xls(tabla2, "tables/tabla2.xlsx")
  
  # Tabla 3. Conocimientos sobre formas de transmisión y efectos del VIH/SIDA 
  # en estudiantes universitarios 
  
  df_know <- df1 %>% 
    select(Puntaje_p1, Puntaje_p2, Puntaje_p3, 
           Puntaje_p4, Puntaje_p5, Puntaje_p6, 
           Puntaje_p7, Puntaje_p8, Puntaje_p9, Puntaje_p10)
  
  
  tabla3 <- createTable(compareGroups(data=df_know,
                                      method = c(3, 3, 3, 3, 3, 
                                                 3, 3, 3, 3, 3)))
  tabla3 
  export2xls(tabla3, "tables/tabla3.xlsx")
  
  
  # Tabla 4. Conocimientos sobre formas de transmisión y efectos del VIH/SIDA 
  # en universitarios por sexo
  df_know_sex <- df1 %>% 
    select(sexo, Puntaje_p1, Puntaje_p2, Puntaje_p3, 
           Puntaje_p4, Puntaje_p5, Puntaje_p6, 
           Puntaje_p7, Puntaje_p8, Puntaje_p9, Puntaje_p10)
  
  
  tabla4 <- createTable(compareGroups(sexo ~ Puntaje_p1+ Puntaje_p2+ Puntaje_p3+ 
                                        Puntaje_p4+ Puntaje_p5+ Puntaje_p6+ 
                                        Puntaje_p7+ Puntaje_p8+ Puntaje_p9+ Puntaje_p10,
                                      data=df_know_sex,byrow = F,
                                      method = c(3,3,3, 
                                                 3,3,
                                                 3,3,3,
                                                 3,3)))
  
  tabla4
  export2xls(tabla4, "tables/tabla3.xlsx")
  
  
  # Tabla 5. Tabla 5. Factores asociados a tener buen conocimiento sobre VIH 
  # en estudiantes universitarios 
  library(sjPlot)
  is.numeric(df2$edad)
  df2$anio_estudios <- as.factor(df2$anio_estudios)
  df2$metodo_anticonceptivo <- as.factor(df2$metodo_anticonceptivo)
  table(df2$metodo_anticonceptivo)
  
  
  model1 <- glm(data=df2, 
                formula = score_vih2 ~ edad + sexo+ 
                  area + religion + 
                  anio_estudios+ lgbti +
                  start_vida_sexual + 
                  previous_vih_test,
                family = poisson(link = "log"))
  
  table(df2$score_vih2)
  
  df2$score_vih2 <- as.numeric(df2$score_vih2)
  
  table(df2$score_vih2)
  
  sjPlot::tab_model(model1)
  summary(model1)
  library(sjPlot)
  tab_model(model1)
  
    RP.Poisson <- function(modelo) 
    {
      library(sandwich)
      m1 <- modelo
      cov.m1 <- vcovHC(m1, type="HC0")
      std.err <- sqrt(diag(cov.m1))
      r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
                     "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
                     LL = coef(m1) - 1.96 * std.err,
                     UL = coef(m1) + 1.96 * std.err,
                     "RP" = exp(coef(m1)),
                     "LCL" = exp(coef(m1) - 1.96 * std.err),
                     "UCL" = exp(coef(m1) + 1.96 * std.err))
      return(r.est)
      rm(m1,cov.m1,std.err,r.est)
    }
    
      
    RP.Poisson(model1)
    
    model1 <- as.data.frame(RP.Poisson(model1))
    
    model1$variable <- rownames(model1)
    
    rownames(model1) <- NULL
    model1
    
    write.xlsx(model1, "tables/Reg1.xlsx")


