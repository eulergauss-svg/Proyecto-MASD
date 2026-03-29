# ==============================
# BIBLIOTECAS
# ==============================

install.packages("tidyverse")
install.packages("readxl")
install.packages("fitdistrplus")
install.packages("actuar")
install.packages("moments")
install.packages("writexl")
install.packages("goftest")

library(tidyverse)
library(readxl)
library(fitdistrplus)
library(actuar)
library(moments)
library(writexl) # para guardar la base de datos limpia
library(goftest)

# ==============================
# CARGAR DATOS
# ==============================

# Asumimos que estamos en el directorio general, el del repositorio de Github

ruta <- "data/Basehistorica_2000_a_2024.xlsx"

# Solo consideraremos "Riesgos Hidrometeorológico"

df_original <- read_excel(ruta) %>% 
  filter(`Clasificación del fenómeno` == "Hidrometeorológico")

# ==============================
# LIMPIEZA DE LOS DATOS
# ==============================

# Sólo trbajamos con datos en los que tenemos el valor de las pérdidas

df_original <- df_original %>%
  mutate(`Total de daños (millones de pesos)` = as.numeric(`Total de daños (millones de pesos)`))

df_original <- df_original %>%
  filter(!is.na(`Total de daños (millones de pesos)`), 
         `Total de daños (millones de pesos)` > 0)

df_original <- df_original %>%
  # Paso 0: Correcciones manuales
  mutate(
    `Fecha de Inicio` = case_when(
      row_number() == 2171 ~ "43699",
      row_number() == 2219 ~ "43796",
      TRUE ~ as.character(`Fecha de Inicio`)
    ),
    `Fecha de Fin` = case_when(
      row_number() == 2171 ~ "43699",
      row_number() == 2219 ~ "43796",
      TRUE ~ as.character(`Fecha de Fin`)
    )
  ) %>%
  
  # Paso 1: Corregir el caso especial "29-Sep"
  mutate(
    `Fecha de Inicio` = if_else(
      `Fecha de Fin` == "29-Sep",
      "37163",
      as.character(`Fecha de Inicio`)
    ),
    `Fecha de Fin` = if_else(
      `Fecha de Fin` == "29-Sep",
      "37163",
      as.character(`Fecha de Fin`)
    )
  ) %>%
  
  # Paso 2: Convertir serial de Excel a Date
  mutate(
    `Fecha de Inicio` = as.Date(
      as.numeric(ifelse(!is.na(`Fecha de Inicio`) & grepl("^[0-9]+$", `Fecha de Inicio`),
                        `Fecha de Inicio`, NA)),
      origin = "1899-12-30"
    ),
    `Fecha de Fin` = as.Date(
      as.numeric(ifelse(!is.na(`Fecha de Fin`) & grepl("^[0-9]+$", `Fecha de Fin`),
                        `Fecha de Fin`, NA)),
      origin = "1899-12-30"
    )
  ) %>%
  
  # Paso 3: Corregir Fechas
  mutate(
    `Fecha de Inicio` = if_else(is.na(`Fecha de Inicio`), `Fecha de Fin`, `Fecha de Inicio`),
    `Fecha de Fin`    = if_else(is.na(`Fecha de Fin`),    `Fecha de Inicio`, `Fecha de Fin`)
  )

df_hidro <- df_original %>%
  # Renombrar columnas 
  rename(
    anio = `Año`,
    clasificacion = `Clasificación del fenómeno`,
    tipo_fenomeno = `Tipo de fenómeno`,
    estado = `Estado`,
    perdidas = `Total de daños (millones de pesos)`
  ) %>%
  
  # Quitamos columnas que no nos sirvan 
  dplyr::select(
    -clasificacion,          
    -`Municipios Afectados`, 
    -`Descripcion general de los daños`, 
    -`Fuente`,             
    -`Defunciones`,          
    -`Población afectada`,               
    -`Viviendas dañadas`,              
    -`Escuelas`,              
    -`Hospitales`,          
    -`Comercios`,       
    -`Area de cultivo dañada / pastizales (h)` 
  ) %>%
  
  # Casos particulares de Varios Estados
  
  mutate(
    estado = if_else(
      row_number() == 1365,  # ajusta el número si cambió tras eliminar fila 33
      "Sonora, Sinaloa, Chihuahua, Durango, Zacatecas, Veracruz",
      estado
    )
  ) %>%
  
  # Limpiar estados
  mutate(
    estado = str_replace_all(estado, " y ", ","),
    estado = str_trim(estado)
  ) %>%
  
  # Contar estados
  mutate(n_estados = str_count(estado, ",") + 1) %>%
  
  # Separar filas
  separate_rows(estado, sep = ",") %>%
  
  # Limpiar otra vez
  mutate(estado = str_trim(estado)) %>%
  
  # Ajustar daños 
  mutate(
    perdidas = perdidas / n_estados
    # La parte de Municipios Afectados ya no es necesaria porque eliminaste esa columna
  ) %>%
  
  dplyr::select(-n_estados)

# Quitamos la fila con clasificación de "Varios Estados sin detalle"
df_hidro <- df_hidro[-34, ]

# Quitamos la fila con clasificación de "otros"
df_hidro <- df_hidro[-5, ]


# ==============================
# Primer Análisis
# ==============================

p <- df_hidro$perdidas
min(p)
max(p)

# Rango muestral 
max(p)-min(p)

# Cuartiles
quantile(p,c(0.25,0.50,0.75))

median(p)
mean(p)

var(p)
sd(p)

# Interválo de Confianza del 68%

cat(mean(p)-sd(p),mean(p)+sd(p))

# Interválo de Confianza del 95%

cat(mean(p)-sd(p)*2,mean(p)+sd(p)*2)

# Interválo de Confianza del 99%

cat(mean(p)-sd(p)*3,mean(p)+sd(p)*3)

# Coeficiente de Variación

sd(p)/mean(p)

# Coeficiente de Asímetria

skewness(p)

# Curtouis

kurtosis(p)

# Análisis Gráfico 

# Regla de Sturges
k_sturges <- nclass.Sturges(df_hidro$perdidas)

# Histograma
hist(p,breaks=k_sturges)

# Boxplot
boxplot(p)

# Visualización de Outliers

# 1. Obtener las estadísticas del boxplot
stats <- boxplot.stats(df_hidro$perdidas)

# 2. Definir los límites que usó la función (Bigotes)
limite_inf <- stats$stats[1] # Extremo del bigote inferior
limite_sup <- stats$stats[5] # Extremo del bigote superior

# 3. Identificar y contar
outliers_menores <- df_hidro %>% filter(perdidas < limite_inf)
outliers_mayores <- df_hidro %>% filter(perdidas > limite_sup)

n_menores <- nrow(outliers_menores)
n_mayores <- nrow(outliers_mayores)
n_total   <- nrow(df_hidro)

# 4. Calcular proporciones
prop_menores <- (n_menores / n_total) * 100
prop_mayores <- (n_mayores / n_total) * 100
prop_total_outliers <- ((n_menores + n_mayores) / n_total) * 100

# 5. Mostrar resultados
cat("Cantidad de datos totales:", n_total, "\n\n")

cat("Outliers MENORES:", n_menores, "(", round(prop_menores, 2), "% )\n")
cat("Outliers MAYORES:", n_mayores, "(", round(prop_mayores, 2), "% )\n")
cat("Proporción TOTAL de outliers:", round(prop_total_outliers, 2), "%\n")

# Función de Distribución
plot(ecdf(p))

# ==============================
# DRIVER DE CLASIFIFCACIÓN 
# ==============================

class(df_hidro)
str(df_hidro)
head(df_hidro)
tail(df_hidro)

# Observemos todas los "Tipos de fenomenos que ocurren"

unique(df_hidro$tipo_fenomeno)

# Proponemos la siguiente clasificación
# Nos basamos en el principal factor destructivo de cada uno de ellos

df_hidro <- df_hidro %>%
  
  mutate(
    fenomeno_clasificado = case_when(
      tipo_fenomeno %in% c("Inundación", "Lluvias", "Lluvia", 
                           "Tormenta tropical", "Tormenta severa", 
                           "Tormenta eléctrica", "Marea de tormenta", 
                           "Mar de fondo") ~ "Agua",
      
      tipo_fenomeno %in% c("Ciclón tropical", "Fuertes Vientos", "Tornado") ~ "Viento",
      
      tipo_fenomeno %in% c("Bajas temperaturas", "Helada", 
                           "Nevada", "Granizada") ~ "Temp_Fria",
      
      tipo_fenomeno %in% c("Sequía", "Temperatura extrema") ~ "Temp_Calida",
      
      TRUE ~ "Otros"
    )
  ) %>%
  
  mutate(
    estado_clasificado = case_when(
      estado %in% c("Baja California", "Baja California Sur", "Sonora", "Sinaloa",
                    "Nayarit", "Jalisco", "Colima", "Michoacán", "Guerrero",
                    "Oaxaca", "Chiapas", "Tamaulipas", "Veracruz",
                    "Tabasco", "Campeche", "Yucatán", "Quintana Roo") ~ "Con_Costa",
      TRUE ~ "Sin_Costa"
    )
  ) %>%
  
  mutate(
    ubicacion_estado = case_when(
      # Costa Pacífico
      estado %in% c("Baja California", "Baja California Sur", "Sonora", "Sinaloa",
                    "Nayarit", "Jalisco", "Colima", "Michoacán", "Michoacan", "Guerrero",
                    "Oaxaca", "Chiapas") ~ "Pacifico",
      
      # Costa Golfo / Caribe
      estado %in% c("Tamaulipas", "Veracruz", "Tabasco", 
                    "Campeche", "Yucatán", "Quintana Roo") ~ "Golfo",
      
      # Sin Costa Norte
      estado %in% c("Chihuahua", "Coahuila", "Nuevo León", "Nuevo Leon", "Durango",
                    "Zacatecas", "San Luis Potosí", "Aguascalientes") ~ "Norte",
      
      # Sin Costa Sur/Centro
      estado %in% c("Ciudad de México", "Estado de México", "Puebla", "Tlaxcala",
                    "Hidalgo", "Morelos", "Querétaro",
                    "Guanajuato", "México") ~ "Sur",
    )
  ) %>%
  
  relocate(fenomeno_clasificado, .after = tipo_fenomeno) %>% 
  relocate(estado_clasificado, .after = estado) %>% 
  relocate(ubicacion_estado, .after = estado_clasificado)

# Guarda la base de datos limpia
write_xlsx(df_hidro, "data/df_limpia.xlsx")

# ==============================
# AJUSTE INFLACIONARIO
# ==============================

# Factores de inflación anual (Diciembre a Diciembre)
# Nota: Se incluye 2024 (4.21%) para valuar a cierre de ese año.
inflacion_anual <- c(
  "2001" = 4.40, "2002" = 5.70, "2003" = 3.98, "2004" = 5.19,
  "2005" = 3.33, "2006" = 4.05, "2007" = 3.76, "2008" = 6.53,
  "2009" = 3.57, "2010" = 4.40, "2011" = 3.82, "2012" = 3.57,
  "2013" = 3.97, "2014" = 4.08, "2015" = 2.13, "2016" = 3.36,
  "2017" = 6.77, "2018" = 4.83, "2019" = 2.83, "2020" = 3.15,
  "2021" = 7.36, "2022" = 7.82, "2023" = 4.66, "2024" = 4.21
)

# Convertir tasas a factores (1 + r)
factores_anuales <- 1 + (inflacion_anual / 100)

# Función para calcular el factor acumulado desde el año X hasta DIC-2024
# Si el dinero es de DIC-2000, debe multiplicarse por inflacion 2001...2024
obtener_factor_acumulado <- function(anio_origen) {
  if (anio_origen >= 2024) return(1.0)
  
  # Los años de inflación que le afectan son desde el siguiente hasta 2024
  anios_a_aplicar <- as.character((anio_origen + 1):2024)
  return(prod(factores_anuales[anios_a_aplicar]))
}

# Crear el dataframe de factores para el join
df_factores <- data.frame(
  anio = 2000:2024,
  factor_inflacion = sapply(2000:2024, obtener_factor_acumulado)
)

# Join + ajuste
df_hidro <- df_hidro %>%
  left_join(df_factores, by = "anio") %>%
  mutate(
    perdidas_actualizadas = perdidas * factor_inflacion
  ) %>%
  dplyr::select(-factor_inflacion)

# ==============================
# TRANSFORMACIÓN LN 
# ==============================

# Suavizar las pérdidas

df_hidro <- df_hidro %>%
  mutate(
    log_perdidas = log(perdidas)
  )

# ==============================
# VISUALIZACIÓN DE LA CLASIFICACIÓN POR 2 CASOS
# ==============================

# POR FENÓMENO CLASIFICADO

par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1)) 

segmentos <- unique(df_hidro$fenomeno_clasificado)

for (i in 1:4) {
  nombre_seg <- segmentos[i]
  datos_log <- df_hidro$log_perdidas[df_hidro$fenomeno_clasificado == nombre_seg]
  
  hist(datos_log, 
       breaks = nclass.Sturges(datos_log), 
       col = hcl.colors(4, "Dark 3")[i], 
       border = "white",
       main = paste("Tipo Fenómeno:", nombre_seg),
       xlab = "Log-Pérdidas (Suavizadas)", 
       ylab = "Frecuencia",
       cex.main = 1)
  
  abline(v = mean(datos_log, na.rm = TRUE), col = "red", lwd = 2, lty = 2)   # Media 
  abline(v = median(datos_log, na.rm = TRUE), col = "black", lwd = 2)        # Mediana 
}

# Título 
mtext("Comparativa de Severidad Suavizada por Fenómeno Clasificado", 
      outer = TRUE, cex = 1.2, font = 2)

par(mfrow = c(1, 1))

# POR UBICACIÓN

par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1), oma = c(0, 0, 2, 0))

regiones_orden <- c("Norte", "Sur", "Pacifico", "Golfo")

for (i in 1:4) {
  reg <- regiones_orden[i]
  datos_log <- df_hidro$log_perdidas[df_hidro$ubicacion_estado == reg]
  
  hist(datos_log, 
       breaks = nclass.Sturges(datos_log), 
       col = hcl.colors(4, "Temps")[i], 
       border = "white",
       main = paste("Zona:", reg),
       xlab = "Log-Pérdidas", 
       ylab = "Frecuencia",
       cex.main = 1.1)
  
  abline(v = mean(datos_log), col = "red", lwd = 2, lty = 2) # Media
  abline(v = median(datos_log), col = "black", lwd = 2) # Mediana 
}

# Título 
mtext("Comparativa de Severidad Suavizada por Región Geográfica", 
      outer = TRUE, cex = 1.2, font = 2)

par(mfrow = c(1, 1))

# ==============================
# SEGMENTACIÓN DE CLASIFICACIÓN POR FENÓMENO CLASIFICADO
# ==============================

df_hidrotemp_calida  <- df_hidro %>% filter(fenomeno_clasificado == "Temp_Calida")
df_hidrotemp_fria     <- df_hidro %>% filter(fenomeno_clasificado == "Temp_Fria")
df_hidroagua <- df_hidro %>% filter(fenomeno_clasificado == "Agua")
df_hidroviento    <- df_hidro %>% filter(fenomeno_clasificado == "Viento")

table(df_hidro$tipo_fenomeno)

# Tamaño
cat("Registros por tabla:\n",
    "Temp_Calida:   ", nrow(df_hidrotemp_calida), "\n",
    "Temp_Fria:     ", nrow(df_hidrotemp_fria), "\n",
    "Agua:", nrow(df_hidroagua), "\n",
    "Viento:   ", nrow(df_hidroviento))

# ==============================
#             Viento
# ==============================

# Análsis Descriptivo 

p_viento <- df_hidroviento$perdidas
logp_viento <- df_hidroviento$log_perdidas

# Estadísticos 
resumen <- data.frame(
  n = length(p_viento),
  Media = mean(p_viento),
  Mediana = median(p_viento),
  Desv_Est = sd(p_viento),
  Coef_Var = sd(p_viento) / mean(p_viento),
  Min = min(p_viento),
  Max = max(p_viento)
)

# Estadísticos de la Variable Suavizada

Cuartiles = quantile(logp_viento, c(0.25, 0.50, 0.75))

resumen_log <- data.frame(
  m_p = mean(logp_viento),
  s_p = sd(logp_viento),
  Rango_Muestral = max(logp_viento) - min(logp_viento),
  Varianza = var(logp_viento),
  Asimetria = skewness(logp_viento),
  Curtosis = kurtosis(logp_viento)
)

# Intervalos de Confianza
intervalos <- data.frame(
  Nivel = c("68%", "95%", "99%"),
  Inferior = c(m_p - s_p, m_p - s_p * 2, m_p - s_p * 3),
  Superior = c(m_p + s_p, m_p + s_p * 2, m_p + s_p * 3)
)

# Frecuencia de los tipos de fenómenos
tabla_frecuencias <- table(df_hidroviento$ubicacion_estado)

cat("REPORTE VIENTO")
print(resumen)
cat("Pérdidas suavizadas")
print(resumen_log)
print(Cuartiles)
cat("Intervalos de Confianza Muestrales")
print(intervalos)
cat("Distribución de Fenómenos")
print(tabla_frecuencias)

# Análisis gráfico

hist(logp_viento, 
     breaks = nclass.Sturges(logp_viento), 
     col = hcl.colors(6, "Zissou 1")[3], 
     border = "white",
     main = "Severidad Viento",
     xlab = "Log-Pérdidas (Escala Logarítmica)", 
     ylab = "Frecuencia de Eventos",
     cex.main = 1.2,
     font.main = 2)

abline(v = mean(logp_viento), col = "red", lwd = 2, lty = 2)   # Media (Punteada Roja)
abline(v = median(logp_viento), col = "black", lwd = 2)        # Mediana (Sólida Negra)

# Notamos que la 












df_ciclon <- df_hidro %>% filter(tipo_fenomeno == "Ciclón tropical")
# 1. Obtener las estadísticas del boxplot
stats <- boxplot.stats(df_ciclon$log_perdidas)

# 2. Definir los límites que usó la función (Bigotes)
limite_inf <- stats$stats[1] # Extremo del bigote inferior
limite_sup <- stats$stats[5] # Extremo del bigote superior

# 3. Identificar y contar
outliers_menores <- df_ciclon %>% filter(log_perdidas < limite_inf)
outliers_mayores <- df_ciclon %>% filter(log_perdidas > limite_sup)

n_menores <- nrow(outliers_menores)
n_mayores <- nrow(outliers_mayores)
n_total   <- nrow(df_ciclon)

# 4. Calcular proporciones
prop_menores <- (n_menores / n_total) * 100
prop_mayores <- (n_mayores / n_total) * 100
prop_total_outliers <- ((n_menores + n_mayores) / n_total) * 100

# 5. Mostrar resultados
cat("Cantidad de datos totales:", n_total, "\n\n")

cat("Outliers MENORES:", n_menores, "(", round(prop_menores, 2), "% )\n")
cat("Outliers MAYORES:", n_mayores, "(", round(prop_mayores, 2), "% )\n")
cat("Proporción TOTAL de outliers:", round(prop_total_outliers, 2), "%\n")}



# 1. Identificamos el umbral (el valor del 5to registro más pequeño)
umbral_min <- df_hidro %>%
  filter(tipo_fenomeno == "Ciclón tropical") %>%
  arrange(log_perdidas) %>%
  slice(5) %>%
  pull(log_perdidas)

# 2. Filtramos la tabla original
# Mantenemos todo lo que NO sea Ciclón Tropical O que sea Ciclón con pérdida > umbral
df_hidro <- df_hidro %>%
  filter(!(tipo_fenomeno == "Ciclón tropical" & log_perdidas <= umbral_min))

# Verificación
cat("Registros eliminados:", nrow(df_hidro) - nrow(df_hidro_limpio))


# ==============================
#             PACIFICO
# ==============================

# Análsis Descriptivo 

p_pac <- df_hidropacifico$perdidas
logp_pac <- df_hidropacifico$log_perdidas

# Estadísticos 
resumen <- data.frame(
  n = length(p_pac),
  Media = mean(p_pac),
  Mediana = median(p_pac),
  Desv_Est = sd(p_pac),
  Coef_Var = sd(p_pac) / mean(p_pac),
  Min = min(p_pac),
  Max = max(p_pac)
)

# Estadísticos de la Variable Suavizada

Cuartiles = quantile(logp_pac, c(0.25, 0.50, 0.75))

resumen_log <- data.frame(
  m_p = mean(logp_pac),
  s_p = sd(logp_pac),
  Rango_Muestral = max(logp_pac) - min(logp_pac),
  Varianza = var(logp_pac),
  Asimetria = skewness(logp_pac),
  Curtosis = kurtosis(logp_pac)
)

# Intervalos de Confianza
intervalos <- data.frame(
  Nivel = c("68%", "95%", "99%"),
  Inferior = c(m_p - s_p, m_p - s_p * 2, m_p - s_p * 3),
  Superior = c(m_p + s_p, m_p + s_p * 2, m_p + s_p * 3)
)

# Frecuencia de los tipos de fenómenos
tabla_frecuencias <- table(df_hidrosur$fenomeno_clasificado)

cat("REPORTE PACÍFICO")
print(resumen)
cat("Pérdidas suavizadas")
print(resumen_log)
print(Cuartiles)
cat("Intervalos de Confianza Muestrales")
print(intervalos)
cat("Distribución de Fenómenos")
print(tabla_frecuencias)

# ==============================
#             GOLFO
# ==============================

# Análsis Descriptivo 

p_gol <- df_hidropacifico$perdidas
logp_gol <- df_hidropacifico$log_perdidas

# Estadísticos 
resumen <- data.frame(
  n = length(p_gol),
  Media = mean(p_gol),
  Mediana = median(p_gol),
  Desv_Est = sd(p_gol),
  Coef_Var = sd(p_gol) / mean(p_gol),
  Min = min(p_gol),
  Max = max(p_gol)
)

# Estadísticos de la Variable Suavizada

Cuartiles = quantile(logp_gol, c(0.25, 0.50, 0.75))

resumen_log <- data.frame(
  m_p = mean(logp_gol),
  s_p = sd(logp_gol),
  Rango_Muestral = max(logp_gol) - min(logp_gol),
  Varianza = var(logp_gol),
  Asimetria = skewness(logp_gol),
  Curtosis = kurtosis(logp_gol)
)

# Intervalos de Confianza
intervalos <- data.frame(
  Nivel = c("68%", "95%", "99%"),
  Inferior = c(m_p - s_p, m_p - s_p * 2, m_p - s_p * 3),
  Superior = c(m_p + s_p, m_p + s_p * 2, m_p + s_p * 3)
)

# Frecuencia de los tipos de fenómenos
tabla_frecuencias <- table(df_hidrosur$fenomeno_clasificado)

cat("REPORTE PACÍFICO")
print(resumen)
cat("Pérdidas suavizadas")
print(resumen_log)
print(Cuartiles)
cat("Intervalos de Confianza Muestrales")
print(intervalos)
cat("Distribución de Fenómenos")
print(tabla_frecuencias)






# ==============================
# 7. para cehcard despues
# ==============================



# ==============================
# 7.1 GRÁFICAS ADICIONALES
# ==============================

# 1. Configurar cuadrícula de 2x2 para tus 4 regiones
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1), oma = c(0, 0, 2, 0))

# 2. Definir el orden para que sea fácil de leer
regiones_orden <- c("Norte", "Sur", "Pacifico", "Golfo")

# 3. Ciclo de graficación
for (i in 1:4) {
  reg <- regiones_orden[i]
  
  # Filtrar datos por la columna ubicacion_estado
  datos_log <- df_hidro$log_perdidas[df_hidro$ubicacion_estado == reg]
  datos_log <- datos_log[!is.na(datos_log)] # Limpieza de NAs
  
  # Histograma en R Base
  hist(datos_log, 
       breaks = nclass.Sturges(datos_log), 
       col = hcl.colors(4, "Temps")[i], 
       border = "white",
       main = paste("Zona:", reg),
       xlab = "Log-Pérdidas", 
       ylab = "Frecuencia",
       cex.main = 1.1)
  
  # Líneas de referencia Actuarial
  # Media en Rojo (discontinua)
  abline(v = mean(datos_log), col = "red", lwd = 2, lty = 2)
  # Mediana en Negro (sólida)
  abline(v = median(datos_log), col = "black", lwd = 2)
}

# Título del panel
mtext("Comparativa de Severidad Suavizada por Región Geográfica", 
      outer = TRUE, cex = 1.2, font = 2)

# Volver a configuración normal
par(mfrow = c(1, 1))

boxplot(df_proyecto$danos_ajustados,
        main = "Boxplot de pérdidas")

plot(ecdf(df_proyecto$danos_ajustados),
     main = "Función de distribución empírica")



# Clasificación ultra-detallada para el Norte
df_hidro <- df_hidro %>%
  mutate(segmento_detalle = case_when(
    # 1. Segmento Térmico del Norte
    ubicacion_estado == "Norte" & fenomeno_clasificado %in% c("Temp_Calida", "Temp_Fria") ~ "Norte: Térmico",
    
    # 2. Segmento de Agua del Norte
    ubicacion_estado == "Norte" & fenomeno_clasificado == "Agua" ~ "Norte: Agua",
    
    # 3. SUBCLASIFICACIÓN DE VIENTO EN EL NORTE
    ubicacion_estado == "Norte" & tipo_fenomeno == "Ciclón tropical" ~ "Norte: Viento (Ciclón)",
    ubicacion_estado == "Norte" & tipo_fenomeno == "Fuertes vientos" ~ "Norte: Viento (Fuertes)",
    ubicacion_estado == "Norte" & tipo_fenomeno == "Tornado"         ~ "Norte: Viento (Tornado)",
    
    # 4. Otros casos de viento en el Norte que no entren en los 3 anteriores
    ubicacion_estado == "Norte" & fenomeno_clasificado == "Viento" ~ "Norte: Viento (Otros)",
    
    # 5. El resto de las regiones se quedan igual
    TRUE ~ ubicacion_estado 
  ))

# 1. Configuración de 8 paneles (4 filas x 2 columnas)
par(mfrow = c(4, 2), mar = c(4, 4, 2.5, 1), oma = c(0, 0, 3, 0))

# 2. Nueva lista de segmentos detallada
orden_graficos <- c("Norte: Térmico", "Norte: Agua", 
                    "Norte: Viento (Ciclón)", "Norte: Viento (Fuertes)", 
                    "Norte: Viento (Tornado)", "Sur", 
                    "Pacifico", "Golfo")

# 3. Ciclo de graficación
for (i in 1:length(orden_graficos)) {
  nom <- orden_graficos[i]
  
  # Filtrar datos de la columna 'segmento_detalle'
  datos_log <- df_hidro$log_perdidas[df_hidro$segmento_detalle == nom]
  datos_log <- datos_log[!is.na(datos_log)] 
  
  if(length(datos_log) > 2) { # Bajamos el umbral a 2 para ver grupos pequeños como Tornados
    hist(datos_log, 
         breaks = nclass.Sturges(datos_log), 
         col = hcl.colors(8, "Zissou 1")[i], 
         border = "white",
         main = nom,
         xlab = "log_perdidas", 
         ylab = "Frecuencia",
         cex.main = 0.9) # Título un poco más pequeño para que quepa
    
    # Líneas de referencia
    abline(v = mean(datos_log), col = "red", lwd = 2, lty = 2)   # Media
    abline(v = median(datos_log), col = "black", lwd = 2, lty = 1) # Mediana
  } else {
    # Cuadro informativo si el grupo está casi vacío
    plot.new()
    text(0.5, 0.5, paste("Datos insuficientes:\n", nom), cex = 0.8)
  }
}

# Título Superior
mtext("Análisis de Severidad: Desglose Norte vs Regiones", 
      outer = TRUE, cex = 1.2, font = 2)

# Resetear lienzo
par(mfrow = c(1, 1))

# ==============================
# 8. SEGMENTACIÓN PARA GRUPOS HOMOGÉNEOS
# ==============================

# En base a los gráficos anteriores, segmentamos nuestra informacón 

# ==============================
# 8. ESTIMACIÓN DE PARÁMETROS
# ==============================
mod1 <- fitdist(p,"weibull", method = "mle")
summary(mod1)

denscomp(mod1)
cdfcomp(mod1)
qqcomp(mod1)
ppcomp(mod1)

# Validación del modelo

# ==============================
# 8. AJUSTE DE DISTRIBUCIONES
# ==============================

datos <- df_proyecto$danos_ajustados

fit_lognorm <- fitdist(datos, "lnorm")
fit_gamma   <- fitdist(datos, "gamma")
fit_pareto  <- fitdist(datos, "pareto",
                       start = list(shape = 1, scale = 1))

# ==============================
# 8.1 COMPARACIÓN GRÁFICA
# ==============================

denscomp(list(fit_lognorm, fit_gamma, fit_pareto),
         legendtext = c("Lognormal", "Gamma", "Pareto"))

cdfcomp(list(fit_lognorm, fit_gamma, fit_pareto),
        legendtext = c("Lognormal", "Gamma", "Pareto"))

qqcomp(list(fit_lognorm, fit_gamma, fit_pareto),
       legendtext = c("Lognormal", "Gamma", "Pareto"))

# ==============================
# 8.2 CRITERIOS DE INFORMACIÓN
# ==============================

AIC(fit_lognorm, fit_gamma, fit_pareto)
BIC(fit_lognorm, fit_gamma, fit_pareto)

# ==============================
# 8.3 ANÁLISIS DE COLAS
# ==============================

# Valores extremos
quantile(df_proyecto$danos_ajustados, probs = c(0.90, 0.95, 0.99))

# ==============================
# 9. PRUEBAS DE BONDAD DE AJUSTE
# ==============================

gofstat(list(fit_lognorm, fit_gamma, fit_pareto))

# ==============================
# 10. ESPERANZA POR MODELO
# ==============================

# Lognormal
mu <- fit_lognorm$estimate["meanlog"]
sigma <- fit_lognorm$estimate["sdlog"]
EX_lognorm <- exp(mu + sigma^2/2)

# Gamma
alpha <- fit_gamma$estimate["shape"]
beta <- fit_gamma$estimate["rate"]
EX_gamma <- alpha / beta

# Pareto
shape <- fit_pareto$estimate["shape"]
scale <- fit_pareto$estimate["scale"]

if(shape > 1){
  EX_pareto <- scale / (shape - 1)
} else {
  EX_pareto <- NA
}

EX_lognorm
EX_gamma
EX_pareto

cat("Esperanza Lognormal:", EX_lognorm, "\n")
cat("Esperanza Gamma:", EX_gamma, "\n")
cat("Esperanza Pareto:", EX_pareto, "\n")

# ==============================
# 11. MODELOS POR GRUPOS
# ==============================

# Con Costa
datos_costa <- df_proyecto %>%
  filter(edo_ajustado == "Con Costa")

fit_costa <- fitdist(datos_costa$danos_ajustados, "lnorm")

# Sin Costa
datos_sincosta <- df_proyecto %>%
  filter(edo_ajustado == "Sin Costa")

fit_sincosta <- fitdist(datos_sincosta$danos_ajustados, "lnorm")

# Ciclones
datos_ciclones <- df_proyecto %>%
  filter(fenomeno_ajustado == "Ciclones")

fit_ciclones <- fitdist(datos_ciclones$danos_ajustados, "lnorm")

# Inundaciones
datos_inundaciones <- df_proyecto %>%
  filter(fenomeno_ajustado == "Inundaciones")

fit_inundaciones <- fitdist(datos_inundaciones$danos_ajustados, "lnorm")

# ==============================
# 12. COSTO PROMEDIO ESPERADO
# ==============================

mu <- fit_lognorm$estimate["meanlog"]
sigma <- fit_lognorm$estimate["sdlog"]

EX <- exp(mu + sigma^2/2)

EX
cat("Costo promedio esperado por evento:", EX, "millones de pesos\n")

# ==============================
# 13. VERIFICACIÓN DEL MODELO
# ==============================

media_empirica <- mean(df_proyecto$danos_ajustados)

media_empirica
EX
EX - media_empirica

# ==============================
# 14. SELECCIÓN DEL MODELO FINAL (DE FORMA "AUTOMÁTICA")
# ==============================
# Elegimos el modelo con el menor AIC automáticamente
modelos <- list(fit_lognorm, fit_gamma, fit_pareto)
nombres <- c("Lognormal", "Gamma", "Pareto")
ganador_idx <- which.min(c(fit_lognorm$aic, fit_gamma$aic, fit_pareto$aic))

cat("--- CONCLUSIÓN TÉCNICA ---\n")
cat("El modelo propuesto es:", nombres[ganador_idx], "\n")
cat("Razón: Menor valor de AIC (", modelos[[ganador_idx]]$aic, ")\n")
