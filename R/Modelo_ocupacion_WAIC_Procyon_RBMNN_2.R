## CARGAR DATOS   #####
library(dplyr)
library(tidyr)
library(stringr)
library(spoccupancy)
library(coda)

### 1. Cargar datos   #####
Procyon_data_RBMNN = read_xlsx("C:/Users/acorona/OneDrive - WWF Mexico/Datos/Fototrampeo/Analisis/Datos/Tablas de ocupacion/survey_data_Occ_Procyon.xlsx")
df <- Procyon_data_RBMNN

#################################################
# 2. CONVERTIR A FORMATO TIDY
#################################################

df_tidy <- df %>%
  pivot_longer(
    cols = -c(state, codigo_del_sitio, specie),
    names_to = "variable_fecha",
    values_to = "valor"
  ) %>%
  separate(variable_fecha,
           into = c("variable", "fecha"),
           sep = "_(?=\\d{4}-\\d{2}-\\d{2})") %>%
  mutate(fecha = as.Date(fecha)) %>%
  pivot_wider(names_from = variable,
              values_from = valor)
#################################################
# 3. ASEGURAR VARIABLES NUMÉRICAS
#################################################
df_tidy <- df_tidy %>%
  mutate(
    det = as.numeric(det),
    humidity_median = as.numeric(humidity_median),
    rain_median = as.numeric(rain_median),
    rain_rate_median = as.numeric(rain_rate_median),
    solar_rad_median = as.numeric(solar_rad_median),
    temp_median = as.numeric(temp_median)
  )
#################################################
# 4. ORDENAR Y CREAR ID DE SITIO
#################################################
df_tidy <- df_tidy %>%
  arrange(codigo_del_sitio, fecha)

fechas <- sort(unique(df_tidy$fecha))
#################################################
# 5. CREAR FUNCION P MATRICES SITIO X VISITA
#################################################
make_matrix_safe <- function(var){
  
  temp <- df_tidy %>%
    select(codigo_del_sitio, fecha, all_of(var)) %>%
    pivot_wider(names_from = fecha, values_from = all_of(var))
  
  temp <- temp[, c("codigo_del_sitio", as.character(fechas))]
  
  as.matrix(temp[,-1])
}
#################################################
#  6. CREAR MATRIZ DETECCION
#################################################
y <- make_matrix_safe("det")

det_covs <- list(
  humidity = make_matrix_safe("humidity_median"),
  rain = make_matrix_safe("rain_median"),
  rain_rate = make_matrix_safe("rain_rate_median"),
  solar = make_matrix_safe("solar_rad_median"),
  temp = make_matrix_safe("temp_median")
)

#################################################
# 7. IMPUTAR NA
#################################################
det_covs <- lapply(det_covs, function(x){
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  return(x)
})


det_covs <- list(
  humidity = make_matrix_safe("humidity_median"),
  rain = make_matrix_safe("rain_median"),
  temp = make_matrix_safe("temp_median")
)

det_covs <- lapply(det_covs, function(x){
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
})
#################################################
# 8. ESCALAR COVARIABLES
#################################################
det_covs <- lapply(det_covs, scale)

sapply(det_covs, function(x) sum(is.na(x)))
dim(y)
n_sites <- nrow(y)
#################################################
# 9. CREAR COVARIABLES DE OCUPACION
#################################################
occ_covs <- data.frame(intercept = rep(1, n_sites))
set.seed(123)
#################################
#   10.  AJUSTAR MODELO FULL
######################################

model_full <- PGOcc(
  occ.formula = ~ 1,
  det.formula = ~ humidity + rain + temp,
  data = list(
    y = y,
    occ.covs = occ_covs,
    det.covs = det_covs
  ),
  n.samples = 5000,
  n.burn = 1000,
  n.thin = 5,
  n.chains = 3,
  verbose = TRUE
)
#############################
# 11. MODELO NULO
#############################
summary(model_full)

model_null <- PGOcc(
  occ.formula = ~1,
  det.formula = ~1,
  data = list(y = y, occ.covs = occ_covs, det.covs = det_covs),
  n.samples = 5000,
  n.burn = 1000,
  n.thin = 5,
  n.chains = 3
)
#############################
# 12. COMPARAR MODELOS CON WAIC
#############################
waic_full <- waicOcc(model_full)
waic_null <- waicOcc(model_null)

waic_full
waic_null

delta_waic <- waic_null - waic_full
delta_waic

######
scale_matrix_safe <- function(mat){
  m <- mean(mat, na.rm = TRUE)
  s <- sd(mat, na.rm = TRUE)
  
  if(s == 0){
    return(mat * 0)  # variable sin variación → queda en 0
  } else {
    return((mat - m) / s)
  }
}

det_covs <- lapply(det_covs, function(x){
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  scale_matrix_safe(x)
})

summary(det_covs$humidity)
sum(is.na(det_covs$humidity))
#############################
# 13. DETERMINAR IMPORTANCIA RELATIVA DE COVARIABLES
#############################
post_det <- as.matrix(model_full$alpha.samples)

importance <- apply(post_det[, c("humidity","rain","temp")], 2, function(x){
  max(mean(x > 0), mean(x < 0))
})

importance
#Interpretación:

#0.50 → efecto nulo

#0.70–0.80 → efecto moderado

#0.90 → efecto fuerte

#0.95 → efecto muy fuerte
#############################
# 14. RESUMEN ESTADISTICO
#############################
summary_stats <- t(apply(post_det[, c("humidity","rain","temp")], 2, function(x){
  c(
    mean = mean(x),
    sd = sd(x),
    l95 = quantile(x, 0.025),
    u95 = quantile(x, 0.975)
  )
}))

summary_stats
#Si el intervalo 95%:

#No cruza 0 → efecto importante

#Cruza 0 → efecto débil o incierto
#############################
# 15. INTERVALOS DE CONFIANZA
#############################
CI <- apply(post_det[, c("humidity","rain","temp")], 2, quantile, c(0.025, 0.975))
CI
## Si el intervalo cruza 0 → efecto incierto
