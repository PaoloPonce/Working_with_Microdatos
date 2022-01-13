
#### 1. import libreries ####

library(tidyverse)
library(haven)
library(sandwich)
library(lmtest)
library(sandwich)
library(stargazer)
library(normtest)
library(broom)
`%notin%` <- Negate(`%in%`)


#### 2. set directories ####

proyecto         <- getwd()
insumos_500      <- paste0(proyecto,"/01_inputs/Modulo_500/")
insumos_300      <- paste0(proyecto,"/01_inputs/Modulo_300/")
insumos_sum      <- paste0(proyecto,"/01_inputs/Modulo_SUM/")
insumos_500_inei <- paste0(proyecto,"/01_inputs/Modulo_500_inei/")

archivos_500      <- list.files(insumos_500)
archivos_300      <- grep("dta", list.files(insumos_300), value = T)
archivos_sum      <- grep("dta|DTA", list.files(insumos_sum), value = T)
archivos_500_inei <- grep("dta|DTA", list.files(insumos_500_inei), value = T)


#### 3. proccess files from inei sumarias ####

dbsum_all <- list()
for (i in 1:length(archivos_sum)) {
  x <- read_dta(paste0(insumos_sum, archivos_sum[i]))
  colnames(x) <- tolower(colnames(x))
  x <- x %>% dplyr::mutate(id_hogar = paste0(año, conglome, vivienda, hogar)) %>% dplyr::select(
    id_hogar, ld) %>% dplyr::rename(deflactor = ld)
  dbsum_all[[i]] <- x
  print(archivos_sum[i])
  rm(x)
}
dbsum_all <- data.table::rbindlist(dbsum_all)
count(distinct(dbsum_all, id_hogar))


#### 3. proccess files from inei M300 ####

db300_all <- list()
for (i in 1:length(archivos_300)) {
  x <- read_dta(paste0(insumos_300, archivos_300[i]))
  colnames(x) <- tolower(colnames(x))
  if ("p301b1" %notin% colnames(x)) {
    x <- x %>% dplyr::mutate(p301b1 = as.double(NA))
  }
  x <- x %>% dplyr::mutate(id = paste0(año, conglome, vivienda, hogar, codperso)) %>% dplyr::select(
    id, p301a1, p301b, p301c, p301d, p301b1) %>% dplyr::mutate(p301a1 = as.character(p301a1)) %>% dplyr::rename(p301a1_m3 = p301a1)
  db300_all[[i]] <- x
  print(archivos_300[i])
  print(class(x$p301b1))
  rm(x)
}
#db300_all <- data.table::rbindlist(db300_all)
db300_all <- bind_rows(db300_all)
count(distinct(db300_all, id))

#### 4. proccess files from mtpe ####

db500_all <- list()
for (arc in archivos_500) {
  x <- read_dta(paste0(insumos_500, arc))
  names(x)[1] <- "año"
  colnames(x) <- tolower(colnames(x))
  x <- x[, !duplicated(colnames(x), fromLast = TRUE)] 
    x <- x %>% dplyr::mutate(id = paste0(año, conglome, vivienda, hogar, codperso), id_hogar = paste0(año, conglome, vivienda, hogar))
  label <- paste0("M5-", str_extract(arc, "(\\d){4}"))
  x <- dplyr::left_join(x, db300_all, by = "id")
  x <- dplyr::left_join(x, dbsum_all, by = "id_hogar")
  x <- x %>% mutate(ano_grado = ifelse(is.na(p301c), p301b,p301c))
  x <- x %>% mutate(educacion = as.numeric(ifelse(
    p301a == 1, 0, ifelse(
    p301a == 2, 1, ifelse(
    p301a == 4, 7, ifelse(
    p301a == 6, 12, ifelse(
    p301a == 8, 15, ifelse(
    p301a == 10, 17, ifelse(
    p301a == 11, 17 + ano_grado, ifelse(
    p301a == 3, 1 + ano_grado, ifelse(p301a == 5, 7 + ano_grado, ifelse((p301a == 7 | p301a == 9), 12 + ano_grado, ano_grado))))))))))))
  x <- x %>% mutate(str_p505_02 = substr(str_pad(as.character(p505), 3, pad = "0"), 1, 2),
                    str_p505_01 = substr(str_pad(as.character(p505), 3, pad = "0"), 1, 1),
                    str_p301_01 = substr(str_pad(as.character(p301a1_m3), 6, pad = "0"), 1, 1),
                    str_p301_03 = substr(str_pad(as.character(p301a1_m3), 6, pad = "0"), 1, 3))
  db500_all[[label]] <- x
  print(arc)
  rm(x)
}

#### 5. manual: homogeneization ####

for (i in 1:length(db500_all)) {
  db <- db500_all[[i]]
  if ("fac500a" %notin% names(db)) {
    db <- db %>% dplyr::rename(fac500a = fac500a7)
  }
  db500_all[[i]] <- db
}


#### 6. select variables ####

df <- list()
for (i in 1:length(db500_all)){
  z <- db500_all[[i]] %>% dplyr::select(año, conglome, vivienda, hogar, codperso, dominio,
                                        p209, p207, p208a, deflactor, p513a1, p513a2,p301d, p301b1,
                                        p505, str_p505_01, str_p505_02, educacion, fac500a,
                                        p301a1_m3, str_p301_01, str_p301_03,
                                        r6,r6prin,r11,rdpto,rarea, p513t, ocu500, p301a,r2,r3) %>% dplyr::mutate(
    p505 = as.character(p505),
    año  = as.numeric(año),
  )
  z <- z %>% dplyr::mutate(
    familia_carrera = factor(dplyr::case_when(
                              str_p301_01 == "1" ~ "Educacion",
                              str_p301_01 == "2" ~ "Humanidades y Arte",
                              str_p301_01 == "3" ~ "Ciencias Sociales, Comerciales y Derecho",
                              str_p301_01 == "4" ~ "Ciencias Naturales, Exactas y de la Computación",
                              str_p301_01 == "5" ~ "Ingeniería, Industria y Construccións",
                              str_p301_01 == "6" ~ "Agropecuaria y Veterinarian",
                              (str_p301_01 == "7") & (str_p301_03 == "711") ~ "Medicina humana",
                              (str_p301_01 == "7") & (str_p301_03 != "711") ~ "Otras ciencias de la Salud",
                              str_p301_01 == "8" ~ "Servicios",
                              str_p301_01 == "9" ~ as.character(NA),
                              str_p301_01 == "0" ~ "Fuerzas Armadas y Policiales",
                              TRUE ~ as.character(NA))),
    perfiles = dplyr::case_when(
      str_p505_02 %in% c("11","12","13","14","21","22","23","24","25"
                        ,"26","27","28")                                    ~ "Cognitivo - no rutinario",
      str_p505_02 %in% c("31","32","33","34","25","36","37","38","29")      ~ "Manual - no rutinario",
      str_p505_02 %in% c("41","42","43","44","45","46","51","73")           ~ "Cognitivo - rutina",
      str_p505_02 %in% c("52","53","54","55","56","57","58","91","92"
                        ,"61","62","63","64","71","79","83","85","72"
                        ,"74","75","76","77","78","81","82","84","97"
                        ,"98","87","88","93","94","95","96","98")            ~ "Manual - rutina",
      TRUE ~ as.character(NA)),
    experiencia = ifelse((is.na(p513a1) & is.na(p513a2)), NA, replace_na(p513a1, 0) + replace_na(p513a2, 0)/12))
  z <- z %>% dplyr::group_by(familia_carrera) %>% dplyr::mutate(
    r11_p25 = quantile(p513t, probs = .25, na.rm = T),
    r11_p75 = quantile(p513t, probs = .75, na.rm = T),
    r11_p50 = quantile(p513t, probs = .5, na.rm = T),
    r6_p1 = quantile(r6, probs = .01, na.rm = T),
    r6_p99 = quantile(r6, probs = .99, na.rm = T),
  )
  z <- z %>% dplyr::mutate(
    r11e = ifelse((p513t < r11_p25), r11_p50, p513t))# %>% dplyr::filter((p513t > 35) & ((p208a > 24) & (p208a < 65)) & 
                                                                        #(rarea ==1) &  ((r6<r6_p99) & (r6>r6_p1)) & 
                                                                        #  (ocu500==1) & (p301a == 10 | p301a ==11))
  df[[i]] <- z
  rm(z)
}
df <- bind_rows(df)
levels(df$familia_carrera)
df$familia_carrera = relevel(df$familia_carrera, ref=4)

df_est <- df %>% dplyr::filter((ocu500==1) & (p301a == 10 | p301a ==11) &  ((r6<r6_p99) & (r6>r6_p1))) %>% dplyr::mutate(
  r6_h = (r6prin)/(r11e*4.33),
  r6_deflactado = r6prin*deflactor,
  r6_deflactado_h = (r6prin*deflactor)/(r11e*4.33),
  d_sexo   = ifelse(p207 == 2, 1, 0),
  d_casado = ifelse(p209 == 2, 1, 0),
  d_lima   = ifelse(rdpto == 15, 1, 0),
  d_perfil_1 = ifelse(perfiles == "Cognitivo - no rutinario", 1, 0),
  d_perfil_2 = ifelse(perfiles == "Manual - no rutinario", 1, 0),
  d_estatal  = ifelse(p301d == 1, 1, 0),
  d_casado_d_sexo = d_casado*d_sexo,
  educacion_d_sexo = educacion*d_sexo,
  d_lima_d_sexo = d_lima*d_sexo,
  experiencia_p = p208a - educacion - 7,
  experiencia_p_d_sexo = experiencia_p*d_sexo,
  experiencia_p2 = experiencia_p*experiencia_p) %>% dplyr::select(dominio, r6, r6_h, r6_deflactado, r6_deflactado_h,
                                                           educacion, familia_carrera, d_casado, d_sexo, experiencia_p, d_lima,
                                                           d_perfil_1,d_perfil_2, d_estatal, r11, r11e, r6prin,
                                                           d_casado_d_sexo, educacion_d_sexo, experiencia_p_d_sexo, d_lima_d_sexo,
                                                           experiencia_p2)
#### 7. model estimation ####

modelo <- lm(log(r6_deflactado) ~ educacion + familia_carrera + d_sexo + 
               d_casado_d_sexo + educacion_d_sexo + experiencia_p_d_sexo + d_lima_d_sexo + 
             d_perfil_1 + d_perfil_2 + d_lima + d_estatal + experiencia_p + experiencia_p2 + log(r11), data = df_est, 
             subset = !is.infinite(log(r6_deflactado_h)), na.action = na.exclude)

coeftest(modelo, vcov  = vcovHC, cluster = df_est$dominio)
df_est$prediccion <- ifelse(is.infinite(log(df_est$r6_deflactado_h)) , NA, exp(predict(modelo,df_est))*exp(((summary(modelo)$sigma)**2)/2))

hist(modelo$residuals)
#jb.norm.test(modelo$residuals)

inf_mod <- broom::tidy(coeftest(modelo, vcov  = vcovHC, cluster = df_est$dominio))
writexl::write_xlsx(inf_mod, paste0(proyecto,"/03_outputs/coeficiente_tab.xlsx"))

resumen <- df_est %>% group_by(familia_carrera) %>% dplyr::summarise(
  w_mu = mean(prediccion, na.rm = T),
  w_sig = var(prediccion, na.rm = T)^0.5) %>% dplyr::mutate(umbral = w_mu - w_sig) %>% dplyr::filter(!is.na(familia_carrera))

#### 8. merge datasets ####

uni_lic <- haven::read_dta(paste0(proyecto,"/01_inputs/licenciamiento.dta")) %>% dplyr::select(cod_mod, Estatus) %>% dplyr::distinct() %>% dplyr::mutate(
  gestion = ifelse(grepl("^16", as.character(cod_mod)), 'Publica', 'Privada'),
  cod_mod = as.character(cod_mod)
)

df <- df %>% dplyr::mutate(p301b1 = as.character(as.integer(p301b1)))

df <- df %>% dplyr::left_join(resumen, by = "familia_carrera") %>% dplyr::left_join(uni_lic, by = c("p301b1" = "cod_mod"))

table(df$Estatus, useNA = "always")


#### 9. compute stats ####

df <- df %>% dplyr::group_by(str_p505_02) %>% dplyr::mutate(
  mu = mean(educacion, na.rm = T),
  sigma = (var(educacion, na.rm = T))^0.5
)

df <- df %>% dplyr::mutate(
  sobre_educacion = ifelse(educacion >= mu + sigma, "Sobre educado", 
                    ifelse((educacion < mu + sigma) & (educacion > mu - sigma), "Educado adecuadamente", "Sub educado")),
  ocupaciones_no_profesionales = ifelse(str_p505_01 %in% c("1","2"), "Ocup. Profesionales", 
                                 ifelse(str_p505_01 %in% c("0","3","4","5","6","7","8","9"), "Ocup. No Profesionales", str_p505_01)),
  sub_pagado = ifelse((is.na(r6prin) | is.na(deflactor) | is.na(w_mu) | is.na(w_sig)), as.character(NA), 
                      ifelse((r6prin*deflactor < (w_mu - w_sig)), "Sub pagado", "Adecuadamente pagado"))
  )


df <- df %>% dplyr::mutate(
  sub_flexible = ifelse(is.na(sobre_educacion)  | is.na(ocupaciones_no_profesionales), as.character(NA),
                 ifelse(sobre_educacion == "Sobre educado" & ocupaciones_no_profesionales == "Ocup. No Profesionales", "SI", "NO")),
  sub_estricto = ifelse(is.na(sobre_educacion) | is.na(ocupaciones_no_profesionales) | is.na(sub_pagado), as.character(NA),
                 ifelse(sobre_educacion == "Sobre educado" & ocupaciones_no_profesionales == "Ocup. No Profesionales" & sub_pagado == "Sub pagado", "SI", "NO"))
)
table(df$sobre_educacion, useNA = "always")
table(df$ocupaciones_no_profesionales, useNA = "always")
table(df$sub_pagado, useNA = "always")

table(df$Estatus, useNA = "always")

#### 8. export ####

haven::write_dta(df, paste0(proyecto,"/03_outputs/resultados_19112021.dta"))
