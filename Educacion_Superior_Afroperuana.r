
#### 1. import libraries ####

library(tidyverse)
library(haven)
library(srvyr)
`%notin%` <- Negate(`%in%`)


#### 2. set directories ####

proyecto         <- getwd()
insumos_300      <- paste0(proyecto,"/01_inputs/01_MODULO_300/")
insumos_500      <- paste0(proyecto,"/01_inputs/02_MODULO_500/")

archivos_300      <- grep("dta", list.files(insumos_300), value = T)
archivos_500      <- grep("dta", list.files(insumos_500), value = T)

#### 3. proccess files from inei M300 ####

db300_all <- list()
for (i in 1:length(archivos_300)) {
  x <- read_dta(paste0(insumos_300, archivos_300[i]))
  colnames(x) <- tolower(colnames(x))
  x <- x %>% dplyr::mutate(id = paste0(año, conglome, vivienda, hogar, codperso)) %>% dplyr::select(
    id, año, conglome, vivienda, hogar, codperso, estrato, p208a, p307, p308a, factor07)
  db300_all[[i]] <- x
  print(archivos_300[i])
  rm(x)
}
db300_all <- bind_rows(db300_all)
count(distinct(db300_all, id))

#### 3. proccess files from inei M500 ####

db500_all <- list()
for (i in 1:length(archivos_500)) {
  x <- read_dta(paste0(insumos_500, archivos_500[i]))
  colnames(x) <- tolower(colnames(x))
  x <- x %>% dplyr::mutate(id = paste0(año, conglome, vivienda, hogar, codperso)) %>% dplyr::select(
    id, p558c)
  db500_all[[i]] <- x
  print(archivos_500[i])
  rm(x)
}
db500_all <- bind_rows(db500_all)
count(distinct(db500_all, id))

#### 4. merge M500 and M300 ####

db <- dplyr::left_join(db300_all, db500_all, by = "id")
table(db$p307, useNA = "always")
table(db$p308a, useNA = "always")

db <- db %>% dplyr::mutate(estudi_sup = ifelse((p307==1) & ((p308a == 4) | (p308a == 5) | (p308a == 6)), "si", "no"),
                           estudi_sup2 = ifelse((p307==1) & (p308a == 5), "si", "no"))

#### 5. export ####

haven::write_dta(db, paste0(proyecto,"/03_outputs/data_to_tab.dta"))


#### 5. survey ####

# https://stackoverflow.com/questions/51772529/proportions-by-group-with-srvyr-package
# https://federicovegetti.github.io/teaching/heidelberg_2018/lab/sst_lab_day2.html
# https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html


srs_design_srvyr <- db %>% as_survey_design(ids = 1, strata = estrato, weights = factor07)

resulta <- srs_design_srvyr %>% tidyr::drop_na() %>% dplyr::filter((p208a>=17) & (p208a<=24)) %>%
  dplyr::mutate(p558c = as_factor(p558c)) %>%
  dplyr::group_by(año, p558c, estudi_sup) %>% 
  dplyr::summarize(proportion = srvyr::survey_mean(),
            total = srvyr::survey_total())

writexl::write_xlsx(resulta, paste0(proyecto,"/03_outputs/resultados_tab.xlsx"))

