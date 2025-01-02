library(readxl)
library(tidyverse)
library(fixest)
library(tseries)

# vytvoreni tabulky
# data <- tibble("kolo" = rep(0:7, each = 140), "podnik" = rep(c(names(obch_jmeni_akcie), "IC_all", "IC_trh1", "IC_trh2", "IC_liche", "IC_sude") , times = 8, each = 7), "cena" = rep(NA_real_, 1120), "cena_t-1" = rep(NA_real_, 1120), "objem_burza_t-1" = rep(NA_real_, 1120), "obch_jmeni_akcie" = rep(NA_real_, 1120), "HV" = rep(NA_real_, 1120), "neprodana_auta" = rep(NA_real_, 1120))

# Scraping
# Zadat vždy burzovní kolo (Výysledky ze dne "".) do fce
source("./scraping.R")
save_price(49)
save_volume(49)
rm(list = ls())

load("data/ceny.RData")
load("data/ceny_t_1.RData")
load("data/objem.RData")
load("data/data.RData")

# Vždy ve středu !!!!!!!===========
vykazy_to_data <- function(kolo, vykazy_list) {
    # Obch jmění
    vlastni_kap_list <- map(vykazy_list, ~ unlist(.[.[[1]] == "Vlastní kapitál", ][-1]))

    obch_jmeni_akcie <- map_dbl(c(vlastni_kap_list[[1]], vlastni_kap_list[[2]]), ~ as.numeric(.) * 1000 / 28e6)
    data[data$kolo == (kolo + 1) & data$podnik %in% names(obch_jmeni_akcie), "obch_jmeni_akcie_t-1"] <- rep(obch_jmeni_akcie, each = 7)

    # HV
    hv_list <- map(vykazy_list, ~ unlist(.[.[[1]] == "HV celkem po zdanění", ][-1]))
    hv <- map_dbl(c(hv_list[[1]], hv_list[[2]]), ~ as.numeric(.))
    data[data$kolo == (kolo + 1) & data$podnik %in% names(hv), "HV_t-1"] <- rep(hv, each = 7)
    return(data)
}

pridani_vykazu <- function(nove_vykazy, kolo) {
    soubory <- dir("Výkazy/", pattern = "*.xlsx", full.names = TRUE)
    data_list <- map(keep(soubory, ~. %in% nove_vykazy),
                     ~ {
                         var <- read_excel(., n_max = 90)
                         var[-1]
                     })
    vykazy_list <- map(data_list, na.omit)
    vykazy_to_data(kolo, vykazy_list)
}

# Přidání nových výkazů /  Načtení výkazů do "data" (jednou za 1 kolo / 7 obch dnu)
# Vyměnit nazev souboru vykazu a kolo
data <- pridani_vykazu(c("Výkazy/24420.xlsx", "Výkazy/24421.xlsx"), 7)

# Vždy v pondělí!!!!!=========
# Výsledky trhu / neprodaná auta
vysledky_to_data <- function(kolo, vysledky_list) {
    zasoby_list <- map(vysledky_list, ~ unlist(.[.[[1]] == "Zásoby", ][-c(1, ncol(.))]))
    zasoby_list <- map(zasoby_list, ~ .[!is.na(.)])

    neprodana_auta <- map_dbl(c(zasoby_list[[1]], zasoby_list[[2]]), ~ as.numeric(.))
    data[data$kolo == (kolo + 1) & data$podnik %in% names(neprodana_auta), "neprodana_auta_t-1"] <- rep(neprodana_auta, each = 7)

    return(data)
}

pridani_vysledku <- function(nove_vysledky, kolo) {
    soubory <- dir("Výsledky/", pattern = "*.xlsx", full.names = TRUE)
    data_list <- map(keep(soubory, ~. %in% nove_vysledky),
                     ~ {
                         var <- read_excel(., skip = 1, n_max = 10)
                         var
                     })
    vysledky_to_data(kolo, data_list)
}

# Přidání nových výsledků /  Načtení výsledků do "data" (jednou za 1 kolo / 7 obch dnu)
data <- pridani_vysledku(c("Výsledky/24399.xlsx", "Výsledky/24400.xlsx"), 7)

# ceny==========
# ulozeni cen a objemu a cena_t-1 do "data"
walk(names(ceny[-1]), ~ {
    data[data$podnik == ., "cena"] <<- ceny[[.]]
    data[data$podnik == ., "objem_burza_t-1"] <<- volume[[.]]
    data[data$podnik == ., "cena_t-1"] <<- ceny_t_1[[.]]
})


save(data, file = "data/data.RData")


# Regrese=============
rm(list = ls())
load("data/data.RData")

test_cena_11 <- data |> 
    filter(podnik == 11) |> 
    pull(cena) |> 
    diff()

adf.test(test_cena_11)
acf(test_cena_11) 
pacf(test_cena_11)


data |> 
    filter(podnik == 11) |> 
    mutate(diff_cena = c(NA, diff(cena))) |> 
    ggplot(
        aes(x = 1:56, y = diff_cena)
    ) +
    geom_line()




# Regrese podnik
model_list <- list()
podniky_vekt <- c("11", "12", "13", "14", "15", "16", "17", "21", "22", "23", "24", "25", "26", "27", "28")

for (pd in podniky_vekt) {
    model <- feols(cena ~ `cena_t-1` + `objem_burza_t-1` + `obch_jmeni_akcie_t-1` + `HV_t-1` + `neprodana_auta_t-1` + kolo, data = filter(data, podnik == pd), na.action = na.exclude)
    model_list[[pd]] <- model
}




#testovani predpokladu z ekoru

#regrese
model_n <- lm(zisk ~ obrat + inflace + hdp + nezaměstnanost, data = data_n)
stargazer(model_n, type = "text")
residuals_n <- residuals(model_n)

#prepoklady
pacf(residuals_n, lag.max = 25) #autokorelace reidui
Box.test(residuals_n) #autokorelace reidui
white(mainlm =  model_n, #homoskedasticita
      interactions = TRUE
)
breusch_pagan(model_n) #homoskedasticita
shapiro.test(residuals_n) #normalita rezidui
vif(model_n) #multikolinearita

resettest(model_n) #korektni specifikace modelu