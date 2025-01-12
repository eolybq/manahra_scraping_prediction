library(tidyverse)
library(ggfortify)
library(fixest)
library(tseries)
library(urca)

rm(list = ls())

# Příprava dat==========================
load("data/data.RData")

# vynechání prvního kola obsahujícího NA, -> zahrnuté pouze podniky a ne IC
data_podnik <- na.omit(data) |>
  arrange(podnik)

# přidání obch_kolo jako indikátor každého "podkola"
data_podnik$obch_kolo <- rep(1:49, times = 15)

# graf všech podniků
data_podnik |>
  ggplot(aes(x = obch_kolo, y = cena, color = podnik)) +
  geom_line() +
  labs(title = "Ceny akcií v čase", x = "Obchodní kolo", y = "Cena akcie")


test_cena_11 <- data_podnik |>
  filter(podnik == 11)

test_cena_11 |>
  ggplot(aes(x = obch_kolo, y = cena)) +
  geom_line() +
  geom_point()

cena_11_ts <- test_cena_11 |>
  pull(cena) |>
  ts(start = 1, end = 49, frequency = 1) |>
  diff() |>
  diff(lag = 7)

autoplot(cena_11_ts)
# TODO - vyřešit sezonnost - mozna sezonni diff nebo mozna staci normal, zkusit ty testy niz.
# WRITE - málo pozorování obtížný výběr lag do ADF testu, když = 5 stacionarni, = 6 nestacionarni -> muze indikovat jen prilis velky lag a ne nestacionaritu, protoze perioda sezonnosti je az = 7

# Sezónní testy: V R existují i testy specifické pro sezónnost, například Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test, který testuje, zda existuje sezónní nebo dlouhodobý trend v datech.
# ur.df(cena_11_ts, type = "trend", lags = 7, selectlags = "AIC") |>
# summary() # nolint: indentation_linter.
adf.test(cena_11_ts, alternative = "stationary", k = 5)

acf(cena_11_ts)
pacf(cena_11_ts)
# WRITE - ACF a PACF jen pro první diferenci vykazují odstranění trendu ale stále je zde sezonnost -> nutno provést sezónní diferenci



# TODO: mozna udělat funkci do které budu postupne to posilat cyklem, a ona vzdy odhandne model vyplyvne predpoklady a graf summary atd.... !!!!!!!!!!!!!!!!!!!!!!!
# Regrese podnik
model_list <- list()
podniky_vekt <- c("11", "12", "13", "14", "15", "16", "17", "21", "22", "23", "24", "25", "26", "27", "28")

for (pd in podniky_vekt) {
  model <- feols(cena ~ `cena_t-1` + `objem_burza_t-1` + `obch_jmeni_akcie_t-1` + `HV_t-1` + `neprodana_auta_t-1` + kolo, data = filter(data, podnik == pd), na.action = na.exclude)
  model_list[[pd]] <- model
}




# testovani predpokladu z ekoru

# regrese
model_n <- lm(zisk ~ obrat + inflace + hdp + nezaměstnanost, data = data_n)
stargazer(model_n, type = "text")
residuals_n <- residuals(model_n)

# prepoklady
pacf(residuals_n, lag.max = 25) # autokorelace reidui
Box.test(residuals_n) # autokorelace reidui
white(
  mainlm = model_n, # homoskedasticita
  interactions = TRUE
)
breusch_pagan(model_n) # homoskedasticita
shapiro.test(residuals_n) # normalita rezidui
vif(model_n) # multikolinearita

resettest(model_n) # korektni specifikace modelu
