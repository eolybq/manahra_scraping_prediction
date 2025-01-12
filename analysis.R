library(tidyverse)
library(ggfortify)
library(fixest)
library(tseries)
library(urca)
library(seasonal)
library(forecast)
library(stargazer)
library(skedastic)
library(car)
library(lmtest)


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


podnik17 <- data_podnik |>
  filter(podnik == 17)

#graf vysoke meritko
podnik17 |>
    ggplot(aes(x = obch_kolo)) +
    geom_line(aes(y = `objem_burza_t-1`, color = "objem_t-1")) +
    geom_line(aes(y = `HV_t-1`, color = "HV_t-1")) +
    geom_line(aes(y = `neprodana_auta_t-1`, color = "nepro_t-1"))

#graf nizke meritko
podnik17 |>
    ggplot(aes(x = obch_kolo)) +
    geom_line(aes(y = `obch_jmeni_akcie_t-1`, color = "obch_jmeni_t-1")) +
    geom_line(aes(y = cena, color = "cena"))
#WRITE: je zde vidět opravdu zajímavý (ale očekávaný #napsat že z těch pravidel(najit to presneji)) vztah mezi těmito proměnnými


# WRITE - málo pozorování obtížný výběr lag do ADF testu, když = 5 stacionarni, = 6 nestacionarni -> muze indikovat jen prilis velky lag a ne nestacionaritu, protoze perioda sezonnosti je az = 7
#Stacionarita
cena_17_ts <- podnik17 |>
    pull(cena) |>
    ts(start = 1, end = 49, frequency = 1) |> 
    diff()

ndiffs(cena_17_ts)
nsdiffs(cena_17_ts)

ur.kpss(cena_17_ts) |> summary()
adf.test(cena_17_ts, alternative = "stationary")
pp.test(cena_17_ts, alternative = "stationary")

acf(cena_17_ts)
pacf(cena_17_ts)



# Regrese
# model <- lm(cena ~ `objem_burza_t-1` + `obch_jmeni_akcie_t-1` + `HV_t-1` + `neprodana_auta_t-1`, data = podnik17)
# model |>
#     stargazer(type = "text")

model_list <- podnik17 |> 
    feols(
        cena ~ sw(
            `objem_burza_t-1` + 
            `obch_jmeni_akcie_t-1` + 
            `HV_t-1` + 
            `neprodana_auta_t-1`,
            log(`objem_burza_t-1`) + 
            log(`obch_jmeni_akcie_t-1`) + 
            log(`HV_t-1`) + 
            log(`neprodana_auta_t-1`)
            ), 
        data = _, 
        vcov = "iid"
        )
model_list |>
    modelsummary(stars = c('*' = .1,'**' = .05, '***' = .01))

residuals <- residuals(model)

# prepoklady
# modely jsou v listu - muzu do predpokladu posilat ruzne modely z model_list

pacf(residuals, lag.max = 25) # autokorelace reidui
Box.test(residuals) # autokorelace reidui
white(
    mainlm = model, # homoskedasticita
    interactions = TRUE
)
breusch_pagan(model) # homoskedasticita
shapiro.test(residuals) # normalita rezidui
vif(model) # multikolinearita
cor(podnik17[, -c(1, 2, 4, 9)])
resettest(model, power = 2) # korektni specifikace modelu








#ARIMA
cena_11_ts <- test_cena_11 |>
    pull(cena) |>
    ts(start = 1, end = 49, frequency = 1) |> 
    log() |> 
    diff(lag = 8)

autoplot(cena_11_ts)

auto.arima(cena_11_ts)
# WRITE - ACF a PACF jen pro první diferenci vykazují odstranění trendu ale stále je zde sezonnost -> nutno provést sezónní diferenci







# TODO: mozna udělat funkci do které budu postupne to posilat cyklem, a ona vzdy odhandne model vyplyvne predpoklady a graf summary atd.... !!!!!!!!!!!!!!!!!!!!!!!
# Regrese podnik

# Analogicky lze použít funkci group_split() z dplyr, která rozdělí tabulku na list dílčích tabulek podle zgrupování. V takovém případě by celý kód vypadal následovně:
#     
#     oly12 %>%
#     group_by(Sport) %>% 
#     group_split() %>% 
#     map(
#         function(x) lm(Models[[1]], data = x)
#     )
# 
# Výsledná proměnná je list, který obsahuje odhadnuté modely:

model_list <- list()
podniky_vekt <- c("11", "12", "13", "14", "15", "16", "17", "21", "22", "23", "24", "25", "26", "27", "28")

for (pd in podniky_vekt) {
  model <- feols(cena ~ `cena_t-1` + `objem_burza_t-1` + `obch_jmeni_akcie_t-1` + `HV_t-1` + `neprodana_auta_t-1` + kolo, data = filter(data, podnik == pd), na.action = na.exclude)
  model_list[[pd]] <- model
}