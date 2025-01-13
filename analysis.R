library(tidyverse)
library(ggfortify)
library(fixest)
library(tseries)
library(urca)
library(forecast)
library(skedastic)
library(car)
library(lmtest)
library(modelsummary)
library(xtable)


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

# graf vysoke meritko
podnik17 |>
  ggplot(aes(x = obch_kolo)) +
  geom_line(aes(y = `objem_burza_t-1`, color = "objem_t-1")) +
  geom_line(aes(y = `HV_t-1`, color = "HV_t-1")) +
  geom_line(aes(y = `neprodana_auta_t-1`, color = "nepro_t-1"))

# graf nizke meritko
podnik17 |>
  ggplot(aes(x = obch_kolo)) +
  geom_line(aes(y = `obch_jmeni_akcie_t-1`, color = "obch_jmeni_t-1")) +
  geom_line(aes(y = cena, color = "cena"))
# WRITE: je zde vidět opravdu zajímavý (ale očekávaný #napsat že z těch pravidel(najit to presneji)) vztah mezi těmito proměnnými


# WRITE - málo pozorování obtížný výběr lag do ADF testu, když = 5 stacionarni, = 6 nestacionarni -> muze indikovat jen prilis velky lag a ne nestacionaritu, protoze perioda sezonnosti je az = 7
# Stacionarita
acf(podnik17$cena)
# WRITE: zde je vidět, že cena není stacionární, protože má vysokou autokorelaci která pomalu klesá








# Stacionarita a tabulka p hodnot testu a doporucenych diferenci
check_stationarity_summary <- function(data) {
  results <- tibble(
    Variable = character(),
    KPSS_p_value = numeric(),
    ADF_p_value = numeric(),
    PP_p_value = numeric(),
    Recommended_Diffs = integer(),
  )

  for (var in names(data)) {
    var_data <- data[[var]]
    kpss_test <- kpss.test(var_data)
    adf_test <- adf.test(var_data, alternative = "stationary")
    pp_test <- pp.test(var_data, alternative = "stationary")
    ndiffs_val <- ndiffs(var_data)

    results <- rbind(results, tibble(
      Variable = var,
      KPSS_p_value = kpss_test$p.value,
      ADF_p_value = adf_test$p.value,
      PP_p_value = pp_test$p.value,
      Recommended_Diffs = ndiffs_val,
    ))
  }

  return(results)
}


variables <- podnik17[, c("cena", "obch_jmeni_akcie_t-1", "HV_t-1", "neprodana_auta_t-1", "objem_burza_t-1")]
stationarity_summary <- check_stationarity_summary(variables)
print(stationarity_summary)


# Export do html
export_to_html <- function(data, file_path) {
  html_table <- xtable(data)
  print(html_table, type = "html", file = file_path)
}


export_to_html(stationarity_summary, "stationarity_summary.html")



# Engle-Granger cointegration test

# Fit the regression model
coint_model <- lm(cena ~ `obch_jmeni_akcie_t-1` + `HV_t-1` + `neprodana_auta_t-1` + `objem_burza_t-1`, data = podnik17)

# Get the residuals
coint_residuals <- residuals(coint_model)

# Perform the ADF test on residuals
adf.test(coint_residuals, alternative = "stationary")

# WRITE: Byla nalezena kointegrace což indikuje, že by data stály za hlubší prozkoumání pomocí VECM modelu



# Regrese

data_regrese <- tibble(
  diff_cena = diff(podnik17$cena),
  lag_diff_cena = lag(diff(podnik17$cena), 1),
  diff_obch_jmeni_akcie_t_1 = diff(podnik17$`obch_jmeni_akcie_t-1`),
  diff_HV_t_1 = diff(podnik17$`HV_t-1`),
  diff_neprodana_auta_t_1 = diff(podnik17$`neprodana_auta_t-1`),
  objem_burza_t_1 = podnik17$`objem_burza_t-1`[-1]
)

# Rozdělení na train a test
train_size <- floor(0.9 * nrow(data_regrese))
train_data <- data_regrese[1:train_size, ]
test_data <- data_regrese[(train_size + 1):nrow(data_regrese), ]

model <- train_data |>
  lm(
    diff_cena ~
      lag_diff_cena +
      diff_obch_jmeni_akcie_t_1 + #WRITE: odstraneni HV kvuli nevyznamnosti -> neprodana auta sice taky nevyznamna ale dle reset testu by jsme pri odstraneni zamitali H0
      log(objem_burza_t_1) +
      diff_neprodana_auta_t_1, #WRITE: logaritmus kvůli opravdu vysokým hodnotám vůči malým odstaních prom. kvůlu diferenci a i jednotce
    data = _,
    # vcov = "iid"
  )

model |>
  modelsummary(stars = c("*" = .1, "**" = .05, "***" = .01))




# prepoklady

residuals <- residuals(model)
acf(residuals)
pacf(residuals)
Box.test(residuals, lag = 25, type = "Ljung-Box")
dwtest(model, alternative = "two.sided")
# WRITE: dwtest stále ukazuje autokorelaci reziduí ale při pohledu na ACF a PACF je vidět, že autokorelace je nyní výrazně nižší než před regresí než při nezahrnutí cena_t-1 a také Ljung-Box test neodmítá nulovou hypotézu o neautokorelaci reziduí # nolint


white(
  mainlm = model, # homoskedasticita
  interactions = TRUE
)
breusch_pagan(model) # homoskedasticita
shapiro.test(residuals) # normalita rezidui
vif(model) # multikolinearita
cor(podnik17[, -c(1, 2, 4, 9)])
resettest(model, power = 2) # korektni specifikace modelu (linearita vztahu)


# Predikce na testovací sadě
predictions <- predict(model, newdata = test_data)
print(predictions)

actuals <- test_data$diff_cena
print(actuals)
mse <- mean((predictions - actuals)^2)
print(mse)
sqrt(mse) #WRITE: RMSE -> je poněkud vyšší než na trénovacích datech což může značit že model neumí příliš generalizovat vztahy na nová data
#WRITE: je vidět, že model má průměrnou chybu okolo 13,5 Kč




#!!!!!!!!!!!!!!!!!!ODSTRANIT KDYŽTAK ->

# ARIMA
cena_11_ts <- test_cena_11 |>
  pull(cena) |>
  ts(start = 1, end = 49, frequency = 1) |>
  log() |>
  diff()

autoplot(cena_11_ts)

auto.arima(cena_11_ts)