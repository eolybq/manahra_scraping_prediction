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
library(Metrics)
library(vtable)
library(readxl)
library(openxlsx)



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
  labs(title = "Ceny akcií v čase", x = "Obchodní kolo", y = "Cena akcie") +
  theme_minimal()



podnik17 <- data_podnik |>
  filter(podnik == 17)


podnik17 |> 
    select(-c("podnik", "kolo", `cena_t-1`, "obch_kolo")) |>
    summary() |> 
    write.xlsx(file = "summary.xlsx")
    


# graf vysoke meritko
podnik17 |>
  ggplot(aes(x = obch_kolo)) +
  geom_line(aes(y = `objem_burza_t-1`, color = "objem_burza")) +
  geom_line(aes(y = `HV_t-1`, color = "HV")) +
  geom_line(aes(y = `neprodana_auta_t-1`, color = "neprodana_auta")) +
    scale_y_continuous(labels = scales::label_number()) + 
    scale_color_manual(values = c("objem_burza" = "blue", "neprodana_auta" = "yellow", "HV" = "red")) +
  theme_minimal() +
    labs(y = NULL)

# graf nizke meritko
podnik17 |>
  ggplot(aes(x = obch_kolo)) +
  geom_line(aes(y = `obch_jmeni_akcie_t-1`, color = "obch_jmeni")) +
  geom_line(aes(y = cena, color = "cena")) +
    scale_color_manual(values = c("obch_jmeni" = "yellow", "cena" = "blue")) +
  theme_minimal() +
    labs(y = NULL)


# Stacionarita
acf(podnik17$cena)

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
    kpss_test <- kpss.test(var_data, null = "Level")
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
stationarity_summary |> 
    write.xlsx(file = "stacionarita.xlsx")





# Engle-Granger cointegration test

# Fit the regression model
coint_model <- lm(cena ~ `obch_jmeni_akcie_t-1` + `HV_t-1` + `neprodana_auta_t-1`, data = podnik17)

# Get the residuals
coint_residuals <- residuals(coint_model)

# Perform the ADF test on residuals
adf.test(coint_residuals, alternative = "stationary")



data_regrese <- tibble(
  diff_cena = diff(podnik17$cena),
  lag_diff_cena = lag(diff(podnik17$cena), 1),
  diff_obch_jmeni_akcie_t_1 = diff(podnik17$`obch_jmeni_akcie_t-1`),
  diff_HV_t_1 = diff(log(podnik17$`HV_t-1`)),
  diff_neprodana_auta_t_1 = diff(podnik17$`neprodana_auta_t-1`),
  objem_burza_t_1 = log(podnik17$`objem_burza_t-1`[-1])
)

variables_2 <- data_regrese[, c("diff_cena", "diff_obch_jmeni_akcie_t_1", "diff_HV_t_1", "diff_neprodana_auta_t_1", "objem_burza_t_1")]
stationarity_summary <- check_stationarity_summary(variables_2)
print(stationarity_summary)


# Regrese

# Rozdělení na train a test
train_size <- floor(0.85 * nrow(data_regrese))
train_data <- data_regrese[1:train_size, ]
test_data <- data_regrese[(train_size + 1):nrow(data_regrese), ]

# Model 1
model <- train_data |>
    lm(
        diff_cena ~
            diff_obch_jmeni_akcie_t_1 + 
            objem_burza_t_1 +
            diff_neprodana_auta_t_1 + 
            diff_HV_t_1, 
        data = _,
        # vcov = "iid"
    )

model <- train_data |>
    lm(
        diff_cena ~
            lag_diff_cena +
            diff_obch_jmeni_akcie_t_1 + 
            objem_burza_t_1 +
            diff_neprodana_auta_t_1 + 
            diff_HV_t_1, 
        data = _,
        # vcov = "iid"
    )

# Model 2
model <- train_data |>
    lm(
        diff_cena ~
            lag_diff_cena +
            diff_obch_jmeni_akcie_t_1 + 
            objem_burza_t_1 +
            diff_neprodana_auta_t_1 + 
            diff_HV_t_1, 
        data = _,
        # vcov = "iid"
    )



model |>
  modelsummary(stars = c("*" = .1, "**" = .05, "***" = .01), output = "regrese.xlsx")



# prepoklady

residuals <- residuals(model)
acf(residuals)
pacf(residuals)
Box.test(residuals, lag = 25, type = "Ljung-Box")
# dwtest(model, alternative = "two.sided")


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
actuals <- test_data$diff_cena

pred <- tibble(
  predictions = predictions,
  realizovane = actuals
)

pred |> 
    write.xlsx("predikce.xlsx")

pred |>
  ggplot(aes(x = 1:nrow(pred))) +
  geom_point(aes(y = predictions, color = "Predikce")) +
  geom_line(aes(y = predictions, color = "Predikce")) +
  geom_point(aes(y = realizovane, color = "Realizovaná")) +
  geom_line(aes(y = realizovane, color = "Realizovaná")) +
  labs(x = "Obchodní kolo", y = "Cena", title = "Predikce vs Realizované hodnoty") +
  scale_color_manual(values = c("Predikce" = "blue", "Realizovaná" = "yellow")) +
  theme_minimal()

mse <- mean((predictions - actuals)^2)
print(mse)
sqrt(mse)
sd(data_regrese$diff_cena)

