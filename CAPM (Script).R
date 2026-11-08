###########################################################
# CAPM: APLICACIÓN EMPÍRICA
# Finanzas Corporativas Avanzadas
###########################################################

# =========================================================
# LIMPIAR ESPACIO DE TRABAJO
# =========================================================

rm(list = ls())
graphics.off()
cat("\014")

# =========================================================
# CARGAR PAQUETES
# =========================================================

library(tidyverse)
library(tidyquant)
library(ggrepel)
library(scales)
library(readxl)
library(lubridate)

# Nota: los paquetes ya se encuentran instalados en el proyecto de Posit Cloud.
# Si estás utilizando este código en RStudio es necesario instalar los paquetes.
# Para ello, usa las siguientes líneas de código:

# install.packages("tidyverse")
# install.packages("tidyquant")
# install.packages("ggrepel")
# install.packages("scales")
# install.packages("readxl")
# install.packages("lubridate")
# Cargar datos desde Bloomberg
bloomberg_data <- read_excel("RETO- 4 EMPRESAS MAS INDICE.xlsx") %>%
  rename(date = Dates) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

prices_monthly <- bloomberg_data %>%
  select(date, SAN.MC, TTE.PA, ITX.MC, ULVR.L)

market_prices <- bloomberg_data %>%
  select(date, STOXX600) %>%
  rename(market = STOXX600)

bund_raw <- read_excel("RETO- Bono aleman 10 años (r0).xlsx") %>%
  rename(date = Dates, bund_yield = PX_LAST) %>%
  mutate(
    date = as.Date(date),
    bund_yield = bund_yield / 100,
    rf_monthly = (1 + bund_yield)^(1/12) - 1
  )

rf_data <- bund_raw %>%
  select(date, rf_monthly) %>%
  mutate(date = ceiling_date(date, unit = "month") - days(1))

# =========================================================
# CÓDIGO
# =========================================================

tickers <- c("SAN.MC", "TTE.PA", "ITX.MC", "ULVR.L")
start_date <- "2020-09-01"
end_date <- as.Date("2026-04-30")

# =========================================================
# DESCARGA DE DATOS: ACTIVOS
# =========================================================
# En esta sección descargamos los precios históricos de los
# activos seleccionados desde Yahoo Finance y los convertimos
# a frecuencia mensual.
#
# Trabajamos con precios ajustados (adjusted).
# =========================================================



head(prices_monthly)

# =========================================================
# RETORNOS MENSUALES DE LOS ACTIVOS
# =========================================================
# A partir de los precios mensuales de las acciones,
# calculamos los retornos logarítmicos mensuales.
# =========================================================

returns_monthly <- prices_monthly %>%
  mutate(across(-date, ~ log(.x / lag(.x)))) %>%
  drop_na()

head(returns_monthly)

# =========================================================
# DESCARGA DE DATOS: MERCADO
# =========================================================
# En esta sección descargamos el índice de mercado que
# utilizaremos como proxy del mercado en la aplicación
# empírica del CAPM.
# =========================================================



head(market_prices)

# =========================================================
# RETORNOS MENSUALES DEL MERCADO
# =========================================================
# A partir de los precios mensuales del índice de mercado,
# calculamos los retornos logarítmicos mensuales.
# =========================================================

market_returns <- market_prices %>%
  mutate(Rm = log(market / lag(market))) %>%
  select(date, Rm) %>%
  drop_na()

head(market_returns)

# =========================================================
# PREPARACIÓN DEL ACTIVO LIBRE DE RIESGO
# =========================================================
# En esta sección leemos el archivo que contiene la serie del
# activo libre de riesgo.
#
# La serie está expresada como un rendimiento anual en porcentaje
# y con frecuencia mensual. Ajustamos la fecha al final de cada
# mes y convertimos el rendimiento a tasa mensual en formato
# decimal.
# =========================================================



head(rf_data)

# =========================================================
# UNIÓN DE LAS BASES DE DATOS
# =========================================================
# En esta sección unimos en una sola base:
# - los retornos mensuales de los activos
# - los retornos mensuales del mercado
# - la tasa libre de riesgo mensual
# =========================================================

data_capm <- returns_monthly %>%
  left_join(market_returns, by = "date") %>%
  left_join(rf_data %>% select(date, rf_monthly), by = "date") %>%
  drop_na()

head(data_capm)

# =========================================================
# EXCESOS DE RETORNO
# =========================================================
# El CAPM explica las primas por riesgo, es decir, los retornos
# en exceso de la tasa libre de riesgo.
# =========================================================

data_capm <- data_capm %>%
  mutate(
    across(all_of(tickers), ~ .x - rf_monthly, .names = "{.col}_excess"),
    Rm_excess = Rm - rf_monthly
  )

head(data_capm)

# =========================================================
# ESTIMACIÓN DEL CAPM: UN ACTIVO
# =========================================================
# Estimamos el modelo CAPM para una acción específica mediante
# una regresión lineal entre:
# - el exceso de retorno del activo
# - el exceso de retorno del mercado
# =========================================================

model_ibe <- lm(IBE.MC_excess ~ Rm_excess, data = data_capm)

summary(model_ibe)

# =========================================================
# RESULTADOS COMPLETOS DEL CAPM
# =========================================================
# En esta sección presentamos los resultados completos del
# modelo CAPM para todos los activos.
#
# Para cada activo obtenemos:
# - alpha y beta
# - estadísticos t
# - p-values
# - R² del modelo
# - interpretación de significancia al 5%
# =========================================================

capm_summary <- map_dfr(tickers, function(asset) {
  
  formula <- as.formula(paste0(asset, "_excess ~ Rm_excess"))
  
  model <- lm(formula, data = data_capm)
  model_summary <- summary(model)
  coefs <- model_summary$coefficients
  
  tibble(
    Asset = asset,
    Alpha = coefs[1, 1],
    Alpha_tstat = coefs[1, 3],
    Alpha_pvalue = coefs[1, 4],
    Beta = coefs[2, 1],
    Beta_tstat = coefs[2, 3],
    Beta_pvalue = coefs[2, 4],
    R_squared = model_summary$r.squared
  )
}) %>%
  mutate(
    Beta_significant = if_else(Beta_pvalue < 0.05, "Sí", "No"),
    Alpha_significant = if_else(Alpha_pvalue < 0.05, "Sí", "No")
  )

capm_summary

# =========================================================
# PRUEBAS DE HIPÓTESIS UNILATERALES (BETA > 1)
# =========================================================
# En esta sección evaluamos si la beta de cada activo es
# mayor que 1.
#
# Hipótesis:
# H0: beta <= 1
# H1: beta > 1
# =========================================================

capm_one_sided_high <- map_dfr(tickers, function(asset) {
  
  formula <- as.formula(paste0(asset, "_excess ~ Rm_excess"))
  
  model <- lm(formula, data = data_capm)
  model_summary <- summary(model)
  coefs <- model_summary$coefficients
  
  beta_hat <- coefs[2, 1]
  se_beta  <- coefs[2, 2]
  t_stat   <- (beta_hat - 1) / se_beta
  df       <- model$df.residual
  p_value  <- 1 - pt(t_stat, df = df)
  
  tibble(
    Asset = asset,
    Beta = beta_hat,
    t_stat_beta_gt1 = t_stat,
    p_value_beta_gt1 = p_value,
    Beta_greater_than_1 = if_else(p_value < 0.05, "Sí", "No")
  )
})

capm_one_sided_high

# =========================================================
# PRUEBAS DE HIPÓTESIS UNILATERALES (BETA < 1)
# =========================================================
# En esta sección evaluamos si la beta de cada activo es
# menor que 1.
#
# Hipótesis:
# H0: beta >= 1
# H1: beta < 1
# =========================================================

capm_one_sided_low <- map_dfr(tickers, function(asset) {
  
  formula <- as.formula(paste0(asset, "_excess ~ Rm_excess"))
  
  model <- lm(formula, data = data_capm)
  model_summary <- summary(model)
  coefs <- model_summary$coefficients
  
  beta_hat <- coefs[2, 1]
  se_beta  <- coefs[2, 2]
  t_stat   <- (beta_hat - 1) / se_beta
  df       <- model$df.residual
  p_value  <- pt(t_stat, df = df)
  
  tibble(
    Asset = asset,
    Beta = beta_hat,
    t_stat_beta_lt1 = t_stat,
    p_value_beta_lt1 = p_value,
    Beta_less_than_1 = if_else(p_value < 0.05, "Sí", "No")
  )
})

capm_one_sided_low

# =========================================================
# PRUEBAS DE HIPÓTESIS: ALPHA
# =========================================================
# En esta sección evaluamos si el alpha de cada activo es
# significativamente distinto de cero.
#
# Hipótesis:
# H0: alpha = 0
# H1: alpha ≠ 0
#
# Este contraste permite evaluar si el CAPM explica
# completamente los retornos de los activos.
# =========================================================

capm_alpha_test <- capm_summary %>%
  select(Asset, Alpha, Alpha_tstat, Alpha_pvalue, Alpha_significant)

capm_alpha_test

# =========================================================
# REPRESENTACIÓN GRÁFICA DEL CAPM: TODOS LOS ACTIVOS
# =========================================================
# Representamos la relación entre el exceso de retorno de cada
# activo y el exceso de retorno del mercado.
# =========================================================

asset_excess_cols <- paste0(tickers, "_excess")

plot_data <- data_capm %>%
  select(date, Rm_excess, all_of(asset_excess_cols)) %>%
  pivot_longer(
    cols = all_of(asset_excess_cols),
    names_to = "Asset",
    values_to = "Asset_excess"
  ) %>%
  mutate(Asset = gsub("_excess", "", Asset))

ggplot(plot_data, aes(x = Rm_excess, y = Asset_excess)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Asset, scales = "free_y") +
  labs(
    title = "CAPM: exceso de retorno del activo vs exceso de retorno del mercado",
    x = "Exceso de retorno del mercado",
    y = "Exceso de retorno del activo"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# =========================================================
# SECURITY MARKET LINE (SML)
# =========================================================
# En esta sección representamos la Security Market Line.
#
# La SML muestra la relación entre:
# - el riesgo sistemático de cada activo (beta)
# - su retorno esperado
# =========================================================

mean_returns <- returns_monthly %>%
  summarise(across(-date, mean)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Asset",
    values_to = "Mean_Return"
  )

mean_market_return <- mean(data_capm$Rm, na.rm = TRUE)
mean_rf <- mean(data_capm$rf_monthly, na.rm = TRUE)

sml_data <- capm_summary %>%
  select(Asset, Beta) %>%
  left_join(mean_returns, by = "Asset")

beta_grid <- seq(0, max(sml_data$Beta, na.rm = TRUE) * 1.2, length.out = 200)

sml_line <- tibble(
  Beta = beta_grid,
  Expected_Return = mean_rf + beta_grid * (mean_market_return - mean_rf)
)

ggplot() +
  geom_line(
    data = sml_line,
    aes(x = Beta, y = Expected_Return),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_point(
    data = sml_data,
    aes(x = Beta, y = Mean_Return),
    size = 3
  ) +
  geom_text_repel(
    data = sml_data,
    aes(x = Beta, y = Mean_Return, label = Asset),
    size = 4,
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "grey50",
    max.overlaps = Inf
  ) +
  labs(
    title = "Security Market Line (SML)",
    x = "Beta",
    y = "Retorno medio observado"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

capm_summary <- capm_summary %>%
  mutate(
    CAPM_Expected_Return = mean_rf + Beta * (mean_market_return - mean_rf)
  )
mean_market_return

