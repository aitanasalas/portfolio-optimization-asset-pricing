###########################################################
# APT: APLICACIÓN EMPÍRICA
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
library(broom)

# =========================================================
# CÓDIGO
# =========================================================

tickers <- c("SAN.MC", "TTE.PA", "ITX.MC", "ULVR.L")

market_ticker <- "^STOXX"

oil_ticker <- "BZ=F"

exchange_ticker <- "EURUSD=X"

start_date <- "2020-09-01"
end_date   <- as.Date("2026-04-30")

###########################################################
# PARTE I: MODELO APT CON FACTORES OBSERVABLES
###########################################################

# =========================================================
# IMPORTAR DATOS BLOOMBERG: ACTIVOS E ÍNDICE
# =========================================================

bloomberg_data <- read_excel("RETO- 4 EMPRESAS MAS INDICE.xlsx") %>%
  rename(date = Dates) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  drop_na()

prices_monthly <- bloomberg_data %>%
  select(date, SAN.MC, TTE.PA, ITX.MC, ULVR.L)

head(prices_monthly)

# =========================================================
# RETORNOS MENSUALES DE LOS ACTIVOS
# =========================================================

returns_monthly <- prices_monthly %>%
  mutate(across(-date, ~ log(.x / lag(.x)))) %>%
  drop_na()

head(returns_monthly)

# =========================================================
# DATOS BLOOMBERG: MERCADO
# =========================================================

market_prices <- bloomberg_data %>%
  select(date, STOXX600) %>%
  rename(market = STOXX600) %>%
  arrange(date) %>%
  drop_na()

head(market_prices)

# =========================================================
# RETORNOS MENSUALES DEL MERCADO
# =========================================================

market_returns <- market_prices %>%
  mutate(MKT = log(market / lag(market))) %>%
  select(date, MKT) %>%
  drop_na()

head(market_returns)

# =========================================================
# DESCARGA DE DATOS: BRENT
# =========================================================

oil_prices <- tq_get(oil_ticker, from = start_date, to = end_date) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = to.monthly,
    indexAt = "lastof"
  ) %>%
  select(date, adjusted) %>%
  rename(oil = adjusted) %>%
  arrange(date) %>%
  drop_na()

# =========================================================
# RETORNOS MENSUALES DEL BRENT
# =========================================================

oil_returns <- oil_prices %>%
  mutate(OIL = log(oil / lag(oil))) %>%
  select(date, OIL) %>%
  drop_na()

head(oil_returns)

# =========================================================
# DESCARGA DE DATOS: TIPO DE CAMBIO EUR/USD
# =========================================================

fx_prices <- tq_get(exchange_ticker, from = start_date, to = end_date) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = to.monthly,
    indexAt = "lastof"
  ) %>%
  select(date, adjusted) %>%
  rename(exchange = adjusted) %>%
  arrange(date) %>%
  drop_na()

head(fx_prices)

# =========================================================
# RETORNOS MENSUALES DEL TIPO DE CAMBIO
# =========================================================

fx_returns <- fx_prices %>%
  mutate(FX = log(exchange / lag(exchange))) %>%
  select(date, FX) %>%
  drop_na()

head(fx_returns)

# =========================================================
# TASA LIBRE DE RIESGO
# =========================================================

rf_data <- read_excel(
  "RETO- Bono aleman 10 años (r0).xlsx",
  sheet = "Hoja1",
  skip = 5
) %>%
  rename(
    date  = Dates,
    yield = PX_LAST
  ) %>%
  mutate(
    date = as.Date(date),
    date = ceiling_date(date, unit = "month") - days(1),
    yield = as.numeric(yield),
    rf_monthly = (1 + yield / 100)^(1/12) - 1
  ) %>%
  filter(!is.na(date), !is.na(rf_monthly)) %>%
  select(date, rf_monthly) %>%
  distinct(date, .keep_all = TRUE)

head(rf_data)
tail(rf_data)

# =========================================================
# BASE FINAL PARA EL MODELO APT
# =========================================================

apt_data <- returns_monthly %>%
  inner_join(market_returns, by = "date") %>%
  inner_join(oil_returns, by = "date") %>%
  inner_join(fx_returns, by = "date") %>%
  inner_join(rf_data, by = "date") %>%
  arrange(date)

dim(apt_data)
head(apt_data)
tail(apt_data)

# =========================================================
# CONSTRUCCIÓN DE EXCESOS DE RETORNO
# =========================================================

apt_data <- apt_data %>%
  mutate(
    excess_SAN = SAN.MC - rf_monthly,
    excess_TTE = TTE.PA - rf_monthly,
    excess_ITX = ITX.MC - rf_monthly,
    excess_ULVR = ULVR.L - rf_monthly
  )

head(apt_data)

# =========================================================
# ESTIMACIÓN DEL MODELO APT: SANTANDER
# =========================================================

modelo_san <- lm(excess_SAN ~ MKT + OIL + FX, data = apt_data)

summary(modelo_san)

# =========================================================
# ESTIMACIÓN DEL MODELO APT: TODOS LOS ACTIVOS
# =========================================================

modelo_tte <- lm(excess_TTE ~ MKT + OIL + FX, data = apt_data)
modelo_itx <- lm(excess_ITX ~ MKT + OIL + FX, data = apt_data)
modelo_ulvr <- lm(excess_ULVR ~ MKT + OIL + FX, data = apt_data)

# =========================================================
# TABLA LIMPIA DE RESULTADOS APT
# =========================================================

resultados_apt <- bind_rows(
  tidy(modelo_san) %>% mutate(activo = "Santander"),
  tidy(modelo_tte) %>% mutate(activo = "TotalEnergies"),
  tidy(modelo_itx) %>% mutate(activo = "Inditex"),
  tidy(modelo_ulvr) %>% mutate(activo = "Unilever")
) %>%
  mutate(
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = round(p.value, 3),
    
    coef = paste0(estimate, stars)
  ) %>%
  select(activo, term, coef, std.error, p.value)

resultados_apt

# =========================================================
# COEFICIENTES DE DETERMINACIÓN
# =========================================================

ajuste_apt <- bind_rows(
  glance(modelo_san) %>% mutate(activo = "Santander"),
  glance(modelo_tte) %>% mutate(activo = "Totalenergies"),
  glance(modelo_itx) %>% mutate(activo = "Inditex"),
  glance(modelo_ulvr) %>% mutate(activo = "Unilever")
) %>%
  mutate(
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    
    r2 = paste0(round(r.squared, 3), stars),
    
    adj.r.squared = round(adj.r.squared, 3),
    sigma = round(sigma, 4),
    
    p.value = ifelse(
      p.value < 0.001,
      "<0.001",
      round(p.value, 3)
    )
  ) %>%
  select(activo, r2, adj.r.squared, sigma, p.value)

ajuste_apt

###########################################################
# PARTE II: EXTENSIÓN CON FACTORES FAMA-FRENCH EUROPE
###########################################################

# =========================================================
# IMPORTAR FACTORES FAMA-FRENCH EUROPE 
# =========================================================

ff_data <- read_excel("Factores Fama French 3.xlsx") %>%
  rename(
    date   = Date,
    MKT_RF = `Mkt-RF`
  ) %>%
  mutate(
    date = as.Date(date),
    date = ceiling_date(date, unit = "month") - days(1),
    across(c(MKT_RF, SMB, HML, RF), ~ as.numeric(.x) / 100)
  ) %>%
  select(date, MKT_RF, SMB, HML, RF) %>%
  drop_na()

head(ff_data)
tail(ff_data)

# =========================================================
# CONVERSIÓN DE RETORNOS DE ACTIVOS A USD
# =========================================================

returns_monthly_usd <- returns_monthly %>%
  inner_join(fx_returns, by = "date") %>%
  mutate(
    SAN_USD = SAN.MC + FX,
    TTE_USD = TTE.PA + FX,
    ITX_USD = ITX.MC + FX,
    ULVR_USD = ULVR.L + FX
  ) %>%
  select(date, SAN_USD, TTE_USD, ITX_USD, ULVR_USD)

head(returns_monthly_usd)

# =========================================================
# BASE FINAL DEL MODELO FAMA-FRENCH
# =========================================================

ff_model_data <- returns_monthly_usd %>%
  inner_join(ff_data, by = "date") %>%
  arrange(date) %>%
  mutate(
    excess_SAN = SAN_USD - RF,
    excess_TTE = TTE_USD - RF,
    excess_ITX = ITX_USD - RF,
    excess_ULVR = ULVR_USD - RF
  )

dim(ff_model_data)
head(ff_model_data)
tail(ff_model_data)

# =========================================================
# ESTIMACIÓN DEL MODELO FAMA-FRENCH: SANTANDER
# =========================================================

modelo_ff_san <- lm(excess_SAN ~ MKT_RF + SMB + HML, data = ff_model_data)

summary(modelo_ff_san)

# =========================================================
# ESTIMACIÓN DEL MODELO FAMA-FRENCH: TODOS LOS ACTIVOS
# =========================================================

modelo_ff_tte <- lm(excess_TTE ~ MKT_RF + SMB + HML, data = ff_model_data)

modelo_ff_itx <- lm(excess_ITX ~ MKT_RF + SMB + HML, data = ff_model_data)

modelo_ff_ulvr <- lm(excess_ULVR ~ MKT_RF + SMB + HML, data = ff_model_data)

# =========================================================
# TABLA DE RESULTADOS
# =========================================================

resultados_ff <- bind_rows(
  tidy(modelo_ff_san) %>% mutate(activo = "Santander"),
  tidy(modelo_ff_tte) %>% mutate(activo = "Totalenergies"),
  tidy(modelo_ff_itx) %>% mutate(activo = "Inditex"),
  tidy(modelo_ff_ulvr) %>% mutate(activo = "Unilever")
) %>%
  mutate(
    term = recode(
      term,
      "(Intercept)" = "Alpha"
    ),
    
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    
    p_value = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p.value, 3))
    ),
    
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    
    coef = paste0(estimate, stars)
  ) %>%
  select(activo, term, coef, std.error, p_value)

resultados_ff

# =========================================================
# COEFICIENTES DE DETERMINACIÓN
# =========================================================

ajuste_ff <- bind_rows(
  glance(modelo_ff_san) %>% mutate(activo = "Santander"),
  glance(modelo_ff_tte) %>% mutate(activo = "Totalenergies"),
  glance(modelo_ff_itx) %>% mutate(activo = "Inditex"),
  glance(modelo_ff_ulvr) %>% mutate(activo = "Unilever")
) %>%
  mutate(
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE ~ ""
    ),
    
    r2 = paste0(round(r.squared, 3), stars),
    
    adj.r.squared = round(adj.r.squared, 3),
    sigma = round(sigma, 4),
    
    p.value = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p.value, 3))
    )
  ) %>%
  select(activo, r2, adj.r.squared, sigma, p.value)

ajuste_ff

# =========================================================
# APT APLICADO AL PORTAFOLIO ÓPTIMO
# =========================================================

apt_data <- apt_data %>%
  mutate(
    portfolio_return =
      0.50 * rf_monthly +
      0.34383 * SAN.MC +
      0.15617 * TTE.PA,
    
    excess_portfolio =
      portfolio_return - rf_monthly
  )

modelo_portfolio <- lm(excess_portfolio ~ MKT + OIL + FX,
                       data = apt_data)

summary(modelo_portfolio)

