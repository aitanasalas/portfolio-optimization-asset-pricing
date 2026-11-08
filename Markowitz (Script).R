###########################################################
# MARKOWITZ + CARTERA TANGENTE + CML
# Finanzas Corporativas Avanzadas
###########################################################

# =========================================================
# LIMPIAR ESPACIO DE TRABAJO
# =========================================================

rm(list = ls())     # elimina todos los objetos del entorno
graphics.off()      # cierra gráficos abiertos
cat("\014")         # limpia la consola
# =========================================================
# INSTALAR PAQUETES (solo la primera vez)
# =========================================================
# install.packages("tidyverse")
# install.packages("tidyquant")
# install.packages("quadprog")
# install.packages("scales")
# install.packages("ggrepel")
# install.packages("writexl")
# install.packages("future")

# =========================================================
# CARGAR PAQUETES
# =========================================================

library(tidyverse)
library(tidyquant)
library(quadprog)
library(scales)
library(ggrepel)
library(writexl)
library(future)


# =========================================================
# CÓDIGO
# =========================================================
library(readxl)

tickers <- c("SAN.MC", "TTE.PA", "ITX.MC", "ULVR.L")
start_date <- "2020-09-01"
end_date <- as.Date("2026-04-30")

# Cargar el Bund alemán a 10 años desde Bloomberg (Excel)
bund_raw <- read_excel("RETO- Bono aleman 10 años (r0).xlsx") %>%
  rename(date = Dates, bund_yield = PX_LAST) %>%
  mutate(
    date = as.Date(date),
    bund_yield = bund_yield / 100,
    rf_monthly_ts = (1 + bund_yield)^(1/12) - 1
  )

# Tasa libre de riesgo mensual: media del período
rf_monthly <- mean(bund_raw$rf_monthly_ts, na.rm = TRUE)
rf_annual  <- (1 + rf_monthly)^12 - 1

cat("Tasa libre de riesgo mensual:", round(rf_monthly, 6), "\n")
cat("Tasa libre de riesgo anual:  ", round(rf_annual,  4), "\n")
# =========================================================
# DESCARGA DE DATOS
# =========================================================
# Descargamos los precios históricos de Yahoo Finance
# y los convertimos a frecuencia mensual

prices_monthly <- tq_get(tickers, from = start_date, to = end_date) %>%
  group_by(symbol) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = to.monthly,
    indexAt = "lastof"
  ) %>%
  select(symbol, date, adjusted) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>% # para cuando exportes a excel no salga en filas, sino en columnas
  arrange(date) %>%
  drop_na()

head(prices_monthly)

# =========================================================
# RETORNOS MENSUALES Y MATRICES DEL MODELO
# =========================================================

# Calculamos retornos logarítmicos mensuales

returns_monthly <- prices_monthly %>%
  mutate(across(-date, ~ log(.x / lag(.x)))) %>%
  drop_na()

# Convertimos los retornos en una matriz
# que será utilizada en el modelo de Markowitz

R <- returns_monthly %>%
  select(-date) %>%
  as.matrix()
# =========================================================
# PARÁMETROS DEL MODELO
# =========================================================
# μ : vector de retornos esperados
# Σ : matriz de covarianzas

mu <- colMeans(R)
Sigma <- cov(R)

# aseguramos que los nombres de los activos viajen
# con el vector de retornos esperados

names(mu) <- colnames(R)

mu
Sigma

# =========================================================
# FUNCIÓN: CARTERA DE MÍNIMA VARIANZA
# =========================================================

min_var_portfolio <- function(Sigma) {
  
  n <- ncol(Sigma)
  
  Dmat <- Sigma
  dvec <- rep(0, n)
  
  Amat <- cbind(rep(1, n), diag(n))
  bvec <- c(1, rep(0, n))
  
  solution <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  
  w <- solution$solution
  w[w < 1e-10] <- 0
  w <- w / sum(w)
  
  return(w)
}

# =========================================================
# CARTERA DE MÍNIMA VARIANZA
# =========================================================

# Esta cartera minimiza el riesgo total sujeto a:
# - suma de pesos = 1
# - no ventas en corto

w_mv <- min_var_portfolio(Sigma)
names(w_mv) <- colnames(R)

weights_mv <- tibble(
  Asset = names(w_mv),
  Weight = w_mv
) %>%
  arrange(desc(Weight))

weights_mv

# =========================================================
# RETORNO Y RIESGO DE LA CARTERA DE MÍNIMA VARIANZA
# =========================================================

# retorno esperado de la cartera

return_mv <- sum(w_mv * mu)

# varianza del portafolio

variance_mv <- as.numeric(t(w_mv) %*% Sigma %*% w_mv)

# desviación estándar (riesgo)

sd_mv <- sqrt(variance_mv)

return_mv
variance_mv
sd_mv


# =========================================================
# GRÁFICO 1: PESOS DE LA CARTERA DE MÍNIMA VARIANZA
# =========================================================

ggplot(weights_mv, aes(x = reorder(Asset, Weight), y = Weight)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Pesos de la cartera de mínima varianza",
    x = "",
    y = "Peso"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# =========================================================
# FUNCIÓN: FRONTERA EFICIENTE CON PESOS
# =========================================================

efficient_frontier_weights <- function(mu, Sigma, n_points = 300) {
  
  n <- length(mu)
  
  asset_names <- names(mu)
  
  if (is.null(asset_names) || any(asset_names == "")) {
    asset_names <- colnames(Sigma)
  }
  
  if (is.null(asset_names) || any(asset_names == "")) {
    asset_names <- paste0("Asset", 1:n)
  }
  
  target_returns <- seq(min(mu), max(mu), length.out = n_points)
  
  frontier <- map_dfr(target_returns, function(target) {
    
    Dmat <- Sigma
    dvec <- rep(0, n)
    
    # Restricciones:
    # sum(w) = 1
    # mu'w = target
    # w >= 0
    Amat <- cbind(rep(1, n), mu, diag(n))
    bvec <- c(1, target, rep(0, n))
    
    solution <- tryCatch(
      solve.QP(Dmat, dvec, Amat, bvec, meq = 2),
      error = function(e) NULL
    )
    
    if (is.null(solution)) return(NULL)
    
    w <- solution$solution
    w[w < 1e-10] <- 0
    
    if (sum(w) > 0) {
      w <- w / sum(w)
    }
    
    out <- tibble(
      return = sum(w * mu),
      risk = sqrt(as.numeric(t(w) %*% Sigma %*% w))
    )
    
    weights_tbl <- as_tibble_row(
      setNames(as.list(w), paste0("w_", asset_names))
    )
    
    bind_cols(out, weights_tbl)
  })
  
  return(frontier)
}

# =========================================================
# ESTIMACIÓN DE LA FRONTERA
# =========================================================

# Calculamos múltiples carteras eficientes
# variando el retorno objetivo

frontier_full <- efficient_frontier_weights(mu, Sigma, n_points = 300)

# =========================================================
# FILTRAR LA PARTE EFICIENTE
# =========================================================

# Nos quedamos solo con la parte eficiente de la frontera
# (retornos superiores a la cartera de mínima varianza)

frontier_efficient <- frontier_full %>%
  filter(return >= return_mv) %>%
  arrange(risk) %>%
  distinct(risk, .keep_all = TRUE)

frontier_efficient

# =========================================================
# ACTIVOS INDIVIDUALES
# =========================================================

assets_points <- tibble(
  Asset = names(mu),
  return = mu,
  risk = sqrt(diag(Sigma))
)

assets_points

# =========================================================
# PUNTO MINVAR
# =========================================================

minvar_label <- tibble(
  Asset = "MinVar",
  risk = sd_mv,
  return = return_mv
)

# =========================================================
# TEMA GRÁFICO COMÚN
# =========================================================

theme_markowitz <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.35, color = "grey85")
  )

# =========================================================
# GRÁFICO 3: VISTA GENERAL DE LA FRONTERA
# =========================================================

grafico_general <- ggplot() +
  
  geom_line(data = frontier_efficient,
            aes(x = risk, y = return),
            color = "steelblue4",
            linewidth = 1.35) +
  
  geom_point(data = assets_points,
             aes(x = risk, y = return),
             color = "black",
             size = 3) +
  
  geom_text_repel(data = assets_points,
                  aes(x = risk, y = return, label = Asset),
                  size = 4.3,
                  box.padding = 0.45,
                  point.padding = 0.35,
                  segment.color = "grey55",
                  segment.linewidth = 0.35,
                  max.overlaps = Inf) +
  
  geom_point(data = minvar_label,
             aes(x = risk, y = return),
             color = "red3",
             size = 4) +
  
  geom_text_repel(data = minvar_label,
                  aes(x = risk, y = return, label = Asset),
                  color = "red3",
                  fontface = "bold",
                  size = 4.3,
                  box.padding = 0.55,
                  point.padding = 0.4,
                  segment.color = "red3",
                  segment.linewidth = 0.35,
                  max.overlaps = Inf,
                  nudge_y = 0.00015) +
  
  labs(
    title = "Frontera eficiente y activos individuales",
    x = "Riesgo (desviación estándar)",
    y = "Retorno esperado"
  ) +
  
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  
  theme_markowitz

grafico_general

# =========================================================
# ZOOM DEL GRÁFICO DE LA FRONTERA
# =========================================================

x_range_zoom <- range(frontier_efficient$risk)
y_range_zoom <- range(c(frontier_efficient$return, minvar_label$return))

x_pad_zoom <- diff(x_range_zoom) * 0.18
y_pad_zoom <- diff(y_range_zoom) * 0.18

if (x_pad_zoom == 0) x_pad_zoom <- 0.005
if (y_pad_zoom == 0) y_pad_zoom <- 0.005

x_min_zoom <- x_range_zoom[1] - x_pad_zoom
x_max_zoom <- x_range_zoom[2] + x_pad_zoom
y_min_zoom <- y_range_zoom[1] - y_pad_zoom
y_max_zoom <- y_range_zoom[2] + y_pad_zoom

assets_points_zoom <- assets_points %>%
  filter(risk >= x_min_zoom,
         risk <= x_max_zoom,
         return >= y_min_zoom,
         return <= y_max_zoom)

# =========================================================
# GRÁFICO 4: ZOOM DE LA FRONTERA
# =========================================================

grafico_zoom <- ggplot() +
  
  geom_line(data = frontier_efficient,
            aes(x = risk, y = return),
            color = "steelblue4",
            linewidth = 1.5) +
  
  geom_point(data = assets_points_zoom,
             aes(x = risk, y = return),
             color = "black",
             size = 3.2) +
  
  geom_text_repel(data = assets_points_zoom,
                  aes(x = risk, y = return, label = Asset),
                  size = 4.3,
                  box.padding = 0.45,
                  point.padding = 0.35,
                  segment.color = "grey55",
                  segment.linewidth = 0.35,
                  max.overlaps = Inf) +
  
  geom_point(data = minvar_label,
             aes(x = risk, y = return),
             color = "red3",
             size = 4.2) +
  
  geom_text_repel(data = minvar_label,
                  aes(x = risk, y = return, label = Asset),
                  color = "red3",
                  fontface = "bold",
                  size = 4.3,
                  box.padding = 0.55,
                  point.padding = 0.4,
                  segment.color = "red3",
                  segment.linewidth = 0.35,
                  max.overlaps = Inf,
                  nudge_y = 0.00015) +
  
  coord_cartesian(xlim = c(x_min_zoom, x_max_zoom),
                  ylim = c(y_min_zoom, y_max_zoom)) +
  
  labs(
    title = "Zoom de la frontera eficiente",
    x = "Riesgo (desviación estándar)",
    y = "Retorno esperado"
  ) +
  
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  
  theme_markowitz

grafico_zoom

# =========================================================
# CARTERA TANGENTE
# =========================================================

# Identificamos la cartera que maximiza el Sharpe ratio

frontier_sharpe <- frontier_efficient %>%
  mutate(sharpe = (return - rf_monthly) / risk)

tangency_point <- frontier_sharpe %>%
  filter(sharpe == max(sharpe, na.rm = TRUE)) %>%
  slice(1)

tangency_point

# =========================================================
# RETORNO, RIESGO Y SHARPE DE LA CARTERA TANGENTE
# =========================================================

return_tg <- tangency_point$return
risk_tg   <- tangency_point$risk
sharpe_tg <- tangency_point$sharpe

return_tg
risk_tg
sharpe_tg

# =========================================================
# PESOS DE LA CARTERA TANGENTE
# =========================================================

weight_cols <- grep("^w_", names(tangency_point), value = TRUE)

weights_tg <- tangency_point %>%
  select(all_of(weight_cols)) %>%
  as.numeric()

names(weights_tg) <- gsub("^w_", "", weight_cols)

weights_tg_table <- tibble(
  Asset = names(weights_tg),
  Weight = weights_tg
) %>%
  arrange(desc(Weight))

weights_tg_table

# =========================================================
# GRÁFICO 5: PESOS DE LA CARTERA TANGENTE
# =========================================================

ggplot(weights_tg_table, aes(x = reorder(Asset, Weight), y = Weight)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Pesos de la cartera tangente",
    x = "",
    y = "Peso"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# =========================================================
# CAPITAL MARKET LINE
# =========================================================

# La CML conecta el activo libre de riesgo
# con la cartera tangente

sigma_cml <- seq(0, max(frontier_efficient$risk) * 1.20, length.out = 300)

cml <- tibble(
  risk = sigma_cml,
  return = rf_monthly + sharpe_tg * sigma_cml
)

rf_point <- tibble(
  Asset = "Rf",
  risk = 0,
  return = rf_monthly
)

tangency_label <- tibble(
  Asset = "Tangency",
  risk = risk_tg,
  return = return_tg
)

# =========================================================
# GRÁFICO 6: VISTA GENERAL - FRONTERA, TANGENCIA Y CML
# =========================================================

grafico_cml_general <- ggplot() +
  
  geom_line(data = frontier_efficient,
            aes(x = risk, y = return),
            color = "steelblue4",
            linewidth = 1.35) +
  
  geom_line(data = cml,
            aes(x = risk, y = return),
            color = "darkgreen",
            linewidth = 1.2,
            linetype = "dashed") +
  
  geom_point(data = assets_points,
             aes(x = risk, y = return),
             color = "black",
             size = 3) +
  
  geom_text_repel(data = assets_points,
                  aes(x = risk, y = return, label = Asset),
                  size = 4,
                  box.padding = 0.45,
                  point.padding = 0.35,
                  segment.color = "grey50",
                  max.overlaps = Inf) +
  
  geom_point(data = minvar_label,
             aes(x = risk, y = return),
             color = "red3",
             size = 4) +
  
  geom_text_repel(data = minvar_label,
                  aes(x = risk, y = return, label = Asset),
                  color = "red3",
                  fontface = "bold",
                  size = 4,
                  box.padding = 0.5,
                  point.padding = 0.35,
                  segment.color = "red3") +
  
  geom_point(data = tangency_label,
             aes(x = risk, y = return),
             color = "darkgreen",
             size = 4) +
  
  geom_text_repel(data = tangency_label,
                  aes(x = risk, y = return, label = Asset),
                  color = "darkgreen",
                  fontface = "bold",
                  size = 4,
                  box.padding = 0.5,
                  point.padding = 0.35,
                  segment.color = "darkgreen") +
  
  geom_point(data = rf_point,
             aes(x = risk, y = return),
             color = "orange3",
             size = 4) +
  
  geom_text_repel(data = rf_point,
                  aes(x = risk, y = return, label = Asset),
                  color = "orange3",
                  fontface = "bold",
                  size = 4,
                  box.padding = 0.5,
                  point.padding = 0.35,
                  segment.color = "orange3") +
  
  labs(
    title = "Frontera eficiente, cartera tangente y CML",
    x = "Riesgo (desviación estándar)",
    y = "Retorno esperado"
  ) +
  
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  
  theme_markowitz

grafico_cml_general

# =========================================================
# ZOOM EN LA ZONA DE TANGENCIA
# =========================================================

x_range_tg <- range(c(frontier_efficient$risk, tangency_label$risk, minvar_label$risk))
y_range_tg <- range(c(frontier_efficient$return, tangency_label$return, minvar_label$return))

x_pad_tg <- diff(x_range_tg) * 0.18
y_pad_tg <- diff(y_range_tg) * 0.18

if (x_pad_tg == 0) x_pad_tg <- 0.005
if (y_pad_tg == 0) y_pad_tg <- 0.005

x_min_tg <- x_range_tg[1] - x_pad_tg
x_max_tg <- x_range_tg[2] + x_pad_tg
y_min_tg <- y_range_tg[1] - y_pad_tg
y_max_tg <- y_range_tg[2] + y_pad_tg

assets_points_tg <- assets_points %>%
  filter(risk >= x_min_tg,
         risk <= x_max_tg,
         return >= y_min_tg,
         return <= y_max_tg)

cml_zoom <- cml %>%
  filter(risk >= x_min_tg,
         risk <= x_max_tg,
         return >= y_min_tg,
         return <= y_max_tg)

# =========================================================
# GRÁFICO 7: ZOOM DE LA TANGENCIA
# =========================================================

grafico_cml_zoom <- ggplot() +
  
  geom_line(data = frontier_efficient,
            aes(x = risk, y = return),
            color = "steelblue4",
            linewidth = 1.4) +
  
  geom_line(data = cml_zoom,
            aes(x = risk, y = return),
            color = "darkgreen",
            linewidth = 1.2,
            linetype = "dashed") +
  
  geom_point(data = assets_points_tg,
             aes(x = risk, y = return),
             color = "black",
             size = 3.1) +
  
  geom_text_repel(data = assets_points_tg,
                  aes(x = risk, y = return, label = Asset),
                  size = 4,
                  box.padding = 0.45,
                  point.padding = 0.35,
                  segment.color = "grey50",
                  max.overlaps = Inf) +
  
  geom_point(data = minvar_label,
             aes(x = risk, y = return),
             color = "red3",
             size = 4.1) +
  
  geom_text_repel(data = minvar_label,
                  aes(x = risk, y = return, label = Asset),
                  color = "red3",
                  fontface = "bold",
                  size = 4,
                  box.padding = 0.5,
                  point.padding = 0.35,
                  segment.color = "red3") +
  
  geom_point(data = tangency_label,
             aes(x = risk, y = return),
             color = "darkgreen",
             size = 4.1) +
  
  geom_text_repel(data = tangency_label,
                  aes(x = risk, y = return, label = Asset),
                  color = "darkgreen",
                  fontface = "bold",
                  size = 4,
                  box.padding = 0.5,
                  point.padding = 0.35,
                  segment.color = "darkgreen") +
  
  coord_cartesian(xlim = c(x_min_tg, x_max_tg),
                  ylim = c(y_min_tg, y_max_tg)) +
  
  labs(
    title = "Zoom de la tangencia",
    x = "Riesgo (desviación estándar)",
    y = "Retorno esperado"
  ) +
  
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  
  theme_markowitz

grafico_cml_zoom

# =========================================================
# TABLA RESUMEN
# =========================================================

summary_table <- tibble(
  Portfolio = c("MinVar", "Tangency"),
  Expected_Return = c(return_mv, return_tg),
  Risk = c(sd_mv, risk_tg),
  Sharpe = c((return_mv - rf_monthly) / sd_mv, sharpe_tg)
)

summary_table

# =========================================================
# EXPORTACIÓN DE LOS RESULTADOS A EXCEL
# =========================================================

#Objetos para exportar a Excel

prices_export <- as.data.frame(prices_monthly)

returns_export <- as.data.frame(R)

minvar_weights_export <- as.data.frame(weights_mv)

frontier_export <- frontier_efficient

tangency_weights_export <- as.data.frame(weights_tg_table)

cml_export <- as.data.frame(cml)

individual_assets_export <- as.data.frame(assets_points)

summary_export <- as.data.frame(summary_table)

# ===============================================
# LISTA FINAL A EXPORTAR
# ===============================================

excel_export <- list(
  Monthly_Prices = prices_export,
  Monthly_Returns = returns_export,
  MinVar_Weights = minvar_weights_export,
  Efficient_Frontier = frontier_export,
  Tangency_Weights = tangency_weights_export,
  Capital_Market_Line = cml_export,
  Individual_Assets = individual_assets_export,
  Summary_Table = summary_export
)

# ===============================================
# EXPORTAR RESULTADOS A EXCEL
# ===============================================

writexl::write_xlsx(excel_export, "Markowitz_results.xlsx")
nrow(prices_monthly)

head(returns_monthly)
# Retornos medios actualizados
mu

# Matriz de covarianzas actualizada
Sigma

# Contribución marginal al riesgo de la cartera de mínima varianza
w_mv_vec <- w_mv

# Contribución marginal al riesgo
MCR <- (Sigma %*% w_mv_vec) / sd_mv

# Contribución total al riesgo
CR <- w_mv_vec * MCR

# Porcentaje de contribución
PCR <- CR / sd_mv

# Tabla resumen
risk_contribution <- tibble(
  Asset = names(w_mv_vec),
  Weight = w_mv_vec,
  Marginal_Contribution = as.numeric(MCR),
  Total_Contribution = as.numeric(CR),
  Pct_Contribution = as.numeric(PCR)
)

risk_contribution

