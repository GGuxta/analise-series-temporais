

# =============================================================================
# Projeto 1 - Análise de Séries Temporais
# Modelagem e Previsão da Temperatura Média Mensal de Brasília por Modelos SARIMA
#
# -----------------------------------------------------------------------------
# ÍNDICE
#   PASSO 1  - Instalação de pacotes e obtenção de metadados da estação
#   PASSO 2  - Download e leitura dos dados horários (INMET / rmet)
#   PASSO 3  - Construção da série de temperatura média mensal
#   PASSO 4  - Construção do objeto de série temporal (ts) e tratamento de NA
#   PASSO 5  - Análise exploratória (gráficos e estatísticas descritivas)
#   PASSO 6  - Decomposição sazonal (STL) e testes de estacionariedade
#   PASSO 7  - Diferenciação sazonal e identificação manual (ACF/PACF)
#   PASSO 8  - Identificação automática via auto.arima() e comparação de modelos
#   PASSO 9  - Diagnóstico completo dos resíduos
#   PASSO 10 - Validação fora da amostra
#   PASSO 11 - Reestimação com a série completa e previsão final (dez/2026)
# =============================================================================


# =============================================================================
# ---- Pacotes ----
# -----------------------------------------------------------------------------
# Todos os pacotes usados ao longo do script são carregados aqui, uma única
# vez, para facilitar a reprodução do ambiente.
# =============================================================================

# install.packages("devtools")                    # necessário para instalar o rmet
# devtools::install_github("rodrigosqrt3/rmet")   # rmet não está no CRAN "clássico"
# install.packages(c("forecast", "tseries", "ggplot2"))

library(rmet)
library(forecast)
library(tseries)
library(ggplot2)


# =============================================================================
# PASSO 1: Metadados da estação de Brasília
# -----------------------------------------------------------------------------
# Consulta aos metadados de todas as estações automáticas do Distrito Federal,
# para confirmar o código, nome e coordenadas da estação de interesse (A001).
# =============================================================================

stations <- inmet_stations(state = "DF")
stations[, c("code", "name", "latitude", "longitude", "elevation")]

# Inspeção da estrutura do objeto retornado (documentação informal)
names(stations)
str(stations)
head(stations)


# =============================================================================
# PASSO 2: Download e leitura dos dados horários
# -----------------------------------------------------------------------------
# Baixa os arquivos anuais do INMET (2000-2026) e lê os dados horários
# já filtrados para a estação de interesse (A001 - Brasília).
# =============================================================================

anos <- 2000:2026

# Baixa os ZIPs anuais (com retomada automática em caso de queda de conexão)
inmet_download(years = anos, quiet = FALSE)

# Lê e organiza os dados já filtrando pela estação A001
dados_hora <- inmet_read(
  years    = anos,
  stations = "A001"
)

str(dados_hora)
summary(dados_hora$temp_dry_c)
range(dados_hora$datetime)


# =============================================================================
# PASSO 3: Construção da série de temperatura média mensal
# -----------------------------------------------------------------------------
# Os dados brutos estão em frequência horária (dados_hora). Para o objetivo
# do projeto, precisamos da média mensal da temperatura de bulbo seco
# (temp_dry_c), que será usada como insumo para a modelagem SARIMA.
# =============================================================================

# Extrai ano e mês a partir do timestamp (datetime é POSIXct)
# -> formato "%Y" e "%m" garantem que ano/mês fiquem como texto,
#    o que facilita o agrupamento (evita problemas de tipo numérico
#    com zeros à esquerda, ex: mês "01" vs 1)
dados_hora$ano <- format(dados_hora$datetime, "%Y")
dados_hora$mes <- format(dados_hora$datetime, "%m")

# ------------------------------------------------------------
# Controle de qualidade: contar quantas observações horárias
# válidas (não-NA) existem em cada mês.
# Isso é importante porque um mês com poucas observações
# (ex: sensor fora do ar por dias) pode gerar uma média
# não representativa e distorcer a série.
# ------------------------------------------------------------
n_obs <- aggregate(
  temp_dry_c ~ ano + mes,
  data = dados_hora,
  FUN  = function(x) sum(!is.na(x))   # conta valores não-NA
)
names(n_obs)[3] <- "n_obs"  # renomeia a 3ª coluna para maior clareza

# ------------------------------------------------------------
# Cálculo da média mensal de temperatura
# na.rm = TRUE garante que os poucos NA's não impeçam o cálculo
# ------------------------------------------------------------
media_mensal <- aggregate(
  temp_dry_c ~ ano + mes,
  data = dados_hora,
  FUN  = mean,
  na.rm = TRUE
)

# Junta a média mensal com a contagem de observações válidas
mensal <- merge(media_mensal, n_obs, by = c("ano", "mes"))

# Ordena cronologicamente (ano e mês como texto ordenam corretamente
# aqui pois "01" < "02" < ... < "12")
mensal <- mensal[order(mensal$ano, mensal$mes), ]

# ------------------------------------------------------------
# Inspeção inicial da série mensal construída
# ------------------------------------------------------------
head(mensal, 15)   # confere o início da série (2000)
tail(mensal, 15)   # confere o fim da série (2025)
nrow(mensal)       # número total de meses disponíveis

# ------------------------------------------------------------
# Verificação de meses potencialmente problemáticos:
# um mês "cheio" tem em torno de 28*24 a 31*24 = 672 a 744
# observações horárias possíveis. Usamos um limiar conservador
# (< 500) para sinalizar meses com cobertura ruim.
# ------------------------------------------------------------
subset(mensal, n_obs < 500)


# =============================================================================
# PASSO 4: Construção do objeto de série temporal (ts) e tratamento de NA
# -----------------------------------------------------------------------------
# Decisões metodológicas (documentadas para o relatório):
#
# (a) A série começa em jan/2001, não em mai/2000. O primeiro mês disponível
#     (mai/2000) é parcial por conta do início de operação da estação, e não
#     fecha um ciclo anual, o que atrapalharia a decomposição sazonal (STL).
#     Logo, optamos por começar no primeiro ano civil completo.
#
# (b) mai/2004 teve apenas 85 das ~720-744 observações horárias esperadas
#     (~11% de cobertura) — muito abaixo dos demais meses "ruins" da série
#     (que ficaram entre 55% e 65%). Tratamos esse valor como não confiável,
#     marcando-o como NA e interpolando via na.interp() (pacote forecast),
#     que respeita a sazonalidade ao preencher o buraco.
# =============================================================================

# Filtra a partir de jan/2001
mensal2 <- subset(mensal, !(ano == "2000"))

# Marca mai/2004 como NA (dado não confiável)
mensal2$temp_dry_c[mensal2$ano == "2004" & mensal2$mes == "05"] <- NA

# Confirma que ficou só 1 NA e que a ordenação está correta
sum(is.na(mensal2$temp_dry_c))
nrow(mensal2)

# Constrói o objeto de série temporal mensal (frequência 12 = mensal)
serie <- ts(
  mensal2$temp_dry_c,
  start     = c(2001, 1),
  frequency = 12
)

# Interpola o único NA usando informação sazonal (forecast::na.interp)
serie <- na.interp(serie)

# Confere que não sobrou NA e visualiza a série completa
sum(is.na(serie))
serie


# =============================================================================
# PASSO 5: Análise exploratória (gráficos e estatísticas descritivas)
# =============================================================================

# --- 5.1 Gráfico da série completa -----------------------------------------
par(mfrow = c(1, 1))
plot(serie,
     main = "Temperatura Média Mensal – Brasília (A001)",
     xlab = "Ano", ylab = "Temperatura (°C)",
     col  = "steelblue", lwd = 1.5)
grid()

# --- 5.2 Boxplot por mês (sazonalidade) -------------------------------------
temp_df <- data.frame(
  temp = as.numeric(serie),
  mes  = factor(cycle(serie),
                labels = c("Jan","Fev","Mar","Abr","Mai","Jun",
                           "Jul","Ago","Set","Out","Nov","Dez"))
)

p_box <- ggplot(temp_df, aes(x = mes, y = temp, fill = mes)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Distribuição da Temperatura por Mês",
       x = "Mês", y = "Temperatura (°C)") +
  theme_bw()
print(p_box)

# --- 5.3 Subséries mensais (monthplot) --------------------------------------
# Cada painel mostra a trajetória de um mês específico ao longo dos anos,
# com a linha horizontal indicando a média daquele mês. Complementa o
# boxplot ao evidenciar a variabilidade INTERANUAL de cada mês.
monthplot(serie,
          main = "Subséries por Mês – Temperatura Média Mensal",
          xlab = "Mês", ylab = "Temperatura (°C)",
          col  = "steelblue")

# --- 5.4 Gráfico sazonal (ciclo por ano, com gradiente de cor) --------------
df_plot <- data.frame(
  ano   = floor(time(serie)),
  mes   = cycle(serie),
  valor = as.numeric(serie)
)

p_season <- ggplot(df_plot, aes(x = mes, y = valor, group = ano, color = ano)) +
  geom_line(alpha = 0.7) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_viridis_c(name = "Ano") +
  labs(
    title = "Ciclo sazonal por ano - Temperatura média mensal (Brasília)",
    x = "Mês", y = "Temperatura (°C)"
  ) +
  theme_minimal()
print(p_season)

# --- 5.5 Estatísticas descritivas gerais ------------------------------------
cat("\n--- Estatísticas Descritivas Gerais ---\n")
print(summary(serie))
cat("Desvio padrão:", round(sd(serie), 3), "\n")

# Estatísticas por mês
cat("\n--- Média por Mês ---\n")
print(round(tapply(as.numeric(serie), cycle(serie), mean), 2))

cat("\n--- Desvio-Padrão por Mês ---\n")
print(round(tapply(as.numeric(serie), cycle(serie), sd), 2))


# =============================================================================
# PASSO 6: Decomposição sazonal (STL) e testes de estacionariedade
# -----------------------------------------------------------------------------
# STL (Seasonal-Trend decomposition using Loess) separa a série em tendência,
# sazonalidade e resíduo de forma mais flexível que uma decomposição clássica
# (não assume forma paramétrica fixa para a tendência).
#
# s.window = "periodic" força o componente sazonal a se repetir de forma
# idêntica todo ano (adequado aqui, já que o boxplot não sugeriu mudança
# estrutural forte no formato sazonal).
# =============================================================================

decomp <- stl(serie, s.window = "periodic")
plot(decomp)

# Força relativa da sazonalidade e da tendência (Hyndman & Athanasopoulos)
# Fs e Ft próximos de 1 indicam componente forte; próximos de 0, fraco.
Ft <- max(0, 1 - var(decomp$time.series[, "remainder"]) /
            var(decomp$time.series[, "trend"] + decomp$time.series[, "remainder"]))
Fs <- max(0, 1 - var(decomp$time.series[, "remainder"]) /
            var(decomp$time.series[, "seasonal"] + decomp$time.series[, "remainder"]))
cat("Força da tendência (Ft):", round(Ft, 3), "\n")
cat("Força da sazonalidade (Fs):", round(Fs, 3), "\n")

# ------------------------------------------------------------
# Testes formais de estacionariedade / raiz unitária
# ------------------------------------------------------------

# ADF: H0 = série tem raiz unitária (não-estacionária)
adf.test(serie)

# KPSS: H0 = série é estacionária (nível)
# -> usamos como "contraprova" do ADF (testam hipóteses opostas)
kpss.test(serie, null = "Level")

# Teste de raiz unitária SAZONAL (existe uma raiz unitária no
# período de 12 meses que exigiria diferenciação sazonal?)
nsdiffs(serie)   # sugestão automática de nº de diferenças sazonais
ndiffs(serie)    # sugestão automática de nº de diferenças regulares

# ------------------------------------------------------------
# ACF e PACF da série original (para já começar a raciocinar
# sobre candidatos SARIMA, mesmo antes de diferenciar)
# ------------------------------------------------------------
acf(serie, lag.max = 48, main = "ACF - Série original")
pacf(serie, lag.max = 48, main = "PACF - Série original")


# =============================================================================
# PASSO 7: Diferenciação sazonal e identificação manual (ACF/PACF)
# -----------------------------------------------------------------------------
# No Passo 6, nsdiffs(serie) indicou D = 1 (uma diferenciação sazonal
# necessária) e ndiffs(serie) indicou d = 0 (sem necessidade de diferenciação
# simples). Aplicamos aqui a diferenciação sazonal:
#
#     nabla_12 Y_t = Y_t - Y_{t-12}
#
# O objetivo é remover a estrutura sazonal determinística que dominava a ACF
# em nível, de modo a poder identificar visualmente as ordens (p,q) não
# sazonais e (P,Q) sazonais na série já "limpa".
# =============================================================================

serie_diff <- diff(serie, lag = 12, differences = 1)

# Confirma que a diferenciação sazonal foi suficiente, sem necessidade
# de diferenciações adicionais (simples ou sazonais)
ndiffs(serie_diff)
nsdiffs(serie_diff)

# ------------------------------------------------------------
# Gráfico da série diferenciada
# ------------------------------------------------------------
plot(
  serie_diff,
  main = "Série diferenciada sazonalmente (D=1)",
  ylab = expression(nabla[12]*Y[t]),
  xlab = "Ano"
)
abline(h = 0, col = "red", lty = 2)

# ------------------------------------------------------------
# ACF e PACF da série diferenciada
# Usadas para identificar manualmente as ordens (p,q)(P,Q) do
# modelo SARIMA candidato.
# ------------------------------------------------------------
acf(serie_diff, lag.max = 48, main = "ACF - Série diferenciada (D=1)")
pacf(serie_diff, lag.max = 48, main = "PACF - Série diferenciada (D=1)")


# =============================================================================
# PASSO 8: Identificação automática via auto.arima() e comparação de modelos
# -----------------------------------------------------------------------------
# Busca exaustiva (sem atalhos stepwise, sem aproximação), com base no AICc,
# para comparar com o candidato identificado manualmente no Passo 7.
# =============================================================================

fit_auto <- auto.arima(
  serie,
  seasonal      = TRUE,
  stepwise      = FALSE,
  approximation = FALSE,
  trace         = TRUE   # mostra todos os modelos testados e seus AICc
)

summary(fit_auto)

# ------------------------------------------------------------
# Ajuste do modelo candidato identificado manualmente
# ------------------------------------------------------------
fit_manual <- Arima(
  serie,
  order    = c(0, 0, 1),
  seasonal = list(order = c(0, 1, 1), period = 12)
)

summary(fit_manual)

# ------------------------------------------------------------
# Comparação direta dos critérios de informação
# ------------------------------------------------------------
data.frame(
  modelo = c("auto.arima", "manual SARIMA(0,0,1)(0,1,1)[12]"),
  AIC    = c(AIC(fit_auto), AIC(fit_manual)),
  BIC    = c(BIC(fit_auto), BIC(fit_manual))
)

# ------------------------------------------------------------
# Verificação de invertibilidade do polinômio SMA(2)
# Extrai os coeficientes sazonais MA diretamente do modelo ajustado
# (mais robusto do que digitar os valores manualmente)
# ------------------------------------------------------------
coefs_sma <- fit_auto$coef[c("sma1", "sma2")]
poli_sma  <- c(1, coefs_sma)   # 1 + Theta1*B + Theta2*B^2

raizes_sma <- polyroot(poli_sma)
raizes_sma
Mod(raizes_sma)   # módulos das raízes -- invertível se todos > 1


# =============================================================================
# PASSO 9: Diagnóstico completo dos resíduos - SARIMA(1,0,1)(0,1,2)[12]
# =============================================================================

res       <- residuals(fit_auto)
ajustados <- fitted(fit_auto)

# --- 9.1 Gráfico dos resíduos ao longo do tempo + média ---------------------
plot(res, main = "Resíduos ao longo do tempo - SARIMA(1,0,1)(0,1,2)[12]",
     ylab = "Resíduo", xlab = "Ano")
abline(h = 0, col = "red", lty = 2)
cat("Média dos resíduos:", round(mean(res), 4), "\n")
cat("Desvio-padrão dos resíduos:", round(sd(res), 4), "\n")

# --- 9.2 Painel padrão: resíduos + ACF + histograma + teste Ljung-Box ------
checkresiduals(fit_auto)

# --- 9.3 Painel complementar: série + ACF + PACF ----------------------------
ggtsdisplay(res, main = "Resíduos, ACF e PACF - SARIMA(1,0,1)(0,1,2)[12]")

# --- 9.4 QQ-plot (normalidade visual) ---------------------------------------
qqnorm(res, main = "QQ-Plot dos Resíduos")
qqline(res, col = "red", lwd = 2)

# --- 9.5 Teste de normalidade formal (Shapiro-Wilk) -------------------------
shapiro.test(res)

# --- 9.6 Resíduos x Valores ajustados (heterocedasticidade / não-linearidade)
plot(as.numeric(ajustados), as.numeric(res),
     main = "Resíduos vs. Valores Ajustados",
     xlab = "Valores ajustados", ylab = "Resíduos",
     pch = 19, col = rgb(0, 0, 0.6, 0.5))
abline(h = 0, col = "red", lty = 2)
lines(lowess(as.numeric(ajustados), as.numeric(res)), col = "blue", lwd = 2)

# --- 9.7 Variância em janelas (checagem informal de homocedasticidade) -----
# Divide os resíduos em 4 blocos temporais e compara a variância de cada um
n      <- length(res)
blocos <- cut(1:n, breaks = 4, labels = paste0("Q", 1:4))
tapply(as.numeric(res), blocos, var)

# --- 9.8 Maiores resíduos absolutos (localizar anomalias, ex: 2017) --------
res_df <- data.frame(
  ano     = floor(time(res)),
  mes     = cycle(res),
  residuo = as.numeric(res)
)
res_df[order(-abs(res_df$residuo)), ][1:10, ]


# =============================================================================
# PASSO 10: Validação fora da amostra
# -----------------------------------------------------------------------------
# Particiona a série em treino e teste, reestima o modelo SOMENTE com o
# treino (mesma ordem já identificada), gera previsões para o horizonte de
# teste e compara com os valores observados.
#
# Usamos os últimos 12 meses como teste (jan/2025 a dez/2025), um horizonte
# razoável para validar a capacidade preditiva de um ciclo sazonal completo.
# =============================================================================

n_total  <- length(serie)
n_teste  <- 12
n_treino <- n_total - n_teste

# Partição treino/teste
treino <- window(serie, end = time(serie)[n_treino])
teste  <- window(serie, start = time(serie)[n_treino + 1])

# Confere as datas de corte
cat("Treino:", start(treino)[1], "-", start(treino)[2], "até",
    end(treino)[1], "-", end(treino)[2], "\n")
cat("Teste :", start(teste)[1], "-", start(teste)[2], "até",
    end(teste)[1], "-", end(teste)[2], "\n")

# ------------------------------------------------------------
# Reestima o modelo com a MESMA ordem já identificada,
# usando apenas os dados de treino
# ------------------------------------------------------------
fit_treino <- Arima(
  treino,
  order    = c(1, 0, 1),
  seasonal = list(order = c(0, 1, 2), period = 12)
)

summary(fit_treino)

# ------------------------------------------------------------
# Previsão para o horizonte de teste (12 meses),
# com intervalos de confiança de 80% e 95%
# ------------------------------------------------------------
previsao_teste <- forecast(fit_treino, h = n_teste, level = c(80, 95))

# Gráfico focado nos últimos anos, para melhor visualização da validação
autoplot(previsao_teste) +
  autolayer(teste, series = "Observado", color = "red") +
  coord_cartesian(xlim = c(2022, 2026)) +
  labs(
    title    = "Validação fora da amostra - SARIMA(1,0,1)(0,1,2)[12]",
    subtitle = "Zoom: 2022-2025 (treino recente + teste)",
    x = "Ano", y = "Temperatura (°C)"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# Métricas de acurácia (treino vs. teste)
# ------------------------------------------------------------
acuracia <- accuracy(previsao_teste, teste)
print(acuracia)

# ------------------------------------------------------------
# Indicador U de Theil (comparação com previsão naive)
# ------------------------------------------------------------
# U < 1 indica desempenho melhor que a previsão ingênua (último valor observado)
naive_pred  <- rep(as.numeric(tail(treino, 1)), n_teste)
erro_modelo <- as.numeric(teste) - as.numeric(previsao_teste$mean)
erro_naive  <- as.numeric(teste) - naive_pred

U_theil <- sqrt(mean(erro_modelo^2)) / sqrt(mean(erro_naive^2))
cat("Indicador U de Theil:", round(U_theil, 3), "\n")

# ------------------------------------------------------------
# Tabela comparativa mês a mês (previsto vs. observado)
# ------------------------------------------------------------
tabela_validacao <- data.frame(
  mes       = month.abb[cycle(teste)],
  observado = round(as.numeric(teste), 2),
  previsto  = round(as.numeric(previsao_teste$mean), 2),
  erro      = round(as.numeric(teste) - as.numeric(previsao_teste$mean), 2)
)
print(tabela_validacao)


# =============================================================================
# PASSO 11: Reestimação com a série completa e previsão final (dez/2026)
# -----------------------------------------------------------------------------
# Após validar a capacidade preditiva do modelo no Passo 10 (fora da
# amostra), reestimamos o SARIMA(1,0,1)(0,1,2)[12] usando TODA a série
# disponível (jan/2001 a dez/2025), para gerar a previsão genuína fora da
# amostra: o ano de 2026 completo.
# =============================================================================

# Reestima o modelo final com a série completa (mesma ordem já validada)
fit_final <- Arima(
  serie,
  order    = c(1, 0, 1),
  seasonal = list(order = c(0, 1, 2), period = 12)
)

summary(fit_final)

# Quantos meses faltam para dezembro de 2026, a partir do fim da série?
ultimo_ano <- end(serie)[1]
ultimo_mes <- end(serie)[2]
h_previsao <- (2026 - ultimo_ano) * 12 + (12 - ultimo_mes)
cat("Horizonte de previsão (meses):", h_previsao, "\n")

# Gera a previsão com intervalos de 80% e 95%
previsao_final <- forecast(fit_final, h = h_previsao, level = c(80, 95))

# ------------------------------------------------------------
# Gráfico da previsão final (série completa + forecast)
# ------------------------------------------------------------
autoplot(previsao_final) +
  labs(
    title    = "Previsão da Temperatura Média Mensal - Brasília (A001)",
    subtitle = "SARIMA(1,0,1)(0,1,2)[12] - Previsão até dezembro de 2026",
    x = "Ano", y = "Temperatura (°C)"
  ) +
  theme_minimal()

# Zoom nos últimos anos + previsão, para melhor visualização
autoplot(previsao_final) +
  coord_cartesian(xlim = c(2022, 2027)) +
  labs(
    title    = "Previsão da Temperatura Média Mensal - Brasília (A001)",
    subtitle = "Zoom: 2022-2026",
    x = "Ano", y = "Temperatura (°C)"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# Tabela com os valores previstos mês a mês, com IC 80% e 95%
# ------------------------------------------------------------
tabela_previsao <- data.frame(
  ano      = floor(time(previsao_final$mean)),
  mes      = month.abb[cycle(previsao_final$mean)],
  previsto = round(as.numeric(previsao_final$mean), 2),
  li_80    = round(as.numeric(previsao_final$lower[,1]), 2),
  ls_80    = round(as.numeric(previsao_final$upper[,1]), 2),
  li_95    = round(as.numeric(previsao_final$lower[,2]), 2),
  ls_95    = round(as.numeric(previsao_final$upper[,2]), 2)
)
print(tabela_previsao)



