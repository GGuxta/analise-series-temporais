
# PROJETO 2 - Mudança Estrutural no Comércio Varejista Brasileiro
#             após a Pandemia de COVID-19
#
# Fonte dos dados: IBGE - Pesquisa Mensal do Comércio (PMC)
#                  Tabela SIDRA 8880
# Pacote de acesso: sidrar


# ------------------------------------------------------

#  PACOTES NECESSÁRIOS

# sidrar    -> acesso programático às tabelas do SIDRA/IBGE
# tidyverse -> manipulação de dados (dplyr) e datas (lubridate)
# forecast  -> funções de séries temporais (ndiffs, auto.arima etc.)
#              (será usado a partir do Objetivo 3 em diante)
# ------------------------------------------------------------

pacotes <- c("sidrar", "tidyverse", "lubridate", "forecast")

for (p in pacotes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}


# ============================================================
# OBJETIVO 1: Obter os dados da PMC utilizando o pacote sidrar
# ============================================================

# A Tabela 8880 do SIDRA contém o "Índice e variação da receita nominal
# e do volume de vendas no comércio varejista (2022 = 100)".
#
# Dentro dessa tabela, é necessário escolher:
#
#   (a) a VARIÁVEL correta:
#       - cod 7169 = "PMC - Número-índice"                (SEM ajuste sazonal)
#       - cod 7170 = "PMC - Número-índice com ajuste sazonal"
#
#       Escolhemos a variável 7169 (SEM ajuste sazonal), pois um dos
#       objetivos do projeto é justamente investigar se o padrão SAZONAL
#       da série mudou após a pandemia. Se usássemos a série já ajustada
#       sazonalmente, estaríamos removendo a priori o componente que
#       precisamos estudar.
#
#   (b) a CLASSIFICAÇÃO (classific_category) c11046 - "Tipos de índice":
#       - cod 56733 = Índice de receita nominal de vendas
#       - cod 56734 = Índice de VOLUME de vendas  <- é este que o
#                     enunciado do projeto pede ("índice de volume
#                     de vendas no comércio varejista")
#
#   (c) a abrangência GEOGRÁFICA: Brasil (nível nacional, conforme pedido)

pmc_raw <- get_sidra(
  x         = 8880,          # Tabela: Índice e variação (receita/volume)
  variable  = 7169,          # Número-índice SEM ajuste sazonal
  period    = "all",         # Toda a série histórica disponível
  geo       = "Brazil",      # Abrangência nacional
  classific = "c11046",      # Classificação: Tipos de índice
  category  = list(56734)    # Categoria: Índice de volume de vendas
)

# Inspeciona a estrutura do objeto retornado pelo sidrar
# (colunas de código e descrição de mês, valor do índice, unidade etc.)
glimpse(pmc_raw)


# ============================================================
# OBJETIVO 2: Construir a série temporal do índice de volume
#             de vendas do comércio varejista
# ============================================================

# ------------------------------------------------------------
# 2.1 Seleção e tratamento das colunas relevantes
# ------------------------------------------------------------
# O sidrar retorna o mês no formato "AAAAMM" (ex.: "202001").
# Convertemos essa string para uma data (dia 1 de cada mês) usando
# lubridate::ym(), o que facilita ordenação, filtragem por período
# (pré/pós-COVID) e a construção do objeto de série temporal (ts).

pmc_df <- pmc_raw %>%
  select(mes_cod = `Mês (Código)`, valor = Valor) %>%
  mutate(
    data  = ym(mes_cod),        # "202001" -> 2020-01-01
    valor = as.numeric(valor)   # garante que o índice é numérico
  ) %>%
  arrange(data)                 # garante ordem cronológica crescente

# ------------------------------------------------------------
# 2.2 Checagens de qualidade dos dados
# ------------------------------------------------------------
# Antes de seguir, verificamos:
#   - o intervalo de datas coberto pela série;
#   - se há valores ausentes (NA);
#   - se não há meses duplicados/faltantes (a contagem de datas únicas
#     deve ser igual ao número de linhas do data frame).

intervalo_datas   <- range(pmc_df$data)
n_valores_ausentes <- sum(is.na(pmc_df$valor))
serie_sem_lacunas  <- n_distinct(pmc_df$data) == nrow(pmc_df)

cat("Período coberto pela série: de", format(intervalo_datas[1], "%m/%Y"),
    "até", format(intervalo_datas[2], "%m/%Y"), "\n")
cat("Número de valores ausentes (NA):", n_valores_ausentes, "\n")
cat("Série sem lacunas/duplicidades de datas?", serie_sem_lacunas, "\n")

# ------------------------------------------------------------
# 2.3 Construção do objeto de série temporal (ts)
# ------------------------------------------------------------
# A PMC é uma série mensal, portanto frequency = 12.
# O início (start) é definido dinamicamente a partir da primeira
# observação disponível, evitando erro caso a tabela do SIDRA seja
# atualizada no futuro com um período inicial diferente.

ano_inicio <- year(intervalo_datas[1])
mes_inicio <- month(intervalo_datas[1])

pmc_ts <- ts(
  data      = pmc_df$valor,
  start     = c(ano_inicio, mes_inicio),
  frequency = 12
)

# Checagens finais do objeto ts
cat("Classe do objeto criado:", class(pmc_ts), "\n")
cat("Início da série:", start(pmc_ts)[1], "/", start(pmc_ts)[2], "\n")
cat("Fim da série:   ", end(pmc_ts)[1], "/", end(pmc_ts)[2], "\n")
cat("Número de observações:", length(pmc_ts), "\n")

# Visualização rápida (apenas para conferência - o gráfico "oficial"
# para o relatório será produzido no Objetivo 3, com ggplot2)
plot(pmc_ts,
     main = "PMC - Índice de Volume de Vendas no Comércio Varejista",
     ylab = "Índice (2022 = 100)",
     xlab = "Ano")
abline(v = 2020, col = "red", lty = 2)  # marca visualmente o início da pandemia

# ============================================================
# OBJETIVO 3: Análise exploratória da série
# ============================================================

# ------------------------------------------------------------
# 3.1 Gráfico da série (versão para o relatório, via ggplot2)
# ------------------------------------------------------------
# Convertendo o objeto ts para data frame para plotar com ggplot2,
# o que facilita a formatação (eixo de datas, tema, cores) exigida
# em um relatório técnico.

pmc_df_plot <- pmc_df %>%
  select(data, valor)

ggplot(pmc_df_plot, aes(x = data, y = valor)) +
  geom_line(color = "steelblue") +
  geom_vline(xintercept = as.Date("2020-01-01"),
             linetype = "dashed", color = "red") +
  labs(
    title = "PMC - Índice de Volume de Vendas no Comércio Varejista",
    subtitle = "Brasil, jan/2000 - abr/2026 (linha vermelha: início de 2020)",
    x = "Ano", y = "Índice (2022 = 100)"
  ) +
  theme_minimal()



# ------------------------------------------------------------
# 3.2  Boxplot mensal (perfil sazonal)
# ------------------------------------------------------------
# Correção: extraímos o número do mês (1-12), que é independente de
# locale, e aplicamos os rótulos manualmente em português - evita o
# problema de month(..., label = TRUE) depender do idioma do sistema.

pmc_df_plot <- pmc_df_plot %>%
  mutate(
    mes_num = lubridate::month(data),
    mes = factor(mes_num, levels = 1:12,
                 labels = c("Jan","Fev","Mar","Abr","Mai","Jun",
                            "Jul","Ago","Set","Out","Nov","Dez"))
  )

ggplot(pmc_df_plot, aes(x = mes, y = valor)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  labs(
    title = "Distribuição do índice de volume de vendas por mês do ano",
    subtitle = "Série completa (2000-2026), em nível",
    x = "Mês", y = "Índice (2022 = 100)"
  ) +
  theme_minimal()



sazonal_df <- pmc_df %>%
  mutate(
    sazonal = as.numeric(pmc_stl$time.series[, "seasonal"]),
    mes_num = lubridate::month(data),
    mes = factor(mes_num, levels = 1:12,
                 labels = c("Jan","Fev","Mar","Abr","Mai","Jun",
                            "Jul","Ago","Set","Out","Nov","Dez"))
  )

ggplot(sazonal_df, aes(x = mes, y = sazonal)) +
  geom_boxplot(fill = "darkorange", alpha = 0.6) +
  labs(
    title = "Componente sazonal (STL) do índice de volume de vendas",
    subtitle = "Por mês do ano - série completa",
    x = "Mês", y = "Componente sazonal"
  ) +
  theme_minimal()




# ------------------------------------------------------------
# 3.3 Decomposição STL da série completa
# ------------------------------------------------------------
# A decomposição STL (Seasonal-Trend decomposition using Loess) permite
# separar a série em tendência, sazonalidade e resíduo, e quantificar
# a força de cada componente (medidas F_T e F_S).
#
# Aplicamos aqui à série COMPLETA apenas para fins de análise
# exploratória geral; a comparação formal pré/pós-COVID (Objetivos 4-7)
# será feita separadamente sobre cada subamostra.

pmc_stl <- stl(pmc_ts, s.window = "periodic", robust = TRUE)

plot(pmc_stl,
     main = "Decomposição STL - PMC Índice de Volume de Vendas")

# Força da tendência e da sazonalidade (Hyndman & Athanasopoulos, 2021)
componentes <- as.data.frame(pmc_stl$time.series)

forca_sazonal <- max(0, 1 - var(componentes$remainder) /
                       var(componentes$seasonal + componentes$remainder))
forca_tendencia <- max(0, 1 - var(componentes$remainder) /
                         var(componentes$trend + componentes$remainder))

cat("Força da componente sazonal (F_S):", round(forca_sazonal, 3), "\n")
cat("Força da componente de tendência (F_T):", round(forca_tendencia, 3), "\n")


# ------------------------------------------------------------
# 3.4 ACF e PACF da série em nível (série completa)
# ------------------------------------------------------------
# Avaliação preliminar da estrutura de dependência temporal antes
# de qualquer diferenciação ou divisão da amostra.

acf(pmc_ts, lag.max = 48,
    main = "ACF - PMC Índice de Volume de Vendas (nível, série completa)")

pacf(pmc_ts, lag.max = 48,
     main = "PACF - PMC Índice de Volume de Vendas (nível, série completa)")


# ------------------------------------------------------------
# 3.5 Testes de estacionariedade (ADF e KPSS) - série completa
# ------------------------------------------------------------
# Utilizados apenas como diagnóstico preliminar da série completa.
# ndiffs() / nsdiffs() indicam o número de diferenças simples e
# sazonais recomendadas.

library(tseries)   # para adf.test() e kpss.test()

adf.test(pmc_ts)
kpss.test(pmc_ts, null = "Trend")

cat("Diferenças simples sugeridas (ndiffs):", ndiffs(pmc_ts), "\n")
cat("Diferenças sazonais sugeridas (nsdiffs):", nsdiffs(pmc_ts), "\n")





# ============================================================
# OBJETIVO 4: Identificação de quebras estruturais
# ============================================================

if (!require(strucchange)) install.packages("strucchange")
library(strucchange)

# ------------------------------------------------------------
# 4.1 Preparação: regressão auxiliar
# ------------------------------------------------------------
# Os testes de quebra estrutural (Chow, CUSUM, Quandt-Andrews) são
# definidos sobre um MODELO DE REGRESSÃO, não diretamente sobre a
# série bruta. Usamos aqui uma regressão auxiliar simples e amplamente
# utilizada nesse tipo de teste: a série regredida sobre o tempo e
# sobre seu próprio valor defasado em 1 mês (AR(1) com tendência),
# que captura de forma parcimoniosa tanto o nível quanto a dependência
# temporal de curto prazo.
#
# y_t = beta0 + beta1 * t + beta2 * y_{t-1} + erro_t

# Criamos a variável defasada e o índice de tempo
pmc_reg_df <- pmc_df %>%
  mutate(
    y      = valor,
    y_lag1 = lag(valor, 1),
    tempo  = row_number()
  ) %>%
  filter(!is.na(y_lag1))  # remove a 1ª observação (sem lag disponível)

# ------------------------------------------------------------
# 4.2 Teste de Chow (ponto de quebra conhecido: jan/2020)
# ------------------------------------------------------------
# sctest() com type = "Chow" exige o ponto de quebra como PROPORÇÃO
# da amostra (0 a 1). Calculamos essa proporção a partir da posição
# de janeiro de 2020 no data frame já sem a 1ª observação.

pos_jan2020 <- which(pmc_reg_df$data == as.Date("2020-01-01"))
prop_quebra <- pos_jan2020 / nrow(pmc_reg_df)

cat("Posição de jan/2020 na amostra:", pos_jan2020, "de", nrow(pmc_reg_df), "\n")
cat("Proporção correspondente:", round(prop_quebra, 3), "\n")

teste_chow <- sctest(y ~ tempo + y_lag1, data = pmc_reg_df,
                     type = "Chow", point = pos_jan2020)
print(teste_chow)

# ------------------------------------------------------------
# 4.3 Teste de Quandt-Andrews (ponto de quebra desconhecido)
# ------------------------------------------------------------
# Fstats() calcula a estatística de Chow para cada ponto candidato
# dentro do intervalo [from, to] (tipicamente 15% a 85% da amostra,
# para evitar instabilidade nas extremidades). sctest() aplicado ao
# objeto Fstats retorna o teste sup-F (Quandt-Andrews).

fs <- Fstats(y ~ tempo + y_lag1, data = pmc_reg_df, from = 0.15, to = 0.85)

plot(fs, main = "Estatística F (Quandt-Andrews) ao longo da amostra")
# Marca visualmente onde estaria jan/2020, para comparação com o
# ponto de quebra estatisticamente mais provável (pico da curva)
abline(v = pmc_reg_df$tempo[pos_jan2020] / nrow(pmc_reg_df),
       col = "red", lty = 2)

teste_qa <- sctest(fs, type = "supF")
print(teste_qa)

# Data estimada do breakpoint de maior estatística F
indice_break <- which.max(fs$Fstats)
data_break_estimada <- pmc_reg_df$data[indice_break]
cat("Data do ponto de quebra mais provável (sup-F):",
    format(data_break_estimada, "%m/%Y"), "\n")


# ------------------------------------------------------------
# 4.4 CUSUM de Quadrados
# ------------------------------------------------------------
# A constante crítica correta do CUSUM de quadrados depende do
# tamanho amostral e vem de tabelas baseadas em quantis Beta -
# diferente da constante do CUSUM simples (0,948), usada por engano
# na versão anterior. Para não arriscar usar um valor tabelado de
# memória incorretamente, obtemos a fronteira crítica por SIMULAÇÃO:
# geramos ruído branco puro (H0: variância estável), calculamos o
# CUSUM de quadrados para cada simulação, e tomamos o percentil 95%
# do desvio máximo em relação à reta esperada (t/n). Isso nos dá uma
# fronteira crítica válida para o nosso n exato, sem depender de
# tabela.

set.seed(897)
n_rec  <- length(resid_rec)
nsim   <- 5000
desvio_max_sim <- numeric(nsim)

for (i in 1:nsim) {
  e_sim  <- rnorm(n_rec)                       # ruído branco sob H0
  s2_sim <- cumsum(e_sim^2) / sum(e_sim^2)     # CUSUM de quadrados simulado
  desvio_max_sim[i] <- max(abs(s2_sim - (1:n_rec) / n_rec))
}

c0_boot <- as.numeric(quantile(desvio_max_sim, 0.95))
cat("Constante crítica c0 (5%, via simulação):", round(c0_boot, 4), "\n")

# Estatística observada nos dados reais
s2 <- cumsum(resid_rec^2) / sum(resid_rec^2)
desvio_obs <- s2 - (1:n_rec) / n_rec
desvio_max_obs <- max(abs(desvio_obs))

cat("Maior desvio observado |CUSUMSQ - t/n|:", round(desvio_max_obs, 4), "\n")
cat("Ultrapassa a fronteira de 5%?", desvio_max_obs > c0_boot, "\n")

# Gráfico com a fronteira correta
tempo_rel <- (1:n_rec) / n_rec
plot(tempo_rel, s2, type = "l",
     main = "CUSUM de Quadrados - Estabilidade da variância",
     xlab = "Tempo (proporção da amostra)",
     ylab = "CUSUM de quadrados (normalizado)")
lines(tempo_rel, tempo_rel, col = "gray50")
lines(tempo_rel, tempo_rel + c0_boot, col = "red", lty = 2)
lines(tempo_rel, tempo_rel - c0_boot, col = "red", lty = 2)
abline(v = pos_jan2020 / n_rec, col = "blue", lty = 3)



# ------------------------------------------------------------
# Salvando os gráficos de quebra estrutural como arquivos JPEG
# (necessário para os \includegraphics no relatório)
# ------------------------------------------------------------

dir.create("imagens", showWarnings = FALSE)

jpeg("imagens/pmc_quandt_andrews.jpeg", width = 900, height = 600, res = 120)
plot(fs, main = "Estatística F (Quandt-Andrews) ao longo da amostra")
abline(v = pmc_reg_df$tempo[pos_jan2020] / nrow(pmc_reg_df), col = "red", lty = 2)
dev.off()

jpeg("imagens/pmc_cusum.jpeg", width = 900, height = 600, res = 120)
plot(cusum_proc, main = "CUSUM - Estabilidade dos coeficientes")
abline(v = pmc_reg_df$tempo[pos_jan2020] / nrow(pmc_reg_df), col = "red", lty = 2)
dev.off()

jpeg("imagens/pmc_cusum_quadrados.jpeg", width = 900, height = 600, res = 120)
plot(tempo_rel, s2, type = "l",
     main = "CUSUM de Quadrados - Estabilidade da variância",
     xlab = "Tempo (proporção da amostra)",
     ylab = "CUSUM de quadrados (normalizado)")
lines(tempo_rel, tempo_rel, col = "gray50")
lines(tempo_rel, tempo_rel + c0_boot, col = "red", lty = 2)
lines(tempo_rel, tempo_rel - c0_boot, col = "red", lty = 2)
abline(v = pos_jan2020 / n_rec, col = "blue", lty = 3)
dev.off()





# ============================================================
# OBJETIVO 5: Divisão da amostra em pré-COVID e pós-COVID
# ============================================================

# ------------------------------------------------------------
# 5.1 Definição do ponto de corte
# ------------------------------------------------------------
# Conforme o enunciado do projeto:
#   - Pré-COVID : até dezembro de 2019 (inclusive)
#   - Pós-COVID : a partir de janeiro de 2020 (inclusive)
#
# Observação: mantemos jan/2020 como o primeiro ponto do período
# "pós", coerente com a data usada no teste de Chow (Seção 3.2) e
# com o enunciado do projeto, mesmo sabendo que o teste de
# Quandt-Andrews apontou fev/2012 como o ponto de quebra mais forte
# de toda a série - a divisão pré/pós-COVID é uma escolha temática do
# projeto, não uma escolha baseada no ponto ótimo estatístico.

pmc_pre  <- window(pmc_ts, end   = c(2019, 12))
pmc_pos  <- window(pmc_ts, start = c(2020, 1))

cat("Período PRÉ-COVID:\n")
cat("  Início:", start(pmc_pre)[1], "/", start(pmc_pre)[2], "\n")
cat("  Fim:   ", end(pmc_pre)[1], "/", end(pmc_pre)[2], "\n")
cat("  Nº de observações:", length(pmc_pre), "\n\n")

cat("Período PÓS-COVID:\n")
cat("  Início:", start(pmc_pos)[1], "/", start(pmc_pos)[2], "\n")
cat("  Fim:   ", end(pmc_pos)[1], "/", end(pmc_pos)[2], "\n")
cat("  Nº de observações:", length(pmc_pos), "\n")

# Verificação de consistência: soma das duas subséries deve bater com
# o total da série completa
cat("\nVerificação: n(pré) + n(pós) == n(total)?",
    (length(pmc_pre) + length(pmc_pos)) == length(pmc_ts), "\n")


# ------------------------------------------------------------
# 5.2 Gráfico comparativo das duas subséries lado a lado
# ------------------------------------------------------------
# Para o relatório: um único gráfico com as duas subséries destacadas
# por cor, facilitando a comparação visual do nível e da variabilidade
# em cada período.

pmc_df_periodos <- pmc_df %>%
  mutate(periodo = ifelse(data < as.Date("2020-01-01"),
                          "Pré-COVID (até dez/2019)",
                          "Pós-COVID (a partir de jan/2020)"))

ggplot(pmc_df_periodos, aes(x = data, y = valor, color = periodo)) +
  geom_line() +
  scale_color_manual(values = c("Pré-COVID (até dez/2019)" = "steelblue",
                                "Pós-COVID (a partir de jan/2020)" = "firebrick")) +
  labs(
    title = "PMC - Índice de Volume de Vendas: Pré-COVID vs. Pós-COVID",
    x = "Ano", y = "Índice (2022 = 100)", color = "Período"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("imagens/pmc_pre_pos_covid.jpeg", width = 9, height = 5, dpi = 300)


# ------------------------------------------------------------
# 5.3 Estatísticas descritivas comparativas
# ------------------------------------------------------------
# Comparação simples de nível e dispersão entre os dois períodos,
# como primeira aproximação numérica às perguntas de discussão do
# projeto (mudança de nível? mudança de volatilidade?).

estat_periodo <- pmc_df_periodos %>%
  group_by(periodo) %>%
  summarise(
    n         = n(),
    media     = mean(valor),
    dp        = sd(valor),
    cv        = dp / media,        # coeficiente de variação
    minimo    = min(valor),
    maximo    = max(valor)
  )

print(estat_periodo)


# ============================================================
# OBJETIVO 3 (repetido para cada subamostra): Análise exploratória
#             separada - pré-COVID e pós-COVID
# ============================================================

# ------------------------------------------------------------
# 5.4 Decomposição STL separada por período
# ------------------------------------------------------------
# Repetimos a decomposição STL (já aplicada à série completa na
# Seção de Análise Descritiva) separadamente para cada subamostra,
# permitindo comparar a força da sazonalidade e da tendência ANTES
# e DEPOIS da pandemia - uma das perguntas centrais do projeto.
#
# Observação: a subsérie pós-COVID tem menos observações (apenas
# alguns anos), o que reduz a robustez da decomposição STL em relação
# à série completa - isso será discutido nos resultados.

stl_pre <- stl(pmc_pre, s.window = "periodic", robust = TRUE)
stl_pos <- stl(pmc_pos, s.window = "periodic", robust = TRUE)

jpeg("imagens/pmc_stl_pre.jpeg", width = 900, height = 700, res = 120)
plot(stl_pre, main = "Decomposição STL - Período Pré-COVID")
dev.off()

jpeg("imagens/pmc_stl_pos.jpeg", width = 900, height = 700, res = 120)
plot(stl_pos, main = "Decomposição STL - Período Pós-COVID")
dev.off()

# Força da sazonalidade e da tendência em cada período
forca_componentes <- function(stl_obj) {
  comp <- as.data.frame(stl_obj$time.series)
  fs <- max(0, 1 - var(comp$remainder) / var(comp$seasonal + comp$remainder))
  ft <- max(0, 1 - var(comp$remainder) / var(comp$trend + comp$remainder))
  c(F_S = fs, F_T = ft)
}

forca_pre <- forca_componentes(stl_pre)
forca_pos <- forca_componentes(stl_pos)

cat("Força sazonal/tendência - PRÉ-COVID: ", round(forca_pre, 3), "\n")
cat("Força sazonal/tendência - PÓS-COVID:", round(forca_pos, 3), "\n")


# ------------------------------------------------------------
# 5.5 ACF e PACF em nível, separadas por período
# ------------------------------------------------------------

jpeg("imagens/pmc_acf_pre.jpeg", width = 900, height = 600, res = 120)
acf(pmc_pre, lag.max = 36, main = "ACF - Período Pré-COVID (nível)")
dev.off()

jpeg("imagens/pmc_pacf_pre.jpeg", width = 900, height = 600, res = 120)
pacf(pmc_pre, lag.max = 36, main = "PACF - Período Pré-COVID (nível)")
dev.off()

jpeg("imagens/pmc_acf_pos.jpeg", width = 900, height = 600, res = 120)
acf(pmc_pos, lag.max = 36, main = "ACF - Período Pós-COVID (nível)")
dev.off()

jpeg("imagens/pmc_pacf_pos.jpeg", width = 900, height = 600, res = 120)
pacf(pmc_pos, lag.max = 36, main = "PACF - Período Pós-COVID (nível)")
dev.off()


# ------------------------------------------------------------
# 5.6 Testes de estacionariedade e ordens de diferenciação,
#     separados por período
# ------------------------------------------------------------

cat("\n--- PRÉ-COVID ---\n")
print(adf.test(pmc_pre))
print(kpss.test(pmc_pre, null = "Trend"))
cat("ndiffs:", ndiffs(pmc_pre), " | nsdiffs:", nsdiffs(pmc_pre), "\n")

cat("\n--- PÓS-COVID ---\n")
print(adf.test(pmc_pos))
print(kpss.test(pmc_pos, null = "Trend"))
cat("ndiffs:", ndiffs(pmc_pos), " | nsdiffs:", nsdiffs(pmc_pos), "\n")


# ============================================================
# OBJETIVO 6: Identificação e ajuste de modelos SARIMA
#             separados para os períodos pré-COVID e pós-COVID
# ============================================================

# ------------------------------------------------------------
# 6.1 Diferenciação das séries (com base nos resultados da Seção
#     anterior: d=1 e D=1 sugeridos para ambos os períodos)
# ------------------------------------------------------------
# Aplicamos diferenciação simples (d=1) seguida de diferenciação
# sazonal (D=1) a cada subsérie, e verificamos se a série resultante
# já é estacionária (ndiffs/nsdiffs == 0), antes de identificar as
# ordens (p,q)(P,Q) via ACF/PACF.

# Observação metodológica: a diferenciação simples (d=1) é aplicada
# apenas à série pré-COVID. Para o pós-COVID, o teste ADF/KPSS e o
# auto.arima() indicaram que d=0 é suficiente - aplicar d=1 aqui
# superdiferenciaria a série, introduzindo autocorrelação negativa
# artificial. Cada período recebe, portanto, a ordem de diferenciação
# que lhe é adequada:
#   - Pré-COVID: d=1, D=1
#   - Pós-COVID: d=0, D=1

pmc_pre_diff <- diff(diff(pmc_pre, lag = 1), lag = 12)
pmc_pos_diff <- diff(pmc_pos, lag = 12)   # apenas D=1, sem d=1

cat("--- Verificação pós-diferenciação (PRÉ-COVID) ---\n")
cat("ndiffs:", ndiffs(pmc_pre_diff), " | nsdiffs:", nsdiffs(pmc_pre_diff), "\n")
cat("Nº de observações após diferenciação:", length(pmc_pre_diff), "\n\n")

cat("--- Verificação pós-diferenciação (PÓS-COVID) ---\n")
cat("ndiffs:", ndiffs(pmc_pos_diff), " | nsdiffs:", nsdiffs(pmc_pos_diff), "\n")
cat("Nº de observações após diferenciação:", length(pmc_pos_diff), "\n")

# Gráficos das séries diferenciadas (verificação visual de
# estacionariedade)
jpeg("imagens/pmc_pre_diff.jpeg", width = 900, height = 500, res = 120)
plot(pmc_pre_diff, main = "Série Pré-COVID após diferenciação (d=1, D=1)",
     ylab = "Valor diferenciado")
abline(h = 0, col = "gray50", lty = 2)
dev.off()

jpeg("imagens/pmc_pos_diff.jpeg", width = 900, height = 500, res = 120)
plot(pmc_pos_diff, main = "Série Pós-COVID após diferenciação (d=0, D=1)",
     ylab = "Valor diferenciado")
abline(h = 0, col = "gray50", lty = 2)
dev.off()


# ------------------------------------------------------------
# 6.2 ACF e PACF das séries diferenciadas (identificação manual)
# ------------------------------------------------------------
# Nota: como cada subsérie tem histórico diferente (240 vs. 76 obs.),
# usamos lag.max proporcional em vez de um valor fixo para ambas,
# evitando lags excessivos em relação ao tamanho da amostra pós-COVID
# (regra prática: lag.max <= n/4).

lag_max_pre <- min(48, floor(length(pmc_pre_diff) / 4))
lag_max_pos <- min(24, floor(length(pmc_pos_diff) / 4))

cat("lag.max usado (pré-COVID):", lag_max_pre, "\n")
cat("lag.max usado (pós-COVID):", lag_max_pos, "\n")

jpeg("imagens/pmc_acf_pre_diff.jpeg", width = 900, height = 600, res = 120)
acf(pmc_pre_diff, lag.max = lag_max_pre,
    main = "ACF - Pré-COVID (diferenciada, d=1,D=1)")
dev.off()

jpeg("imagens/pmc_pacf_pre_diff.jpeg", width = 900, height = 600, res = 120)
pacf(pmc_pre_diff, lag.max = lag_max_pre,
     main = "PACF - Pré-COVID (diferenciada, d=1,D=1)")
dev.off()

jpeg("imagens/pmc_acf_pos_diff.jpeg", width = 900, height = 600, res = 120)
acf(pmc_pos_diff, lag.max = lag_max_pos,
    main = "ACF - Pós-COVID (diferenciada, d=0,D=1)")
dev.off()

jpeg("imagens/pmc_pacf_pos_diff.jpeg", width = 900, height = 600, res = 120)
pacf(pmc_pos_diff, lag.max = lag_max_pos,
     main = "PACF - Pós-COVID (diferenciada, d=0,D=1)")
dev.off()


# ------------------------------------------------------------
# 6.3 Identificação automática via auto.arima() - PRÉ-COVID
# ------------------------------------------------------------
# stepwise = FALSE e approximation = FALSE: busca exaustiva sobre o
# espaço de modelos candidatos (mais lenta, porém mais confiável do
# que a busca stepwise padrão), com base no critério AICc.

modelo_auto_pre <- auto.arima(
  pmc_pre,
  stepwise = FALSE,
  approximation = FALSE,
  seasonal = TRUE
)

cat("\n=== MODELO AUTOMÁTICO - PRÉ-COVID ===\n")
print(modelo_auto_pre)
cat("AIC:", AIC(modelo_auto_pre), " | BIC:", BIC(modelo_auto_pre), "\n")


# ------------------------------------------------------------
# 6.4 Identificação automática via auto.arima() - PÓS-COVID
# ------------------------------------------------------------
# Observação: com apenas 76 observações, o espaço de busca é
# naturalmente mais restrito (auto.arima() limita a ordem máxima de
# acordo com o tamanho amostral disponível).

modelo_auto_pos <- auto.arima(
  pmc_pos,
  stepwise = FALSE,
  approximation = FALSE,
  seasonal = TRUE
)

cat("\n=== MODELO AUTOMÁTICO - PÓS-COVID ===\n")
print(modelo_auto_pos)
cat("AIC:", AIC(modelo_auto_pos), " | BIC:", BIC(modelo_auto_pos), "\n")


# ------------------------------------------------------------
# 6.5 Coeficientes estimados de cada modelo (para tabela no relatório)
# ------------------------------------------------------------

cat("\n--- Coeficientes PRÉ-COVID ---\n")
print(lmtest::coeftest(modelo_auto_pre))

cat("\n--- Coeficientes PÓS-COVID ---\n")
print(lmtest::coeftest(modelo_auto_pos))














# ============================================================
# OBJETIVO 8: Diagnóstico dos resíduos dos modelos SARIMA
#             (pré-COVID e pós-COVID)
# ============================================================

# ------------------------------------------------------------
# 8.1 Diagnóstico padrão via checkresiduals() - PRÉ-COVID
# ------------------------------------------------------------
# checkresiduals() (pacote forecast) já produz: gráfico dos resíduos
# ao longo do tempo, ACF dos resíduos e histograma, além de realizar
# automaticamente o teste de Ljung-Box com o número de graus de
# liberdade corrigido pelo número de parâmetros do modelo.

jpeg("imagens/pmc_pre_checkresiduals.jpeg", width = 900, height = 700, res = 120)
checkresiduals(modelo_auto_pre)
dev.off()

# Reexecuta fora do jpeg() para capturar o output impresso do teste
res_lb_pre <- checkresiduals(modelo_auto_pre, plot = FALSE)
print(res_lb_pre)


# ------------------------------------------------------------
# 8.2 Diagnóstico padrão via checkresiduals() - PÓS-COVID
# ------------------------------------------------------------

jpeg("imagens/pmc_pos_checkresiduals.jpeg", width = 900, height = 700, res = 120)
checkresiduals(modelo_auto_pos)
dev.off()

res_lb_pos <- checkresiduals(modelo_auto_pos, plot = FALSE)
print(res_lb_pos)


# ------------------------------------------------------------
# 8.3 ACF e PACF dos resíduos (painel complementar ao checkresiduals)
# ------------------------------------------------------------
# Útil para verificar visualmente se sobra alguma estrutura de
# dependência não capturada pelo modelo, com mais detalhe do que o
# painel padrão do checkresiduals().

jpeg("imagens/pmc_pre_acf_resid.jpeg", width = 900, height = 500, res = 120)
acf(residuals(modelo_auto_pre), lag.max = 36,
    main = "ACF dos resíduos - SARIMA Pré-COVID")
dev.off()

jpeg("imagens/pmc_pos_acf_resid.jpeg", width = 900, height = 500, res = 120)
acf(residuals(modelo_auto_pos), lag.max = 24,
    main = "ACF dos resíduos - SARIMA Pós-COVID")
dev.off()


# ------------------------------------------------------------
# 8.4 Teste de normalidade dos resíduos (Shapiro-Wilk)
# ------------------------------------------------------------

cat("--- Shapiro-Wilk: resíduos PRÉ-COVID ---\n")
print(shapiro.test(residuals(modelo_auto_pre)))

cat("\n--- Shapiro-Wilk: resíduos PÓS-COVID ---\n")
print(shapiro.test(residuals(modelo_auto_pos)))


# ------------------------------------------------------------
# 8.5 QQ-plot dos resíduos (normalidade, visual)
# ------------------------------------------------------------

jpeg("imagens/pmc_pre_qqplot.jpeg", width = 700, height = 700, res = 120)
qqnorm(residuals(modelo_auto_pre), main = "QQ-plot - Resíduos SARIMA Pré-COVID")
qqline(residuals(modelo_auto_pre), col = "red")
dev.off()

jpeg("imagens/pmc_pos_qqplot.jpeg", width = 700, height = 700, res = 120)
qqnorm(residuals(modelo_auto_pos), main = "QQ-plot - Resíduos SARIMA Pós-COVID")
qqline(residuals(modelo_auto_pos), col = "red")
dev.off()


# ------------------------------------------------------------
# 8.6 Resíduos versus valores ajustados (homocedasticidade e
#     adequação funcional)
# ------------------------------------------------------------

jpeg("imagens/pmc_pre_resid_fitted.jpeg", width = 800, height = 600, res = 120)
plot(as.numeric(fitted(modelo_auto_pre)), as.numeric(residuals(modelo_auto_pre)),
     xlab = "Valores ajustados", ylab = "Resíduos",
     main = "Resíduos vs. Ajustados - SARIMA Pré-COVID")
abline(h = 0, col = "gray50", lty = 2)
lines(lowess(fitted(modelo_auto_pre), residuals(modelo_auto_pre)), col = "red")
dev.off()

jpeg("imagens/pmc_pos_resid_fitted.jpeg", width = 800, height = 600, res = 120)
plot(as.numeric(fitted(modelo_auto_pos)), as.numeric(residuals(modelo_auto_pos)),
     xlab = "Valores ajustados", ylab = "Resíduos",
     main = "Resíduos vs. Ajustados - SARIMA Pós-COVID")
abline(h = 0, col = "gray50", lty = 2)
lines(lowess(fitted(modelo_auto_pos), residuals(modelo_auto_pos)), col = "red")
dev.off()


# ------------------------------------------------------------
# 8.7 Comparação formal da VARIÂNCIA residual entre os períodos
# ------------------------------------------------------------
# Esta é a comparação metodologicamente correta para responder à
# pergunta de discussão do projeto sobre volatilidade/previsibilidade
# pós-pandemia - diferentemente da comparação de CV nos dados brutos
# feita na Seção 5.1 (contaminada pela tendência), aqui comparamos a
# variância do erro de previsão de um passo à frente (resíduos), já
# livre de tendência e sazonalidade.

var_resid_pre <- var(residuals(modelo_auto_pre))
var_resid_pos <- var(residuals(modelo_auto_pos))

cat("Variância residual - PRÉ-COVID:", round(var_resid_pre, 3), "\n")
cat("Variância residual - PÓS-COVID:", round(var_resid_pos, 3), "\n")
cat("Razão (pós/pré):", round(var_resid_pos / var_resid_pre, 3), "\n")

teste_var <- var.test(residuals(modelo_auto_pos), residuals(modelo_auto_pre))
print(teste_var)





# ============================================================
# Teste adicional: o modelo PRÉ-COVID ainda descreve o
#                  período PÓS-COVID?
# ============================================================
# Arima(y, model = fit_anterior) REUTILIZA os coeficientes já
# estimados no modelo pré-COVID (não reestima nada), apenas aplicando
# essa mesma estrutura aos dados do período pós-COVID. Se os
# resíduos resultantes ainda forem ruído branco, o modelo pré-COVID
# "sobrevive" no novo período; se não forem, há evidência direta de
# quebra estrutural na relação capturada pelo modelo.

modelo_pre_aplicado_pos <- Arima(pmc_pos, model = modelo_auto_pre)

cat("=== Modelo PRÉ-COVID aplicado aos dados PÓS-COVID ===\n")
print(modelo_pre_aplicado_pos)

# Teste de Ljung-Box sobre os resíduos resultantes
res_cruzado <- checkresiduals(modelo_pre_aplicado_pos, plot = FALSE)
print(res_cruzado)

jpeg("imagens/pmc_modelo_pre_aplicado_pos.jpeg", width = 900, height = 700, res = 120)
checkresiduals(modelo_pre_aplicado_pos)
dev.off()

# Comparação da variância residual: modelo pré-COVID original vs.
# o mesmo modelo aplicado (sem reestimar) ao período pós-COVID
cat("\nVariância residual - modelo PRÉ-COVID (dados originais):",
    round(var(residuals(modelo_auto_pre)), 3), "\n")
cat("Variância residual - modelo PRÉ-COVID aplicado aos dados PÓS-COVID:",
    round(var(residuals(modelo_pre_aplicado_pos)), 3), "\n")
cat("Variância residual - modelo PÓS-COVID (próprio, reestimado):",
    round(var(residuals(modelo_auto_pos)), 3), "\n")





# ============================================================
# ANÁLISE DE SENSIBILIDADE 
# ============================================================

# ------------------------------------------------------------
#  O modelo AR(4) do pós-COVID é realmente melhor que uma
#    estrutura mais simples (ar2, ar3 não significativos)?
# ------------------------------------------------------------
# Comparamos o modelo automático SARIMA(4,0,0)(0,1,0)+drift com
# alternativas mais parcimoniosas (AR(1) e AR(2)), fixando d=0, D=1
# (já justificados pelos testes de estacionariedade), para verificar
# se a complexidade adicional (ar2, ar3) é realmente necessária.

modelo_pos_ar1 <- Arima(pmc_pos, order = c(1, 0, 0),
                        seasonal = list(order = c(0, 1, 0)),
                        include.drift = TRUE)

modelo_pos_ar2 <- Arima(pmc_pos, order = c(2, 0, 0),
                        seasonal = list(order = c(0, 1, 0)),
                        include.drift = TRUE)

cat("=== Comparação de parcimônia - PÓS-COVID ===\n")
cat("AR(4) [modelo original]  - AIC:", AIC(modelo_auto_pos),
    " | BIC:", BIC(modelo_auto_pos), "\n")
cat("AR(2) [alternativa]      - AIC:", AIC(modelo_pos_ar2),
    " | BIC:", BIC(modelo_pos_ar2), "\n")
cat("AR(1) [alternativa]      - AIC:", AIC(modelo_pos_ar1),
    " | BIC:", BIC(modelo_pos_ar1), "\n")

# Diagnóstico residual rápido dos modelos alternativos, para checar
# se a simplificação prejudica a captura da dependência temporal
cat("\n--- Ljung-Box: AR(2) ---\n")
print(checkresiduals(modelo_pos_ar2, plot = FALSE))

cat("\n--- Ljung-Box: AR(1) ---\n")
print(checkresiduals(modelo_pos_ar1, plot = FALSE))


# ------------------------------------------------------------
#  A maior variância residual pós-COVID é robusta à remoção do
#    outlier de 2020 (o choque agudo inicial), ou é inteiramente
#    espúria, causada só por esse ponto?
# ------------------------------------------------------------


residuos_pos <- as.numeric(residuals(modelo_auto_pos))
indice_outlier <- which.max(abs(residuos_pos))
data_outlier <- time(residuals(modelo_auto_pos))[indice_outlier]

cat("\nÍndice do maior resíduo (outlier):", indice_outlier, "\n")
cat("Data aproximada do outlier:", round(data_outlier, 2), "\n")
cat("Valor do resíduo nesse ponto:", round(residuos_pos[indice_outlier], 3), "\n")

# Variância COM e SEM o outlier
var_com_outlier <- var(residuos_pos)
var_sem_outlier <- var(residuos_pos[-indice_outlier])

cat("\nVariância residual pós-COVID (com outlier):", round(var_com_outlier, 3), "\n")
cat("Variância residual pós-COVID (sem outlier):  ", round(var_sem_outlier, 3), "\n")

# Refaz o teste F de comparação de variâncias, agora sem o outlier
residuos_pre <- as.numeric(residuals(modelo_auto_pre))
teste_var_sem_outlier <- var.test(residuos_pos[-indice_outlier], residuos_pre)
print(teste_var_sem_outlier)

cat("\nRazão de variâncias (pós sem outlier / pré):",
    round(var_sem_outlier / var(residuos_pre), 3), "\n")

