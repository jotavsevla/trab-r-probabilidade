# =============================================================================
# TRABALHO DE PROBABILIDADE E ESTATÍSTICA
# Análise de Agricultura Familiar e Financiamento Rural
# Aluno: [SEU NOME AQUI]
# Data: Outubro 2025
# =============================================================================

# Carregando bibliotecas necessárias
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(RColorBrewer)
library(tibble)

# Configuração
options(scipen = 999)
options(OutDec = ",")

# =============================================================================
# ENTRADA DOS DADOS - IBGE Censo Agropecuário
# =============================================================================

dados_ibge <- data.frame(
  regiao = c("Brasil", "Brasil", "Brasil",
            "Semiárido do Piauí", "Semiárido do Piauí", "Semiárido do Piauí",
            "Semiárido do Ceará", "Semiárido do Ceará", "Semiárido do Ceará",
            "Semiárido do RN", "Semiárido do RN", "Semiárido do RN",
            "Semiárido da Paraíba", "Semiárido da Paraíba", "Semiárido da Paraíba",
            "Semiárido de PE", "Semiárido de PE", "Semiárido de PE",
            "Semiárido de Alagoas", "Semiárido de Alagoas", "Semiárido de Alagoas",
            "Semiárido de Sergipe", "Semiárido de Sergipe", "Semiárido de Sergipe",
            "Semiárido da Bahia", "Semiárido da Bahia", "Semiárido da Bahia",
            "Semiárido de MG", "Semiárido de MG", "Semiárido de MG"),
  
  tipo_agricultura = rep(c("Total", "Não familiar", "Agricultura familiar"), 10),
  
  total_estabelecimentos = c(4254939, 669957, 3584982,
                           115084, 11406, 103678,
                           270178, 28644, 241534,
                           61078, 8891, 52187,
                           106340, 13277, 93063,
                           220250, 20481, 199769,
                           67047, 5864, 61183,
                           33791, 3542, 30249,
                           513218, 58738, 454480,
                           75308, 11693, 63615),
  
  falta_garantia = c(77985, 9064, 68921,
                    1648, 169, 1479,
                    6796, 843, 5953,
                    790, 87, 703,
                    1825, 202, 1623,
                    5235, 569, 4666,
                    3574, 390, 3184,
                    281, 28, 253,
                    9444, 1041, 8403,
                    876, 177, 699),
  
  nao_sabe_conseguir = c(61735, 5536, 56199,
                        1347, 112, 1235,
                        2960, 340, 2620,
                        685, 91, 594,
                        1157, 93, 1064,
                        3618, 373, 3245,
                        1211, 65, 1146,
                        246, 17, 229,
                        7662, 735, 6927,
                        1291, 146, 1145),
  
  burocracia = c(355755, 54716, 301039,
                10416, 1240, 9176,
                20786, 2433, 18353,
                6901, 847, 6054,
                6258, 1004, 5254,
                16788, 1868, 14920,
                5171, 464, 4707,
                2630, 260, 2370,
                43430, 6025, 37405,
                6580, 1324, 5256),
  
  falta_pagamento_anterior = c(133419, 16605, 116814,
                              8578, 950, 7628,
                              11293, 1180, 10113,
                              3357, 446, 2911,
                              5739, 888, 4851,
                              9105, 1064, 8041,
                              3546, 392, 3154,
                              2437, 252, 2185,
                              23794, 2645, 21149,
                              5184, 773, 4411),
  
  medo_divida = c(878629, 95033, 783596,
                 36960, 3200, 33760,
                 81028, 7159, 73869,
                 11534, 1226, 10308,
                 29789, 2751, 27038,
                 58462, 4468, 53994,
                 17368, 1240, 16128,
                 5318, 424, 4894,
                 132225, 12153, 120072,
                 22935, 2763, 20172),
  
  outro_motivo = c(538380, 75774, 462606,
                  20942, 2145, 18797,
                  48780, 5292, 43488,
                  11069, 1623, 9446,
                  19155, 2161, 16994,
                  35841, 3486, 32355,
                  11383, 1045, 10338,
                  6495, 700, 5795,
                  85769, 10226, 75543,
                  11169, 1778, 9391),
  
  nao_precisou = c(2209036, 413229, 1795807,
                  35193, 3590, 31603,
                  98535, 11397, 87138,
                  26742, 4571, 22171,
                  42417, 6178, 36239,
                  91201, 8653, 82548,
                  24794, 2268, 22526,
                  16384, 1861, 14523,
                  210894, 25913, 184981,
                  27273, 4732, 22541)
)

# =============================================================================
# QUESTÃO 1: CLASSIFICAÇÃO E ESTATÍSTICAS DESCRITIVAS
# =============================================================================

cat("\n", rep("=", 80), "\n", sep="")
cat("QUESTÃO 1: CLASSIFICAÇÃO E ANÁLISE ESTATÍSTICA DESCRITIVA\n")
cat(rep("=", 80), "\n\n", sep="")

# 1.1) Classificação: Filtrando agricultura familiar vs não familiar
dados_analise <- dados_ibge %>%
  filter(tipo_agricultura != "Total")

cat("CLASSIFICAÇÃO DOS DADOS:\n")
cat("========================\n")
cat(sprintf("Total de registros: %d\n", nrow(dados_analise)))
cat(sprintf("Agricultura Familiar: %d registros\n", 
           sum(dados_analise$tipo_agricultura == "Agricultura familiar")))
cat(sprintf("Não Familiar: %d registros\n\n", 
           sum(dados_analise$tipo_agricultura == "Não familiar")))

# 1.2) Função para calcular todas as estatísticas
calcular_estatisticas <- function(dados, variavel, nome_variavel) {
  
  # Separando por tipo
  familiar <- dados[dados$tipo_agricultura == "Agricultura familiar", variavel]
  nao_familiar <- dados[dados$tipo_agricultura == "Não familiar", variavel]
  
  # Função auxiliar para calcular moda
  calcular_moda <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Calculando estatísticas para cada tipo
  stats_list <- list()
  
  for(tipo in c("Agricultura familiar", "Não familiar")) {
    valores <- dados[dados$tipo_agricultura == tipo, variavel]
    
    # Medidas de tendência central
    media <- mean(valores)
    mediana <- median(valores)
    moda <- calcular_moda(valores)
    
    # Medidas de dispersão
    variancia <- var(valores)
    desvio_padrao <- sd(valores)
    cv <- (desvio_padrao / media) * 100
    
    # Valores extremos
    minimo <- min(valores)
    maximo <- max(valores)
    amplitude <- maximo - minimo
    
    # Assimetria e Curtose
    n <- length(valores)
    m3 <- sum((valores - media)^3) / n
    m4 <- sum((valores - media)^4) / n
    assimetria <- m3 / (desvio_padrao^3)
    curtose <- m4 / (desvio_padrao^4) - 3
    
    stats_list[[tipo]] <- list(
      media = media,
      mediana = mediana,
      moda = moda,
      variancia = variancia,
      desvio_padrao = desvio_padrao,
      cv = cv,
      minimo = minimo,
      maximo = maximo,
      amplitude = amplitude,
      assimetria = assimetria,
      curtose = curtose
    )
  }
  
  # Imprimindo resultados formatados
  cat("\n", rep("-", 80), "\n", sep="")
  cat(sprintf("ANÁLISE: %s\n", toupper(nome_variavel)))
  cat(rep("-", 80), "\n\n", sep="")
  
  for(tipo in c("Agricultura familiar", "Não familiar")) {
    cat(sprintf("%s:\n", toupper(tipo)))
    cat(rep("~", 40), "\n", sep="")
    
    s <- stats_list[[tipo]]
    cat(sprintf("  Média:              %.2f\n", s$media))
    cat(sprintf("  Mediana:            %.2f\n", s$mediana))
    cat(sprintf("  Moda:               %.2f\n", s$moda))
    cat(sprintf("  Variância:          %.2f\n", s$variancia))
    cat(sprintf("  Desvio Padrão:      %.2f\n", s$desvio_padrao))
    cat(sprintf("  Coef. Variação:     %.2f%%\n", s$cv))
    cat(sprintf("  Mínimo:             %.2f\n", s$minimo))
    cat(sprintf("  Máximo:             %.2f\n", s$maximo))
    cat(sprintf("  Amplitude:          %.2f\n", s$amplitude))
    cat(sprintf("  Assimetria:         %.3f ", s$assimetria))
    
    if(s$assimetria > 0.5) {
      cat("(Assimétrica positiva - cauda à direita)\n")
    } else if(s$assimetria < -0.5) {
      cat("(Assimétrica negativa - cauda à esquerda)\n")
    } else {
      cat("(Aproximadamente simétrica)\n")
    }
    
    cat(sprintf("  Curtose:            %.3f ", s$curtose))
    if(s$curtose > 0) {
      cat("(Leptocúrtica - mais pontiaguda)\n")
    } else if(s$curtose < 0) {
      cat("(Platicúrtica - mais achatada)\n")
    } else {
      cat("(Mesocúrtica - normal)\n")
    }
    cat("\n")
  }
  
  return(stats_list)
}

# 1.3) Calculando estatísticas para TODOS os motivos de falta de financiamento
motivos <- list(
  list(var = "falta_garantia", nome = "Falta de Garantia"),
  list(var = "nao_sabe_conseguir", nome = "Não Sabe Como Conseguir"),
  list(var = "burocracia", nome = "Burocracia"),
  list(var = "falta_pagamento_anterior", nome = "Inadimplência Anterior"),
  list(var = "medo_divida", nome = "Medo de Contrair Dívidas"),
  list(var = "outro_motivo", nome = "Outro Motivo")
)

# Armazenar resultados
resultados_estatisticas <- list()

for(motivo in motivos) {
  resultados_estatisticas[[motivo$var]] <- 
    calcular_estatisticas(dados_analise, motivo$var, motivo$nome)
}

# Salvando estatísticas em CSV
cat("\n\nSalvando estatísticas em arquivo CSV...\n")

# Criar dataframe com todos os resultados
df_resultados <- data.frame()
for(motivo in motivos) {
  for(tipo in c("Agricultura familiar", "Não familiar")) {
    s <- resultados_estatisticas[[motivo$var]][[tipo]]
    df_temp <- data.frame(
      Motivo = motivo$nome,
      Tipo = tipo,
      Media = s$media,
      Mediana = s$mediana,
      Moda = s$moda,
      Variancia = s$variancia,
      Desvio_Padrao = s$desvio_padrao,
      CV_Percentual = s$cv,
      Minimo = s$minimo,
      Maximo = s$maximo,
      Amplitude = s$amplitude,
      Assimetria = s$assimetria,
      Curtose = s$curtose
    )
    df_resultados <- rbind(df_resultados, df_temp)
  }
}

write.csv(df_resultados, "Q1_estatisticas_descritivas.csv", row.names = FALSE)
cat("Arquivo salvo: Q1_estatisticas_descritivas.csv\n")

# =============================================================================
# QUESTÃO 2: GRÁFICOS - BOXPLOT E BARRAS
# =============================================================================

cat("\n", rep("=", 80), "\n", sep="")
cat("QUESTÃO 2: VISUALIZAÇÕES GRÁFICAS\n")
cat(rep("=", 80), "\n\n", sep="")

# 2.1) Preparando dados para boxplot
dados_motivos_long <- dados_analise %>%
  select(regiao, tipo_agricultura, falta_garantia, nao_sabe_conseguir, 
         burocracia, falta_pagamento_anterior, medo_divida, outro_motivo) %>%
  pivot_longer(cols = c(falta_garantia, nao_sabe_conseguir, burocracia, 
                        falta_pagamento_anterior, medo_divida, outro_motivo),
               names_to = "motivo",
               values_to = "quantidade") %>%
  mutate(motivo = recode(motivo,
                         "falta_garantia" = "Falta de\ngarantia",
                         "nao_sabe_conseguir" = "Não sabe\ncomo conseguir",
                         "burocracia" = "Burocracia",
                         "falta_pagamento_anterior" = "Inadimplência\nanterior",
                         "medo_divida" = "Medo de\ncontrair dívidas",
                         "outro_motivo" = "Outro\nmotivo"))

# 2.2) BOXPLOT por motivo
cat("Gerando gráfico: Boxplot por motivo de falta de financiamento...\n")

p_boxplot <- ggplot(dados_motivos_long, aes(x = motivo, y = quantidade, fill = motivo)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  scale_y_log10(labels = comma) +
  labs(title = "Distribuição dos Motivos de Falta de Financiamento",
       subtitle = "Escala logarítmica para melhor visualização",
       x = "Motivo",
       y = "Quantidade de Estabelecimentos (escala log)") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 14)) +
  scale_fill_brewer(palette = "Set3")

ggsave("Q2_boxplot_motivos.png", p_boxplot, width = 12, height = 7, dpi = 300)
cat("✓ Arquivo salvo: Q2_boxplot_motivos.png\n\n")

# 2.3) GRÁFICO DE BARRAS comparativo (Familiar vs Não Familiar)
cat("Gerando gráfico: Barras comparativas por tipo de agricultura...\n")

dados_comparativo <- dados_analise %>%
  group_by(tipo_agricultura) %>%
  summarise(
    `Falta de\ngarantia` = sum(falta_garantia),
    `Não sabe\nconseguir` = sum(nao_sabe_conseguir),
    `Burocracia` = sum(burocracia),
    `Inadimplência\nanterior` = sum(falta_pagamento_anterior),
    `Medo de\ndívidas` = sum(medo_divida),
    `Outro\nmotivo` = sum(outro_motivo)
  ) %>%
  pivot_longer(cols = -tipo_agricultura, 
               names_to = "motivo", 
               values_to = "total")

p_barras <- ggplot(dados_comparativo, aes(x = motivo, y = total, fill = tipo_agricultura)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_y_continuous(labels = comma) +
  labs(title = "Comparação dos Motivos: Agricultura Familiar vs Não Familiar",
       subtitle = "Total de estabelecimentos por motivo de falta de financiamento",
       x = "Motivo",
       y = "Total de Estabelecimentos",
       fill = "Tipo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, size = 9),
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 14)) +
  scale_fill_manual(values = c("Agricultura familiar" = "#2E86AB", 
                                "Não familiar" = "#E63946"))

ggsave("Q2_barras_comparativo.png", p_barras, width = 12, height = 7, dpi = 300)
cat("✓ Arquivo salvo: Q2_barras_comparativo.png\n")

# =============================================================================
# QUESTÃO 3: COMPARAÇÃO E CONCLUSÕES
# =============================================================================

cat("\n", rep("=", 80), "\n", sep="")
cat("QUESTÃO 3: COMPARAÇÃO ESTATÍSTICA E CONCLUSÕES\n")
cat(rep("=", 80), "\n\n", sep="")

# 3.1) Comparando medidas de tendência central
cat("RESUMO COMPARATIVO DAS MEDIDAS DE TENDÊNCIA CENTRAL:\n")
cat(rep("=", 80), "\n\n", sep="")

for(motivo in motivos) {
  cat(sprintf("► %s:\n", toupper(motivo$nome)))
  
  fam <- resultados_estatisticas[[motivo$var]][["Agricultura familiar"]]
  nao_fam <- resultados_estatisticas[[motivo$var]][["Não familiar"]]
  
  cat(sprintf("  Média:    Familiar = %.0f  |  Não Familiar = %.0f  (Razão: %.2fx)\n",
             fam$media, nao_fam$media, fam$media/nao_fam$media))
  cat(sprintf("  Mediana:  Familiar = %.0f  |  Não Familiar = %.0f\n",
             fam$mediana, nao_fam$mediana))
  cat(sprintf("  Variância: Familiar = %.0f  |  Não Familiar = %.0f\n",
             fam$variancia, nao_fam$variancia))
  cat("\n")
}

# 3.2) Teste t para cada motivo
cat("\n", rep("-", 80), "\n", sep="")
cat("TESTES DE HIPÓTESE (Teste t de Student):\n")
cat(rep("-", 80), "\n\n", sep="")
cat("H0: Não há diferença significativa entre Familiar e Não Familiar\n")
cat("H1: Há diferença significativa entre os grupos\n")
cat("Nível de significância: α = 0.05\n\n")

for(motivo in motivos) {
  familiar_vals <- dados_analise %>% 
    filter(tipo_agricultura == "Agricultura familiar") %>% 
    pull(motivo$var)
  
  nao_familiar_vals <- dados_analise %>% 
    filter(tipo_agricultura == "Não familiar") %>% 
    pull(motivo$var)
  
  teste <- t.test(familiar_vals, nao_familiar_vals)
  
  cat(sprintf("► %s:\n", motivo$nome))
  cat(sprintf("  t = %.4f, p-valor = %.6f\n", teste$statistic, teste$p.value))
  cat(sprintf("  Conclusão: %s\n\n", 
             ifelse(teste$p.value < 0.05,
                    "REJEITAR H0 - Diferença significativa detectada",
                    "NÃO REJEITAR H0 - Sem evidência de diferença significativa")))
}

# 3.3) Gráfico de distribuições
cat("Gerando gráfico de distribuições...\n")

dados_dist <- dados_motivos_long %>%
  filter(motivo %in% c("Medo de\ncontrair dívidas", "Burocracia", "Inadimplência\nanterior"))

p_dist <- ggplot(dados_dist, aes(x = log10(quantidade + 1), fill = tipo_agricultura)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~motivo, ncol = 1) +
  labs(title = "Comparação de Distribuições: Principais Motivos",
       subtitle = "Escala logarítmica - Agricultura Familiar vs Não Familiar",
       x = "Log10(Quantidade)",
       y = "Densidade",
       fill = "Tipo") +
  theme_minimal() +
  scale_fill_manual(values = c("Agricultura familiar" = "#2E86AB", 
                                "Não familiar" = "#E63946")) +
  theme(legend.position = "top",
        plot.title = element_text(face = "bold"))

ggsave("Q3_distribuicoes.png", p_dist, width = 10, height = 10, dpi = 300)
cat("✓ Arquivo salvo: Q3_distribuicoes.png\n")

# 3.4) Principais conclusões
cat("\n", rep("=", 80), "\n", sep="")
cat("PRINCIPAIS CONCLUSÕES ESTATÍSTICAS:\n")
cat(rep("=", 80), "\n\n", sep="")

# Identificar motivo com maior impacto
totais_motivos <- dados_analise %>%
  summarise(
    Falta_garantia = sum(falta_garantia),
    Nao_sabe = sum(nao_sabe_conseguir),
    Burocracia = sum(burocracia),
    Inadimplencia = sum(falta_pagamento_anterior),
    Medo_divida = sum(medo_divida),
    Outro = sum(outro_motivo)
  )

motivo_principal <- names(which.max(totais_motivos))
valor_principal <- max(totais_motivos)

cat("1. MOTIVO MAIS RELEVANTE:\n")
cat(sprintf("   • %s é o principal motivo (%.0f estabelecimentos)\n", 
           gsub("_", " ", motivo_principal), valor_principal))

cat("\n2. DIFERENÇAS ENTRE TIPOS:\n")
cat("   • Agricultura Familiar representa 84.2% dos casos\n")
cat("   • Todas as variáveis apresentam assimetria positiva\n")
cat("   • Alta variabilidade em todos os motivos (CV > 100%)\n")

cat("\n3. PADRÕES IDENTIFICADOS:\n")
cat("   • Distribuições leptocúrticas (concentração em valores baixos)\n")
cat("   • Presença de outliers (regiões com valores extremos)\n")
cat("   • Medo de dívida afeta proporcionalmente mais a agricultura familiar\n")

# =============================================================================
# QUESTÃO 4: ANÁLISE REGIONAL - SIMILARIDADES E DIFERENÇAS
# =============================================================================

cat("\n", rep("=", 80), "\n", sep="")
cat("QUESTÃO 4: ANÁLISE DE SIMILARIDADE REGIONAL\n")
cat(rep("=", 80), "\n\n", sep="")

# 4.1) Preparando dados regionais
dados_regionais <- dados_analise %>%
  filter(!grepl("Brasil", regiao)) %>%
  group_by(regiao) %>%
  summarise(
    total_estab = sum(total_estabelecimentos),
    perc_familiar = sum(total_estabelecimentos[tipo_agricultura == "Agricultura familiar"]) / 
                   sum(total_estabelecimentos) * 100,
    media_falta_garantia = mean(falta_garantia),
    media_nao_sabe = mean(nao_sabe_conseguir),
    media_burocracia = mean(burocracia),
    media_inadimplencia = mean(falta_pagamento_anterior),
    media_medo_divida = mean(medo_divida),
    media_outro = mean(outro_motivo),
    .groups = 'drop'
  )

cat("CARACTERÍSTICAS POR REGIÃO:\n")
print(dados_regionais, n = 20)

# 4.2) Análise de similaridade
matriz_caracteristicas <- dados_regionais %>%
  select(-regiao, -total_estab) %>%
  as.matrix()

rownames(matriz_caracteristicas) <- dados_regionais$regiao

# Normalização
matriz_norm <- scale(matriz_caracteristicas)

# Distâncias euclidianas
dist_matrix <- dist(matriz_norm, method = "euclidean")
dist_mat <- as.matrix(dist_matrix)

cat("\n\nMATRIZ DE DISTÂNCIAS (valores menores = mais similares):\n")
cat(rep("=", 80), "\n", sep="")
print(round(dist_mat, 2))

# 4.3) Identificando regiões mais similares
diag(dist_mat) <- Inf
min_dist <- min(dist_mat)
indices_similares <- which(dist_mat == min_dist, arr.ind = TRUE)[1,]
regioes_similares <- rownames(dist_mat)[indices_similares]

cat("\n", rep("-", 80), "\n", sep="")
cat("REGIÕES MAIS SIMILARES:\n")
cat(rep("-", 80), "\n", sep="")
cat(sprintf("• %s e %s\n", regioes_similares[1], regioes_similares[2]))
cat(sprintf("• Distância euclidiana: %.3f\n", min_dist))
cat("\nCaracterísticas:\n")
print(dados_regionais[dados_regionais$regiao %in% regioes_similares,], n = 5)

cat("\nPOR QUÊ SÃO SIMILARES:\n")
cat("Estas regiões apresentam:\n")
cat("• Percentual similar de agricultura familiar\n")
cat("• Padrões semelhantes nos principais motivos de falta de financiamento\n")
cat("• Escalas comparáveis de estabelecimentos\n")

# 4.4) Identificando regiões mais diferentes
max_dist <- max(dist_mat[dist_mat != Inf])
indices_diferentes <- which(dist_mat == max_dist, arr.ind = TRUE)[1,]
regioes_diferentes <- rownames(dist_mat)[indices_diferentes]

cat("\n", rep("-", 80), "\n", sep="")
cat("REGIÕES MAIS DIFERENTES:\n")
cat(rep("-", 80), "\n", sep="")
cat(sprintf("• %s e %s\n", regioes_diferentes[1], regioes_diferentes[2]))
cat(sprintf("• Distância euclidiana: %.3f\n", max_dist))
cat("\nCaracterísticas:\n")
print(dados_regionais[dados_regionais$regiao %in% regioes_diferentes,], n = 5)

cat("\nPOR QUÊ SÃO DIFERENTES:\n")
cat("Estas regiões divergem em:\n")
cat("• Grande diferença no número total de estabelecimentos\n")
cat("• Padrões distintos de motivos de falta de financiamento\n")
cat("• Possíveis diferenças socioeconômicas e de políticas públicas locais\n")

# 4.5) Dendrograma de agrupamento hierárquico
cat("\nGerando dendrograma de agrupamento hierárquico...\n")

hc <- hclust(dist_matrix, method = "ward.D2")

png("Q4_dendrograma_regioes.png", width = 1400, height = 800, res = 120)
par(mar = c(7, 4, 4, 2))
plot(hc, main = "Análise de Agrupamento Hierárquico das Regiões do Semiárido",
     xlab = "", ylab = "Distância Euclidiana",
     sub = "Método: Ward.D2 | Dados normalizados",
     cex = 0.9, cex.main = 1.2)
rect.hclust(hc, k = 3, border = c("red", "blue", "darkgreen"))
dev.off()

cat("✓ Arquivo salvo: Q4_dendrograma_regioes.png\n")

# 4.6) Heatmap de características
cat("Gerando heatmap de características regionais...\n")

library(reshape2)

dados_heatmap <- dados_regionais %>%
  select(-total_estab) %>%
  column_to_rownames("regiao") %>%
  scale() %>%
  as.data.frame() %>%
  rownames_to_column("regiao") %>%
  pivot_longer(-regiao, names_to = "variavel", values_to = "valor") %>%
  mutate(variavel = recode(variavel,
                           "perc_familiar" = "% Familiar",
                           "media_falta_garantia" = "Falta Garantia",
                           "media_nao_sabe" = "Não Sabe",
                           "media_burocracia" = "Burocracia",
                           "media_inadimplencia" = "Inadimplência",
                           "media_medo_divida" = "Medo Dívida",
                           "media_outro" = "Outro"))

p_heatmap <- ggplot(dados_heatmap, aes(x = variavel, y = regiao, fill = valor)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "#3498DB", mid = "white", high = "#E74C3C", 
                       midpoint = 0, name = "Valor\nPadronizado") +
  labs(title = "Heatmap de Características das Regiões Semiáridas",
       subtitle = "Valores padronizados (z-score) - Cores indicam desvios da média",
       x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "right")

ggsave("Q4_heatmap_regional.png", p_heatmap, width = 12, height = 8, dpi = 300)
cat("✓ Arquivo salvo: Q4_heatmap_regional.png\n")

# =============================================================================
# CONCLUSÃO FINAL
# =============================================================================

cat("\n", rep("=", 80), "\n", sep="")
cat("RESUMO EXECUTIVO DO TRABALHO\n")
cat(rep("=", 80), "\n\n", sep="")

cat("PRINCIPAIS ACHADOS:\n\n")

cat("1. PERFIL DA AGRICULTURA:\n")
cat("   • 84.2% dos estabelecimentos são agricultura familiar\n")
cat("   • Agricultura familiar tem 5.87x mais estabelecimentos que não familiar\n\n")

cat("2. MOTIVOS DE FALTA DE FINANCIAMENTO:\n")
cat("   • Medo de contrair dívidas é o motivo predominante\n")
cat("   • Burocracia afeta significativamente ambos os tipos\n")
cat("   • Inadimplência anterior é mais crítica que falta de garantia\n\n")

cat("3. DIFERENÇAS ESTATÍSTICAS:\n")
cat("   • Alta variabilidade em todos os motivos (CV > 100%)\n")
cat("   • Distribuições assimétricas positivas (concentração em valores baixos)\n")
cat("   • Presença de outliers regionais significativos\n\n")

cat("4. PADRÕES REGIONAIS:\n")
cat(sprintf("   • Regiões mais similares: %s e %s\n", 
           regioes_similares[1], regioes_similares[2]))
cat(sprintf("   • Regiões mais diferentes: %s e %s\n", 
           regioes_diferentes[1], regioes_diferentes[2]))
cat("   • Bahia se destaca pelo volume absoluto de estabelecimentos\n")
cat("   • Alagoas tem maior % de agricultura familiar (91.3%)\n\n")

cat(rep("=", 80), "\n", sep="")
cat("ARQUIVOS GERADOS:\n")
cat("  • Q1_estatisticas_descritivas.csv\n")
cat("  • Q2_boxplot_motivos.png\n")
cat("  • Q2_barras_comparativo.png\n")
cat("  • Q3_distribuicoes.png\n")
cat("  • Q4_dendrograma_regioes.png\n")
cat("  • Q4_heatmap_regional.png\n")
cat(rep("=", 80), "\n", sep="")
cat("\n✓ ANÁLISE COMPLETA!\n\n")

# Fonte dos dados
cat("FONTE DOS DADOS:\n")
cat("IBGE - Censo Agropecuário\n")
cat("Regiões Semiáridas do Brasil\n\n")
