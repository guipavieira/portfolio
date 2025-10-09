# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", "tidyverse", "ggrepel", "knitr", "kableExtra", 
             "reshape2", "PerformanceAnalytics", "psych", "Hmisc", 
             "readxl", "cluster", "factoextra", "ggplot2", "geobr", "sf")

if (sum(as.numeric(!pacotes %in% installed.packages())) != 0) {
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for (i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()
  }
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

# Carregando a base de dados
estat.eleitorado1 <- read.csv("H:\\O meu disco\\MBA DSA\\TCC\\escolaridade_eleitores.csv", sep = ";", dec = ".", encoding = "latin-1")

# Limpeza e preparação dos dados
colnames(estat.eleitorado1) <- c("UF", "Escolaridade", "Ano", "Quantidade")

# Remover duplicatas e limpar a categoria "NÃO INFORMADO"
estat.eleitorado1 <- estat.eleitorado1 %>% distinct() %>%
  filter(Escolaridade != "NÃO INFORMADO")

# Filtrar os dados para os anos de 2014 e 2024
dados_comparacao <- estat.eleitorado1 %>%
  filter(Ano %in% c(2014, 2024))

# Calcular a soma total de eleitores por estado e ano
total_eleitores <- dados_comparacao %>%
  group_by(UF, Ano) %>%
  summarise(Total = sum(Quantidade, na.rm = TRUE), .groups = 'drop')

# Calcular a proporção de eleitores para cada nível de escolaridade
proporcao_escolaridade <- dados_comparacao %>%
  left_join(total_eleitores, by = c("UF", "Ano")) %>%
  mutate(Proporcao = Quantidade / Total) %>%
  select(UF, Escolaridade, Ano, Proporcao)

# Organizar os dados em formato largo (wide) para comparação
dados_pareados <- proporcao_escolaridade %>%
  pivot_wider(names_from = Ano, values_from = Proporcao, names_prefix = "Ano_") %>%
  filter(!is.na(Ano_2014) & !is.na(Ano_2024))

# Calcular a diferença entre 2014 e 2024 para cada estado e escolaridade
melhorias_estados <- dados_pareados %>%
  mutate(diferenca = Ano_2024 - Ano_2014) %>%
  arrange(desc(diferenca))

# Transformar os dados em formato largo (wide), onde cada estado é uma linha e as categorias de escolaridade são colunas
dados_wide <- melhorias_estados %>%
  select(UF, Escolaridade, diferenca) %>%
  pivot_wider(names_from = Escolaridade, values_from = diferenca)

# Aplicar a normalização Z-score aos dados
dados_para_cluster_zscore <- scale(dados_wide[, c("ANALFABETO", "LÊ E ESCREVE", 
                                                  "ENSINO FUNDAMENTAL COMPLETO", 
                                                  "ENSINO FUNDAMENTAL INCOMPLETO", 
                                                  "ENSINO MÉDIO COMPLETO", 
                                                  "ENSINO MÉDIO INCOMPLETO", 
                                                  "SUPERIOR COMPLETO", 
                                                  "SUPERIOR INCOMPLETO")])

# Adicionar a coluna de UF à tabela normalizada para facilitar o uso nas análises
dados_cluster_zscore <- data.frame(UF = dados_wide$UF, dados_para_cluster_zscore)

#-------------------CLUSTERIZAÇÃO K-MEANS---------------------

# Método de Elbow para encontrar o número ideal de clusters
set.seed(123)
fviz_nbclust(dados_cluster_zscore[, -1], kmeans, method = "wss") +
  labs(title = "Método de Elbow para Seleção do Número de Clusters",
       x = "Número de Clusters", 
       y = "Soma das Distâncias Quadráticas Internas (WCSS)") +
  theme_minimal()

# Definir o número de clusters com base no método de Elbow (exemplo: 4)
num_clusters <- 4

# Aplicar K-means nos dados normalizados
cluster_kmeans <- kmeans(dados_cluster_zscore[, -1], centers = num_clusters)

# Adicionar o resultado da clusterização K-means de volta aos dados
dados_wide$cluster_kmeans <- as.factor(cluster_kmeans$cluster)
dados_cluster_zscore$cluster_kmeans <- as.factor(cluster_kmeans$cluster)

# Teste de Silhouette para K-means
# Calcular a matriz de dissimilaridade
dist_kmeans <- dist(dados_cluster_zscore[, -1])  # Remover a coluna UF

# Calcular o índice de Silhouette
silhouette_kmeans <- silhouette(cluster_kmeans$cluster, dist_kmeans)

# Visualizar o gráfico de Silhouette
fviz_silhouette(silhouette_kmeans) +
  labs(title = "Gráfico de Silhouette - K-means")

# Calcular a média do índice de Silhouette
media_silhouette_kmeans <- mean(silhouette_kmeans[, "sil_width"])
cat("Média do Índice de Silhouette para K-means: ", media_silhouette_kmeans, "\n")


#-------------------CLUSTERIZAÇÃO HIERÁRQUICA------------------

# Calcular a matriz de dissimilaridade usando distância Euclidiana
dist_matrix <- dist(dados_cluster_zscore[, -1], method = "euclidean")

# Aplicar o método hierárquico aglomerativo com COMPLETE Linkage
hc <- hclust(dist_matrix, method = "complete")

# Visualizar o dendrograma
plot(hc, labels = dados_wide$UF, main = "Dendrograma da Clusterização Hierárquica", sub = "", xlab = "", cex = 0.7)

# Cortar o dendrograma em 5 clusters e descobrir a altura do corte
clusters_hierarquicos <- cutree(hc, k = 5)

# Adicionar linha no gráfico na altura do corte
rect.hclust(hc, k = 5, border = "red")  # Desenhar retângulos ao redor dos clusters

# Para saber a altura exata em que o dendrograma foi cortado
height_cut <- hc$height[length(hc$height) - (5 - 1)]
abline(h = height_cut, col = "blue", lty = 2)  # Traçar a linha na altura do corte
cat("A altura de corte para os 5 clusters foi:", height_cut, "\n")

# Adicionar o resultado da clusterização hierárquica de volta aos dados
dados_wide$cluster_hierarquico <- as.factor(clusters_hierarquicos)
dados_cluster_zscore$cluster_hierarquico <- as.factor(clusters_hierarquicos)

# Teste de Silhouette para Clusterização Hierárquica
# Calcular o índice de Silhouette
silhouette_hierarquico <- silhouette(clusters_hierarquicos, dist_matrix)

# Visualizar o gráfico de Silhouette
fviz_silhouette(silhouette_hierarquico) +
  labs(title = "Gráfico de Silhouette - Clusterização Hierárquica")

# Calcular a média do índice de Silhouette
media_silhouette_hierarquico <- mean(silhouette_hierarquico[, "sil_width"])
cat("Média do Índice de Silhouette para a Clusterização Hierárquica: ", media_silhouette_hierarquico, "\n")


#----------------ANOVA PARA CLUSTERS (Z-SCORE)-----------------

# Realizar ANOVA para cada variável em K-means com Z-score
anova_kmeans_analfabeto <- aov(ANALFABETO ~ cluster_kmeans, data = dados_cluster_zscore)
anova_kmeans_le_escreve <- aov(`LÊ.E.ESCREVE` ~ cluster_kmeans, data = dados_cluster_zscore)
anova_kmeans_fundamental_completo <- aov(`ENSINO.FUNDAMENTAL.COMPLETO` ~ cluster_kmeans, data = dados_cluster_zscore)
anova_kmeans_fundamental_incompleto <- aov(`ENSINO.FUNDAMENTAL.INCOMPLETO` ~ cluster_kmeans, data = dados_cluster_zscore)
anova_kmeans_medio_completo <- aov(`ENSINO.MÉDIO.COMPLETO` ~ cluster_kmeans, data = dados_cluster_zscore)
anova_kmeans_medio_incompleto <- aov(`ENSINO.MÉDIO.INCOMPLETO` ~ cluster_kmeans, data = dados_cluster_zscore)
anova_kmeans_superior_completo <- aov(`SUPERIOR.COMPLETO` ~ cluster_kmeans, data = dados_cluster_zscore)
anova_kmeans_superior_incompleto <- aov(`SUPERIOR.INCOMPLETO` ~ cluster_kmeans, data = dados_cluster_zscore)

# Realizar ANOVA para cada variável em hierárquica com Z-score
anova_hierarquico_analfabeto <- aov(ANALFABETO ~ cluster_hierarquico, data = dados_cluster_zscore)
anova_hierarquico_le_escreve <- aov(`LÊ.E.ESCREVE` ~ cluster_hierarquico, data = dados_cluster_zscore)
anova_hierarquico_fundamental_completo <- aov(`ENSINO.FUNDAMENTAL.COMPLETO` ~ cluster_hierarquico, data = dados_cluster_zscore)
anova_hierarquico_fundamental_incompleto <- aov(`ENSINO.FUNDAMENTAL.INCOMPLETO` ~ cluster_hierarquico, data = dados_cluster_zscore)
anova_hierarquico_medio_completo <- aov(`ENSINO.MÉDIO.COMPLETO` ~ cluster_hierarquico, data = dados_cluster_zscore)
anova_hierarquico_medio_incompleto <- aov(`ENSINO.MÉDIO.INCOMPLETO` ~ cluster_hierarquico, data = dados_cluster_zscore)
anova_hierarquico_superior_completo <- aov(`SUPERIOR.COMPLETO` ~ cluster_hierarquico, data = dados_cluster_zscore)
anova_hierarquico_superior_incompleto <- aov(`SUPERIOR.INCOMPLETO` ~ cluster_hierarquico, data = dados_cluster_zscore)


#----------------VISUALIZAÇÃO RESULTADOS DAS ANOVAS--------------

# Tabela com os valores de F e p-valores para K-means e Hierárquico
resultados_anova <- data.frame(
  Variável = c("ANALFABETO", "LÊ.E.ESCREVE", "ENSINO.FUNDAMENTAL.COMPLETO", "ENSINO.FUNDAMENTAL.INCOMPLETO", 
               "ENSINO.MÉDIO.COMPLETO", "ENSINO.MÉDIO.INCOMPLETO", "SUPERIOR.COMPLETO", "SUPERIOR.INCOMPLETO"),
  
  # F-value para K-means
  F_value_kmeans = c(
    summary(anova_kmeans_analfabeto)[[1]]$`F value`[1],
    summary(anova_kmeans_le_escreve)[[1]]$`F value`[1],
    summary(anova_kmeans_fundamental_completo)[[1]]$`F value`[1],
    summary(anova_kmeans_fundamental_incompleto)[[1]]$`F value`[1],
    summary(anova_kmeans_medio_completo)[[1]]$`F value`[1],
    summary(anova_kmeans_medio_incompleto)[[1]]$`F value`[1],
    summary(anova_kmeans_superior_completo)[[1]]$`F value`[1],
    summary(anova_kmeans_superior_incompleto)[[1]]$`F value`[1]
  ),
  
  # p-value para K-means
  p_value_kmeans = c(
    summary(anova_kmeans_analfabeto)[[1]]$`Pr(>F)`[1],
    summary(anova_kmeans_le_escreve)[[1]]$`Pr(>F)`[1],
    summary(anova_kmeans_fundamental_completo)[[1]]$`Pr(>F)`[1],
    summary(anova_kmeans_fundamental_incompleto)[[1]]$`Pr(>F)`[1],
    summary(anova_kmeans_medio_completo)[[1]]$`Pr(>F)`[1],
    summary(anova_kmeans_medio_incompleto)[[1]]$`Pr(>F)`[1],
    summary(anova_kmeans_superior_completo)[[1]]$`Pr(>F)`[1],
    summary(anova_kmeans_superior_incompleto)[[1]]$`Pr(>F)`[1]
  ),
  
  # F-value para Clusterização Hierárquica
  F_value_hierarquico = c(
    summary(anova_hierarquico_analfabeto)[[1]]$`F value`[1],
    summary(anova_hierarquico_le_escreve)[[1]]$`F value`[1],
    summary(anova_hierarquico_fundamental_completo)[[1]]$`F value`[1],
    summary(anova_hierarquico_fundamental_incompleto)[[1]]$`F value`[1],
    summary(anova_hierarquico_medio_completo)[[1]]$`F value`[1],
    summary(anova_hierarquico_medio_incompleto)[[1]]$`F value`[1],
    summary(anova_hierarquico_superior_completo)[[1]]$`F value`[1],
    summary(anova_hierarquico_superior_incompleto)[[1]]$`F value`[1]
  ),
  
  # p-value para Clusterização Hierárquica
  p_value_hierarquico = c(
    summary(anova_hierarquico_analfabeto)[[1]]$`Pr(>F)`[1],
    summary(anova_hierarquico_le_escreve)[[1]]$`Pr(>F)`[1],
    summary(anova_hierarquico_fundamental_completo)[[1]]$`Pr(>F)`[1],
    summary(anova_hierarquico_fundamental_incompleto)[[1]]$`Pr(>F)`[1],
    summary(anova_hierarquico_medio_completo)[[1]]$`Pr(>F)`[1],
    summary(anova_hierarquico_medio_incompleto)[[1]]$`Pr(>F)`[1],
    summary(anova_hierarquico_superior_completo)[[1]]$`Pr(>F)`[1],
    summary(anova_hierarquico_superior_incompleto)[[1]]$`Pr(>F)`[1]
  )
)

# Visualizar os resultados em forma de tabela
print(resultados_anova)

# Exportar os resultados para CSV
#write.csv2(resultados_anova, "resultados_anova_clusters.csv", row.names = FALSE)


#-------------------MÉDIAS POR CLUSTER------------------------
#----------------VISUALIZAÇÃO DAS MÉDIAS POR CLUSTER----------------

# Calcular as médias das variáveis por cluster (para K-means e Hierárquico)
medias_por_cluster_kmeans <- dados_wide %>%
  group_by(cluster_kmeans) %>%
  summarise(
    `media analfabeto` = mean(ANALFABETO, na.rm = TRUE),
    `media lê e escreve` = mean(`LÊ E ESCREVE`, na.rm = TRUE),
    `media ensino fundamental completo` = mean(`ENSINO FUNDAMENTAL COMPLETO`, na.rm = TRUE),
    `media ensino fundamental incompleto` = mean(`ENSINO FUNDAMENTAL INCOMPLETO`, na.rm = TRUE),
    `media ensino médio completo` = mean(`ENSINO MÉDIO COMPLETO`, na.rm = TRUE),
    `media ensino médio incompleto` = mean(`ENSINO MÉDIO INCOMPLETO`, na.rm = TRUE),
    `media superior completo` = mean(`SUPERIOR COMPLETO`, na.rm = TRUE),
    `media superior incompleto` = mean(`SUPERIOR INCOMPLETO`, na.rm = TRUE)
  )

medias_por_cluster_hierarquico <- dados_wide %>%
  group_by(cluster_hierarquico) %>%
  summarise(
    `media analfabeto` = mean(ANALFABETO, na.rm = TRUE),
    `media lê e escreve` = mean(`LÊ E ESCREVE`, na.rm = TRUE),
    `media ensino fundamental completo` = mean(`ENSINO FUNDAMENTAL COMPLETO`, na.rm = TRUE),
    `media ensino fundamental incompleto` = mean(`ENSINO FUNDAMENTAL INCOMPLETO`, na.rm = TRUE),
    `media ensino médio completo` = mean(`ENSINO MÉDIO COMPLETO`, na.rm = TRUE),
    `media ensino médio incompleto` = mean(`ENSINO MÉDIO INCOMPLETO`, na.rm = TRUE),
    `media superior completo` = mean(`SUPERIOR COMPLETO`, na.rm = TRUE),
    `media superior incompleto` = mean(`SUPERIOR INCOMPLETO`, na.rm = TRUE)
  )

# Visualização das médias por cluster (K-means)
medias_por_cluster_kmeans %>%
  pivot_longer(cols = -cluster_kmeans, names_to = "Variável", values_to = "Média") %>%
  ggplot(aes(x = Média, y = cluster_kmeans, fill = Variável)) +  # Alterado para y = cluster_kmeans
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Médias das Variáveis por Cluster (K-means)", 
       x = "Média", y = "Cluster K-means") +  # Atualizado os rótulos
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Mantenha o texto do eixo y na horizontal

# Visualização das médias por cluster (Hierárquico)
medias_por_cluster_hierarquico %>%
  pivot_longer(cols = -cluster_hierarquico, names_to = "Variável", values_to = "Média") %>%
  ggplot(aes(x = Média, y = cluster_hierarquico, fill = Variável)) +  # Alterado para y = cluster_hierarquico
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Médias das Variáveis por Cluster (Hierárquico)", 
       x = "Média", y = "Cluster Hierárquico") +  # Atualizado os rótulos
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))  # Mantenha o texto do eixo y na horizontal



########_______ESTADOS POR CLUSTER--------------------------------

# Listar os estados de cada cluster K-means
estados_por_cluster_kmeans <- dados_wide %>%
  group_by(cluster_kmeans) %>%
  summarise(Estados = paste(UF, collapse = ", "))

# Exibir os estados de cada cluster K-means
print("Estados por cluster (K-means):")
print(estados_por_cluster_kmeans)

# Listar os estados de cada cluster hierárquico
estados_por_cluster_hierarquico <- dados_wide %>%
  group_by(cluster_hierarquico) %>%
  summarise(Estados = paste(UF, collapse = ", "))

# Exibir os estados de cada cluster hierárquico
print("Estados por cluster (Hierárquico):")
print(estados_por_cluster_hierarquico)


# Dataframe original com clusters (uma linha por cluster com estados separados por vírgula)
estados_por_cluster_hierarquico <- tibble::tribble(
  ~cluster, ~estados,
  "1", "AM, BA, CE, PA",
  "2", "MA, PE, PB, TO, PI, AC, RN",
  "3", "SP, PR, MT, MS, SC",
  "4", "MG, GO, RS, RO, ES, RJ",
  "5", "AP, AL, SE, RR"
)

# Separar os estados em linhas individuais e renomear a coluna para 'estado'
estados_cluster <- estados_por_cluster_hierarquico %>%
  separate_rows(estados, sep = ",\\s*") %>%
  rename(estado = estados)

# Carregar o shapefile dos estados do Brasil
brasil <- read_state(year = 2020, showProgress = FALSE)

# Unir os dados do shapefile com os clusters, usando a coluna de siglas
brasil <- brasil %>%
  left_join(estados_cluster, by = c("abbrev_state" = "estado"))

# Gerar o mapa com ggplot2, colorindo os estados de acordo com o cluster
ggplot(brasil) +
  geom_sf(aes(fill = factor(cluster)), color = "white", size = 0.3) +
  labs(title = "Mapa do Brasil - Clusters dos Estados", fill = "Cluster") +
  theme_minimal()