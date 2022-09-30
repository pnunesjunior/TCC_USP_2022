#Versão Final - TCC - Data Science & Analytics

# Adesão de crédito financeiro após o início da pandemia, interferiu nas vendas efetuadas no e-commerce?

# Objetivo:
#O presente trabalho tem como objetivo desenvolver e documentar um estudo,
#realizado por meio do modelo de Diff-in-Diff, sobre o efeito causado nas vendas
#geradas pelo canal de e-commerce aos vendedores que optaram pela aquisição de
#crédito após o início da pandemia. Portanto, foram coletados dados de quantidades
#vendidas antes e depois do início da pandemia, e dados de características de cada
#vendedor na plataforma, como por exemplo, UF do vendedor, se tem assessoria, vertical de vendas,
#etc para execução da metodologia.
#Baseado na quantidade de vendas geradas e nas características de cada vendedor,
#verificou-se se o efeito causado sobre elas no período  pós aquisição do crédito financeiro.


########################################################################################
# Instalção dos pacotes
########################################################################################
install.packages("tidyverse")
install.packages("scales")
install.packages("readxl")
install.packages("broom")
install.packages("modelsummary")
# install.packages("AER")

library(tidyverse)
library(scales)
library(readxl)
library(broom)
library(modelsummary)

########################################################################################
# Carregar Banco de Dados
########################################################################################

# Premissas para coletas dos dados
# Vendas geradas de 01/01/2018 á 31/05/2022 -> 26 meses antes e depois do inicio da pandemia.
# Emprestimo tomados de 01/03/2020 à 31/12/2020 (inicio da Pandemia até o fim do mesmo ano)

# Cod_Vendedor --> ID do Vendedor
# Vertical_Venda --> Vertical de Venda
# Segmento_Vendas --> Long Tail, Mid Tail ou Loja Oficial
# Tipo_Assessoria --> Acessorado ou Não Acessorado
# UF_Vendedor --> UF de Venda
# Emprestimo --> Se pegou ou não pegou emprestimo no período considerado na premissa
# Periodo_Venda --> Se observação pertence antes ou depois do tratamento
# Tipo_Documento --> Troca de PF para PJ após emprestimo. A dado 0 siginifica que não trocou e 1 que houve a troca.
# Total_Itens --> Total de Anúncios disponíveis no mês da venda.
# Total_Pedidos --> Total de Pedido

dataset_diff <- read.csv("Base_TCC/Seller_Diff_Diff.csv")

dataset_diff <- dataset_diff %>%
  mutate(Cod_Vendedor=as.character(as.numeric(Cod_Vendedor)))


########################################################################################
## normalizando a quantidade de pedidos e pedidos pelo Log10
########################################################################################

# Para retirar qualquer viés sobre sobre unidade de medida da quantidade
# de pedidos vendidos e itens ativos, normalizamos o valor dessas váriaveis por Log10.


dataset_diff["log_pedidos"] <- log(1+dataset_diff$Total_Pedidos)

dataset_diff["log_itens"] <- log(1+dataset_diff$Total_Itens)

summary(dataset_diff)

########################################################################################
## Análise dos Grupos
########################################################################################


table(dataset_diff$Emprestimo) # 0 não tem emprestimo / 1 tem emprestimo

table(dataset_diff$Periodo_Venda) # 0 Antes do inicio da pandemia / 1 depois do inicio da pandemia


########################################################################################
## Analise sobre o volume de vendas
########################################################################################

# Total de Vendas
ggplot(data = dataset_diff, mapping = aes(x = log_pedidos)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 0) +
  labs(title = "Difference in Differences",
       subtitle = "Histograma por total de vendas")+
  theme(plot.title = element_text(size = 20, color = "Black"),
        plot.subtitle = element_text(size = 10, color = "Black"))+
  xlab("Total Vendas")+
  ylab("Total Sellers ")

# Total por grupo de controle e tratado
ggplot(data = dataset_diff, mapping = aes(x = log_pedidos)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 0) +
  labs(title = "Difference in Differences",
       subtitle = "Histograma por total de vendas - Grupo Controle vs Tratado")+
  theme(plot.title = element_text(size = 20, color = "Black"),
        plot.subtitle = element_text(size = 10, color = "Black"))+
  xlab("Total Vendas")+
  ylab("Total Sellers")+
  facet_wrap(vars(Emprestimo))

# Total por período do efeito
ggplot(data = dataset_diff, mapping = aes(x = log_pedidos)) +
  geom_histogram(binwidth = 1, color = "white", boundary = 0) +
  labs(title = "Difference in Differences",
       subtitle = "Histograma por total de vendas - Por período")+
  theme(plot.title = element_text(size = 20, color = "Black"),
        plot.subtitle = element_text(size = 10, color = "Black"))+
  xlab("Total Vendas")+
  ylab("Total Sellers")+
  facet_wrap(vars(Periodo_Venda))

########################################################################################
## Analise
########################################################################################

plot_data <- dataset_diff %>%
  mutate(Seller_Emprestimo = factor(Emprestimo, labels = c("s/ Emprestimo", "c/ Emprestimo")),
         Efeito_Tratamento = factor(Periodo_Venda, labels = c("Até Inicio Pandemia", "Depois Inicio Pandemia"))) %>%
  group_by(Efeito_Tratamento,Seller_Emprestimo) %>%
  summarize(mean_pedidos = mean(log_pedidos),
            se_pedidos = sd(log_pedidos) / sqrt(n()),
            upper = mean_pedidos + (1.96 * se_pedidos),
            lower = mean_pedidos + (-1.96 * se_pedidos))

ggplot(plot_data, aes(x = Seller_Emprestimo, y = mean_pedidos)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), color = "darkgreen", size = 1) +
  labs(title = "Difference in Differences",
       subtitle = "Indicador da média de Vendas")+
  theme(plot.title = element_text(size = 20, color = "Black"),
        plot.subtitle = element_text(size = 10, color = "Black"))+
  xlab("Período das Vendas")+
  ylab("Média de Orders")+
  facet_wrap(vars(Efeito_Tratamento))

# Dispersão de Vendas entre os Sellers  que tomaram ou não emprestimo antes e depois da Pandemia.
# Indicadores está sobre médida de log dos pedidos.

########################################################################################
## Analise
########################################################################################


ggplot(plot_data, aes(x = Efeito_Tratamento, y = mean_pedidos, color = Seller_Emprestimo)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  labs(title = "Difference in Differences",
       subtitle = "Indicador da média de Vendas")+
  theme(plot.title = element_text(size = 20, color = "Black"),
        plot.subtitle = element_text(size = 10, color = "Black"))+
  xlab("Período das Vendas")+
  ylab("Média de Orders")+
  scale_x_discrete("Período das Vendas", labels = c("Até Inicio Pandemia", "Depois Inicio Pandemia"))+
  scale_colour_discrete(name="Grupo Vendedores", labels=c("Vendedor s/ Emprestimo", "Vendedor c/ Emprestimo"))+
  geom_line(aes(group = Seller_Emprestimo))

########################################################################################
## Diff - in - Diff Calculo manual
########################################################################################

diffs <- dataset_diff %>%
  group_by(Periodo_Venda,Emprestimo) %>%
  summarize(mean_orders = mean(log_pedidos),
            med_pedidos_vendidos = mean(Total_Pedidos))
diffs


#

# Grupo de Tratamento - Pegou Emprestimo
# Grupo de Controle - Não pegou emprestimo

# Media de Vendas de Vendas de Sellers que pegaram emprestimo antes do inicio da pandemia - Grupo de Tratamento
antes_grupo_tratamento <- diffs %>%
  filter(Periodo_Venda == 0, Emprestimo == 1) %>%
  pull(mean_orders)

# Media de Vendas de Vendas de Sellers que não pegaram emprestimo antes do inicio da pandemia - Grupo de Controle
antes_grupo_controle <- diffs %>%
  filter(Periodo_Venda == 0, Emprestimo == 0) %>%
  pull(mean_orders)

# Media de Vendas deSellers que pegaram emprestimo depois do inicio da pandemia - Grupo de Tratamento
depois_grupo_tratamento <- diffs %>%
  filter(Periodo_Venda == 1, Emprestimo == 1) %>%
  pull(mean_orders)

# Media de Vendas de Sellers que não pegaram emprestimo depois do inicio da pandemia - Grupo de Controle
depois_grupo_controle <- diffs %>%
  filter(Periodo_Venda == 1, Emprestimo == 0) %>%
  pull(mean_orders)

# Dif entre as medias de vendas antes e depois do inicio da pandemia - Grupo de tratamento
diff_grupo_tratamento_antes_depois <- depois_grupo_tratamento - antes_grupo_tratamento
diff_grupo_tratamento_antes_depois

# Dif entre as medias de vendas antes e depois do inicio da pandemia - Grupo de Controle
diff_grupo_controle_antes_depois <- depois_grupo_controle - antes_grupo_controle
diff_grupo_controle_antes_depois

# Dif entre as medias de vendas entre os grupos
diff_diff <- diff_grupo_tratamento_antes_depois - diff_grupo_controle_antes_depois
diff_diff

########################################################################################
## Diff - in - Diff Gráfico
########################################################################################

ggplot(diffs, aes(x = as.factor(Periodo_Venda), y = mean_orders,
                  color = as.factor(Emprestimo))) +
  geom_point() +
  labs(title = "Difference in Differences",
       subtitle = "Efeito da aquisição de emprestimo sobre as vendas")+
  theme(plot.title = element_text(size = 20, color = "Black"),
        plot.subtitle = element_text(size = 10, color = "Black"))+
  xlab("Período das Vendas")+
  ylab("Média de Orders")+
  scale_x_discrete("Período das Vendas", labels = c("Até Inicio Pandemia", "Depois Inicio Pandemia"))+
  scale_colour_discrete(name="Grupos Vendedores", labels=c("Vendedor s/ Emprestimo", "Vendedor c/ Emprestimo"))+
  geom_line(aes(group = as.factor(Emprestimo))) +

  annotate(geom = "segment", x = "0", xend = "1",
           y = antes_grupo_tratamento, yend = depois_grupo_tratamento - diff_diff,
           linetype = "dashed", color = "grey50") +
  annotate(geom = "segment", x = "1", xend = "1",
           y = depois_grupo_tratamento, yend = depois_grupo_tratamento - diff_diff,
           linetype = "dotted", color = "blue") +
  annotate(geom = "label", x = "1", y = depois_grupo_tratamento - (diff_diff / 2),
           label = "Efeito", size = 2)

########################################################################################
## Diff - In - Diff Simples - Regressão Linear
########################################################################################

modelo_simples <- lm(log_pedidos ~ Periodo_Venda + Emprestimo + (Periodo_Venda * Emprestimo),
                     data = dataset_diff)
tidy(modelo_simples)

########################################################################################
## Diff - In - Diff com regrassao Linear + Variáveis de controles
########################################################################################

dataset_diff_ <- dataset_diff %>%
  mutate(Segmento_Vendas = as.factor(Segmento_Vendas),
         Tipo_Assessoria = as.factor(Tipo_Assessoria),
         Vertical_Venda = as.factor(Vertical_Venda),
         UF_Vendedor = as.factor(UF_Vendedor),
         Tipo_Documento
  )

modelos_variaveis <- lm(log_pedidos ~ Periodo_Venda + Emprestimo + (Periodo_Venda * Emprestimo) +
                          Segmento_Vendas + Tipo_Assessoria + Vertical_Venda + Tipo_Documento + Vertical_Venda,
                        data = dataset_diff_)
tidy(modelos_variaveis)


diff_diff_controls <- tidy(modelos_variaveis) %>%
  filter(term == "Seller_Emprestimo:Efeito_Tratamento") %>%
  pull(estimate)


modelsummary(list("Simples" = modelo_simples, "Completo" = modelos_variaveis))

# Nenhuma variavel mostrou importancia estatistica significante para o estudo.

# Não foi possíveis incluir a variavel de UF do vendedor, pois o limite de memoria não suportou o processamento.

## Conclusão: Adesão de emprestimo não fez com que houvesse aumento de vendas superiores para quem não pegou emprestimo.
# Existe a possibilidade de outros fatores implicarem nesse estudo, como aquisiçao de emprestimo para quitar outras dividas e não para investimento,
