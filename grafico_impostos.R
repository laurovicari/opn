# carregando pacotes

library (dplyr)
library (httr)
library (jsonlite)
library (writexl)
library (stringr)
library (data.table)
library (ggplot2)

# coletando dados de empenho de despesa

n <- 1 # definindo a pagina 1
resultado <- list() # criando objeto lista que recebera cada pagina

# executar at? quando a condicao ("Dados nao encontrados") for verdadeira, ou seja, ate a ultima pagina de empenhos existente

while (TRUE) {
  url <- sprintf("https://dadosabertos-portalfacil.azurewebsites.net/api/receitas?type=json&idCliente=472&page=%i&pageSize=100&numAno=2022", n)
  req <- GET(url)
  result <- content(req, "text")
  conteudo <- fromJSON(result)
  resultado[[n]] <- conteudo
  if (conteudo == "Dados n?o encontrados") {
    break
  } else {
    n <- n + 1
  }
}

# excluindo ultimo item ("Dados nao encontrados")
resultado <- resultado[-length(resultado)]

# transformando em df
resultado_df <- do.call("rbind", resultado)

# Mudando as classes do resultado

resultado_df <- resultado_df %>% mutate(vlRealizado = as.numeric(vlRealizado))
resultado_df <- resultado_df %>% mutate(vlPrevisto  = as.numeric(vlPrevisto ))

class(resultado_df$vlPrevisto)
class(resultado_df$vlRealizado)


# Criando uma vari?vel para avaliar a porcentagem do realizado
resultado_df["percentil"] <- ifelse (resultado_df$vlPrevisto != 0,
                                     resultado_df$vlRealizado / resultado_df$vlPrevisto *100,
                                     0)

# Quero retirar linhas que tanto o valor previsto quanto o realizado s?o iguais a zero:

resultado <- subset(resultado_df, resultado_df$vlPrevisto != 0 | resultado_df$vlRealizado != 0)

##### An?lise
## Poss?veis an?lises:
# Arrecada??o Pr?pria x Arrecada??o por Transferenciais
# Por tipo de arrecada??o (Ex. Por tipo imposto ou tipo de transferencias)
# Previsto x Realizado

# Analise da porcentagem j? arrecadada do previsto

previsto <- sum(resultado$vlPrevisto)
realizado <- sum(resultado$vlRealizado)
arrecada??o <- (realizado / previsto) * 100
arrecada??o

# Selecionando IPTU, ITBI e ISS

tomatch <- c("^1.1.1.2.50", "^1.1.1.2.53", "^1.1.1.4.51")

impostos <- subset(resultado, 
                   grepl(paste(tomatch, collapse = "|"), resultado[[2]]))

# Agrupando

impostos_sum <- impostos %>% 
  group_by(str_sub(numReceita, 1, 10)) %>% 
  summarise(percent_real_prev = 100 * sum(vlRealizado)/sum(vlPrevisto),
            percent_prev_real = 100 - 100 * (sum(vlRealizado)/sum(vlPrevisto))) %>% 
  as.data.frame()

impostos_sum[,c(2,3)] <- round(impostos_sum[,c(2,3)], digits = 2)

impostos_sum$imposto <- c("IPTU", "ITBI", "ISS")

impostos_graph <- melt(setDT(impostos_sum[,c(2:4)]), id.vars = "imposto", variable.name = "variable")

impostos_graph$variable <- factor(impostos_graph$variable, levels = c("percent_prev_real", "percent_real_prev"))

graph <- ggplot(data = impostos_graph, aes(x = imposto, y = value, fill = variable)) +
  geom_bar(position="stack", stat="identity", width = 0.3) + 
  geom_text(data = subset(impostos_graph, variable == "percent_real_prev"), aes(label = paste0(value, "%")), color = "black", size = 3, position = position_stack(vjust = 0.5)) +
  labs(title = "Impostos municipais - Valor realizado / Valor previsto", subtitle = paste0("Exerc?cio 2022: valores atualizados at? ", format(Sys.time(), "%d/%m/%Y")),
       caption = "Fonte: Portal da Transpar?ncia da Prefeitura de Ponte Nova"
  ) +
  scale_fill_manual(values = c("#CCEEF9","#008ECE")) + xlab(NULL) + ylab(NULL) + scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  theme_test() +
  theme(axis.text.y = element_text(size = 7),
        axis.text.x = element_text(face = "bold", size = 8),
        legend.position = "none",
        plot.caption = element_text(hjust = 0))

name <- format(Sys.time(), "%Y_%m_%d %H_%M_%S")

ggsave(plot = graph, paste0("D:/OPN/painel/graphs_impostos/", name, ".png"))
