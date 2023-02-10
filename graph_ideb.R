# Gr?ficos para o texto do Ideb PN

library (dplyr)

library (ggplot2)

# Gr?fico Ideb 2019 de PN: notas x metas

nota_meta <- data.frame(Ciclo = c("EF - Anos iniciais", "EF - Anos finais", "Ensino M?dio", "EF - Anos iniciais", "EF - Anos finais", "Ensino M?dio"),
                                Grau = c(rep("Nota",3), rep("Meta", 3)),
                                Nota = c(6.1, 4.0, 4.0, 6.1, 5.0, 3.7))

nota_meta$Ciclo = factor(nota_meta$Ciclo, levels = c("EF - Anos iniciais", "EF - Anos finais", "Ensino M?dio"), ordered = TRUE)

nota_meta$Grau = factor(nota_meta$Grau, levels = c("Nota", "Meta"), ordered = TRUE)

ggplot(nota_meta, aes(x = Ciclo, y = Nota, fill = Grau)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Nota), size = 4, position = position_dodge(width = 1),
            vjust = -0.5, size = 2) +
  labs(title = "Ideb 2019 - Ponte Nova ", 
       subtitle = "Nota x Meta", y = "Nota", x = "",
       caption = "Fonte: Ideb (Inep)") + theme_classic() +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm"))) +
  theme(title = element_text(vjust = 1),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank(),
        plot.title = element_text(size = 18, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1.2),  # title
        plot.subtitle = element_text(size = 12, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0),  # subtitle
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0), # caption
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")) + scale_fill_brewer(palette="Dark2")


# Gr?fico da s?rie hist?rica das notas

serie_ideb_ciclo <- data.frame(Ciclo = c(rep("EF - anos iniciais", 8), rep("EF - anos finais", 8), rep("Ensino M?dio", 8)),
                               Ano = c(rep(seq(2005, by = 2, len = 8), 3)),
                               Nota = c(3.9,	4.5,	5.2,	5.8,	5.9,	5.9,	5.9,	6.1, 3.2,	3.3,	3.7,	3.9,	4.3,	
                                        4.2,	3.9,	4.0, "", "", "", "", "", "", 3.5, 4),
                               data = as.Date(c(rep(c("2005-01-01", "2007-01-01", "2009-01-01", "2011-01-01", "2013-01-01", "2015-01-01", 
                                        "2017-01-01", "2019-01-01"), 3))))

serie_ideb_ciclo$Nota <- as.numeric(as.character(serie_ideb_ciclo$Nota))                                        

ggplot(serie_ideb_ciclo, aes(x = data, y = Nota)) + 
  geom_line(aes(color = Ciclo, linetype = Ciclo), size = 2.5, linetype = "solid") +
  geom_point(size = 1) +
geom_text(aes(label = Nota), size = 4, position = position_dodge(width = 1),
  vjust = -0.5, size = 2) +
  labs(title = "Ideb 2019 - Ponte Nova ", 
       subtitle = "Ciclo de Ensino", y = "Nota", x = "",
       caption = "Fonte: Ideb (Inep)") + theme_classic() +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm"))) +
  theme(title = element_text(vjust = 1),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank(),
        plot.title = element_text(size = 18, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1.2),  # title
        plot.subtitle = element_text(size = 12, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0),  # subtitle
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0), # caption
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")) + scale_fill_brewer(palette="Dark2") +
        scale_x_date(breaks = serie_ideb_ciclo$data, date_labels = "%Y")
      
  
# Gr?ficos: i) Nota x Meta; ii) Nota por rede de ensino

ideb_notaxmeta <- data.frame(Rendimento = c(rep("Ideb alcançado", 7), rep("Ideb meta", 7)),
                               Nota = c(3.3,	3.7,	3.9,	4.3,	4.2,	3.9,	4.0,	3.2,	3.4,	3.6,	4.1,	4.5,	4.7,	5),
                               data = as.Date(c(rep(c("2007-01-01", "2009-01-01", "2011-01-01", "2013-01-01", "2015-01-01", 
                                                      "2017-01-01", "2019-01-01"), 2))))

ideb_notaxmeta$Nota <- as.numeric(as.character(ideb_notaxmeta$Nota))    

ggplot(ideb_notaxmeta, aes(x = data, y = Nota)) + 
  geom_line(aes(color = Rendimento, linetype = Rendimento), size = 2.5, linetype = "solid") +
  scale_color_manual(values=c( "red", "green")) +
  geom_point(size = 1) +
  geom_text(aes(label = Nota), size = 5, position = position_dodge(width = 1),
            vjust = -0.5, size = 2) +
  labs(title = "Ideb - Ponte Nova ", 
       subtitle = "Ensino fundamental II: nota x meta", y = "Nota", x = "",
       caption = "Fonte: Ideb (Inep)") + theme_classic() +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm"))) +
  theme(title = element_text(vjust = 1),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank(),
        plot.title = element_text(size = 18, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1.2),  # title
        plot.subtitle = element_text(size = 12, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0),  # subtitle
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0), # caption
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")) + scale_fill_brewer(palette="Dark2") +
  scale_x_date(breaks = ideb_notaxmeta$data, date_labels = "%Y") +
  theme_get()


ideb_por_rede <- data.frame(Rede = c(rep("Estadual", 8), rep("Municipal", 8)),
                             Nota = c(3.1, 3.3,	3.7, 3.9,	4.4, 3.8, 3.6, 3.9, 3.4, 3.3, 3.6, 3.8, 4.2, 5.0,	4.4, 4.1),
                             data = as.Date(c(rep(c("2005-01-01", "2007-01-01", "2009-01-01", "2011-01-01", "2013-01-01", "2015-01-01", 
                                                    "2017-01-01", "2019-01-01"), 2))))

ideb_por_rede$Nota <- as.numeric(as.character(ideb_por_rede$Nota))    

ggplot(ideb_por_rede, aes(x = data, y = Nota)) + 
  geom_line(aes(color = Rede, linetype = Rede), size = 2.5, linetype = "solid") +
  scale_color_manual(values=c( "orange", "blue")) +
  geom_point(size = 1) +
  geom_text(aes(label = Nota), size = 4, position = position_dodge(width = 1),
            vjust = -0.5, size = 2) +
  labs(title = "Ideb 2019 - Ponte Nova ", 
       subtitle = "Rede de ensino", y = "Nota", x = "",
       caption = "Fonte: Ideb (Inep)") + theme_classic() +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm"))) +
  theme(title = element_text(vjust = 1),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank(),
        plot.title = element_text(size = 18, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1.2),  # title
        plot.subtitle = element_text(size = 12, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0),  # subtitle
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0), # caption
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")) + scale_fill_brewer(palette="Dark2") +
  scale_x_date(breaks = ideb_por_rede$data, date_labels = "%Y")


# Gr?fico do indicador de rendimento por escola

i_rend_escola <- read.csv("C:/Users/Lauro Vicari/Desktop/OGM/ideb_irendimento_escola.csv", sep = ";", 
                          header = T, stringsAsFactors = F)

i_rend_escola$Nota <- gsub("\\,", ".", i_rend_escola$Nota)

i_rend_escola$Nota <- as.numeric(as.character(i_rend_escola$Nota))

i_rend_escola$Nota <- round(i_rend_escola$Nota, digits = 2)

i_rend_escola$data <- lubridate::dmy(i_rend_escola$data)

ggplot(i_rend_escola, aes(x = data, y = Nota)) + 
  geom_line(aes(color = Escola, linetype = Escola), size = 2.5, linetype = "solid") +
  geom_point(size = 1) +
  labs(title = "Ideb 2019 - Ponte Nova ", 
       subtitle = "Indicador de rendimento (aprova??o)", y = "Indicador de rendimento", x = "",
       caption = "Fonte: Ideb (Inep)") + theme_classic() +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm"))) +
  theme(title = element_text(vjust = 1),
        legend.position = "right",
        legend.key.size = unit(1, "cm"),
        legend.spacing.y = unit(0.3, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(size = 18, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1.2),  # title
        plot.subtitle = element_text(size = 12, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0),  # subtitle
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0), # caption
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")) + scale_fill_brewer(palette="Dark2") +
  scale_x_date(breaks = i_rend_escola$data, date_labels = "%Y")

# Gr?fico de varia??o em pp de cada escola

i_desemp_escola <- data.frame (Escola = c(rep("EE C.M", 4), rep("EE C.T", 4),
                                          rep("EE C.C.D.", 4), rep("EE P.A.G.L", 4),
                                          rep("EE P.R.M.F", 4), rep("EM R.A.C", 4),
                                          rep("EE S.A.M.", 4), rep("EM S.M.L", 4),
                                          rep("EM J.M.F.", 4)),
                               Ano = c(rep(c("var_6?", "var_7?", "var_8?", "var_9?"),9)),
                               Valor = c(-20.7,	-29.7,	-28.8,	-0.5,	-10.6,	-2.2,	-5.3,	-3.7,	0.7,	-19.3,	-1.1,	-7.6,	-5.6,	-7.7,	-15.6,
                                         -8.4,	-14.2,	-29.2,	-23.2,	-12.2,	-4.2,	2.3,	-1.6,	-16.6,	-8.3,	-18.2,	-3.3,	5.8,	3.7,	28.4,	
                                         27.6,	14.7,	15.3,	8.3,	2.7,	6.6))
  
ggplot(i_desemp_escola, aes(x = Escola, y = Valor, fill = Ano)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Valor), size = 3.5, position = position_dodge(width = 1),
            vjust = -0.5, size = 2) +
  labs(title = "Ideb 2019 - Ponte Nova ", 
       subtitle = "Indicador de desempenho: varia??o em p.p (2013 e 2015)\nEscolas selecionadas*", y = "Nota", x = "",
       caption = "Fonte: Ideb (Inep)") + theme_classic() +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm"))) +
  theme(title = element_text(vjust = 1),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank(),
        plot.title = element_text(size = 18, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1.2),  # title
        plot.subtitle = element_text(size = 12, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0),  # subtitle
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0), # caption
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")) + scale_fill_brewer(palette="Dark2")



i_desemp_escola_b <- data.frame (Escola = c(rep("EE C.M", 4), rep("EE C.T", 4),
                                          rep("EE C.C.D.", 4), rep("EE P.A.G.L", 4),
                                          rep("EE P.R.M.F", 4), rep("EM R.A.C", 4),
                                          rep("EE S.A.M.", 4), rep("EM S.M.L", 4),
                                          rep("EM J.M.F.", 4)),
                               Ano = c(rep(c("var_6?", "var_7?", "var_8?", "var_9?"),9)),
                               Valor = c(-10.8,	-3.8,	12,	-16,
                                         -7.8,	-20.3,	-0.7,	-4.9,
                                         -22.4,	-5,	1.7,	5.2,
                                         -9.5,	-21.3,	-5.5,	-12.2,
                                         9.9,	23.3,	44.9,	2.9,
                                         -13,	-8.1,	-36,	-11.5,
                                         7.5,	12.5,	-2.9,	-16.9,
                                         -10,	-36,	-25.5,	-35.9,
                                         -13,	-3.7,	-12.9,	-12))
                              

ggplot(i_desemp_escola_b, aes(x = Escola, y = Valor, fill = Ano)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Valor), size = 3.5, position = position_dodge(width = 1),
            vjust = -0.5, size = 2) +
  labs(title = "Ideb 2019 - Ponte Nova ", 
       subtitle = "Indicador de desempenho: varia??o em p.p (2015 e 2017)\nEscolas selecionadas*", y = "Nota", x = "",
       caption = "Fonte: Ideb (Inep)") + theme_classic() +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm"))) +
  theme(title = element_text(vjust = 1),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank(),
        plot.title = element_text(size = 18, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1.2),  # title
        plot.subtitle = element_text(size = 12, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0),  # subtitle
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0), # caption
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")) + scale_fill_brewer(palette="Dark2")


# Gr?fico sobre a m?dia do ?ndice de desempenho 


i_desemp_medio <- data.frame(Ano = as.Date(c("2005-01-01", "2007-01-01", "2009-01-01", "2011-01-01", "2013-01-01",
                                      "2015-01-01", "2017-01-01", "2019-01-01")),
                            Nota = as.numeric(c(4.75, 4.93, 5.04, 5.19, 5.00, 5.18, 5.20, 5.2)))
  

ggplot(i_desemp_medio, aes(x = Ano, y = Nota)) + 
  geom_line(color = "steel blue", size = 2.5, linetype = "solid") +
  geom_point(size = 1) +
  geom_text(aes(label = Nota), size = 4, position = position_dodge(width = 1),
            vjust = -0.6, size = 2) +
  labs(title = "Ideb 2019 - Ponte Nova ", 
       subtitle = "Indicador de desempenho (m?dia)", y = "Indicador de desempenho", x = "",
       caption = "Fonte: Ideb (Inep)") + theme_classic() +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm"))) +
  theme(title = element_text(vjust = 1),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank(),
        plot.title = element_text(size = 18, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1.2),  # title
        plot.subtitle = element_text(size = 12, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0),  # subtitle
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0), # caption
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")) + scale_fill_brewer(palette="Dark2") +
  scale_x_date(breaks = i_desemp_medio$Ano, date_labels = "%Y")

# Gr?fico das escolas

saeb_escolas <- read.csv("C:/Users/Lauro Vicari/Desktop/OGM/saeb_por_escola_por_ano.csv", sep = ";", 
                          header = T, stringsAsFactors = F)

saeb_escolas$Nota <- as.numeric(gsub("\\,", ".", saeb_escolas$Nota))

saeb_escolas$Ano <- lubridate::dmy(saeb_escolas$Ano)

CM <- saeb_escolas[which(saeb_escolas$Escola == "EE CAETANO MARINHO"),]

CT <- saeb_escolas[which(saeb_escolas$Escola == "EE CARLOS TRIVELLATO"),]

CD <- saeb_escolas[which(saeb_escolas$Escola == "EE CORONEL CANTIDIO DRUMOND"),]

BF <- saeb_escolas[which(saeb_escolas$Escola == "EE GOVERNADOR BIAS FORTES"),]

LMSS <- saeb_escolas[which(saeb_escolas$Escola == "EM LUIZ MARTINS SOARES SOBRINHO"),]


PAGL <- saeb_escolas[which(saeb_escolas$Escola == "EE PROFESSOR ANTONIO GONCALVES LANNA"),]

RAC <- saeb_escolas[which(saeb_escolas$Escola == "ESCOLA MUNICIPAL REINALDO ALVES COSTA"),]

SAM <- saeb_escolas[which(saeb_escolas$Escola == "EE SENADOR ANTONIO MARTINS"),]

SML <- saeb_escolas[which(saeb_escolas$Escola == "EM SEN MIGUEL LANA"),]

JMF <- saeb_escolas[which(saeb_escolas$Escola == "EM JOSE MARIA DA FONSECA"),]


ggplot(JMF, aes(x = Ano, y = Nota)) + 
  geom_line(aes(color = Prova, linetype = Prova), size = 2.5, linetype = "solid") +
  scale_color_manual(values=c( "red", "steel blue")) +
  geom_point(size = 1) +
  geom_text(aes(label = Nota), size = 4, position = position_dodge(width = 1),
            vjust = -0.5, size = 2) +
  labs(title = "Notas na prova Saeb", 
       subtitle = JMF[1,1], y = "Nota", x = "",
       caption = "Fonte: Ideb (Inep)") + theme_classic() +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm"))) +
  theme(title = element_text(vjust = 1),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank(),
        plot.title = element_text(size = 18, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1.2),  # title
        plot.subtitle = element_text(size = 12, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0),  # subtitle
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0), # caption
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")) + scale_fill_brewer(palette="Dark2") +
  scale_x_date(breaks = JMF$Ano, date_labels = "%Y")

# Gr?fico das escolas 2019

saeb_2019_escolas <- read.csv("C:/Users/Lauro Vicari/Desktop/OGM/saeb_2019_escola.csv", sep = ";", 
                         header = T, stringsAsFactors = F)

saeb_2019_escolas$Nota <- as.numeric(gsub("\\,", ".", saeb_2019_escolas$Nota))

nota_meta$Grau = factor(nota_meta$Grau, levels = c("Nota", "Meta"), ordered = TRUE)

ggplot(saeb_2019_escolas, aes(x = Escola, y = Nota, fill = Prova)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Nota), size = 3, position = position_dodge(width = 1),
            vjust = -0.5, size = 2) +
  labs(title = "Ideb 2019 - Ponte Nova ", 
       subtitle = "Notas na prova Saeb (2019)", y = "Nota", x = "",
       caption = "Fonte: Ideb (Inep)") + theme_classic() +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.5, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm"))) +
  theme(title = element_text(vjust = 1),
        legend.position = "bottom",
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.title = element_blank(),
        plot.title = element_text(size = 18, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1.2),  # title
        plot.subtitle = element_text(size = 12, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0),  # subtitle
        plot.caption = element_text(size = 10,
                                    face = "bold",
                                    hjust = 0), # caption
        axis.text.x = element_text(size = 10, colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black")) + scale_fill_brewer(palette="Dark2") 

