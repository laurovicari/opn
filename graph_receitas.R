df <- data.frame(RECEITAS = c(
                          "ITBI", "IPTU","ISS"
                         
                         ),
                 Total = c(1443660.32, 9544384.05,  8155379.84))

df$RECEITAS <- factor(df$RECEITAS, levels = c(
                                              "ITBI", "IPTU","ISS"))

df$Percentual <- round(100 * df$Total/sum(df$Total), digits = 2)


library (ggplot2)

library (dplyr)

df$lab.ypos <- c(96.5, 68, 20)

mycols <- c("royalblue4", "orange1", "red1")

df$label <- paste0(df$Percentual, "%")

g <- ggplot(df, aes(x = 2, y = Percentual, fill = RECEITAS)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = label), color = "white", size = 4)+
  labs(title = "Receitas de Impostos do Município de Ponte Nova", 
       subtitle = "Exercício financeiro de 2021",
       caption = "Fonte: Balanço Anual (DCA)") + theme_classic() +
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5) +
  theme(title = element_text(vjust = 0),
        legend.position = "right",
        legend.spacing.x = unit(0.3, "cm"),
        legend.box.spacing = unit(-0.5, "cm"),
        legend.title = element_blank(),
        plot.title = element_text(size = 12, 
                                  face = "bold", 
                                  family = "American Typewriter",
                                  hjust = 0,
                                  lineheight = 1),  # title
        plot.subtitle = element_text(size = 10, 
                                     family = "American Typewriter",
                                     face = "bold",
                                     hjust = 0,
                                     vjust = 1.5),  # subtitle
        plot.caption = element_text(size = 8,
                                    face = "bold",
                                    hjust = 0,
                                    vjust = 2.5)) +
  theme(plot.title = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.subtitle = element_text(margin=margin(b = 0.25, unit = "cm")),
        plot.caption = element_text(margin=margin(b = -0.1, unit = "cm")))

ggsave(plot = g, "D:/OPN/graph_receitas_pn.png")

