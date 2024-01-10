# PIRâMIDES ETÁRIAS 

# SETANDO... ----
library(readr)
library(tidyverse)
library(apyramid)
library(lemon)
library(ggpubr)

ordem_idades <- c("0-4", "5-9", "10-14", "15-19",
                  "20-24", "25-29", "30-34", "35-39",
                  "40-44", "45-49", "50-54", "55-59",
                  "60-64", "65-69", "70-74", "75-79",
                  "80+")

cores <- c("#FB5A4B", "#0092E0")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 8.5),
      axis.title.x = ggplot2::element_text(colour = "black", size = 10),
      axis.text = ggplot2::element_text(colour = "black", size = 4.35),
      panel.border = ggplot2::element_blank(),
      title = ggplot2::element_text(colour = "black", size = 9),
      axis.line = ggplot2::element_line(colour = "black"),
      text = element_text(family = "sans", size = 12),
      legend.position = "top",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores),
      scale_colour_manual(values = cores)
    )
  )
}

options(scipen = 999)

# QUESTÃO 1-A ----
## 1991 ----
a1_1991 <- read_delim("Dados - Pirâmides etárias/1-a/1991.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

a1_1991 <- a1_1991 %>% pivot_longer(cols=c('Masculino', 'Feminino'),
                                    names_to='Sexo',
                                    values_to='População')

a1_1991$`Faixa Etária` <- a1_1991$`Faixa Etária` %>%
  str_replace("anos", "") %>%
  str_replace(" a ", "-") %>%
  str_trim()


a1_1991$`Faixa Etária` <- factor(a1_1991$`Faixa Etária`, 
                                 levels = ordem_idades)


age_pyramid(a1_1991, age_group = `Faixa Etária`, split_by = Sexo, count = População)

ggplot(data = a1_1991, 
       mapping = aes(x = ifelse(Sexo == "Masculino", yes = -População, no = População), 
                     y = `Faixa Etária`, fill = Sexo, width=.85)) +
  geom_col() +
  scale_x_symmetric(label = abs) +
  labs(x = "População") +
  scale_fill_manual(values =  cores, name= "Sexo") +
  theme_estat()



a1_1991_gra <- ggplot(data = a1_1991, 
                 mapping = aes(
                   x = `Faixa Etária`, 
                   y = ifelse(test = Sexo == "Masculino",  yes = -População, no = População), 
                   fill = Sexo,
                   label = format(População, big.mark = ".", decimal.mark = ","),
                   width=.85)) +
  geom_bar(stat = "identity") +
  labs(y = "População") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  geom_text(hjust=ifelse(test = a1_1991$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=1.05, colour="#505050") +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(a1_1991$População) * c(-1,1) * 1.1) +
  coord_flip()  +
  ggtitle('1991') +
  theme_estat() +
  theme(axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("Dados - Pirâmides etárias/1-a/1991.pdf", width = 158, height = 93, units = "mm")

## 2000 ----

a1_2000 <- read_delim("Dados - Pirâmides etárias/1-a/2000.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

a1_2000 <- a1_2000 %>% pivot_longer(cols=c('Masculino', 'Feminino'),
                                    names_to='Sexo',
                                    values_to='População')

a1_2000$`Faixa Etária` <- a1_2000$`Faixa Etária` %>%
  str_replace("anos", "") %>%
  str_replace(" a ", "-") %>%
  str_trim()


a1_2000$`Faixa Etária` <- factor(a1_2000$`Faixa Etária`, 
                                 levels = ordem_idades)


a1_2000_gra <- ggplot(data = a1_2000, 
       mapping = aes(
         x = `Faixa Etária`, 
         y = ifelse(test = Sexo == "Masculino",  yes = -População, no = População), 
         fill = Sexo,
         label = format(População, big.mark = ".", decimal.mark = ","),
         width=.85)) +
  geom_bar(stat = "identity") +
  labs(y = "População") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  geom_text(hjust=ifelse(test = a1_2000$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=1.05, colour="#505050") +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(a1_2000$População) * c(-1,1) * 1.1) +
  coord_flip()  +
  ggtitle('2000') +
  theme_estat() +
  theme(axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("Dados - Pirâmides etárias/1-a/2000.pdf", width = 158, height = 93, units = "mm")

## 2010 ----

a1_2010 <- read_delim("Dados - Pirâmides etárias/1-a/2010.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

a1_2010 <- a1_2010 %>% pivot_longer(cols=c('Masculino', 'Feminino'),
                                    names_to='Sexo',
                                    values_to='População')

a1_2010$`Faixa Etária` <- a1_2010$`Faixa Etária` %>%
  str_replace("anos", "") %>%
  str_replace(" a ", "-") %>%
  str_trim()


a1_2010$`Faixa Etária` <- factor(a1_2010$`Faixa Etária`, 
                                 levels = ordem_idades)


a1_2010_gra <- ggplot(data = a1_2010, 
                      mapping = aes(
                        x = `Faixa Etária`, 
                        y = ifelse(test = Sexo == "Masculino",  yes = -População, no = População), 
                        fill = Sexo,
                        label = format(População, big.mark = ".", decimal.mark = ","),
                        width=.85)) +
  geom_bar(stat = "identity") +
  labs(y = "População") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  geom_text(hjust=ifelse(test = a1_2010$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=1.05, colour="#505050") +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(a1_2010$População) * c(-1,1) * 1.1) +
  coord_flip()  +
  ggtitle('2010') +
  theme_estat() +
  theme(axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) 
  
ggsave("Dados - Pirâmides etárias/1-a/2010.pdf", width = 158, height = 93, units = "mm")

## 2015 ----

a1_2015 <- read_delim("Dados - Pirâmides etárias/1-a/2015.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

a1_2015 <- a1_2015 %>% pivot_longer(cols=c('Masculino', 'Feminino'),
                                    names_to='Sexo',
                                    values_to='População')

a1_2015$`Faixa Etária` <- a1_2015$`Faixa Etária` %>%
  str_replace("anos", "") %>%
  str_replace(" a ", "-") %>%
  str_trim()


a1_2015$`Faixa Etária` <- factor(a1_2015$`Faixa Etária`, 
                                 levels = ordem_idades)


a1_2015_gra <- ggplot(data = a1_2015, 
                      mapping = aes(
                        x = `Faixa Etária`, 
                        y = ifelse(test = Sexo == "Masculino",  yes = -População, no = População), 
                        fill = Sexo,
                        label = format(População, big.mark = ".", decimal.mark = ","),
                        width=.85)) +
  geom_bar(stat = "identity") +
  labs(y = "População") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  geom_text(hjust=ifelse(test = a1_2015$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=1.05, colour="#505050") +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(a1_2015$População) * c(-1,1) * 1.1) +
  coord_flip()  +
  ggtitle('2015') + 
  theme_estat() +
  theme(axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("Dados - Pirâmides etárias/1-a/2015.pdf", width = 158, height = 93, units = "mm")

## 2020 ----

a1_2020 <- read_delim("Dados - Pirâmides etárias/1-a/2020.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

a1_2020 <- a1_2020 %>% pivot_longer(cols=c('Masculino', 'Feminino'),
                                    names_to='Sexo',
                                    values_to='População')

a1_2020$`Faixa Etária` <- a1_2020$`Faixa Etária` %>%
  str_replace("anos", "") %>%
  str_replace(" a ", "-") %>%
  str_trim()


a1_2020$`Faixa Etária` <- factor(a1_2020$`Faixa Etária`, 
                                 levels = ordem_idades)


a1_2020_gra <- ggplot(data = a1_2020, 
                      mapping = aes(
                        x = `Faixa Etária`, 
                        y = ifelse(test = Sexo == "Masculino",  yes = -População, no = População), 
                        fill = Sexo,
                        label = format(População, big.mark = ".", decimal.mark = ","),
                        width=.85)) +
  geom_bar(stat = "identity") +
  labs(y = "População") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  geom_text(hjust=ifelse(test = a1_2020$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=1.05, colour="#505050") +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(a1_2020$População) * c(-1,1) * 1.1) +
  coord_flip()  +
  ggtitle('2020') +
  theme_estat() +
  theme(axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
  
ggsave("Dados - Pirâmides etárias/1-a/2020.pdf", width = 158, height = 93, units = "mm")

## 2030 ----

a1_2030 <- read_delim("Dados - Pirâmides etárias/1-a/2030.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

a1_2030 <- a1_2030 %>% pivot_longer(cols=c('Masculino', 'Feminino'),
                                    names_to='Sexo',
                                    values_to='População')

a1_2030$`Faixa Etária` <- a1_2030$`Faixa Etária` %>%
  str_replace("anos", "") %>%
  str_replace(" a ", "-") %>%
  str_trim()


a1_2030$`Faixa Etária` <- factor(a1_2030$`Faixa Etária`, 
                                 levels = ordem_idades)


a1_2030_gra <- ggplot(data = a1_2030, 
                      mapping = aes(
                        x = `Faixa Etária`, 
                        y = ifelse(test = Sexo == "Masculino",  yes = -População, no = População), 
                        fill = Sexo,
                        label = format(População, big.mark = ".", decimal.mark = ","),
                        width=.85)) +
  geom_bar(stat = "identity") +
  labs(y = "População") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  geom_text(hjust=ifelse(test = a1_2030$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=1.05, colour="#505050") +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(a1_2030$População) * c(-1,1) * 1.1) +
  coord_flip()  +
  ggtitle('2030') + 
  theme_estat() +
  theme(axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("Dados - Pirâmides etárias/1-a/2030.pdf", width = 158, height = 93, units = "mm")

## Juntando tudo ----
ggarrange(a1_1991_gra, a1_2000_gra, a1_2010_gra, a1_2015_gra, a1_2020_gra, a1_2030_gra,
          ncol = 2, nrow = 3,
          common.legend = TRUE, legend = "top")
ggsave("Dados - Pirâmides etárias/1-a/teste.pdf", width = 158, height = 158, units = "mm")

# QUESTÃO 1-C ----

piramide_simples <- read_delim("Dados - Pirâmides etárias/1-c/PiramideEtariaPorIdadeSimples.csv", 
                                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

colnames(piramide_simples)[1] <- "Faixa Etária"

piramide_simples <- piramide_simples %>% pivot_longer(cols=c('Masculino', 'Feminino'),
                                    names_to='Sexo',
                                    values_to='População')

piramide_simples$`Faixa Etária` <- piramide_simples$`Faixa Etária` %>%
  str_replace("anos", "") %>%
  str_replace(" a ", "-") %>%
  str_trim()


piramide_simples$`Faixa Etária` <- factor(piramide_simples$`Faixa Etária`, 
                                 levels = c("< 1", seq(1, 99, 1), "100+"))


piramide_simples_gra <- ggplot(data = piramide_simples, 
                      mapping = aes(
                        x = `Faixa Etária`, 
                        y = ifelse(test = Sexo == "Masculino",  yes = -População, no = População), 
                        fill = Sexo,
                        label = format(População, big.mark = ".", decimal.mark = ","),
                        width=.85)) +
  geom_bar(stat = "identity") +
  labs(y = "População") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  geom_text(hjust=ifelse(test = piramide_simples$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=1.15, colour="#505050") +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(piramide_simples$População) * c(-1,1) * 1.1) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 5)]) +
  coord_flip()  +
  theme_estat() +
  theme(axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text = ggplot2::element_text(colour = "black", size = 6.5)) 
ggsave("Dados - Pirâmides etárias/1-c/piramide_idade_simples.pdf", width = 158, height = 148, units = "mm")


# QUESTÃO 2-C ----

ordem_idades_2 <- c("0-4", "5-9", "10-14", "15-19",
                  "20-24", "25-29", "30-34", "35-39",
                  "40-44", "45-49", "50-54", "55-59",
                  "60-64", "65-69", "70-74", "75-79",
                  "80-84", "85-89", "90+")

## 2010 ----

c2_2010 <- read_delim("Dados - Pirâmides etárias/2-c/2010.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

colnames(c2_2010)[1] <- "Faixa Etária"

c2_2010  <- c2_2010  %>% pivot_longer(cols=c('Masculino', 'Feminino'),
                                                      names_to='Sexo',
                                                      values_to='População')

c2_2010$`Faixa Etária` <- factor(c2_2010$`Faixa Etária`, 
                                 levels = ordem_idades_2)

c2_2010_gra <- ggplot(data = c2_2010, 
                      mapping = aes(
                        x = `Faixa Etária`, 
                        y = ifelse(test = Sexo == "Masculino",  yes = -População, no = População), 
                        fill = Sexo,
                        label = format(População, big.mark = ".", decimal.mark = ","),
                        width=.85)) +
  geom_bar(stat = "identity") +
  labs(y = "População") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  geom_text(hjust=ifelse(test = c2_2010$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=1.05, colour="#505050") +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(c2_2010$População) * c(-1,1) * 1.1) +
  coord_flip()  +
  ggtitle('2010') + 
  theme_estat() +
  theme(axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("Dados - Pirâmides etárias/2-c/2010.pdf", width = 158, height = 93, units = "mm")

## 2020 ----

c2_2020 <- read_delim("Dados - Pirâmides etárias/2-c/2020.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

colnames(c2_2020)[1] <- "Faixa Etária"

c2_2020[2, 1] <- "5-9"
c2_2020[3, 1] <- "10-14"

c2_2020  <- c2_2020  %>% pivot_longer(cols=c('Masculino', 'Feminino'),
                                      names_to='Sexo',
                                      values_to='População')

c2_2020$`Faixa Etária` <- factor(c2_2020$`Faixa Etária`, 
                                 levels = ordem_idades_2)

c2_2020_gra <- ggplot(data = c2_2020, 
                      mapping = aes(
                        x = `Faixa Etária`, 
                        y = ifelse(test = Sexo == "Masculino",  yes = -População, no = População), 
                        fill = Sexo,
                        label = format(População, big.mark = ".", decimal.mark = ","),
                        width=.85)) +
  geom_bar(stat = "identity") +
  labs(y = "População") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  geom_text(hjust=ifelse(test = c2_2020$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=1.05, colour="#505050") +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(c2_2020$População) * c(-1,1) * 1.1) +
  coord_flip()  +
  ggtitle('2020') + 
  theme_estat() +
  theme(axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("Dados - Pirâmides etárias/2-c/2020.pdf", width = 158, height = 93, units = "mm")

## 2060 ----

c2_2060 <- read_delim("Dados - Pirâmides etárias/2-c/2060.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

colnames(c2_2060)[1] <- "Faixa Etária"

c2_2060[2, 1] <- "5-9"
c2_2060[3, 1] <- "10-14"

c2_2060  <- c2_2060  %>% pivot_longer(cols=c('Masculino', 'Feminino'),
                                      names_to='Sexo',
                                      values_to='População')

c2_2060$`Faixa Etária` <- factor(c2_2060$`Faixa Etária`, 
                                 levels = ordem_idades_2)

c2_2060_gra <- ggplot(data = c2_2060, 
                      mapping = aes(
                        x = `Faixa Etária`, 
                        y = ifelse(test = Sexo == "Masculino",  yes = -População, no = População), 
                        fill = Sexo,
                        label = format(População, big.mark = ".", decimal.mark = ","),
                        width=.85)) +
  geom_bar(stat = "identity") +
  labs(y = "População") +
  #geom_text( aes(label = TotalCount, TotalCount = TotalCount + 0.05)) +
  #  scale_y_continuous(limits=c(0,max(appArr$Count)*1.7)) +
  geom_text(hjust=ifelse(test = c2_2060$Sexo == "Masculino",  yes = 1.1, no = -0.1), size=1.05, colour="#505050") +
  # The 1.1 at the end is a buffer so there is space for the labels on each side
  scale_y_continuous(labels = abs, limits = max(c2_2060$População) * c(-1,1) * 1.1) +
  coord_flip()  +
  ggtitle('2060') + 
  theme_estat() +
  theme(axis.text.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
ggsave("Dados - Pirâmides etárias/2-c/2060.pdf", width = 158, height = 93, units = "mm")

## Juntando.. ----

ggarrange(c2_2010_gra, c2_2020_gra, c2_2060_gra,
          ncol = 1, nrow = 3,
          common.legend = TRUE, legend = "top")
ggsave("Dados - Pirâmides etárias/2-c/juntos.pdf", width = 158, height = 168, units = "mm")

# Razão por sexo e idade ----

Razao_sexo_idade <- read_delim("Dados - Pirâmides etárias/Razao_sexo_idade.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

Razao_sexo_idade <- Razao_sexo_idade %>% pivot_longer(cols=c('2000', '2010', '2030'),
                                    names_to='Ano',
                                    values_to='Razão de sexo')

Razao_sexo_idade$`Faixa Etária` <- Razao_sexo_idade$`Faixa Etária` %>%
  str_replace("anos", "") %>%
  str_replace(" a ", "-") %>%
  str_trim()


Razao_sexo_idade$`Faixa Etária` <- factor(Razao_sexo_idade$`Faixa Etária`, 
                                 levels = ordem_idades_2)

cores2 <- c("#FB5A4B", "#0092E0", "#62AB37")

ggplot(Razao_sexo_idade) +
  aes(x = `Faixa Etária`, y = `Razão de sexo`, group = Ano, colour = Ano) +
  geom_line(linewidth = 0.6) +
  geom_point() +
  scale_y_continuous(breaks = seq(from = 40, to = 120, by = 20), limits=c(40, 120)) +
  theme_estat() +
  theme(axis.text.x = element_text(size = 8, angle = 45, margin = margin(t = 10)))

ggsave("Dados - Pirâmides etárias/razao_Sexo.pdf", width = 158, height = 93, units = "mm")
