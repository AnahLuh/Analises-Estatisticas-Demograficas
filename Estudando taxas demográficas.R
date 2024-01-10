library(tidyverse)
library(readxl)

nascimentos <- read_excel("Estudando taxas - demog/Nascimentos_DF.xlsx")
homens <- read_excel("Estudando taxas - demog/População_homens_DF.xlsx")
mulheres <- read_excel("Estudando taxas - demog/População_mulheres_DF.xlsx")

# Taxa Bruta de Natalidade - TBN ----
# Removendo algumas linhas
nascimentos <- nascimentos[-13, ]
homens <- homens[-1, ]
mulheres <- mulheres[-1, ]
população <- mulheres[,]

# Somando as populações de homens e mulheres por ano
for (i in 2:ncol(população)) {
  população[ , i] <- mulheres[ , i] + homens[ , i]
}
população_soma <- população[,- 1]
população_soma <- população_soma %>%
  summarise_all(sum)

# Somando o número de nascidos vivos por ano
nascimentos_totais <- nascimentos %>%
  t() %>%
  as.data.frame()
nascimentos_totais <- nascimentos_totais[c(1, 14),]
colnames(nascimentos_totais) <- nascimentos_totais[1,]
nascimentos_totais <- nascimentos_totais[2,]
nascimentos_totais <- nascimentos_totais %>%
  mutate(across(everything(), as.numeric))

# TBN
tbn <- nascimentos_totais[]

for (i in 1:ncol(tbn)) {
  tbn[ , i] <- (nascimentos_totais[ , i] / população_soma[ , i]) * 1000
}

# O TBN indica quantos bebes nascem a cada 1000 habitantes num dado ano.

# Taxa de Fecundidade Geral - TFG ----

nascimentos <- read_excel("Nascimentos_DF.xlsx")
mulheres <- read_excel("População_mulheres_DF.xlsx")

# Ajeitando as colunas
nascimentos <- nascimentos[-13, c(-2, -10:-14)]
nascimentos <- nascimentos %>%
  t() %>%
  as.data.frame()
colnames(nascimentos) <- nascimentos[1,]
nascimentos <- nascimentos[-1,]
nascimentos <- nascimentos %>%
  mutate(across(everything(), as.numeric))

mulheres <- mulheres[5:11,]
mulheres <- mulheres %>%
  mutate(across(everything(), as.numeric))

# Somando os indivíduos
nascimentos <- nascimentos %>%
  summarise_all(sum)

mulheres <- mulheres %>%
  summarise_all(sum)
mulheres <- mulheres[, -1]

# TFG
tfg <- nascimentos[]

for (i in 1:ncol(tfg)) {
  tfg[ , i] <- (nascimentos[ , i] / mulheres[ , i]) * 1000
}

# Relaciona o total de nascimentos ao total de mulheres de 15 a 49 anos, 
# indicando o número de nascimentos ocorridos por mil mulheres nessa faixa etária.

# Taxas Específicas de Fecundidade (nfx) - TEF ----

nascimentos <- read_excel("Nascimentos_DF.xlsx")
mulheres <- read_excel("População_mulheres_DF.xlsx")

# Ajeitando as colunas
nascimentos <- nascimentos[-13, c(-2, -10:-14)]
nascimentos <- nascimentos %>%
  t() %>%
  as.data.frame()
colnames(nascimentos) <- nascimentos[1,]
nascimentos <- nascimentos[-1,]
nascimentos <- nascimentos %>%
  mutate(across(everything(), as.numeric))

mulheres <- mulheres[5:11,]

mulheres <- mulheres %>%
  mutate(across(-c(`GRUPO ETÁRIO`), as.numeric))

# TEF

tef <- mulheres[]
for (i in 1:ncol(tef)) {
  tef[,i+1] <- (nascimentos[,i] / mulheres[,i+1])
}

# Indica o número de filhos tido POR MULHER por ano pelas faixas etárias das mães.

# Taxa de Fecundidade Total - TFT ----

tft <- tef[, -1]

tft <- tft %>%
  summarise_all(sum)

tft <- tft * 5

# Indica o número médio de filhos que uma mulher teria ao terminar o período reprodutivo,
# caso as TEFs observadas se mantivessem ao longo desse período.

# Taxa Bruta de Mortalidade - TBM ----

mortalidade <- read.csv("Estudando taxas - demog/mortalidade_DF.csv", sep= ";")

mortalidade_total <- mortalidade[14, c(-1, -14)]
colnames(mortalidade_total) <- colnames(população_soma)

mortalidade_total <- mortalidade_total %>%
  as.data.frame()
mortalidade_total <- mortalidade_total %>%
  mutate(across(everything(), as.numeric))

# TBM
tbm <- mortalidade_total[]

for (i in 1:ncol(tbm)) {
  tbm[ , i] <- (mortalidade_total[ , i] / população_soma[ , i]) * 1000
}

# O TBM indica quantas pessoas morreram a cada 1000 habitantes num dado ano.

# Taxas específicas de Mortalidade por idade - nMx - TEM ---- 

# Ajeitando as faixas etárias

homens[2, 1] <- "5-9"
mulheres[2, 1] <- "5-9"
mortalidade_2 <- mortalidade[c(-13, -14), 1:13]
mortalidade_2 <- mortalidade_2[1:12,]
colnames(mortalidade_2) <- colnames(homens)

mortalidade_2 <- mortalidade_2 %>%
  as.data.frame()

mortalidade_2 <- mortalidade_2 %>%
  mutate(across(-c(`GRUPO ETÁRIO`), as.numeric))

ate4 <- mortalidade_2[1, -1] + mortalidade_2[2, -1]
 ...
