library(tidyverse)
library(xtable)
options(scipen=999)


# MÉXICO ----
## Número de mortes ----
mexico_mort <- mortalidade %>%
  filter(Location == "Mexico") %>%
  filter(Time == "2022") 

mexico_mort$DeathTotal  <- mexico_mort$DeathTotal * 1000


mexico_mort <- mexico_mort[ , c(15,20)]
mexico_mort$AgeGrp <- mexico_mort$AgeGrp %>%
  str_replace("\\+", "")

mexico_mort$AgeGrp <- as.integer(mexico_mort$AgeGrp)

mexico_mort_final <- data.frame(grupo_etario = c("0-4", "5-9", "10-14", "15-19",
                                                 "20-24", "25-29", "30-34", "35-39",
                                                 "40-44", "45-49", "50-54", "55-59",
                                                 "60-64", "65-69", "70-74", "75-79",
                                                 "80-84", "85-89", "90-94", "95-99",
                                                 "100+"),
    mortes = c(with(mexico_mort, sum(DeathTotal[AgeGrp <= 4])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 5 , 9)])) ,
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 10 , 14)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 15 , 19)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 20 , 24)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 25 , 29)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 30 , 34)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 35 , 39)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 40 , 44)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 45 , 49)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 50 , 54)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 55 , 59)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 60 , 64)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 65 , 69)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 70 , 74)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 75 , 79)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 80 , 84)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 85 , 89)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 90 , 94)])),
               with(mexico_mort, sum(DeathTotal[between(AgeGrp, 95 , 99)])),
               4309))

## População ----

mexico_pop <- Population %>%
  filter(Location == "Mexico") %>%
  filter(Time == "2022") 

mexico_pop$PopTotal  <- mexico_pop $PopTotal  * 1000

mexico_pop  <- mexico_pop[ , c(15,20)]
mexico_pop$AgeGrp <- mexico_pop$AgeGrp %>%
  str_replace("\\+", "")

mexico_pop$AgeGrp <- as.integer(mexico_pop$AgeGrp)

mexico_pop_final <- data.frame(grupo_etario = c("0-4", "5-9", "10-14", "15-19",
                                                 "20-24", "25-29", "30-34", "35-39",
                                                 "40-44", "45-49", "50-54", "55-59",
                                                 "60-64", "65-69", "70-74", "75-79",
                                                 "80-84", "85-89", "90-94", "95-99",
                                                 "100+"),
                                populacao = 
                                         c(with(mexico_pop, sum(PopTotal[AgeGrp < 5])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 5 , 9)])) ,
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 10 , 14)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 15 , 19)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 20 , 24)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 25 , 29)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 30 , 34)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 35 , 39)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 40 , 44)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 45 , 49)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 50 , 54)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 55 , 59)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 60 , 64)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 65 , 69)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 70 , 74)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 75 , 79)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 80 , 84)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 85 , 89)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 90 , 94)])),
                                           with(mexico_pop, sum(PopTotal[between(AgeGrp, 95 , 99)])),
                                           9140))

## TEM ----

estrutura_etaria_mex <- mexico_pop_final[]

colnames(estrutura_etaria_mex)[2] <- "TEM"

for (i in 1:nrow(estrutura_etaria_mex)) {
  estrutura_etaria_mex[i, 2] <- (mexico_mort_final[i , 2] / mexico_pop_final[i , 2]) 
}

# HOLANDA ----
## Número de mortes ----
holanda_mort <- mortalidade %>%
    filter(Location == "Netherlands") %>%
    filter(Time == "2022")

holanda_mort$DeathTotal  <- holanda_mort$DeathTotal * 1000

holanda_mort <- holanda_mort[ , c(15,20)]

holanda_mort$AgeGrp <- holanda_mort$AgeGrp %>%
  str_replace("\\+", "")

holanda_mort$AgeGrp <- as.integer(holanda_mort$AgeGrp)

holanda_mort_final <- data.frame(grupo_etario = c("0-4", "5-9", "10-14", "15-19",
                                                 "20-24", "25-29", "30-34", "35-39",
                                                 "40-44", "45-49", "50-54", "55-59",
                                                 "60-64", "65-69", "70-74", "75-79",
                                                 "80-84", "85-89", "90-94", "95-99",
                                                 "100+"),
                                mortes = c(with(holanda_mort, sum(DeathTotal[AgeGrp < 5])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 5 , 9)])) ,
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 10 , 14)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 15 , 19)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 20 , 24)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 25 , 29)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 30 , 34)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 35 , 39)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 40 , 44)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 45 , 49)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 50 , 54)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 55 , 59)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 60 , 64)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 65 , 69)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 70 , 74)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 75 , 79)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 80 , 84)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 85 , 89)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 90 , 94)])),
                                           with(holanda_mort, sum(DeathTotal[between(AgeGrp, 95 , 99)])),
                                           1349))


## População ----

holanda_pop <- Population %>%
  filter(Location == "Netherlands") %>%
  filter(Time == "2022")

holanda_pop$PopTotal  <- holanda_pop$PopTotal * 1000

holanda_pop <- holanda_pop[ , c(15,20)]

holanda_pop$AgeGrp <- holanda_pop$AgeGrp %>%
  str_replace("\\+", "")

holanda_pop$AgeGrp <- as.integer(holanda_pop$AgeGrp)

holanda_pop_final <- data.frame(grupo_etario = c("0-4", "5-9", "10-14", "15-19",
                                                  "20-24", "25-29", "30-34", "35-39",
                                                  "40-44", "45-49", "50-54", "55-59",
                                                  "60-64", "65-69", "70-74", "75-79",
                                                  "80-84", "85-89", "90-94", "95-99",
                                                  "100+"),
                                 populacao = c(with(holanda_pop, sum(PopTotal[AgeGrp < 5])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 5 , 9)])) ,
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 10 , 14)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 15 , 19)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 20 , 24)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 25 , 29)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 30 , 34)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 35 , 39)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 40 , 44)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 45 , 49)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 50 , 54)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 55 , 59)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 60 , 64)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 65 , 69)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 70 , 74)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 75 , 79)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 80 , 84)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 85 , 89)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 90 , 94)])),
                                            with(holanda_pop, sum(PopTotal[between(AgeGrp, 95 , 99)])),
                                            2572))
## TEF ----

estrutura_etaria_hol <- holanda_pop_final[]

colnames(estrutura_etaria_hol)[2] <- "TEM"

for (i in 1:nrow(estrutura_etaria_hol)) {
  estrutura_etaria_hol[i, 2] <- (holanda_mort_final[i , 2] / holanda_pop_final[i , 2]) 
}

# Juntando os dois ----

estrutura_etaria_mex$pais <- "México"
estrutura_etaria_hol$pais <- "Países Baixos"

estrutura_etaria_ambos <- rbind(estrutura_etaria_mex, estrutura_etaria_hol)
estrutura_etaria_ambos$grupo_etario <- factor(estrutura_etaria_ambos$grupo_etario,
                                              levels = 
                                                c("0-4", "5-9", "10-14", "15-19",
                                                  "20-24", "25-29", "30-34", "35-39",
                                                  "40-44", "45-49", "50-54", "55-59",
                                                  "60-64", "65-69", "70-74", "75-79",
                                                  "80-84", "85-89", "90-94", "95-99",
                                                  "100+"))

## Gráfico ----

ggplot(estrutura_etaria_ambos) +
  aes(x = grupo_etario, y = TEM, group = pais, colour = pais) +
  geom_line( linewidth = 0.5) +

  labs(x = "Grupo etário", y = "Taxa específica de mortalidade (log10)") +
  theme_estat() +
  scale_y_log10() +
  scale_color_manual(values=c("#5A9D58", "#EC7505"), name="País") +
  theme(axis.text.x = ggplot2::element_text(colour = "black", size = 6.5, angle = 45, vjust =  0.65))
ggsave("tef_mex_hol.pdf", width = 158, height = 93, units = "mm")

## TEF Padrão ----

estrutura_etaria_padrao <- estrutura_etaria_hol[]

for (i in 1:nrow(estrutura_etaria_padrao)) {
  estrutura_etaria_padrao[i, 2] <- (estrutura_etaria_hol[i , 2] + estrutura_etaria_mex[i , 2]) / 2 
}

estrutura_etaria_padrao$pais <- "Padrão"

estrutura_etaria_com_padrao <- rbind(estrutura_etaria_ambos, estrutura_etaria_padrao)

estrutura_etaria_com_padrao$pais <- factor(estrutura_etaria_com_padrao$pais, 
                                           levels = c("México", "Países Baixos",
                                                      "Padrão"))

## Gráfico com tef padronizada ----

ggplot(estrutura_etaria_com_padrao) +
  aes(x = grupo_etario, y = TEM, group = pais, colour = pais) +
  geom_line( linewidth = 0.5) +
  labs(x = "Grupo etário", y = "Taxa específica de mortalidade (log10)") +
  theme_estat() +
  scale_y_log10() +
  scale_color_manual(values=c("#5A9D58", "#EC7505", "#B31921"), name="País") +
  theme(axis.text.x = ggplot2::element_text(colour = "black", size = 6.5, angle = 45, vjust =  0.65))
ggsave("tef_mex_hol_padr.pdf", width = 158, height = 93, units = "mm")

## Estrutura etária padrão ----

holanda_pop_final_fn <- holanda_pop_final[]

# sum(holanda_pop_final$populacao) = 17 549 250

holanda_pop_final_fn$populacao <- holanda_pop_final_fn$populacao / sum(holanda_pop_final_fn$populacao)


mex_pop_final_fn <- mexico_pop_final[]

# sum(mexico_pop_final$populacao) = 127 524 446

mex_pop_final_fn$populacao <- mex_pop_final_fn$populacao / sum(mex_pop_final_fn$populacao)

estrutura_etaria_padrao_fn <- estrutura_etaria_padrao[]

for (i in 1:nrow(estrutura_etaria_padrao_fn)) {
  estrutura_etaria_padrao_fn[i, 2] <- (holanda_pop_final_fn[i , 2] + mex_pop_final_fn[i , 2]) / 2 
}

colnames(estrutura_etaria_padrao_fn)[2] <- "Fn"

## Tabela como no slide da Ana Maria ----

Tabela_padronizacao <- data.frame("Grupo etário" = estrutura_etaria_padrao_fn$grupo_etario ,
                                  "nMx - M" = estrutura_etaria_mex$TEM,
                                  "nCx - M" = estrutura_etaria_padrao_fn$Fn,
                                  "nMx - H" = estrutura_etaria_hol$TEM,
                                  "nCx - H" = estrutura_etaria_padrao_fn$Fn
                                  )


# TBM Padronizada México - 10.28
# TBM Padronizada Holanda - 6.35

for (i in 1:nrow(Tabela_padronizacao)) {
  Tabela_padronizacao[i, 6] <- Tabela_padronizacao[i, 4] * Tabela_padronizacao[i, 5]
}


Tabela_padronizacao$nMx...M <- Tabela_padronizacao$nMx...M  %>%
  str_replace("\\.", ",") %>%
  str_sub( 1, 8)

Tabela_padronizacao$nMx...H <- Tabela_padronizacao$nMx...H  %>%
  str_replace("\\.", ",") %>%
  str_sub( 1, 8)

Tabela_padronizacao$nCx...M <- Tabela_padronizacao$nCx...M  %>%
  str_replace("\\.", ",") %>%
  str_sub( 1, 8)

Tabela_padronizacao$nCx...H <- Tabela_padronizacao$nCx...H  %>%
  str_replace("\\.", ",") %>%
  str_sub( 1, 8)

xtable(Tabela_padronizacao)

## Tabelas com nMx e nCx de cada país ----

Tabela_nMx_nCx <- data.frame("Grupo etário" = Tabela_padronizacao$Grupo.etário,
                                  "nMx - M" = estrutura_etaria_mex$TEM,
                                  "nCx - M" = mex_pop_final_fn$populacao,
                                  "nMx - H" = estrutura_etaria_hol$TEM,
                                  "nCx - H" = holanda_pop_final_fn$populacao)

# TBM México - sum(Tabela_padronizacao$nMx...M) = 6.7
# TBM Holanda - sum(Tabela_padronizacao$nMx...H) = 8.9

#for (i in 1:nrow(Tabela_nMx_nCx)) {
#   Tabela_nMx_nCx[i, 6] <- Tabela_nMx_nCx[i, 4] * Tabela_nMx_nCx[i, 5]
# }

Tabela_nMx_nCx$nMx...M <- Tabela_nMx_nCx$nMx...M  %>%
  str_replace("\\.", ",") %>%
  str_sub( 1, 8)

Tabela_nMx_nCx$nMx...H <- Tabela_nMx_nCx$nMx...H  %>%
  str_replace("\\.", ",") %>%
  str_sub( 1, 8)

Tabela_nMx_nCx$nCx...M <- Tabela_nMx_nCx$nCx...M  %>%
  str_replace("\\.", ",") %>%
  str_sub( 1, 8)

Tabela_nMx_nCx$nCx...H <- Tabela_nMx_nCx$nCx...H  %>%
  str_replace("\\.", ",") %>%
  str_sub( 1, 8)


xtable(Tabela_nMx_nCx)

