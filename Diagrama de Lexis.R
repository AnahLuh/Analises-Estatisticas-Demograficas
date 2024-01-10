library(readr)
library(tidyverse)
library(LexisPlotR)
library(lubridate)
library(xtable)

# REFERENCIAS ----
# https://rpubs.com/rafaeldeacypreste/lexis_diagram
# https://rpubs.com/caspgalvao/demo_acre

# 1- A) Nascidos vivos ----
## GRÁFICO ----

demog <- read.csv("datasus.csv", sep=";")

demog <- demog[80, c(-1, -24)]
colnames(demog) <- c(2000:2021)

demog <- demog %>%
  mutate(across(everything(), as.numeric))

demog <- pivot_longer(demog, cols = c(1:22), names_to = "Anos", values_to = "Nascimentos_vivos")

demog <- demog %>%
  mutate(idade = 0.1)

demog$Anos <- ISOdate(demog$Anos, 1, 1) 

demog$Anos <- as.Date(demog$Anos)
demog$Anos <- ymd(demog$Anos)
demog$Anos <- demog$Anos %m+% months(6) 
demog$Anos <- demog$Anos %m+% days(15)

class(demog$Anos)

lexis_grid(              # Linhas base do gráfico
  year_start = 2000,
  year_end   = 2022,
  age_start  = 0,
  age_end    = 5
) +
  annotate(              # Anotações nos gráficos
    "text",
    x     = demog$Anos,
    y     = demog$idade,
    label = demog$Nascimentos_vivos,     # Respostas da questão
    size  = 2.5,
    colour = "black"
  ) +
  labs(x     = "Ano de nascimento",
       y     = "Anos completos",
       caption = "Fonte: MS/SVS/DASIS - Sistema de Informações sobre Nascidos Vivos - SINASC") 
ggsave("lexis_1.pdf", width = 208, height = 63, units = "mm")

## TABELA ----

demog <- read.csv("datasus.csv", sep=";")

demog <- demog[80, c(-1, -24)]
colnames(demog) <- c(2000:2021)

demog <- demog %>%
  mutate(across(everything(), as.numeric))

demog <- pivot_longer(demog, cols = c(1:22), names_to = "Anos", values_to = "Nascimentos_vivos")

xtable(demog)

# 1 - A) Mortalidade ----
## GRÁFICO ----
demog2 <- read.csv("mortalidade.csv", sep=";")

demog2 <- demog2[1:22, c(1, 2, 3)]

demog2$Ano.do.Óbito <- ISOdate(demog2$Ano.do.Óbito, 1, 1) 

demog2$Ano.do.Óbito <- as.Date(demog2$Ano.do.Óbito)
demog2$Ano.do.Óbito <- ymd(demog2$Ano.do.Óbito)
demog2$Ano.do.Óbito <- demog2$Ano.do.Óbito %m+% months(6) 
demog2$Ano.do.Óbito <- demog2$Ano.do.Óbito %m+% days(15)

# 1 a 5 anos
polygons <- data.frame(
  group = as.character(c(rep(2000, 4),
                         rep(2001, 4),
                         rep(2002, 4),
                         rep(2003, 4),
                         rep(2004, 4),
                         rep(2005, 4),
                         rep(2006, 4),
                         rep(2007, 4),
                         rep(2008, 4),  # To transform fill in discrete
                         rep(2009, 4),
                         rep(2010, 4),
                         rep(2011, 4),
                         rep(2012, 4),
                         rep(2013, 4),
                         rep(2014, 4),
                         rep(2015, 4),
                         rep(2016, 4),
                         rep(2017, 4),
                         rep(2018, 4),
                         rep(2019, 4),
                         rep(2020, 4),
                         rep(2021, 4))),
  x = c(               # Coordenada x do polígono
    "2000-01-01",
    rep("2001-01-01", 2),
    "2000-01-01",
    "2001-01-01",
    rep("2002-01-01", 2), #
    "2001-01-01",
    "2002-01-01",
    rep("2003-01-01", 2),
    "2002-01-01",
    "2003-01-01",
    rep("2004-01-01", 2),
    "2003-01-01",
    "2004-01-01",
    rep("2005-01-01", 2),
    "2004-01-01",
    "2005-01-01",
    rep("2006-01-01", 2),
    "2005-01-01",
    "2006-01-01",
    rep("2007-01-01", 2),
    "2006-01-01",
    "2007-01-01",
    rep("2008-01-01", 2),#
    "2007-01-01",
    "2008-01-01",
    rep("2009-01-01", 2),
    "2008-01-01",
    "2009-01-01",
    rep("2010-01-01", 2), 
    "2009-01-01",
    "2010-01-01",
    rep("2011-01-01", 2), 
    "2010-01-01",
    "2011-01-01",
    rep("2012-01-01", 2), 
    "2011-01-01",
    "2012-01-01",
    rep("2013-01-01", 2), 
    "2012-01-01",
    "2013-01-01",
    rep("2014-01-01", 2), #
    "2013-01-01",
    "2014-01-01",
    rep("2015-01-01", 2), 
    "2014-01-01",
    "2015-01-01",
    rep("2016-01-01", 2), 
    "2015-01-01",
    "2016-01-01",
    rep("2017-01-01", 2), 
    "2016-01-01",
    "2017-01-01",
    rep("2018-01-01", 2), 
    "2017-01-01",
    "2018-01-01",
    rep("2019-01-01", 2), 
    "2018-01-01",
    "2019-01-01",
    rep("2020-01-01", 2), 
    "2019-01-01",
    "2020-01-01",
    rep("2021-01-01", 2), 
    "2020-01-01",
    "2021-01-01",
    rep("2022-01-01", 2),
    "2021-01-01"
  ),
  y = rep(c(1, 1, 5, 5), 22) # Coordenada x do polígono
)

# menor de 1
polygons_1_year <- data.frame(
  group = as.character(c(rep(2000, 4),
                         rep(2001, 4),
                         rep(2002, 4),
                         rep(2003, 4),
                         rep(2004, 4),
                         rep(2005, 4),
                         rep(2006, 4),
                         rep(2007, 4),
                         rep(2008, 4),  # To transform fill in discrete
                         rep(2009, 4),
                         rep(2010, 4),
                         rep(2011, 4),
                         rep(2012, 4),
                         rep(2013, 4),
                         rep(2014, 4),
                         rep(2015, 4),
                         rep(2016, 4),
                         rep(2017, 4),
                         rep(2018, 4),
                         rep(2019, 4),
                         rep(2020, 4),
                         rep(2021, 4))),
  x = c(               # Coordenada x do polígono
    "2000-01-01",
    rep("2001-01-01", 2),
    "2000-01-01",
    "2001-01-01",
    rep("2002-01-01", 2), #
    "2001-01-01",
    "2002-01-01",
    rep("2003-01-01", 2),
    "2002-01-01",
    "2003-01-01",
    rep("2004-01-01", 2),
    "2003-01-01",
    "2004-01-01",
    rep("2005-01-01", 2),
    "2004-01-01",
    "2005-01-01",
    rep("2006-01-01", 2),
    "2005-01-01",
    "2006-01-01",
    rep("2007-01-01", 2),
    "2006-01-01",
    "2007-01-01",
    rep("2008-01-01", 2),#
    "2007-01-01",
    "2008-01-01",
    rep("2009-01-01", 2),
    "2008-01-01",
    "2009-01-01",
    rep("2010-01-01", 2), 
    "2009-01-01",
    "2010-01-01",
    rep("2011-01-01", 2), 
    "2010-01-01",
    "2011-01-01",
    rep("2012-01-01", 2), 
    "2011-01-01",
    "2012-01-01",
    rep("2013-01-01", 2), 
    "2012-01-01",
    "2013-01-01",
    rep("2014-01-01", 2), #
    "2013-01-01",
    "2014-01-01",
    rep("2015-01-01", 2), 
    "2014-01-01",
    "2015-01-01",
    rep("2016-01-01", 2), 
    "2015-01-01",
    "2016-01-01",
    rep("2017-01-01", 2), 
    "2016-01-01",
    "2017-01-01",
    rep("2018-01-01", 2), 
    "2017-01-01",
    "2018-01-01",
    rep("2019-01-01", 2), 
    "2018-01-01",
    "2019-01-01",
    rep("2020-01-01", 2), 
    "2019-01-01",
    "2020-01-01",
    rep("2021-01-01", 2), 
    "2020-01-01",
    "2021-01-01",
    rep("2022-01-01", 2),
    "2021-01-01"
  ),
  y = rep(c(0, 0, 1, 1), 22) # Coordenada x do polígono
)

years <- as.Date(c("2000-07-01",
                   "2001-07-01",
                   "2002-07-01",
                   "2003-07-01",
                   "2004-07-01",
                   "2005-07-01",
                   "2006-07-01",
                   "2007-07-01",
                   "2008-07-01",
                   "2009-07-01",
                   "2010-07-01",
                   "2011-07-01",
                   "2012-07-01",
                   "2013-07-01",
                   "2014-07-01",
                   "2015-07-01",
                   "2016-07-01",
                   "2017-07-01",
                   "2018-07-01",
                   "2019-07-01",
                   "2020-07-01",
                   "2021-07-01"))


cores = c("#6B1D0F", "#7D2212", "#A12C17", "#B33019", "#D63A1F",
          "#DB5B06", "#DB5B06", "#EF6306", "#F97924", 
          "#DA9A1B", "#E4A425", "#E6AC37", "#E9B449", 
          "#68815F", "#728D68", "#7C9772", "#87A07E", 
          "#0CAAE9", "#0A8EC2", "#08719B", "#076388", "#04394E")

cores = c(rep(c("#232F23", "#3D523D"), 11))

lexis_grid(              # Linhas base do gráfico
  year_start = 2000,
  year_end   = 2022,
  age_start  = 0,
  age_end    = 5
) +
  geom_polygon(aes(      # Polígonos para 1 a 4 anos     
    x     = as.Date(polygons$x),
    y     = polygons$y,
    group = polygons$group,
    fill  = polygons$group
  ),
  alpha = .3)  +         # Densidade da cor do polígono
  annotate(              # Anotações nos gráficos
    "text",
    x     = years,
    y     = 3,
    label = demog2$X1.a.4.anos,     # Respostas da questão
    size  = 3.85,
    color = "white"
  ) +
  geom_polygon(aes(     # Polígonos para menores de 1 ano
    x     = as.Date(polygons_1_year$x),
    y     = polygons_1_year$y,
    group = polygons_1_year$group,
    fill  = polygons_1_year$group
  ), 
  alpha = .7) +
  annotate(
    "text",
    x     = years,
    y     = .5,
    label = demog2$Menor.1.ano, # Respostas da questão
    size  = 3.7,
    color = "white"
  ) +
  theme(legend.position = 'none') +
  scale_fill_manual(values = cores) +
  labs(x     = "Ano do óbito",
       y     = "Anos completos",
       caption = "Fonte:  MS/SVS/CGIAE - Sistema de Informações sobre Mortalidade - SIM - Óbitos por ocorrência")
ggsave("lexis_2.pdf", width = 208, height = 63, units = "mm")

# 1 - B) ----

demog_prob <- demog[1:17,]
nascidos_vivos_total <- sum(demog_prob$Nascimentos_vivos) #914.093

demog2_prob <- demog2[1:17,]
obitos_total <- sum(demog2_prob$Menor.1.ano) + sum(demog2_prob$X1.a.4.anos)#15.002

1 - (obitos_total/nascidos_vivos_total) #prob de sobrevivencia #0.9835

# 1 - C) ----

demog_prob <- demog[1:21,]
nascidos_vivos_total <- sum(demog_prob$Nascimentos_vivos) #1.135.352

demog2_prob <- demog2[1:21,]
obitos_menor1_total <- sum(demog2_prob$Menor.1.ano) #15.076

1 - (obitos_menor1_total/nascidos_vivos_total) #0.9867
