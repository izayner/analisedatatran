library("readr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("MASS")
library("AER")
library("rcompanion")


datatran2025 <- read_delim("datatran2025.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                           trim_ws = TRUE)

datatran2024 <- read_delim("datatran2024.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                           trim_ws = TRUE)

datatran2023 <- read_delim("datatran2023.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                           trim_ws = TRUE)

datatran2022 <- read_delim("datatran2022.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                           trim_ws = TRUE)

datatran2021 <- read_delim("datatran2021.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                           trim_ws = TRUE)

datatran2018 <- read_delim("datatran2018.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                           trim_ws = TRUE)

datatran2019 <- read_delim("datatran2019.csv", 
                           delim = ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
                           trim_ws = TRUE)


arquivos <- c(
  "datatran2021.csv",
  "datatran2022.csv",
  "datatran2023.csv",
  "datatran2024.csv",
  "datatran2025.csv"
)

dados <- arquivos |>
  lapply(read_csv2) |>
  bind_rows()

dados <-read_csv("tabela.csv", locale = locale(encoding = "WINDOWS-1252"))


dados <- dados %>%
  mutate(
    ano = year(data_inversa),
    mes = month(data_inversa)
  )

top10 <- dados %>%
  count(br, sort = TRUE) %>%
  slice(1:10)

top10

dados_top <- dados %>%
  filter(br %in% top10$br)

dados_mensais <- dados_top %>%
  group_by(ano, mes) %>%
  summarise(
    acidentes = n(),
    .groups = "drop"
  ) %>%
  arrange(ano, mes)

dados_mensais <- dados_mensais %>%
  mutate(
    tempo = 1:n()
  )

ggplot(dados_mensais,
       aes(x = tempo,
           y = acidentes)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Tempo (meses)",
    y = "Número de acidentes"
  ) +
  theme_minimal()

modelo_poisson <- glm(
  acidentes ~ tempo,
  family = poisson(link = "log"),
  data = dados_mensais
)

summary(modelo_poisson)
dispersiontest(modelo_poisson)



#binominal negativa 

modelo_nb <- glm.nb(
  acidentes ~ tempo,
  data = dados_mensais
)

summary(modelo_nb)


AIC(modelo_poisson,
    modelo_nb)


dados_mensais$predito <-
  predict(
    modelo_nb,
    type = "response"
  )

ggplot(dados_mensais,
       aes(tempo, acidentes)) +
  geom_point() +
  geom_line() +
  geom_line(aes(y = predito),
            linewidth = 1) +
  labs(
    x = "Tempo (meses)",
    y = "Número de acidentes"
  ) +
  theme_minimal()

coef(modelo_nb)



arquivos2 <- c(
  "datatran2018.csv",
  "datatran2019.csv",
  "datatran2021.csv",
  "datatran2022.csv",
  "datatran2023.csv",
  "datatran2024.csv",
  "datatran2025.csv"
)

dados2 <- arquivos2 |>
  lapply(read_csv2) |>
  bind_rows()

dados2 <-read_csv("tabela2.csv", locale = locale(encoding = "WINDOWS-1252"))



dados2 <- dados2 %>%
  mutate(
    data_inversa = ymd(data_inversa),
    ano = year(data_inversa)
  )

dados2 <- dados2 %>%
  mutate(
    periodo = case_when(
      ano %in% c(2018,2019) ~ "Pré-pandemia",
      ano %in% c(2022,2023,2024,2025) ~ "Pós-pandemia",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(periodo))



dados2 <- dados2 %>%
  mutate(
    causa_padronizada = case_when(
      
      ## Desatenção / reação do condutor
      causa_acidente %in% c(
        "Falta de Atenção à Condução",
        "Ausência de reação do condutor",
        "Reação tardia ou ineficiente do condutor"
      ) ~ "Desatenção do condutor",
      
      ## Distância de segurança
      causa_acidente %in% c(
        "Não guardar distância de segurança",
        "Condutor deixou de manter distância do veículo da frente"
      ) ~ "Distância de segurança",
      
      ## Álcool e drogas
      causa_acidente %in% c(
        "Ingestão de Álcool",
        "Ingestão de álcool pelo condutor",
        "Ingestão de Substâncias Psicoativas",
        "Ingestão de substâncias psicoativas pelo condutor"
      ) ~ "Álcool ou drogas",
      
      ## Sono/Fadiga
      causa_acidente %in% c(
        "Condutor Dormindo"
      ) ~ "Sono/Fadiga",
      
      ## Velocidade
      causa_acidente %in% c(
        "Velocidade Incompatível"
      ) ~ "Velocidade incompatível",
      
      ## Ultrapassagem
      causa_acidente %in% c(
        "Ultrapassagem Indevida"
      ) ~ "Ultrapassagem indevida",
      
      ## Mudança de faixa
      causa_acidente %in% c(
        "Manobra de mudança de faixa"
      ) ~ "Mudança de faixa",
      
      ## Contramão
      causa_acidente %in% c(
        "Transitar na contramão"
      ) ~ "Contramão",
      
      ## Problemas mecânicos
      causa_acidente %in% c(
        "Defeito Mecânico no Veículo",
        "Demais falhas mecânicas ou elétricas",
        "Problema com o freio",
        "Problema na suspensão",
        "Avarias e/ou desgaste excessivo no pneu"
      ) ~ "Problemas mecânicos",
      
      ## Problemas da via
      causa_acidente %in% c(
        "Defeito na Via",
        "Demais falhas na via",
        "Pista esburacada",
        "Pista Escorregadia",
        "Acostamento em desnível",
        "Afundamento ou ondulação no pavimento",
        "Acumulo de água sobre o pavimento",
        "Acumulo de areia ou detritos sobre o pavimento",
        "Acumulo de óleo sobre o pavimento",
        "Falta de acostamento",
        "Iluminação deficiente",
        "Deficiência do Sistema de Iluminação/Sinalização"
      ) ~ "Problemas da via",
      
      ## Clima
      causa_acidente %in% c(
        "Chuva",
        "Neblina",
        "Fenômenos da Natureza",
        "Demais Fenômenos da natureza",
        "Fumaça"
      ) ~ "Condições climáticas",
      
      ## Animais
      causa_acidente %in% c(
        "Animais na Pista"
      ) ~ "Animais na pista",
      
      ## Pedestres
      causa_acidente %in% c(
        "Falta de Atenção do Pedestre",
        "Entrada inopinada do pedestre",
        "Pedestre andava na pista",
        "Pedestre cruzava a pista fora da faixa",
        "Área urbana sem a presença de local apropriado para a travessia de pedestres"
      ) ~ "Pedestres",
      
      ## Mal súbito
      causa_acidente %in% c(
        "Mal Súbito",
        "Mal súbito do condutor"
      ) ~ "Mal súbito",
      
      ## Demais causas
      TRUE ~ "Outras causas"
    )
  )




tabela <- table(
  dados2$periodo,
  dados2$causa_padronizada
)

teste <- chisq.test(tabela)

teste


cramerV(tabela)


# questão 3

dadosmensais <- dados %>%
  mutate(
    data_inversa = ymd(data_inversa),
    mes = floor_date(data_inversa, "month")
  ) %>%
  group_by(mes) %>%
  summarise(
    pessoas = sum(pessoas),
    .groups = "drop"
  ) %>%
  arrange(mes) %>%
  mutate(
    tempo = 1:n()
  )

modelo <- glm.nb(
  pessoas ~ tempo,
  data = dadosmensais
)

summary(modelo)

dadosmensais$predito <- predict(modelo, type = "response")


ggplot(dadosmensais,
       aes(mes, pessoas)) +
  geom_line() +
  geom_line(aes(y = predito),
            linewidth = 1.2) +
  theme_minimal()




