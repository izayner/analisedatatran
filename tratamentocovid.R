library(readr)
acovid2025 <- read_delim("HIST_PAINEL_COVIDBR_2025_Parte1_05set2025.csv", 
                                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)


bcovid2025 <- read_delim("HIST_PAINEL_COVIDBR_2025_Parte2_05set2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)



acovid2024 <- read_delim("HIST_PAINEL_COVIDBR_2024_Parte1_05set2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)


bcovid2024 <- read_delim("HIST_PAINEL_COVIDBR_2024_Parte2_05set2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)


acovid2023 <- read_delim("HIST_PAINEL_COVIDBR_2023_Parte1_05set2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)


bcovid2023 <- read_delim("HIST_PAINEL_COVIDBR_2023_Parte2_05set2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

acovid2022 <- read_delim("HIST_PAINEL_COVIDBR_2022_Parte1_05set2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

bcovid2022 <- read_delim("HIST_PAINEL_COVIDBR_2022_Parte2_05set2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

acovid2021 <- read_delim("HIST_PAINEL_COVIDBR_2021_Parte1_05set2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

bcovid2021 <- read_delim("HIST_PAINEL_COVIDBR_2021_Parte2_05set2025.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)




acovid2025 <- acovid2025 %>% 
  dplyr::select(data, obitosNovos)

bcovid2025 <- bcovid2025 %>% 
  dplyr::select(data, obitosNovos)

acovid2024 <- acovid2024 %>% 
  dplyr::select(data, obitosNovos)

bcovid2024 <- bcovid2024 %>% 
  dplyr::select(data, obitosNovos)

acovid2023 <- acovid2023 %>% 
  dplyr::select(data, obitosNovos)

bcovid2023 <- bcovid2023 %>% 
  dplyr::select(data, obitosNovos)

acovid2022 <- acovid2022 %>% 
  dplyr::select(data, obitosNovos)

bcovid2022 <- bcovid2022 %>% 
  dplyr::select(data, obitosNovos)

acovid2021 <- acovid2021 %>% 
  dplyr::select(data, obitosNovos)

bcovid2021 <- bcovid2021 %>% 
  dplyr::select(data, obitosNovos)

acovid2025_mensal <- acovid2025 %>%
  mutate(data = as.Date(data)) %>% 
  
  mutate(mes = floor_date(data, "month")) %>% 
  
  group_by(mes) %>% 
  
  summarise(obitosNovos = sum(obitosNovos, na.rm = TRUE)) %>% 

  ungroup()



bcovid2025_mensal <- bcovid2025 %>%
 
  mutate(data = as.Date(data)) %>% 
  
  mutate(mes = floor_date(data, "month")) %>% 

  group_by(mes) %>% 

  summarise(obitosNovos = sum(obitosNovos, na.rm = TRUE)) %>% 

  ungroup()


acovid2024_mensal <- acovid2024 %>%
  
  mutate(data = as.Date(data)) %>% 
  
  mutate(mes = floor_date(data, "month")) %>% 
  
  group_by(mes) %>% 
  
  summarise(obitosNovos = sum(obitosNovos, na.rm = TRUE)) %>% 
  
  ungroup()

bcovid2024_mensal <- bcovid2024 %>%
  
  mutate(data = as.Date(data)) %>% 
  
  mutate(mes = floor_date(data, "month")) %>% 
  
  group_by(mes) %>% 
  
  summarise(obitosNovos = sum(obitosNovos, na.rm = TRUE)) %>% 
  
  ungroup()


acovid2023_mensal <- acovid2023 %>%
  
  mutate(data = as.Date(data)) %>% 
  
  mutate(mes = floor_date(data, "month")) %>% 
  
  group_by(mes) %>% 
  
  summarise(obitosNovos = sum(obitosNovos, na.rm = TRUE)) %>% 
  
  ungroup()

bcovid2023_mensal <- bcovid2023 %>%
  
  mutate(data = as.Date(data)) %>% 
  
  mutate(mes = floor_date(data, "month")) %>% 
  
  group_by(mes) %>% 
  
  summarise(obitosNovos = sum(obitosNovos, na.rm = TRUE)) %>% 
  
  ungroup()


acovid2022_mensal <- acovid2022 %>%
  
  mutate(data = as.Date(data)) %>% 
  
  mutate(mes = floor_date(data, "month")) %>% 
  
  group_by(mes) %>% 
  
  summarise(obitosNovos = sum(obitosNovos, na.rm = TRUE)) %>% 
  
  ungroup()

bcovid2022_mensal <- bcovid2022 %>%
  
  mutate(data = as.Date(data)) %>% 
  
  mutate(mes = floor_date(data, "month")) %>% 
  
  group_by(mes) %>% 
  
  summarise(obitosNovos = sum(obitosNovos, na.rm = TRUE)) %>% 
  
  ungroup()



acovid2021_mensal <- acovid2021 %>%
  
  mutate(data = as.Date(data)) %>% 
  
  mutate(mes = floor_date(data, "month")) %>% 
  
  group_by(mes) %>% 
  
  summarise(obitosNovos = sum(obitosNovos, na.rm = TRUE)) %>% 
  
  ungroup()

bcovid2021_mensal <- bcovid2021 %>%
  
  mutate(data = as.Date(data)) %>% 
  
  mutate(mes = floor_date(data, "month")) %>% 
  
  group_by(mes) %>% 
  
  summarise(obitosNovos = sum(obitosNovos, na.rm = TRUE)) %>% 
  
  ungroup()



tabela_final_longa <- bind_rows(
  bcovid2021_mensal,
  acovid2021_mensal,
  bcovid2022_mensal,
  acovid2022_mensal,
  bcovid2023_mensal,
  acovid2023_mensal,
  bcovid2024_mensal,
  acovid2024_mensal,
  bcovid2025_mensal,
  acovid2025_mensal
) %>% 
  arrange(mes)
