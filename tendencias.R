library("forecast")
library("tseries")
library("scales")
library("dplyr")
library("Kendall")

seriedatatra <- read.csv("seriedatatra.csv")
seriecovid <- read.csv("seriecovid.csv")


serie <- left_join(
  seriecovid,
  seriedatatra,
  by = "mes"
)
head(serie)

serie <- serie %>%
  mutate(
    acidentes_z = scale(serie$obitosacidentes)[,1],
    covid_z = scale(serie$obitoscovid)[,1]
  )


serie <- serie %>% 
  rename(obitoscovid = obitosNovos.x)

serie <- serie %>% 
  rename(obitosacidentes = obitosNovos.y)

ts_covid <- ts(
  serie$obitoscovid,
  start = c(2021, 1),
  frequency = 12
)

ts_acidentes <- ts(
  serie$obitosacidentes,
  start = c(2021, 1),
  frequency = 12
)


stl_covid <- stl(
  ts_covid,
  s.window = "periodic"
)

stl_acidentes <- stl(
  ts_acidentes,
  s.window = "periodic"
)


tend <- data.frame(
  mes = serie$mes,
  covid = stl_covid$time.series[, "trend"],
  acidentes = stl_acidentes$time.series[, "trend"]
)

tend$covid <- as.numeric(scale(tend$covid))
tend$acidentes <- as.numeric(scale(tend$acidentes))



ggplot(tend, aes(x = mes)) +
  
  geom_line(aes(y = covid,
                color = "COVID-19"),
            linewidth = 1.2) +
  
  geom_line(aes(y = acidentes,
                color = "Acidentes"),
            linewidth = 1.2) +
  
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m"
  ) +
  
  labs(
    x = "Mês",
    y = "Tendência padronizada",
    color = ""
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45,
                               hjust = 1)
  )


MannKendall(ts_covid)

MannKendall(ts_acidentes)


cor.test(
  tend$covid,
  tend$acidentes,
  method = "spearman"
)

ccf(
  tend$covid,
  tend$acidentes,
  lag.max = 12
)
