# load library

library(tidyverse)
library(lubridate)
library(RcppRoll)

getwd()
setwd("E:/Proyectos R/covidperu")
getwd()

# get data

link_fallecidos = "https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download"

load_data = download.file(url = link_fallecidos,
                          destfile = "E:/Proyectos R/covidperu/fallecidos_covid.csv")

bd_fallecidos = read.csv("fallecidos_covid.csv", sep = ";",
                        encoding = "UTF -8")

colnames(bd_fallecidos)

# clean data

l_select_data = c("FECHA_FALLECIMIENTO", "DEPARTAMENTO", "PROVINCIA", "DISTRITO")
df_fallecidos = subset(bd_fallecidos[l_select_data])


df_fallecidos$FECHA_FALLECIMIENTO = as.Date(as.character(df_fallecidos$FECHA_FALLECIMIENTO),
                                            format = "%Y%m%d")


# 
df_fallecidos$year = year(df_fallecidos$FECHA_FALLECIMIENTO)
df_fallecidos$dia = day(df_fallecidos$FECHA_FALLECIMIENTO)

colnames(df_fallecidos)

# totales diarios (1)

df_totales = df_fallecidos %>%
  group_by(DEPARTAMENTO, FECHA_FALLECIMIENTO, year, dia) %>%
  summarise(fallecidos = n())


# promedio movil 7 dias (2)

df_prom = df_totales %>%
  split(.$DEPARTAMENTO) %>%
  map(~ roll_mean(.$fallecidos, n = 7, fill = NA, align = "right"))


# fusion (1) y (2)

df_fal_sum_prom = data.frame(df_totales, promedio = unlist(df_prom))
colnames(df_fal_sum_prom)

# grafico: fallecidos nivel nacional

ggplot(df_fal_sum_prom) +
  geom_col(aes(x= FECHA_FALLECIMIENTO, y = fallecidos)) +
  labs(title = "Fallecidos diarios por Covid - 19 desde el 16/03/2020 al 31/05/2021",
       subtitle = "Elaborado: Luis Miguel Meza Ramos",
       caption = "Fuente: MINSA",
       x = "Fecha de defunción",
       y = "Número diario de fallecidos por Covid - 19")

# grafico: fallecidos nivel nacional por departamento

ggplot(df_fal_sum_prom) +
  geom_col(aes(x= FECHA_FALLECIMIENTO, y = fallecidos, fill = factor(year))) + 
  geom_line(aes(x= FECHA_FALLECIMIENTO, y = promedio, linetype = "promedio movil 7 días"), size = 1, color = "blue") +
  facet_wrap(~ DEPARTAMENTO, scales = "free_y") +
  scale_fill_manual(name = "Año", values=c("gray", "tomato")) +
  theme(legend.position = "top") +
  labs(title = "Fallecidos diarios por Covid - 19 desde 03/03/2020 al 31/05/2021",
       subtitle = "Elaborado: Luis Miguel Meza Ramos",
       caption = "Fuente: MINSA")

# filter departamentos

l_dep = data.frame(dep = unique(df_fal_sum_prom$DEPARTAMENTO))
l_f_dep = l_dep[1:26,]

1-9
10-18
19-26

df_fil_dep_sum_prom = filter(df_fal_sum_prom, DEPARTAMENTO %in% l_f_dep)

range(df_fil_dep_sum_prom$FECHA_FALLECIMIENTO)

ggplot(df_fil_dep_sum_prom) +
  geom_col(aes(x= FECHA_FALLECIMIENTO, y = fallecidos, fill = factor(year))) + 
  geom_line(aes(x= FECHA_FALLECIMIENTO, y = promedio, linetype = "promedio movil 7 días"), size = 1, color = "blue") +
  facet_wrap(~ DEPARTAMENTO, scales = "free") +
  scale_fill_manual(name = "Año", values=c("gray", "tomato")) +
  theme(legend.position = "top") +
  labs(title = "Fallecidos diarios por Covid - 19 desde el 16/03/2020 al 31/05/2021",
       subtitle = "Elaborado: Luis Miguel Meza Ramos",
       caption = "Fuente: MINSA",
       x = "Fecha de defunción",
       y = "Número diario de fallecidos por Covid - 19")

ggsave("fallecidos_covid19_03.png", scale = 2)
dev.off()



colnames(df_fil_dep_sum_prom)


aggregate(df_fil_dep_sum_prom$fallecidos,
          by = list(df_fil_dep_sum_prom$DEPARTAMENTO),
          FUN = sum)

aggregate(fallecidos ~ DEPARTAMENTO+ year, data=df_fil_dep_sum_prom,FUN=sum)

tapply(df_fil_dep_sum_prom$fallecidos, df_fil_dep_sum_prom[c("DEPARTAMENTO", "year")],sum)

by(df_fil_dep_sum_prom$fallecidos, df_fil_dep_sum_prom[c("DEPARTAMENTO", "year")],sum)

var = data.frame(tapply(df_fil_dep_sum_prom$fallecidos, df_fil_dep_sum_prom[c("DEPARTAMENTO", "year")],sum)
)


var_cal = round(100*((var$X2021 - var$X2020)/var$X2020), 2)
fac_cal = round(((var$X2021/var$X2020)), 2)

df_var = cbind(var, var_cal, fac_cal)
arrange(df_var, desc(var_cal))
