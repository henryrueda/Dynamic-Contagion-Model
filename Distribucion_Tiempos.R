library(tidyverse)
library(lubridate)
library(readxl)
#####################################################
setwd("C:/Users/hsrc_/OneDrive - Universidad de los andes/Unal/Tesis/Libros tesis/Contagio/Grupo Bolivar app")
#####################################################
##duplicado: GSO y PSO
datos_completos <- readxl::read_excel("2020-11 - siniestrosARL_largos.xlsx",sheet = "15230009698") %>% 
                    select(starts_with("FECHA_"),NUMERO_MOVIMIENTO_SINIESTRO, EXPEDIENTE,NOMBRE_EXPEDIENTE,
                           CODIGO_CONCEPTO_LIQUIDACION, NOMBRE_CONCEPTO_LIQUIDACION, LIQUIDADO_BOLIVAR
                           ) %>% 
                    mutate(across(.cols = starts_with("FECHA_"),.fns = ~as_date(.x))) %>% 
                    filter(LIQUIDADO_BOLIVAR > 0) %>% 
                    group_by(FECHA_MOVIMIENTO) %>% 
                    summarise(suma_liq = sum(LIQUIDADO_BOLIVAR),
                              exp_max = max(EXPEDIENTE),
                              expe_dist = n_distinct(EXPEDIENTE),.groups = "drop") %>% 
                    arrange(FECHA_MOVIMIENTO) %>% 
                    mutate(FECHA_MOV_LAG = lag(FECHA_MOVIMIENTO), 
                           ELAPSED_TIME = as.numeric(FECHA_MOVIMIENTO-FECHA_MOV_LAG)
                           )
##############REVISION DE UNA EXPONENCIAL##################
func_estimate <- function(variable_estudio,datos, porc_lost,superior = F)
{
  #@datos: datos que tienen los valores de los tiempos de llegada
  #@porc_lost: Porcentaje de saltos que se eliminan (Definir que es un impacto!!!)
  #@variable_estudio: nombre de la variable en comillas que se quiere estudiar
  #variable_estudio = "ELAPSED_TIME"
  #datos <- datos_completos; porc_lost <- 20/100 
  ##############3
  variable_estudio_sym = sym(variable_estudio)
  if(superior)
  {
    pruebas <- datos %>% 
      mutate(rango_salto = percent_rank({{variable_estudio_sym}})) %>% 
      filter(rango_salto <= 1- porc_lost) 
    subti <- str_c('Eliminando el ', scales::percent(porc_lost), ' Superior')
    salto_limite = min(pruebas[[variable_estudio]])
  }
  if(!superior) 
  {
    pruebas <- datos %>% 
      mutate(rango_salto = percent_rank({{variable_estudio_sym}}))%>% 
      filter(rango_salto >= porc_lost)
    subti <- str_c('Eliminando el ', scales::percent(porc_lost), ' Inferior')
    salto_limite = min(pruebas[[variable_estudio]])
  }
  test_kolmogrov <- ks.test(pruebas[[variable_estudio]] , "pexp", 1/mean(pruebas[[variable_estudio]]))
  p <- pruebas %>% 
    ggplot(aes(x = {{variable_estudio_sym}}, col = 'black')) + 
    stat_ecdf(size = 1) +
    stat_function(aes(col = 'red'), fun=pexp, args = list(rate=1/mean(pruebas[[variable_estudio]])), size=1) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    ggtitle('Comparación funcion de distribución', subti) +
    scale_colour_manual(values=c("black","red"))
  retorno = list(p_valor = test_kolmogrov$p.value, grafico = p,salto_limite=salto_limite)
}
### CASO SIN QUITAR DATOS
trial_sin_perder <- func_estimate(variable_estudio = "ELAPSED_TIME",datos = datos_completos,porc_lost = 0,superior = F)

trial_sin_perder$p_valor
trial_sin_perder$grafico
#################PRUEBAS VARIAS##################################
prueba_inferior = map(0:90/100, ~func_estimate(variable_estudio = "ELAPSED_TIME",datos = datos_completos,porc_lost = .x,superior = F))
prueba_superior = map(0:90/100, ~func_estimate(variable_estudio = "ELAPSED_TIME",datos = datos_completos,porc_lost = .x,superior = T))
p_valores_inferior <- prueba_inferior %>% map_dbl(~.x$p_valor) 
valores_limite_inferior <- prueba_inferior %>% map_dbl(~.x$salto_limite) 
p_valores_superior <- prueba_superior %>% map_dbl(~.x$p_valor)
valores_limite_superior <- prueba_superior %>% map_dbl(~.x$salto_limite) 
tabla = tibble(x = 0:90/100, y = p_valores_superior, min_valor = valores_limite_superior)
graf <- tabla %>% 
  ggplot(aes(x= x, y=y)) +
  geom_path(size = 1) +
  geom_path(y=tabla$min_valor, color = 'magenta') +
  xlab('Porcentaje eliminado') + 
  ylab('P-valores') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  geom_hline(aes(yintercept=0.01, color = 'blue'), size = 1) +
  geom_hline(aes(yintercept=0.05, color = 'green'), size = 1) + 
  geom_hline(aes(yintercept=0.1, color = 'red'), size = 1) +
  scale_colour_manual(values=c("blue","green","red"), labels=c('1%','5%','10%'), name = 'p-valor') +
  ggtitle('P valores para la prueba de la distribución exponencial')
plotly::ggplotly(graf)
