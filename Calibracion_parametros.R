library(tidyverse)
library(lubridate)
library(readxl)
library(DEoptim)
#####################GET DATA################################
setwd("C:/Users/hsrc_/OneDrive - Universidad de los andes/Unal/Tesis/Libros tesis/Contagio/Grupo Bolivar app")
##duplicado: GSO y PSO
datos_completos <- readxl::read_excel("2020-11 - siniestrosARL_largos.xlsx",sheet = "15230009698") %>% 
  select(starts_with("FECHA_"),NUMERO_MOVIMIENTO_SINIESTRO, EXPEDIENTE,NOMBRE_EXPEDIENTE,
         CODIGO_CONCEPTO_LIQUIDACION, NOMBRE_CONCEPTO_LIQUIDACION, LIQUIDADO_BOLIVAR
  ) %>% 
  mutate(across(.cols = starts_with("FECHA_"),.fns = ~as_date(.x))) %>% 
  filter(LIQUIDADO_BOLIVAR > 0) %>% 
  group_by(FECHA_MOVIMIENTO) %>% 
  summarise(fecha_sini = min(FECHA_AVISO),
            suma_liq = sum(LIQUIDADO_BOLIVAR),
            exp_max = max(EXPEDIENTE),
            expe_dist = n_distinct(EXPEDIENTE),
            sum_liq = sum(LIQUIDADO_BOLIVAR),
            .groups = "drop") %>% 
  arrange(FECHA_MOVIMIENTO) %>% 
  mutate(FECHA_MOV_LAG = lag(FECHA_MOVIMIENTO), 
         ELAPSED_TIME = as.numeric(FECHA_MOVIMIENTO-FECHA_MOV_LAG)
  ) %>% 
  mutate(año_mes = year(FECHA_MOVIMIENTO)*100+month(FECHA_MOVIMIENTO)) %>% 
  mutate(año_mes = ymd(str_c(año_mes,"01")))
fechas_completas <- tibble(fecha_comp = seq(min(datos_completos[["año_mes"]]),max(datos_completos[["año_mes"]]),by="month"))
datos_opti <- fechas_completas %>% 
              left_join(datos_completos, by=c("fecha_comp"="año_mes"))%>% 
              group_by(fecha_comp) %>% 
              summarise(incrementos = sum(!is.na(FECHA_MOVIMIENTO)),
                        fecha_sini = ymd("2011-09-21"),
                        .groups = "drop") %>% 
  arrange(fecha_comp) %>% 
  mutate(proceso_Nt = cumsum(incrementos))%>% 
  mutate(time_elapsed_t = interval(fecha_comp, fecha_sini)) %>% 
  mutate(time_elapsed_t = time_elapsed_t %/% months(1)) %>% 
  mutate(time_elapsed_t = as.numeric(time_elapsed_t) %>% abs())

  sum(datos_completos[["sum_liq"]])  
  
test_kolmogrov <- ks.test(datos_completos[["sum_liq"]] , "pexp", 1/mean(yy[["sum_liq"]]))

######################################################################
expected_value = function(delta,beta,rho,alpha,a,t)
{
  if(delta*beta <= 1)
  {
    retorno = Inf
    return(retorno)
  }
  else
  {
    retorno = t*(rho/alpha + a*delta)/(delta - 1/beta)
    return(retorno)
  }
}
func_opti <- function(vec_parameters)
{
  ##vec_parameters: parameters of function delta,beta,rho,alpha,a
  datos_opti <- datos_opti %>% 
                     mutate(expected_value = expected_value(delta = vec_parameters[1],
                                                            beta = vec_parameters[2],
                                                            rho = vec_parameters[3],
                                                            alpha = vec_parameters[4],
                                                            a = vec_parameters[5],
                                                            t = time_elapsed_t)
                            ) %>% 
                     mutate(diference = (expected_value - proceso_Nt)^2)  
  N_rows = nrow(datos_opti)
  rmse = sum(datos_opti[["diference"]])/N_rows
  rmse
}
func_optiv2 <- function(vec_parameters)
{
  ##vec_parameters: parameters of function delta,beta,rho,alpha,a
  datos_opti <- datos_opti %>% 
    mutate(expected_value = expected_value(delta = vec_parameters[1],
                                           beta = vec_parameters[2],
                                           rho = vec_parameters[3],
                                           alpha = vec_parameters[4],
                                           a = vec_parameters[5],
                                           t = 1)
    ) %>% 
    mutate(diference = (expected_value - incrementos)^2)  
  N_rows = nrow(datos_opti)
  rmse = sum(datos_opti[["diference"]])/N_rows
  rmse
}
##########################################################################################
lower_bound = c(0,0,0.01,0,0)
upper_bound = c(10, 15, 0.03, 20, 0)*1 # Con cota superior, de no requerir cambiar el valor de 0.03 al deseado. 
matriz_candinates <- map2(lower_bound,upper_bound,~seq(.x,.y,length.out = 100)) %>% 
  bind_cols() %>% as.matrix()
ctr = DEoptim.control(itermax = 1000,VTR = 1.1,initialpop=matriz_candinates)
aa <- DEoptim(fn = func_optiv2,lower = lower_bound,upper = upper_bound,control = ctr) 
bb <- DEoptim(fn = func_optiv2,lower = lower_bound,upper = upper_bound,control = ctr) 
aa$optim
bb$optim

datos_opti %>% 
  mutate(expected_observed_ver1 = expected_value(delta = 1.153579,beta = 5.615616,rho = 9.616243,alpha = 6.913208,a = 1.369586,t = time_elapsed_t)) %>% 
  mutate(expected_observed_ver2 = expected_value(delta = 6.179476e+04 ,beta = 4.772110e+04,rho = 6.667991e+04,alpha = 1.059555e+05,a = 1.425916e+00,t = time_elapsed_t)) %>% 
  View()
