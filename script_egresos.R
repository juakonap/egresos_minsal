rm(list=ls())
library(data.table)
library(xlsx)

#setwd("your_work_directory")

temp <- tempfile()
datos_egresos <- lapply("https://github.com/juakonap/Egresos-Minsal/raw/master/datos/ssegreso2009.zip", function(x){download.file(x,temp);fread(unzip(zipfile = temp))})
unlink(temp)
gc()

temp <- tempfile()
download.file("https://github.com/juakonap/Egresos-Minsal/raw/master/datos/ssegreso2011.zip",temp)
datos_egresos <- fread(unzip(zipfile = temp))
unlink(temp)
gc()



#lista_datos <- lapply(archivos_disponibles[length(archivos_disponibles)], function(x) fread(unzip(x)))
datos <- fread(unzip(archivos_disponibles[length(archivos_disponibles)]))[, `:=`(establecimiento = as.numeric(ESTAB),
                                                                                 seremi = as.numeric(Seremi),
                                                                                 serv_salud=as.numeric(ServicioSalud),
                                                                                 sexo = as.numeric(SEXO),
                                                                                 edad = as.numeric(EDAD),
                                                                                 prevision = as.numeric(PREVI),
                                                                                 tramo_fonasa = as.factor(BENEF),
                                                                                 modalidad_atencion = as.factor(MOD),
                                                                                 comuna = as.numeric(COMUNA),
                                                                                 region = ifelse(!(REGION %in% c(1:15)),NA, as.numeric(REGION)),
                                                                                 servicio_salud_ref = as.numeric(SERV_RES),
                                                                                 fecha_egreso = as.IDate(FECHA_EGR, format = "%Y-%m-%d"),
                                                                                 servicio_egreso = as.numeric(SERC_EGR),
                                                                                 diag_principal = DIAG1,
                                                                                 diag_causa_externa = DIAG2,
                                                                                 dias_estadia = cut(as.numeric(DIAS_ESTAD),c(-Inf,1,2,3,4,5,6,7,14,30,+Inf), labels = c("1 d?a","2 d?as","3 d?as","4 d?as","5 d?as","6 d?as","1 semana","Entre 1 y 2 semanas","Entre 2 semanas y un mes","+1 mes")),
                                                                                 condicion_egreso = ifelse(COND_EGR == "1", "Vivo","Fallecido"),
                                                                                 intervencion_quirurgica = ifelse(INTERV_Q == "1", "Si","No"))
                                                                          ][, c("ESTAB","Seremi","ServicioSalud","SEXO","EDAD","PREVI","BENEF",
                                                                                "MOD","COMUNA","REGION","SERV_RES","FECHA_EGR","SERC_EGR","DIAG1",
                                                                                "DIAG2","DIAS_ESTAD","COND_EGR","INTERV_Q") := NULL
                                                                          ][, `:=`(anio_egreso = year(fecha_egreso), mes_egreso = month(fecha_egreso), dia_egreso = mday(fecha_egreso))]
file.remove(list.files(pattern=".csv"))


#tablas auxiliares

cod_comuna_serv_salud <- data.table(gdata::read.xls("http://github.com/juakonap/Egresos-Minsal/raw/master/tablas_auxiliares/2015/DPA2015.xls", header = F, skip = 1, stringsAsFactors = F))
names(cod_comuna_serv_salud) <- c("cod_comuna_hasta_99", "nombre_comuna", "cod_comuna_desde_00", "cod_comuna_desde_08", "cod_comuna_desde_10", 
                                  "prov_desde_00", "cod_prov_desde_00", "prov_desde_08", "cod_prov_desde_08", "prov_desde_10", "cod_prov_desde_10",
                                  "region_desde_00", "cod_region_desde_00", "region_desde_08", "cod_region_desde_08", "serv_salud_hasta_07", "cod_serv_salud_hasta_07", "serv_salud_desde_08", "cod_serv_salud_desde_08")


cod_establecimiento <- data.table(gdata::read.xls("http://github.com/juakonap/Egresos-Minsal/raw/master/tablas_auxiliares/2015/Establecimientos%202015.xls", header = F, skip = 1, stringsAsFactors = F))
names(cod_establecimiento) <- c("region", "id_servicio", "seremi", "id_comuna", "nombre_comuna", "nombre_tpo_establ", "cod_establ", "nombre", "establecimiento")


temp <- tempfile()
download.file("https://github.com/juakonap/Egresos-Minsal/raw/master/tablas_auxiliares/2015/Servicio%20clinico%20o%20nivel%20de%20cuidado.xlsx",temp, mode="wb")
cod_serv_medico <- data.table(xlsx::read.xlsx2(temp, sheetName = "Hoja1",stringsAsFactors = F, encoding="UTF-8", header=F, startRow=2))[, X1:=as.numeric(X1)][, "encoding":=NULL]
names(cod_serv_medico) <- c("cod_serv_clinico_niv_cuidado",	"nombre_serv_clinico")
unlink(temp)


temp <- tempfile()
download.file("https://github.com/juakonap/Egresos-Minsal/raw/master/tablas_auxiliares/2015/DIAG1.xlsx",temp, mode="wb")
cod_diag_principal  <- data.table(xlsx::read.xlsx2(temp, sheetName = "Hoja1",stringsAsFactors = F, encoding="UTF-8", header=F, startRow=2))[, "encoding":=NULL]
names(cod_diag_principal) <- c("codigo", "descriptor")
unlink(temp)

#Cruces de variables

#par(mfrow=c(2,1))
#ts.plot(datos[between(fecha_egreso,"2015-01-01","2015-03-31"),.N, by = "fecha_egreso"][,N], ylab = "")
#ts.plot(datos[between(fecha_egreso,"2015-04-01","2015-06-30"),.N, by = "fecha_egreso"][,N], ylab = "")
#ts.plot(datos[between(fecha_egreso,"2015-07-01","2015-09-30"),.N, by = "fecha_egreso"][,N], ylab = "")
#ts.plot(datos[between(fecha_egreso,"2015-10-01","2015-12-31"),.N, by = "fecha_egreso"][,N], ylab = "")

