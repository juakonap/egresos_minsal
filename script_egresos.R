rm(list=ls())
library(data.table)
library(xlsx)

setwd("C:/Users/joaquin/Dropbox/Minsal/Egresos Minsal/datos")

#egresos_link <- read.table(file = "C:/Users/joaquin/Dropbox/Rodolfo/Egresos Minsal/manuales/egresos_enlaces_descarga.txt", sep = "\n", stringsAsFactors = F)

#temp <- tempfile()
#datos_egresos <- lapply(egresos_link[c(1,3,4),], function(x){download.file(x,temp);fread(unzip(zipfile = temp))})
#unlink(temp)
#gc()


archivos_disponibles <- list.files(pattern = ".zip")
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
                                                                                 dias_estadia = cut(as.numeric(DIAS_ESTAD),c(-Inf,1,2,3,4,5,6,7,14,30,+Inf), labels = c("1 día","2 días","3 días","4 días","5 días","6 días","1 semana","Entre 1 y 2 semanas","Entre 2 semanas y un mes","+1 mes")),
                                                                                 condicion_egreso = ifelse(COND_EGR == "1", "Vivo","Fallecido"),
                                                                                 intervencion_quirurgica = ifelse(INTERV_Q == "1", "Si","No"))
                                                                          ][, c("ESTAB","Seremi","ServicioSalud","SEXO","EDAD","PREVI","BENEF",
                                                                                "MOD","COMUNA","REGION","SERV_RES","FECHA_EGR","SERC_EGR","DIAG1",
                                                                                "DIAG2","DIAS_ESTAD","COND_EGR","INTERV_Q") := NULL
                                                                          ][, `:=`(anio_egreso = year(fecha_egreso), mes_egreso = month(fecha_egreso), dia_egreso = mday(fecha_egreso))]
file.remove(list.files(pattern=".csv"))


#tablas auxiliares

cod_comuna_serv_salud <- as.data.table(read.xlsx2("tablas_2015/DPA2015.xls", sheetName="DPA2015",encoding="UTF-8",stringsAsFactors = F))[,"encoding":=NULL
                                                                                                                                        ][, `:=`(nombre_comuna = Nombre.Comuna,
                                                                                                                                                 cod_comuna_2010 = as.numeric(Código.Comuna.desde.2010),
                                                                                                                                                 cod_provincia_2010 = as.numeric(Código.Provincia.desde.2010),
                                                                                                                                                 nombre_provincia_2010 = Provincia.desde.2010,
                                                                                                                                                 cod_servicio_2008 = as.numeric(Código.Servicio.Salud.desde.2008),
                                                                                                                                                 nombre_servicio_2008 = Nombre.Servicio.de.Salud.desde.2008)
                                                                                                                                        ][, names(cod_comuna_serv_salud)[grep("[.]",names(cod_comuna_serv_salud))]:=NULL]


cod_establecimiento <- as.data.table(read.xlsx2("tablas_2015/Establecimientos 2015.xls", sheetName="2015", encoding = "UTF-8",stringsAsFactors = F))[,"encoding":=NULL
                                                                                                                                                    ][, `:=`(region = as.numeric(Región),
                                                                                                                                                             id_servicio = as.numeric(Id.Servicio),
                                                                                                                                                             seremi = as.numeric(SEREMI),
                                                                                                                                                             nombre_comuna = Nombre.Comuna,
                                                                                                                                                             cod_establecimiento = as.numeric(Codigo.Establecimiento),
                                                                                                                                                             tipo_establecimiento = Nombre.Tipo.de.Establecimiento,
                                                                                                                                                             nombre_establecimiento = Nombre,
                                                                                                                                                             establecimiento = Establecimiento.)
                                                                                                                                                    ][, c("Región","Id.Servicio","SEREMI","Id..Comuna",
                                                                                                                                                          "Nombre.Comuna","Nombre.Tipo.de.Establecimiento",
                                                                                                                                                          "Codigo.Establecimiento","Nombre","Establecimiento."):=NULL]


cod_serv_medico <- as.data.table(read.xlsx2("tablas_2015/Servicio clinico o nivel de cuidado.xlsx", sheetName = "Hoja1", encoding = "UTF-8",stringsAsFactors = F))[,"encoding":=NULL
                                                                                                                                                                  ][,`:=`(cod_servicio_clinico = as.numeric(Codigo.servicio.clínico.Nivel.de.cuidado),
                                                                                                                                                                         nombre_servicio_clinico = Nombre.servicio.clínico)
                                                                                                                                                                  ][, c("Codigo.servicio.clínico.Nivel.de.cuidado","Nombre.servicio.clínico"):=NULL]
cod_diag_principal <- as.data.table(read.xlsx2("tablas_2015/DIAG1.xlsx", sheetName = "Hoja1", encoding = "UTF-8",stringsAsFactors = F))[,"encoding":=NULL]


#Cruces de variables

#par(mfrow=c(2,1))
#ts.plot(datos[between(fecha_egreso,"2015-01-01","2015-03-31"),.N, by = "fecha_egreso"][,N], ylab = "")
#ts.plot(datos[between(fecha_egreso,"2015-04-01","2015-06-30"),.N, by = "fecha_egreso"][,N], ylab = "")
#ts.plot(datos[between(fecha_egreso,"2015-07-01","2015-09-30"),.N, by = "fecha_egreso"][,N], ylab = "")
#ts.plot(datos[between(fecha_egreso,"2015-10-01","2015-12-31"),.N, by = "fecha_egreso"][,N], ylab = "")

