
#Importamos el reporte de tabaco
###############################################################################
tabaco <- read.csv("./data/raw/autodiagnostico_tabaco.csv", 
                  header = TRUE, na.strings = "")

#Limpieza de variables
###############################################################################

tabaco$LGBTI <- factor(tabaco$LGBTI, levels = c("No", "Si"))
tabaco$INDIGENA <- factor(tabaco$INDIGENA, levels = c("No", "Si"))
tabaco$ESTADO <- factor(tabaco$ESTADO)

#Creación de nuevas variables
###############################################################################
tabaco$sexo[tabaco$SEXO == "Hombre"] <- "Hombre"
tabaco$sexo[tabaco$SEXO == "Hombre Cisgénero"] <- "Hombre"
tabaco$sexo[tabaco$SEXO == "Hombre Trans"] <- "Hombre"
tabaco$sexo[tabaco$SEXO == "Mujer"] <- "Mujer"
tabaco$sexo[tabaco$SEXO == "Mujer Cisgénero"] <- "Mujer"
tabaco$sexo[tabaco$SEXO == "Mujer Trans"] <- "Mujer"
tabaco$sexo[tabaco$SEXO == "No deseo especificar"] <- "Otro"
tabaco$sexo[tabaco$SEXO == "Otro"] <- "Otro"
tabaco$sexo[tabaco$SEXO == "Queer"] <- "Otro"
tabaco$sexo[tabaco$SEXO == "Persona intersexual"] <- "Otro"
tabaco$sexo[tabaco$SEXO == "Persona no binaria"] <- "Otro"

tabaco$sexo <- factor(tabaco$sexo, levels = c("Hombre", "Mujer", "Otro"))

tabaco$date <- lubridate::dmy_hm(tabaco$FECHA.CAPTURA, tz = "Mexico/General")
tabaco$FECHA.CAPTURA <- NULL

#tabaco$puntos <- rowSums(tabaco[,c("R1", "R2", "R3", "R4", "R5", "R6")])

#Evaluamos la confiabilidad interna del instrumento de tabacosión (Alfa = 0.943)
print(ltm::cronbach.alpha(tabaco[, c("R1", "R2", "R3", "R4", "R5", "R6")]))

tabaco$year <- lubridate::year(tabaco$date)
tabaco$year <- factor(tabaco$year, levels = c(2021, 2022))

tabaco$month <- lubridate::month(tabaco$date, label = TRUE, abbr = TRUE)
tabaco$month <- factor(tabaco$month, levels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"))

tabaco$day <- lubridate::day(tabaco$date)

tabaco$wday <- lubridate::wday(tabaco$date, label = TRUE, abbr = TRUE, week_start = 1)
tabaco$wday <- substr(tabaco$wday, start = 1, stop = 3)
tabaco$wday <- factor(tabaco$wday, levels = c("lun", "mar", "mié", "jue", "vie", "sáb", "dom"))

tabaco$hour <- lubridate::hour(tabaco$date)

tabaco$minute <- lubridate::minute(tabaco$date)

tabaco$solici_atend[!is.na(tabaco$NOMBRE)] <- 1
tabaco$solici_atend[is.na(tabaco$NOMBRE)] <- 0
table(tabaco$solici_atend)
#FALSE   TRUE 
#14925  3658 

tabaco$solici_atend <- factor(tabaco$solici_atend, levels = c(1,0))
tabaco$EDAD <- abs(tabaco$EDAD)

#Eliminamos las columnas de los reactivos (Datos de identificación y rectivos individuales)
tabaco$NOMBRE <- NULL
tabaco$APELLIDO.UNO <- NULL
tabaco$APELLIDO.DOS <- NULL
tabaco$EMAIL <- NULL
tabaco$TELEFONO <- NULL
tabaco$SEXO <- NULL
tabaco$RESULTADO <- NULL
tabaco$CEC <- NULL
tabaco$EMP.ATENDIO <- NULL
tabaco$EMP.DERIVO <- NULL
tabaco$ATENCION.BRINDADA <- NULL
tabaco$CIJ.DERIVADO <- NULL
tabaco$EXPEDIENTE <- NULL
tabaco$FECHA.DERIVACION <- NULL


tabaco$FOLIO <- NULL
tabaco$FECHA.DE.ATENCION <- NULL
tabaco$MUNICIPIO <- NULL
tabaco$CP <- NULL
tabaco$DIAS.TRANSCURRIDOS <- NULL
#tabaco$date <- NULL
tabaco$VÃ.A.CONSUMO <- NULL