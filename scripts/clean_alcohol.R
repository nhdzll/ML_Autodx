
#Importamos el reporte de alcohol
###############################################################################
alcohol <- read.csv("./data/raw/autodiagnostico_alcohol.csv", 
                  header = TRUE, na.strings = "")

#Limpieza de variables
###############################################################################

alcohol$LGBTI <- factor(alcohol$LGBTI, levels = c("No", "Si"))
alcohol$INDIGENA <- factor(alcohol$INDIGENA, levels = c("No", "Si"))
alcohol$ESTADO <- factor(alcohol$ESTADO)

#Creación de nuevas variables
###############################################################################
alcohol$sexo[alcohol$SEXO == "Hombre"] <- "Hombre"
alcohol$sexo[alcohol$SEXO == "Hombre Cisgénero"] <- "Hombre"
alcohol$sexo[alcohol$SEXO == "Hombre Trans"] <- "Hombre"
alcohol$sexo[alcohol$SEXO == "Mujer"] <- "Mujer"
alcohol$sexo[alcohol$SEXO == "Mujer Cisgénero"] <- "Mujer"
alcohol$sexo[alcohol$SEXO == "Mujer Trans"] <- "Mujer"
alcohol$sexo[alcohol$SEXO == "No deseo especificar"] <- "Otro"
alcohol$sexo[alcohol$SEXO == "Otro"] <- "Otro"
alcohol$sexo[alcohol$SEXO == "Queer"] <- "Otro"
alcohol$sexo[alcohol$SEXO == "Persona intersexual"] <- "Otro"
alcohol$sexo[alcohol$SEXO == "Persona no binaria"] <- "Otro"

alcohol$sexo <- factor(alcohol$sexo, levels = c("Hombre", "Mujer", "Otro"))

alcohol$date <- lubridate::dmy_hm(alcohol$FECHA.CAPTURA, tz = "Mexico/General")
alcohol$FECHA.CAPTURA <- NULL

#alcohol$puntos <- rowSums(alcohol[,c("R1", "R2", "R3", "R4", "R5", "R6", "R7", 
#                                 "R8", "R9", "R10")])

#Evaluamos la confiabilidad interna del instrumento de alcoholsión (Alfa = 0.943)
print(ltm::cronbach.alpha(alcohol[, c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", 
                              "R10")]))

alcohol$year <- lubridate::year(alcohol$date)
alcohol$year <- factor(alcohol$year, levels = c(2021, 2022))

alcohol$month <- lubridate::month(alcohol$date, label = TRUE, abbr = TRUE)
alcohol$month <- factor(alcohol$month, levels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"))

alcohol$day <- lubridate::day(alcohol$date)

alcohol$wday <- lubridate::wday(alcohol$date, label = TRUE, abbr = TRUE, week_start = 1)
alcohol$wday <- substr(alcohol$wday, start = 1, stop = 3)
alcohol$wday <- factor(alcohol$wday, levels = c("lun", "mar", "mié", "jue", "vie", "sáb", "dom"))

alcohol$hour <- lubridate::hour(alcohol$date)
alcohol$minute <- lubridate::minute(alcohol$date)

alcohol$solici_atend[!is.na(alcohol$NOMBRE)] <- 1
alcohol$solici_atend[is.na(alcohol$NOMBRE)] <- 0
alcohol$solici_atend <- factor(alcohol$solici_atend, levels = c(1,0))

alcohol$EDAD <- abs(alcohol$EDAD)

alcohol <- subset(alcohol, date < as.POSIXct("2022-06-20 16:20", 
                                             format = "%Y-%m-%d %H:%M"))

#Eliminamos las columnas de los reactivos (Datos de identificación y rectivos individuales)

alcohol$NOMBRE <- NULL
alcohol$APELLIDO.UNO <- NULL
alcohol$APELLIDO.DOS <- NULL
alcohol$EMAIL <- NULL
alcohol$TELEFONO <- NULL
alcohol$SEXO <- NULL
alcohol$RESULTADO <- NULL
alcohol$CEC <- NULL
alcohol$EMP.ATENDIO <- NULL
alcohol$EMP.DERIVO <- NULL
alcohol$ATENCION.BRINDADA <- NULL
alcohol$CIJ.DERIVADO <- NULL
alcohol$EXPEDIENTE <- NULL
alcohol$FECHA.DERIVACION <- NULL

alcohol$FOLIO <- NULL
alcohol$FECHA.DE.ATENCION <- NULL
alcohol$MUNICIPIO <- NULL
alcohol$CP <- NULL
alcohol$DIAS.TRANSCURRIDOS <- NULL
#alcohol$date <- NULL

