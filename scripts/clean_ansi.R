#Importamos el reporte de ansiedad
###############################################################################
ansi <- read.csv("./data/raw/Autodiagnostico_ansiedad.csv", 
                 header = TRUE, na.strings = "")

#Limpieza de variables
###############################################################################

#print(lapply(ansi, function(x) sum(is.na(x))))

ansi$LGBTI <- factor(ansi$LGBTI, levels = c("No", "Si"))
ansi$INDIGENA <- factor(ansi$INDIGENA, levels = c("No", "Si"))
ansi$ESTADO <- factor(ansi$ESTADO)

#Creación de nuevas variables
###############################################################################
ansi$sexo[ansi$SEXO == "Hombre"] <- "Hombre"
ansi$sexo[ansi$SEXO == "Hombre Cisgénero"] <- "Hombre"
ansi$sexo[ansi$SEXO == "Hombre Trans"] <- "Hombre"
ansi$sexo[ansi$SEXO == "Mujer"] <- "Mujer"
ansi$sexo[ansi$SEXO == "Mujer Cisgénero"] <- "Mujer"
ansi$sexo[ansi$SEXO == "Mujer Trans"] <- "Mujer"
ansi$sexo[ansi$SEXO == "No deseo especificar"] <- "Otro"
ansi$sexo[ansi$SEXO == "Otro"] <- "Otro"
ansi$sexo[ansi$SEXO == "Queer"] <- "Otro"
ansi$sexo[ansi$SEXO == "Persona intersexual"] <- "Otro"
ansi$sexo[ansi$SEXO == "Persona no binaria"] <- "Otro"

ansi$sexo <- factor(ansi$sexo, levels = c("Hombre", "Mujer", "Otro"))

ansi$date <- lubridate::dmy_hm(ansi$FECHA.CAPTURA, tz = "Mexico/General")
ansi$FECHA.CAPTURA <- NULL

#ansi$puntos <- rowSums(ansi[,c("R1", "R2", "R3", "R4", "R5", "R6", "R7", 
#                                 "R8", "R9", "R10", "R11", "R12", "R13", "R14", 
#                                 "R15", "R16", "R17", "R18", "R19", "R20", "R21")])

#Evaluamos la confiabilidad interna del instrumento de ansiedad (Alfa = 0.94)
print(ltm::cronbach.alpha(ansi[, c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", 
                              "R10", "R11", "R12", "R13", "R14", "R15", "R16", "R17", 
                              "R18", "R19", "R20", "R21")]))

ansi$year <- lubridate::year(ansi$date)
ansi$year <- factor(ansi$year, levels = c(2021, 2022))

ansi$month <- lubridate::month(ansi$date, label = TRUE, abbr = TRUE)
ansi$month <- factor(ansi$month, levels = c("ene", "feb", "mar", "abr", 
                                            "may", "jun", "jul", "ago", "sep", 
                                            "oct", "nov", "dic"))
ansi$day <- lubridate::day(ansi$date)

ansi$wday <- lubridate::wday(ansi$date, label = TRUE, abbr = TRUE, week_start = 1)
ansi$wday <- substr(ansi$wday, start = 1, stop = 3)
ansi$wday <- factor(ansi$wday, levels = c("lun", "mar", "mié", "jue", "vie", "sáb", "dom"))

ansi$hour <- lubridate::hour(ansi$date)

ansi$minute <- lubridate::minute(ansi$date)

ansi$solici_atend[!is.na(ansi$NOMBRE)] <- 1
ansi$solici_atend[is.na(ansi$NOMBRE)] <- 0
ansi$solici_atend <- factor(ansi$solici_atend, levels = c(1,0))

ansi$EDAD <- abs(ansi$EDAD)

#Eliminamos las columnas de los reactivos (Datos de identificación y rectivos individuales)
ansi$NOMBRE <- NULL
ansi$APELLIDO.UNO <- NULL
ansi$APELLIDO.DOS <- NULL
ansi$EMAIL <- NULL
ansi$TELEFONO <- NULL
ansi$SEXO <- NULL
ansi$RESULTADO <- NULL
ansi$CEC <- NULL
ansi$EMP.ATENDIO <- NULL
ansi$EMP.DERIVO <- NULL
ansi$ATENCION.BRINDADA <- NULL
ansi$CIJ.DERIVADO <- NULL
ansi$EXPEDIENTE <- NULL
ansi$FECHA.DERIVACION <- NULL

ansi$FOLIO <- NULL
ansi$FECHA.DE.ATENCION <- NULL
ansi$MUNICIPIO <- NULL
ansi$CP <- NULL
ansi$DIAS.TRANSCURRIDOS <- NULL
#ansi$date <- NULL