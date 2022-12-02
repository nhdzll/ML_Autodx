#Importamos el reporte de depresión
###############################################################################
depre <- read.csv("./data/raw/autodiagnostico_depresion.csv", 
                  header = TRUE, na.strings = "")

#Limpieza de variables
###############################################################################
depre$SEXO[depre$FOLIO == "10317"] <- "No deseo especificar" #Corregimos un valor vacio de SEXO

#print(lapply(depre, function(x) sum(is.na(x))))

depre$LGBTI <- factor(depre$LGBTI, levels = c("No", "Si"))
depre$INDIGENA <- factor(depre$INDIGENA, levels = c("No", "Si"))

#Creación de nuevas variables
###############################################################################
depre$sexo[depre$SEXO == "Hombre"] <- "Hombre"
depre$sexo[depre$SEXO == "Hombre Cisgénero"] <- "Hombre"
depre$sexo[depre$SEXO == "Hombre Trans"] <- "Hombre"
depre$sexo[depre$SEXO == "Mujer"] <- "Mujer"
depre$sexo[depre$SEXO == "Mujer Cisgénero"] <- "Mujer"
depre$sexo[depre$SEXO == "Mujer Trans"] <- "Mujer"
depre$sexo[depre$SEXO == "No deseo especificar"] <- "Otro"
depre$sexo[depre$SEXO == "Otro"] <- "Otro"
depre$sexo[depre$SEXO == "Queer"] <- "Otro"
depre$sexo[depre$SEXO == "Persona intersexual"] <- "Otro"
depre$sexo[depre$SEXO == "Persona no binaria"] <- "Otro"

depre$date <- lubridate::dmy_hm(depre$FECHA.CAPTURA, tz = "Mexico/General")
depre$FECHA.CAPTURA <- NULL

depre$puntos <- rowSums(depre[,c("R1", "R2", "R3", "R4", "R5", "R6", "R7", 
                                 "R8", "R9", "R10", "R11", "R12", "R13", "R14", 
                                 "R15", "R16", "R17", "R18", "R19", "R20", "R21")])

#Evaluamos la confiabilidad interna del instrumento de depresión (Alfa = 0.943)
print(ltm::cronbach.alpha(depre[, c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", 
                              "R10", "R11", "R12", "R13", "R14", "R15", "R16", "R17", 
                              "R18", "R19", "R20", "R21")]))

depre$year <- lubridate::year(depre$date)
depre$year <- factor(depre$year, levels = c(2021, 2022))

depre$month <- lubridate::month(depre$date, label = TRUE, abbr = TRUE)
depre$month <- factor(depre$month, levels = c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"))

depre$day <- lubridate::day(depre$date)

depre$wday <- lubridate::wday(depre$date, label = TRUE, abbr = TRUE, week_start = 1)
depre$wday <- substr(depre$wday, start = 1, stop = 3)
depre$wday <- factor(depre$wday, levels = c("lun", "mar", "mié", "jue", "vie", "sáb", "dom"))

depre$hour <- lubridate::hour(depre$date)

depre$minute <- lubridate::minute(depre$date)

depre$solici_atend[!is.na(depre$NOMBRE)] <- 1
depre$solici_atend[is.na(depre$NOMBRE)] <- 0
depre$solici_atend <- factor(depre$solici_atend, levels = c(1,0))
#FALSE   TRUE 
#114450  2839 = 2.48%

depre$EDAD <- abs(depre$EDAD)

#Eliminamos las columnas de los reactivos (Datos de identificación y rectivos individuales)
depre$NOMBRE <- NULL
depre$APELLIDO.UNO <- NULL
depre$APELLIDO.DOS <- NULL
depre$EMAIL <- NULL
depre$TELEFONO <- NULL
depre$SEXO <- NULL
depre$RESULTADO <- NULL
depre$CEC <- NULL
depre$EMP.ATENDIO <- NULL
depre$EMP.DERIVO <- NULL
depre$ATENCION.BRINDADA <- NULL
depre$CIJ.DERIVADO <- NULL
depre$EXPEDIENTE <- NULL
depre$FECHA.DERIVACION <- NULL

depre$FOLIO <- NULL
depre$FECHA.DE.ATENCION <- NULL
depre$MUNICIPIO <- NULL
depre$CP <- NULL
depre$DIAS.TRANSCURRIDOS <- NULL
#depre$date <- NULL
depre$puntos <- NULL