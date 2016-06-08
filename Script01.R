###################################################
#########     Análisis tweets CouchDB     #########
###################################################

install.packages('R4CouchDB', dependencies = TRUE)
library(R4CouchDB)
ls("package:R4CouchDB")

# Ping
fromJSON(getURLContent("http://localhost:5984"))

# Creamos base
httpPUT("http://localhost:5984/base")

# Enviamos objetos de R a CouchDB
jfit <- toJSON(rnorm(10))
fromJSON(httpPUT("http://localhost:5984/base/fit", jfit))

# Listamos los documentos
index <- fromJSON(httpGET("http://localhost:5984/my_database/_all_docs"))$rows

# Apuntamos a la base
cdb <- cdbIni()
cdb$DBName <- "my_database"

# Obtenemos los documentos por medio de su "id"
datos <- ldply(seq(length(index)), function(i){cdb$id <- index[[i]]$id; unlist(cdbGetDoc(cdb)$res)})
datos
