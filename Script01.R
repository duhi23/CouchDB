###################################################
#########     Análisis tweets CouchDB     #########
###################################################

# Librerias para la conexión a CouchDB
install.packages('R4CouchDB', dependencies = TRUE)
library(R4CouchDB)
library(plyr)
ls("package:R4CouchDB")

# Ping
fromJSON(getURLContent("http://localhost:5984"))

# Listamos los documentos locales
index <- fromJSON(httpGET("http://localhost:5984/tweets_corregido/_all_docs"))$rows
str(index)

# Apuntamos a la base
cdb <- cdbIni()
cdb$DBName <- "tweets_corregido"

# Obtenemos los documentos por medio de su "id", unicamente las variables de interés
reg <- function(i){
      cdb$id <- index[[i]]$id;
      return(unlist(cdbGetDoc(cdb)$res[c(8,9,16,3,5,6,7,10,11,12,15,17)]))
}

# Consolidamos los tweets en una tabla
info <- reg(1)
for(i in 2:22799){
      info <- rbind(info, reg(i))      
}

# Revisamos la nueva estructura y dimensiones
str(info)
dim(info)

# Cargamos librerias para la visualización
library(ggvis)
library(dplyr)
library(ggplot2)
library(ggfortify)

# Damos formato a la variable fecha
fecha <- as.Date(info[,1], format="%a %b %d %H:%M:%S %z %Y")

# Consolidamos la tabla final
data <- data.frame(fecha=fecha, ciudad=info[,2], pais=info[,3], gutierres=as.numeric(info[,4]),
                   noboa=as.numeric(info[,5]), gonzalez=as.numeric(info[,6]), correa=as.numeric(info[,7]),
                   tiban=as.numeric(info[,8]), glass=as.numeric(info[,9]), rodas=as.numeric(info[,10]),
                   lasso=as.numeric(info[,11]))

# Gutierrez
gdata <- data %>% filter(gutierres != 999) %>% select(fecha, gutierres) %>% table() %>% as.data.frame()
ggplot(gdata, aes(x=fecha, y=Freq, fill=gutierres)) + geom_bar(stat="identity")
ggplot(gdata, aes(x=fecha, y=Freq)) + geom_bar(stat="identity") + facet_grid(gutierres ~ .)

# Noboa
ndata <- data %>% filter(noboa != 999) %>% select(fecha, noboa) %>% table() %>% as.data.frame()
ggplot(ndata, aes(x=fecha, y=Freq, fill=noboa)) + geom_bar(stat="identity")
ggplot(ndata, aes(x=fecha, y=Freq)) + geom_bar(stat="identity") + facet_grid(noboa ~ .)

# Gonzalez
zdata <- data %>% filter(gonzalez != 999) %>% select(fecha, gonzalez) %>% table() %>% as.data.frame()
ggplot(zdata, aes(x=fecha, y=Freq, fill=gonzalez)) + geom_bar(stat="identity")
ggplot(zdata, aes(x=fecha, y=Freq)) + geom_bar(stat="identity") + facet_grid(gonzalez ~ .)

# Correa
cdata <- data %>% filter(correa != 999) %>% select(fecha, correa) %>% table() %>% as.data.frame()
ggplot(cdata, aes(x=fecha, y=Freq, fill=correa)) + geom_bar(stat="identity")
ggplot(cdata, aes(x=fecha, y=Freq)) + geom_bar(stat="identity") + facet_grid(correa ~ .)

# Tiban
tdata <- data %>% filter(tiban != 999) %>% select(fecha, tiban) %>% table() %>% as.data.frame()
ggplot(tdata, aes(x=fecha, y=Freq, fill=tiban)) + geom_bar(stat="identity")
ggplot(tdata, aes(x=fecha, y=Freq)) + geom_bar(stat="identity") + facet_grid(tiban ~ .)

# Glass
sdata <- data %>% filter(glass != 999) %>% select(fecha, glass) %>% table() %>% as.data.frame()
ggplot(sdata, aes(x=fecha, y=Freq, fill=glass)) + geom_bar(stat="identity")
ggplot(sdata, aes(x=fecha, y=Freq)) + geom_bar(stat="identity") + facet_grid(glass ~ .)

# Rodas
ddata <- data %>% filter(rodas != 999) %>% select(fecha, rodas) %>% table() %>% as.data.frame()
ggplot(ddata, aes(x=fecha, y=Freq, fill=rodas)) + geom_bar(stat="identity")
ggplot(ddata, aes(x=fecha, y=Freq)) + geom_bar(stat="identity") + facet_grid(rodas ~ .)

# Lasso
adata <- data %>% filter(lasso != 999) %>% select(fecha, lasso) %>% table() %>% as.data.frame()
ggplot(adata, aes(x=fecha, y=Freq, fill=lasso)) + geom_bar(stat="identity")
ggplot(adata, aes(x=fecha, y=Freq)) + geom_bar(stat="identity") + facet_grid(lasso ~ .)
