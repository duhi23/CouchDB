###################################################
#########     Análisis tweets CouchDB     #########
###################################################

install.packages('R4CouchDB', dependencies = TRUE)
library(R4CouchDB)
library(plyr)
ls("package:R4CouchDB")

# Ping
fromJSON(getURLContent("http://localhost:5984"))

# Creamos base
httpPUT("http://localhost:5984/base")

# Enviamos objetos de R a CouchDB
jfit <- toJSON(rnorm(10))
fromJSON(httpPUT("http://localhost:5984/base/fit", jfit))


# Listamos los documentos
index <- fromJSON(httpGET("http://localhost:5984/tweets_corregido/_all_docs"))$rows
str(index)

# Apuntamos a la base
cdb <- cdbIni()
cdb$DBName <- "tweets_corregido"

# Obtenemos los documentos por medio de su "id"

reg <- function(i){
      cdb$id <- index[[i]]$id;
      return(unlist(cdbGetDoc(cdb)$res[c(8,9,16,3,5,6,7,10,11,12,15,17)]))
}

info <- reg(1)
for(i in 2:22799){
      info <- rbind(info, reg(i))      
}

str(info)
dim(info)

# Formato variables
library(ggvis)
library(dplyr)
library(ggplot2)
library(ggfortify)

fecha <- as.Date(info[,1], format="%a %b %d %H:%M:%S %z %Y")

data <- data.frame(fecha=fecha, ciudad=info[,2], pais=info[,3], gutierres=as.numeric(info[,4]),
                   noboa=as.numeric(info[,5]), gonzalez=as.numeric(info[,6]), correa=as.numeric(info[,7]),
                   tiban=as.numeric(info[,8]), glass=as.numeric(info[,9]), rodas=as.numeric(info[,10]),
                   lasso=as.numeric(info[,11]))


data %>% filter(gutierres != 999) %>% select(fecha, gutierres) %>% table() %>% as.data.frame() %>% 
      ggvis(x = ~gutierres, y = ~Freq) %>% layer_bars()

gdata <- data %>% filter(gutierres != 999) %>% select(fecha, gutierres) %>% table() %>% as.data.frame()

ggplot(gdata, aes(x=fecha, y=Freq)) + 

autoplot(ts(round(c(info[,4], est4-9),0), start = c(2006,1), frequency = 12)) +
      geom_vline(aes(xintercept = as.numeric(as.Date("2016-04-01"))), colour = 'red', linetype = 'longdash')

data %>% table()

