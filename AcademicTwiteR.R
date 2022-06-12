#importa librerias

install.packages("academictwitteR")
install.packages("rtweet")

library(academictwitteR)
library(rtweet)
library(dplyr)
library(rlang)

#########################################################
#establece credenciales de la api para la conexion

set_bearer()
create_token(
  app = "HTspread",
  Sys.getenv('consumer_key'),
  Sys.getenv('consumer_secret'),
  access_token =Sys.getenv('access_token'),
  access_secret = Sys.getenv('access_secret'),
  set_renv = TRUE
)


##########################################################
#cuenta los tweets por query y devulve lista con la cantida de tweets con el query por dia
#también suma estos para contar  y los ordena por fecha para ver el progreso

counttw <-
  count_all_tweets( query = "lol",
                    start_tweets = "2006-05-03T00:00:00Z",
                    end_tweets = "2007-04-25T00:00:00.000Z",
                    file = "lolCount",
                    data_path = "data/",
                    n = Inf
  )
countw_ordenado <- counttw[order(counttw$start), ]
sum(countw_ordenado$tweet_count)
#representa con gráfico de barras los tweets por días
barplot(countw_ordenado$tweet_count, names=countw_ordenado$start )

######################################################
#llama a academictwitter para una petición full archive. ordena los resultados y los guarda. 
#en lugar de llamar a get_alltweets se puede guardar desde archivo

tweets <-
  get_all_tweets(
    query = "lol",
    start_tweets = "2006-05-04T00:00:00Z",
    end_tweets = "2007-04-25T00:00:00.000Z",
    file = "lol",
    data_path = "data/",
    n = Inf,
  )


 #########################################################
#Pretratamiento de lso datos
#Lee y guarda en dat el resultado guardado de get_all_tweets
dat <- readRDS("lol")
tweets=dat
#Crea la variable rt 
tweets$rt<-ifelse(substr(tweets$text,1,2)=='RT',TRUE,FALSE)

#ordena los tweets por fecha
tw_ordenado <- tweets[order(dat$created_at), ]

#eliminar usuario repetidos por haber usado el hastag varias veces. nos quedamos con el primero
#tw_dist=tw_ordenado %>% group_by(author_id) %>%slice_head(n=1)
tw_dist=tw_ordenado[1,]

for (i in 2:length(tw_ordenado$author_id)) {
  if(!(tw_ordenado$author_id[i]%in%tw_dist$author_id)){
    tw_dist=rbind(tw_dist,tw_ordenado[i,])
  }
}

#ordena de nuevo por fecha de creación
tw_ordenado <- tw_dist[order(tw_dist$created_at), ]

#Crea las variables dateformat y millisecs a partir de created_at
tw_ordenado$dateformat=as.POSIXct(tw_ordenado$created_at, format = "%Y-%m-%dT%H:%M:%OSZ")
tw_ordenado$mins=(as.numeric(tw_ordenado$dateformat)-as.numeric(tw_ordenado$dateformat[1]))/60
tw_ordenado$dia=trunc(tw_ordenado$mins/(24*60))

users=data.frame("author_id"=tw_ordenado$author_id,"created_at"=tw_ordenado$created_at, "rt_count"= tw_ordenado$public_metrics$retweet_count, "likescount"=tw_ordenado$public_metrics$like_count, "text"=tw_ordenado$text,"rt"=tw_ordenado$rt, "dateFormat"=tw_ordenado$dateformat,"mins"=tw_ordenado$mins,"dia"=tw_ordenado$dia)

#bucle para capturar el número de seguidores y de seguidos de los usuarios
friends <- data.frame(j = numeric(),userfollowedby = numeric(),userfollowing = numeric())
j=1;
repeat{
  user <- get_user_profile(users[j,1])
  friends[j,1]=j;
  print(j)
  if (is_empty(user$public_metrics$followers_count)) {
    friends[j,2]=-1
    friends[j,3]=-1
    print("User info not avliable")
  } else{
    friends[j,2]= user$public_metrics$followers_count
    friends[j,3]=user$public_metrics$following_count
  }
  j=j+1
  if(j>1000) {
    break
  }
}

#añade le número de seguidos y de seguidores a users
#almacena este df con write.csv, también la captura de redes de amistad y los lee
users=cbind(users[1:1000,], friends[,2:3])
write.csv(users, file = "C:/Users/ALEX/OneDrive/Documentos/tw_lol_users.csv", row.names = FALSE) # guarda un archivo csv
users=read.csv(file="C:/Users/ALEX/OneDrive/Documentos/tw_lol_users.csv")

################################################
#crea un array con los usuarios de los que buscar la info, osea lo que usan el hastag o palabra y otras caracteristicas
#llama a getfriends para conseguir los amigos. como este metodo no es consistente y a veces falla en el proceso
#se van haciendo los peticiones por numeros mas pequeños de usuarios y se van almacenando para luego concatenarlos
#en un daa frame en q la primera columna seran los usuarios y la segunda cada uno de sus amigos
#fitramos para quedarnos solo las relaciones en que esta segunda tambien este en el array de usuarios y por tanto haya usado el ht o palabra
#de esta manera nos quedamos sollo con las lreaciones de amistad utiles


#llama getfriends para conseguir el formato de los resultados de get_friends
tw_frieds_aux<-get_friends(users[1:2,1], n = 5000, retryonratelimit = TRUE,  page = "-1",
                               parse = TRUE, verbose = TRUE)


#inicializa la varialbe donde se almacenaran las redes de amistad, i a 0 y ejecuta el bucle para la ca
#captrua de las redes de amistad
tw_frieds<-tw_frieds_aux[FALSE,]
i=1;
k=-1;
repeat {
    aux<-tryCatch(get_friends(users[i,1], n = 5000, retryonratelimit = TRUE,  page = k,
                    parse = TRUE, verbose = TRUE),error=function(e) e, warning=function(w) w)
    if (is(aux,"warning")|is(aux,"error")) {
      print(aux)
      write.csv(tw_frieds, file = "C:/Users/ALEX/OneDrive/Documentos/tw_lol_frieds2.csv", row.names = FALSE) # guarda un archivo csv
      
      if (aux$message=="Rate limit exceeded - 88" | aux$message=="<simpleError in curl::curl_fetch_memory(url, handle = handle): Send failure: Connection was reset>") {
        print(i)
        Sys.sleep(61)
      } else{
        print(i)
        i=i+1;
      }
    } else {
      tw_frieds<-rbind(tw_frieds, aux)
      print(paste(i,k, sep="..k="))
      if(nrow(aux)==5000){
        k=next_cursor(aux)
      }else{
        i=i+1;
        k=-1;
      }
    }
    if(i>1000 ) {
      break
    }
}

#k =-1 si 5000 k=k+1 e i=i-1 pa ir recorrendo las pestañas

write.csv(tw_frieds, file = "C:/Users/ALEX/OneDrive/Documentos/tw_lol_frieds2.csv", row.names = FALSE) # guarda un archivo csv
twf1=read.csv(file="C:/Users/ALEX/OneDrive/Documentos/tw_lol_frieds.csv", colClasses = c(user='character', user_id='character'))

##########################################################
#Creación de la red
#Red https://rpubs.com/arquez9512/613766
#https://programminghistorian.org/es/lecciones/analisis-temporal-red
library(igraph)
#https://arcruz0.github.io/libroadp/networks.html
library(tidygraph)

#https://igraph.org/r/doc/graph_from_data_frame.html
usersGraph=as.data.frame(users[1:1000,])
colnames(usersGraph) <- c('UserID','Date','Rtweet','like', 'text','rt','dateformat','mins','dia', 'followers_count','following_count')
twf1=tw_frieds

#crea la matriz de enlaces de la red al comparar los usuarios que han surgido en la busqueda de amigos de los usuarios que habian tweeteado usando el ht
#con los usuarios que han usado el ht. se queda solo con las lineas en las que hay una coincidencia entre estos dos, eliminando todas las relaciones
#que no sean de interés al no formar parte de la red de usuario que usaron el ht
edges2<- subset(twf1 ,user_id%in%usersGraph$UserID)
#edges2=subset(edges2,user%in%usersGraph$UserID)

#crea la red
graphDifusion=graph_from_data_frame(edges2, directed = TRUE, vertices = usersGraph)

V(graphDifusion)
E(graphDifusion)
V(graphDifusion)$color <- ifelse(V(graphDifusion)$Date < "2018-01-21T15:40:49.000Z", "orange", "dodgerblue")
plot(graphDifusion, vertex.label = NA, vertex.size=6, edge.width=1)


#####################################################################################3
#Analisis de la red final

cfg <- cluster_fast_greedy(as.undirected(graphDifusion))
V(graphDifusion)$membership <- cfg$membership
modularity(as.undirected(graphDifusion))

outdegree=degree( graphDifusion, v = V(graphDifusion),mode = "out",normalized = FALSE)
indegree=degree( graphDifusion, v = V(graphDifusion),mode = "in",normalized = FALSE)
totaldegree=degree( graphDifusion, v = V(graphDifusion),mode = "total",normalized = FALSE)
V(graphDifusion)$indegree=indegree
V(graphDifusion)$outdegree=outdegree
V(graphDifusion)$totaldegree=totaldegree
V(graphDifusion)$difusionProbability=V(graphDifusion)$indegree/V(graphDifusion)$followers_count

mean_distance(graphDifusion)
edge_density(graphDifusion)
transitivity(graphDifusion)
reciprocity(graphDifusion)
diameter(graphDifusion)
mean(V(graphDifusion)$difusionProbability, na.rm=TRUE)
length(V(graphDifusion)[V(graphDifusion)$totaldegree==0])
length(V(graphDifusion)[V(graphDifusion)$totaldegree==0])/length(V(graphDifusion))
vcount(graphDifusion)
ecount(graphDifusion)
mean(V(graphDifusion)$indegree, na.rm=TRUE)
mean(V(graphDifusion)$outdegree, na.rm=TRUE)
mean(V(graphDifusion)$totaldegree, na.rm=TRUE)
mean(V(graphDifusion)$followers_count, na.rm=TRUE)
mean(V(graphDifusion)$following_count, na.rm=TRUE)

V(graphDifusion)$betweeness=betweenness(graphDifusion)
V(graphDifusion)$closeness=closeness(graphDifusion)
V(graphDifusion)$authority=authority.score(graphDifusion)$vector
V(graphDifusion)$excentricidad=eccentricity(graphDifusion)

hist(V(graphDifusion)$indegree)
hist(V(graphDifusion)$outdegree)

install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
graphNo0DegreNodes=delete_vertices(graphDifusion, V(graphDifusion)$totaldegree ==0)
tablecheck=data.frame(V(graphNo0DegreNodes)$excentricidad,V(graphNo0DegreNodes)$followers_count)
chart.Correlation(tablecheck)



tablecheck[order(tablecheck$V.graphAux..indegree),]
plot(tablecheck$V.graphAux..indegree,tablecheck$V.graphAux..followers_count,type='p')
#https://rpubs.com/osoramirez/316691   https://r-coder.com/grafico-correlacion-r/#La_funcion_chartCorrelation
pairs(V(graphDifusion)$authority ~ V(graphDifusion)$followers_count)



#pasar a gephi https://igraph.org/r/doc/write_graph.html
write_graph(graphDifusion,'redLol2.graphml',  format = 'graphml')
graphDifusion=read_graph('C:/Users/ALEX/OneDrive/Documentos/redLol2.graphml', format = 'graphml')

###########################################################
########Analisis de comunidades
comunidades=data.frame(table(V(graphDifusion)$membership))

medidas <- data.frame(numeroVertices=numeric(), numeroEnlaces=numeric(),distanciamedia = numeric(),densidad = numeric(),reciprocidad = numeric(), diametro=numeric(), tiempo=numeric())
for (i in 1:4) {
  graphCommunity=delete_vertices(graphDifusion, V(graphDifusion)$membership != 1)
  
  medidas[i,1]=1
  medidas$numeroVertices[i]=vcount(graphCommunity)
  medidas$numeroEnlaces[i]=ecount(graphCommunity)
  medidas$distanciamedia[i]=mean_distance(graphCommunity)
  medidas$densidad[i]=edge_density(graphCommunity)
  medidas$reciprocidad[i]=reciprocity(graphCommunity)
  medidas$diametro[i]=diameter(graphCommunity)
  medidas$tiempo[i]=min(V(graphCommunity)$dia)
}

install.packages('tm')
install.packages('SnowballC')
install.packages('NLP')
library(NLP)
library(tm)
library(SnowballC)
#wcloud <- function(df) {
myCorpus<-Corpus(VectorSource(V(graphCommunity)$text))
#Funciones que eliminan caracteres no deseados
tweet.removeEmoji = function(x) gsub("\\p{So}|\\p{Cn}", "", x, perl = TRUE)
tweet.removeSpecialChar = function(x) gsub("[^[:alnum:]///' ]", "", x)
#Followed by the tm_map calls:
myCorpus = tm_map(myCorpus, content_transformer(tweet.removeEmoji))
myCorpus = tm_map(myCorpus, content_transformer(tweet.removeSpecialChar))

myCorpus<-tm_map(myCorpus,content_transformer(tolower))
#quitar la puntuaci??n
myCorpus<-tm_map(myCorpus,removePunctuation)
#quitar numeros
myCorpus<-tm_map(myCorpus,removeNumbers)
#quitar stopwords
myStopwords<-c(stopwords('spanish'),stopwords('english'), 'avalaible','via','lol')
myCorpus<-tm_map(myCorpus,removeWords,myStopwords)
#Raices de las palabras
dictCorpus<-myCorpus
myCorpus<-tm_map(myCorpus,stemDocument)
#Stem completion
stemCompletion_mod <- function(x,dict) {
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")), dictionary = dict, type = "shortest"), sep = "", collapse = " ")))
}
myCorpus<-tm_map(myCorpus,stemCompletion_mod,dictCorpus)
#Construyendo una matriz DT (document-Term)
myDtm<-TermDocumentMatrix(myCorpus,control=list(minWordLength=1))
#inspect(myDtm[1:5,1:15])
#Nube de palabras
library(wordcloud)
m<-as.matrix(myDtm)
#Calcla la frecuencia de las palabras
v<-sort(rowSums(m),decreasing=TRUE)
myNames<-names(v)
d<-data.frame(word=myNames,freq=v)
wordcloud(d$word,d$freq,min.freq=3,colors='dark green', vfont=c("sans serif","plain"))

############################################################
#Análisis dinámico de la red
#evolucion https://support.noduslabs.com/hc/en-us/articles/360013974580--How-to-Watch-the-Dynamic-Evolution-of-a-Graph-
#https://infranodus.com/#usecases

boxplot(V(graphDifusion)$dia)
graphBoxPlot= delete_vertices(graphDifusion, !(V(graphDifusion)$membership%in%c(1,2,3,4)))

boxplot(V(graphBoxPlot)$dia ~ V(graphBoxPlot)$membership)

evoluciontemporal=cbind(V(graphDifusion)$dia,1:1000)
plot(evoluciontemporal[,1],evoluciontemporal[,2], type = 'l')

n=1
for (n in 1:1000) {
  graphHipo= delete_vertices(graphDifusion, V(graphDifusion)$mins>V(graphDifusion)$mins[n])
  V(graphHipo)$degreeHipo=degree(graphHipo, v = V(graphHipo),mode = "out",normalized = FALSE)
  if(V(graphHipo)$degreeHipo[n]>0){
    V(graphDifusion)$hipothesis[n]=TRUE;
  } else{
    V(graphDifusion)$hipothesis[n]=FALSE;
  }
}
sum(V(graphDifusion)$hipothesis==TRUE)

plot(1:1000,cumsum(V(graphDifusion)$hipothesis), type='l')

graph=graphDifusion;
numinterval=50
i=14

difusionAnalysis(graph, numinterval){
  
  if(numinterval<1){
    return("error")
  }
  
  minMax=V(graph)$mins[length(V(graph))]+0.1
  minsinterval=minMax/numinterval;
  difStats <- data.frame(l= numeric(),timemins = numeric(),timedays = numeric())
  difStats[1,1]=1
  
  
  for (l in 1:numinterval) {
    intervalmax=l*minsinterval;
    graphAux=delete_vertices(graph, V(graph)$mins >= intervalmax)
    difStats[l,1]=l;
    difStats$timemins[l]=intervalmax
    difStats$timedays[l]=intervalmax/(60*24)

    #distribucion de grado https://igraph.org/r/doc/degree.html
    V(graphAux)$outdegree=degree( graphAux, v = V(graphAux),mode = "out",normalized = FALSE)
    V(graphAux)$indegree=degree( graphAux, v = V(graphAux),mode = "in",normalized = FALSE)
    V(graphAux)$totaldegree=degree( graphAux, v = V(graphAux),mode = "total",normalized = FALSE)
    V(graphAux)$difusionProbability=V(graphAux)$indegree/V(graphAux)$followers_count
    
    difStats$mean_distance[l]=mean_distance(graphAux)
    difStats$edge_density[l]=edge_density(graphAux)
    difStats$transitivity[l]=transitivity(graphAux)
    difStats$reciprocity[l]=reciprocity(graphAux)
    difStats$diameter[l]=diameter(graphAux)
    difStats$difusionProbability[l]=mean(V(graphAux)$difusionProbability, na.rm=TRUE)
    difStats$propnumvdegree0[l]=length(V(graphAux)[V(graphAux)$totaldegree==0])/length(V(graphAux))
    difStats$numvertex[l]=vcount(graphAux)
    difStats$numedges[l]=ecount(graphAux)
    difStats$meanoutdegree[l]=mean(V(graphAux)$outdegree, na.rm=TRUE)
    difStats$meanindegree[l]=mean(V(graphAux)$indegree, na.rm=TRUE)
    difStats$meantotaldegree[l]=mean(V(graphAux)$totaldegree, na.rm=TRUE)
    difStats$meanfollowers[l]=mean(V(graphAux)$followers_count, na.rm=TRUE)
    difStats$meanfollowing[l]=mean(V(graphAux)$following_count, na.rm=TRUE)
    difStats$modularity[l]=modularity(cluster_fast_greedy(as.undirected(graphAux)))
    
    if(l==1){
      difStats$velocidad[l]=difStats$numvertex[l]/minsinterval
    } else{
      difStats$velocidad[l]=(difStats$numvertex[l]-difStats$numvertex[l-1])/minsinterval
    }
  }
  difStats <- mutate_all(difStats, ~replace(., is.na(.), 0))
  
  
  V(graphAux)$betweeness=betweenness(graphAux)
  V(graphAux)$closeness=closeness(graphAux)
  V(graphAux)$authority=authority.score(graphAux)$vector
  V(graphAux)$excentricidad=eccentricity(graphAux)
  
  #deteccion de comunidad https://ajaxhispano.com/ask/cuales-son-las-diferencias-entre-los-algoritmos-de-deteccion-de-la-comunidad-en-igraph-25910/
  #https://github.com/elaragon/R-igraph-Network-Workshop/blob/master/NetSciX%202016%20Workshop.R
  cfg <- cluster_fast_greedy(as.undirected(graphAux))
  plot(cfg, as.undirected(graphAux), vertex.label = NA) # hulls
  V(graphAux)$community <- cfg$membership
  modularity(as.undirected(graphAux))
}

hist(V(graphAux)$indegree)

plot(difStats$timedays,difStats$mean_distance, type = 'l')
plot(difStats$timedays,difStats$meanfollowers, type = 'l')
plot(difStats$timedays,difStats$meanfollowing, type = 'l')
plot(difStats$timedays,difStats$meantotaldegree, type = 'l')
plot(difStats$timedays,difStats$meanoutdegree, type = 'l')
plot(difStats$timedays,difStats$meanindegree, type = 'l')
plot(difStats$timedays,difStats$numedges, type = 'l')
plot(difStats$timedays,difStats$numvdegree0, type = 'l')
plot(difStats$timedays,difStats$propnumvdegree0, type = 'l')
plot(difStats$timedays,difStats$numvertex, type = 'l')
plot(difStats$timedays,difStats$edge_density, type = 'l')
plot(difStats$timedays,difStats$transitivity, type = 'l')
plot(difStats$timedays,difStats$reciprocity, type = 'l')
plot(difStats$timedays,difStats$diameter, type = 'l')
plot(difStats$timedays,difStats$difusionProbability, type = 'l')
plot(difStats$timedays,difStats$velocidad, type = 'l')
plot(difStats$timedays,difStats$modularity, type = 'l')



###########################################################################################33
#extra

install.packages('tidytext')
install.packages('wordcloud')

library(tidytext)
library(wordcloud)
library(dplyr)

stop_words <- read_delim("stop_words.csv", ";")

word_freq <- V(graphAux2)$text %>%
  unnest_tokens(output = word,
                input = texto,
                token = "words",
                format = "text") %>%
  anti_join(stop_words, by = c("word" = "texto")) %>%
  count(word) # Cantidad de palabras 

head(word_freq$word, 15)

wordcloud(words = word_freq$word,
          freq = word_freq$n,
          min.freq = 2,
          max.words = 20,
          colors =  c("Green","Yellow", "Red"),
          random.order = F,
          random.color = F,
          scale = c(3 ,0.1),
          rot.per = 0.3)

#Estaría chulo ir recorriendo uno a uno cada nodo para ver si en el momento que tuiteo el término seguía a alguien que ya lo había tuiteado. Importante. 
#for recorre los usuarios de user que estan ordenados. para cada uno realiza un subgrafo de la red
#eliminando todos los usuarios que tuitearon posteriormente
#calcula el grado de salida del usuario, buscandolo por su id, para saber si en el moment que tuiteo
#tenía alguna relación con alguien que ya hubiera tuiteado