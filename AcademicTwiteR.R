########################################################################################
#Este programa ha sido creado para el análisis de la difusión de términos en twitter mediante redes de amistad
# los pasos a seguir para ello son
# 1. Creación de una cuenta en "twitter academics": necesaria para acceder a la API
# de twitter y conseguir los datos necesarios.
# 2. Establecer conexión desde R con los servicios de la API de twitter.
# 3. Selección del término al que hacer el seguimiento y preanálisis.
# 4. Extracción de los tweets que contienen el término: puesto que la idea es ver la 
# difusión desde que le término es creado, es necesario acceder al 100% de los 
# tweets que contengan el término indiferentemente del momento de su creación, 
# para poder acceder a ellos desde el primer tweet creado con ese término.
# 5. Creación del dataframe de los pioneros con los atributos útiles: una vez se tenga 
# la lista de tweets con el término y sus usuarios correspondientes, tratar estos datos 
# para que sean adecuados para el trabajo, eliminando repeticiones de usuarios y 
# añadiendo datos de interés.
# 6. Captura de la red egocéntrica de los pioneros: conseguir todos los usuarios a los 
# que sigue cada uno de los usuarios que participó en la difusión del término.
# 7. Construcción de la red: a partir de los usuarios que han participado en la difusión 
# del término y las relaciones de seguimiento entre ellos.
# 8. Análisis de la red, visualización y creación de indicadores
#En este script se podrán encontrar el código para los pasos del 2 al 8, como esta descrito en la memoria

#############################################################################
#Install the packages needed 
install.packages("academictwitteR")
install.packages("rtweet")

library(academictwitteR)
library(rtweet)
library(dplyr)
library(rlang)

#########################################################
#establece credenciales de la api para la conexion
#the set_bearer() function shows in console how to save the keys in .Renviron
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
#counts the tweets containing the query per day and sves it in a dataframe 
# with a list of the ammount of tweets with the query per day
counttw <-
  count_all_tweets( query = "lol",
                    start_tweets = "2006-05-03T00:00:00Z",
                    end_tweets = "2007-04-25T00:00:00.000Z",
                    file = "lolCount",
                    data_path = "data/",
                    n = Inf
  )
#order the result for a better understnding
countw_ordenado <- counttw[order(counttw$start), ]
#sums the columns with the amount of tweets with the wuery per day to know 
#the total amount of tweets with the query in the interval
sum(countw_ordenado$tweet_count)
#plot in a bar graphic the results per day
barplot(countw_ordenado$tweet_count, names=countw_ordenado$start )

######################################################
#get_all_tweets() calls twitter API V2 full archive search endpoint to get 
#all the tweets containing a certain query in a certain interval, and saves it in a file
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
#Pretreatment of the data
#Read the data saved with get_all_tweets()
dat <- readRDS("lol")
tweets=dat
#Create variable rt wich contains a bolean indicating if the tweet is a retweet 
tweets$rt<-ifelse(substr(tweets$text,1,2)=='RT',TRUE,FALSE)

#order the tweets by date
tw_ordenado <- tweets[order(dat$created_at), ]

#erase all the rows which user has already appear in the dataframe
#users that has already use the word studied as we just need to know
#when a user has aquired the word
tw_dist=tw_ordenado[1,]

for (i in 2:length(tw_ordenado$author_id)) {
  if(!(tw_ordenado$author_id[i]%in%tw_dist$author_id)){
    tw_dist=rbind(tw_dist,tw_ordenado[i,])
  }
}

#order again by date
tw_ordenado <- tw_dist[order(tw_dist$created_at), ]

#Create variables dateformat, mins and dia from created_at, containgin the date in datetime,
#minuts from firts tweet searched, and days form the first tweet
tw_ordenado$dateformat=as.POSIXct(tw_ordenado$created_at, format = "%Y-%m-%dT%H:%M:%OSZ")
tw_ordenado$mins=(as.numeric(tw_ordenado$dateformat)-as.numeric(tw_ordenado$dateformat[1]))/60
tw_ordenado$dia=trunc(tw_ordenado$mins/(24*60))

#saves the data with the important variables in users dataframe
users=data.frame("author_id"=tw_ordenado$author_id,"created_at"=tw_ordenado$created_at, "rt_count"= tw_ordenado$public_metrics$retweet_count, "likescount"=tw_ordenado$public_metrics$like_count, "text"=tw_ordenado$text,"rt"=tw_ordenado$rt, "dateFormat"=tw_ordenado$dateformat,"mins"=tw_ordenado$mins,"dia"=tw_ordenado$dia)

#look for the ammount of followers and users followed that each user has, 
#calling get_user profile in a repeat loop for going throw the first 1000 rows of users
friends <- data.frame(j = numeric(),userfollowedby = numeric(),userfollowing = numeric())
j=1;
repeat{
  user <- get_user_profile(users[j,1])
  friends[j,1]=j;
  print(j)
  #checks if the result is empty, it means there is no acces to that user
  #so just fill the line with a -1 and jumps to the next one
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

#concatenate the result to users
users=cbind(users[1:1000,], friends[,2:3])

#saves and read users in and from the computer in csv format
write.csv(users, file = "C:/Users/ALEX/OneDrive/Documentos/tw_lol_users.csv", row.names = FALSE) # guarda un archivo csv
users=read.csv(file="C:/Users/ALEX/OneDrive/Documentos/tw_lol_users.csv")

################################################
#crea un dataframe con los usuarios de los que buscar la info, osea lo que usan el hastag o palabra y otras caracteristicas
#llama a get_friends para conseguir los amigos. como este metodo no es consistente y a veces falla en el proceso
#se van haciendo los peticiones por numeros mas pequeños de usuarios y se van almacenando para luego concatenarlos
#en un dataframe en que la primera columna seran los usuarios y la segunda cada uno de sus amigos

#crates a dataframe with the usrs we are investigating, and the users they are following
#it calls function get_friends() but in a repeat loop that ask one by one, and wait when the 15petitions/15minits 
#is reached. o we get a dataframe with the firts 1.000 users and the users they are following


#calls get_friends to get an example of the dataframe
tw_frieds_aux<-get_friends(users[1:2,1], n = 5000, retryonratelimit = TRUE,  page = "-1",
                               parse = TRUE, verbose = TRUE)


#inicitalizate the variable adn starts the loop
tw_frieds<-tw_frieds_aux[FALSE,]
i=1;
k=-1;
repeat {
    aux<-tryCatch(get_friends(users[i,1], n = 5000, retryonratelimit = TRUE,  page = k,
                    parse = TRUE, verbose = TRUE),error=function(e) e, warning=function(w) w)
    if (is(aux,"warning")|is(aux,"error")) {
      print(aux)
      #saves the result in the cmputer in case there is any problem i twill be saved
      write.csv(tw_frieds, file = "C:/Users/ALEX/OneDrive/Documentos/tw_lol_frieds2.csv", row.names = FALSE) # guarda un archivo csv
      
      #check if there is a known error and retry
      if (aux$message=="Rate limit exceeded - 88" | aux$message=="<simpleError in curl::curl_fetch_memory(url, handle = handle): Send failure: Connection was reset>") {
        print(i)
        Sys.sleep(61)
        #otherwise just jump one in the loop
      } else{
        print(i)
        i=i+1;
      }
    } else {
      tw_frieds<-rbind(tw_frieds, aux)
      print(paste(i,k, sep="..k="))
      #in case the result has 5.000 rows, it means there are mor users followed that were not captured
      #it calls next_cursos() to get the next cursos, and save it in k so in the next step in the loop
      #it can be used to access the rest of users followed
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

#save and read the results in csv format in the computer
write.csv(tw_frieds, file = "C:/Users/ALEX/OneDrive/Documentos/tw_lol_frieds2.csv", row.names = FALSE) # guarda un archivo csv
twf1=read.csv(file="C:/Users/ALEX/OneDrive/Documentos/tw_lol_frieds.csv", colClasses = c(user='character', user_id='character'))


##########################################################
#Creación de la red
#network building
#https://rpubs.com/arquez9512/613766
#https://programminghistorian.org/es/lecciones/analisis-temporal-red
library(igraph)
#https://arcruz0.github.io/libroadp/networks.html
library(tidygraph)

#https://igraph.org/r/doc/graph_from_data_frame.html
#create the dataframe with the nodes of the network to build it 
usersGraph=as.data.frame(users[1:1000,])
colnames(usersGraph) <- c('UserID','Date','Rtweet','like', 'text','rt','dateformat','mins','dia', 'followers_count','following_count')
twf1=tw_frieds

#build the dataframe of the edges filttering the dataframe with the egocentric networks
#keeping just the register of the relations that ends in another user of the first that used lol
edges2<- subset(twf1 ,user_id%in%usersGraph$UserID)

#build the network
graphDifusion=graph_from_data_frame(edges2, directed = TRUE, vertices = usersGraph)

V(graphDifusion)
E(graphDifusion)
V(graphDifusion)$color <- ifelse(V(graphDifusion)$Date < "2018-01-21T15:40:49.000Z", "orange", "dodgerblue")
plot(graphDifusion, vertex.label = NA, vertex.size=6, edge.width=1)


#####################################################################################3
#Analisis de la red final
#Final network analisis

#measures modularity
cfg <- cluster_fast_greedy(as.undirected(graphDifusion))
V(graphDifusion)$membership <- cfg$membership
modularity(as.undirected(graphDifusion))

#get different measures of the nodes
outdegree=degree( graphDifusion, v = V(graphDifusion),mode = "out",normalized = FALSE)
indegree=degree( graphDifusion, v = V(graphDifusion),mode = "in",normalized = FALSE)
totaldegree=degree( graphDifusion, v = V(graphDifusion),mode = "total",normalized = FALSE)
V(graphDifusion)$indegree=indegree
V(graphDifusion)$outdegree=outdegree
V(graphDifusion)$totaldegree=totaldegree
V(graphDifusion)$difusionProbability=V(graphDifusion)$indegree/V(graphDifusion)$followers_count

#get different measures of the graph
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

#plot an histogram of the degrees of conection
hist(V(graphDifusion)$indegree)
hist(V(graphDifusion)$outdegree)

#check correlation
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
graphNo0DegreNodes=delete_vertices(graphDifusion, V(graphDifusion)$totaldegree ==0)
tablecheck=data.frame(V(graphNo0DegreNodes)$difusionProbability,V(graphNo0DegreNodes)$followers_count)
chart.Correlation(tablecheck)



tablecheck[order(tablecheck$V.graphAux..indegree),]
plot(tablecheck$V.graphAux..indegree,tablecheck$V.graphAux..followers_count,type='p')
#https://rpubs.com/osoramirez/316691   https://r-coder.com/grafico-correlacion-r/#La_funcion_chartCorrelation
pairs(V(graphDifusion)$authority ~ V(graphDifusion)$followers_count)



#export the graph to load it on Gephi https://igraph.org/r/doc/write_graph.html
write_graph(graphDifusion,'redLol2.graphml',  format = 'graphml')
graphDifusion=read_graph('C:/Users/ALEX/OneDrive/Documentos/redLol2.graphml', format = 'graphml')

###########################################################
########Analisis de comunidades

#chech the size of the communities
comunidades=data.frame(table(V(graphDifusion)$membership))

#get measures of the four bigger communities
medidas <- data.frame(numeroVertices=numeric(), numeroEnlaces=numeric(),distanciamedia = numeric(),densidad = numeric(),reciprocidad = numeric(), diametro=numeric(), tiempo=numeric())
for (i in 1:4) {
  graphCommunity=delete_vertices(graphDifusion, V(graphDifusion)$membership != i)
  
  medidas[i,1]=1
  medidas$numeroVertices[i]=vcount(graphCommunity)
  medidas$numeroEnlaces[i]=ecount(graphCommunity)
  medidas$distanciamedia[i]=mean_distance(graphCommunity)
  medidas$densidad[i]=edge_density(graphCommunity)
  medidas$reciprocidad[i]=reciprocity(graphCommunity)
  medidas$diametro[i]=diameter(graphCommunity)
  medidas$tiempo[i]=min(V(graphCommunity)$dia)
}

#word cloud from the text of the tweets of each communitty
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
#dinamic analisis of the network
#https://support.noduslabs.com/hc/en-us/articles/360013974580--How-to-Watch-the-Dynamic-Evolution-of-a-Graph-
#https://infranodus.com/#usecases

#plot box diagrams of the day of aparition of the nodes, all of them and differencing in comunities
boxplot(V(graphDifusion)$dia)
graphBoxPlot= delete_vertices(graphDifusion, !(V(graphDifusion)$membership%in%c(1,2,3,4)))

boxplot(V(graphBoxPlot)$dia ~ V(graphBoxPlot)$membership)

#plots the temoral evolution of the number of nodes
evoluciontemporal=cbind(V(graphDifusion)$dia,1:1000)
plot(evoluciontemporal[,1],evoluciontemporal[,2], type = 'l')

#for the aparition of each node create a subset of the graph and checks if the new node has out connections
#by this way it checks if the hipothesis that a new word is aquired when the user sees it in another user
#timeline can be said that is true
for (n in 1:1000) {
  graphHipo= delete_vertices(graphDifusion, V(graphDifusion)$mins>V(graphDifusion)$mins[n])
  V(graphHipo)$degreeHipo=degree(graphHipo, v = V(graphHipo),mode = "out",normalized = FALSE)
  if(V(graphHipo)$degreeHipo[n]>0){
    V(graphDifusion)$hipothesis[n]=TRUE;
  } else{
    V(graphDifusion)$hipothesis[n]=FALSE;
  }
}

#plots the evolution of the percentage of nodes that meet the hipothesis
sum(V(graphDifusion)$hipothesis==TRUE)
V(graphDifusion)$prophipothesis=cumsum(V(graphDifusion)$hipothesis)/1:1000

plot(1:1000,V(graphDifusion)$prophipothesis, type='l')

#for bucle that goes through the 1.000 sections of time, intervals, in wich it divides the total time of the evolution
#in each intervale make a subgraph of the graph with the nodes that have already appeared by that time
# and takes measures and saves it in a new dataframe
graph=graphDifusion;
numinterval=50

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
  
  library(fitdistrplus)
  descdist(difStats$numvertex, discrete = FALSE)
  
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

hist(V(graphAux)$indegree)

#plots the evolution of the measures trhough tim
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


#makes a regression with of the evolution of the number of vertex over time
model <- lm ( log (difStats$numvertex) ~ difStats$timemins)
coef1=model$coefficients[1]
coef2=model$coefficients[2]
coef1exp=exp(coef1)
coef2exp=exp(coef2)
summary(model)