# install.packages("ergm")
# install.packages("igraph")
# install.packages("ggplot2")

# Clean global environment
rm(list = ls())

library(ergm)
library(network)
library(DEoptimR)
library(robustbase)
library(coda)
library(trust)
library(lpSolve)
library(statnet.common)
library(igraph)
library(readr)
library(ggplot2)

# @Gerrit, zijn al die pakketten nou echt nodig? 
# Is er een manier om het hele script te runnen en dan te kijken welke pakketten 
# daadwerkelijk gebruikt zijn zodat ik de rest eruit kan gooien? 

# Even aanpassen indien nodig...
setwd("~/Desktop/R/Leesplankje")

# Lees de data
# @Gerrit: de eerlijkheid gebied te zeggen dat ik niet precies weet wat hier gebeurt. 
# Het werkt... maar kan het ook eenvoudiger?

Data <- read_csv("NetworkData_Edges.csv", 
                 col_types = cols(Id = col_integer(), 
                                  Weight = col_integer()))

# Subset Source en Target tot een Edgelis
Data_Edges <- subset(Data, select = c(Source, Target))

# Lees de edgelist als een netwerk en controleer of het gelukt is. 

# @Gerrit Het is me een raadsel wat het verschil is tussen een table, 
# een data.frame en een matrix. 
# Ik zie wel dat het uitmaakt want ze zijn niet inwisselbaar. 

Network <- as.network(Data_Edges, matrix.type='edgelist')
class(Network)
plot(Network, displaylabels=TRUE)

# Lees de edgelist als een graph en controleer of het gelukt is. 
Data_Edges_Matrix <- as.matrix(subset(Data, select = c(Source, Target)))
Graph <- graph_from_edgelist(Data_Edges_Matrix, directed = TRUE)
class(Graph)
plot(Graph)

# @Gerrit Zelfde vraag over het verschil tussen een Netwerk en een Graph. 
# Die zijn niet precies hetzelfde maar wat het verschil is mij niet duidelijk. 

# Bereken het aantal nodes en edges
AantalEdges <- gsize(Graph)
AantalNodes <-gorder(Graph)

# @Gerrit Waarom staat er in de global environment 17L (die "L") bij het aantal nodes?

# Genereer een random netwerk met dezelfde afmetingen als Graph
RandomGraph <- erdos.renyi.game(AantalNodes, AantalEdges, type = "gnm", directed = TRUE)
plot(RandomGraph)

# Bereken triad census van de Graph en van de RandomGraph.  
TriadCensus_Graph <- triad.census(Graph)
TriadCensus_RandomGraph <- triad.census(RandomGraph)

# Just checking...
edge_density(Graph)
edge_density(RandomGraph)

# Maak een tabel van Triadcensus_Graph en Triadcensus_RandomGraph
Triad <-rep(c("003", "012", "102", "021D", "021U", "021C", "111D", 
              "111U", "030T", "030C", "201", "120D", "120U", "120C", "210", "300"), 2)
Label <-rep(c("A,B,C", "A->B,C", "A<->B,C", "A<-B->C", 
              "A->B<-C", "A->B->C", "A<->B<-C", "A<->B->C", 
              "A->B<-C,A->C", "A<-B<-C,A->C", "A<->B<->C", "A<-B->C,A<->C", 
              "A->B<-C,A<->C", "A->B->C,A<->C", "A->B<->C,A<->C", "A<->B<->C,A<->C"), 2)
Values <-c(TriadCensus_Graph, TriadCensus_RandomGraph)
Type <-c(rep("TriadCensus_Graph", 16), rep("TriadCensus_RandomGraph", 16))

Vergelijk_TriadCensus <- data.frame(Type, Triad, Label, Values)

# Maak een plot

# @Gerrit, is ggplot de standaard aanpak? Ik weet niet precies wat hier gebeurt. 
# Waar kan ik meer (gewone mensentaal) uitleg vinden over de mogelijkheden van ggplot? 
# Bijvoorbeeld: andere kleuren, andere achtergrond, andere breedte, andere plek legenda etc. 

p <-ggplot(Vergelijk_TriadCensus, aes(Triad, Values))
p +geom_bar(stat = "identity", aes(fill = Type), position = "dodge", show.legend = TRUE)

# @Gerrit, ik krijg het idee dat het op veel punten anders, eleganter kan. 
# Is dat nodig? Komt dat vanzelf? Schrijf je je code steeds opnieuw? 
# Hoe weet je of het allemaal klopt? Nog andere tips? 



