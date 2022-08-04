library(igraph)
#prima
from = sample(LETTERS, 30, replace=FALSE)
 to = sample(LETTERS, 30,  replace=FALSE)
 graph_data = data.frame(from,to)
 
 g <- graph.data.frame(graph_data, direct= FALSE)
 E(g)$w <-1
 #E(g)$weight <- runif(length(E(g)), 1)
 l=cbind( get.edgelist(g) , round( E(g)$w, 3 ))
ni=get.edgelist(g)
a=ni[,1]
b=ni[,2]
 c=c(a,b)
 c= data.frame(c)
  c=unique(c)
 n=dim(c)[1]
  ww=rep(1,n)
cc=data.frame(c,ww)
names(cc)[1]<-"node"
names(cc)[2]<-"color"
 
 #seconda
 from2 = sample(1:100, 30, replace=TRUE)
 
 to2 = sample(1:100, 30, replace= TRUE)


graph_data2 = data.frame(from2,to2)
 
 g2 <- graph.data.frame(graph_data2, direct= FALSE)
 E(g2)$w <-2
 #E(g2)$weight <- runif(length(E(g2)), 2)
 l2=cbind( get.edgelist(g2) , round( E(g2)$w, 3 ))
 
 ni2=get.edgelist(g2)
a2=ni2[,1]
b2=ni2[,2]
 c2=c(a2,b2)
 c2= data.frame(c2)
 c2=unique(c2)
 n2=dim(c2)[1]
  ww2=rep(2,n2)
cc2=data.frame(c2,ww2)
names(cc2)[1]<-"node"
names(cc2)[2]<-"color"

nodefinal=rbind(cc,cc2)
 #inter
 
 graph_data3 = data.frame(from, to2)
 
  g3 <- graph.data.frame(graph_data3, direct= FALSE)
 E(g3)$w <-3
 #E(g3)$weight <- runif(length(E(g3)), 2)
 
 l3=cbind( get.edgelist(g3) , round( E(g3)$w, 3 ))


list=rbind(l,l2,l3)


#similarità
sim=data.frame(nodefinal$node, nodefinal$node)
write.table(sim,"~/Desktop/sim5.txt",append = FALSE, quote = FALSE,row.names=FALSE, col.names=FALSE, sep="\t")


write.table(list,"~/Desktop/edge_list5.txt",append = FALSE, quote = FALSE,row.names=FALSE, col.names=FALSE, sep="\t")

write.table(nodefinal,"~/Desktop/node5.txt",append = FALSE, quote = FALSE,row.names=FALSE, col.names=TRUE, sep="\t")


cnode=read.table("~/Desktop/node5.txt",header=TRUE)

topology=read.table("~/Desktop/edge_list5.txt",header=FALSE)
graph.data.frame(topology,vertices=cnode,  directed=FALSE)