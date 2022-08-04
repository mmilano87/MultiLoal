 multiAl3<-function() { 
 library(igraph)
 library(stringr)
 library(multinet)



cnode=read.table(file.choose(),header=TRUE)

topology=read.table(file.choose(),header=FALSE)


cnode2=read.table(file.choose(),header=TRUE)



topology2=read.table(file.choose(),header=FALSE)

sim=read.table(file.choose(),header=FALSE)
 
 #sim1=sim[,-3]
 #sim1=as.matrix(sim1)
 h6 = NULL
   h7=NULL
    h8=NULL
   index = 1
 for (i in 1:dim(sim)[1]) { for (j in 1:dim(topology)[1])   {
 if (sim[i,1]== topology[j,1]|sim[i,1]== topology[j,2]) {h6[j]= topology[j,1]; h7[j]= topology[j,2]; h8[j]=topology[j,3]}}}
 dh9=cbind(h6,h7,h8)
 dh9=cbind(h6,h7)

dh9=topology

 
 #creo i grafi1 e 2
 g <- graph.data.frame(topology,vertices=cnode,  directed=FALSE)
#g <- simplify(g)

 h6 = NULL
   h7=NULL
    h8=NULL
   index = 1
 for (i in 1:dim(sim)[1]) { for (j in 1:dim(topology2)[1])   {
 if (sim[i,1]== topology2[j,1]|sim[i,1]== topology2[j,2]) {h6[j]= topology2[j,1]; h7[j]= topology2[j,2]; h8[j]=topology2[j,3]}}}
 dh9=cbind(h6,h7,h8)
 dh9=cbind(h6,h7)

dh9=topology2


 g2 <- graph.data.frame(topology2, vertices=cnode2, directed=FALSE)
#g2 <- simplify(g2)


 
 
 #separare le reti per layer
 
#creo layer 1 e 2 da rete g1 
 
 V(g)$w <-cnode$color
 
l1_g1 <- delete.vertices(g, which(V(g)$w==2))

l2_g1 <- delete.vertices(g, which(V(g)$w==1))
 #creo layer 1 e 2 da rete g2
  V(g2)$w <-cnode2$color
 
l1_g2 <- delete.vertices(g2, which(V(g2)$w==2))

l2_g2 <- delete.vertices(g2, which(V(g2)$w==1))



# creazione del primo alignment graph l1_g1-l1_g2

#calcolo shortest path
  
 dm = shortest.paths(l1_g1)
dm2 = shortest.paths(l1_g2)

#creo matrice di distanza e edge list 
cor_g <- graph_from_adjacency_matrix(dm, mode='undirected', weighted = TRUE)
 cor_edge_list <- as_data_frame(cor_g, 'edges')

listg1=as.matrix(cor_edge_list)



cor_g2 <- graph_from_adjacency_matrix(dm2, mode='undirected', weighted = TRUE)
 cor_edge_list2 <- as_data_frame(cor_g2, 'edges')

listg2=as.matrix(cor_edge_list2)



#creo una edge di posizioni + distanza

name=V(l1_g1)$name

h = NULL
  h1=NULL
  index = 1
 for (i in 1:dim(listg1)[1]) {  h[index]=(which(name==listg1[i,1])); h1[index]=(which(name== listg1[i,2]));index = index+1 }
 
 dh=cbind(h,h1)
 
 r=cbind(dh, listg1[,3])
 
 
 name2=V(l1_g2)$name


 h2 = NULL
  h3=NULL
  index = 1

for (i in 1:dim(listg2)[1]) {  h2[index]=(which(name2==listg2[i,1])); h3[index]=(which(name2== listg2[i,2]));index = index+1 }


dh2=cbind(h2,h3)


r2=cbind(dh2, listg2[,3])


cnodem=cbind(V(l1_g1)$name,V(l1_g1)$w)
cnodem2=cbind(V(l1_g2)$name,V(l1_g2)$w)
#cnodem=as.matrix(cnode)
#cnodem2=as.matrix(cnode2)
 
 
 
b=0
ind=1



delta=2


# # for (i in 1:dim(r)[1])   {
    # if (r[i,3]==1 & r2[i,3]==1 ){ b[ind]=1 }   else if (r[i,3]==1 & r2[i,3] < delta | r2[i,3]==1 & r[i,3] < delta ) {b[ind]=0.2 }  else if (r[i,3]==1 & r2[i,3] > delta | r2[i,3]==1 & r[i,3] > delta ) {b[ind]=0.5 }  else {b[ind]=0 }; ind=ind+1}

 for (i in 1:dim(r)[1])   {
    if (r[i,3]==1 & r2[i,3]==1 & cnodem[dh[i,1],2]==cnodem[dh[i,2],2] & cnodem2[dh2[i,1],2]==cnodem2[dh2[i,2],2]){ b[ind]=1 } else if (r[i,3]==1 & r2[i,3]==1 & (cnodem[dh[i,1],2]!=cnodem[dh[i,2],2] | cnodem2[dh2[i,1],2]!=cnodem2[dh2[i,2],2])){ b[ind]=0.9 }  else if (r[i,3]==1 & r2[i,3] < delta | r2[i,3]==1 & r[i,3] < delta & ( cnodem[dh[i,1],2]==cnodem[dh[i,2],2] & cnodem2[dh2[i,1],2]==cnodem2[dh2[i,2],2])) {b[ind]=0.2 } else if (r[i,3]==1 & r2[i,3] < delta | r2[i,3]==1 & r[i,3] < delta & ( cnodem[dh[i,1],2]!=cnodem[dh[i,2],2] | cnodem2[dh2[i,1],2]!=cnodem2[dh2[i,2],2])) {b[ind]=0.1 } else if (r[i,3]==1 & r2[i,3] > delta | r2[i,3]==1 & r[i,3] > delta & ( cnodem[dh[i,1],2]==cnodem[dh[i,2],2] & cnodem2[dh2[i,1],2]==cnodem2[dh2[i,2],2])) {b[ind]=0.5 } else if (r[i,3]==1 & r2[i,3] > delta | r2[i,3]==1 & r[i,3] > delta & ( cnodem[dh[i,1],2]!=cnodem[dh[i,2],2] | cnodem2[dh2[i,1],2]!=cnodem2[dh2[i,2],2])) {b[ind]=0.4 } else {b[ind]=0 }; ind=ind+1}
finall1=listg1[,-3]
finall2=listg2[,-3]

 finale=rbind(finall1, finall2)


 d4=cbind(finale,b)
 
 
 d5=d4

 h2 = NULL
  h3=NULL
   h4=NULL
  index = 1

for (i in 1:dim(d5)[1])   { if(d5[i,3]!=0){ h2[index]=d5[i,1]; h3[index]=d5[i,2];h4[index]=d5[i,3] } else{h2[index]=d5[i,1]; h3[index]=d5[i,2];h4[index]=d5[i,3]};index = index+1 }

d=cbind(h2,h3)
d=cbind(d,h4)
el <- d[rowSums(is.na(d)) == 0,]


#aggiungo il colore
colore=rep(1, dim(el)[1])

ell=cbind(el,colore)

#------------------------------------------
#creao il grafo per il l2g1-l2g2

#gl1 =graph.data.frame(ell,directed=FALSE )


dm = shortest.paths(l2_g1)
dm2 = shortest.paths(l2_g2)

#creo matrice di distanza e edge list 
cor_g <- graph_from_adjacency_matrix(dm, mode='undirected', weighted = TRUE)
 cor_edge_list <- as_data_frame(cor_g, 'edges')

listg1=as.matrix(cor_edge_list)



cor_g2 <- graph_from_adjacency_matrix(dm2, mode='undirected', weighted = TRUE)
 cor_edge_list2 <- as_data_frame(cor_g2, 'edges')

listg2=as.matrix(cor_edge_list2)



#creo una edge di posizioni + distanza

name=V(l2_g1)$name

h = NULL
  h1=NULL
  index = 1
 for (i in 1:dim(listg1)[1]) {  h[index]=(which(name==listg1[i,1])); h1[index]=(which(name== listg1[i,2]));index = index+1 }
 
 dh=cbind(h,h1)
 
 r=cbind(dh, listg1[,3])
 
 
 name2=V(l2_g2)$name


 h2 = NULL
  h3=NULL
  index = 1

for (i in 1:dim(listg2)[1]) {  h2[index]=(which(name2==listg2[i,1])); h3[index]=(which(name2== listg2[i,2]));index = index+1 }


dh2=cbind(h2,h3)


r2=cbind(dh2, listg2[,3])


cnodem=cbind(V(l2_g1)$name,V(l2_g1)$w)
cnodem2=cbind(V(l2_g2)$name,V(l2_g2)$w)
 
 
 
b=0
ind=1






# for (i in 1:dim(r)[1])   {
    # if (r[i,3]==1 & r2[i,3]==1 ){ b[ind]=1 }   else if (r[i,3]==1 & r2[i,3] < delta | r2[i,3]==1 & r[i,3] < delta ) {b[ind]=0.2 }  else if (r[i,3]==1 & r2[i,3] > delta | r2[i,3]==1 & r[i,3] > delta ) {b[ind]=0.5 }  else {b[ind]=0 }; ind=ind+1}


 for (i in 1:dim(r)[1])   {
    if (r[i,3]==1 & r2[i,3]==1 & cnodem[dh[i,1],2]==cnodem[dh[i,2],2] & cnodem2[dh2[i,1],2]==cnodem2[dh2[i,2],2]){ b[ind]=1 } else if (r[i,3]==1 & r2[i,3]==1 & (cnodem[dh[i,1],2]!=cnodem[dh[i,2],2] | cnodem2[dh2[i,1],2]!=cnodem2[dh2[i,2],2])){ b[ind]=0.9 }  else if (r[i,3]==1 & r2[i,3] < delta | r2[i,3]==1 & r[i,3] < delta & ( cnodem[dh[i,1],2]==cnodem[dh[i,2],2] & cnodem2[dh2[i,1],2]==cnodem2[dh2[i,2],2])) {b[ind]=0.2 } else if (r[i,3]==1 & r2[i,3] < delta | r2[i,3]==1 & r[i,3] < delta & ( cnodem[dh[i,1],2]!=cnodem[dh[i,2],2] | cnodem2[dh2[i,1],2]!=cnodem2[dh2[i,2],2])) {b[ind]=0.1 } else if (r[i,3]==1 & r2[i,3] > delta | r2[i,3]==1 & r[i,3] > delta & ( cnodem[dh[i,1],2]==cnodem[dh[i,2],2] & cnodem2[dh2[i,1],2]==cnodem2[dh2[i,2],2])) {b[ind]=0.5 } else if (r[i,3]==1 & r2[i,3] > delta | r2[i,3]==1 & r[i,3] > delta & ( cnodem[dh[i,1],2]!=cnodem[dh[i,2],2] | cnodem2[dh2[i,1],2]!=cnodem2[dh2[i,2],2])) {b[ind]=0.4 } else {b[ind]=0 }; ind=ind+1}
finall1=listg1[,-3]
finall2=listg2[,-3]

 finale=rbind(finall1, finall2)


 d4=cbind(finale,b)
 
 
 d5=d4

 h2 = NULL
  h3=NULL
   h4=NULL
  index = 1

for (i in 1:dim(d5)[1])   { if(d5[i,3]!=0){ h2[index]=d5[i,1]; h3[index]=d5[i,2];h4[index]=d5[i,3] }else{h2[index]=d5[i,1]; h3[index]=d5[i,2];h4[index]=d5[i,3]};index = index+1 }

d=cbind(h2,h3)
d=cbind(d,h4)
el <- d[rowSums(is.na(d)) == 0,]


#aggiungo il colore
colore=rep(2, dim(el)[1])

ell2=cbind(el,colore)




#costruisco alignment graph intralayer
E(g)$weight =edge_attr(g)$V3
 
  #creo grafo per intelayer
s_int1=delete.edges(g, which(E(g)$weight==2))
  s_int1 =delete.edges(s_int1, which(E(s_int1)$weight==1))

 #creo layer 1 e 2 da rete g2
 E(g2)$weight =edge_attr(g2)$V3 
  #V(g2)$w <-cnode2$color
  

s_int2=delete.edges(g2, which(E(g2)$weight==2))
  s_int2 =delete.edges(s_int2, which(E(s_int2)$weight==1))


 list=get.edgelist(s_int1)
 
 list2=get.edgelist(s_int2)
if (dim(list)[1]==dim(list2)[1]) {
 h2 = NULL
  h3=NULL
   h4=NULL
   h5=NULL
   h6=NULL
   h7=NULL
   h8=NULL
   h9=NULL
  index = 1

for (i in 1:dim(list2)[1])  { if (list2[i,1]== list[i,1] & list2[i,2]== list[i,2] ){ h2[index]=list2[i,1]; h3[index]=list2[i,2]; h4[index]=0.9 } else {h5[index]=list2[i,1]; h6[index]=list2[i,2];h7[index]=list[i,1]; h8[index]=list[i,2]; h9[index]=0.4}; index = index+1}	
  interl=cbind(h2,h3,h4)
  interl2=cbind(h5,h6)
    interl22=cbind(h7,h8)
interl222=rbind(interl2, interl22)
 interl222=cbind(interl222,h9)
 interf=rbind(interl, interl222)
interf <- interf[rowSums(is.na(interf)) == 0,]} else if(dim(list)[1]>dim(list2)[1]){h2 = NULL
  h3=NULL
   h4=NULL
   h5=NULL
   h6=NULL
   h7=NULL
   h8=NULL
   h9=NULL
    h10=NULL
for (i in 1:dim(list2)[1])  { for (j in (dim(list2)[1]+1):dim(list)[1])  { if (list2[i,1]== list[i,1] & list2[i,2]== list[i,2] ){ h2[i]=list2[i,1]; h3[i]=list2[i,2]; h4[i]=0.9 } else {h5[i]=list2[i,1]; h6[i]=list2[i,2]; h7[i]=0.4;  h8[j]=list[j,1]; h9[j]=list[j,2]; h10[j]=0.4  }}} 	
  interl=cbind(h2,h3,h4)
  interl2=cbind(h5,h6,h7)
    interl222=cbind(h8, h9,h10)
 interf=rbind(interl, interl2, interl222)
interf <- interf[rowSums(is.na(interf)) == 0,] 	}else {
 h2 = NULL
  h3=NULL
   h4=NULL
   h5=NULL
   h6=NULL
   h7=NULL
   h8=NULL
   h9=NULL
    h10=NULL
for (i in 1:dim(list)[1])  { for (j in (dim(list)[1]+1):dim(list2)[1])  { if (list[i,1]== list2[i,1] & list[i,2]== list2[i,2] ){ h2[i]=list[i,1]; h3[i]=list[i,2]; h4[i]=0.9 } else {h5[i]=list[i,1]; h6[i]=list[i,2]; h7[i]=0.4;  h8[j]=list2[j,1]; h9[j]=list2[j,2]; h10[j]=0.4  }}} 	
  interl=cbind(h2,h3,h4)
  interl2=cbind(h5,h6,h7)
  interl222=cbind(h8, h9,h10)
 interf=rbind(interl, interl2, interl222)
interf <- interf[rowSums(is.na(interf)) == 0,] 	
 }

colore=rep(3, dim(interf)[1])

interf =cbind(interf,colore)






#costruisco grafo finale con i due intra e un inter che è relativo aggli archi

alignfinal=rbind(ell,ell2, interf)


#scambio la colonna colore con peso
#alignfinal =cbind(paste(alignfinal[,1], alignfinal[,1],sep="-"), paste(alignfinal[,2], alignfinal[,2],sep="-"), alignfinal[,4], alignfinal[,3])
alignfinal =cbind(paste(alignfinal[,1], alignfinal[,1],sep="-"), paste(alignfinal[,2], alignfinal[,2],sep="-"), alignfinal[,3], alignfinal[,4])
 write.table(alignfinal,"~/Desktop/testputt.txt",append = FALSE, quote = FALSE,row.names=FALSE, col.names=FALSE, sep=",")

 write.table(alignfinal,"~/Desktop/25.txt",append = FALSE, quote = FALSE,row.names=FALSE, col.names=FALSE, sep="\t")

x=read_ml("testputt.txt", name = "x", sep = ',', aligned = FALSE)

 # c1 <- glouvain_ml(x)
    c1 <- infomap_ml(x)

names(c1)[1] <-("node")

names(c1)[2] <-("weight")

names(c1)[3] <-("community")


c11=c(c1$weight)
c11=as.numeric(c11)
h=0
index=1

for (i in 1:length(c11))   {
 if(c11[i]==1){ h[index]="HomMatch "}
else if (c11[i]==0.5) {h[index]="HomMissMatch"}
else if (c11[i]==0.2) {h[index]="HomGap"}
else if (c11[i]==0.9) {h[index]="HetMatch"}
else if (c11[i]==0.4) {h[index]="HetMissMatch"} else{ h[index]="no edge"};
index = index+1 }

c2=cbind(c1,h)
c22=cbind(c2[1], c2[3],c2[2],c2[4])
names(c22)[4] <-("edge type")

 l_a<-split(c22, c22$community)
 (capture.output(print(l_a), file="25c.txt"))
 
 print("done")






}
