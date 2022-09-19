##Algoritmo Genético

library(dplyr)
max_weight = 500
values <-   c(55, 70,
              31, 32,
              43, 36,
              23, 65,
              48, 49,
              40, 62,
              53, 25,
              54, 47,
              39, 78,
              77, 65,
              39, 23,
              59, 45,
              53, 43,
              28, 44,
              71, 71,
              36, 67,
              62, 48,
              30, 36,
              48, 76,
              42, 2)
weights_bids <- matrix(values,ncol = 2, byrow = TRUE) 
colnames(weights_bids)<-c('peso','pago')

plot(weights_bids)

chromosome <- function(weights){
  weight   <- 0
  pool     <- weights
  pool_idx <- 1:length(pool)
  selected <- c()
  for(i in 1:length(pool)){
    value_idx <- sample(pool_idx,1)
    pool_idx  <- setdiff(pool_idx,value_idx)
    weight    <- weight+pool[value_idx]
    if(weight > max_weight) break
    selected  <- c(selected,value_idx)
  }
  out <- rep(0,length(pool) )
  out[selected] <- 1
  return(out)
}

population <- t(sapply(1:500,function(x,w){chromosome(w)},w=weights_bids[,2] ))
population <- cbind(population,population %*% weights_bids[,1])

# Crossover half weight 
crossover<- function(child){
p1<- population[sample(1:500,1),-21]
p2 <- population[sample(1:500,1),-21]
(rcut <- which(cumsum(p1*weights_bids[,2])>max_weight/2)[1])
if(!is.na(rcut)){
  (lcut <- rcut-1)
  proto_child <- c(p1[1:lcut],p2[rcut:20])
} else {
  (rcut <- which(cumsum(p2*weights_bids[,2])>max_weight/2)[1])
  (lcut <- rcut-1)
  proto_child <- c(p2[1:lcut],p1[rcut:20])
}
remove_weight <- which(cumsum(proto_child*weights_bids[,2])>500)
proto_child[remove_weight] <- 0
child <- proto_child
return(child)
}
p1
p2
child

#Mutate
mutacion<- function(child){
  child <- child[2:(length(child)-1)] 
  index<-sample(1:length(child),2)
  i <- child[index[1]]
  child[index[1]] <- child[index[2]]
  child[index[2]] <- i
  child <- c(1,child,1)
  return(child)
}
child



#Crear nueva población

N=20
nuevapoblacion<- matrix(rep(0,N=1),ncol = N+1)
for (i in 1:500) {
  parents<- sample(1:nrow(population),size = 2)
  padres<- population[parents,1:(N+1)]
  hijo<- crossover(padres)
  if(runif(1)<0.01){
    hijo<- mutacion(hijo)
  }
  nuevapoblacion<- rbind(nuevapoblacion,hijo)
}
population<- nuevapoblacion[-1,-1]
population <- cbind(population,population %*% weights_bids[,1])

population
data.frame(population)






## Simulated ANNEALING




costo_paquete <- function(route,costo){
  sum_costo<-0
  for(i in 1:length(route[-1])){
    out <- costo[ route[i], route[i+1]  ]
    sum_costo <- sum_costo + out
  }
  return(sum_costo)
}

sample_paquete<- function(initial_node=1,nodes=4){
  nodes<- 1:nodes
  sample_nodes<- nodes[-initial_node]
  attach_nodes<- sample(sample_nodes)
  out<- c(initial_node,attach_nodes,initial_node)
  return(out)
}




initial_paquete <- function(n=20,pac_costo){
  dist<-data.frame()
  paquete <- nrow(pac_costo)
  for(i in 1:n){
    ruta <- sample_paquete(1,nrow(pac_costo))
    dist<-rbind( dist, c(ruta, costo_paquete(ruta,pac_costo) )  )
  }
  names(dist)[ncol(dist)]<-"costo"
  x <- t(dist[dist$costo==max(dist$costo), ])[,1]
  x <- as.integer(x[1:(paquete+1) ])
  return(x)
} 

vecino <- function(vec){
  n <- length(vec)-1
  change <- sample(2:n, size = 2, replace = FALSE)
  temp <- vec[change[1]]
  vec[change[1]] <- vec[change[2]]
  vec[change[2]] <- temp
  return(vec)
} 



anneal <- function(weights_bids,N=50, temp=20,   alpha = 0.95){
  temp_min = 0.001
  
  capacidad <- weights_bids
  pac_costo <- as.matrix(dist(capacidad[,1:2]))
  ruta <- initial_paquete(N,pac_costo)
  costo <- costo_paquete(ruta,pac_costo)
  while(temp > temp_min){
    i <- 1
    print(temp)
    while(i <= 100){
      nuevos_paquetes <- vecino(ruta)
      nuevo_costo <- costo_paquete(nuevos_paquetes,pac_costo) 
      acceptance_probability <- exp( (costo-nuevo_costo)/temp)
      if(acceptance_probability < runif(1) ){
        ruta <- nuevos_paquetes
        costo <- nuevo_costo
      } else if(nuevo_costo>costo) {
        costo <- nuevo_costo
        ruta <- nuevos_paquetes
      }
      i <- i+1
    }
    temp <- temp*alpha  
  }
  plot(capacidad[,1:2])
  x<- ruta 
  polygon(capacidad[x,1:2],border="red",lwd=3)
  return(View(data.frame(c(ruta,costo))))
}



