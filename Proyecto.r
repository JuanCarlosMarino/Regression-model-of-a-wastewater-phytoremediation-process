#Datos Atipicos
Y<-c(1.6,3.6,4.4,3.4,2.2,2.8,3.8,4.6,5.5,45,6.2,-300,200,1000)
X<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
B<-quantile(Y)
QR<-B[4]-B[2]
MIN<-B[2]-(1.5*QR)
MAX<-B[4]+(1.5*QR)
estado<-0
q<-c()
r<-c()
for(i in 1:length(Y)){
  if (Y[i]<MIN || Y[i]>MAX) {
    print("Numero atípico= ")
    print(Y[i])
    print("En la posición= ")
    print(i)
    estado<-1
  }else{
    w<-c(Y[i])
    z<-c(X[i])
    q<-c(q,w)
    r<-c(r,z)
  }
}
if (estado == 0) {
  print("No hay numeros atípicos")
}
X<-r
Y<-q
#Interpolar
W<-c()
for (i in X[1]:X[length(X)]) {
  valor<-FALSE
  for (j in 1:length(X)) {
    if (i == X[j]) {
      valor<-TRUE
    }
  }
  if (valor == FALSE) {
    Z<-c(i)
    W<-c(W,Z)
  }
}

valorm<-length(X)%/%2
valorf<-length(X)

for (k in W) {
  inte=((k-X[valorm])/(X[1]-X[valorm]))*((k-X[valorf])/(X[1]-X[valorf]))*(Y[1])
  inte= inte+(((k-X[1])/(X[valorm]-X[1]))*((k-X[valorf])/(X[valorm]-X[valorf]))*(Y[valorm]))
  inte= inte+(((k-X[valorm])/(X[valorf]-X[valorm]))*((k-X[1])/(X[valorf]-X[1]))*(Y[valorf]))
  print("La interpolacion de: ")
  print(k)
  print("Es: ")
  print(inte)
  
}