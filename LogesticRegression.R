
bls<-function(x0,x,y){
  x0%*%solve(t(x)%*%x,t(x)%*%y)
}

knn<-function(x0,x,y,k){
      x=as.matrix(x)
      p=dim(x)[2]	
      n=dim(x)[1]        
      dis=rep(0,n)
      for(i in 1:p){
        dis= (x0[i]-x[,i])^2+dis
      }
      ind=order(dis)[1:k]
      mean(y[ind])
}

loess0<-function(x0,x,y,kern,lam){
  x=as.matrix(x)
  p=dim(x)[2]	
  n=dim(x)[1]        
  dis=rep(0,n)
  for(i in 1:p){
    dis= (x0[i]-x[,i])^2+dis
  }
  teta0=sum(kern(dis,lam)*y)/sum(kern(dis,lam))
}

loess1<-function(x0,x,y,kern,lam){
  x=as.matrix(x)
  p=dim(x)[2]	
  n=dim(x)[1]        
  dis=rep(0,n)
  for(i in 1:p){
    dis= (x0[i]-x[,i])^2+dis
  }
  ##Your code here
  a<-sum(kern(dis,lam))
  b<-sum(kern(dis,lam)*x)
  c<-sum(kern(dis,lam)*x*x)
  d<-sum(kern(dis,lam)*y)
  e<-sum(kern(dis,lam)*x*y)
  A <- matrix(c(a, b, b, c),nrow=2)
  B <- matrix(c(d,e),nrow=2)
  teta <- solve(A, B)
  teta[1]+teta[2]*x0
}

kern<-function(x,lam){
  exp(-x/lam)/lam
}
######
## sin(x) Regression Example
######
set.seed(100)
x=runif(100,0,2*pi)
y=sin(x)+rnorm(100,,0.1)

xgrid=seq(0,2*pi,length=500)
n=length(xgrid)
ygrid=vector(length=n)
k=15
lam=0.01

for(i in 1:n){
##  ygrid[i]=##loess d=0 , d=1
  ygrid[i]=loess0(xgrid[i],x,y,kern,lam)
  ygrid[i]=loess1(xgrid[i],x,y,kern,lam)
}
plot(x,y,pch=16)
lines(xgrid,ygrid,col=c("red"))
lines(xgrid,sin(xgrid),col=c("blue"))
