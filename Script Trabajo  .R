
# Parte 2  ----------------------------------------------------------------

install.packages("pdfetch")
install.packages("ggplot2")

library(pdfetch)
library(ggplot2)

# Almacenamos las bases 

AAPL <- pdfetch_YAHOO("AAPL", fields ="close", from = as.Date("2000-01-01"), to = as.Date("2018-08-31"), interval = "1m")
MSFT<- pdfetch_YAHOO("MSFT", fields ="close", from = as.Date("2000-01-01"), to = as.Date("2018-08-31"), interval = "1m")

# Creamos la funcion 

funcion_tarea<- function(x,graph,return,jb) {
  
  if (jb=="yes") {
    Skew <- (sum((x - mean(x))^3)/length(x))/(sum((x - mean(x))^2)/length(x))^(3/2)
    Kur <- length(x) * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
    Jb <- (length(x)/6) * (Skew^2 + 0.25 * ((Kur - 3)^2))
    pval <- 1 - pchisq(Jb, df = 2)
    if (pval<0.05) {
      print("SE RECHAZA LA NULA DE NORMALIDAD")
    }else {
      print("SE ACEPTA LA NULA DE NORMALIDAD")
    }
  }
    
  if (return=="yes") {
    # Retornos
    x$Retornos<- diff(x)/x[-length(x)]
    print(x$Retornos)
  }
    if (return=="no") {
      # Retornos Acumulados
      x$Retornos[which(is.na(x$Retornos))] <- 0 
      x$Retornos_cum <- cumsum(x$Retornos)
      print(x$Retornos_cum)
    }
    
  if (graph=="tipo1") {
    # Graficos Retornos
    plot.ts(x$Retornos)
  }
    if (graph=="tipo2") {
      # Graficos Retornos Acumulados 
      plot.ts(x$Retornos_cum)     
    }
}


# Probamos la función 

funcion_tarea(AAPL,graph ="tipo1",return = "yes",jb="yes")
funcion_tarea(MSFT,graph ="tipo1",return = "yes",jb="yes")

# Parte 3  ----------------------------------------------------------------

n = c(50,100,500,1000)
reps=1000
betas=matrix(NA,nrow=reps,ncol=4)
beta0=2
beta1=2.5
beta2=1

for (j in 1:length(n)) {
  x1<-rnorm(n[j],20,1)
for (i in 1:reps) {
  u=rnorm(n[j],0,1)
  e=rnorm(n[j],0,1)
  x2 <- 0.8*x1 + e
  v <- beta2*x2 + u
  y=beta0+beta1*x1+v
  model_ols <- lm(y ~ x1)
  betas[i,j]=model_ols$coef[2]

}
}

mean(betas[,1])
mean(betas[,2])
mean(betas[,3])
mean(betas[,4])

var(betas[,1])
var(betas[,2])
var(betas[,3])
var(betas[,4])

# Para graficar los Betas Estimados

betas <- as.data.frame(betas)

ggplot(data = betas, aes(x=V1)) + geom_histogram(color="white",fill="#0d8796") + theme_minimal()  + ggtitle(label = " N°50")
ggplot(data = betas, aes(x=V2)) + geom_histogram(color="white",fill="#0d8796") + theme_minimal() + ggtitle(label = " N°100")
ggplot(data = betas, aes(x=V3)) + geom_histogram(color="white",fill="#0d8796") + theme_minimal() + ggtitle(label = " N°500")
ggplot(data = betas, aes(x=V4)) + geom_histogram(color="white",fill="#0d8796") + theme_minimal() + ggtitle(label = " N°1000")

# Que pasa si X2 es uniforme

n = c(50,100,500,1000)

reps=1000
betas_c2=matrix(NA,nrow=reps,ncol=4)

beta0=2
beta1=2.5
beta2=1

for (j in 1:length(n)) {
for (i in 1:reps) {
  
  u=rnorm(n[j],0,1)
  x1<-rnorm(n[j],20,1)
  x2 <- runif(n[j],0,1)
  v <- beta2*x2 + u 
  y=beta0+beta1*x1+ v
  model_ols <- lm(y ~ x1 + x2)
  betas_c2[i,j]=model_ols$coef[2]

}
}

betas_c2 <- as.data.frame(betas_c2)

ggplot(data = betas_c2, aes(x=V1)) + geom_histogram(color="white",fill="#0d8796",bins = 30) + theme_minimal()  + ggtitle(label = " N°50")
ggplot(data = betas_c2, aes(x=V2)) + geom_histogram(color="white",fill="#0d8796",bins = 30) + theme_minimal() + ggtitle(label = " N°100")
ggplot(data = betas_c2, aes(x=V3)) + geom_histogram(color="white",fill="#0d8796",bins = 30) + theme_minimal() + ggtitle(label = " N°500")
ggplot(data = betas_c2, aes(x=V4)) + geom_histogram(color="white",fill="#0d8796",bins = 30) + theme_minimal() + ggtitle(label = " N°1000")


