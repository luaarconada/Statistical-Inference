library(ggplot2)

#Uniform density function on the interval (-2,2)
uniform <- function(x){0.25*(x >= -2)*(x <= 2)}

#We define the absolute value of the derivatives of the partitions of g^-1
aux1 <- function(y){1/(sqrt(1-4*y))}
aux2 <- function(y){1/(sqrt(1+4*y))}

#We define the partitions of g^-1
g1 <- function(y){(-1 - sqrt(1 - 4*y))/2}
g2 <- function(y){
  if(y < 0){(1 - sqrt(1 + 4*y))/2}
  else{(-1 + sqrt(1 + 4*y))/2}
  }
g3 <- function(y){(1 + sqrt(1 + 4*y))/2}

#We define the density function of Y = g(X) as a piecewise function
density <- function(y){
  
  if(y < -1/4){ uniform(g1(y))*aux1(y) }
  
  else if((-1/4 < y)&&(0 > y)){ 
    uniform(g1(y))*aux1(y) + uniform(g2(y))*aux2(y) + uniform(g3(y))*aux2(y)
  }

  else if((y > 0)&&(y < 1/4)){
    uniform(g1(y))*aux1(y) + uniform(g2(y))*aux1(y) + uniform(g3(y))*aux2(y)
  }

  else if(y > 1/4){ uniform(g3(y))*aux2(y) }
  
  else{0}
}

#Graphical representation of the density
y <- seq(-2.1, 2.1, length.out = 1000)
fy <- unlist(lapply(y, FUN = density))
df <- data.frame(y = y, fy = fy)
fig1 <- ggplot(df, aes(x = y, y = fy)) +
        geom_line(color = 'red', size = 0.75) +
        xlab('Domain') +
        ylab('Density') +
        ggtitle('Density of Y = g(X)') +
        theme_classic()

#We obtain a similar graph through simulation
set.seed(42)
g <- function(x){if(x < 0){ -x*(x+1) } else if(x >= 0){ x*(x-1) }}
sim <- 4*runif(100000)-2
g_sim <- unlist(lapply(sim,FUN = g))
df2 <- data.frame(valor = g_sim)
fig2 <- ggplot(df2,aes(x=g_sim))+
        geom_histogram(aes(y = ..density..), binwidth = 0.05, fill="#FF9999", colour="black") +
        xlab('Domain') +
        ylab('Density') +
        ggtitle('Density histogram of 100000 values of Y') + 
        theme_classic()

#We plot the derivative of g and its inverse
dg <- function(x){if(x < 0){-2*x - 1} else if(x >= 0){-2*x + 1} }
dg.inv <- function(x){1/dg(x)}

x <- seq(-1,1, length.out = 1000)

g.x <- unlist(lapply(x, FUN = g))
dg.x <- abs(unlist(lapply(x, FUN = dg)))
dg.inv.x <- abs(unlist(lapply(x, FUN = dg.inv)))

df3 <- data.frame(x = x, val = g.x)
df4 <- data.frame(x = x, der = dg.x)
df5 <- data.frame(x = x, der = dg.inv.x)

fig3 <- ggplot(df3, aes(x = x, y = val)) +
        geom_line(color = 'red', size = 0.75) +
        xlab('Domain') +
        ylab('Value') +
        ggtitle('Plot of g') +
        theme_classic()

fig4 <- ggplot(df4, aes(x = x, y = der)) +
        geom_line(color = 'red', size = 0.75) +
        xlab('Domain') +
        ylab('Value') +
        ggtitle('Plot of dg/dy') +
        theme_classic()

fig5 <- ggplot(df5, aes(x = x, y = der)) +
        geom_line(color = 'red', size = 0.75) +
        xlab('Domain') +
        ylab('Value') +
        ggtitle('Plot of the inverse of dg/dy') +
        theme_classic()

#We plot a simple proof of concept

concept <- function(x){(x< -0.2) - (-0.2<=x)*(x<0.2)*5*x - (0.2<x)}

x2 <- seq(-1,1, length.out = 1000)
concept.x <- unlist(lapply(x2, FUN = concept))
df6 <- data.frame(x = x2, val = concept.x)

fig6 <- ggplot(df6, aes(x = x2, y = val)) +
        geom_line(color = 'red', size = 0.75) +
        xlab('Domain') +
        ylab('Value') +
        ggtitle('Proof of concept') +
        theme_classic()


fig1
fig2
fig3
fig4
fig5
fig6

hist(g_sim, prob = TRUE, 
     col = 'white',
     ylab = 'Density',
     main = 'Density curve over the histogram',
     xlab = 'Domain',
     breaks = seq(-2,2,length.out=100))
lines(y, fy, col = 'red', lwd = 2)
