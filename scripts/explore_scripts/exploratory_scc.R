library('dplyr')
library('haven')
library('labelled')
library('here')
library('ggplot2')
library('fields')
library('stringr')
library('spmodel')
library('spdep')

lat <- runif(200)
lon <- runif(200)

plot(lon,lat)

coords <- cbind(lon, lat)

b <- 1+3*(lat>0.5)

x <- runif(200)

plot(coords,col = c('red','blue')[ceiling(b/2)])

y <- b*x + runif(200)

plot(x,y)


A <- (((rdist(scale(cbind(coords,y))))))

nb.w <- mat2listw(A, style = 'B')



mst.bh <- mstree(nb.w,128)
plot(mst.bh, coords, col = 2, cex.lab = .6, cex.circles = 0.025, fg = 'blue', add = F)


k=2 ## two clusters

res1 <- skater(mst.bh[,1:2], b, k-1) 
membership = res1$groups
### groups size
table(membership) 

plot(res1, coords, cex.circles=0.035, cex.lab=.7) 


## Lasso function
f1 <- function(beta, lambda, H){
return(
  (1/200)*sum((y-x*beta)^2) + lambda*sum(abs(beta[H[,1]] - beta[H[,2]]))
)
}

pars <- optim(rep(2,200),f1,lambda = 0, H = mst.bh,
              method = 'L-BFGS-B')$par

n1 <- c()
test_lambda <- seq(from = 0.01637, to = 0.0164, length.out= 100)

for(i in test_lambda){
  pars <- optim(rep(1,200),f1,lambda = i, H = mst.bh,
                method = 'L-BFGS-B')$par
  n1 <- c(n1,length(unique(round(pars,2))))
}

n1

table(round(pars, 2))
