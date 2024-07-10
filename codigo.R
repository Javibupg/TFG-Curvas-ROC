# Ejemplos curvas ROC ----
library(movieROC)
library(tidyverse)
library(latex2exp)

#Binormal con la misma desviacion tipica y distinta media (curva ROC usual)
n <- 1e5
xi <- rnorm(n, 2, 1)
chi <- rnorm(n, 0, 1)
X <- c(xi, chi) #Realizacion muestral del marcador
D <- c(rep(1,n),rep(0,n))
roc <- gROC(X,D)

dev.off()
layout(mat = matrix(c(1,2), nrow = 1, ncol = 2), heights = c(2, 2), widths = c(3, 5))
plot_densities(roc, main='Funciones de densidad', xlab = 'Marcador',
               cex.axis = 1.5, cex.lab = 1.5,
               h=2, col = c("dodgerblue4", "#D2691E"), cex.main = 1.5)
legend('topright', legend = c(TeX('$\\chi$'), TeX('$\\xi$')),
       col = c('dodgerblue4', '#D2691E'), lwd = 2, bty = 'n',
       inset = 0.01, cex = 1.5)
par(mfg = c(1,2))
plot.regions(roc, plot.roc=FALSE, xlim=c(-3,5), legend=FALSE,
             main='Intervalos de clasificación', xlab = 'Marcador',
             FPR = NULL, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)


# Binormal con la misma media y distinta desviacion tipica (curva gROC)
n <- 1e5
xi <- rnorm(n, 0, 2)
chi <- rnorm(n, 0, 1)
X <- c(xi, chi)
D <- c(rep(1,n),rep(0,n))
roc <- gROC(X,D)
groc <- gROC_param(X,D, side= "both")

dev.off()
layout(mat = matrix(c(1,2), nrow = 1, ncol = 2), heights = c(2, 2), widths = c(3, 5))
plot_densities(roc, main='Funciones de densidad', xlab = 'Marcador',
               cex.axis = 1.5, cex.lab = 1.5,
               h=2, col = c("dodgerblue4", "#D2691E"), cex.main = 1.5)
legend('topright', legend = c(TeX('$\\chi$'), TeX('$\\xi$')),
       col = c('dodgerblue4', '#D2691E'), lwd = 2, bty = 'n',
       inset = 0.01, cex = 1.5)
par(mfg = c(1,2))
plot.regions(groc, plot.roc=FALSE, xlim=c(-3,5), legend=FALSE,
             main='Intervalos de clasificación', xlab = 'Marcador',
             FPR = NULL, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)


#Ejemplo Prop. escenario binormal
n <- 1e5
xi <- rnorm(n, 0.3, 1.5)
chi <- rnorm(n, 0, 1)
X <- c(xi, chi) #Realizacion muestral del marcador
D <- c(rep(1, n), rep(0, n))
roc <- gROC(X, D, side = 'right')
dev.off()
groc <- gROC_param(X,D, side= "both")

a <- 1
b <- 2
plot_densities(roc, main='Funciones de densidad', xlab = 'Marcador',
               cex.axis = 1.5, cex.lab = 1.5,
               h=2, col = c("dodgerblue4", "#D2691E"), cex.main = 1.5)
legend('topright', legend = c(TeX('$\\chi$'), TeX('$\\xi$')),
       col = c('dodgerblue4', '#D2691E'), lwd = 2, bty = 'n',
       inset = 0.01, cex = 1.5)
plot.regions(roc, xlim=c(-3,5), legend=FALSE,
             main='Intervalos de confianza', xlab = 'Marcador',
             cex.main = b+0.5, cex.axis = b, cex.lab = b, FPR=NULL)

plot.regions(groc, plot.roc=TRUE, xlim=c(-3,5), legend=FALSE,
             main='Intervalos de confianza', xlab = 'Marcador',
             FPR = NULL, cex.main = b+0.5, cex.axis = b, cex.lab = b)


##Plots de areas bajo la curva en función de distintos parametros ----
n <- 1e5
chi <- rnorm(n, 0, 1)
D <- c(rep(1, n), rep(0, n))
b <- 5
#y1 <- c()
y2 <- c()
x <- seq(0,5,0.1)
y1 <- pnorm(x/sqrt(1+b^2))

for (a in x) {
  xi <- rnorm(n, a, b)
  X <- c(xi, chi) #Realizacion muestral del marcador
  #roc <- gROC(X, D)
  eroc <- gROC_param(X, D, side = 'both')
  #y1 <- c(y1, roc$auc)
  y2 <- c(y2, eroc$auc)
}
x <- seq(-5, 5, 0.1)
y1 <- c(rev(y1)[1:length(y1)-1], y1)
y2 <- c(rev(y2)[1:length(y2)-1], y2)
ggplot(data.frame(x = x, y = y2)) +
  scale_x_continuous(breaks = c(-5,-2.5,0,2.5,5)) +
  scale_y_continuous(breaks = c(0.5,0.75,1), limits = c(0.45,1)) +
  geom_line(aes(x = x, y = y1), linetype = 1, linewidth = 2, col = 'darkolivegreen4') +
  geom_line(aes(x = x, y = y2), linetype = 1, linewidth = 2, col = 'darkorange3') +
  theme(panel.background = element_rect(fill='white'),
        panel.border = element_rect(fill = NA),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  xlab(TeX('$(\\mu_\\xi-\\mu_\\chi)/\\sigma_\\chi$')) +
  ylab('AUC') +
  annotate('text', label = TeX('$\\frac{\\sigma_\\xi}{\\sigma_\\chi}=5$'),
           x = -3.5, y = 0.55, size = 5)


b <- 2.2
y2 <- c()
x <- seq(0,5,0.1)
y1 <- pnorm(x/sqrt(1+b^2))

for (a in x) {
  xi <- rnorm(n, a, b)
  X <- c(xi, chi) #Realizacion muestral del marcador
  #roc <- gROC(X, D)
  eroc <- gROC_param(X, D, side = 'both')
  #y1 <- c(y1, roc$auc)
  y2 <- c(y2, eroc$auc)
}
x <- seq(-5, 5, 0.1)
y1 <- c(rev(y1)[1:length(y1)-1], y1)
y2 <- c(rev(y2)[1:length(y2)-1], y2)
ggplot(data.frame(x = x, y = y2)) +
  scale_x_continuous(breaks = c(-5,-2.5,0,2.5,5)) +
  scale_y_continuous(breaks = c(0.5,0.75,1), limits = c(0.45,1)) +
  geom_line(aes(x = x, y = y1), linetype = 1, linewidth = 2, col = 'darkolivegreen4') +
  geom_line(aes(x = x, y = y2), linetype = 1, linewidth = 2, col = 'darkorange3') +
  theme(panel.background = element_rect(fill='white'),
        panel.border = element_rect(fill = NA),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  xlab(TeX('$(\\mu_\\xi-\\mu_\\chi)/\\sigma_\\chi$')) +
  ylab('AUC') +
  annotate('text', label = TeX('$\\frac{\\sigma_\\xi}{\\sigma_\\chi}=2.2$'),
           x = -3.5, y = 0.55, size = 5)


b <- 0.6
y2 <- c()
x <- seq(0,5,0.1)
y1 <- pnorm(x/sqrt(1+b^2))

for (a in x) {
  xi <- rnorm(n, a, b)
  X <- c(xi, chi) #Realizacion muestral del marcador
  roc <- gROC(X, D)
  eroc <- gROC_param(X, D, side = 'both2')
  y2 <- c(y2, eroc$auc)
}
x <- seq(-5, 5, 0.1)
y1 <- c(rev(y1)[1:length(y1)-1], y1)
y2 <- c(rev(y2)[1:length(y2)-1], y2)
ggplot(data.frame(x = x, y = y2)) +
  scale_x_continuous(breaks = c(-5,-2.5,0,2.5,5)) +
  scale_y_continuous(breaks = c(0.5,0.75,1), limits = c(0.45,1)) +
  geom_line(aes(x = x, y = y1), linetype = 1, linewidth = 2, col = 'darkolivegreen4') +
  geom_line(aes(x = x, y = y2), linetype = 1, linewidth = 2, col = 'darkorange3') +
  theme(panel.background = element_rect(fill='white'),
        panel.border = element_rect(fill = NA),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  xlab(TeX('$(\\mu_\\xi-\\mu_\\chi)/\\sigma_\\chi$')) +
  ylab('AUC') +
  annotate('text', label = TeX('$\\frac{\\sigma_\\xi}{\\sigma_\\chi}=0.6$'),
           x = -3.5, y = 0.55, size = 5)


b <- 0.1
y2 <- c()
x <- seq(0,5,0.1)
y1 <- pnorm(x/sqrt(1+b^2))

for (a in x) {
  xi <- rnorm(n, a, b)
  X <- c(xi, chi) #Realizacion muestral del marcador
  roc <- gROC(X, D)
  eroc <- gROC_param(X, D, side = 'both2')
  y2 <- c(y2, eroc$auc)
}
x <- seq(-5, 5, 0.1)
y1 <- c(rev(y1)[1:length(y1)-1], y1)
y2 <- c(rev(y2)[1:length(y2)-1], y2)
ggplot(data.frame(x = x, y = y2)) +
  scale_x_continuous(breaks = c(-5,-2.5,0,2.5,5)) +
  scale_y_continuous(breaks = c(0.5,0.75,1), limits = c(0.45,1)) +
  geom_line(aes(x = x, y = y1), linetype = 1, linewidth = 2, col = 'darkolivegreen4') +
  geom_line(aes(x = x, y = y2), linetype = 1, linewidth = 2, col = 'darkorange3') +
  theme(panel.background = element_rect(fill='white'),
        panel.border = element_rect(fill = NA),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15)) +
  xlab(TeX('$(\\mu_\\xi-\\mu_\\chi)/\\sigma_\\chi$')) +
  ylab('AUC') +
  annotate('text', label = TeX('$\\frac{\\sigma_\\xi}{\\sigma_\\chi}=0.1$'),
           x = -3.5, y = 0.55, size = 5)

# Umbral optimo binormal ----

library(movieROC)
library(tidyverse)
library(latex2exp)
set.seed(1212)

#Ejemplo del cálculo del test de diagnóstico óptimo para el escenario binormal
n <- 5e5

muxi <- 2
sigmaxi <- 0.5
xi <- rnorm(n, muxi, sigmaxi)

muchi <- 1
sigmachi <- 0.8
chi <- rnorm(n, muchi, sigmachi)

X <- c(xi, chi) #Realizacion muestral del marcador
D <- c(rep(1, n), rep(0, n))
roc <- gROC(X, D) #curva ROC a derechas

#plot(roc)

#primero calculamos numericamente el valor de c para el cual el indice de Youden es maximo
c <- roc$c
t <- roc$t
R <- roc$roc
pos <- which.max(1-t+R) #posicion del punto en el que es maximo el indice de Youden
c_opt <- c[pos]
t_opt <- t[pos]
R_opt <- R[pos]

plot_densities(roc, main='Funciones de densidad', xlab = 'Marcador',
               cex.axis = 1.5, cex.lab = 1.5,
               h=2, col = c("dodgerblue4", "#D2691E"), cex.main = 1.5)
polygon(x = c(c_opt,4,seq(4,c_opt,-.01)),
        y = c(0,0,dnorm(seq(4,c_opt,-.01), muchi, sigmachi)),
        col = adjustcolor("dodgerblue4", alpha = .5))
polygon(x = c(c_opt,5,seq(5,c_opt,-.01)),
        y = c(0,0,-dnorm(seq(5,c_opt,-.01), muxi, sigmaxi)),
        col = adjustcolor("#D2691E", alpha = .5))
abline(v = c_opt, lty = 2)
legend('topright', legend = c(TeX('$\\chi$'), TeX('$\\xi$')),
       col = c('dodgerblue4', '#D2691E'), lwd = 2, bty = 'n',
       inset = 0.01, cex = 1.5)
plot.regions(roc, plot.roc=TRUE, xlim=c(-3,5), legend=FALSE,
             main='Intervalos de clasificación', xlab = 'Marcador',
             FPR = t_opt, cex.main = 2.5, cex.axis = 2, cex.lab = 2.5)

#Ahora calculamos el valor teórico
a <- (muxi-muchi)/sigmaxi
b <- sigmachi/sigmaxi
Tt <- a*b*sigmachi + (b^2-1)*muchi
Rt <- (a*sigmachi + b*muchi)^2 - muchi^2 - 2*sigmachi^2*log(b)
c <- (Tt - sqrt(Tt^2-(b^2-1)*Rt))/(b^2-1)


# Bandas de confianza ----
## Ejemplo de bandas de confianza en el escenario binormal ----
#Este apartado creo que lo iba a corregir Sonia, cambiar ggplot por plot
#Bandas de confianza
library(nsROC)

set.seed(123)
n <- 5e2
a <- 1
b <- 2
alpha <- 0.05
xi <- rnorm(n, a, b)
mu1 <- mean(xi)
sigma1 <- sqrt(var(xi))
chi <- rnorm(n, 0, 1)
mu0 <- mean(chi)
sigma0 <- sqrt(var(chi))

X <- c(xi, chi) #Realizacion muestral del marcador
D <- c(rep(1, n), rep(0, n))

groc.obj <- gROC(X,D)
bandas <- ROCbands(groc.obj, plot.bands = TRUE, plot.bar = TRUE, conf.level = 1-alpha)
U <- bandas$U #limite superior de las bandas de confianza
L <- bandas$L #limite inferior de las bandas de confianza
R <- bandas$ROC.t #curva ROC numérica
N <- length(U)

#calculamos
t <- seq(0,1,1/(N-1))
Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))

plot(bandas, col.inside = 'darkseagreen1', col = 'darkseagreen1',
     col.frontier = 'darkolivegreen', main = '')
lines(t, pnorm(a/b + qnorm(t)/b), lwd = 2, lty = 2) #curva teorica
lines(t, pnorm(Xt + qnorm(1-alpha/2)*sqrt(var)), col = 'blue', lwd = 2) #intervalos de confianza (superior)
lines(t, pnorm(Xt - qnorm(1-alpha/2)*sqrt(var)), col = 'blue', lwd = 2) #intervalos de confianza (inferior)
legend('bottomright', legend = c('Curva teórica', 'Intervalos de confianza', 'Bandas de confianza (PSN)'), col = c('black', 'blue', 'darkolivegreen'), 
       lty = c(2, 1, 1), lwd = 2, bty = "n", inset = 0.01)








# gROC vs ROC usual ----
library(movieROC)
library(tidyverse)
library(latex2exp)

set.seed(123)
n <- 1e4
xi <- rnorm(n, 3, 2)
chi <- rnorm(n, 2, 1)
X <- c(xi, chi) #Realizacion muestral del marcador
D <- c(rep(1,n),rep(0,n))

#curva ROC usual a derechas y el punto con mayor Se+Sp
roc <- gROC(X,D)
R1 <- roc$roc
t1 <- roc$t
c <- roc$c
pos1 <- which.max(1-t1+R1) #posicion del punto en el que es maximo el indice de Youden
c_opt <- c[pos1]
R_opt1 <- R1[pos1]
t_opt1 <- t1[pos1]
J1 <- 1-t_opt1 + R_opt1 #indice de Youden
AUC1 <- roc$auc

#curva gROC el punto con mayor Se+Sp
groc <- gROC_param(X, D, side = 'both')
R2 <- groc$roc
t2 <- groc$t
xl <- groc$xl
xu <- groc$xu
pos2 <- which.max(1-t2+R2) #posicion del punto en el que es maximo el indice de Youden
xl_opt <- xl[pos2]
xu_opt <- xu[pos2]
R_opt2 <- R2[pos2]
t_opt2 <- t2[pos2]
J2 <- 1-t_opt2 + R_opt2 #indice de Youden
AUC2 <- groc$auc

plot_densities(roc, main='Funciones de densidad', xlab = 'Marcador',
               cex.axis = 1.2, cex.lab = 1.2,
               h=2, col = c("dodgerblue4", "#D2691E"), cex.main = 1.5)
legend('topright', legend = c(TeX('$\\chi$'), TeX('$\\xi$')),
       col = c('dodgerblue4', '#D2691E'), lwd = 2, bty = 'n',
       inset = 0.01, cex = 1.2)

plot.regions(roc, plot.roc=TRUE, xlim=c(-2,6), legend=FALSE,
             main='Intervalos de clasificación', xlab = 'Marcador',
             FPR = t_opt1, cex.main = 2, cex.axis = 2, cex.lab = 2,
             plot.auc = TRUE)

plot.regions(groc, plot.roc=TRUE, xlim=c(-2,6), legend=FALSE, h=5,
             main='Intervalos de clasificación', xlab = 'Marcador',
             FPR = t_opt2, cex.main = 2, cex.axis = 2, cex.lab = 2,
             plot.auc = TRUE)





# Contraejemplo ----
library(movieROC)
plot_regions.groc <- function(x, FPR = 0.15, plot.roc = TRUE, plot.auc = FALSE, col = c('white','grey'), col.FPR = 'blue', lwd = 2, new.window = TRUE, type.plotroc = 's', xlim = NULL, mar = c(5,6,4,0.25),
                              cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.75, main = NULL, xlab = "", ylab = "False-Positive Rate", main.plotroc = "ROC curve", legend = TRUE, cex.legend = 1, ...){
  
  obj <- x
  side <- obj$side
  if(is.null(main)) main <- ifelse(obj$param, "Classification subsets [Parametric]", "Classification subsets [Non-Parametric]")
  
  if(is.null(xlim)){
    inf <- min(c(obj$controls, obj$cases)); sup <- max(c(obj$controls, obj$cases))
  }else{
    inf <- xlim[1]; sup <- xlim[2]
  }
  xlim <- c(inf,sup)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  if(new.window){
    par(oma=c(0.2,0.5,0.2,0.5))
    if(plot.roc) layout(rbind(c(1,1,1,2)))
  }
  if(plot.roc) par(mar=mar)
  
  colTrans <- col
  colTrans[1] <- rgb(red=col2rgb(col[1])[1], green=col2rgb(col[1])[2], blue=col2rgb(col[1])[3], alpha=0.25*255, maxColorValue=255)
  colTrans[2] <- rgb(red=col2rgb(col[2])[1], green=col2rgb(col[2])[2], blue=col2rgb(col[2])[3], alpha=0.25*255, maxColorValue=255)
  
  if(side=='right' || side=='left'){
    
    if(length(obj$t) > 151){
      index.t <- sapply(seq(0,1,length.out=151), function(fpr){which.min(abs(obj$t - fpr))})
      obj$c <- obj$c[index.t]
      obj$t <- obj$t[index.t]
      obj$roc <- obj$roc[index.t]
    }
    
    plot(obj$c, 1-obj$t, xlab = xlab, ylab = ylab, main = main, xlim = xlim, ylim = c(0,1), yaxt = 'n', xaxs = 'i', col = 'white', cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis)
    polygon(c(rep(inf,length(obj$t)), rev(obj$c)), c(1-obj$t, rev(1-obj$t)), col = ifelse(side=='right',colTrans[1], colTrans[2]), border = NA)
    polygon(c(obj$c, rev(rep(sup,length(obj$t)))), c(1-obj$t, rev(1-obj$t)), col = ifelse(side=='right',colTrans[2], colTrans[1]), border = NA)
    segments(rep(inf,length(obj$t)), 1-obj$t, obj$c, 1-obj$t, col = ifelse(side=='right', col[1], col[2]))
    segments(obj$c, 1-obj$t, rep(sup,length(obj$t)), 1-obj$t, col = ifelse(side=='right', col[2], col[1]))
    
    ticks.axis1 <- axis(1, cex.axis = cex.axis)
    space0 <- max(diff(ticks.axis1))
    axis(1, at = seq(min(ticks.axis1)-space0, max(ticks.axis1)+space0, space0/4), tcl = -0.4, labels = FALSE)
    axis(1, at = seq(min(ticks.axis1)-space0, max(ticks.axis1)+space0, space0/8), tcl = -0.3, labels = FALSE)
    
    axis(2, at = seq(1,0,-0.1), labels = F, tck = -0.02, cex.axis = cex.axis)
    axis(2, at = seq(0,1,0.5), labels = seq(1,0,-0.5), tck = -0.04, cex.axis = cex.axis)
    axis(4, at = c(0,1), labels = c("",""), tck = 0, cex.axis = cex.axis)
    
    if(!is.null(FPR)){
      for(i in 1:length(FPR)){
        info <- predict(obj, FPR=FPR[i])
        info$ClassSubsets <- ifelse(info$ClassSubsets == -Inf, inf, ifelse(info$ClassSubsets == Inf, sup, info$ClassSubsets))
        arrows(info$ClassSubsets[1], info$Specificity, info$ClassSubsets[2], info$Specificity, col = col.FPR, angle = 75, code = 3, length = 0.03)
      }
    }
    
    if(legend) legend('topleft', obj$levels, pch = 22, col='black', pt.bg = col, title = "Classification:", inset = 0.01, bty = 'n', cex = cex.legend)
  }
  if(side=='both' || side=='both2'){
    
    if(length(obj$t) > 151){
      index.t <- sapply(seq(0,1,length.out=151), function(fpr){which.min(abs(obj$t - fpr))})
      obj$xl <- obj$xl[index.t]
      obj$xu <- obj$xu[index.t]
      obj$t <- obj$t[index.t]
      obj$roc <- obj$roc[index.t]
    }
    
    plot(obj$xl, 1-obj$t, xlab = xlab, ylab = ylab, main = main, xlim = xlim, ylim = c(0,1), yaxt = 'n', xaxs = 'i', col = 'white', cex.lab = cex.lab, cex.main = cex.main, cex.axis = cex.axis)
    polygon(c(obj$xl, rev(obj$xu)), c(1-obj$t, rev(1-obj$t)), col=ifelse(side=='both2',colTrans[2], colTrans[1]), border=NA)
    polygon(c(rep(inf,length(obj$t)), rev(obj$xl)), c(1-obj$t, rev(1-obj$t)), col=ifelse(side=='both2',colTrans[1], colTrans[2]), border=NA)
    polygon(c(obj$xu, rev(rep(sup,length(obj$t)))), c(1-obj$t, rev(1-obj$t)), col=ifelse(side=='both2',colTrans[1], colTrans[2]), border=NA)
    segments(obj$xl, 1-obj$t, obj$xu, 1-obj$t, col=ifelse(side=='both2',col[2], col[1]))
    segments(rep(inf,length(obj$t)), 1-obj$t, obj$xl, 1-obj$t, col=ifelse(side=='both2',col[1], col[2]))
    segments(obj$xu, 1-obj$t, rep(sup,length(obj$t)), 1-obj$t, col=ifelse(side=='both2',col[1], col[2]))
    
    ticks.axis1 <- axis(1, cex.axis = cex.axis)
    space0 <- max(diff(ticks.axis1))
    axis(1, at =seq(min(ticks.axis1)-space0, max(ticks.axis1)+space0, space0/4), tcl = -0.4, labels = FALSE)
    axis(1, at =seq(min(ticks.axis1)-space0, max(ticks.axis1)+space0, space0/8), tcl = -0.3, labels = FALSE)
    
    axis(2, at=seq(1,0,-0.1), labels=F, tck=-0.02, cex.axis = cex.axis)
    axis(2, at=seq(0,1,0.5), labels=seq(1,0,-0.5), tck=-0.04, cex.axis = cex.axis)
    axis(4, at=c(0,1), labels=c("",""), tck=0, cex.axis = cex.axis)
    
    if(!is.null(FPR)){
      for(i in 1:length(FPR)){
        info <- predict(obj, FPR=FPR[i])
        info$ClassSubsets <- ifelse(info$ClassSubsets == -Inf, inf, ifelse(info$ClassSubsets == Inf, sup, info$ClassSubsets))
        if(length(info$ClassSubsets) == 2){info$ClassSubsets <- matrix(info$ClassSubsets, nrow = 1)}
        arrows(info$ClassSubsets[,1], info$Specificity, info$ClassSubsets[,2], info$Specificity, col = col.FPR, angle = 75, code = 3, length = 0.03)
      }
    }
    
    if(legend) legend('topleft', obj$levels, pch = 22, col='black', pt.bg = col, title = "Classification:", inset = 0.01, bty = 'n', cex = cex.legend)
    
  }
  
  if(plot.roc){
    par(mar=c(par("mar")[1],1,par("mar")[3],5))
    plot(c(0,obj$roc,1),c(1,1-obj$t,0), type = type.plotroc,main=" ",xlab="True-Positive Rate", yaxt="n", cex.lab=cex.lab, cex.main=cex.main, cex.axis=cex.axis, xaxt="n", xlim=c(0,1), ylim=c(0,1), ylab=" ", lwd=lwd)
    lines(c(0,1),c(1,0),lty=2)
    text(x=1.15, y=0.5, labels = main.plotroc, srt=-90, xpd=TRUE,font=2, cex = cex.lab)
    axis(1,at=c(0,0.5,1),labels=c(0,0.5,1), cex.axis = cex.axis)
    axis(1,xaxp=c(0,1,40),tcl=-0.2,tcl=-0.2,labels=F, cex.axis = cex.axis)
    axis(3,at=0.5,labels=substitute(paste(bold("Curva gROC"))),tcl=0, cex.axis = cex.axis)
    if(!is.null(FPR)){
      for(i in 1:length(FPR)){
        index.FPR <- which.min(abs(obj$t - FPR[i]))
        lines(x = c(0,obj$roc[index.FPR]), y = c(1-obj$t[index.FPR],1-obj$t[index.FPR]), lty = 3, col = col.FPR)
        lines(x = c(obj$roc[index.FPR],obj$roc[index.FPR]), y = c(0,1-obj$t[index.FPR]), lty = 3, col = col.FPR)
        points(obj$roc[index.FPR], 1-obj$t[index.FPR], pch = 16, col = col.FPR, cex = 1.5)
      }
    }
    if(plot.auc) legend('bottomleft', paste("AUC=",round(obj$auc,3),sep=''), cex = 0.75*cex.axis, bty='n', inset=0.01)
  }
}


#definimos la funcion de densidad de la distribucion beta bimodal
Z <- function(a, b, rho, delta){
  1 + rho - 2*delta*a/(a+b) + delta^2*a*(a+1)/((a+b)*(a+b+1))
}
f <- function(x, a, b, rho, delta){
  (rho+(1-delta*x)^2)/(Z(a, b, rho, delta)*beta(a, b))*x^(a-1)*(1-x)^(b-1)
}

#incomplete beta function
ibeta <- function(x,a,b){
  pbeta(x,a,b)*beta(a,b)
}


a <- 2.05
b <- 2
rho <- .05
delta <- 2
x <- seq(0,1, by = 0.001)
#plot(x,f(x, a, b, rho, delta), lwd = 1, col = "red", main = "Density")
#mean(x*f(x, a, b, rho, delta))

#funcion de distribucion de una beta bimodal
Fdist <- function(x, a, b, rho, delta){
  1/Z(a, b, rho, delta)*((1 + rho)*ibeta(x, a, b)/beta(a, b) - 2*delta*ibeta(x, a+1, b)/beta(a, b) + delta^2*ibeta(x, a+2, b)/beta(a,b))
}
Func <- function(x) Fdist(x, a, b, rho, delta)
#plot(Func)

#inversa de la funcion de distribucion
inv <- function (f, lower = 0.001, upper = 0.999) {
  function (y) uniroot((function (x) {f(x) - y}), lower = lower, upper = upper)[1]
}
inversa <- inv(function (x) Fdist(x, a, b, rho, delta))
Q <- Vectorize(function(y){inversa(y)})

#Utilizamos Q para generar los datos:
m <- 1e4
n <- 5e3
set.seed(1234)
u <- runif(m)
chi <- Q(u)
chi <- unlist(chi, use.names=FALSE)
xi <- runif(n,0,1)
X <- c(xi,chi)
D <- c(rep(1,n),rep(0,m))
groc <- gROC(X, D, side = 'both')
groc$auc
t <- roc$t
ind1 <- which(t == 0.3)
groc$roc[ind1]
c(groc$xl[ind1],groc$xu[ind1])
ind2 <- which(t == 0.5)
groc$roc[ind2]
c(groc$xl[ind2],groc$xu[ind2])

par(mar = c(5.1, 5, 4.1, 2.1))
plot(x, f(x, a, b, rho, delta), type = 'l', lwd = 3, col = "dodgerblue4", 
     main = "Funciones de densidad", xlab = "Marcador", ylab = "f(x)",
     ylim = c(-1.65,1.65), xlim = c(0,1.14), yaxt = "n", frame = FALSE,
     cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
axis(2, at = seq(-1.5,1.5,.5), labels = abs(seq(-1.5,1.5,.5)), cex.axis = 1.5)
lines(x, -dunif(x,0,1), lwd = 3, col = "#D2691E")
lines(x, rep(0,length(x)), lwd = 1.5, lty = 2, col = 'grey')
legend("topright", legend = c(expression(chi), expression(xi)), col = c("dodgerblue4", "#D2691E"),
       lwd = 2, bty = "n", inset = .01, cex = 1.5)


# plot_densities(roc, main = 'Funciones de densidad', xlab = 'Marcador',
#                cex.axis = 1, cex.lab = 1,
#                h = 1, col = c("dodgerblue4", "#D2691E"), cex.main = 1.5)
#pdf("Contraejemplo_Clasif.pdf")
plot_regions.groc(roc, legend = FALSE, main = 'Intervalos de clasificación', plot.auc = TRUE,
                  FPR = c(0.3, 0.5), xlab = 'Marcador', plot.roc = TRUE, main.plotroc = 'Curva gROC',
                  cex.main = 2, cex.axis = 2, cex.lab = 2)



#Simulaciones ----


## Simulación de la cobertura de las bandas ----
# La idea es simular N bandas de confianza a partir de los datos y estudiar
#la cantidad de ellas que contienen a la curva ROC real.

library(pbapply)
library(nsROC)
library(latex2exp)
N <- 1e3 #número de escenarios a generar en la simulación

###Densidades y curvas ROC teoricas----
#Escenario 1
a1 <- 1
b1 <- 2
AUCteo1 <- pnorm(a1/(1+b1^2))
AUCteo1
x <- seq(-6,10, by = 0.01)

layout(mat = matrix(c(1, 2), nrow = 1, ncol = 2),
       heights = c(2, 2), # Heights of the two rows
       widths = c(4, 2)) # Widths of the two columns

plot(x, dnorm(x, 0, 1), type = 'l', lwd = 3, col = "dodgerblue4", 
     main = "Funciones de densidad", xlab = "Marcador", ylab = "f(x)",
     xlim = c(-6,10), ylim = c(-0.3,0.5), yaxt = "n", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5)
axis(2, at = seq(-0.3,0.5,.15), labels = abs(seq(-0.3,0.5,.15)), cex.axis = 1.5)
abline(h = 0, lty = 2, lwd = 1.5, col = "gray")
lines(x, -dnorm(x, a1, b1), lwd = 2, col = "#D2691E")
legend("topright", legend = c(expression(chi), expression(xi)), cex = 1.5,
       col = c("dodgerblue4", "#D2691E"), lwd = 2, bty = "n", inset = .01)

t <- seq(0,1,1/1e4)
R_teo <- pnorm(a1/b1 + qnorm(t)/b1)
plot(t, R_teo, type = 'l', lwd = 3, main = "Curva ROC usual", xlab = "False-Positive Rate",
     ylim = c(0,1), ylab = "True-Positive Rate", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5)
lines(t, t, col = 'grey', lwd = 2, lty = 2)
text(x = 0.6, y = 0.1, 'AUC=0.579', cex = 1.5)


#Escenario 2
a2 <- 1.5
b2 <- 1
AUCteo2 <- pnorm(a2/(1+b2^2))
AUCteo2
x <- seq(-4,6, by = 0.01)

layout(mat = matrix(c(1, 2), nrow = 1, ncol = 2),
       heights = c(2, 2), # Heights of the two rows
       widths = c(4, 2)) # Widths of the two columns

plot(x, dnorm(x, 0, 1), type = 'l', lwd = 3, col = "dodgerblue4", 
     xlab = "Marcador", ylab = "f(x)",
     xlim = c(-4,6), ylim = c(-0.5,0.5), yaxt = "n", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5)
axis(2, at = seq(-0.5,0.5,.25), labels = abs(seq(-0.5,0.5,.25)), cex.axis = 1.5)
abline(h = 0, lty = 2, lwd = 2, col = "gray")
lines(x, -dnorm(x, a2, b2), lwd = 2, col = "#D2691E")
legend("topright", legend = c(expression(chi), expression(xi)), cex = 1.5,
       col = c("dodgerblue4", "#D2691E"), lwd = 2, bty = "n", inset = .01)

t <- seq(0,1,1/1e4)
R_teo <- pnorm(a2/b2 + qnorm(t)/b2)
plot(t, R_teo, type = 'l', lwd = 3, xlab = "False-Positive Rate",
     ylim = c(0,1), ylab = "True-Positive Rate", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5)
lines(t, t, col = 'grey', lwd = 2, lty = 2)
text(x = 0.6, y = 0.1, 'AUC=0.773', cex = 1.5)


#Escenario 3
a3 <- 2.5
b3 <- 1.25
AUCteo3 <- pnorm(a3/(1+b3^2))
AUCteo3
x <- seq(-4,8, by = 0.01)

layout(mat = matrix(c(1, 2), nrow = 1, ncol = 2),
       heights = c(2, 2), # Heights of the two rows
       widths = c(4, 2)) # Widths of the two columns

plot(x, dnorm(x, 0, 1), type = 'l', lwd = 3, col = "dodgerblue4", 
     xlab = "Marcador", ylab = "f(x)",
     xlim = c(-4,8), ylim = c(-0.5,0.5), yaxt = "n", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5)
axis(2, at = seq(-0.5,0.5,.25), labels = abs(seq(-0.5,0.5,.25)), cex.axis = 1.5)
abline(h = 0, lty = 2, lwd = 2, col = "gray")
lines(x, -dnorm(x, a3, b3), lwd = 2, col = "#D2691E")
legend("topright", legend = c(expression(chi), expression(xi)), cex = 1.5,
       col = c("dodgerblue4", "#D2691E"), lwd = 2, bty = "n", inset = .01)

t <- seq(0,1,1/1e4)
R_teo <- pnorm(a3/b3 + qnorm(t)/b3)
plot(t, R_teo, type = 'l', lwd = 3, xlab = "False-Positive Rate",
     ylim = c(0,1), ylab = "True-Positive Rate", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5)
lines(t, t, col = 'grey', lwd = 2, lty = 2)
text(x = 0.6, y = 0.1, 'AUC=0.835', cex = 1.5)




###Escenario 1 ----
a <- 1
b <- 2
alpha = 0.1 #nivel de confianza 1-alpha

n <- 50 #tamaño muestral de poblacion positiva y negativa
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)
counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting[3:4,], 1, sd)
n50 <- c(medias[1:3], sigma[1], medias[4], sigma[2])
# sprintf('La cobertura de las bandas de confianza (PSN) y los intervalos de confianza (Demidenko) es %f y %f, respectivamente.',
#       medias[1], medias[2])
# sprintf('El area de cobertura de las bandas de confianza (PSN) y los intervalos de confianza (Demidenko) es %f+-%f y %f+-%f, respectivamente.',
#       medias[3], sigma[1], medias[4], sigma[2])


n <- 100
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)

counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting,1,mean)
sigma <- apply(counting[3:4,], 1, sd)
n100 <- c(medias[1:3], sigma[1], medias[4], sigma[2])


n <- 200 #tamaño muestral de poblacion positiva y negativa
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)

counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting,1,mean)
sigma <- apply(counting[3:4,], 1, sd)
n200 <- c(medias[1:3], sigma[1], medias[4], sigma[2])


n <- 500 #tamaño muestral de poblacion positiva y negativa
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)

counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting,1,mean)
sigma <- apply(counting[3:4,], 1, sd)
n500 <- c(medias[1:3], sigma[1], medias[4], sigma[2])

escenario1 <- rbind(n50, n100, n200, n500)
escenario1


###Escenario 2 ----
a <- 1.5
b <- 1
AUCteo2 <- pnorm(a/(1+b^2))
AUCteo2
alpha = 0.1 #nivel de confianza 1-alpha

n <- 50 #tamaño muestral de poblacion positiva y negativa
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)
counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting[3:4,], 1, sd)
n50 <- c(medias[1:3], sigma[1], medias[4], sigma[2])
# sprintf('La cobertura de las bandas de confianza (PSN) y los intervalos de confianza (Demidenko) es %f y %f, respectivamente.',
#       medias[1], medias[2])
# sprintf('El area de cobertura de las bandas de confianza (PSN) y los intervalos de confianza (Demidenko) es %f+-%f y %f+-%f, respectivamente.',
#       medias[3], sigma[1], medias[4], sigma[2])


n <- 100
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)

counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting,1,mean)
sigma <- apply(counting[3:4,], 1, sd)
n100 <- c(medias[1:3], sigma[1], medias[4], sigma[2])


n <- 200 #tamaño muestral de poblacion positiva y negativa
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)

counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting,1,mean)
sigma <- apply(counting[3:4,], 1, sd)
n200 <- c(medias[1:3], sigma[1], medias[4], sigma[2])


n <- 500 #tamaño muestral de poblacion positiva y negativa
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)

counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting,1,mean)
sigma <- apply(counting[3:4,], 1, sd)
n500 <- c(medias[1:3], sigma[1], medias[4], sigma[2])

escenario2 <- rbind(n50, n100, n200, n500)
escenario2



###Escenario 3 ----
a <- 2.5
b <- 1.25
AUCteo2 <- pnorm(a/(1+b^2))
AUCteo2
alpha = 0.1 #nivel de confianza 1-alpha

n <- 50 #tamaño muestral de poblacion positiva y negativa
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)
counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting[3:4,], 1, sd)
n50 <- c(medias[1:3], sigma[1], medias[4], sigma[2])
# sprintf('La cobertura de las bandas de confianza (PSN) y los intervalos de confianza (Demidenko) es %f y %f, respectivamente.',
#       medias[1], medias[2])
# sprintf('El area de cobertura de las bandas de confianza (PSN) y los intervalos de confianza (Demidenko) es %f+-%f y %f+-%f, respectivamente.',
#       medias[3], sigma[1], medias[4], sigma[2])


n <- 100
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)

counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting,1,mean)
sigma <- apply(counting[3:4,], 1, sd)
n100 <- c(medias[1:3], sigma[1], medias[4], sigma[2])


n <- 200 #tamaño muestral de poblacion positiva y negativa
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)

counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting,1,mean)
sigma <- apply(counting[3:4,], 1, sd)
n200 <- c(medias[1:3], sigma[1], medias[4], sigma[2])


n <- 500 #tamaño muestral de poblacion positiva y negativa
#calculamos la curva ROC teórica
t <- seq(1/n,1-1/n,1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)

counting <- pbsapply(1:N, function(i) {
  set.seed(i)
  
  xi <- rnorm(n, a, b)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  
  groc.obj <- gROC(X,D) #curva ROC a derechas
  bandas <- ROCbands(groc.obj, plot.bands = FALSE, plot.bar = FALSE,
                     conf.level = 1-alpha)
  U <- bandas$U #banda de confianza superior
  Ufun <- approxfun(groc.obj$t,U)
  nU <- sum(Ufun(t)<R_teo) #numero de puntos en los que la curva teorica es mayor que la banda superior
  L <- bandas$L #banda de confianza inferior
  Lfun <- approxfun(groc.obj$t,L)
  nL <- sum(Lfun(t)>R_teo) #numero de puntos en los que la curva teorica es menor que la banda inferior
  count_band <-(nU == 0 && nL ==0)
  
  area1 <- mean(Ufun(t)-Lfun(t)) #calculo numerico del area entre las bandas
  
  #ahora calculamos las bandas union de intervalos de confianza
  Xt <- (mu1-mu0+sigma0*qnorm(t))/sigma1
  var <- 1/sigma1^2*(sigma0^2/n+sigma1^2/n) + (qnorm(t)^2*sigma0^2)/(2*sigma1^2*(n-1)) + (mu1-mu0+sigma0*qnorm(t))^2/(2*sigma1^2*(n-1))
  sup <- pnorm(Xt + qnorm(1-alpha/2)*sqrt(var))
  nsup <- sum(sup[2:length(sup)] < R_teo[2:length(R_teo)])
  inf <- pnorm(Xt - qnorm(1-alpha/2)*sqrt(var))
  ninf <- sum(inf[1:(length(inf)-1)] > R_teo[1:(length(R_teo)-1)])
  count_interval <- (nsup == 0 && ninf ==0)
  
  area2 <- mean(sup-inf) #calculo numerico del area entre las bandas
  
  c(count_band,count_interval,area1,area2)
})
medias <- apply(counting,1,mean)
sigma <- apply(counting[3:4,], 1, sd)
n500 <- c(medias[1:3], sigma[1], medias[4], sigma[2])

escenario3 <- rbind(n50, n100, n200, n500)
escenario3




##Robustez del estimador binormal para la curva ROC usual y gROC ----

###Graficas de ejemplo ----
a <- 0.6
b <- 2.2
AUCteo1 <- pnorm(a/sqrt(1+b^2))
AUCteo1

n <- 100
#calculamos la curva ROC usual teórica
t <- seq(1/1e6, 1, 1/1e4)
R_teo <- pnorm(a/b + qnorm(t)/b)

#calculamos la curva gROC teórica
gt <- function(g,t) {
  g - 1/t*pnorm(2*a/(1-b^2) + qnorm((1-g)*t))
}
gamma_t <- pbsapply(t, function(t) {
  uniroot(function(g) gt(g,t) , lower = max(0, 1-1/t), upper = 1)$root
})
Rg_teo <- pnorm(a/b + 1/b*qnorm((1-gamma_t)*t)) + 1 - pnorm(a/b + 1/b*qnorm(1-gamma_t*t))
plot(t,Rg_teo)


#simulamos los datos
set.seed(92)
xi <- rnorm(n, a, b)
mu1 <- mean(xi)
sigma1 <- sqrt(var(xi))
chi <- rnorm(n, 0, 1)
mu0 <- mean(chi)
sigma0 <- sqrt(var(chi))
ahat <- (mu1-mu0)/sigma0
bhat <- sigma1/sigma0

#estimacion de la curva ROC usual
R_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
sqrt(n)*mean(abs(R_teo-R_hat))*(1-2/n) #sqrt(n)*\int|R_teo-R_hat|dt
plot(t, R_teo, type = 'l', lwd = 2, main = 'Curvas ROC', xlab = 'False-Positive Rate',
     ylab = 'True-Positive Rate', frame = FALSE, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5,
     xlim = c(0,1), ylim = c(0,1))
lines(t, R_hat, lwd = 2, col = 'tomato4')
lines(t, t, lwd = 2, lty = 2, col = 'grey')
polygon(x = c(t, rev(t)), y = c(R_teo, rev(R_hat)),
        col = adjustcolor("tomato3", alpha = .3))
text(x = 0.6, y = 0.1, 'AUC=0.541', cex = 1.5)
legend("topleft", legend = c('Curva teórica', 'Estimación paramétrica'), cex = 1.5,
       col = c("black", "tomato4"), lwd = 2, bty = "n", inset = .01)

#estimacion de la curva gROC
gt_hat <- function(g,t) {
  g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))
}
gamma_t <- pbsapply(t, function(t) {
  uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root
})
Rg_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))

plot(t, Rg_teo, type = 'l', lwd = 2, main = 'Curvas gROC', xlab = 'False-Positive Rate',
     ylab = 'True-Positive Rate', frame = FALSE, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5,
     xlim = c(0,1), ylim = c(0,1))
lines(t, Rg_hat, lwd = 2, col = 'royalblue4')
lines(t, t, lwd = 2, lty = 2, col = 'grey')
polygon(x = c(t, rev(t)), y = c(Rg_teo, rev(Rg_hat)),
        col = adjustcolor("royalblue3", alpha = .3))
text(x = 0.1, y = 0.9, 'AUC=', cex = 1.5)
legend("bottomright", legend = c('Curva teórica', 'Estimación paramétrica'), cex = 1.5,
       col = c("black", "royalblue4"), lwd = 2, bty = "n", inset = .01)




###Densidades y curvas ROC teoricas----
####Escenario 1----
a1 <- 0.6
b1 <- 2.2
AUCteo1 <- pnorm(a1/sqrt(1+b1^2))

#curva ROC usual teórica
t <- seq(1/1e6, 1, 1/1e4)
R_teo1 <- pnorm(a1/b1 + qnorm(t)/b1)

#curva gROC teórica
gt <- function(g,t) {
  g - 1/t*pnorm(2*a1/(1-b1^2) + qnorm((1-g)*t))
}
gamma_t <- sapply(t, function(t) {
  uniroot(function(g) gt(g,t) , lower = max(0, 1-1/t), upper = 1)$root
})
Rg_teo1 <- pnorm(a1/b1 + 1/b1*qnorm((1-gamma_t)*t)) + 1 - pnorm(a1/b1 + 1/b1*qnorm(1-gamma_t*t))
gAUCteo1 <- mean(Rg_teo1)

c(AUCteo1, gAUCteo1)

plot(t, R_teo1, type = 'l')
lines(t, Rg_teo1)

####Escenario 2 ----
a2 <- 2
b2 <- 2.5
AUCteo2 <- pnorm(a2/sqrt(1+b2^2))

#curva ROC usual teórica
R_teo2 <- pnorm(a2/b2 + qnorm(t)/b2)

#curva gROC teórica
gt <- function(g,t) {
  g - 1/t*pnorm(2*a2/(1-b2^2) + qnorm((1-g)*t))
}
gamma_t <- sapply(t, function(t) {
  uniroot(function(g) gt(g,t) , lower = max(0, 1-1/t), upper = 1)$root
})
Rg_teo2 <- pnorm(a2/b2 + 1/b2*qnorm((1-gamma_t)*t)) + 1 - pnorm(a2/b2 + 1/b2*qnorm(1-gamma_t*t))
gAUCteo2 <- mean(Rg_teo2)

c(AUCteo2, gAUCteo2)

plot(t, R_teo2, type = 'l')
lines(t, Rg_teo2)

####Escenario 3----
a3 <- 4
b3 <- 2.9
AUCteo3 <- pnorm(a3/sqrt(1+b3^2))

#curva ROC usual teórica
R_teo3 <- pnorm(a3/b3 + qnorm(t)/b3)

#curva gROC teórica
gt <- function(g,t) {
  g - 1/t*pnorm(2*a3/(1-b3^2) + qnorm((1-g)*t))
}
gamma_t <- sapply(t, function(t) {
  uniroot(function(g) gt(g,t) , lower = max(0, 1-1/t), upper = 1)$root
})
Rg_teo3 <- pnorm(a3/b3 + 1/b3*qnorm((1-gamma_t)*t)) + 1 - pnorm(a3/b3 + 1/b3*qnorm(1-gamma_t*t))
gAUCteo3 <- mean(Rg_teo3)

c(AUCteo3, gAUCteo3)

plot(t, R_teo3, type = 'l')
lines(t, Rg_teo3)

####Escenario 4: t de student----
v <- 5 #grados de libertad
t <- seq(1/1e6, 1, 1/1e4)

R_r <- function(t) {1-pt((qnorm(1-t)-1)/2.2, v)}
R_teo4 <- R_r(t) #curva ROC usual teorica
AUCteo4 <- mean(R_teo4)

#curva gROC teorica
Rg_teo4 <- pbsapply(t, function(t) {
  gamma <- seq(max(0,1-1/t), min(1,1/t), 1e-4)
  max(1 - R_r(1-gamma*t) + R_r((1-gamma)*t))
})
gAUCteo4 <- mean(Rg_teo4)

c(AUCteo4, gAUCteo4)

plot(t, R_teo4, type = 'l', xlim = c(0,1), ylim = c(0,1))
lines(t, Rg_teo4)
lines(t, t, col = 'grey', lty = 2)


####Escenario 5: gamma ----
k <- 3
theta <- 2
t <- seq(1/1e6, 1, 1/1e4)

R_r <- function(t) {1-pgamma(qnorm(1-t)+3.5, shape=k, scale=theta)}
R_teo5 <- R_r(t) #curva ROC usual teorica
AUCteo5 <- mean(R_teo5)

#curva gROC teorica
Rg_teo5 <- pbsapply(t, function(t) {
  gamma <- seq(max(0,1-1/t), min(1,1/t), 1e-4)
  max(1 - R_r(1-gamma*t) + R_r((1-gamma)*t))
})
gAUCteo5 <- mean(Rg_teo5)

plot(t, R_teo5, type = 'l', xlim = c(0,1), ylim = c(0,1))
lines(t, Rg_teo5)
lines(t, t, col = 'grey', lty = 2)


####Escenario 6: beta ----
alpha <- 2
beta <- 2
t <- seq(1/1e6, 1, 1/1e4)

R_r <- function(t) {1-pbeta((qnorm(1-t)+3.7)/10, alpha, beta)}
R_teo6 <- R_r(t) #curva ROC usual teorica
AUCteo6 <- mean(R_teo6)

#curva gROC teorica
Rg_teo6 <- pbsapply(t, function(t) {
  gamma <- seq(max(0,1-1/t), min(1,1/t), 1e-4)
  max(1 - R_r(1-gamma*t) + R_r((1-gamma)*t))
})
gAUCteo6 <- mean(Rg_teo6)

c(AUCteo6, gAUCteo6)

plot(t, R_teo6, type = 'l', xlim = c(0,1), ylim = c(0,1))
lines(t, Rg_teo6)
lines(t, t, col = 'grey', lty = 2)






####Densidades de los 3 primeros escenarios----
x <- seq(-6,12, by = 0.01)

layout(mat = matrix(c(1, 2, 3), nrow = 1, ncol = 3),
       heights = c(2, 2, 2), # Heights of the two rows
       widths = c(5, 2, 2)) # Widths of the two columns

plot(x, dnorm(x, 0, 1), type = 'l', lwd = 3, col = "dodgerblue4", 
     main = "Funciones de densidad", xlab = "Marcador", ylab = "f(x)",
     xlim = c(-6,12), ylim = c(-0.2,0.5), yaxt = "n", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5)
axis(2, at = seq(-0.3,0.5,.15), labels = abs(seq(-0.3,0.5,.15)), cex.axis = 1.5)
lines(x, -dnorm(x, a1, b1), lwd = 2, col = "#D2691E")
lines(x, -dnorm(x, a2, b2), lwd = 2, col = "darkolivegreen")
lines(x, -dnorm(x, a3, b3), lwd = 2, col = "firebrick4")
lines(x, rep(0,length(x)), lwd = 1.5, lty = 2, col = 'grey')
legend("topright", legend = c(expression(chi), expression(xi[1]), expression(xi[2]), expression(xi[3])),
       cex = 1.5, col = c("dodgerblue4", "#D2691E", 'darkolivegreen', 'firebrick4'), lwd = 2, bty = "n", inset = .01)

plot(t, R_teo1, type = 'l', lwd = 3, main = "Curva ROC usual", xlab = "False-Positive Rate",
     ylim = c(0,1), ylab = "True-Positive Rate", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5, col = '#D2691E')
lines(t, R_teo2, col = 'darkolivegreen', lwd = 2, lty = 1)
lines(t, R_teo3, col = 'firebrick4', lwd = 2, lty = 1)
lines(t, t, col = 'grey', lwd = 2, lty = 2)
legend("bottomright", legend = c('Escenario 1', 'Escenario 2', 'Escenario 3'),
       cex = 1.2, col = c("#D2691E", 'darkolivegreen', 'firebrick4'), lwd = 2, bty = "n", inset = .01)

plot(t, Rg_teo1, type = 'l', lwd = 3, main = "Curvas gROC", xlab = "False-Positive Rate",
     ylim = c(0,1), ylab = "True-Positive Rate", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5, col = '#D2691E')
lines(t, Rg_teo2, col = 'darkolivegreen', lwd = 2, lty = 1)
lines(t, Rg_teo3, col = 'firebrick4', lwd = 2, lty = 1)
lines(t, t, col = 'grey', lwd = 2, lty = 2)
legend("bottomright", legend = c('Escenario 1', 'Escenario 2', 'Escenario 3'),
       cex = 1.2, col = c("#D2691E", 'darkolivegreen', 'firebrick4'), lwd = 2, bty = "n", inset = .01)



####Densidades de los 3 ultimos escenarios----
x <- seq(-6,12, by = 0.01)

layout(mat = matrix(c(1, 2, 3), nrow = 1, ncol = 3),
       heights = c(2, 2, 2), # Heights of the two rows
       widths = c(5, 2, 2)) # Widths of the two columns

plot(x, dnorm(x, 0, 1), type = 'l', lwd = 3, col = "dodgerblue4", 
     main = "Funciones de densidad", xlab = "Marcador", ylab = "f(x)",
     xlim = c(-6,12), ylim = c(-0.2,0.5), yaxt = "n", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5)
axis(2, at = seq(-0.2,0.5,.1), labels = abs(seq(-0.2,0.5,.1)), cex.axis = 1.5)
lines(x, -dt((x-1)/2.2, v)/2.2, lwd = 2, col = "#D2691E")
lines(x, -dgamma(x+3.5, shape = k, scale = theta), lwd = 2, col = "darkolivegreen")
lines(x, -dbeta((x+3.7)/10, 2, 2)/10, lwd = 2, col = "firebrick4")
lines(x, rep(0,length(x)), lwd = 1.5, lty = 2, col = 'grey')
legend("topright", legend = c(expression(chi), expression(xi[4]), expression(xi[5]), expression(xi[6])),
       cex = 1.5, col = c("dodgerblue4", "#D2691E", 'darkolivegreen', 'firebrick4'), lwd = 2, bty = "n", inset = .01)

plot(t, R_teo4, type = 'l', lwd = 3, main = "Curva ROC usual", xlab = "False-Positive Rate",
     ylim = c(0,1), ylab = "True-Positive Rate", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5, col = '#D2691E')
lines(t, R_teo5, col = 'darkolivegreen', lwd = 2, lty = 1)
lines(t, R_teo6, col = 'firebrick4', lwd = 2, lty = 1)
lines(t, t, col = 'grey', lwd = 2, lty = 2)
legend("bottomright", legend = c('Escenario 4', 'Escenario 5', 'Escenario 6'),
       cex = 1.2, col = c("#D2691E", 'darkolivegreen', 'firebrick4'), lwd = 2, bty = "n", inset = .01)

plot(t, Rg_teo4, type = 'l', lwd = 3, main = "Curvas gROC", xlab = "False-Positive Rate",
     ylim = c(0,1), ylab = "True-Positive Rate", frame = FALSE, cex.main = 1.5,
     cex.axis = 1.5, cex.lab = 1.5, col = '#D2691E')
lines(t, Rg_teo5, col = 'darkolivegreen', lwd = 2, lty = 1)
lines(t, Rg_teo6, col = 'firebrick4', lwd = 2, lty = 1)
lines(t, t, col = 'grey', lwd = 2, lty = 2)
legend("bottomright", legend = c('Escenario 4', 'Escenario 5', 'Escenario 6'),
       cex = 1.2, col = c("#D2691E", 'darkolivegreen', 'firebrick4'), lwd = 2, bty = "n", inset = .01)




### Escenario 1 ----
N <- 1e3
a1 <- 0.6
b1 <- 2.2
n <- 50
t <- seq(1/1e6, 1, 1/1e4)

#curva ROC teorica
R_teo <- pnorm(a1/b1 + qnorm(t)/b1)

#curva gROC teorica
gt <- function(g,t) {
  g - 1/t*pnorm(2*a1/(1-b1^2) + qnorm((1-g)*t))}
gamma_t <- sapply(t, function(t) {
  uniroot(function(g) gt(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
Rg_teo <- pnorm(a1/b1 + 1/b1*qnorm((1-gamma_t)*t)) + 1 - pnorm(a1/b1 + 1/b1*qnorm(1-gamma_t*t))

counting <- pbsapply(1:N, function(i) {
  set.seed(-i+23)
  
  #simulamos los datos
  xi <- rnorm(n, a1, b1)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n50 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 100
counting <- pbsapply(1:N, function(i) {
  set.seed(-i+23)
  
  #simulamos los datos
  xi <- rnorm(n, a1, b1)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n100 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
          medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 200
counting <- pbsapply(1:N, function(i) {
  set.seed(-i+23)
  
  #simulamos los datos
  xi <- rnorm(n, a1, b1)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n200 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
          medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

escenario1 <- rbind(n50, n100, n200)
colnames(escenario1) <- c('AUC.par_hat', '', 'Int.par', '', 'AUC_hat', '', 'Int', '', 'gAUC.par_hat', '', 'gInt.par', '', 'gAUC_hat', '', 'gInt', '')
escenario1



### Escenario 2 ----
N <- 1e3
a2 <- 2
b2 <- 2.5
n <- 50
t <- seq(1/1e6, 1, 1/1e4)

#curva ROC teorica
R_teo <- pnorm(a2/b2 + qnorm(t)/b2)

#curva gROC teorica
gt <- function(g,t) {
  g - 1/t*pnorm(2*a2/(1-b2^2) + qnorm((1-g)*t))}
gamma_t <- sapply(t, function(t) {
  uniroot(function(g) gt(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
Rg_teo <- pnorm(a2/b2 + 1/b2*qnorm((1-gamma_t)*t)) + 1 - pnorm(a2/b2 + 1/b2*qnorm(1-gamma_t*t))

counting <- pbsapply(1:N, function(i) {
  set.seed(-i+23)
  
  #simulamos los datos
  xi <- rnorm(n, a2, b2)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n50 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 100
counting <- pbsapply(1:N, function(i) {
  set.seed(-i+23)
  
  #simulamos los datos
  xi <- rnorm(n, a2, b2)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n100 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 200
counting <- pbsapply(1:N, function(i) {
  set.seed(-i+23)
  
  #simulamos los datos
  xi <- rnorm(n, a2, b2)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n200 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
          medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

escenario2 <- rbind(n50, n100, n200)
colnames(escenario2) <- c('AUC.par_hat', '', 'Int.par', '', 'AUC_hat', '', 'Int', '', 'gAUC.par_hat', '', 'gInt.par', '', 'gAUC_hat', '', 'gInt', '')
escenario2

### Escenario 3 ----
N <- 1e3
a3 <- 4
b3 <- 2.9
n <- 50
t <- seq(1/1e6, 1, 1/1e4)

#curva ROC teorica
R_teo <- pnorm(a2/b2 + qnorm(t)/b2)

#curva gROC teorica
gt <- function(g,t) {
  g - 1/t*pnorm(2*a2/(1-b2^2) + qnorm((1-g)*t))}
gamma_t <- sapply(t, function(t) {
  uniroot(function(g) gt(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
Rg_teo <- pnorm(a2/b2 + 1/b2*qnorm((1-gamma_t)*t)) + 1 - pnorm(a2/b2 + 1/b2*qnorm(1-gamma_t*t))

counting <- pbsapply(1:N, function(i) {
  set.seed(-i+23)
  
  #simulamos los datos
  xi <- rnorm(n, a2, b2)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n50 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 100
counting <- pbsapply(1:N, function(i) {
  set.seed(-i+23)
  
  #simulamos los datos
  xi <- rnorm(n, a2, b2)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n100 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
          medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 200
counting <- pbsapply(1:N, function(i) {
  set.seed(-i+23)
  
  #simulamos los datos
  xi <- rnorm(n, a2, b2)
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n200 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
          medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

escenario3 <- rbind(n50, n100, n200)
colnames(escenario3) <- c('AUC.par_hat', '', 'Int.par', '', 'AUC_hat', '', 'Int', '', 'gAUC.par_hat', '', 'gInt.par', '', 'gAUC_hat', '', 'gInt', '')
escenario3



### Escenario 4 ----
N <- 1e3
v <- 5 #grados de libertad
n <- 1e4
t <- seq(1/1e6, 1, 1/1e4)

R_r <- function(t) {1-pt((qnorm(1-t)-1)/2.2, v)}
R_teo <- R_r(t) #curva ROC usual teorica
AUCteo4 <- mean(R_teo)

#curva gROC teorica
Rg_teo <- pbsapply(t, function(t) {
  gamma <- seq(max(0,1-1/t), min(1,1/t), 1e-4)
  max(1 - R_r(1-gamma*t) + R_r((1-gamma)*t))
})
gAUCteo4 <- mean(Rg_teo)

c(AUCteo4, gAUCteo4)

n <- 50
counting <- pbsapply(1:N, function(i) {
  set.seed(i+31)
  
  #simulamos los datos
  xi <- 2.2*rt(n, v) + 1
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n50 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 100
counting <- pbsapply(1:N, function(i) {
  set.seed(i-64)
  
  #simulamos los datos
  xi <- 2.2*rt(n, v) + 1
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n100 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 200
counting <- pbsapply(1:N, function(i) {
  set.seed(-i*2)
  
  #simulamos los datos
  xi <- 2.2*rt(n, v) + 1
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n200 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
          medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

escenario4 <- rbind(n50, n100, n200)
colnames(escenario4) <- c('AUC.par_hat', '', 'Int.par', '', 'AUC_hat', '', 'Int', '', 'gAUC.par_hat', '', 'gInt.par', '', 'gAUC_hat', '', 'gInt', '')
escenario4


### Escenario 5 ----
N <- 1e3
k <- 3
theta <- 2
t <- seq(1/1e6, 1, 1/1e4)

R_r <- function(t) {1-pgamma(qnorm(1-t)+3.5, shape=k, scale=theta)}
R_teo <- R_r(t) #curva ROC usual teorica
AUCteo <- mean(R_teo)

#curva gROC teorica
Rg_teo <- pbsapply(t, function(t) {
  gamma <- seq(max(0,1-1/t), min(1,1/t), 1e-4)
  max(1 - R_r(1-gamma*t) + R_r((1-gamma)*t))
})
gAUCteo <- mean(Rg_teo)

c(AUCteo, gAUCteo)

n <- 50
counting <- pbsapply(1:N, function(i) {
  set.seed(i+31)
  
  #simulamos los datos
  xi <- rgamma(n, shape = k, scale = theta)-3.5
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n50 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 100
counting <- pbsapply(1:N, function(i) {
  set.seed(i+31)
  
  #simulamos los datos
  xi <- rgamma(n, shape = k, scale = theta)-3.5
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n100 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 200
counting <- pbsapply(1:N, function(i) {
  set.seed(i+31)
  
  #simulamos los datos
  xi <- rgamma(n, shape = k, scale = theta)-3.5
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n200 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

escenario5 <- rbind(n50, n100, n200)
colnames(escenario5) <- c('AUC.par_hat', '', 'Int.par', '', 'AUC_hat', '', 'Int', '', 'gAUC.par_hat', '', 'gInt.par', '', 'gAUC_hat', '', 'gInt', '')
escenario5


### Escenario 6 ----
N <- 1e3
alpha <- 2
beta <- 2
t <- seq(1/1e6, 1, 1/1e4)

R_r <- function(t) {1-pbeta((qnorm(1-t)+3.7)/10, alpha, beta)}
R_teo <- R_r(t) #curva ROC usual teorica
AUCteo <- mean(R_teo)

#curva gROC teorica
Rg_teo <- pbsapply(t, function(t) {
  gamma <- seq(max(0,1-1/t), min(1,1/t), 1e-4)
  max(1 - R_r(1-gamma*t) + R_r((1-gamma)*t))
})
gAUCteo <- mean(Rg_teo)

c(AUCteo, gAUCteo)

n <- 50
counting <- pbsapply(1:N, function(i) {
  set.seed(i+31)
  
  #simulamos los datos
  xi <- rbeta(n, 2, 2)*10-3.7
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n50 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

n <- 100
counting <- pbsapply(1:N, function(i) {
  set.seed(i+2)
  
  #simulamos los datos
  xi <- rbeta(n, 2, 2)*10-3.7
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n100 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
         medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])


n <- 200
counting <- pbsapply(1:N, function(i) {
  set.seed(i*4+3)
  
  #simulamos los datos
  xi <- rbeta(n, 2, 2)*10-3.7
  mu1 <- mean(xi)
  sigma1 <- sqrt(var(xi))
  chi <- rnorm(n, 0, 1)
  mu0 <- mean(chi)
  sigma0 <- sqrt(var(chi))
  ahat <- (mu1-mu0)/sigma0
  bhat <- sigma1/sigma0
  X <- c(xi, chi) #Realizacion muestral del marcador
  D <- c(rep(1, n), rep(0, n))
  roc <- gROC(X, D)
  groc <- gROC(X, D, side = 'both')
  
  #estimacion parametrica curva ROC usual
  R.par_hat <- pnorm(ahat/bhat + qnorm(t)/bhat)
  AUC.par_hat <- pnorm(ahat/sqrt(1+bhat^2))
  Int.par <- sqrt(n)*mean(abs(R_teo-R.par_hat))
  
  #estimacion empirica curva ROC usual
  R_hat <- roc$roc
  R_hat <- approxfun(roc$t,R_hat)
  AUC_hat <- roc$auc
  Int <- sqrt(n)*mean(abs(R_teo-R_hat(t)))
  
  #estimacion parametrica curva gROC
  gt_hat <- function(g,t) {
    g - 1/t*pnorm(2*ahat/(1-bhat^2) + qnorm((1-g)*t))}
  gamma_t <- sapply(t, function(t) {
    uniroot(function(g) gt_hat(g,t) , lower = max(0, 1-1/t), upper = 1)$root})
  Rg.par_hat <- pnorm(ahat/bhat + 1/bhat*qnorm((1-gamma_t)*t)) + 1 - pnorm(ahat/bhat + 1/bhat*qnorm(1-gamma_t*t))
  gAUC.par_hat <- mean(Rg.par_hat)
  gInt.par <- sqrt(n)*mean(abs(Rg_teo-Rg.par_hat))
  
  #estimacion empirica curva gROC
  Rg_hat <- groc$roc
  Rg_hat <- approxfun(groc$t,Rg_hat)
  gAUC_hat <- groc$auc
  gInt <- sqrt(n)*mean(abs(Rg_teo-Rg_hat(t)))
  
  c(AUC.par_hat, Int.par, AUC_hat, Int, gAUC.par_hat, gInt.par, gAUC_hat, gInt)
})
medias <- apply(counting, 1, mean)
sigma <- apply(counting, 1, sd)
n200 <- c(medias[1], sigma[1], medias[2], sigma[2], medias[3], sigma[3], medias[4], sigma[4],
          medias[5], sigma[5], medias[6], sigma[6], medias[7], sigma[7], medias[8], sigma[8])

escenario6 <- rbind(n50, n100, n200)
colnames(escenario6) <- c('AUC.par_hat', '', 'Int.par', '', 'AUC_hat', '', 'Int', '', 'gAUC.par_hat', '', 'gInt.par', '', 'gAUC_hat', '', 'gInt', '')
escenario6


