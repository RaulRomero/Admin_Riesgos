# Creditmetrics - bonos
library(timeSeries)
library(ggplot2)


# función multibene -------------------------------------------------------

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# datos generales ---------------------------------------------------------

tasa_recuper <- 0.4
niv_conf <- 0.95
alpha    <- 1 - niv_conf
zeta     <- pnorm(alpha)

# spreads en puntos base --------------------------------------------------

# matriz de spreads para 6 calificaciones y default por año - 5 años
calif   <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "default")
años    <- 1:5
spreads <- matrix(0, nrow = length(calif), ncol = length(años))

# AAA
spreads[1,1] <- 25
spreads[1,2] <- 55
spreads[1,3] <- 84
spreads[1,4] <- 116
spreads[1,5] <- 125
# AA
spreads[2,1] <- 56
spreads[2,2] <- 87
spreads[2,3] <- 117
spreads[2,4] <- 148
spreads[2,5] <- 159
# A 
spreads[3,1] <- 201
spreads[3,2] <- 232
spreads[3,3] <- 264
spreads[3,4] <- 291
spreads[3,5] <- 303
# BBB
spreads[4,1] <- 351
spreads[4,2] <- 382
spreads[4,3] <- 411
spreads[4,4] <- 443
spreads[4,5] <- 467
# BB
spreads[5,1] <- 438
spreads[5,2] <- 476
spreads[5,3] <- 512
spreads[5,4] <- 553
spreads[5,5] <- 583
# B
spreads[6,1] <- 564
spreads[6,2] <- 614
spreads[6,3] <- 614
spreads[6,4] <- 697
spreads[6,5] <- 730
# CCC
spreads[7,1] <- 892
spreads[7,2] <- 970
spreads[7,3] <- 1028
spreads[7,4] <- 1091
spreads[7,5] <- 1136

spreads <- spreads / 100^2
colnames(spreads) <- años
rownames(spreads) <- calif

spreads <- cbind(spreads, replicate(4, spreads[ ,5]))

# risk free rate ----------------------------------------------------------

rf <- c(3.51, 3.89, 4.56, 4.91, 5.12) / 100
names(rf) <- años

# parte espuria - por variar - editar{}
rf <- c(rf, replicate(4, rf[5]))

# datos de bonos ----------------------------------------------------------


# definir variables relevantes
horizonte <- 1
columnas <- c("coupon", "vencimiento", "calif_num")
calif_num <- 1:(length(calif)-1)
names(calif_num) <- calif[1:(length(calif)-1)]

# información sobre los bonos
liverpool_aaa <- c(7.64/100, 7  - horizonte, 1)
herdez_aa     <- c(8.02/100, 8  - horizonte, 2)
alsea_a       <- c(8.07/100, 10 - horizonte, 3)
cemex_bb      <- c(7.40/100, 2  - horizonte, 5)

# definir matriz de bonos (informativo)
bonos <- cbind(liverpool_aaa, herdez_aa, alsea_a, cemex_bb)
rownames(bonos) <- columnas
names_bonos <- colnames(bonos)

# valor nominal del bono 
face_value <- replicate(4, 100)

# determinación de flujos de efectivo
max_venc <- max(bonos[2, ])
flujos   <- matrix(0, nrow = ncol(bonos), ncol=(max_venc))

for(i in 1:ncol(bonos)){
  # condicional para determinar flujos de bonos
  if(max_venc - bonos[2, i]>0){
    flujos[i, ] <- c(replicate(bonos[2, i], face_value[i]*bonos[1,i]),
                     replicate(max_venc - bonos[2, i],0))
  }else{
    flujos[i, ] <- replicate(bonos[2, i], face_value[i]*bonos[1,i])
  }
  
  flujos[i, bonos[2, i]] <- flujos[i, bonos[2, i]]+face_value[i]
}

# determinación de matriz de valor presente
p_value <- list()
m <- matrix(0, nrow = (nrow(spreads)-1), ncol=(max_venc))
# ciclo para recorrer lista
for( k in 1:ncol(bonos)){
  # recorrer spreads
  for( i in 1:(nrow(spreads)-1)){
    for( j in 1:ncol(spreads)){
      m[i, j] <- flujos[k,j]/(1+rf[j]+spreads[i,j])^j
    }
  }
  rownames(m) <- c(calif[1:(length(calif)-1)])
  colnames(m) <- 1:ncol(m)
  p_value[[k]] <- m
}

names(p_value) <- names_bonos

# determinación de valor presente total por bono según su calif. de vencim.
calif2 <- c(calif, "WD")
present_value <- list()
m2 <- numeric(nrow(m))
for(k in 1:ncol(bonos)){
  for(i in 1:nrow(m)){
    m2[i] <- sum(p_value[[k]][i, ])
  }
  m2 <- c(m2, tasa_recuper * face_value[k], 0)
  names(m2) <- calif2
  present_value[[k]] <- m2
  m2 <- numeric(nrow(m))
}
names(present_value) <- names_bonos

# transition matrix -------------------------------------------------------
tm <-  matrix(0, nrow = length(calif2), ncol = length(calif)-1)
tm_aaa <- c(100, 0, 0, 0, 0, 0, 0, 0, 0)/100
tm_aa  <- c(0, 88.57, 2.86, 0, 0, 0, 0, 0, 8.57)/100
tm_a   <- c(0, 0.73, 90.48, 5.13, 0,0,0,0,3.66)/100
tm_bbb <- c(0, 0, 2.26, 85.97, 1.36, 0, 0, 0, 10.41)/100
tm_bb  <- c(0, 0, 0, 4.95, 75.23, 0.93, 0, 0, 18.89)/100
tm_b   <- c(0, 0, 0, 0.43, 3.88, 65.95, 12.5, 3.02, 14.22)/100
tm_ccc <- c(0, 0, 0, 0, 0, 7.69, 53.85, 23.08, 15.38)/100

tm <- cbind(tm_aaa,tm_aa,tm_a,tm_bbb,tm_bb,tm_b,tm_ccc)
colnames(tm) <- calif[1:(length(calif)-1)]
rownames(tm) <- calif2

# probabilidades de interés filtradas 
tm_f <- tm[ ,c(1, 2, 3, 5)]

# creditmetrics por bono --------------------------------------------------

# determinación del valor esperado 
valor_esperado <- numeric()
for( i in 1:ncol(bonos)){
  valor_esperado[i] <- present_value[[i]] %*% tm_f[ ,i]
}
names(valor_esperado) <- names_bonos

# determinación de la varianza
desv_est <- numeric(length(valor_esperado))
for( i in 1:ncol(bonos)){
  for(j in 1:length(calif2)){
    m3 <- tm_f[j, i] * (present_value[[i]][j] - valor_esperado[i])^2
    desv_est[i] <- desv_est[i] + m3
  }
  desv_est[i] <- sqrt(desv_est[i])
}
names(desv_est) <- names_bonos

# determinación de la función de distribución acumulada
acum <- matrix(0, nrow = nrow(tm_f), ncol = ncol(tm_f))
acum[1, ] <- tm_f["WD", ]
for(j in 1:ncol(tm_f)){
  for(i in 2:nrow(tm_f)){
    acum[i, j] <- acum[i-1, j] + tm_f[nrow(tm_f)-i+1,j]
  }
}

# VaR ---------------------------------------------------------------------

# definir la posición en la cual se encuentra la frec acum f1 < alpha < f2
c_var <- matrix(0, nrow = 2, ncol = length(names_bonos))

for(i in 1:length(names_bonos)){
  cont <- 1
  while(acum[cont, i] < alpha){
    c_var[1,i] <- cont
    cont <- cont + 1
    c_var[2,i] <- cont
  }
  
  if(c_var[2,i] == 0){
    c_var[2,i] <- 1
  }
  
}

colnames(c_var) <- names_bonos
rownames(c_var) <- c('Pos. Inicial', 'Pos final')


# interpolación y determinación del Valor en Riesgo
VaR <- numeric()

for(i in 1:ncol(bonos)){
  
  if(c_var[1, i] == 0){ 
    y1 <- 0
    y2 <- acum[c_var[2,i], i]
    x1 <- 0
    x2 <- present_value[[i]][c_var[2, i]]
  } else {
    y1 <- acum[c_var[1,i], i]
    y2 <- acum[c_var[2,i], i]
    x1 <- present_value[[i]][c_var[1, i]]
    x2 <- present_value[[i]][c_var[2, i]]
    }
  VaR[i] <- (alpha- y1) * (x2 - x1) / (y2 - y1) + x1
}

names(VaR) <- names_bonos

# gráficas por bono -------------------------------------------------------


# gráficas
grafikis <- list()
for(k in 1:length(names_bonos)){
  gt <- cbind(present_value[[k]], tm_f[,k], acum[nrow(acum):1, k])
  colnames(gt) <- c("valor_presente_del_bono",
                    "freq.rel", "freq.acum")
  
  gt <- as.data.frame(gt)
  
  g <- ggplot()+geom_line(data = gt, aes(x = valor_presente_del_bono, y=freq.rel),
                          colour = "dark green", size = 1, linetype = "longdash")+
    geom_line(data = gt, aes(x = valor_presente_del_bono, y=freq.acum), 
              colour = "dark red", size = 1)+ ggtitle(names_bonos[k])+
    geom_vline(xintercept=VaR[k], linetype = "longdash", colour = "blue")
  grafikis[[k]] <- g
}

multiplot(grafikis[[1]],grafikis[[2]],grafikis[[3]],grafikis[[4]],cols = 2)

# Portafolio de 2 bonos ---------------------------------------------------

pv <- list(present_value[[2]], present_value[[3]])
names(pv) <- names_bonos[2:3]

# determinar posibles valores del portafolio
port <- numeric()

port[1] <- 0
for(k in 1:length(calif2)){
  port <- c(port,pv$herdez_aa[k]+pv$alsea_a)
}
port <- as.numeric(port[c(-1)])
names(port) <- 1:length(port)

la_ley <- as.numeric(names(sort(port)))
port <- as.numeric(sort(port))

# determinar probabilidad conjunta del portafolio (frec rel)
tm_ff <- tm_f[ ,c(2,3)]
frec_rel <- numeric()

frec_rel[1] <- 0
for(k in 1:length(calif2)){
  frec_rel <- c(frec_rel,tm_ff[k, 1]*tm_ff[ ,2])
}
frec_rel <- as.numeric(frec_rel[c(-1)])
frec_rel <- frec_rel[la_ley]

# determinación de frecuencia acumulada
frec_acum <- numeric()
frec_acum[1] <- frec_rel[1]
for(i in 2:length(frec_rel)){
  frec_acum[i] <- frec_acum[i-1] + frec_rel[i]
}

# determinación del valor esperado

expected_value_port <- frec_rel %*% port
stdev <- 0
for(i in 1:length(port)){
  stdev <- stdev+frec_rel[i]*(port[i]-expected_value_port)^2
}
stdev <- sqrt(stdev)

# valor en riesgo del portafolio ------------------------------------------

# determinar posición...
p <- c(0,0)
for(i in 1:length(frec_acum)){
  if(frec_acum[i] < alpha){
    p[1] <- i
    p[2] <- p[1] + 1
  }
}

# interpolación para determinar valor en riesgo 
Y1 <- frec_acum[p[1]]
Y2 <- frec_acum[p[2]]
X1 <- port[p[1]]
X2 <- port[p[2]]
VaR_portaf <- (alpha - Y1)*(X2 - X1) / (Y2 - Y1) + X1

# gráficas portafolio -----------------------------------------------------

solver <- as.data.frame(cbind(port, frec_rel, frec_acum))

g2 <- ggplot()+geom_line(data = solver, aes(x = port, y=frec_rel),
                         colour = "dark green", size = 1, linetype = "longdash")+
  geom_line(data = solver, aes(x = port, y=frec_acum), 
            colour = "dark red", size = 1)+ ggtitle("distr_prob_portafolio")+
  geom_vline(xintercept=VaR_portaf, linetype = "longdash", colour = "blue")
g2

# Presentación de resultados ----------------------------------------------

# [agregar redacción de resultados]