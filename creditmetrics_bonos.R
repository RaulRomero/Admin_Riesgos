# Creditmetrics - bonos


# spreads en puntos base --------------------------------------------------

# matriz de spreads para 6 calificaciones 5 años
spreads <- matrix(0, nrow = 7, ncol = 5)
# AAA
spreads[1,1] <- 25
spreads[1,2] <- 55
spreads[1,3] <- 84
spreads[1,4] <- 116
spreads[1,5] <- 125
# AA
spreads[1,1] <- 56
spreads[1,2] <- 87
spreads[1,3] <- 117
spreads[1,4] <- 148
spreads[1,5] <- 159
# A 
spreads[1,1] <- 201
spreads[1,2] <- 232
spreads[1,3] <- 264
spreads[1,4] <- 291
spreads[1,5] <- 303
# BBB
spreads[1,1] <- 351
spreads[1,2] <- 382
spreads[1,3] <- 411
spreads[1,4] <- 443
spreads[1,5] <- 467
# BB
spreads[1,1] <- 438
spreads[1,2] <- 476
spreads[1,3] <- 512
spreads[1,4] <- 553
spreads[1,5] <- 583
# B
spreads[1,1] <- 564
spreads[1,2] <- 614
spreads[1,3] <- 614
spreads[1,4] <- 697
spreads[1,5] <- 730
# CCC
spreads[1,1] <- 892
spreads[1,2] <- 970
spreads[1,3] <- 1028
spreads[1,4] <- 1091
spreads[1,5] <- 1136

spreads <- spreads / 100^2
# risk free rate ----------------------------------------------------------

rf <- c(3.51, 3.89, 4.56, 4.91, 5.12) / 100
