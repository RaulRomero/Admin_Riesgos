# Posibles calificaciones de análisis financiero 

# editar calificaciones esperadas
calif <- c(0.85, 0.85, 0.88)

# no editar
acum <- c(43.4, 41.9)
names(acum) <- c("radrigo", "raúl")
necesario <- 85 - acum
names(calif) <- c("examen 3", "examen 4", "proyecto")
calif_final <- calif %*% c(15, 15, 20) + acum
names(calif_final) <- c("radrigo", "raúl")
calif_final