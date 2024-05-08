# Marco Aurelio González Tagle
# Matrícula
# 08/05/2024


# Ingresar datos ----------------------------------------------------------

# Ingresar datos del diseño aleatorio
# 5 tratamientos germinativos
# 4 repeticiones cada tratamiento


germ <- c(3, 3, 4, 2, 7, 8, 7, 6, 8, 9, 8, 7,
          6, 7, 7, 6, 3, 2, 1, 3)
trat <- gl(5, 4, 20, labels = c("Ctrl", "EM", "Ra4h", 
           "AG", "AC"))

Exper <- data.frame(trat, germ)


boxplot(Exper$germ ~ Exper$trat, col = "lightgreen",
        xlab = "Tratamientos", ylab = "Germinación (%)")

# Revisar normalidad
shapiro.test(Exper$germ)

# Revisa la igualdad de varianzas
bartlett.test(Exper$germ, Exper$trat)

med.trat <- tapply(Exper$germ, Exper$trat, mean)
med.trat

# Media general
MG <- mean(Exper$germ)

var.trat <- tapply(Exper$germ, Exper$trat, var)
var.trat

Exper$SC <- (Exper$germ - MG)^2

# Suma de cuadrados del experimento SCTotal
SCtot <- sum(Exper$SC)

# Suma de cuadrados del tratamiento SCTrat

SCtrat <- sum((med.trat-MG)^2 * 4)
SCtrat

# Suma cuadrado del error
SCtot - SCtrat

SCtrat/4
9.7/15


# ANOVA usando funcion aov

Exp.aov <- aov(Exper$germ ~ Exper$trat)
summary(Exp.aov)

# Existen diferencias entre los trat de germn
# Por lo tanto, aplicaremos una prueba de Tukey

TukeyHSD(Exp.aov)
plot(TukeyHSD(Exp.aov))
