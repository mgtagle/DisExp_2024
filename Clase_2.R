# Marco Aurelio González Tagle
# 29/04/2024
# 123456


# Importar datos ----------------------------------------------------------

# Utilizar función read.csv sirve para importar datos

cr <- read.csv("Cedro.csv", header = TRUE)

# Revisión datos ----------------------------------------------------------

mean(cr$diametro)
mean(cr$altura)
mean(cr$diametro);median(cr$diametro)

sd(cr$diametro); sd(cr$altura)
range(cr$diametro)

fivenum(cr$diametro)


# Representación gráfica -------------------------------------------------

boxplot(cr$diametro)

boxplot(cr$altura, col = "lightgreen",
        ylim=c(10,30), ylab = "Altura (cm)",
        main = "Cedro rojo")

hist(cr$altura, xlab = "Altura (cm)",
     main = "Cedro rojo",
     ylab = "Frecuencia",
     col = "red")

stem(cr$altura)
