#quiero generar unos datos de densidad variable para ver que falla el dbscan
#dos clusters de varianza similar circulares
n1 <- 300
A_x<- rnorm(n1,2,0.5)
A_y <- rnorm(n1,4,0.5)

B_x<- rnorm(n1,4.5,0.5)
B_y <- rnorm(n1,2,0.5)

#ahora 3 clusters pequeños cerquita y con mayor concentracion
n2 <- 200
C1_x <- rnorm(n2,4.8,0.1)
C1_y <- rnorm(n2,5.2,0.1)

C2_x <- rnorm(n2,6,0.2)
C2_y <- rnorm(n2,4.85,0.2)

C3_x <- rnorm(n2,5.8,0.15)
C3_y <- rnorm(n2,6,0.15)

D_x <- c(A_x,B_x,C1_x,C2_x,C3_x)
D_y <- c(A_y,B_y,C1_y,C2_y,C3_y)
class <- c(rep(1, times = n1), rep(2, times = n1), rep(3, times = n2), rep(4, times = n2), rep(5,times = n2))
D <- data.frame(D_x,D_y)

D <- mutate(D, class = factor(class))
ggplot(data = D) + geom_point(mapping = aes(x = D_x, y = D_y), color = class)
