#Data set que probe a hacer yo para probar a hacer letras
#despues de ejecutarlo se obtiene el data.frame UwU que tiene las coord x, y y las clases de cada punto en el

source('01_setup.r')
#rectangulos de las Us
n <- 1000
U1left_x <- runif(n,1,2)
U1left_y <- runif(n,6,10)
U1right_x <- runif(n,4,5)
U1right_y <- runif(n,6,10)

U2left_x <- runif(n,11,12)
U2left_y <- runif(n,6,10)
U2right_x <- runif(n,14,15)
U2right_y <- runif(n,6,10)
#semicirculo de las Us
U1_r <- runif(n,1,2)
U1_theta <- -runif(n,0,pi)


U2_r <- runif(n,1,2)
U2_theta <- -runif(n,0,pi)

U_x <- c(U1left_x, U1right_x, U2left_x, U2right_x, U1_r*cos(U1_theta)+3,U2_r*cos(U2_theta)+13)
U_y <- c(U1left_y, U1right_y, U2left_y, U2right_y, U1_r*sin(U1_theta)+6,U2_r*sin(U2_theta)+6)
plot(U_x,U_y)


#la W
Wleft1_y <- runif(n,4,7)
Wleft2_y <- runif(n,4,7)
Wright1_y <- runif(n,4,7)
Wright2_y <- runif(n,4,7)
Wleft1_x <- c()
Wleft2_x <- c()
Wright1_x <- c()
Wright2_x <- c()
for( i in 1:n){
  Wleft1_x <- c(Wleft1_x,runif(1,1/6*Wleft1_y[i]+13/3,1/6*Wleft1_y[i]+16/3))
  Wleft2_x <- c(Wleft2_x,runif(1,-1/6*Wleft2_y[i]+13/3+0.5,-1/6*Wleft2_y[i]+16/3+0.5))
  Wright1_x <- c(Wright1_x,runif(1,1/6*Wright1_y[i]+13/3+2.5,1/6*Wright1_y[i]+16/3+2.5))
  Wright2_x <- c(Wright2_x,runif(1,-1/6*Wright2_y[i]+13/3+3,-1/6*Wright2_y[i]+16/3+3))
  
}
W_x <- c(Wleft1_x, Wleft2_x, Wright1_x, Wright2_x)
W_x <- W_x+1.75
W_y <- c(Wleft1_y, Wleft2_y, Wright1_y, Wright2_y)



UwU_x <- c(U_x,W_x)
UwU_y <- c(U_y,W_y)
UwU <- data.frame(UwU_x,UwU_y)
Classes <- c(rep(1, times =2000), rep(2,times =2000),rep(1, times = 1000), rep(2, times = 1000), rep(3,times =4000))
UwU <- mutate(UwU, class = factor(Classes))

ggplot(data = UwU)+geom_point(mapping = aes(x = UwU_x, y = UwU_y, colour = class))

