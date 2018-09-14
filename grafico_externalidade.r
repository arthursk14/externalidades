#programa que cria um gráfico ilustrando o nível de produção de um bem ou serviço gerador de externalidades negativas.
#funções de custo marginal, benefício marginal e externalidade na margem arbitrárias e lineares.
x <- 0:20

a1 <- 2 
a2 <- 15
a <- a1*x+a2

b1 <- 4
b2 <- 15
b <- b1*x+b2

c1 <- 2
c2 <- 0
c <- c1*x+c2

d1 <- -5
d2 <- 100
d <- d1*x+d2

plot(c(0,20),c(-60,100),axes=FALSE,frame.plot=TRUE,type='n',xlab=NA,ylab=NA) 

lines(x,a,col='red')
lines(x,b,col='blue')
lines(x,c,col='green')
lines(x,d,col='black')

legend(0, -5, legend=c("Custo Marginal Privado", "Custo Marginal Social", 
                       "Externalidade na Margem", "Benefício Marginal", 
                       "Produto", "Produto Socialmente Ótimo"),
       col=c("red", "blue", "green", "black", "red", "blue"), cex=0.75, text.font=2,
       lty=c(1,1,1,1,2,2), bty='n', ncol=2, x.intersp = 2)

inter <- function(p1,p2,p3,p4) {
          A <- rbind(c(-p1, 1),
                     c(-p3, 1))
          B <- c(p2,p4);
          solve(A, B)
}

i <- inter(a1,a2,d1,d2)
j <- inter(b1,b2,d1,d2)

library(plotrix)
ablineclip(v=i[1],y1=0,y2=100,col="red",lty=2)
ablineclip(v=j[1],y1=0,y2=100,col="blue",lty=2)

#a3 <- 2
#a4 <- 15+2*(j[1])
#ap <- a3*x+a4
#
#lines(x,ap,col='purple')
#inter(a3,a4,d1,d2)

