OGs <- function(n,k_i) {
  return((n+1-k_i)/n)
}

OGs_mod <- function(n,k_i,p_t) {
  return( (n+1-k_i)/n*1/(p_t) )
}

P1 <- matrix(c(1, 2, 3,0.0333,0.15,0.5667), nrow = 2, ncol = 3,byrow = TRUE)

P2 <- matrix(c(1,2,3,4,5,6,7,0.167,0.15,0.15,0.1833,0.1833,0.1833,0.1167),nrow = 2,ncol=7,byrow = TRUE)

data_P1_OGs <- OGs(3,P1[1,])
data_P1_OGs_mod <- OGs_mod(3,P1[1,],P1[2,])

data_P2_OGs <- OGs(7,P2[1,])
data_P2_OGs_mod <- OGs_mod(7,P2[1,],P2[2,])


plot(P2[1,], data_P2_OGs, type = "b", pch = 1, col = "blue", xlab = "k_i", ylab = "s_i",ylim = c(0,1))
lines(P2[1,], data_P2_OGs_mod/max(data_P2_OGs_mod), type = "b", pch = 2, col = "red")

plot(P1[1,], data_P1_OGs, type = "b", pch = 1, col = "blue", xlab = "k_i", ylab = "s_i",ylim = c(0,1))
lines(P1[1,], data_P1_OGs_mod/max(data_P1_OGs_mod), type = "b", pch = 2, col = "orange")