# Define parameter values
A <- 2; eta <- 2; mu <- 0.2; N <- 100; gamma <- 0.4

# Define HH level emission output vector
e_g <- seq(0,10,by=0.001)

# Calculate a the first derivative of Q w.r.t. e 
D_Q_e_g <- ((gamma*A*e_g^(gamma-1)-1)/(1+A*e_g^(gamma))-e_g)- eta*(N^eta)*(mu^(-eta))*e_g^(eta-1)

# Plot the derivatives:
plot(e_g,D_Q_e_g, type = 'l')

# Get the derivatives for the three smallest outputs:
D_Q_e_g[1:3]

# There is no numerical solution found.


# Alternative: directly maximize the utility w.r.t. e_g: 
# beta is needed; chosen at some plausible level
b <- 0.96

# Give utility function in terms of e_g
U <- log(((1)/(1+b))*(1+A*e_g^(gamma)-e_g)) + b*log(((b)/(1+b))*(1+A*e_g^(gamma)-e_g)) - (1+b)*((e_g*N)/mu)^(eta)

# plot utility function w.r.t. e_g
plot(e_g,U, type = 'l')





# Define the same as before but for shorter range of emission values:
e_g_2 <- seq(0.00000001,0.001,length.out = 10000)
D_Q_e_g_2 <- ((gamma*A*e_g_2^(gamma-1)-1)/(1+A*e_g_2^(gamma))-e_g_2)- eta*(N^eta)*(mu^(-eta))*e_g_2^(eta-1)
U_2 <- log(((1)/(1+b))*(1+A*e_g_2^(gamma)-e_g_2)) + b*log(((b)/(1+b))*(1+A*e_g_2^(gamma)-e_g_2)) - (1+b)*((e_g_2*N)/mu)^(eta)

plot(e_g_2,D_Q_e_g_2, type = 'l')
plot(e_g_2,U_2, type = 'l')

# it appears as the utility is maximized for an output very close to zero, 
# but not exactly zero



#############################################################################################
# It follows the same procedure but to find the optimal emissions for varying gamma and A:
eta <- 2; mu <- 0.2; b <- 0.96
# Define HH level emission output vector
e_g <- seq(0.0000001,0.0001,length.out = 100)
gamma <- seq(0.2,0.5,length.out = 30)
A <- seq(1,2,length.out = 30)
e_g_outer <- matrix(rep(0,30^2),ncol = 30,nrow = 30)

for(i in 1:length(gamma)){
  for(j in 1:length(A)){
    e_g_outer[i,j] <- e_g[which.max(log(((1)/(1+b))*(1+A[j]*e_g^(gamma[i])-e_g)) + 
                            b*log(((b)/(1+b))*(1+A[j]*e_g^(gamma[i])-e_g)) - 
                            (1+b)*((e_g*N)/mu)^(eta))]
  }
}
e_g_outer
persp(gamma,A,e_g_outer,zlim = c(0.00001,0.00015),ticktype = 'detailed')


# It looks like this is almost independent of gamma and A values. Maybe an approach with mu and eta varying explains more:
# vary eta and mu
gamma <- 0.5; A <- 2; b <- 0.96
e_g <- seq(0.0000001,0.0001,length.out = 100)
mu <- seq(0.01,0.99,length.out = 30)
eta <- seq(1,2,length.out = 30)
e_g_outer <- matrix(rep(0,30^2),ncol = 30,nrow = 30)

for(i in 1:length(mu)){
  for(j in 1:length(eta)){
    e_g_outer[i,j] <- e_g[which.max(log(((1)/(1+b))*(1+A*e_g^(gamma)-e_g)) + 
                                      b*log(((b)/(1+b))*(1+A*e_g^(gamma)-e_g)) - 
                                      (1+b)*((e_g*N)/mu[i])^(eta[j]))]
  }
}
persp(mu,eta,e_g_outer,ticktype = 'detailed', theta = -45, phi=20)

# clearly the varuation in mu explains the most change, while eta does appear to have a greater influence than A or gamma


