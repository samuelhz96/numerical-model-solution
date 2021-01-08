# Define parameter values
A <- 2; eta <- 2; mu <- 0.2; N <- 100; gamma <- 0.4

# Define HH level emission output vector
e_g <- seq(0,10,by=0.001)

# Calculate a the first derivative of Q w.r.t. e 
D_Q_e_g <- ((gamma*A*e_g^(gamma-1)-1)/(1+A*e_g^(gamma))-e_g)- eta*(N^eta)*(mu^(-eta))*e_g^(eta-1)

# Plot the dericatives:
plot(e_g,D_Q_e_g, type = 'l')

# Get the derivatives for the three smallest outputs:
D_Q_e_g[1:3]

# There is no numerical solution found.