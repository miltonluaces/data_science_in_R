# FUNCTIONS FOR DLM-SMC FOR NEGATIVE BINOMIAL DISTRIBUTION
# ==========================================================================================================
# ==========================================================================================================


library(dlm)
library(mvtnorm)

        
# SMC Function for Binomial
#-----------------------------------------------------------------------------------------------------------
SeqMCNegBin  =  function(Data, n, F, G, m0, C0, V, W, nW, nSamples) {
    
    #Declarations
    nD = length(Data)
    nF = length(F)
    A = array(0, dim=c(nF,nW,nD+1))
    InvArav = matrix(0, nD+1,nW)
    m = array(0, dim=c(nF,nW,nD+1))
    C = array(0, dim=c(nF,nF,nW,nD+1))
    w = matrix(0,nD+1,nW)
    theta = array(0, dim=c(nF,nW,nD+1))
    thetaT = array(0, dim=c(nF,nW,nD))
    thetaHat = matrix(0,nF,nD)
    yHat = matrix(nrow=nSamples,ncol=nD)
  
    #Initial values
    w[1,] = 1/nW
    m[,,1] = m0
    C[,,,1] = C0
    y = c(0, Data); 
    
    for(i in 1:nW) {
        v = rmvnorm(1, m[,i,1], C[,,i,1])
        theta[,i,1] = t(v)
    }

    for( t in 2:(nD+1)) {
        for(i in 1:nW) {
            #intermediate calculations
            R = G %*% C[,,i,t-1] %*% t(G) + W  #
            Q = exp(t(F) %*% G %*% m[,i,t-1])[1,1]  #
            A[,i,t] = (n*Q/((1+Q)^2)) * F
            InvArav[t,i] = solve(t(A[,i,t]) %*% R %*% A[,i,t] + V)
            #importance density mean: m_t = E(t_t|t_t-1,y_t) = G * m_t-1 * R_t * A_t * (A'_t * R_t * A_t + V)^-1 * (y_t - mG)
            m[,i,t] = G %*% m[,i,t-1] + R %*% A[,i,t] %*% InvArav[t,i] %*% (y[t] - n*Q/(1+Q))  #
            #importance density covariance: C_t = Var(t_t|t_t-1,y_t) = R_t - R_t * A_t * (A'_t * R_t * A_t + V)^-1 * A'_t * R_t
            C[,,i,t] = R-(R %*% A[, i, t] %*% InvArav[t,i]) %*% t(A[,i,t]) %*% R 
            #draw theta_t = g(theta_t | theta_t-1, y_t)
            theta[,i,t] = rmvnorm(1, m[,i,t], C[,,i,t])
            #importance density
            impDens = dmvnorm(theta[,i,t], m[,i,t], C[,,i,t])
            #t_t = G(t_t-1) + w_t
            probTt = dmvnorm(theta[,i,t], G %*% m[,i,t-1], R)            
            #y_t|t_t = p(y_t, Binomial(F' t_t))
            p = 1/ (1+ exp(t(F) %*% theta[,i,t]));
            probYt = dbinom(y[t], n, p)   #
            #calculate importance weights
            w[t,i]  = w[t-1,i] * probYt * probTt / impDens
        }

        #normalize importance weights
        w[t,] = w[t,]/(sum(w[t,]))

        nEff = 1/crossprod(w[t,])
        if(nEff < nW/3) {
            index = sample(nW, nW, replace=TRUE, prob = w[t,])
            theta[,,t] = theta[,index,t]
            w[t,] = 1/nW
        }

        for(i in 1:nW) { thetaT[,i,t-1] =  w[t,i] * theta[,i,t] }
        for(f in 1:nF) { thetaHat[f,t-1]  =  sum(thetaT[f,,t-1]) }
  
        # integral : int[ p(y_t| t_t) * p(t_t|D_t-1)]
        p = 1/ (1+exp(t(F) %*% thetaHat[,t-1]));
        yh = rbinom(nSamples, n, p);
        yHat[,t-1] = yh
    }
    yHat
}


# Forecast. Mean for each point
#-----------------------------------------------------------------------------------------------------------
CalcFcst = function(yHat) {
  fc = NULL
  for(i in 1:length(yHat[1,])) { 
    m = mean(yHat[,i]) 
    fc = c(fc, m)
  }
  fc
}

