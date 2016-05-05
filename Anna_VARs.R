###PREPARATION
#############################################
#############################################

###VARIABLES
#Select variables
end <- cbind(hp$mid, hp$aff, hp$lux)                          # endogenous variables
colnames(end) <- c("mid", "aff", "lux")

enex <- cbind(hp$mid, hp$aff, hp$lux, hp.lag8$h_emp, hp.lag4$income, hp$morrate)
colnames(enex) <- c("mid", "aff", "lux", "l8 h_emp", "l4 income", "morrate")

##############################################
##############################################

###VEC, HOUSE PRICES ONLY
##############################################
##############################################
###LAG SELECTION
var <- VARselect(end, lag.max = pmax, type = "both")
k_aic <- var$selection[1]
k_hq  <- var$selection[2]
k_sic <- var$selection[3]
k <- min(k_aic,k_sic,k_hq)

###COINTEGRATION RANK TEST WITHOUT STRUCTURAL BREAKS
VEC1 <- ca.jo(end, ecdet = "trend", K=k)
summary(VEC1)
# => 1 cointegration relationship

#slotNames(VEC1)
lambda   <- VEC1@lambda        # eigenvalues
beta     <- VEC1@V             # cointegration vector beta
alpha    <- VEC1@W             # loading matrix alpha
pi       <- VEC1@PI            # coefficient matrix pi of the variables in lagged levels
res      <- VEC1@RK            # residual matrix of the regression in lagged levels

coint1   <- beta[,1] %*% t(end)
plot(hp$date,coint1,type="l")    #plot of the first cointegration relation


###COINTEGRATION RANK TEST WITH 1 STRUCTURAL BREAK
dummies <- cbind(d_sb_1)
colnames(dummies) <- c("1st SB")

VEC1_sb1 <- ca.jo(end, ecdet="none",  type = "eigen", K=k, dumvar=dummies)
summary(VEC1_sb1)
# => 1-2 cointegration relationships

beta <- VEC1_sb1@V             # cointegration vector beta
coint1 <- beta[,1] %*% t(end)
plot(hp$date,coint1,type="l")    #plot of the first cointegration relation
coint2 <- beta[,2] %*% t(end)
plot(hp$date,coint2,type="l")    #plot of the second cointegration relation

###SENSITIVITY TEST: COINTEGRATION RANK TEST WITH ENDOGENOUS STRUCTURAL BREAK
VEC1_sbend <- cajolst(end, trend=TRUE, K=k)
summary(VEC1_sbend)
as.character(hp$date[VEC1_sbend@bp])

###COINTEGRATION RANK TEST WITH 2 STRUCTURAL BREAKS
dummies <- cbind(d_sb_1,d_sb_2)
colnames(dummies) <- c("1st SB","2nd SB")

VEC1_sb2 <- ca.jo(end, ecdet="trend",  type = "eigen", K=k, dumvar=dummies)
summary(VEC1_sb2)
# => 2 cointegration relationships

beta <- VEC1_sb1@V             # cointegration vector beta
coint1 <- beta[,1] %*% t(end)
plot(hp$date,coint1,type="l")    #plot of the first cointegration relation
coint2 <- beta[,2] %*% t(end)
plot(hp$date,coint2,type="l")    #plot of the second cointegration relation


###JOHANSEN ML: RESTRICTIONS ON ALPHA
##############################################
# Likelihood ratio test for restrictions on alpha: alrtest(z,A,r)

A1.1 <- matrix(NA,nrow=3,ncol=2) # (n x m matrix)
A1.1[1,] <- c(0,0)    # zero-restrictions on mid
A1.1[2,] <- c(1,0)    # aff
A1.1[3,] <- c(0,1)    # lux
A1.1
VEC1_A1.1 <- alrtest(VEC1,A=A1.1,r=1)
summary(VEC1_A1.1)
#=> Cannot reject hypothesis of weak exogeneity of middle segment

A1.2 <- matrix(NA,nrow=3,ncol=2) # (n x m matrix)
A1.2[1,] <- c(1,0)    # mid
A1.2[2,] <- c(0,0)    # zero-restrictions on aff
A1.2[3,] <- c(0,1)    # lux
A1.2
VEC1_A1.2 <- alrtest(VEC1,A=A1.2,r=1)
summary(VEC1_A1.2)
#=> Can reject hypothesis of weak exogeneity of the affordable segment

A1.3 <- matrix(NA,nrow=3,ncol=2) # (n x m matrix)
A1.3[1,] <- c(1,0)    # mid
A1.3[2,] <- c(0,1)    # aff
A1.3[3,] <- c(0,0)    # zero-restrictions on lux
A1.3
VEC1_A1.3 <- alrtest(VEC1,A=A1.3,r=1)
summary(VEC1_A1.3)
#=> Ambiguous; mostly cannot reject hypothesis of weak exogeneity of luxury segment
##############################################

###JOHANSEN ML:  ON BETA
#!!!!!!!!!!!!!! NEED TO UPDATE WITH STRUCTURAL BREAKS
##############################################
# Likelihood ratio test for restrictions on beta
#blrtest(z,H,r) for specific restrictions on all cointegration vectors, e.g. beta=(1,-1,*,*,*) in all cointgration relations
#bh5lrtest(z,H,r), which assumes that one cointgration relation is known while the others have to be estimated e.g. beta_1=(1,-1,0,0,0)
#bh6lrtest(z,H,r,r1) test for the existence of one more general cointegration vector between a subset of variables, e.g. in the form (a,b,0,0,0)

B1.1 <- matrix(NA,nrow=3,ncol=1) # (n x s matrix)
B1.1[1,] <- c(1) #mid
B1.1[2,] <- c(-1) #aff
B1.1[3,] <- c(0) #lux
B1.1
VEC1_B1.1 <- blrtest(VEC1,H=B1.1,r=1)
summary(VEC1_B1.1)
#=> Must reject the the hypothesis that there is an equilibrium ratio between middle and affordable

B1.2 <- matrix(NA,nrow=3,ncol=1) # (n x s matrix)
B1.2[1,] <- c(1) #mid
B1.2[2,] <- c(0) #aff
B1.2[3,] <- c(-1) #lux
B1.2
VEC1_B1.2 <- blrtest(VEC1,H=B1.2,r=1)
summary(VEC1_B1.2)
#=> Must reject the the hypothesis that there is an equilibrium ratio between middle and luxury

B1.3 <- matrix(NA,nrow=3,ncol=1) # (n x s matrix)
B1.3[1,] <- c(0) #mid
B1.3[2,] <- c(1) #aff
B1.3[3,] <- c(-1) #lux
B1.3
VEC1_B1.3 <- blrtest(VEC1,H=B1.3,r=1)
summary(VEC1_B1.3)
#=> Must reject the the hypothesis that there is an equilibrium ratio between middle and luxury

B1.4 <- matrix(NA,nrow=3,ncol=1) # (n x s matrix)
B1.4[1,] <- c(1) #mid
B1.4[2,] <- c(-1) #aff
B1.4[3,] <- c(0) #lux
B1.4
VEC1_B1.4 <- bh5lrtest(VEC1,H=B1.4,r=2)
summary(VEC1_B1.4)
#=> Cannot reject the the hypothesis that there is an equilibrium ratio between middle and affordable

B1.5 <- matrix(NA,nrow=3,ncol=1) # (n x s matrix)
B1.5[1,] <- c(1) #mid
B1.5[2,] <- c(0) #aff
B1.5[3,] <- c(-1) #lux
B1.5
VEC1_B1.5 <- bh5lrtest(VEC1,H=B1.5,r=2)
summary(VEC1_B1.5)
#=> Cannot reject the the hypothesis that there is an equilibrium ratio between middle and luxury

B1.6 <- matrix(NA,nrow=3,ncol=1) # (n x s matrix)
B1.6[1,] <- c(0) #mid
B1.6[2,] <- c(1) #aff
B1.6[3,] <- c(-1) #lux
B1.6
VEC1_B1.6 <- bh5lrtest(VEC1,H=B1.6,r=2)
summary(VEC1_B1.6)
#=> Cannot reject the the hypothesis that there is an equilibrium ratio between affordable and luxury


##############################################
##############################################



###VEC, INCLUDING EXOGENOUS VARIABLES
##############################################
##############################################
###SET PARAMETERS
vars <- enex
n <- ncol(vars)
t <- nrow(vars)

trend <- 1:t
varst <- cbind(vars,trend)

###LAG SELECTION
var <- VARselect(vars, lag.max = 14, type = "trend")
k_aic <- var$selection[1]
k_hq  <- var$selection[2]
k_sic <- var$selection[3]
k <- min(k_aic,k_sic,k_hq)

VAR2 <- VAR(vars,p=k,type= "trend")
VAR2

###COINTEGRATION RANK TEST
VEC2 <- ca.jo(vars, ecdet = "trend",  type = "eigen", K=k)
summary(VEC2)
r <- 3   # => 2-3 cointegration relationships

#slotNames(VEC2)
lambda   <- VEC2@lambda        # eigenvalues
beta     <- VEC2@V             # cointegration vector beta
alpha    <- VEC2@W             # loading matrix alpha
pi       <- VEC2@PI            # coefficient matrix pi of the variables in lagged levels
res      <- VEC2@RK            # residual matrix of the regression in lagged levels

coint1   <- beta[,1] %*% t(varst)
plot(hp$date,coint1,type="l")   #plot of the potential first cointegration relation
coint2   <- beta[,2] %*% t(varst)
plot(hp$date,coint2,type="l")   #plot of the potential second cointegration relation
coint3   <- beta[,3] %*% t(varst)
plot(hp$date,coint3,type="l")   #plot of the potential third cointegration relation

###MODEL ESTIMATION (OLS REGRESSION OF VECM)
VEC2_unres <- cajorls(VEC2,r=r)     # r gives the number of cointegrating restrictions identied earlier
VEC2_unres

VAR2_unres <- vec2var(VEC2,r=r) # vec2var converts VEC into its level VAR representation
VAR2_unres

###SENSITIVITY TEST 1: COINTEGRATION RANK TEST WITH STRUCTURAL SHIFT DUMMIES
VEC2_sb1 <- ca.jo(vars, ecdet="trend", K=k, dumvar=dpre1986)
summary(VEC2_sb1)
# => 2-3 cointegration relationships (just like without structural break)

###SENSITIVITY TEST 2: COINTEGRATION RANK TEST WITH ENDOGENOUS STRUCTURAL SHIFT
VEC2_sb2 <- cajolst(vars, trend=TRUE, K=k)
# => Too many variables, critical values cannot be computed


###JOHANSEN ML: RESTRICTIONS ON ALPHA
#!!!!!!!!!!!!!! NEED TO UPDATE WITH STRUCTURAL BREAKS
##############################################
# Likelihood ratio test for restrictions on alpha: alrtest(z,A,r)

A1.1 <- matrix(NA,nrow=n,ncol=5) # (n x m matrix)
A1.1[1,] <- c(0,0,0,0,0)    # zero-restrictions on mid
A1.1[2,] <- c(1,0,0,0,0)    # aff
A1.1[3,] <- c(0,1,0,0,0)    # lux
A1.1[4,] <- c(0,0,1,0,0)    # h_emp
A1.1[5,] <- c(0,0,0,1,0)    # income
A1.1[6,] <- c(0,0,0,0,1)    # interest
A1.1
VEC2_A1.1 <- alrtest(VEC2,A=A1.1,r=r)
summary(VEC2_A1.1)
#=> Ambiguous; Mostly can  reject hypothesis of weak exogeneity of middle segment

A1.2 <- matrix(NA,nrow=n,ncol=5)  # (n x m matrix alpha)
A1.2[1,] <- c(1,0,0,0,0) #mid
A1.2[2,] <- c(0,0,0,0,0) #zero-restrictions on aff
A1.2[3,] <- c(0,1,0,0,0) #lux
A1.2[4,] <- c(0,0,1,0,0) #h_emp
A1.2[5,] <- c(0,0,0,1,0) #income
A1.2[6,] <- c(0,0,0,0,1) #interest
A1.2
VEC2_A1.2 <- alrtest(VEC2,A=A1.2,r=r)
summary(VEC2_A1.2)
#=> Can reject hypothesis of weak exogeneity of affordable segment at 1%

A1.3 <- matrix(NA,nrow=n,ncol=5)  # (n x m matrix alpha)
A1.3[1,] <- c(1,0,0,0,0) #mid
A1.3[2,] <- c(0,1,0,0,0) #aff
A1.3[3,] <- c(0,0,0,0,0) #zero-restrictions on lux
A1.3[4,] <- c(0,0,1,0,0) #h_emp
A1.3[5,] <- c(0,0,0,1,0) #income
A1.3[6,] <- c(0,0,0,0,1) #interest
A1.3
VEC2_A1.3 <- alrtest(VEC2,A=A1.3,r=r)
summary(VEC2_A1.3)
#=> Ambiguous; Mostly cannot reject hypothesis of weak exogeneity of luxury segment

A1.4 <- matrix(NA,nrow=n,ncol=5)  # (n x m matrix alpha)
A1.4[1,] <- c(1,0,0,0,0) #mid
A1.4[2,] <- c(0,1,0,0,0) #aff
A1.4[3,] <- c(0,0,1,0,0) #lux
A1.4[4,] <- c(0,0,0,0,0) #zero-restrictions on h_emp
A1.4[5,] <- c(0,0,0,1,0) #income
A1.4[6,] <- c(0,0,0,0,1) #interest
A1.4
VEC2_A1.4 <- alrtest(VEC2,A=A1.4,r=r)
summary(VEC2_A1.4)
#=> Ambiguous; Can mostly accept hypothesis of weak exogeneity of h_emp (if l_hemp>=4)

A1.5 <- matrix(NA,nrow=n,ncol=5)  # (n x m matrix alpha)
A1.5[1,] <- c(1,0,0,0,0) #mid
A1.5[2,] <- c(0,1,0,0,0) #aff
A1.5[3,] <- c(0,0,1,0,0) #lux
A1.5[4,] <- c(0,0,0,1,0) #h_emp
A1.5[5,] <- c(0,0,0,0,0) #zero-restrictions on income
A1.5[6,] <- c(0,0,0,0,1) #interest
A1.5
VEC2_A1.5 <- alrtest(VEC2,A=A1.5,r=r)
summary(VEC2_A1.5)
#=> Ambiguous; Must mostly reject hypothesis of weak exogeneity of income (even if l_income = 4)

A1.6 <- matrix(NA,nrow=n,ncol=5)  # (n x m matrix alpha)
A1.6[1,] <- c(1,0,0,0,0) #mid
A1.6[2,] <- c(0,1,0,0,0) #aff
A1.6[3,] <- c(0,0,1,0,0) #lux
A1.6[4,] <- c(0,0,0,1,0) #h_emp
A1.6[5,] <- c(0,0,0,0,1) #income
A1.6[6,] <- c(0,0,0,0,0) #zero-restrictions on interest
A1.6
VEC2_A1.6 <- alrtest(VEC2,A=A1.6,r=r)
summary(VEC2_A1.6)
#=> Ambiguous; Can mostly accept hypothesis of weak exogeneity of interest rates (if l_hemp=l_income=4)

A1 <- matrix(NA,nrow=n,ncol=4)
A1[1,] <- c(1,0,0,0) #mid
A1[2,] <- c(0,1,0,0) #aff
A1[3,] <- c(0,0,1,0) #lux
A1[4,] <- c(0,0,0,0) #zero-restrictions on h_emp
A1[5,] <- c(0,0,0,1) #income
A1[6,] <- c(0,0,0,0) #zero-restrictions on interest

VEC2_A1 <- alrtest(VEC2,A=A1,r=r)
summary(VEC2_A1)
#=> Cannot accept hypothesis of weak exogeneity of all our exogenous variables

###JOHANSEN ML:  ON BETA
#!!!!!!!!!!!!!! NEED TO UPDATE WITH STRUCTURAL BREAKS
##############################################
# Likelihood ratio test for restrictions on beta
#blrtest(z,H,r) for specific restrictions on all cointegration vectors, e.g. beta=(1,-1,*,*,*) in all cointgration relations
#bh5lrtest(z,H,r), which assumes that one cointgration relation is known while the others have to be estimated e.g. beta_1=(1,-1,0,0,0)
#bh6lrtest(z,H,r,r1) test for the existence of one more general cointegration vector between a subset of variables, e.g. in the form (a,b,0,0,0)

# 1) co-integration relationship between mid, aff and lux (H6)
B1.1 <- matrix(NA,nrow=n+1,ncol=3) # (n x s matrix)
B1.1[1,] <- c(1,0,0) #mid
B1.1[2,] <- c(0,1,0) #aff
B1.1[3,] <- c(0,0,1) #lux
B1.1[4,] <- c(0,0,0) #zero-restrictions on h_emp
B1.1[5,] <- c(0,0,0) #zero-restrictions on income
B1.1[6,] <- c(0,0,0) #zero-restrictions on interest
B1.1[7,] <- c(0,0,0) #zero-restrictions on trend

VEC2_B1.1 <- bh6lrtest(VEC2,H=B1.1,r=r,r1=1)
summary(VEC2_B1.1)
#=> Must reject the existence of a cointegration relationships between mid, aff and lux

# 2) co-integration relationship between mid and explanatories  (H6)
B1.2 <- matrix(NA,nrow=n+1,ncol=4) # (n x s matrix)
B1.2[1,] <- c(1,0,0,0) #mid
B1.2[2,] <- c(0,0,0,0) #zero-restrictions on aff
B1.2[3,] <- c(0,0,0,0) #zero-restrictions on lux
B1.2[4,] <- c(0,1,0,0) #h_emp
B1.2[5,] <- c(0,0,1,0) #income
B1.2[6,] <- c(0,0,0,1) #interest
B1.2[7,] <- c(0,0,0,0) #zero-restrictions on trend

VEC2_B1.2 <- bh6lrtest(VEC2,H=B1.2,r=r,r1=1)
summary(VEC2_B1.2)
#=> Must reject the existence of a cointegration relationships between mid and explanatories
#WHY??
test <- cbind(hp$mid, hp.lag8$h_emp, hp.lag4$income, hp$morrate)
colnames(test) <- c("mid","hemp","income","interest")
VECtest <- ca.jo(test, ecdet="none", K=k)
summary(VECtest)

###JOHANSEN ML: RESTRICTIONS ON ALPHA AND BETA
##############################################
I = diag(n)

VEC_A1B1 <- ablrtest(VEC2,H=I,A=A1,r=2)
summary(VEC_A1B1)


###CLEANUP
##############################################
rm(A1,A1.1,A1.2,A1.3,A1.4,A1.5,A1.6)
rm(B1,B1.1,B1.2)
rm(alpha,beta,lambda,)
rm(coint1,coint2,coint3)
rm(dummies)
rm(end,exo,enex)
rm(k,k_aic,K_sic,k_hq)
rm(n,r)
rm(var)

