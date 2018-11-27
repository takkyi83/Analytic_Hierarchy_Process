# ANP (Analytic network process)
## Airport Accessibility 
library(gdata)
#MAR
M <- diag(7);
M[1,2]<-1/7;
M[1,3]<-1/3;
M[1,4]<-1/3;
M[1,5]<-1;
M[1,6]<-1/7;
M[1,7]<-1/2;
M[2,3]<-5;
M[2,4]<-5;
M[2,5]<-5;
M[2,6]<-3;
M[2,7]<-5;
M[3,4]<-2;
M[3,5]<-2;
M[3,6]<-1/5;
M[3,7]<-3;
M[4,5]<-3;
M[4,6]<-1/5;
M[4,7]<-1;
M[5,6]<-1/5;
M[5,7]<-2;
M[6,7]<-5;
lowerTriangle(M) <- 1/upperTriangle(M, byrow=TRUE);
M;
# Sum the columns
V<-c(0,0,0,0,0);
V<-colSums(M);
V;
# Divide the sum of the columns back to M
MP<-matrix(0,7,7);
MP[,1]<-M[,1]/V[1];
MP[,2]<-M[,2]/V[2];
MP[,3]<-M[,3]/V[3];
MP[,4]<-M[,4]/V[4];
MP[,5]<-M[,5]/V[5];
MP[,6]<-M[,6]/V[6];
MP[,7]<-M[,7]/V[7];

MP;

# Compute the average of rows, to obtain the CWV
CWV<-c(0,0,0,0,0,0,0);
CWV<-rowMeans(MP);
CWV;


library(gdata)
#ACC
M <- diag(7);
M[1, 2]<-3
M[1, 3]<-0.33
M[1, 4]<-0.2
M[1, 5]<-0.2
M[1, 6]<-3
M[1, 7]<-2
M[2, 3]<-0.2
M[2, 4]<-0.2
M[2, 5]<-0.2
M[2, 6]<-0.5
M[2, 7]<-0.33
M[3, 4]<-2
M[3, 5]<-2
M[3, 6]<-5
M[3, 7]<-3
M[4, 5]<-0.5
M[4, 6]<-5
M[4, 7]<-3
M[5, 6]<-5
M[5, 7]<-3
M[6, 7]<-0.33
lowerTriangle(M) <- 1/upperTriangle(M, byrow=TRUE);
M;
#ACC
# Sum the columns
V<-c(0,0,0,0,0,0,0);
V<-colSums(M);
V;
# Divide the sum of the columns back to M
MP<-matrix(0,7,7);
MP[,1]<-M[,1]/V[1];
MP[,2]<-M[,2]/V[2];
MP[,3]<-M[,3]/V[3];
MP[,4]<-M[,4]/V[4];
MP[,5]<-M[,5]/V[5];
MP[,6]<-M[,6]/V[6];
MP[,7]<-M[,7]/V[7];
MP;
# Compute the average of rows, to obtain the CWV
CWV1<-c(0,0,0,0,0,0,0);
CWV1<-rowMeans(MP);
CWV1;

#COS
COS <- diag(7)
COS[1,2]=1/6
COS[1,3]=1/7
COS[1,4]=5
COS[1,5]=1/7
COS[1,6]=1/9
COS[1,7]=7
COS[2,3]=1/3
COS[2,4]=7
COS[2,5]=3
COS[2,6]=1/3
COS[2,7]=7
COS[3,4]=7
COS[3,5]=5
COS[3,6]=1/2
COS[3,7]=8
COS[4,5]=1/8
COS[4,6]=1/6
COS[4,7]=3
COS[5,6]=1/5
COS[5,7]=9
COS[6,7]=7
lowerTriangle(COS) <- 1/upperTriangle(COS,byrow = T)
COS
#cwv
CosV <- c(0,0,0)
CosV <- colSums(COS)
CosV

CosMP <- matrix(0,7,7)
for(i in c(0:7))
{
  CosMP[,i] <- COS[,i]/CosV[i]
}
CosMP

CosCWV2 <- c(0,0,0) 
CosCWV2<- rowMeans(CosMP)     
CosCWV2

#CAP
M <- diag(7);
M[1,2] = 1/2
M[1,3] = 2
M[1,4] = 2
M[1,5] = 1/3
M[1,6] = 5
M[1,7] = 1/5
M[2,3] = 3
M[2,4] = 2
M[2,5] = 1/3
M[2,6] = 5
M[2,7] = 4
M[3,4] = 2
M[3,5] = 1/5
M[3,6] = 5
M[3,7] = 1/7
M[4,5] = 1/3
M[4,6] = 5
M[4,7] = 5
M[5,6] = 5
M[5,7] = 1/3
M[6,7] = 1/9


lowerTriangle(M) <- 1/upperTriangle(M, byrow=TRUE);
M;

# Sum the columns
V<-colSums(M);
V;
# Divide the sum of the columns back to M
MP<-matrix(0,7,7);
MP[,1]<-M[,1]/V[1];
MP[,2]<-M[,2]/V[2];
MP[,3]<-M[,3]/V[3];
MP[,4]<-M[,4]/V[4];
MP[,5]<-M[,5]/V[5];
MP[,6]<-M[,6]/V[6];
MP[,7]<-M[,7]/V[7];
MP;
# Compute the average of rows, to obtain the CWV
CWV3<-c(0, 0, 0, 0, 0, 0, 0);
CWV3<-rowMeans(MP);
CWV3;

S<- diag(4);
S[1,2] = 4
S[1,3] = 1/2
S[1,4] = 2
S[2,1] = 1/4
S[2,3] = 1/4
S[2,4] = 1/4
S[3,4] = 4
lowerTriangle(S) <- 1/upperTriangle(S, byrow=TRUE);
S
# Sum the columns
V<-c(0,0,0,0);
V<-colSums(S);
V;
# Divide the sum of the columns back to M
SP<-matrix(0,4,4);
SP[,1]<-S[,1]/V[1];
SP[,2]<-S[,2]/V[2];
SP[,3]<-S[,3]/V[3];
SP[,4]<-S[,4]/V[4];
SP;

# Compute the average of rows, to obtain the CWV
CWV4<-c(0, 0, 0, 0);
CWV4<-rowMeans(SP);
CWV4;

CWV
CWV1
CosCWV2
CWV3
X=rbind(CWV,CWV1,CosCWV2,CWV3)
colnames(X)=c('A1','A2','A3','A4','A5','A6','A7')
X

SI = t(X)%*%CWV4

SI

rank(-SI)
