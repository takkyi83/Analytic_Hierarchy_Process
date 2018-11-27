```R
# My First AHP Program
library(gdata);
M <- diag(3);
M[2,3]<-1/3;
M[1,3]<-3;
M[1,2]<-5;
lowerTriangle(M) <- 1/upperTriangle(M, byrow=TRUE);
M;
# Sum the columns
V<-c(0,0,0);
V<-colSums(M);
V;
# Divide the sum of the columns back to M
MP<-matrix(0,3,3);
MP[,1]<-M[,1]/V[1];
MP[,2]<-M[,2]/V[2];
MP[,3]<-M[,3]/V[3];
MP;
# Compute the average of rows, to obtain the CWV
CWV<-c(0,0,0);
CWV<-rowMeans(MP);
CWV;

# Use Geometrical Mean to generate the data
CWV2<-c(0,0,0);
CWV2[1]<-(MP[1,1]*MP[1,2]*MP[1,3])^(1/3);
CWV2[2]<-(MP[2,1]*MP[2,2]*MP[2,3])^(1/3);
CWV2[3]<-(MP[3,1]*MP[3,2]*MP[3,3])^(1/3);
CWV2;

# Do the Consistency Analysis (In fact, inconsistency test) 
# to believe that CWV is true based on the true opinion of the DM

# Step 1 (see PPT p.62 step 1)
# Multiply each value in the first column of the pairwise 
# comparison matrix by the relative priority of the first 
# item considered. Same procedures for other items.


# Step 1-1
# Sum the values across the rows to obtain a vector of 
# values labeled “weighted sum.”
V <- M %*% CWV;

# Step 2
# Divide the elements of the vector of weighted sums 
# obtained in Step 1 by the corresponding priority value
X<- V/CWV;

# Step 3
# Compute the average of the values computed in step 2.  
# This average is denoted as Lambda_max.
Lambda_Max <- mean(X);

# Step 4
# Compute the consistency index (CI)
# CI=(Lambda_Max-n)/(n-1)
CI <- (Lambda_Max-3)/(3-1);

# Step 5
# Compute the consistency ratio (CR)
# CR=CI/RI, where RI is an index to be looked up
CR <- CI/0.58;

# Step 6 
# See if acceptable according to CR 
if (CR > 0.1)
{
  print("沒通過一致性檢定");
}
else
{
  print("通過一致性檢定");
}
```
