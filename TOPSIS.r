# TOPSIS (Technique for Order Preference by Similarity to the Ideal Solution) Test  

# Step 0
# Load the car selection problem data as the example
DecisionMatrix<-matrix(0,4,4);
DecisionMatrix[1,]<-c( 49, 81, 81, 64);
DecisionMatrix[2,]<-c( 64, 49, 64, 49);
DecisionMatrix[3,]<-c( 81, 36, 64, 81);
DecisionMatrix[4,]<-c( 36, 49, 64, 36);
DecisionMatrix;

# Step 1: Normalize the decision matrix according to the 
#         standard operation defined by TOPSIS
NumRows<-nrow(DecisionMatrix);
NumCols<-ncol(DecisionMatrix);
D_normalize<-matrix(0,NumRows,NumCols);
for ( j in c(1:NumCols) )
 {   D_normalize[,j]<-DecisionMatrix[,j]/sqrt(sum(DecisionMatrix[1:NumRows,j]^2))
 };
D_normalize;

# Step 2: Setup the criterion weight vector CWV and compute 
#         the weighted (normalized) decision matrix WNDMatrix
CWV<-c(.1,.4,.3,.2);
WNDMatrix<-matrix(0,NumRows,NumCols);
for ( j in c(1:NumCols))
 {   WNDMatrix[,j]<-CWV[j]*D_normalize[,j];
 };
WNDMatrix;

# Step 3: Determine the ideal A_plus and anti-ideal solution
#         A_minus
A_plus<-c(0,0,0,0);
for ( j in c(1:NumCols))
 {   A_plus[j]<-max(WNDMatrix[1:NumRows,j]);
 };
A_plus;
# Give a special process because 4 is TLTB
A_plus[4]<-min(WNDMatrix[1:NumRows,4]);
A_plus;

A_minus<-c(0,0,0,0);
for ( j in c(1:NumCols))
 {   A_minus[j]<-min(WNDMatrix[1:NumRows,j]);
 };
# Give a special process because 4 is TLTB
A_minus[4]<-max(WNDMatrix[1:NumRows,4]);
A_minus;

# Step 4: Calculate the seperation measure for each alternative

# Distances toward ideal solution A_plus
S_to_ideal<-c(1:NumRows);
for( i in c(1:NumRows) )
 {   S_to_ideal[i]<-sqrt( sum(  
                               (WNDMatrix[i,]-A_plus[])^2 
                             )
                        );
 };
S_to_ideal;
# Distances toward anti-ideal solution A_minus
S_to_anti_ideal<-c(1:NumRows);
for( i in c(1:NumRows) )
 {   S_to_anti_ideal[i]<-sqrt( sum(  
                                   (WNDMatrix[i,]-A_minus[])^2 
                                  )
                             );
 };
S_to_anti_ideal;

# Step 5: Calculate the relative closeness (of each alternative)
#         to the ideal solution, which is the Tie-Jin Xi-Shu
RC<-c(1:NumRows);
for( i in c(1:NumRows) )
 {   RC[i]<-S_to_anti_ideal[i]/(S_to_ideal[i]+S_to_anti_ideal[i]);
 }; # Equivalant to: RC<-S_to_anti_ideal/(S_to_ideal+S_to_anti_ideal)
RC;

# Step 6: Rank the preference order (for the alternatives)
#         using data from RC
order(RC, decreasing=TRUE);
RC[order(RC, decreasing=TRUE)];   # Check
