y <- data.frame(matrix(data = 0, nrow = 4, ncol = 4))
#View(y)
rownames(y) <- c('Civic', 'Saturn', 'Ford', 'Mazda');
colnames(y) <- c('Style', 'Reliability', 'FuelEco', 'Cost');
y['Civic',] <- c(7, 9, 9, 8)
y['Saturn',] <- c(8, 7, 8, 7)
y['Ford',] <- c(9, 6, 8, 9)
y['Mazda',] <- c(6, 7, 8, 6)
CWV <- c(0.1, 0.4, 0.3, 0.2);
y_norm <- data.frame(matrix(data = 0, nrow = 4, ncol = 4));
for ( i in c(1:4) )
{   y_norm[,i]<-y[,i]/sqrt(sum(y[,i]^2));
}
windma <- data.frame(matrix(data = 0, nrow = 4, ncol = 4));
for( j in c(1:4) )
{   windma[,j]<-y_norm[,j]*CWV[j];
}
# Determine Ideal
I<-c(0,0,0,0);
I[1]<-max(windma[,1]);
I[2]<-max(windma[,2]);
I[3]<-max(windma[,3]);
I[4]<-min(windma[,4]);
# Determine Anti-Ideal
V<-c(0,0,0,0);
V[1]<-min(windma[,1]);
V[2]<-min(windma[,2]);
V[3]<-min(windma[,3]);
V[4]<-max(windma[,4]);

# Sep. Measure to Ideal
S_star<-c(0,0,0,0);
for(j in c(1:4))
{  S_star[j]<-sqrt(sum((I-windma[j,])^2));
}
S_star;  #S* 到正理想解之間的距離

# Sep. Measure to Anti-Ideal
S_plum<-c(0,0,0,0);
for(j in c(1:4))
{  S_plum[j]<-sqrt(sum((V-windma[j,])^2));
}
S_plum;  #S' 到負理想解之間的距離

#Ci* 接近係數: S‘i / (Si* +S’i )   
RCIV <- S_plum/(S_star+S_plum)
RCIV

#Give Results
RCIV2 <- as.data.frame(RCIV)
rownames(RCIV2)<-rownames(y)


#Rank the Alternatives
ODR<-order(RCIV2, decreasing=TRUE);
RCIV2$CarName<-rownames(RCIV2);  #把CarName放到rownames
RCIV3<-RCIV2[ODR, ];  #根據ODR再order一次
RCIV3$RankOrder<-seq(1,4,1);
RCIV3


  
