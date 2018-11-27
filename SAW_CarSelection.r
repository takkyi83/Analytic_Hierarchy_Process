##############################
# SAW (Simple Additive Weighting) Test for Car Selection
##############################

# Create empty data frame
y <- data.frame(matrix(data=0, ncol = 3, nrow = 4));
colnames(y) <- c("Style","Reliability","FuelEco");
rownames(y) <- c("Civic","Saturn","Ford","Mazda");
str(y);

# Assign atrribute values to each alternative row
y["Civic",] <- c(7,9,9);
y["Saturn",] <- c(8,7,8);
y["Ford",] <- c(9,6,8);
y["Mazda",] <- c(6,7,8);
y;

# Append a column to store the SAW-ed scores for each alternative
SAW_score_vector <- c(0,0,0,0);
y<-cbind(y,SAW_score_vector);
y;

# Setup up the weight vector (from my DM's mind)
SAW_weight_vector <- c(0.3,0.4,0.3);

# Compute the weighted aggregated scores for each alternative
SAW_weight_vector * y["Civic",];
y["Civic","SAW_score_vector"] <- sum (SAW_weight_vector * y["Civic",]);
y;

SAW_weight_vector * y["Saturn",];
y["Saturn","SAW_score_vector"] <- sum (SAW_weight_vector * y["Saturn",]);
y;

SAW_weight_vector * y["Ford",];
y["Ford","SAW_score_vector"] <- sum (SAW_weight_vector * y["Ford",]);
y;

SAW_weight_vector * y["Mazda",];
y["Mazda","SAW_score_vector"] <- sum (SAW_weight_vector * y["Mazda",]);
y;

# Rank them!
SAW_rank_vector <- order(-y$SAW_score_vector);
y<-cbind(y,SAW_rank_vector);
y;
