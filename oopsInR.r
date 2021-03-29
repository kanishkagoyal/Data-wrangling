
# **************** Question 1 ******************

setRefClass("myDataframe",
            fields = list(
              my_df = "data.frame")
            )

all_files = list.files(pattern="*.txt")

reference_to_dfs = list()

for (i in 1:(length(all_files))){
  reference_to_dfs[[i]] <- assign(paste0("df_",substr(all_files[[i]],1,10)), 
           new("myDataframe",
               my_df = read.csv(all_files[i],
                                header = FALSE)))
  
  }

# **************** Question 2 ******************

marker_indices <- list()

#finding all data markers

for( i in 1:length(reference_to_dfs)){
  marker_indices[[i]] <- which(reference_to_dfs[[i]]$my_df$V1 == "Sample") 
  marker_indices[[i]] <- append(marker_indices[[i]],nrow(reference_to_dfs[[i]]$my_df))
}

#finding start and end marker for largest data set

for( i in 1:length(reference_to_dfs)){
  largest_datset <- start_index <- end_index <- 0L
  
  for( j in length(marker_indices[[i]]):2){
    predicted_large <- marker_indices[[i]][j] - marker_indices[[i]][j-1]
    if( largest_datset < predicted_large ){
      largest_datset = predicted_large
      start_index = marker_indices[[i]][j-1]
      end_index = marker_indices[[i]][j]-1
    }
  }
  
  marker_indices[[i]] <- c(start_index,end_index)
}

# **************** Question 3 ******************

for(i in 1:length(reference_to_dfs)){
  names(reference_to_dfs[[i]]$my_df) <- as.character(unlist(
    tolower(reference_to_dfs[[i]]$my_df[marker_indices[[i]][1],])))
}

#testing------------------------------
print(names(reference_to_dfs[[11]]$my_df))
for(i in 1:length(reference_to_dfs)){
  print(names(reference_to_dfs[[i]]$my_df))
}

# **************** Question 4 ******************

for(i in 1:length(reference_to_dfs)){
  reference_to_dfs[[i]]$my_df <- reference_to_dfs[[i]]$my_df[,c("sample","gx","gy","gz")]
}

# **************** Question 5 ******************

new_references <- list()

for(i in 1:length(reference_to_dfs)){
  start_index <- marker_indices[[i]][1] + 1
  end_index <- marker_indices[[i]][2]
  select_data <- reference_to_dfs[[i]]$my_df[start_index:end_index, ]
  new_references[[i]] <- assign(paste0("new_df_",substr(all_files[[i]],1,10)), 
             new("myDataframe",
                 my_df = select_data))
  row.names(new_references[[i]]$my_df) <- NULL
}


# **************** Question 6 ******************

rm(list = ls()[-grep("new", ls())])


# **************** Question 7 ******************

#test
for(i in 1:length(new_references)){
  print(nrow(new_references[[i]]$my_df))
}
# code
for(i in 1:length(new_references)){
  na.omit(new_references[[i]]$my_df)
  for(j in 1:4){
    for( k in 1:nrow(new_references[[i]]$my_df)){
      if(new_references[[i]]$my_df[k,j] == ""){
        new_references[[i]]$my_df <- new_references[[i]]$my_df[-k,]
      }
    }
  }
  row.names(new_references[[i]]$my_df) <- NULL
}
#test
for(i in 1:length(new_references)){
  print(nrow(new_references[[i]]$my_df))
}

# **************** Question 8 ******************

for(i in 1:length(new_references)){
  for(j in 1:length(new_references[[i]]$my_df)){
    vec_logical <- grepl('[[:alpha:]]', new_references[[i]]$my_df[[j]])
    for(x in 1:length(vec_logical)){
      if(vec_logical[x] == TRUE)
        new_references[[i]]$my_df <- new_references[[i]]$my_df[-x,]
    }
  }
  row.names(new_references[[i]]$my_df) <- NULL
}

for(i in 1:length(new_references)){
  for(j in 1:length(new_references[[i]]$my_df)){
    if(class(new_references[[i]]$my_df[[j]]) != "numeric"){
      new_references[[i]]$my_df[[j]] <- as.numeric(new_references[[i]]$my_df[[j]])
    }
  }
  row.names(new_references[[i]]$my_df) <- NULL
}

# **************** Question 9 ******************

for(i in 1:length(new_references)){
    top_10 <- as.integer(nrow(new_references[[i]]$my_df)/5)
    for( j in 2:length(new_references[[i]]$my_df)){
      mean_first_rest <- as.integer(mean(new_references[[i]]$my_df[1:top_10,j]))
      for(k in 1:nrow(new_references[[i]]$my_df)){
        new_references[[i]]$my_df[k,j] <- new_references[[i]]$my_df[k,j] - mean_first_rest
      }
    }
}

# **************** Question 10 ******************

dataframe_for_each_subject <- function(x){
  result_df <- new_references[[x]]$my_df
  for(i in (x+1):(x+3)){
    result_df <- rbind(result_df,new_references[[i]]$my_df)
  }
  result_df$Sample <- seq.int(nrow(result_df))
  return(result_df)
}

index <- 1
for( i in seq(1,length(new_references), by = 4)){
  assign(paste0("subj_",index,"_df"), dataframe_for_each_subject(i))
  index <- index + 1
}

graphics.off()
par(mfrow=c(3,1))

plot(subj_1_df$Sample,
     subj_1_df$gX, type = "l", col = "red",
     xlim = c(1,5000), ylim = c(-10000,10000),xlab = "Sample",
     ylab = "Rotational Velocity")
lines(subj_1_df$Sample,subj_1_df$gY, col = "green")
lines(subj_1_df$Sample,subj_1_df$gz, col = "blue")
legend("topleft",legend=c("gX", "gY", "gz"), lty = 1,
       col=c("red", "green", "blue"), cex=0.6)


plot(subj_2_df$Sample,
     subj_2_df$gX, type = "l", col = "red",
     xlim = c(1,5000),ylim = c(-10000,10000), xlab = "Sample",
     ylab = "Rotational Velocity")
lines(subj_2_df$Sample,subj_2_df$gY, col = "green")
lines(subj_2_df$Sample,subj_2_df$gz, col = "blue")
legend("topleft",legend=c("gX", "gY", "gz"), lty = 1,
       col=c("red", "green", "blue"), cex=0.6)



plot(subj_3_df$Sample,
     subj_3_df$gX, type = "l", col = "red",
     xlim = c(1,5000),ylim = c(-10000,10000), xlab = "Sample",
     ylab = "Rotational Velocity")
lines(subj_3_df$Sample,subj_3_df$gY, col = "green")
lines(subj_3_df$Sample,subj_3_df$gz, col = "blue")
legend("topleft",legend=c("gX", "gY", "gz"), lty = 1,
       col=c("red", "green", "blue"), cex=0.6)












                         
                         