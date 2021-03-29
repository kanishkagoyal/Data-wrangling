

#****************** Question: 1*************************************************

files <- list.files(pattern = "*txt")
dataframe_list <-  list()
for (f in 1:length(files)) {
  dataframe_list[[f]] <-
    paste0('df_', substr(files[f], 1, 10), sep = "")
  
  assign(dataframe_list[[f]],
         read.delim(
           files[f],
           row.names = NULL,
           header = F,
           sep = ","
         ))
  
}

#comments - Reading all data files into dataframes, at the end we will have
#           12 dataframes and a list containing "names" of dataframes

#****************** Question: 2*************************************************

start_index_array_list <- list()
end_index_array_list <- list()

index <- 1

#comments - Retrieving Data Markers from each dataframe and storing it in start 
#           and end index array list  

for (df in dataframe_list) {
  temp <- which((get(df))$V1 == "Sample")
  start_index_array_list[[index]] <- temp
  end_index_array_list[[index]] <- append(temp, nrow(get(df)))
  for (j in 2:length(temp)) {
    end_index_array_list[[index]][j] <-
      end_index_array_list[[index]][j] - 1
  }
  end_index_array_list[[index]] <-
    end_index_array_list[[index]][2:(length(temp) + 1)]
  index <- index + 1
}


#comments - Function definition to retrieve max difference from indices, this  
#           will be called from question 5  

getmaxDifferenceAndIndex <- function(start_marker, end_marker) {
  difference <- tempDifference <- 0L
  for (i in 1:length(start_marker)) {
    difference <- max(difference, end_marker[i] - start_marker[i])
    if (difference > tempDifference) {
      my_list <- c(start_marker[i], end_marker[i])
    }
    tempDifference = difference
  }
  return(my_list)
}


#****************** Question: 3*************************************************

#Comments - Using for loop renaming column names of dataframes 
#           using data markers

i <- 1
for (df in dataframe_list) {
  columnName <-
    as.character(unlist(get(df)[start_index_array_list[[i]][1],]))
  columnName <- tolower(columnName)
  #As gz has spaces at last, we want to trim it and assign column-8 as "gz"
  columnName[8] <- "gz"  
  assign(df, setNames(get(df),  columnName))
  i <- i + 1
}


#****************** Question: 4*************************************************

#Comments - Subsetting only gyroscope data i.e four columns

for (df in dataframe_list) {
  assign(df, subset(get(df), select = c("sample", "gx", "gy", "gz")))
}


#****************** Question: 5*************************************************

#Comments - Using question-2's function getmaxDifferenceAndIndex, getting maximum 
#           of data markers, and creating separate dataframes.

new_dataframe_list <- list()

for (f in 1:length(files)) {
  marker_output <-
    getmaxDifferenceAndIndex(start_index_array_list[[f]],
                             end_index_array_list[[f]])
  start_marker <- marker_output[1]
  end_marker <- marker_output[2]
  new_dataframe_list[[f]] <- paste0('new_', dataframe_list[[f]])
  assign(new_dataframe_list[[f]], get(dataframe_list[[f]])[(start_marker +
                                                              1):end_marker,])
}

#****************** Question: 6*************************************************

#Comments - Remove every dataframe/variables but new i.e keeping dataframes which 
#           are created in Question-5 

rm(list = ls()[-grep("new", ls())])

#****************** Question: 7*************************************************

#Comments - Removing empty cells or NAs from each dataframe

delete.dirt <- function(DF) {
  DF[DF == ""] <- NA
  DF[DF == " "] <- NA
  DF <- na.omit(DF)
}

for (new_df in new_dataframe_list)
{
  assign(new_df, delete.dirt(get(new_df)))
  
}

#****************** Question: 8*************************************************

#Comments - Function to remove special characters from each row (wherever present)
#           and in each dataframe

removeSpecialChars <- function(DF) {
  for (j in 1:length(DF)) {
    vec_logical <- grepl('[[:alpha:]]', DF[, j])
    for (x in 1:length(vec_logical)) {
      if (vec_logical[x] == TRUE) {
        DF <- DF[-x,]
      }
    }
  }
  return(DF)
}

#Comments - Function to check and convert each variable as numeric in 
#           each dataframe 

convertToNumeric <- function(DF) {
  DF <-
    data.frame(apply(DF, 2, function(x) {
      if (class(x) != "numeric") {
        as.numeric(x)
      }
    }))
}

for (df in new_dataframe_list) {
  assign(df, removeSpecialChars(get(df)))
  assign(df, convertToNumeric(get(df)))
}

#****************** Question: 9*************************************************

#Comment - Function to get ten seconds of data from each dataframe and 
#          removing offset using mean of 10second data from each variable of 
#          each dataframe

getRestData <- function(DF) {
  ten_second <- round(nrow(DF) / 5)
}

for (df in new_dataframe_list) {
  tempdf <- get(df)
  first_rest <- getRestData(tempdf)
  for (j in 2:length(tempdf)) {
    tempMean <- mean(tempdf[1:first_rest, j])
    tempdf[, j] <-  tempdf[, j] - tempMean
  }
  assign(df, tempdf)
}

#****************** Question: 10************************************************

#Comments - This function will clean Plot/Graph area

removePreviousGraphs <- function() {
  try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
  try(dev.off(), silent = TRUE)
  op <- par(no.readonly = TRUE)
}

#Comments - Function will set dimensions of the plot
setPlotDimensions <- function(width, height) {
  options(repr.plot.width=width, repr.plot.height=height)
}


#Comments - Function to plot graph for each subject as per its activities
plot_activities <- function(df1, df2, df3, df4, graphName, height) {
  removePreviousGraphs()
  setPlotDimensions(16, 4)
  plot(
    df4$sample,
    y = df4$gx,
    xlab = "",
    ylab = "",
    type = "l",
    col = "white",
    xlim = c(0, 4000)
  )
  abline(h = height, col = "purple")
  abline(v = 1000, col = "grey")
  abline(v = 2200, col = "grey")
  abline(v = 3200, col = "grey")
  title(main = graphName)
  legend(
    "topleft",
    lty = 1, cex=0.6,
    col = c("green", "red", "blue"),
    legend = c("gx", "gy", "gz")
  )
  par(new = TRUE)
  par(oma = c(1, 4, 5, 1))
  par(mfcol = c(1, 4), mfg = c(1, 1))
  par(mar = c(12, 5, 1, 1) + 0.1)
  setPlotDimensions(4, 4)
  plot(
    df1$sample,
    y = df1$gx,
    xlab = "Activity1",
    ylab = "",
    type = "l",
    col = "green",
    axes = F,
    xlim = c(0, 4000),
    ylim = c(-10000, 30000)
  )
  lines(df1$sample, y = df1$gy, col = "red")
  lines(df1$sample, y = df1$gz, col = "blue")
  setPlotDimensions(4, 4)
  plot(
    df2$sample,
    y = df2$gx,
    type = "l",
    col = "green",
    xlab = "Activity2",
    ylab = "",
    axes = F,
    xlim = c(0, 4000),
    ylim = c(-10000, 30000)
  )
  lines(df2$sample, y = df2$gy, col = "red")
  lines(df2$sample, y = df2$gz, col = "blue")
  setPlotDimensions(4, 4)
  plot(
    df3$sample,
    y = df3$gx,
    type = "l",
    col = "green",
    xlab = "Activity3",
    ylab = "",
    axes = F,
    xlim = c(0, 4000),
    ylim = c(-10000, 30000)
  )
  lines(df3$sample, y = df3$gy, col = "red")
  lines(df3$sample, y = df3$gz, col = "blue")
  setPlotDimensions(4, 4)
  plot(
    df4$sample,
    y = df4$gx,
    type = "l",
    col = "green",
    xlab = "Activity4",
    ylab = "",
    axes = F,
    xlim = c(0, 4000),
    ylim = c(-10000, 30000)
  )
  lines(df4$sample, y = df4$gy, col = "red")
  lines(df4$sample, y = df4$gz, col = "blue")
}

#Comments - Expand plot to get better visibility
#Comment - Plot for Subject - 1
plot_activities(
  new_df_subj1_act1,
  new_df_subj1_act2,
  new_df_subj1_act3,
  new_df_subj1_act4,
  "Subject-1",
  4200
)
#Comment - Plot for Subject - 2
plot_activities(
  new_df_subj2_act1,
  new_df_subj2_act2,
  new_df_subj2_act3,
  new_df_subj2_act4,
  "Subject-2",-4250
)

#Comment - Plot for Subject - 3
plot_activities(
  new_df_subj3_act1,
  new_df_subj3_act2,
  new_df_subj3_act3,
  new_df_subj3_act4,
  "Subject-3",-1190
)




#****************** Question: 11 *************************************************

#Comments - Function to get 30 seconds of activity data 
removeRestData <- function(DF) {
  firstLast10Sec <- round(nrow(DF) / 5)
  DF <- DF[-(1:firstLast10Sec), , drop = FALSE]
  n <- dim(DF)[1]
  DF <- DF[1:(n - firstLast10Sec), ]
}

i <- 1
for (df in new_dataframe_list) {
  assign(df,  removeRestData(get(df)))
  i <- i + 1
}

#****************** Question: 12 **********************************************

#Comments - One list to have all subject, each subject to have data as per their
#           activities and further divided into matrices 


list_all_activities <- list()

#Comments - To combine matrices for each activity
for (df in new_dataframe_list) {
  total_data <- nrow(get(df))
  list_each_activity <- list()
  five_second <- round((total_data) / 10)
  start_index <- 1
  end_index <- five_second
  
  i <- 1
  while (total_data > 0) {
    name_for_matrices <- paste0('matrix_', i)
    list_each_activity[[name_for_matrices]] <-
      as.matrix(get(df)[start_index:end_index, 2:4])
    start_index <- end_index + 1
    end_index <- end_index + five_second
    total_data <- total_data - five_second
    i <- i + 1
  }
  name <- substr(df, 8, 18)
  list_all_activities[[name]] <- list_each_activity
}

#Comments - To Combine activities for each subjects
sub_index <- 1
for (j in seq(1, length(list_all_activities), by = 4)) {
  assign(paste0("subject_", sub_index), list_all_activities[j:(j + 3)])
  sub_index <- sub_index + 1
}

#Comments - To combine all subjects into one
list_all_subjects <- list("subject_1" = subject_1,
                          "subject_2" = subject_2,
                          "subject_3" = subject_3)

#****************** Question: 13 **********************************************

#Comments - Create mean and standard deviation for each 
#           activity and for each participant within each window

for (i in 1:4) {
  assign(
    paste0("meanAct", i, "DataFrame", sep = ""),
    data.frame(
      gx = numeric(),
      gy = numeric(),
      gz = numeric()
    )
  )
  assign(
    paste0("sdAct", i, "DataFrame", sep = ""),
    data.frame(
      gx = numeric(),
      gy = numeric(),
      gz = numeric()
    )
  )
}

#Comments - get all subject according to activity 
for (subject in names(list_all_subjects)) {
  subject_name <- list_all_subjects[subject]
  for (i in 1:4) {
    assign(paste(subject, "act" , i, sep = ""), subject_name[[1]][[i]])
  }
  listOfActs <-
    c(
      paste(subject, "act1", sep = ""),
      paste(subject, "act2", sep = ""),
      paste(subject, "act3", sep = ""),
      paste(subject, "act4", sep = "")
    )
  
  #Comments - Using these activities from each matrices get Mean and SD
  for (act in listOfActs) {
    for (act_matrix in 2:length(get(act)) - 1) {
      metrix <- get(act)[[act_matrix]]
      
      meangx <- mean(metrix[, 1])
      sdgx <- sd(metrix[, 1])
      meangy <- mean(metrix[, 2])
      sdgy <- sd(metrix[, 2])
      meangz <- mean(metrix[, 3])
      sdgz <- sd(metrix[, 3])
      
      #Comments - Combining all mean and standard deviation w.r.t activities
      tempMeanList <- list(gx = meangx, gy = meangy, gz = meangz)
      tempSDList <- list(gx = sdgx, gy = sdgy, gz = sdgz)
      if (grepl("act1", act)) {
        meanAct1DataFrame <- rbind(meanAct1DataFrame, tempMeanList)
        sdAct1DataFrame <- rbind(sdAct1DataFrame, tempSDList)
      } else if (grepl("act2", act)) {
        meanAct2DataFrame <- rbind(meanAct2DataFrame, tempMeanList)
        sdAct2DataFrame <- rbind(sdAct2DataFrame, tempSDList)
      } else if (grepl("act3", act)) {
        meanAct3DataFrame <- rbind(meanAct3DataFrame, tempMeanList)
        sdAct3DataFrame <- rbind(sdAct3DataFrame, tempSDList)
      } else if (grepl("act4", act)) {
        meanAct4DataFrame <- rbind(meanAct4DataFrame, tempMeanList)
        sdAct4DataFrame <- rbind(sdAct4DataFrame, tempSDList)
      }
    }
  }
}


#****************** Question: 14 **********************************************

#Comment - Plot means of each activity by combining all the subjects

plotMean <- function(df1, df2, df3, df4){
  removePreviousGraphs()
  plot(df1[[1]],y = df1[[2]],
       xlab = "",ylab = "",type = "l",col = "white",las = 2,cex.axis = 1)
  title(main = "Mean for Activities")
  legend("bottomright", pch = 0,  col = c("green", "red", "blue"),
         legend = c("X", "Y", "Z"), cex=0.6)
  par(new = TRUE) 
  par(oma = c(1, 4, 5, 1)) 
  par(mfcol = c(1, 4), mfg = c(1, 1))
  par(mar = c(11, 2, 1, 3) + 0.1)
  boxplot(df1, col = c("green", "red", "blue"),axes = F, xlab = "Act1")
  boxplot(df2, col = c("green", "red", "blue"),axes = F, xlab = "Act2")
  boxplot(df3, col = c("green", "red", "blue"),axes = F, xlab = "Act3")
  boxplot(df4, col = c("green", "red", "blue"),axes = F, xlab = "Act4")
}

plotMean(meanAct1DataFrame, meanAct2DataFrame, meanAct3DataFrame, meanAct4DataFrame)
#Analysis - Mean of Activity-1 will be highest and Activity-4 is lowest. 
#           Activity-1 has highest because it is sitting 
#           which hasn't been combined with anything else.
#           Activity-1 has largest spread and Activity-3,4 have lower spreads. 

#Comment - Plot standard deviation of each activity by combining all the subjects 

plotSD <- function(df1, df2, df3, df4){
  removePreviousGraphs()
  plot(df1[[1]],y = df1[[2]], xlab = "",  ylab = "",
       type = "l",  col = "white",  las = 2,  cex.axis = 1)
  title(main = "Standard deviation for Activities ")
  legend( "bottomright",  pch = 0,  col = c("green", "red", "blue"),
          legend = c("X", "Y", "Z"), cex = 0.6)
  par(new = TRUE)
  par(oma = c(1, 4, 5, 1))
  par(mfcol = c(1, 4), mfg = c(1, 1))
  par(mar = c(11, 2, 1, 3) + 0.1)
  boxplot(df1,col = c("green", "red", "blue"),axes = F,xlab = "Act1")
  boxplot(df2,col = c("green", "red", "blue"),axes = F,xlab = "Act2")
  boxplot(df3,col = c("green", "red", "blue"),axes = F, xlab = "Act3")
  boxplot(df4,col = c("green", "red", "blue"),axes = F, xlab = "Act4")
}

plotSD(sdAct1DataFrame, sdAct2DataFrame, sdAct3DataFrame, sdAct4DataFrame)

#Analysis - Standard Deviation of Activity-1 is smallest and has less number of outliers. 
#           Whereas Activity-2 and 4 have larger values of SD   
#           Activity -3 has multiple outliers for each co-ordinate. 

#**************************** END **********************************************