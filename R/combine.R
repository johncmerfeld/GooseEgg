library(stats)
library(data.table)
library(factoextra)
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

find_matches <- function(big_n) {

  raw_data <- fread("~/Documents/Code/R/combine.csv", header = T,
                    na.strings=c("NA","","NULL"))
  names(raw_data) <- c("first", "last", "school", "gyear", "cyear", "pos", "ht",
                       "wt", "forty", "vert", "broad", "ldrill", "shuttle",
                       "breps", "bwt", "ten")
  
  raw_data$height_in_inches <- sapply(strsplit(as.character(raw_data$ht),
                                               "'|\""),
                            function(x){12*as.numeric(x[1]) + as.numeric(x[2])})
  
  raw_data$broad_in_inches <- sapply(strsplit(as.character(raw_data$broad),
                                              "'|\""),
                          function(x){12*as.numeric(x[1]) + as.numeric(x[2])})
  
  fn_frame <- as.data.frame(raw_data)
  
  numeric_cols <- c("forty", "vert", "ldrill", "shuttle", "breps")
  
  fn_frame[numeric_cols] <- sapply(fn_frame[numeric_cols],as.numeric)
  fn_data <- as.data.table(fn_frame)
  fn_data$broad <- NULL
  fn_data$ten <- NULL
  
  # median replacements
  fn_data[is.na(fn_data$forty)]$forty <- median(fn_data$forty, na.rm = TRUE)
  fn_data[is.na(fn_data$vert)]$vert <- median(fn_data$vert, na.rm = TRUE)
  fn_data[is.na(fn_data$wt)]$wt <- median(fn_data$wt, na.rm = TRUE)
  fn_data[is.na(fn_data$ldrill)]$ldrill <- median(fn_data$ldrill, na.rm = TRUE)
  fn_data[is.na(fn_data$broad_in_inches)]$broad_in_inches <- 
    median(fn_data$broad_in_inches, na.rm = TRUE)
  fn_data[is.na(fn_data$height_in_inches)]$height_in_inches <- 
    median(fn_data$height_in_inches, na.rm = TRUE)
  fn_data[is.na(fn_data$shuttle)]$shuttle <- median(fn_data$shuttle, na.rm = TRUE)
  fn_data[is.na(fn_data$breps)]$breps <- median(fn_data$breps, na.rm = TRUE)
  fn_data[is.na(fn_data$bwt)]$bwt <- median(fn_data$bwt, na.rm = TRUE)
  
  fn_data$bench_score <- fn_data$bwt * log(fn_data$breps + 1)
  fn_data$breps <- NULL
  fn_data$bwt <- NULL
  fn_data$ht <- NULL
  
  num_cols <- c("wt", "forty", "vert", "ldrill", "shuttle", "height_in_inches",
                "broad_in_inches", "bench_score")
  num_data <- scale(fn_data[,num_cols, with = F])
  
  dm <- as.data.frame(as.matrix(dist(num_data, upper = F, diag = F)))
  
  my_name <- function(n) {
    return(paste(fn_data[n,]$first,
                  fn_data[n,]$last))
  }
  buddy_name <- function(n) {
    row <- dm[n,]
    for (i in 1:length(row)) {
      if (row[[i]] == 0) {
        row[[i]] = 1000
      }
    }
    return(paste(x[which.min(row)]$first,
                 x[which.min(row)]$last))
  }
  
  n_best_buddies <- function(x, num_buddies, dist_mat) {
    dist_mat <- as.data.frame(dist_mat)
    buddy_vector <- c()
    for (i in 1:nrow(x)) {
      #print(i)
      col <- dist_mat[order(dist_mat[,i]),i]
      buddy_string <- ""
      for (j in 2:(1+num_buddies)) {
        buddy_string <- paste0(buddy_string,
                               x[which(dist_mat[,i] == col[j])]$first, " ",
                               x[which(dist_mat[,i] == col[j])]$last, ", ")
      }
      buddy_string <- gsub('.{2}$', '', buddy_string)
      buddy_vector[i] <- buddy_string
    }
    return(buddy_vector)
  }
  
  output_data <- raw_data
  output_data$ordered_matches <- n_best_buddies(fn_data, big_n, dm)
  write.csv(output_data,
            file = paste0("combine_results_", big_n, "_matches.csv"),
            row.names = F,
            col.names = T)
  
}

for (i in 1:10) {
  find_matches(i)
}

#km <- kmeans(num_data, centers = 8, nstart = 25, iter.max = 100)
#fn_data$kgroup <- km$cluster
#fn_data[fn_data$kgroup == 6]

#fviz_nbclust(num_data, kmeans, method = "wss")
#fviz_nbclust(num_data, kmeans, method = "silhouette")

#fviz_cluster(km,
#             data = num_data,
#             geom = "point"
#)