library(readxl)
library(stringr)
library(data.table)
library(writexl)
library(tictoc)
library(pracma)

rm(list = ls())
#Import Data
data <- read_excel("Computer Science for BA\\data.xlsx")

################################################################################
###############     create the Binary Product Matrix
################################################################################

W <- list()
for(j in 1:nrow(data)){
  product_title <- unlist(data[j,ncol(data)])
  word_ended <- FALSE 
  i <- 1 
  product_title <- tolower(product_title)
  product_title <- gsub('-|(|)|,|diagonal|diag', "", product_title)
  product_title <- gsub('Inch|inches|"|-inch| inch|inch', 'inch', product_title)
  product_title <- gsub('Hertz|hertz|Hz|HZ| hz|-hz|hz', 'hz', product_title)
  data[j,ncol(data)] <- product_title
  while(word_ended == FALSE){
    word <- word(product_title, start = i)
    i <- i + 1
    if(is.na(word)){
      word_ended <- TRUE 
    }
    else{
      if(word %in% W || nchar(word) < 3) {} else {W <- append(W, word)}
    }
  }
}

#create binary vectors and put the input matrix for LSH together 
binaryMatrix <- data.frame(matrix(data = 0, nrow = length(W), ncol = nrow(data)+1))
binaryMatrix[,1] <- unlist(W)
products <- unlist(data[,1])
colnames(binaryMatrix) <- c("words", products)

for(i in 1:nrow(data)){
  product_title <- unlist(data[i,ncol(data)])
  for(j in 1:length(W)){
    if(W[[j]] %in% unlist(strsplit(product_title," "))){
      binaryMatrix[j,i+1] <- 1 
    }
  }
}

binaryMatrix <- rbind(binaryMatrix, binaryMatrix_web)

################################################################################
#########################   create the signature matrix
################################################################################

n_permutations <- nrow(binaryMatrix)/2
signature <- data.frame(matrix(nrow = n_permutations, ncol = ncol(binaryMatrix) - 1))

for(i in 1:n_permutations){
  permutation <- randperm(1:nrow(binaryMatrix), nrow(binaryMatrix))
  for(j in 2:ncol(binaryMatrix)){
    ones <- which(binaryMatrix[,j] == 1)
    ones_permutated <- permutation[ones]
    signature[i, j-1] <- min(ones_permutated)
  }
}

sets <- names(binaryMatrix)
sets <- sets[2:length(sets)]
colnames(signature) <- c(sets)

################################################################################
#################     Gather evaluation statistics
################################################################################

threshold_list <- seq(0.25, 1, by = 0.1)
pair_compl_list <- list()
pair_qual_list <- list()
f1_list <- list() 
f1_star_list <- list()
for(q in 1:length(threshold_list)){
  ################################################################################
  #########################   create the candidate matrix
  ################################################################################
  
  t <- threshold_list[q]
  n <- nrow(signature)
  get_band_amnt <- function(b){
    value <- ((1/b)^(b/n)) - t 
  }
  band_amnt <- ceiling(uniroot(get_band_amnt, lower = 1, upper = n, tol = 0.01)$root)
  cat("band_amnt: ", band_amnt)
  print("")
  rows_band <- floor(n/band_amnt)
  r_start = seq(1,n, by= rows_band)
  r_end = seq(rows_band, n, by=rows_band)
  r_end[band_amnt] <- n
  
  candidates <- data.frame(matrix(data = 0, ncol = ncol(signature), nrow = ncol(signature)))
  #sets <- names(signature)
  #colnames(candidates) <- sets
  #rownames(candidates) <- sets
  
  for(i in 1:band_amnt){
    band <- signature[r_start[i]:r_end[i],]
    a <- sample.int(ncol(signature), 1)
    b <- sample.int(ncol(signature), 1)
    buckets <- list() 
    for(j in 1:ncol(signature)){
      band_column <- band[,j]
      band_signature <- paste(unlist(band_column), collapse = "")
      buckets <- append(buckets, band_signature)
    }
    k_start <- 1 
    k_end <- length(buckets)-1
    for(k in k_start:k_end){
      candidates_indices <- which(buckets == buckets[[k]])
      if(length(candidates_indices ) > 1){
        l_start <- 2 
        l_end <- length(candidates_indices)
        for(l in l_start:l_end){
          candidates[k, candidates_indices[l]] <- 1 
          candidates[candidates_indices[l], k] <- 1 
        }
      }
    }
  }
  
  print(sum(candidates))
  
  ################################################################################
  ###################  Create similarity matrix for candidate pairs 
  ################################################################################
  
  jaccard <- function(a, b) {
    intersection = length(intersect(which(a == 1), which(b == 1)))
    union = length(which(a==1)) + length(which(b==1)) - intersection
    jaccard_sim = intersection/union
    return (jaccard_sim)
  }
  
  calc_sim_matrix <- function(x){
    similarity_matrix[i,i] <<- 0 
    product1 <- binaryMatrix[,i+1]
    i <<- i + 1 
    candidates_indices <- which(x == 1)
    for(j in candidates_indices){
      product2 <- binaryMatrix[,j+1]
      sim <- jaccard(unlist(product1), unlist(product2))
      similarity_matrix[i,j] <<- sim 
      similarity_matrix[j,i] <<- sim
    }
  }
  
  # find duplicates through similarity matrix 
  similarity_matrix <- as.data.frame(matrix(data = 0, nrow = nrow(candidates), ncol = ncol(candidates)))
  products <- colnames(candidates)
  colnames(similarity_matrix) <- products
  rownames(similarity_matrix) <- products
  i <- 1 
  apply(candidates, 1, calc_sim_matrix)
  similarity_matrix <- similarity_matrix[1:1624,1:1624]
  
  # classify duplicates
  found_duplicates <- list() 
  i_start <- 1 
  i_end <- length(similarity_matrix)-1
  for(i in i_start:i_end){
    print("")
    duplicate_indices <- which(similarity_matrix[i] >= t)
    duplicate_indices <- duplicate_indices[which(duplicate_indices > i)]
    if(length(duplicate_indices) > 0){
      list <- list()
      list[1] <- i 
      j_start <- 1 
      j_end <- length(duplicate_indices)
      for(j in j_start:j_end){
        list[2] <- duplicate_indices[j]
        found_duplicates <- append(found_duplicates, list(list))
      }
    }
  }
  found_dupl_amnt <- length(found_duplicates)
  
  # find real duplicates
  real_duplicates <- list()
  models <- colnames(signature)
  duplicated_models <- models[duplicated(models)]
  duplicated_models <- duplicated_models[!duplicated(duplicated_models)]
  for(duplicate in duplicated_models){
    duplicate_indices <- which(models == duplicate) 
    j_start <- 1 
    j_end <- length(duplicate_indices)-1
    for(j in j_start:j_end){
      list <- list()
      list[1] <- duplicate_indices[j]
      k_start <- j+1 
      k_end <- length(duplicate_indices)
      for(k in k_start:k_end){
        list[2] <- duplicate_indices[k]
        real_duplicates <<- append(real_duplicates, list(list))
      }
    }
  }
  real_dupl_amnt <- length(real_duplicates)
  
  ################################################################################
  #################   Get the evaluation statistics
  ################################################################################
  
  TP <- 0
  FP <- 0 
  for(i in 1:found_dupl_amnt){
    found_duplicate <- found_duplicates[[i]]
    if(list(found_duplicate) %in% real_duplicates){
      TP <- TP + 1 
    }
    else{
      FP <- FP + 1 
    }
  }
  precision <- TP/found_dupl_amnt
  recall <- TP/real_dupl_amnt
  f1 <- 2/((1/recall)+(1/precision))
  
  pair_compl <- TP/real_dupl_amnt
  pair_qual <- TP/sum(candidates)
  f1_star <- (2*pair_qual*pair_compl)/(pair_qual + pair_compl)
  
  f1_list <- append(f1_list, f1)
  pair_compl_list <- append(pair_compl_list, pair_compl)
  pair_qual_list <- append(pair_qual_list, pair_qual)
  f1_star_list <- append(f1_star_list, f1_star) 
}

fraction_of_comparisons <- rev(abs(1-threshold_list))
f1_plot <- rev(f1_list)
f1_star_plot <- rev(f1_star_list)
pair_compl_plot <- rev(pair_compl_list)
pair_qual_plot <- rev(pair_qual_list)
par(mfrow=c(2,2))
plot(fraction_of_comparisons, f1_star_plot, type = "l", xlab = "fraction of comparisons", ylab = "F1")
plot(fraction_of_comparisons, f1_plot,  type = "l",xlab = "fraction of comparisons", ylab = "F1*")
plot(fraction_of_comparisons, pair_compl_plot,  type = "l",xlab = "fraction of comparisons", ylab = "pair completeness")
plot(fraction_of_comparisons, pair_qual_plot, type = "l", xlab = "fraction of comparisons", ylab = "pair quality")









