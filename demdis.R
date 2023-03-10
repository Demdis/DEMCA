library(dplyr)
library(factoextra)

top_comms <- function(partic, comms) {
  # Calculate comment points based on consensus metric, 'agree' votes and
  # 'agree' percentage
  n_total <- partic %>% filter(!is.na(group.id)) %>% summarize(n = n())
  partic <- partic %>% filter(!is.na(group.id)) # only clustered voters
  
  # Comments with at least 25% votes (incl. skips) from all clustered voters
  good_cols <- which(apply(!is.na(partic[, 8:ncol(partic)]), 2, sum) / 
                       unlist(n_total) >= 0.25) + 7
  # Calculate summary statistics for each cluster, long format for comments
  cluster_votes <- data.frame() 
  for (i in good_cols) {
    add <- partic %>% group_by(group.id) %>% 
      summarize(unseen = sum(is.na(!!as.symbol(colnames(partic)[i]))),
                n.votes = sum(!is.na(!!as.symbol(colnames(partic)[i]))),
                n.agr = sum(!!as.symbol(colnames(partic)[i]) == 1, na.rm = T), 
                n.skip = sum(!!as.symbol(colnames(partic)[i]) == 0, na.rm = T),
                n.dis = sum(!!as.symbol(colnames(partic)[i]) == -1, na.rm = T), 
                comment.id = as.integer(gsub(".*?([0-9]+).*", "\\1", 
                                             colnames(partic)[i])))
    cluster_votes <- rbind(cluster_votes, add)
  }
  
  # Add agreement percentage 
  cluster_votes <- cluster_votes %>% 
    mutate(agr_pct = round(n.agr / (n.agr + n.dis), 3))
  ### Dataset of point calculations = mk, mean_ap, agr_total 
  points <- cluster_votes %>% group_by(comment.id) %>% 
    summarize(mk = max_diff(agr_pct))
  # Repair NA values if a comment had no votes within given cluster 
  cluster_votes$agr_pct[is.na(cluster_votes$agr_pct)] <- 0
  points <- cbind(points, round(cluster_votes %>% group_by(comment.id) %>% 
                                  summarize(mean_ap = mean(agr_pct)) %>% 
                                  select(mean_ap), 3))
  points <- cbind(points, cluster_votes %>% group_by(comment.id) %>% 
                    summarize(agr_total = sum(n.agr)) %>% select(agr_total))
  # Calculate final points, t(MK) = 4^MK
  points <- points %>% mutate(body = floor(agr_total * mean_ap / 4^mk)) %>% 
    arrange(desc(body))
  points$body <- as.integer(points$body)
  points$comment.id <- as.integer(points$comment.id)
  # Dataset with comments ranked
  final <- left_join(points, comms) %>% 
    select(comment.id, body, comment.body) %>% arrange(desc(body))
  return(list(clust = cluster_votes, points = points, final = final))
}

double <- function(partic, num1, num2, clust = 0, seen = T) {
  # 2-comment contingency table of votes withing given cluster
  cluster <- partic %>% filter(group.id == clust) %>% 
    select(paste('X', num1, sep = ''), paste('X', num2, sep = ''))
  cluster[cluster == 0] <- 'S' # skips
  cluster[is.na(cluster)] <- '0' # never seen
  # To ordered factors 
  cluster[, paste('X', num1, sep = '')] <- ordered(cluster[, paste('X', num1, 
                                                                   sep = '')])
  cluster[, paste('X', num2, sep = '')] <- ordered(cluster[, paste('X', num2, 
                                                                   sep = '')])
  # Print it
  # Indices of those users, who have seen&voted on both comments (incl. skips)
  ind <- cluster[, paste('X', num1, sep = '')] != 0 & 
    cluster[, paste('X', num2, sep = '')] != 0 
  if (seen != T) print(table(cluster))
  else {
    cluster <- cluster[ind, ] # subset the rows of aforementioned users
    cluster <- cluster %>% droplevels() # remove '0' (unseen) level
    print(table(cluster))
  }
  #stat <- cor.test(as.numeric(cluster[, paste('X', num1, sep = '')]),
  #                 as.numeric(cluster[, paste('X', num2, sep = '')]),
  #                 method = 'kendall')
  #print(list(statistic = stat$estimate))
}

# Lambda func. for maximal difference in a vector 
max_diff <- function(x) return(max(x, na.rm = T) - min(x, na.rm = T))

clean <- function(df) {
  # Preparation of data for clustering (only users w/ 7+ votes, only comments
  # with at least one vote, return vote matrix)
  vote_mat <- df[apply(!is.na(df[, 8:ncol(df)]), 1, sum) >= 7, 8:ncol(df)]
  vote_mat <- vote_mat[, apply(!is.na(vote_mat), 2, sum) != 0]
  return(vote_mat)
}

clean_proj <- function(df) {
  vote_mat <- df[apply(!is.na(df), 1, sum) >= 7, ]
  vote_mat <- vote_mat[, apply(!is.na(vote_mat), 2, sum) != 0]
  return(vote_mat)
}

polis.clust <- function(vote, force = NA) {
  # Impute vote matrix data, perform PCA to 2D and cluster voters by kmeans 
  # algorithm into best number of clusters (force = NA) or force k
  
  ans <- apply(!is.na(vote), 1, sum)
  for (i in 1:ncol(vote)) {
    vote[is.na(vote[, i]), i] <- mean(vote[, i], na.rm = T)
  }
  pr <- prcomp(vote)$x[, 1:2]
  pts <- pr * sqrt(ncol(vote) / ans)
  if (is.na(force)) {
    k <- rep(NA, 30)
    for (i in 1:30) {
      lloyd <- kmeans(pts, min(100,  as.data.frame(pts) %>% distinct %>% nrow),
                      algorithm = 'Lloyd')
      k[i] <- tryCatch({which.max(fviz_nbclust(lloyd$centers, kmeans)$data$y[1:5])
      }, error = function(e){cat("ERROR :",conditionMessage(e), "\n")
                             return(NA)})
      rm(.Random.seed, envir = globalenv())
    }
    k <- as.numeric(dimnames(table(k))$k[which.max(table(k))])
    print(paste('Optimal number of clusters determined as ', k, '.', sep = ''))
    lloyd <- kmeans(pts, min(100,  as.data.frame(pts) %>% distinct %>% nrow), 
                    algorithm = 'Lloyd')
    temp <- cbind(lloyd$centers, 1:min(100,  as.data.frame(pts) %>% distinct %>% nrow))
    kms <- kmeans(temp[, 1:2], k)
    temp <- data.frame(ll.cl = temp[, 3], cl = kms$cluster)
    cl <- left_join(data.frame(ll.cl = lloyd$cluster), temp)
    vote$cluster <- cl$cl
  }
  else {
    print(paste('Optimal number of clusters forced as ', force, '.', sep = ''))
    lloyd <- kmeans(pts, min(100,  as.data.frame(pts) %>% distinct %>% nrow), 
                    algorithm = 'Lloyd')
    temp <- cbind(lloyd$centers, 1:min(100,  as.data.frame(pts) %>% distinct %>% nrow))
    kms <- kmeans(temp[, 1:2], force)
    temp <- data.frame(ll.cl = temp[, 3], cl = kms$cluster)
    cl <- left_join(data.frame(ll.cl = lloyd$cluster), temp)
    vote$cluster <- cl$cl
  }
  return(cbind(vote, pts)) 
}


recreate <- function(partic, clust) {
  # Recreates the participant dataset with my own clusters 
  ind <- apply(!is.na(partic[, 8:ncol(partic)]), 1, sum) >= 7
  partic$group.id[ind] <- clust$cluster
  partic$group.id[!ind] <- NA
  return(partic)
}

comms.custom <- function(partic, comms, ind) {
  subpartic <- partic[ind, ]
  df <- data.frame()
  for (i in 8:(dim(subpartic)[2])) {
    add <- subpartic %>%
      summarize(unseen = sum(is.na(!!as.symbol(colnames(partic)[i]))),
                n.votes = sum(!is.na(!!as.symbol(colnames(partic)[i]))),
                n.agr = sum(!!as.symbol(colnames(partic)[i]) == 1, na.rm = T), 
                n.skip = sum(!!as.symbol(colnames(partic)[i]) == 0, na.rm = T),
                n.dis = sum(!!as.symbol(colnames(partic)[i]) == -1, na.rm = T), 
                comment.id = as.integer(gsub(".*?([0-9]+).*", "\\1", 
                                             colnames(partic)[i])))
    df <- rbind(df, add)
  }
  df$group.id <- 0 # random value for function compatibility
  df <- df %>% mutate(agr_pct = round(n.agr / (n.agr + n.dis), 3))
  return(list(clust = df))
}

