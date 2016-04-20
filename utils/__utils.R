suppressMessages(require(dplyr))
suppressMessages(require(reshape2))
suppressMessages(require(car))

# distributed cumulative gain
dcg_K <- function(r, k = min(5, length(r))) {
  r <- as.vector(r)[1:k]
  sum(( 2^r - 1 )/ log2( 2:(length(r)+1)) )
}

# normalized distributed culmulative gain @ 5
ndcg_K <- function(r, k  = min(5, length(r))) {
  r <- as.vector(r)[1:k]
  if (sum(r) <= 0) return (0)     # no hits (dcg_max = 0)
  dcg_max <- dcg_K(sort(r, decreasing=TRUE)[1:k], k)
  return ( dcg_K(r, k) / dcg_max )
}

# calculate cross-validation ndgc score xgboost
ndgcScore<- function( preds, truth ) {
  # preds: matrix or data.frame
  # one row for each observation, one column for each prediction.
  # Columns are sorted from left to right descending in order of likelihood.
  # truth: vector
  # one row for each observation.
  preds <- as.matrix(preds)
  truth <- as.vector(truth)
  
  stopifnot( length(truth) == nrow(preds))
  r <- apply( cbind( truth, preds), 1
              , function(x) ifelse( x == x[1], 1, 0))[ -1, ]
  if ( ncol(preds) == 1) r <-  rbind( r, r)  #workaround for 1d matrices
  as.vector( apply(r, 2, ndcg_K) )
  
}

# reshape xgboost multi:softprob outputs
reshape_probs <- function(preds, Ids = NULL, num_class = 12) {
  
  p <- data.frame(t(matrix(pred, nrow = num_class, ncol = length(pred)/num_class)))
  cnames <- c("NDF", "US", "other", "FR", "CA", "GB", "ES", "IT", "PT", "NL", "DE", "AU")
  names(p) <- cnames
  
  if(is.null(Ids)){
    
    return(p)
    
  } else {
  
    p <- cbind(id = Ids, p)
    return(p)
    
  }
}

# get top 5 alternatives from probability rankings
topChoices <- function(predMatrix, n = 5) {
  
  if(ncol(predMatrix) > 12) {
    a <- data.frame(t(apply(predMatrix[, -1], 1, function(x) names(sort(x, decreasing = T))[1:5])))
    names(a) <- c("c1", "c2", "c3", "c4", "c5")
    a <- cbind(id  = predMatrix$id, a)
  } else {
    
    a <- data.frame(t(apply(predMatrix, 1, function(x) names(sort(x, decreasing = T))[1:5])))
    names(a) <- c("c1", "c2", "c3", "c4", "c5")
  }
  
  return(a)
}

# create the submission file
createSubmission <- function(choices) {
  
  a <- melt(choices, id.vars = "id")
  a$variable <- NULL
  a <- a[order(a$id),]
  names(a)[2] <- "country"
  return(a)
}

hardcode <- function(x) {
  
  y <- recode(x, "0 = 'NDF';
              1= 'US';
              2= 'other';
              3='FR';
              4= 'CA';
              5= 'GB';
              6= 'ES';
              7= 'IT';
              8= 'PT';
              9= 'NL';
              10= 'DE';
              11= 'AU' ")
  
  return(y)
  
}
