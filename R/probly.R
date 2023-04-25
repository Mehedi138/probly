#' Creates histogram, boxplot and numeric summary
#' @export
#' @param x numeric variable


`cards` <- function (jokers = FALSE, makespace = FALSE){
  x <- c(2:10, "J", "Q", "K", "A")
  y <- c("Club", "Diamond", "Heart", "Spade")
  res <- expand.grid(rank = x, suit = y)
  if (jokers) {
    levels(res$rank) <- c(levels(res$rank), "Joker")
    res <- rbind(res, data.frame(rank = c("Joker", "Joker"),
                                 suit = c(NA, NA)))
  }
  if (makespace) {
    res$probs <- rep(1, dim(res)[1])/dim(res)[1]
  }
  return(res)
}


`euchredeck` <- function (benny = FALSE, makespace = FALSE){
  x <- c(9:10, "J", "Q", "K", "A")
  y <- c("Club", "Diamond", "Heart", "Spade")
  res <- expand.grid(value = x, suit = y)
  if (benny) {
    levels(res$value) <- c(levels(res$value), "Joker")
    res <- rbind(res, data.frame(value = c("Joker"), suit = NA))
  }
  if (makespace) {
    res$probs <- rep(1, dim(res)[1])/dim(res)[1]
  }
  return(res)
}



`rolldie` <- function (times, nsides = 6, makespace = FALSE){
  temp = list()
  for (i in 1:times) {
    temp[[i]] <- 1:nsides
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  names(res) <- c(paste(rep("X", times), 1:times, sep = ""))
  if (makespace)
    res$probs <- rep(1, nsides^times)/nsides^times
  return(res)
}



`roulette` <- function (european = FALSE, makespace = FALSE){
  if (european) {
    num = c("0", "26", "3", "35", "12", "28", "7", "29",
            "18", "22", "9", "31", "14", "20", "1", "33", "16",
            "24", "5", "10", "23", "8", "30", "11", "36", "13",
            "27", "6", "34", "17", "25", "2", "21", "4", "19",
            "15", "32")
    color <- c("Green", rep(c("Black", "Red"), 18))
  }
  else {
    num = c("27", "10", "25", "29", "12", "8", "19", "31",
            "18", "6", "21", "33", "16", "4", "23", "35", "14",
            "2", "0", "28", "9", "26", "30", "11", "7", "20",
            "32", "17", "5", "22", "34", "15", "3", "24", "36",
            "13", "1", "00")
    color <- c(rep(c("Red", "Black"), 9), "Green", rep(c("Black",
                                                         "Red"), 9), "Green")
  }
  res <- data.frame(num = num, color = color)
  if (makespace) {
    res$probs <- rep(1, length(num))/length(num)
  }
  return(res)
}



`tosscoin` <- function (times, makespace = FALSE){
  temp <- list()
  for (i in 1:times) {
    temp[[i]] <- c("H", "T")
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  names(res) <- c(paste(rep("toss", times), 1:times, sep = ""))
  if (makespace)
    res$probs <- rep(1, 2^times)/2^times
  return(res)
}



`urnsamples` <- function (x, ...)
  UseMethod("urnsamples")


`urnsamples.data.frame` <- function (x, size, replace = FALSE, ordered = FALSE, ...){
  nurn <- dim(x)[1]
  if (isTRUE(replace)) {
    if (isTRUE(ordered)) {
      temp <- list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      ind <- t(as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE)))
    }
    else {
      temp <- list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      res <- as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE))
      ind <- t(unique(t(apply(res, 1, sort))))
    }
  }
  else {
    if (size > nurn)
      stop("cannot take a sample larger than the urn size when 'replace = FALSE'")
    if (isTRUE(ordered)) {
      ind <- permsn(1:nurn, size)
    }
    else {
      ind <- combn(1:nurn, size)
    }
  }
  if (!is.null(x$probs))
    x$probs <- NULL
  nss <- dim(ind)[2]
  out <- list()
  for (i in 1:nss) {
    out[[i]] <- x[ind[, i], ]
  }
  return(out)
}



`urnsamples.default` <- function (x, size, replace = FALSE, ordered = FALSE, ...){
  nurn <- length(x)
  if (isTRUE(replace)) {
    if (isTRUE(ordered)) {
      temp = list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      ind <- t(as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE)))
    }
    else {
      temp = list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      res <- as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE))
      ind <- t(unique(t(apply(res, 1, sort))))
    }
  }
  else {
    if (size > nurn)
      stop("cannot take a sample larger than the urn size when 'replace = FALSE'")
    if (isTRUE(ordered)) {
      ind <- permsn(1:nurn, size)
    }
    else {
      ind <- combn(1:nurn, size)
    }
  }
  nss <- dim(ind)[2]
  out <- matrix(nrow = nss, ncol = size)
  for (i in 1:nss) {
    out[i, ] <- x[ind[, i]]
  }
  return(data.frame(out))
}





`iidspace` <- function (x, ntrials, probs = NULL){
  temp = list()
  for (i in 1:ntrials) {
    temp[[i]] <- x
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  if (is.null(probs)) {
    res$probs <- rep(1/dim(res)[1], dim(res)[1])
  }
  else {
    if (!identical(length(x), length(probs))) {
      stop("'probs' is not the same length as 'outcomes'")
    }
    if (any(probs < 0)) {
      stop("'probs' contains negative values")
    }
    probs <- probs/sum(probs)
    ptemp = list()
    for (i in 1:ntrials) {
      ptemp[[i]] <- probs
    }
    pdf <- expand.grid(ptemp, KEEP.OUT.ATTRS = FALSE)
    pres <- apply(pdf, 1, prod)
    res$probs <- pres
  }
  names(res) <- c(paste(rep("X", ntrials), 1:ntrials, sep = ""),
                  "probs")
  return(res)
}



`is.probspace` <- function (x){
  if (any(class(x) == "ps"))
    return(TRUE)
  if (!is.data.frame(x) | is.null(x$probs))
    return(FALSE)
  if (any(x$probs < 0))
    return(FALSE)
  return(TRUE)
}



`probspace` <- function (x, ...)
  UseMethod("probspace")



`probspace.default` <- function (x, probs, ...){
  y <- data.frame(x)
  if (missing(probs)) {
    y$probs <- rep(1, dim(y)[1])/dim(y)[1]
  }
  else {
    if (any(probs < 0)) {
      stop("'probs' contains negative values")
    }
    y$probs <- probs/sum(probs)
  }
  return(y)
}


`probspace.list` <- function (x, probs, ...){
  y <- x
  if (missing(probs)) {
    probs <- rep(1, length(y))/length(y)
  }
  else {
    if (any(probs < 0)) {
      stop("'probs' contains negative values")
    }
    probs <- probs/sum(probs)
  }
  res <- list(outcomes = y, probs = probs)
  class(res) <- c("ps", "list")
  return(res)
}



`countrep` <- function (x, ...)
  UseMethod("countrep")

`countrep.data.frame` <- function (x, ...){
  apply(x, MARGIN = 1, FUN = countrep, ...)
}


`countrep.default` <- function (x, vals = unique(x), nrep = 2, ...){
  res <- 0
  if (length(x) >= nrep) {
    for (i in 1:length(vals)) {
      if (sum(mapply(all.equal, x, vals[i]) == TRUE) ==
          nrep) {
        res <- res + 1
      }
    }
  }
  return(res)
}


`isin` <- function (x, ...)
  UseMethod("isin")



`isin.data.frame` <- function (x, ...){
  apply(x, MARGIN = 1, FUN = isin, ...)
}


`isin.default` <- function (x, y, ordered = FALSE, ...){
  res <- (length(y) <= length(x))
  if (res) {
    temp <- x
    for (i in 1:length(y)) {
      if (is.element(y[i], temp)) {
        if (!ordered) {
          temp <- temp[-which(temp %in% y[i])[1]]
        }
        else {
          temp <- temp[-(1:which(temp %in% y[i])[1])]
        }
      }
      else {
        res <- FALSE
        i <- length(y)
      }
    }
  }
  return(res)
}



`isrep` <- function (x, ...)
  UseMethod("isrep")


`isrep.data.frame` <- function (x, ...){
  apply(x, MARGIN = 1, FUN = isrep, ...)
}



`isrep.default` <- function (x, vals = unique(x), nrep = 2, ...){
  res <- FALSE
  if (length(x) >= nrep) {
    for (i in 1:length(vals)) {
      if (sum(mapply(all.equal, x, vals[i]) == TRUE) ==
          nrep) {
        res <- TRUE
        i <- length(vals)
      }
    }
  }
  return(res)
}



`empirical` <- function (x){
  if (any(class(x) == "ps"))
    stop("not implemented for class 'ps'")
  if (!is.data.frame(x))
    message("'x' must be a data frame")
  temp <- x
  n <- dim(temp)[1]
  vars <- names(temp)
  temp$probs <- rep(1, n)/n
  return(marginal(temp))
}



`sim` <- function (x, ...)
  UseMethod("sim")


`sim.default` <- function (x, ntrials, ...){
  out <- data.frame(x[, -which(names(x) == "probs")])
  names(out) <- names(x)[-which(names(x) == "probs")]
  p <- x$probs
  d <- dim(x)[1]
  ind <- sample(1:d, size = ntrials, replace = TRUE, prob = p)
  res <- as.data.frame(out[ind, ])
  names(res) <- names(out)
  rownames(res) <- 1:ntrials
  return(res)
}



`sim.ps` <- function (x, ntrials, ...){
  out <- x$outcomes
  p <- x$probs
  d <- length(x$outcomes)
  ind <- sample(1:d, size = ntrials, replace = TRUE, prob = p)
  res <- out[ind]
  return(res)
}



`addrv` <- function (space, FUN = NULL, invars = NULL, name = NULL, ...){
  if (any(class(space) == "ps"))
    stop("not implemented for class 'ps'")
  if (!is.data.frame(space) | is.null(space$probs)) {
    message("'space' is not a proper probability space")
    stop("see ?probspace")
  }
  bnames <- names(space)[which(names(space) != "probs")]
  out <- subset(space, select = bnames)
  probs <- subset(space, select = probs)
  if (is.null(invars))
    invars <- bnames
  if (!is.character(invars))
    stop("vars should be a character vector")
  if (!is.null(FUN)) {
    if (is.null(name))
      name <- "X"
    temp <- apply(subset(space, select = invars), 1, FUN)
    val <- cbind(out, temp, probs)
    names(val) <- c(bnames, name, "probs")
  }
  else {
    val <- transform(out, ...)
    val$probs <- probs
  }
  return(val)
}



`marginal` <- function (space, vars = NULL){
  if (!is.data.frame(space) | is.null(space$probs)) {
    message("'space' is not a proper probability space")
    stop("see ?probspace")
  }
  if (is.null(vars))
    vars <- names(space)[names(space) != "probs"]
  if (!is.character(vars)) {
    stop("'vars' must be a character vector")
  }
  if (length(vars) > 1) {
    res <- aggregate(space$probs, by = as.list(space[, vars]),
                     FUN = sum)
  }
  else {
    res <- aggregate(space$probs, by = list(space[, vars]),
                     FUN = sum)
  }
  names(res) <- c(vars, "probs")
  return(res)
}


`noorder` <- function (space){
  if (!is.data.frame(space)) {
    message("'space' is missing a probs column")
    stop("see ?probspace")
  }
  if (is.null(space$probs)) {
    if (dim(space)[2] < 2)
      stop("'space' has only one column of outcomes; already unordered")
    n <- names(space)
    res <- unique(data.frame(t(apply(space, 1, sort))))
    names(res) <- n
  }
  else {
    if (dim(space)[2] < 3)
      stop("'space' has only one column of outcomes; already unordered")
    A <- subset(space, select = -probs)
    probs <- subset(space, select = probs)
    n <- names(A)
    sA <- data.frame(t(apply(A, 1, sort)))
    res <- cbind(sA, probs)
    res <- aggregate(res$probs, by = as.list(sA), sum)
    names(res) <- c(n, "probs")
  }
  return(res)
}




`intersect` <- function (x, ...)
  UseMethod("intersect")


`intersect.data.frame` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  x[match(intersect(a, b), a), ]
}



`intersect.default` <- function (x, y, ...){
  y <- as.vector(y)
  unique(y[match(as.vector(x), y, 0)])
}



`intersect.ps` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  e <- match(intersect(a, b), a)
  res <- list(outcomes = x$outcomes[e], probs = x$probs[e])
  class(res) <- c("ps", "list")
  return(res)
}



`setdiff` <- function (x, ...)
  UseMethod("setdiff")


`setdiff.data.frame` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  x[match(setdiff(a, b), a), ]
}


`setdiff.default` <- function (x, y, ...){
  x <- as.vector(x)
  y <- as.vector(y)
  unique(if (length(x) || length(y))
    x[match(x, y, 0) == 0]
    else x)
}


`setdiff.ps` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  e <- match(setdiff(a, b), a)
  res <- list(outcomes = x$outcomes[e], probs = x$probs[e])
  class(res) <- c("ps", "list")
  return(res)
}


`subset.ps` <- function (x, subset, ...){
  e <- substitute(subset)
  r <- sapply(x$outcomes, function(t) {
    eval(e, t)
  })
  if (!is.logical(r))
    stop("'subset' must be logical")
  res <- list(outcomes = x$outcomes[r & !is.na(r)], probs = x$probs[r &
                                                                      !is.na(r)])
  class(res) <- c("ps", "list")
  return(res)
}


`union` <- function (x, ...)
  UseMethod("union")


`union.data.frame` <- function (x, y, ...){
  res <- unique(rbind(as.data.frame(y), x))
  res[order(as.numeric(rownames(res))), ]
}



`union.default` <- function (x, y, ...)
  unique(c(as.vector(x), as.vector(y)))


`union.ps` <- function (x, y, ...){
  na <- length(x$outcomes)
  nb <- length(y$outcomes)
  temp <- x
  for (i in 1:nb) {
    temp$outcomes[[na + i]] <- y$outcomes[[i]]
    temp$probs[[na + i]] <- y$probs[[i]]
  }
  a <- do.call("paste", c(temp, sep = "\r"))
  e <- !duplicated(a)
  res <- list(outcomes = temp$outcomes[e], probs = temp$probs[e])
  class(res) <- c("ps", "list")
  return(res)
}

#  Characteristic functions
#  Released under GPL 2 or greater
#  Copyright January 2009, G. Jay Kerns


cfbeta <- function(t, shape1, shape2, ncp = 0){
  if (shape1 <=0 || shape2 <=0)
    stop("shape1, shape2 must be positive")
  if (identical(all.equal(ncp, 0), TRUE)){
    # require(fAsianOptions)
    kummerM(1i*t, shape1, shape1 + shape2)
  } else {
    fr <- function(x) cos(t*x)*dbeta(x, shape1, shape2, ncp)
    fi <- function(x) sin(t*x)*dbeta(x, shape1, shape2, ncp)
    Rp <- integrate(fr, lower = 0, upper = 1)$value
    Ip <- integrate(fi, lower = 0, upper = 1)$value
    return( Rp + 1i*Ip )
  }
}


cfbinom <- function(t, size, prob){
  if (size <= 0 )
    stop("size must be positive")
  if (prob < 0 || prob > 1)
    stop("prob must be in [0,1]")
  (prob*exp(1i*t) + (1 - prob))^size
}


cfcauchy = function(t, location = 0, scale = 1){
  if (scale <= 0 )
    stop("scale must be positive")
  exp(1i*location*t - scale*abs(t))
}

cfchisq <- function(t, df, ncp = 0){
  if (df < 0 || ncp < 0  )
    stop("df and ncp must be nonnegative")
  exp(1i*ncp*t/(1-2i*t))/(1 - 2i*t)^(df/2)
}

cfexp <- function(t, rate = 1){
  cfgamma(t, shape = 1, scale = 1/rate)
}

cff <- function(t, df1, df2, ncp, kmax = 10){
  if (df1 <= 0 || df2 <= 0  )
    stop("df1 and df2 must be positive")
  # require(fAsianOptions)
  if( identical(all.equal(ncp, 0), TRUE) ){
    gamma((df1+df2)/2) / gamma(df2/2) * kummerU(-1i*df2*t/df1, df1/2, 1 - df2/2)
  } else {
    exp(-ncp/2)*sum((ncp/2)^(0:kmax)/factorial(0:kmax)* kummerM(-1i*df2*t/df1, df1/2 + 0:kmax, -df2/2))
  }
}


cfgamma <- function(t, shape, rate = 1, scale = 1/rate){
  if (rate <= 0  || scale <= 0)
    stop("rate must be positive")
  (1-scale*1i*t)^(-shape)
}


cfgeom <- function(t, prob){
  cfnbinom(t, size = 1, prob = prob)
}


cfhyper <- function(t, m, n, k){
  if (m < 0 || n < 0 || k < 0)
    stop("m, n, k must be positive")
  hypergeo::hypergeo(-k, -m, n - k + 1, exp(1i*t))/hypergeo::hypergeo(-k, -m, n - k + 1, 1)
}


cflnorm <- function(t, meanlog = 0, sdlog = 1){
  if (sdlog <= 0)
    stop("sdlog must be positive")
  if (identical(all.equal(t, 0), TRUE)){
    return(1+0i)
  } else {
    t <- t * exp(meanlog)
    Rp1 <- integrate(function(y) exp(-log(y/t)^2/2/sdlog^2) * cos(y)/y, lower = 0, upper = t )$value
    Rp2 <- integrate(function(y) exp(-log(y*t)^2/2/sdlog^2) * cos(1/y)/y, lower = 0, upper = 1/t )$value
    Ip1 <- integrate(function(y) exp(-log(y/t)^2/2/sdlog^2) * sin(y)/y, lower = 0, upper = t )$value
    Ip2 <- integrate(function(y) exp(-log(y*t)^2/2/sdlog^2) * sin(1/y)/y, lower = 0, upper = 1/t )$value
    return((Rp1 + Rp2 + 1i*(Ip1 + Ip2))/(sqrt(2*pi) * sdlog))
  }
}


cflogis <- function(t, location = 0, scale = 1){
  if (scale <= 0)
    stop("scale must be positive")
  ifelse( identical(all.equal(t, 0), TRUE),
          return(1),
          return(exp(1i*location)*pi*scale*t/sinh(pi*scale*t)))
}


cfnbinom <- function(t, size, prob, mu){
  if (size <= 0 )
    stop("size must be positive")
  if (prob <= 0 || prob > 1)
    stop("prob must be in (0,1]")
  if (!missing(mu)) {
    if (!missing(prob))
      stop("'prob' and 'mu' both specified")
    prob <- size/(size+mu)
  }
  (prob/(1-(1-prob)*exp(1i*t)))^size
}


cfnorm <- function(t, mean = 0, sd = 1){
  if (sd <= 0)
    stop("sd must be positive")
  exp(1i*mean - (sd*t)^2/2)
}

cfpois <- function(t, lambda){
  if (lambda <= 0)
    stop("lambda must be positive")
  exp(lambda*(exp(1i*t) - 1))
}


cfsignrank <- function(t, n){
  sum(exp(1i*t*0:((n+1)*n/2)) * dsignrank(0:((n+1)*n/2), n))
}


cft <- function(t, df, ncp){
  if(missing(ncp)) ncp <- 0
  if (df <= 0)
    stop("df must be positive")
  if (identical(all.equal(ncp, 0), TRUE)){
    ifelse(identical(all.equal(t, 0), TRUE), 1+0i,
           as.complex(besselK(sqrt(df)*abs(t), df/2)*(sqrt(df)*abs(t))^(df/2)/( gamma(df/2) * 2^(df/2 - 1) ))
    )
  } else {
    fr <- function(x) cos(t*x)*dt(x, df, ncp)
    fi <- function(x) sin(t*x)*dt(x, df, ncp)
    Rp <- integrate(fr, lower = -Inf, upper = Inf)$value
    Ip <- integrate(fi, lower = -Inf, upper = Inf)$value
    return(Rp + 1i*Ip)
  }
}


cfunif <- function(t, min = 0, max = 1){
  if (max < min)
    stop("min cannot be greater than max")
  ifelse( identical(all.equal(t, 0), TRUE),
          1+0i,
          (exp(1i*t*max) - exp(1i*t*min))/(1i*t*(max - min)))
}


cfweibull <- function(t, shape, scale = 1){
  if (shape <= 0 || scale <= 0)
    stop("shape and scale must be positive")
  fr <- function(x) cos(t*x)*dweibull(x, shape, scale)
  fi <- function(x) sin(t*x)*dweibull(x, shape, scale)
  Rp <- integrate(fr, lower = 0, upper = Inf)$value
  Ip <- integrate(fi, lower = 0, upper = Inf)$value
  return( Rp + 1i*Ip )
}


cfwilcox <- function(t, m, n){
  sum(exp(1i*t*0:(m*n)) * dwilcox(0:(m*n), m, n))
}




####################################
# Functions to generate data
# for pedagogical purposes

#######################################################
# continuous X data

genXdata <- function(n, nvar = 1,
                     mu = rep(0, nvar),
                     Sigma = diag(length(mu)),
                     varnames = paste("x", 1:length(mu), sep = ""),
                     roundto = NULL
){
  tmp <- as.data.frame(MASS::mvrnorm(n, mu = mu, Sigma = Sigma))
  names(tmp) <- varnames
  if (!is.null(roundto)){
    tmp <- round(tmp, roundto)
  }
  tmp
}

# genXdata(10, nvar = 3, roundto = 2)
# X = genXdata(10, nvar = 3, roundto = 2)

#######################################################
# logistic regression data

genLogRegData <- function(xdata,
                          beta = rep(1, ncol(xdata)),
                          yname = "y"){
  tmp <- as.matrix(xdata) %*% beta
  probs <- exp(tmp)/(1 + exp(tmp))
  y <- apply(probs, 1, function(p){rbinom(1, size = 1, prob = p)})
  resdata <- cbind(xdata, y)
  as.data.frame(resdata, col.names = c(names(xdata), yname))
}


#params <- c(1,2,3,4)
#require(MASS)
#xmean <- Null(params)[ , 1]
#X = genXdata(10, mu = xmean, roundto = 2)
#genLogRegData(X, beta = params)


######################################################3
# contingency tables

genIndepTable <- function(n = sample(100:500, size = 1),
                          prow = 1:3, pcol = 1:4,
                          dmnames = list(X = paste("x", 1:length(prow), sep = ""),
                                         Y = paste("y", 1:length(pcol), sep = "")),
                          addmargins = TRUE,
                          as.df = FALSE, untable = TRUE){
  prow <- prow/sum(prow)
  pcol <- pcol/sum(pcol)
  pmatrix <- outer(prow, pcol)
  probs <- as.numeric(pmatrix)
  x <- factor(sample(1:length(probs), size = n, replace = TRUE, prob = probs),
              levels = 1:length(probs))
  tmp <- matrix(as.integer(table(x)), nrow = length(prow))
  dimnames(tmp) <- dmnames
  tmp <- as.table(tmp)

  if (as.df){
    tmp <- as.data.frame(tmp)
    if (untable){
      tmp <- with(tmp, reshape::untable(tmp, Freq))
      tmp[ , "Freq"] <- NULL
      rownames(tmp) <- 1:dim(tmp)[1]
    }
    tmp
  } else if (addmargins) {
    addmargins(tmp)
  } else {
    tmp
  }
}

#
# genIndepTable(n = 100)
# genIndepTable(n = 100, nfixed = TRUE)
# genIndepTable(n = 100, nfixed = TRUE, as.df = TRUE)
# genIndepTable(n = 100, nfixed = TRUE, as.df = TRUE, untable = FALSE)
#
# tmp = genIndepTable(n = 10, nfixed = TRUE, as.df = TRUE)
# tmp
#
# model.matrix(~., data = tmp)
# tmp2 = as.data.frame(model.matrix(~ X*Y, data = tmp))
# tmp2
#
# genLogRegData(tmp2)
#
# A = genIndepTable(n = 500, nfixed = TRUE, as.df = TRUE)
# chisq.test(xtabs(~., data = A))
#


######################################################3
# general two-way tables

gen2wayTable <- function(n = sample(100:500, size = 1),
                         pmatrix = matrix(1:12, nrow = 3),
                         dmnames = list(X = paste("x", 1:nrow(pmatrix), sep = ""),
                                        Y = paste("y", 1:ncol(pmatrix), sep = "")),
                         addmargins = TRUE,
                         as.df = FALSE, untable = TRUE){
  probs <- as.numeric(pmatrix)
  x <- factor(sample(1:length(probs), size = n, replace = TRUE, prob = probs),
              levels = 1:length(probs))
  tmp <- matrix(as.integer(table(x)), nrow = nrow(pmatrix))
  dimnames(tmp) <- dmnames
  tmp <- as.table(tmp)

  if (as.df){
    tmp <- as.data.frame(tmp)
    if (untable){
      tmp <- with(tmp, reshape::untable(tmp, Freq))
      tmp[ , "Freq"] <- NULL
      rownames(tmp) <- 1:dim(tmp)[1]
    }
    tmp
  } else if (addmargins) {
    addmargins(tmp)
  } else {
    tmp
  }
}

#
# gen2wayTable(n = 100)
# gen2wayTable(n = 100, nfixed = TRUE)
# gen2wayTable(n = 100, nfixed = TRUE, as.df = TRUE)
# gen2wayTable(n = 100, nfixed = TRUE, as.df = TRUE, untable = FALSE)
#
# w = matrix(c(8, 5, 3, 2, 5, 5), nrow = 2)
#
# B = gen2wayTable(n = 300, pmatrix = w, addmargins = FALSE)
# chisq.test(B)
#


`nsamp` <- function (n, k, replace = FALSE, ordered = FALSE){
  m <- length(n)
  if (length(k) != m)
    stop("number of urns doesn't equal number of sample sizes")
  if (length(replace) != m) {
    replace <- rep(replace, length.out = m)
  }
  if (length(ordered) != m) {
    ordered <- rep(ordered, length.out = m)
  }
  res <- c()
  for (i in 1:m) if (isTRUE(replace[i])) {
    if (isTRUE(ordered[i])) {
      res[i] <- n[i]^k[i]
    }
    else {
      res[i] <- choose(n[i] - 1 + k[i], k[i])
    }
  }
  else {
    if (isTRUE(ordered[i])) {
      res[i] <- factorial(n[i])/factorial(n[i] - k[i])
    }
    else {
      res[i] <- choose(n[i], k[i])
    }
  }
  return(res)
}



`permsn` <- function (x, m)
{

  # require(combinat)
  if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x)

    x <- seq(x)
  temp <- combn(x, m)
  if ( isTRUE(all.equal(m,1)) ) {

    P <- temp
  } else if (isTRUE(all.equal(m, length(x)))) {

    temp <- matrix(x, ncol = 1)
    P <- array(unlist(permn(temp[, 1])), dim = c(m, factorial(m)))
  } else {
    k <- dim(temp)[1]
    n <- dim(temp)[2]
    P <- array(unlist(permn(temp[, 1])), dim = c(k, factorial(k)))
    for (i in 2:n) {
      a <- temp[, i]
      perms <- array(unlist(permn(a)), dim = c(k, factorial(k)))
      P <- cbind(P, perms)
    }


  }
  return(P)
}


`Prob` <- function (x, ...)
  UseMethod("Prob")

`prob` <- function (x, ...){
  message("'prob' is deprecated; use 'Prob' instead.")
  Prob(x, ...)
}

`Prob.default` <- function (x, event = NULL, given = NULL, ...){
  if (is.null(x$probs)) {
    message("'space' is missing a probs column")
    stop("see ?probspace")
  }
  if (missing(event)) {
    r <- TRUE
  }
  else {
    e <- substitute(event)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r))
      stop("'event' must evaluate to logical")
    r <- r & !is.na(r)
    if (!isTRUE(all.equal(sum(x$probs), 1)))
      warning("'space' does not have probability 1.")
  }
  A <- x[r, ]
  if (missing(given)) {
    p <- sum(A$probs)
  }
  else {
    f <- substitute(given)
    g <- eval(f, x, enclos = parent.frame())
    if (!is.logical(g)) {
      if (!is.data.frame(given))
        stop("'given' must be data.frame or evaluate to logical")
      B <- given
    }
    else {
      if (missing(event))
        stop("'event' must be specified when 'given' is an expression")
      g <- g & !is.na(g)
      B <- x[g, ]
    }
    if (sum(B$probs <= 0))
      stop("prob(given) must be positive")
    p <- sum(intersect(A, B)$probs)/sum(B$probs)
  }
  return(p)
}


`Prob.ps` <- function (x, event = NULL, given = NULL, ...){
  if (is.null(x$probs)) {
    message("'space' is missing a probs component")
    stop("see ?probspace")
  }
  if (missing(event)) {
    A <- x
  }
  else {
    e <- substitute(event)
    r <- sapply(x$outcomes, function(t) {
      eval(e, t, enclos=parent.frame())
    })
    if (!is.logical(r))
      stop("'event' must evaluate to logical")
    r <- r & !is.na(r)
    if (!isTRUE(all.equal(sum(x$probs), 1)))
      warning("'space' does not have probability 1.")
    A <- list(outcomes = x$outcomes[r], probs = x$probs[r])
  }
  if (missing(given)) {
    p <- sum(A$probs)
  }
  else {
    f <- substitute(given)
    g <- sapply(x$outcomes, function(t) {
      eval(f, t, enclos=parent.frame())
    })
    if (!is.logical(g)) {
      if (!is.probspace(given))
        stop("'given' must be a probspace or evaluate to logical")
      B <- given
    }
    else {
      if (missing(event))
        stop("'event' must be specified when 'given' is an expression")
      g <- g & !is.na(g)
      B <- list(outcomes = x$outcomes[g], probs = x$probs[g])
    }
    if (sum(B$probs <= 0))
      stop("prob(given) must be positive")
    p <- sum(intersect(A, B)$probs)/sum(B$probs)
  }
  return(p)
}

