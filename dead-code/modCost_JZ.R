#' Markov Chain parameter estimates
#'
#' \code{modCost_JZ} Computes and Markov Chain Monte Carlo parameter estimate for a given model.  This is a change from the code provided in the FME package to account for the likelihood.
#'


#'
#' @import FME
#' @export


modCost_JZ<- function(model, obs, x = "time", y = NULL, err = NULL, weight = "none",
                      scaleVar = FALSE, cost = NULL, ...)
{
  if (is.vector(obs)) {
    cn <- names(obs)
    obs <- matrix(data = obs, nrow = 1)
    colnames(obs) <- cn
  }
  if (is.vector(model)) {
    cn <- names(model)
    model <- matrix(data = model, nrow = 1)
    colnames(model) <- cn
  }
  ix <- 0
  if (!is.null(x)) {
    if (length(x) > 1)
      stop("multiple independent variables in 'obs' are not yet supported")
    if (!is.character(x))
      stop("'x' should be the *name* of the column with the independent variable in 'obs' or NULL")
    ix <- which(colnames(obs) %in% x)
    if (length(ix) != length(x))
      stop(paste("Independent variable column not found in observations",
                 x))
  }
  else ix <- NULL
  ierr <- 0
  if (!is.null(err)) {
    if (!is.character(err))
      stop("'err' should be the *name* of the column with the error estimates in obs or NULL")
    ierr <- which(colnames(obs) == err)
    if (length(ierr) == 0)
      stop(paste("Column with error estimates not found in observations",
                 err))
  }
  type <- 1
  if (!is.null(y)) {
    Names <- as.character(unique(obs[, 1]))
    Ndat <- length(Names)
    ilist <- 1:Ndat
    if (!is.character(y))
      stop("'y' should be the *name* of the column with the values of the dependent variable in obs")
    iy <- which(colnames(obs) == y)
    if (length(iy) == 0)
      stop(paste("Column with value of dependent variable not found in observations",
                 y))
    type <- 2
  }
  else {
    Ndat <- NCOL(obs) - 1
    Names <- colnames(obs)
    ilist <- (1:NCOL(obs))
    exclude <- ix
    if (ierr > 0)
      exclude <- c(ix, ierr)
    if (length(exclude) > 0)
      ilist <- ilist[-exclude]
  }
  ModNames <- colnames(model)
  if (length(ix) > 1) {
    ixMod <- NULL
    for (i in 1:length(ix)) {
      ix2 <- which(colnames(model) == x[i])
      if (length(ix2) == 0)
        stop(paste("Cannot calculate cost: independent variable not found in model output",
                   x[i]))
      ixMod <- c(ixMod, ix2)
    }
    xMod <- model[, ixMod]
  }
  else if (length(ix) == 1) {
    ixMod <- which(colnames(model) == x)
    if (length(ixMod) == 0)
      stop(paste("Cannot calculate cost: independent variable not found in model output",
                 x))
    xMod <- model[, ixMod]
  }
  Residual <- NULL
  CostVar <- NULL
  xDat <- 0
  iDat <- 1:nrow(obs)
  for (i in ilist) {
    ii <- which(ModNames == Names[i])
    if (length(ii) == 0)
      stop(paste("observed variable not found in model output",
                 Names[i]))
    yMod <- model[, ii]
    if (type == 2) {
      iDat <- which(obs[, 1] == Names[i])
      if (length(ix) > 0)
        xDat <- obs[iDat, ix]
      obsdat <- obs[iDat, iy]
    }
    else {
      if (length(ix) > 0)
        xDat <- obs[, 1]
      obsdat <- obs[, i]
    }
    ii <- which(is.na(obsdat))
    if (length(ii) > 0) {
      xDat <- xDat[-ii]
      obsdat <- obsdat[-ii]
    }
    if (length(ix) > 0)
      ModVar <- approx(xMod, yMod, xout = xDat)$y
    else {
      ModVar <- mean(yMod)
      obsdat <- mean(obsdat)
    }
    iex <- which(!is.na(ModVar))
    ModVar <- ModVar[iex]
    obsdat <- obsdat[iex]
    xDat <- xDat[iex]
    if (ierr > 0) {
      Err <- obs[iDat, ierr]
      Err <- Err[iex]
    }
    else {
      if (weight == "std")
        Err <- sd(obsdat)
      else if (weight == "mean")
        Err <- mean(abs(obsdat))
      else if (weight == "none")
        Err <- 1
      else stop("error: do not recognize 'weight'; should be one of 'none', 'std', 'mean'")
    }
    if (any(is.na(Err)))
      stop(paste("error: cannot estimate weighing for observed variable: ",
                 Names[i]))
    if (min(Err) <= 0)
      stop(paste("error: weighing for observed variable is 0 or negative:",
                 Names[i]))
    if (scaleVar)
      Scale <- 1/length(obsdat)
    else Scale <- 1
    Res <- (ModVar - obsdat)
    res <- Res/Err
    resScaled <- res * Scale
    Residual <- rbind(Residual, data.frame(name = Names[i],
                                           x = xDat, obs = obsdat, mod = ModVar, weight = 1/Err,
                                           res.unweighted = Res, res = res))
    CostVar <- rbind(CostVar, data.frame(name = Names[i],
                                         scale = Scale, N = length(Res), SSR.unweighted = sum(Res^2),
                                         SSR.unscaled = sum(res^2), SSR = sum(resScaled^2)))
  }
  Cost <- sum(CostVar$SSR * CostVar$scale)
  #Lprob <- -sum(log(pmax(0, dnorm(Residual$mod, Residual$obs,
  #                                1/Residual$weight))))
  Lprob <- -sum(dnorm(Residual$mod, Residual$obs,1/Residual$weight,log=TRUE))
  if (!is.null(cost)) {
    Cost <- Cost + cost$model
    CostVar <- rbind(CostVar, cost$var)
    Residual <- rbind(Residual, cost$residuals)
    Lprob <- Lprob + cost$minlogp
  }
  out <- list(model = Cost, minlogp = Lprob, var = CostVar,
              residuals = Residual)
  class(out) <- "modCost"
  return(out)
}
