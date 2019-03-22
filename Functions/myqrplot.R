myqrplot <- function (x, parm = NULL, level = 0.95, ols = TRUE,
          mar = NULL, ylim = NULL, main = NULL, col = gray(c(0, 0.75)), 
          border = NULL, lcol = 2, lty = 1:2, cex = 1, pch = 20, 
          type = "b", xlab = "", ylab = "",axes=NULL,lwd=2,mgp=c(4,1,0), ...) {
  zalpha <- qnorm(1 - (1 - level)/2)
  taus <- sapply(x, function(x) x$tau)
  cf <- lapply(x, coef)
  if (ncol(cf[[1]]) == 4) {
    for (i in 1:length(cf)) {
      cfi <- cf[[i]]
      cfi <- cbind(cfi[, 1], cfi[, 1] - cfi[, 2] * zalpha, 
                   cfi[, 1] + cfi[, 2] * zalpha)
      colnames(cfi) <- c("coefficients", "lower bd", "upper bd")
      cf[[i]] <- cfi
    }
  }
  if (ncol(cf[[1]]) != 3) 
    stop("summary.rqs components have wrong dimension")
  if (is.null(parm)) 
    parm <- rownames(cf[[1]])
  if (is.null(axes)) 
    axes <- "TRUE"
  if (is.numeric(parm)) 
    parm <- rownames(cf[[1]])[parm]
  cf <- lapply(cf, function(x) x[parm, , drop = FALSE])
  names(cf) <- paste("tau=", taus)
  if (ols) {
    obj <- x[[1]]
    mt <- terms(obj)
    mf <- model.frame(obj)
    y <- model.response(mf)
    X <- model.matrix(mt, mf, contrasts = obj$contrasts)
    olscf <- summary(lm(y ~ X))$coefficients
    rownames(olscf) <- rownames(coef(obj))
    olscf <- cbind(olscf[parm, 1, drop = FALSE], olscf[parm, 
                                                       1, drop = FALSE] - olscf[parm, 2, drop = FALSE] * 
                     zalpha, olscf[parm, 1, drop = FALSE] + olscf[parm, 
                                                                  2, drop = FALSE] * zalpha)
    colnames(olscf) <- c("coefficients", "lower bd", "upper bd")
  }
  mar_orig <- par("mar")
  if (is.null(mar)) 
    mar <- c(5, 5, 2, 2)
  par(mar = mar,cex=cex,lwd=lwd)
  col <- rep(col, length.out = 2)
  lty <- rep(lty, length.out = 2)
  if (is.null(border)) 
    border <- col[2]
  if (is.null(main)) 
    main <- parm
  main <- rep(main, length.out = length(parm))
  xlab <- rep(xlab, length.out = length(parm))
  ylab <- rep(ylab, length.out = length(parm))
  ylim0 <- ylim
  for (i in seq(along = parm)) {
    b <- t(sapply(seq(along = cf), function(tau) cf[[tau]][i, 
                                                           ]))
 if (is.null(ylim)) {
   if (ols)
      ylim <- range(c(b[, 2], b[, 3], olscf[i]))
    else ylim <- range(b[, 2], b[, 3])
   }
    plot(rep(taus, 2), c(b[, 2], b[, 3]), type = "n", ylim = ylim, xlim=c(0,1),
         xlab = xlab[i], ylab = "", main = main,axes=axes,cex=cex)
    title(ylab = ylab[i], mgp = mgp)
    polygon(c(taus, rev(taus)), c(b[, 2], rev(b[, 3])), col = col[2], 
            border = border)
    points(taus, b[, 1], cex = 1, pch = pch, type = type, 
           col = col[1], ...)
    axis(1, at = seq(0,1,0.1), labels = seq(0,1,0.1),mgp=c(4,1,0))
    axis(2, at = seq(ylim[1],ylim[2],0.05), labels = seq(ylim[1],ylim[2],0.05),las=1,mgp=c(5,1,0))
    if (ols) {
      clip(0, 1, -100, 100)
      abline(h = olscf[i, 1], col = lcol, lty = lty[1])
      abline(h = olscf[i, 2], col = lcol, lty = lty[2])
      abline(h = olscf[i, 3], col = lcol, lty = lty[2])
    }
    abline(h = 0, col = gray(0.3))
    ylim <- ylim0
  }
  par(mar = mar,mgp=c(3,1,0))
  if (ols) 
    x <- c(cf, list(ols = olscf))
  else x <- cf
  invisible(structure(as.vector(unlist(x)), .Dim = c(dim(x[[1]]), 
                                                     length(x)), .Dimnames = list(rownames(x[[1]]), colnames(x[[1]]), 
                                                                                  names(x))))
}


save(myqrplot,file="~/Dropbox/Research Seminar/R/Functions/myqrplot.Rda")

