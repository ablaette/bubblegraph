#' @rdname bubblegraph
setGeneric("bubblegraph", function(object, ...){standardGeneric("bubblegraph")})

#' bubblegraph visualisation
#' 
#' visualise the distribution of counts/frequencies
#' 
#' @param object a data.frame
#' @param col color, either data.frame or vector
#' @param rex a radius expansion factor to control the bubble size
#' @param leftMargin adjust left margin of the plot
#' @param bottomMargin adjust bottom margin of the plot
#' @param cex character expansion factor
#' @param font set font parameter from par
#' @param ... further parameters that will be passed to the plot function
#' @return a nice plot, hopefully
#' @exportMethod bubblegraph
#' @examples
#' \dontrun{
#' library(polmineR)
#' bt <- partition("PLPRBTTXT", def=list(text_lp="17", text_type="speech"))
#' dist <- dispersion(bt, "Energiewende", c("text_year", "text_party"))
#' bubblegraph(slot(dist, "abs"))
#' }
#' @docType methods
#' @rdname bubblegraph
#' @name bubblegraph
#' @aliases bubblegraph bubblegraph,matrix-method bubblegraph,data.frame-method
setMethod("bubblegraph", "matrix", function(object, col="darkgrey", rex=1, leftMargin=1.2, bottomMargin=1.6, cex=1, font=1, ...) {
  par(mai=c(bottomMargin,leftMargin, 0.2, 0.2), mfcol=c(1,1), cex.axis=0.8)
  xrange <- c(1, ncol(object))
  yrange <- c(1, nrow(object))
  plot(c(1,1), type = "n", xlab=c(""), ylab=c(""),
       xlim=c(xrange[1]-0.5, xrange[2]+0.5),
       ylim=c(yrange[1]-0.5, yrange[2]+0.5),
       axes=F, main=c(""), ...)
  box(lwd=2.0)
  par(cex=cex, font.axis=font)
  axis(side=2, labels=rownames(object), tick=TRUE, at=c(yrange[1]:yrange[2]), las=2)
  axis(side=1, labels=colnames(object), tick=TRUE, at=c(xrange[1]:xrange[2]), las=2)
  grid(col = "lightgray", lty = "dotted", lwd=1.5)  
  radius <- sqrt(object/pi)
  radius <- (radius/max(radius, na.rm=TRUE))/2 # Normalisierung der Radien
  radius <- radius * rex
  for ( i in 1:nrow(radius) ) { radius[i,] <- sapply(radius[i,], FUN = function(x){ if (is.nan(x)) {0} else {x}})}
  for (i in 1:(nrow(object))) {
    if (!is.null(nrow(col))){
      if (nrow(col) == nrow(object)){
        col <- col[i,]
      } else if (nrow(col) == 1) {
        col <- col[1,]
      } else {
        warning("number of columns in the col object mismatch")
      }
    } else if (length(col) == ncol(object)){
      col <- col
    }
    symbols(c(1:xrange[2]), rep(i, ncol(object)), circles=radius[i,], add=TRUE, bg=col, fg=col, inches=FALSE)
  }
})

#' @rdname bubblegraph
setMethod("bubblegraph", "data.frame", function(object, col="darkgrey", rex=1, leftMargin=1.2, bottomMargin=1.6, cex=1, font=1, ...) {
  bubblegraph(as.matrix(object), col=col, rex=rex, leftMargin=leftMargin, bottomMargin=bottomMargin, cex=cex, font=font, ...)
})

