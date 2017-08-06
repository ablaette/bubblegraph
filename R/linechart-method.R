#' Line chart method
#' 
#' Draw a line chart
#' 
#' Suitable for time series data. If time series data is provided, dates 
#' should be in columns 
#' 
#' @param object data to be presented (data.frame)
#' @param col color for rows
#' @param ymax maximum y value for chart
#' @param ... further arguments
#' @return output is a plot
#' @author Andreas Blaette
#' @importFrom RColorBrewer brewer.pal
#' @exportMethod linechart
#' @name linechart
#' @rdname linechart
#' @examples
#' \dontrun{
#' library(polmineR)
#' bt <- partition("PLPRBTTXT", def=list(text_lp="17", text_type="speech"))
#' dist <- dispersion(bt, "Energiewende", c("text_year", "text_party"))
#' linechart(slot(dist, "abs"))
#' }
setGeneric("linechart", function(object, ...) standardGeneric("linechart"))

#' @rdname linechart
setMethod("linechart", "data.frame",function (object, col=NULL, ymax=NULL, ...) {
  if (is.null(col)){
    colors <- append(brewer.pal(9,"Set1"), brewer.pal(12,"Set3"))[1:nrow(object)]
  }
  if (is.null(ymax)){
    ylim <- c(0, max(object)[1])
  } else {
    ylim <- c(0,ymax)
  }
  plot(c(0), c(0), xlab=xlab, xlim=c(1, ncol(object)), xaxt="n", ylim=ylim, type="n", ...)
  axis(1, at=c(1:ncol(object)),labels=colnames(object))
  for (i in 1:nrow(object)) {
    lines(c(1:ncol(object)), object[i,], col=colors[i], lwd=3)
  }
  legend(c('top'), rownames(object), col=colors, ncol=2, cex=1, lwd=5)
})


