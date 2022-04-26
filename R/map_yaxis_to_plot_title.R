#' Map yaxis to plot title
#' @param y character string with y axis
#' 
map_yaxis_to_plot_title <- function(y) {
  
  if (y == "Bets") {
    z <- "Number of bets over time"
  } else if (y == "Stake") {
    z <- "Amount of money at stake over time"
  } else if (y == "Revenue") {
    z <- "Total revenue in kroner over time"
  } else if (y == "Earnings") {
    z <- "Total earnings in kroner over time"
  } else if (y == "Return") {
    z <- "Total return in % over time"
  } else {
    stop("y must be one of: Bets, Stake, Revenue, Earnings or Return")
  }
  
}
