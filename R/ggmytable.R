#' ggmytable
#'
#' @param df The desired data frame to be analyzed
#' @param x A qualitative variable designated to the x axis
#' @param y A qualitative variable to group the data
#' @param z A quantitative variable used to calculate mean within the summary
#'
#' @return A bar graph of x, facet wrapped with y, and a summary of function n along with the mean of z
#' @importFrom ggplot2 ggplot geom_bar facet_wrap ggtitle aes .data
#' @importFrom dplyr group_by summarize n
#' @export
#'
#' @examples
#' \dontrun{ggmytable(mtbe, "MTBE-Detect", "Aquifier", "MTBE-Level")}
ggmytable <- function(df, x, y, z){
  g <- df |>
    ggplot(aes(x = .data[[x]])) +
    geom_bar() +
    facet_wrap(~.data[[y]]) +
    ggtitle("Alex Force")
  print(g)
  df |>
    group_by(.data[[y]]) |>
    summarize(mean = mean(.data[[z]]), n = n())
}
