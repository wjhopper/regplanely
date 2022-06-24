#' @importFrom magrittr %>%
#' @importFrom plotly plot_ly
#' @importFrom plotly add_markers
#' @importFrom plotly layout
#' @importFrom plotly add_trace
#' @importFrom stats predict
#'
regression_plane <- function(model, n_points = 100, mesh=FALSE, mesh_step=1) {

  outcome_name <- as.character(model$terms)[[2]]
  terms <- attr(attr(model$model, "terms"), "term.labels")

  if (length(terms) < 2) {
    stop("Only models with two explanatory variables should be visualized using a plane.")
  }

  data <- model$model

  p <- plotly::plot_ly(x = data[[terms[1]]],
               y = data[[terms[2]]],
               z = data[[outcome_name]],
               opacity = 0.6
               ) %>%
    plotly::add_markers(marker = list(size=3), showlegend=FALSE, hoverinfo='none') %>%
    plotly::layout(scene = list(camera = list(eye = list(x = -1.25, y = -1.25, z = 1.25)),
                        xaxis = list(title = terms[1]),
                        yaxis = list(title = terms[2]),
                        zaxis = list(title = outcome_name)
                        )
           )

  x <- seq(min(data[[terms[1]]]),  max(data[[terms[1]]]), length = n_points)
  y <- seq(min(data[[terms[2]]]),  max(data[[terms[2]]]), length = n_points)
  predictor.grid <- expand.grid(x, y)
  colnames(predictor.grid) <- terms[!grepl(":", terms)]

  z_hat <- stats::predict(model, newdata = predictor.grid)
  z_hat <- matrix(z_hat, nrow=n_points, byrow = TRUE)

  p <- plotly::add_surface(p, x= ~x, y=~y, z=~z_hat, showlegend=FALSE, hoverinfo='none', showscale = FALSE, hoverinfo='none')

  if (mesh) {
    x2 <- seq(min(data[[terms[1]]]),  max(data[[terms[1]]]), by=mesh_step)
    y2 <- seq(min(data[[terms[2]]]),  max(data[[terms[2]]]), by=mesh_step)

    predictor.grid2 <- expand.grid(x2, y2)
    colnames(predictor.grid2) <- terms[!grepl(":", terms)]

    z_hat2 <- predict(model, newdata = predictor.grid2)
    z_hat2 <- matrix(z_hat2, nrow=length(y2), byrow = TRUE)

    for (i in 1:ncol(z_hat2)) {
      p <- p %>% plotly::add_trace(type="scatter3d", x=x2[i], y=y2, z=z_hat2[,i],
                           mode="lines", line = list(color = "black"),
                           showlegend=FALSE
      )
    }

    for (j in 1:nrow(z_hat2)){
      p <- p %>% add_trace(type="scatter3d", x=x2, y=y2[j], z=z_hat2[j,],
                           mode="lines", line = list(color = "black"),
                           showlegend=FALSE
      )
    }
  }

  return(p)

}
