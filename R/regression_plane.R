#' Visualize a multiple regression model's predictions as a 3D plane
#'
#' @param model A fitted model object, e.g., the output from the lm() function.
#' @param n_points Integer, the number of steps from minimum to maximum value for which predicted values are generated from the model
#' @param mesh Logical, controls whether the regression plane has a mesh grid overlain on it
#' @param mesh_step step size for generating mesh grid lines
#'
#'
#' @importFrom plotly plot_ly
#' @importFrom plotly add_markers
#' @importFrom plotly layout
#' @importFrom plotly add_trace
#' @importFrom stats predict
#' @importFrom RColorBrewer brewer.pal
#'
#' @return a plotly plot object
#'
#' @export
#'
#' @examples
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' regression_plane(m)
#'

regression_plane <- function(model, n_points = 100, mesh=FALSE, mesh_step=1) {

  data <- model$model

  outcome_name <- as.character(model$terms[[2]])

  terms <- attr(model$terms, "term.labels")
  real_vars <- terms[!grepl(":", terms) &
                      !grepl("^I\\(.*\\^[[:digit:]]+\\)$", terms)
                    ]

  exp_var_types <- attr(model$terms, "dataClasses")[real_vars]
  numeric_vars <- names(exp_var_types)[exp_var_types == "numeric"]
  cat_var <- setdiff(names(exp_var_types), numeric_vars)

  if (length(cat_var) == 0) {
    cat_var <- NULL
  }

  if (!is.null(cat_var)) {
  cat_levels <- unique(data[[cat_var]])

  color_pallete <- suppressWarnings(RColorBrewer::brewer.pal(length(cat_var), "Set2"))
  names(color_pallete) <- cat_levels
  }

  if (length(numeric_vars) != 2) {
    stop("Only models with two numeric variables should be visualized using a plane.")
  }

  p <- plotly::plot_ly(x = data[[numeric_vars[1]]],
                       y = data[[numeric_vars[2]]],
                       z = data[[outcome_name]],
                       color = if (is.null(cat_var)) NULL else { data[[cat_var]] },
                       colors = if (is.null(cat_var)) NULL else { color_pallete },
                       opacity = 0.6
                       ) |>
    plotly::add_markers(marker = list(size=3), hoverinfo='none') |>
    plotly::layout(scene = list(camera = list(eye = list(x = -1.25, y = -1.25, z = 1.25)),
                        xaxis = list(title = numeric_vars[1]),
                        yaxis = list(title = numeric_vars[2]),
                        zaxis = list(title = outcome_name)
                        )
           )

  x_range <- range(data[[numeric_vars[1]]])
  y_range <- range(data[[numeric_vars[2]]])

  x <- seq(x_range[1], x_range[2], length = n_points)
  y <- seq(y_range[1], y_range[2], length = n_points)

  if (is.null(cat_var)) {
    predictor.grid <- expand.grid(x, y)
  } else {
    predictor.grid <- expand.grid(x, y, cat_levels, stringsAsFactors = FALSE)
  }

  colnames(predictor.grid) <- c(numeric_vars, cat_var)

  predictor.grid <- cbind(predictor.grid,
                          z = stats::predict(model, newdata = predictor.grid)
                          )

  if (is.null(cat_var)) {

    z_hat <- matrix(predictor.grid$z, nrow=n_points, byrow = TRUE)
    p <- plotly::add_surface(p, x = ~x, y = ~y, z = ~z_hat,
                             showlegend = FALSE, hoverinfo = 'none', showscale = FALSE
                             )

  } else {

    for (cat in cat_levels) {
      z_hat <- matrix(predictor.grid$z[predictor.grid[[cat_var]] == cat],
                      nrow=n_points, byrow = TRUE
                      )

      p <- plotly::add_surface(p, x = x, y= y, z = z_hat,
                               colorscale = list(c(0,1), c(color_pallete[cat], color_pallete[cat])),
                               showlegend = FALSE, hoverinfo = 'none', showscale = FALSE
                               )
    }

  }


  if (mesh) {
    x2 <- seq(x_range[1], x_range[2], by=mesh_step)
    y2 <- seq(y_range[1], y_range[2], by=mesh_step)

    if (is.null(cat_var)) {
      predictor.grid <- expand.grid(x2, y2)
    } else {
      predictor.grid <- expand.grid(x2, y2, cat_levels, stringsAsFactors = FALSE)
    }

    colnames(predictor.grid) <- c(numeric_vars, cat_var)

    predictor.grid <- cbind(predictor.grid,
                            z = stats::predict(model, newdata = predictor.grid)
                            )

    if (is.null(cat_var)) {

      z_hat2 <- matrix(predictor.grid$z, nrow=length(y2), byrow = TRUE)

      for (i in 1:ncol(z_hat2)) {
        p <- p |> plotly::add_trace(type="scatter3d", x=x2[i], y=y2, z=z_hat2[,i],
                                     color = NULL,
                                     mode="lines", line = list(color = "black"),
                                     showlegend=FALSE
        )
      }

      for (j in 1:nrow(z_hat2)){
        p <- p |> add_trace(type="scatter3d", x=x2, y=y2[j], z=z_hat2[j,],
                             color = NULL,
                             mode="lines", line = list(color = "black"),
                             showlegend=FALSE
        )
      }

    } else {

      for (cat in cat_levels) {

        z_hat2 <- matrix(predictor.grid$z[predictor.grid[[cat_var]] == cat],
                         nrow = length(y2), byrow = TRUE
                         )

        for (i in 1:ncol(z_hat2)) {
          p <- p |> plotly::add_trace(type="scatter3d", x=x2[i], y=y2, z=z_hat2[,i],
                                       color = NULL,
                                       mode="lines", line = list(color = "black"),
                                       showlegend=FALSE
                                       )
        }

        for (j in 1:nrow(z_hat2)){
          p <- p |> add_trace(type="scatter3d", x=x2, y=y2[j], z=z_hat2[j,],
                               color = NULL,
                               mode="lines", line = list(color = "black"),
                               showlegend=FALSE
                               )
        }

      }

    }

  }

  return(p)

}



