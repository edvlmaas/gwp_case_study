#' Calculatates additional soil properties required for certain models
#'
#' @param my_soil_properties Soil properties from NATMAP adjusted to have the appropriate layers for your chosen model
#' @param model_name The abbreviated name of the model you are using. Possible values are "apsim"
#' @return A dataframe with all soil properties plus any addtional ones required for your chosen model
#' @export
#'
#' @examples
#' series_properties <- get_series_properties("BATCOMBE", "AR")
#' layer_matrix <- compare_soil_depths(series_properties, "apsim")
#' my_soil_properties <- adjust_for_layer_depth(series_properties, layer_matrix, "rlm")
#' calculate_additional_properties(my_soil_properties, "apsim")
calculate_additional_properties <- function(my_soil_properties, model_name) {
  if (model_name == "apsim") {
    my_soil_properties$om <- (my_soil_properties$oc * 1.72)
    my_soil_properties$sand_total <- my_soil_properties$sand_total / 100
    my_soil_properties$clay <- my_soil_properties$clay / 100
    predw <- -0.024 * my_soil_properties$sand_total +
      0.487 * my_soil_properties$clay + 0.006 * my_soil_properties$om +
      0.005 * my_soil_properties$sand_total * my_soil_properties$om -
      0.013 * my_soil_properties$clay * my_soil_properties$om +
      0.068 * my_soil_properties$sand_total * my_soil_properties$clay +
      0.031

    my_soil_properties$LL <- predw + 0.14 * predw - 0.02

    predy <- -0.251 * my_soil_properties$sand_total +
      0.195 * my_soil_properties$clay + 0.011 * my_soil_properties$om +
      0.006 * my_soil_properties$sand_total * my_soil_properties$om -
      0.027 * my_soil_properties$clay * my_soil_properties$om +
      0.452 * my_soil_properties$sand_total * my_soil_properties$clay +
      0.299
    predz <- predy + (1.283 * predy * predy - 0.374 * predy - 0.015)
    predaa <- 0.278 * my_soil_properties$sand_total +
      0.034 * my_soil_properties$clay + 0.022 * my_soil_properties$om -
      0.018 * my_soil_properties$sand_total * my_soil_properties$om -
      0.027 * my_soil_properties$clay * my_soil_properties$om -
      0.584 * my_soil_properties$sand_total * my_soil_properties$clay +
      0.078
    predab <- predaa + (0.636 * predaa - 0.107)
    predac <- predab + predz
    predad <- -0.097 * my_soil_properties$sand_total + 0.043
    predae <- predac + predad
    predaf <- (1 - predae) * 2.65
    predag <- predaf * (1)
    predai <- (1 - predag / 2.65) - (1 - predaf / 2.65)

    my_soil_properties$DUL <- predz + 0.2 * predai

    my_soil_properties$SAT <- 1 - (predag / 2.65)

    predah <- 1 - (predag / 2.65)
    predaj <- predz + 0.2 * predai
    predak <- predah - predaj
    predx <- predw + 0.14 * predw - 0.02
    predal <- (log(predaj) - log(predx)) / (log(1500) - log(33))
    predam <- (1 - 0) / (1 - 0 * (1 - 1.5 * (predag / 2.65)))
    my_soil_properties$KS <- 1930 * (predak)^(3 - predal) * predam * 24
  }
  return(my_soil_properties)
}
