
#' Calculate species importance within inventory plots
#'
#' Internal function used by [get_dominant_sps()] and [get_main_sps()]
#' to calculate the absolute and relative importance of each tree
#' species within individual inventory plots.
#'
#' @param df A data frame containing tree-level forest inventory data.
#'   It must include the plot code, species name, tree diameter,
#'   tree density, province and autonomous community.
#' @param criterion Character string specifying the importance criterion.
#'   Either `"BA"` (basal area) or `"N"` (number of trees).
#' @param province Optional character vector specifying the provinces
#'   to include.
#' @param com_aut Optional character vector specifying the autonomous
#'   communities to include.
#'
#' @return A data frame with one row per species and inventory plot.
#'   It includes:
#'   \describe{
#'     \item{Codigo}{Inventory plot code.}
#'     \item{Prov_Name}{Province.}
#'     \item{CCAA}{Autonomous community.}
#'     \item{Sps_name}{Tree species name.}
#'     \item{value}{Absolute importance, expressed as basal area
#'     (m²/ha) or number of trees (trees/ha).}
#'     \item{total}{Total basal area or number of trees in the plot.}
#'     \item{importance}{Relative importance of the species within
#'     the plot, expressed as a percentage.}
#'   }
#'
#' @details
#' Species importance is calculated by aggregating tree-level values
#' by species and plot.
#'
#' For the `"BA"` criterion, basal area is calculated from diameter
#' and tree density:
#' \deqn{BA = \frac{\pi D^2 N}{4}}
#' where diameter is expressed in metres and density in trees per hectare.
#'
#' For the `"N"` criterion, tree density is summed directly.
#'
#' Relative importance is calculated as:
#' \deqn{Importance = 100 \frac{value}{total}}
#'
#' The optional geographical filters are applied before calculating
#' species importance.
#'
#' This is an internal function and is not exported.
#'
#' @keywords internal

.get_species_importance <- function(df, criterion,
                                    province = NULL, com_aut = NULL) {

  # Check required columns
  required_cols <- c("Codigo", "Sps_name", "Dn", "N",
                     "Prov_Name", "CCAA")

  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop("Missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # Check criterion
  if (length(criterion) != 1 ||
      !criterion %in% c("BA", "N")) {
    stop("criterion must be either 'BA' or 'N'.")
  }

  # Filter by autonomous community
  if (!is.null(com_aut)) {

    invalid_com <- setdiff(com_aut, unique(df$CCAA))

    if (length(invalid_com) > 0) {
      stop("Unknown autonomous community: ",
           paste(invalid_com, collapse = ", "))
    }

    df <- df |>
      dplyr::filter(CCAA %in% com_aut)
  }

  # Filter by province
  if (!is.null(province)) {

    invalid_prov <- setdiff(province, unique(df$Prov_Name))

    if (length(invalid_prov) > 0) {
      stop("Unknown province: ",
           paste(invalid_prov, collapse = ", "))
    }

    df <- df |>
      dplyr::filter(Prov_Name %in% province)
  }

  # Calculate selected criterion
  if (criterion == "BA") {
    df <- df |>
      dplyr::mutate(value = pi * (Dn / 100)^2 * N / 4)
  } else {
    df <- df |>
      dplyr::mutate(value = N)
  }

  # Calculate species importance within each plot
  result <- df |>
    dplyr::group_by(Codigo, Prov_Name, CCAA, Sps_name) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(Codigo) |>
    dplyr::mutate(
      total = sum(value, na.rm = TRUE),
      importance = 100 * value / total
    ) |>
    dplyr::ungroup()

  return(result)
}


#' Identify plots dominated by a given tree species
#'
#' Identifies the inventory plots in which a given tree species
#' reaches or exceeds a specified percentage of the total basal
#' area or number of trees.
#'
#' @param df A data frame containing tree-level forest inventory data.
#' @param species Character string with the species name to be evaluated.
#' @param criterion Character string specifying the dominance criterion.
#'   Either `"BA"` (basal area) or `"N"` (number of trees).
#' @param threshold Numeric value (0–100) specifying the minimum
#'   percentage of dominance required.
#' @param province Optional character vector specifying the provinces
#'   to include.
#' @param com_aut Optional character vector specifying the autonomous
#'   communities to include.
#'
#' @return A data frame containing the plots where the selected species
#'   reaches or exceeds the specified threshold. It includes the plot code
#'   (`Codigo`), province (`Prov_Name`), autonomous community (`CCAA`),
#'   and percentage of dominance (`Dominance`).
#'
#' @details
#' Dominance is calculated separately for each inventory plot as the
#' percentage contribution of the selected species to the total basal
#' area or number of trees, depending on the selected criterion.
#'
#' Basal area is calculated from tree diameter and density as:
#' \deqn{BA = \frac{\pi D^2 N}{4}}
#' where diameter is expressed in metres and density in trees per hectare.
#'
#' @examples
#' data(IFN4_example)
#'
#' dominant_plots <- get_dominant_sps(
#'   df = IFN4_example,
#'   species = "Pinus sylvestris",
#'   criterion = "BA",
#'   threshold = 50
#' )
#'
#' @export

get_dominant_sps <- function(df, species, criterion, threshold,
                             province = NULL, com_aut = NULL) {

  # Check threshold
  if (length(threshold) != 1 ||
      !is.numeric(threshold) ||
      is.na(threshold) ||
      threshold < 0 || threshold > 100) {
    stop("threshold must be a numeric value between 0 and 100.")
  }

  # Calculate species importance
  importance <- .get_species_importance(
    df = df,
    criterion = criterion,
    province = province,
    com_aut = com_aut
  )

  # Check species availability
  if (!species %in% unique(importance$Sps_name)) {
    stop("Species '", species,
         "' not found in the selected data.")
  }

  # Select plots where species reaches the threshold
  result <- importance |>
    dplyr::filter(
      Sps_name == species,
      importance >= threshold,
      is.finite(importance)
    ) |>
    dplyr::select(
      Codigo, Prov_Name, CCAA,
      Dominance = importance
    )

  return(result)
}

#' Identify the main tree species in each inventory plot
#'
#' Identifies and ranks the most important tree species within each
#' inventory plot, according to basal area or number of trees.
#'
#' @param df A data frame containing tree-level forest inventory data.
#' @param criterion Character string specifying the importance criterion.
#'   Either `"BA"` (basal area) or `"N"` (number of trees).
#' @param n_species Optional positive integer specifying the maximum
#'   number of species to retain individually in each plot.
#' @param threshold Optional numeric value (0–100) specifying the
#'   minimum percentage of importance required for a species to be
#'   retained individually.
#' @param province Optional character vector specifying the provinces
#'   to include.
#' @param com_aut Optional character vector specifying the autonomous
#'   communities to include.
#'
#' @return A data frame containing the species composition of each plot.
#'   It includes the plot code (`Codigo`), province (`Prov_Name`),
#'   autonomous community (`CCAA`), species name (`Sps_name`),
#'   absolute importance (`value`) and relative importance (`importance`).
#'
#' @details
#' Species importance is calculated separately for each inventory plot
#' as the percentage contribution of each species to the total basal
#' area or number of trees.
#'
#' The function allows three selection criteria:
#' \itemize{
#'   \item If only `n_species` is specified, the most important species
#'   are retained individually.
#'   \item If only `threshold` is specified, all species reaching the
#'   specified percentage are retained individually.
#'   \item If both are specified, species must satisfy both conditions.
#' }
#'
#' Species not retained individually are grouped into an additional
#' category called `"Other"`. If neither `n_species` nor `threshold`
#' is specified, all species are returned individually.
#'
#' @examples
#' data(IFN4_example)
#'
#' # Three most important species in each plot
#' main_species <- get_main_sps(
#'   df = IFN4_example,
#'   criterion = "BA",
#'   n_species = 3
#' )
#'
#' # All species representing at least 10% of basal area
#' main_species_threshold <- get_main_sps(
#'   df = IFN4_example,
#'   criterion = "BA",
#'   threshold = 10
#' )
#'
#' @export
#'

get_main_sps <- function(df, criterion, n_species = NULL,
                         threshold = NULL,
                         province = NULL, com_aut = NULL) {

  # Check n_species
  if (!is.null(n_species)) {
    if (length(n_species) != 1 ||
        !is.numeric(n_species) ||
        is.na(n_species) ||
        n_species < 1 ||
        n_species != as.integer(n_species)) {
      stop("n_species must be NULL or a positive integer.")
    }
  }

  # Check threshold
  if (!is.null(threshold)) {
    if (length(threshold) != 1 ||
        !is.numeric(threshold) ||
        is.na(threshold) ||
        threshold < 0 || threshold > 100) {
      stop("threshold must be NULL or a numeric value between 0 and 100.")
    }
  }


  # Calculate species importance
  result <- .get_species_importance(
    df = df,
    criterion = criterion,
    province = province,
    com_aut = com_aut
  )

  # Select main species within each plot
  result <- result |>
    dplyr::group_by(Codigo) |>
    dplyr::arrange(dplyr::desc(importance), .by_group = TRUE) |>
    dplyr::mutate(
      rank = dplyr::row_number(),
      is_main = TRUE
    )

  # Apply n_species criterion
  if (!is.null(n_species)) {
    result <- result |>
      dplyr::mutate(
        is_main = is_main & rank <= n_species
      )
  }

  # Apply threshold criterion
  if (!is.null(threshold)) {
    result <- result |>
      dplyr::mutate(
        is_main = is_main & importance >= threshold
      )
  }

  # Group non-dominant species into "Other"
  result <- result |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Sps_name = dplyr::if_else(is_main, Sps_name, "Other")
    ) |>
    dplyr::group_by(Codigo, Prov_Name, CCAA, Sps_name) |>
    dplyr::summarise(
      value = sum(value),
      importance = sum(importance),
      .groups = "drop"
    ) |>
    dplyr::group_by(Codigo) |>
    dplyr::arrange(dplyr::desc(importance), .by_group = TRUE) |>
    dplyr::ungroup()

  return(result)
}
