
#' Prepare and group forest inventory data
#'
#' Internal function used by [get_BA()], [get_N()], [get_diam()]
#' and [get_hei()] to filter and group tree-level forest inventory
#' data before calculating stand attributes.
#'
#' @param IFN Character string indicating the National Forest Inventory
#'   version. Either `"IFN3"` or `"IFN4"`.
#' @param df Data frame containing tree-level forest inventory data.
#' @param province Optional character vector specifying the provinces
#'   to include.
#' @param com_aut Optional character vector specifying the autonomous
#'   communities to include.
#' @param species Optional character vector specifying the tree species
#'   to include, using species names (`Sps_name`).
#' @param per_CD Logical. If `TRUE`, data are additionally grouped by
#'   diameter class.
#' @param per_species Logical. If `TRUE`, data are additionally grouped
#'   by species.
#'
#' @return A grouped data frame (tibble) containing the filtered
#'   inventory records. The grouping structure depends on the values
#'   of `per_CD` and `per_species`.
#'
#' @details
#' The function performs the following operations:
#'
#' \enumerate{
#'   \item Filters invalid or unidentified tree records according to
#'   the selected inventory version. For IFN3, records with missing
#'   species codes and specified invalid `OrdenIf3` values are excluded.
#'   For IFN4, records with missing species codes and specified invalid
#'   `OrdenIf4` values are excluded.
#'   \item Calculates diameter classes (`CD`) using 5 cm intervals,
#'   starting at 7.5 cm, with class labels from 10 to 70 cm.
#'   \item Calculates basal area per hectare (`BA`) from tree diameter
#'   and density:
#'   \deqn{BA = \frac{\pi D^2 N}{4}}
#'   where diameter is expressed in metres and density in trees per hectare.
#'   \item Applies optional filters by autonomous community, province
#'   and species.
#'   \item Groups the resulting data according to the selected options.
#' }
#'
#' The grouping variables are:
#' \itemize{
#'   \item Always: province, autonomous community, plot information
#'   and plot code.
#'   \item Additionally, species identifiers and names when
#'   `per_species = TRUE`.
#'   \item Additionally, diameter class when `per_CD = TRUE`.
#' }
#'
#' This is an internal function and is not exported.
#'
#' @keywords internal

.get_groups <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                   per_CD = FALSE, per_species = FALSE) {


    if (IFN == "IFN4") {
      df <- df |>
        drop_na(Especie) |>
        filter(OrdenIf4 != "888",
               OrdenIf4 != "444",
               OrdenIf4 != "999",
               OrdenIf4 != "000",
               OrdenIf4 != "555",
               OrdenIf4 != "666")
    }

    if (IFN == "IFN3") {
      df <- df |>
        drop_na(Especie) |>
        filter(OrdenIf3 != "888",
               OrdenIf3 != "999",
               OrdenIf3 != "000")
      }

    df <- df |>
      mutate(CD = as.numeric(as.character(
        cut(Dn, breaks = c(seq(7.5, 67.5, 5), Inf),
            labels = seq(10,70,5), include.lowest = T))),
        BA = pi*(Dn/100)^2*N/4)

      if(!is.null(com_aut)) {
      assertthat::assert_that(all(com_aut %in% unique(df$CCAA)),
                              msg = paste0("The CCAA ", com_aut[!com_aut %in% unique(df$CCAA)], " does not exist in the database. Type `com_aut` to see the available options" ))
    #     if(all(com_aut %in% unique(df$CCAA))) df <- df |> filter(CCAA %in% com_aut)
    #     else  stop("The CCAA ", com_aut[!com_aut %in% unique(df$CCAA)], " does not exist in the database. Type `com_aut` to see the available options")
      df <- df |> filter(CCAA %in% com_aut)
      }

    if(!is.null(province)) {
        if(all(province %in% unique(df$Prov_Name))) df <- df |> filter(Prov_Name %in% province)
        else  stop("The province ", province[!province %in% unique(df$Prov_Name)], " does not exist in the database. Type `province` to see the available options")
    }

    if(!is.null(species)) {
        if(all(species %in% unique(df$Sps_name))) df <- df |> filter(Sps_name %in% species)
        else  stop("The species ", species[!species %in% unique(df$Sps_name)], " does not exist in the database. Type `Species` to see the available options")
    }

    if(isTRUE(per_species) & isTRUE(per_CD)) {
        df |>
            group_by(Provincia, Prov_Name, CCAA,
                     Estadillo, Codigo, Tipo_Parc,
                     Especie, Sps_name, CD)
    } else if (isTRUE(per_species)) {
        df |>
            group_by(Provincia, Prov_Name, CCAA,
                     Estadillo, Codigo,Tipo_Parc,
                     Especie, Sps_name)

    } else if (isTRUE(per_CD)) {
        df |>
            group_by(Provincia, Prov_Name, CCAA,
                     Estadillo, Codigo, Tipo_Parc,
                     CD)
    } else {
        df |>
            group_by(Provincia, Prov_Name, CCAA,
                     Estadillo, Codigo,Tipo_Parc)
    }

}


#' Calculate basal area in forest inventory plots
#'
#' Calculates the total basal area (BA, m2/ha) for each inventory plot,
#' optionally disaggregated by species and/or diameter class.
#' The relative contribution of each group to the total plot basal area
#' is also calculated.
#'
#' @param IFN Character. Forest National Inventory version. Must be
#'   either "IFN3" or "IFN4".
#' @param df Data frame containing tree-level inventory data.
#' @param province Optional character vector with the names of the
#'   provinces to include.
#' @param com_aut Optional character vector with the names of the
#'   autonomous communities to include.
#' @param species Optional character vector with the species names
#'   to include.
#' @param per_CD Logical. If TRUE, results are calculated separately
#'   for each diameter class.
#' @param per_species Logical. If TRUE, results are calculated
#'   separately for each species.
#'
#' @return A data frame containing basal area (BA, m2/ha) and its
#'   relative contribution (prop_BA, %), at the selected aggregation
#'   level.
#'
#' @examples
#' # Calculate basal area per plot
#' get_BA(IFN = "IFN4", df = IFN4_example)
#'
#' # Calculate basal area by species
#' get_BA(IFN = "IFN4", df = IFN4_example,
#'        per_species = TRUE)
#'
#' # Calculate basal area by species and diameter class
#' get_BA(IFN = "IFN4", df = IFN4_example,
#'        per_species = TRUE, per_CD = TRUE)
#'
#' @export


get_BA <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                   per_CD = FALSE, per_species = FALSE) {

    .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
        summarise(BA = sum(BA, na.rm = T)) |>
        group_by(Codigo, Tipo_Parc) |>
        mutate(prop_BA = round(100*BA/sum(BA), 2))

}


#' Calculate tree density in forest inventory plots
#'
#' Calculates tree density (N, trees/ha) for each inventory plot,
#' optionally disaggregated by species and/or diameter class.
#' The relative contribution of each group to total tree density
#' is also calculated.
#'
#' @inheritParams get_BA
#'
#' @return A data frame containing tree density (N, trees/ha) and
#'   its relative contribution (prop_N, %), at the selected
#'   aggregation level.
#'
#' @examples
#' # Calculate total tree density per plot
#' get_N(IFN = "IFN3", df = IFN4_example)
#'
#' # Calculate tree density by species
#' get_N(IFN = "IFN3", df = IFN4_example,
#'       per_species = TRUE)
#'
#' # Calculate tree density by diameter class
#' get_N(IFN = "IFN4", df = IFN4_example,
#'       per_CD = TRUE)
#'
#' @export


get_N <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                  per_CD = FALSE, per_species = FALSE) {

    .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
        summarise(N = sum(N, na.rm = T)) |>
        group_by(Codigo, Tipo_Parc) |>
        mutate(prop_N = round(100*N/sum(N),2))

}

#' Calculate mean diameter in forest inventory plots
#'
#' Calculates the mean diameter (cm) for each inventory plot,
#' optionally disaggregated by species and/or diameter class.
#' Mean diameter is calculated as the arithmetic mean weighted
#' by tree density (N).
#'
#' @inheritParams get_BA
#'
#' @return A data frame containing the weighted mean diameter
#'   (Diam, cm) at the selected aggregation level.
#'
#' @examples
#' # Calculate mean diameter per plot
#' get_diam(IFN = "IFN3", df = IFN3_example)
#'
#' # Calculate mean diameter by species
#' get_diam(IFN = "IFN4", df = IFN4_example,
#'          per_species = TRUE)
#'
#' @export

get_diam <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                     per_CD = FALSE, per_species = FALSE) {

    .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
        summarise(Diam = weighted.mean(Dn,N, na.rm = T))

}

#' Calculate mean tree height in forest inventory plots
#'
#' Calculates the mean tree height (m) for each inventory plot,
#' optionally disaggregated by species and/or diameter class.
#' Mean height is calculated as the arithmetic mean weighted
#' by tree density (N).
#'
#' @inheritParams get_BA
#'
#' @return A data frame containing the weighted mean height
#'   (Height, m) at the selected aggregation level.
#'
#' @examples
#' # Calculate mean height per plot
#' get_hei(IFN = "IFN3", df = IFN3_example)
#'
#' # Calculate mean height by species
#' get_hei(IFN = "IFN4", df = IFN4_example,
#'         per_species = TRUE)
#'
#' @export

get_hei <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                    per_CD = FALSE, per_species = FALSE) {

    .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
        summarise(Height = weighted.mean(Ht,N, na.rm = T))

}

#' Calculate quadratic mean diameter (QMD)
#'
#' Calculates the quadratic mean diameter (QMD) of trees based on
#' diameter-class data. The calculation accounts for the number of
#' trees per hectare in each diameter class.
#'
#' QMD is calculated as:
#'
#' \deqn{QMD = \sqrt{\frac{\sum_{i=1}^{k} N_i D_i^2}
#' {\sum_{i=1}^{k} N_i}}}
#'
#' where \eqn{D_i} is the representative diameter of diameter class
#' \eqn{i}, and \eqn{N_i} is the number of trees per hectare in that class.
#'
#' @param IFN IFN inventory dataset.
#' @param df Data frame containing the inventory data.
#' @param province Optional. Province used to filter the inventory.
#' @param com_aut Optional. Autonomous community used to filter the inventory.
#' @param species Optional. Species used to filter the inventory.
#' @param per_CD Logical. If TRUE, calculates QMD by diameter class.
#' @param per_species Logical. If TRUE, calculates QMD by species.
#'
#' @return A data frame containing the quadratic mean diameter (QMD).
#'
#' @references
#' Husch, B., Beers, T. W., & Kershaw, J. A. (2003).
#' Forest Mensuration (4th ed.). Wiley.
#'
#' @export

get_QMD <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                    per_CD = FALSE, per_species = FALSE) {

  .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
    summarise(QMD = sqrt(weighted.mean(Dn^2, N, na.rm = TRUE)))

}


#' Calculate dominant diameter according to Assmann
#'
#' Calculates the dominant diameter (Ddom) following Assmann's definition,
#' based on the quadratic mean diameter of the 100 thickest trees per hectare.
#'
#' Trees are selected from the largest diameter classes until 100 trees
#' per hectare are reached. When the threshold falls within a diameter
#' class, only the corresponding proportion of trees from that class
#' is considered.
#'
#' If fewer than 100 trees per hectare are available, all trees are
#' included in the calculation.
#'
#' Ddom is calculated as:
#'
#' \deqn{Ddom = \sqrt{\frac{\sum_{i=1}^{k} N_{i,dom} D_i^2}
#' {\sum_{i=1}^{k} N_{i,dom}}}}
#'
#' where \eqn{D_i} is the representative diameter of diameter class
#' \eqn{i}, and \eqn{N_{i,dom}} is the number of trees per hectare
#' from that class included in the calculation.
#'
#' @param IFN IFN inventory dataset.
#' @param df Data frame containing the inventory data.
#' @param province Optional. Province used to filter the inventory.
#' @param com_aut Optional. Autonomous community used to filter the inventory.
#' @param species Optional. Species used to filter the inventory.
#' @param per_CD Logical. If TRUE, calculates Ddom separately for each
#'   diameter class. Note that this grouping may have limited biological
#'   and silvicultural interpretation, as dominant diameter is normally
#'   defined at stand level.
#' @param per_species Logical. If TRUE, calculates Ddom separately for
#'   each species.
#'
#' @return A data frame containing the dominant diameter (Ddom).
#'
#' @references
#' Assmann, E. (1970). The Principles of Forest Yield Study.
#' Pergamon Press.
#'
#' @export


get_Ddom <- function(IFN, df, province = NULL, com_aut = NULL,
                     species = NULL, per_CD = FALSE,
                     per_species = FALSE) {

  .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
    arrange(desc(Dn)) |>
    mutate(N_cum = cumsum(N)) |>
    filter(N_cum - N < 100) |>
    mutate(N_dom = pmin(N, 100 - (N_cum - N))) |>
    summarise(Ddom = sqrt(sum(Dn^2 * N_dom, na.rm = TRUE) /
                            sum(N_dom, na.rm = TRUE)))

}


#' Calculate vertical structural diversity (Pretzsch index)
#'
#' Calculates the vertical structural diversity of forest stands
#' according to the index proposed by Pretzsch. The index considers
#' the distribution of tree species across three vertical strata,
#' accounting for both species diversity and vertical stratification.
#'
#' The three vertical strata are defined relative to the maximum
#' tree height (\eqn{H_{max}}) within each inventory plot:
#'
#' \itemize{
#'   \item Lower stratum: 0-50% of \eqn{H_{max}}.
#'   \item Middle stratum: 50-80% of \eqn{H_{max}}.
#'   \item Upper stratum: 80-100% of \eqn{H_{max}}.
#' }
#'
#' The Pretzsch index is calculated as:
#'
#' \deqn{A = -\sum_{i=1}^{3}\sum_{j=1}^{S} p_{ij}\ln(p_{ij})}
#'
#' where \eqn{p_{ij}} is the proportion of trees belonging to species
#' \eqn{j} in vertical stratum \eqn{i}, relative to the total number
#' of trees in the plot, and \eqn{S} is the number of species present.
#'
#' The relative index is calculated as:
#'
#' \deqn{A_{rel} = \frac{A}{\ln(3S)}}
#'
#' where \eqn{3S} represents the maximum number of possible
#' species-stratum combinations.
#'
#' The function uses tree density (number of trees per hectare)
#' to calculate the relative abundance of each species-stratum
#' combination. Trees with missing or non-positive heights or
#' densities are excluded from the calculation.
#'
#' @param IFN Inventory dataset identifier ("IFN3" or "IFN4").
#' @param df Data frame containing the forest inventory data.
#' @param province Optional. Province used to filter the inventory.
#' @param com_aut Optional. Autonomous community used to filter the inventory.
#'
#' @return A data frame containing the following variables for each
#'   inventory plot:
#'   \describe{
#'     \item{A}{Pretzsch vertical structural diversity index.}
#'     \item{A_rel}{Relative Pretzsch index, ranging from 0 to 1.}
#'     \item{N_lower}{Tree density in the lower stratum (trees/ha).}
#'     \item{N_middle}{Tree density in the middle stratum (trees/ha).}
#'     \item{N_upper}{Tree density in the upper stratum (trees/ha).}
#'   }
#'
#' @references
#' Pretzsch, H. (1998). Structural diversity as a result of
#' silvicultural operations. In: Bachmann, P., Köhl, M. & Päivinen,
#' R. (eds.), Assessment of Biodiversity for Improved Forest Planning.
#' European Forest Institute Proceedings, 18, 243-255.
#'
#' @examples
#' # Calculate vertical structural diversity using IFN4 data
#' get_vertical_structure(IFN = "IFN4", df = IFN4_example)
#'
#' # Calculate vertical structural diversity for a particular province
#' get_vertical_structure(IFN = "IFN4", df = IFN4_example,
#'                        province = "Leon")
#'
#' @export

get_vertical_structure <- function(IFN, df, province = NULL,
                                   com_aut = NULL) {

  plot_vars <- c("Provincia", "Prov_Name", "CCAA",
                 "Estadillo", "Codigo", "Tipo_Parc")

  df_vertical <- .get_groups(IFN, df, province, com_aut) |>
    mutate(Hmax = max(Ht, na.rm = TRUE)) |>
    filter(!is.na(Ht), Ht > 0,
           !is.na(N), N > 0,
           is.finite(Hmax), Hmax > 0) |>
    mutate(
      Stratum = case_when(
        Ht <= 0.5 * Hmax ~ "Lower",
        Ht <= 0.8 * Hmax ~ "Middle",
        TRUE ~ "Upper"
      )
    ) |>
    group_by(across(all_of(plot_vars)), Especie, Sps_name, Stratum) |>
    summarise(N = sum(N, na.rm = TRUE), .groups = "drop")

  df_vertical |>
    group_by(across(all_of(plot_vars))) |>
    mutate(p = N / sum(N)) |>
    summarise(
      A = -sum(p * log(p)),
      S = n_distinct(Sps_name),
      N_lower = sum(N[Stratum == "Lower"]),
      N_middle = sum(N[Stratum == "Middle"]),
      N_upper = sum(N[Stratum == "Upper"]),
      .groups = "drop"
    ) |>
    mutate(A_rel = A / log(3 * S)) |>
    select(-S)

}

#' Calculate the Gini coefficient
#'
#' Calculates the Gini coefficient to quantify inequality in tree
#' diameter or individual basal area within forest stands.
#'
#' The Gini coefficient is calculated using tree density (number
#' of trees per hectare) as weights. Two variables can be considered:
#'
#' \itemize{
#'   \item Diameter (Dn): measures inequality in tree diameter.
#'   \item Individual basal area (BA): measures inequality in the
#'   individual basal area of trees, calculated from their diameter.
#' }
#'
#' The weighted Gini coefficient is calculated as:
#'
#' \deqn{
#' G = \frac{\sum_i\sum_j N_i N_j |x_i-x_j|}
#' {2\mu_x N_T^2}
#' }
#'
#' where \eqn{x_i} is the variable of interest, \eqn{N_i} is the
#' number of trees per hectare in diameter class \eqn{i},
#' \eqn{\mu_x} is the weighted arithmetic mean of the variable,
#' and \eqn{N_T} is the total number of trees per hectare.
#'
#' When variable = "BA", individual basal area is calculated as:
#'
#' \deqn{BA_i = \frac{\pi (Dn_i/100)^2}{4}}
#'
#' The coefficient ranges from 0 (complete equality) to values
#' approaching 1 (high inequality).
#'
#' Calculations are based on representative diameters of diameter
#' classes and therefore provide an estimate of the Gini coefficient
#' of the individual tree distribution.
#'
#' @param IFN Inventory dataset identifier ("IFN3" or "IFN4").
#' @param df Data frame containing the forest inventory data.
#' @param province Optional. Province used to filter the inventory.
#' @param com_aut Optional. Autonomous community used to filter the inventory.
#' @param species Optional. Species used to filter the inventory.
#' @param per_species Logical. If TRUE, calculates the Gini coefficient
#'   separately for each species.
#' @param variable Character. Variable used to calculate the Gini
#'   coefficient. Either "Dn" (tree diameter) or "BA"
#'   (individual basal area). Defaults to "Dn".
#'
#' @return A data frame containing the Gini coefficient (Gini)
#'   for each inventory plot, or separately for each species
#'   when per_species = TRUE.
#'
#' @references
#' Gini, C. (1912). Variabilità e mutabilità. Contributo allo studio
#' delle distribuzioni e delle relazioni statistiche.
#'
#' @examples
#' # Calculate Gini coefficient based on tree diameter
#' get_Gini(IFN = "IFN4", df = IFN4_example)
#'
#' # Calculate Gini coefficient based on individual basal area
#' get_Gini(IFN = "IFN4", df = IFN4_example, variable = "BA")
#'
#' # Calculate Gini coefficient separately by species
#' get_Gini(IFN = "IFN4", df = IFN4_example, per_species = TRUE)
#'
#' @export

get_Gini <- function(IFN, df, province = NULL, com_aut = NULL,
                     species = NULL, per_species = FALSE,
                     variable = "Dn") {

  variable <- match.arg(variable, c("Dn", "BA"))

  .get_groups(IFN, df, province, com_aut, species,
              per_species = per_species) |>
    summarise(
      Gini = {
        x <- if (variable == "Dn") {
          Dn
        } else {
          pi * (Dn / 100)^2 / 4
        }

        w <- N

        valid <- is.finite(x) & is.finite(w) &
          x >= 0 & w > 0

        x <- x[valid]
        w <- w[valid]

        if (length(x) == 0 || sum(w * x) <= 0) {
          NA_real_
        } else {
          ord <- order(x)
          x <- x[ord]
          w <- w[ord]

          W <- sum(w)

          sum(w * x * (2 * cumsum(w) - w - W)) /
            (W * sum(w * x))
        }
      }
    )

}

#' Calculate Stand Density Index (SDI)
#'
#' Calculates the Stand Density Index (SDI) according to Reineke's
#' stand density rule. The SDI expresses stand density relative to
#' a reference quadratic mean diameter.
#'
#' The index is calculated as:
#'
#' \deqn{SDI = N \left(\frac{QMD}{D_{ref}}\right)^b}
#'
#' where \eqn{N} is the number of trees per hectare, \eqn{QMD} is
#' the quadratic mean diameter (cm), \eqn{D_{ref}} is the reference
#' diameter (cm), and \eqn{b} is the exponent of the Reineke
#' stand density relationship.
#'
#' By default, the function uses the original Reineke exponent
#' (1.605) and a reference diameter of 25 cm.
#'
#' @param IFN IFN inventory dataset.
#' @param df Data frame containing the inventory data.
#' @param province Optional. Province used to filter the inventory.
#' @param com_aut Optional. Autonomous community used to filter the inventory.
#' @param species Optional. Species used to filter the inventory.
#' @param per_CD Logical. If TRUE, calculates SDI separately for each
#'   diameter class. Note that this grouping does not represent the
#'   conventional stand-level SDI.
#' @param per_species Logical. If TRUE, calculates SDI separately
#'   for each species.
#' @param exponent Numeric. Exponent of the Reineke stand density
#'   relationship. Defaults to 1.605.
#' @param D_ref Numeric. Reference quadratic mean diameter (cm).
#'   Defaults to 25.
#'
#' @return A data frame containing the Stand Density Index (SDI).
#'
#' @references
#' Reineke, L. H. (1933). Perfecting a stand-density index for
#' even-aged forests. Journal of Agricultural Research, 46, 627–638.
#'
#' @examples
#' # Calculate stand-level SDI
#' get_SDI(IFN = "IFN3", df = IFN3_example)
#'
#' # Calculate SDI by species
#' get_SDI(IFN = "IFN3", df = IFN3_example, per_species = TRUE)
#'
#' # Calculate SDI using a species-specific exponent
#' get_SDI(IFN = "IFN3", df = IFN3_example, species = "Pinus sylvestris",
#'         exponent = 1.605, D_ref = 25)
#'
#' @export


get_SDI <- function(IFN, df, province = NULL, com_aut = NULL,
                    species = NULL, per_CD = FALSE,
                    per_species = FALSE,
                    exponent = 1.605, D_ref = 25) {

  .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
    summarise(
      SDI = sum(N, na.rm = TRUE) *
        (sqrt(weighted.mean(Dn^2, N, na.rm = TRUE)) / D_ref)^exponent
    )

}


#' Calculate tree species diversity indices
#'
#' Calculates taxonomic diversity indices based on tree species abundance
#' (number of trees per hectare) within each inventory plot.
#'
#' The function calculates four complementary diversity indices:
#'
#' \itemize{
#'   \item Species richness (Richness): number of species present.
#'   \item Shannon diversity index (Shannon): accounts for both species
#'   richness and relative abundance.
#'   \item Simpson diversity index (Simpson): calculated as 1-D,
#'   representing the probability that two randomly selected trees
#'   belong to different species.
#'   \item Pielou's evenness index (Pielou): measures the evenness
#'   of species abundances.
#' }
#'
#' The indices are calculated using the relative abundance of each
#' species, expressed as the proportion of trees per hectare:
#'
#' \deqn{p_i = \frac{N_i}{N}}
#'
#' where \eqn{N_i} is the number of trees per hectare of species
#' \eqn{i}, and \eqn{N} is the total number of trees per hectare.
#'
#' The indices are calculated as follows:
#'
#' \deqn{Richness = S}
#'
#' \deqn{Shannon = -\sum_{i=1}^{S} p_i \ln(p_i)}
#'
#' \deqn{Simpson = 1 - \sum_{i=1}^{S} p_i^2}
#'
#' \deqn{Pielou = \frac{Shannon}{\ln(S)}}
#'
#' When only one species is present, Shannon and Simpson indices
#' equal zero, whereas Pielou's evenness is undefined and returns NA.
#'
#' @param IFN Inventory dataset identifier ("IFN3" or "IFN4").
#' @param df Data frame containing the forest inventory data.
#' @param province Optional. Province used to filter the inventory.
#' @param com_aut Optional. Autonomous community used to filter the inventory.
#'
#' @return A data frame containing the following diversity indices
#'   for each inventory plot:
#'   \describe{
#'     \item{Richness}{Species richness (number of species).}
#'     \item{Shannon}{Shannon diversity index.}
#'     \item{Simpson}{Simpson diversity index (1-D).}
#'     \item{Pielou}{Pielou's evenness index.}
#'   }
#'
#' @references
#' Shannon, C. E. (1948). A mathematical theory of communication.
#' Bell System Technical Journal, 27, 379-423.
#'
#' Simpson, E. H. (1949). Measurement of diversity.
#' Nature, 163, 688.
#'
#' Pielou, E. C. (1966). The measurement of diversity in different
#' types of biological collections. Journal of Theoretical Biology,
#' 13, 131-144.
#'
#' @examples
#' # Calculate diversity indices using IFN4 data
#' get_diversity(IFN = "IFN4", df = IFN4_example)
#'
#' # Calculate diversity indices for a particular province
#' get_diversity(IFN = "IFN4", df = IFN4_example,
#'               province = "Leon")
#'
#' @export


get_diversity <- function(IFN, df, province = NULL, com_aut = NULL) {

  .get_groups(IFN, df, province, com_aut, species=NULL,
              per_species = TRUE) |>
    summarise(N = sum(N, na.rm = TRUE), .groups = "drop") |>
    filter(N > 0) |>
    group_by(Codigo, Tipo_Parc) |>
    summarise(
      Richness = n_distinct(Sps_name),
      Shannon = {
        p <- N / sum(N)
        -sum(p * log(p))
      },
      Simpson = {
        p <- N / sum(N)
        1 - sum(p^2)
      },
      Pielou = if (n() > 1) Shannon / log(n()) else 0,
      .groups = "drop"
    )

}
