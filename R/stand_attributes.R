
# load("./data/IFN3_Spain.Rdata")
# load("./data/IFN3_Exs_Spain.Rdata")
#
# load("./data/IFN4_Spain.Rdata")
# load("./data/IFN4_Exs_Spain.Rdata")

# df = PCMayores_IFN4
# IFN = "IFN4"

library(dplyr)
.get_groups <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                   per_CD = FALSE, per_species = FALSE) {
    require(dplyr)
    require(tidyr)
    # require(ggplot2)

    if (IFN == "IFN4") {
      df <- df |>
        drop_na(Especie) |>
        filter(OrdenIf4 != "888",
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
#' get_BA(IFN = "IFN4", df = inventory)
#'
#' # Calculate basal area by species
#' get_BA(IFN = "IFN4", df = inventory,
#'        per_species = TRUE)
#'
#' # Calculate basal area by species and diameter class
#' get_BA(IFN = "IFN4", df = inventory,
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
#' get_N(IFN = "IFN3", df = inventory)
#'
#' # Calculate tree density by species
#' get_N(IFN = "IFN3", df = inventory,
#'       per_species = TRUE)
#'
#' # Calculate tree density by diameter class
#' get_N(IFN = "IFN4", df = inventory,
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
#' get_diam(IFN = "IFN3", df = inventory)
#'
#' # Calculate mean diameter by species
#' get_diam(IFN = "IFN4", df = inventory,
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
#' get_hei(IFN = "IFN3", df = inventory)
#'
#' # Calculate mean height by species
#' get_hei(IFN = "IFN4", df = inventory,
#'         per_species = TRUE)
#'
#' @export

get_hei <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                    per_CD = FALSE, per_species = FALSE) {

    .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
        summarise(Height = weighted.mean(Ht,N, na.rm = T))

}
