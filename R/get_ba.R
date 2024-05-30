
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
    require(ggplot2)

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
            labels = seq(10,70,5)))),
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
                     Estadillo, Codigo,Clase, Subclase, Tipo_Parc,
                     Especie, Sps_name, CD)
    } else if (isTRUE(per_species)) {
        df |>
            group_by(Provincia, Prov_Name, CCAA,
                     Estadillo, Codigo,Clase, Subclase, Tipo_Parc,
                     Especie, Sps_name)

    } else if (isTRUE(per_CD)) {
        df |>
            group_by(Provincia, Prov_Name, CCAA,
                     Estadillo, Codigo,Clase, Subclase, Tipo_Parc,
                     CD)
    } else {
        df |>
            group_by(Provincia, Prov_Name, CCAA,
                     Estadillo, Codigo,Clase, Subclase, Tipo_Parc)
    }

}



#' Calculates dasometric summaries for IFN plots
#'
#' @param df data frame
#' @param province vector of characters containing the name of the provinces to be included in the analyses
#' @param com_aut vector of characters containing the name of the autonomous regions (Comunidad Autónoma) to be included in the analyses
#' @param species vector of characters containing the name of the tree species to be included in the analyses
#' @param per_CD optional. When `per_CD = TRUE`, the desired variable is summarized per plot and diameter class
#' @param per_species optional. When `per_species = TRUE`, the desired variable is summarized per plot and species
#'
#' @return A data frame containing the level of aggregation specified (per plot, per plot & species, per plot and DC, per plot, species and DC), and the amount of the outùt variable specified
#' @export
#'
#'



get_BA <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                   per_CD = FALSE, per_species = FALSE) {

    .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
        summarise(BA = sum(BA, na.rm = T)) |>
        group_by(Codigo, Tipo_Parc) |>
        mutate(prop_BA = round(100*BA/sum(BA), 2))

}

get_N <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                  per_CD = FALSE, per_species = FALSE) {

    .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
        summarise(N = sum(N, na.rm = T)) |>
        group_by(Codigo, Tipo_Parc) |>
        mutate(prop_N = round(100*N/sum(N),2))

}

get_diam <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                     per_CD = FALSE, per_species = FALSE) {

    .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
        summarise(Diam = weighted.mean(Dn,N, na.rm = T))

}

get_hei <- function(IFN, df, province = NULL, com_aut = NULL, species = NULL,
                    per_CD = FALSE, per_species = FALSE) {

    .get_groups(IFN, df, province, com_aut, species, per_CD, per_species) |>
        summarise(Height = weighted.mean(Ht,N, na.rm = T))

}

#' @examples
#'
#' #'
#'
#' foo <- get_BA(IFN = "IFN3", df = PCMayores_IFN3, per_species = F )
#' foo2 <- get_BA(IFN = "IFN3", Mayores_exs_IFN3, per_species = F)
#'
#' foo3 <- foo |> left_join(foo2,
#'                          by =c("Provincia", "Prov_Name",
#'                                "CCAA", "Estadillo", "Codigo",
#'                                "Clase", "Subclase", "Tipo_Parc")) |>
#'   mutate(dif = BA.x - BA.y)
#'
#' plot(foo3$BA.x, foo3$BA.y)
#' abline(0,1)

#
# get_diam(PCMayores_IFN4, com_aut = "Euskadi", per_species = TRUE)
#
#
# get_BA(PCMayores_IFN4, species = "Pinus sylvestris")
#
# get_N(PCMayores_IFN4, com_aut = "Cataluñña", per_species = FALSE)
