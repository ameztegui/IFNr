
#get_ba/get_N/get_d/get_H

#AÑADIR PORCENTAJE POR ESPECIES SI HACE FALTA


#df <- PCMayores_IFN4
#province <- NULL
#per_species <- F
#per_CD = F
#com_aut = "Catalunya"
#species <- c("Pinus sylvestris", "Pinus uncinata")

.get_groups <- function(df, province = NULL, com_aut = NULL, species = NULL,
                   per_CD = FALSE, per_species = FALSE) {
    require(dplyr)
    require(tidyr)

    df <- df |>
        drop_na(Especie) |>
        filter(OrdenIf4 != "888",
               OrdenIf4 != "999") |>
        mutate(BA = pi*(Dn/100)^2*N/4)

    if(!is.null(com_aut)) {
        if(all(com_aut %in% unique(df$CCAA))) df <- df |> filter(CCAA %in% com_aut)
        else  stop("The CCAA ", com_aut[!com_aut %in% unique(df$CCAA)], " does not exist in the database. Type `com_aut` to see the available options")
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


get_BA <- function(df, province = NULL, com_aut = NULL, species = NULL,
                   per_CD = FALSE, per_species = FALSE) {

    .get_groups(df, province, com_aut, species, per_CD, per_species) |>
        summarise(BA = sum(BA, na.rm = T)) |>
        group_by(Codigo) |>
        mutate(prop_BA = round(100*BA/sum(BA), 2))

}

get_N <- function(df, province = NULL, com_aut = NULL, species = NULL,
                  per_CD = FALSE, per_species = FALSE) {

    .get_groups(df, province, com_aut, species, per_CD, per_species) |>
        summarise(N = sum(N, na.rm = T)) |>
        group_by(Codigo) |>
        mutate(prop_N = round(100*N/sum(N),2))

}

get_diam <- function(df, province = NULL, com_aut = NULL, species = NULL,
                     per_CD = FALSE, per_species = FALSE) {

    .get_groups(df, province, com_aut, species, per_CD, per_species) |>
        summarise(Diam = weighted.mean(Dn,N, na.rm = T))

}

get_hei <- function(df, province = NULL, com_aut = NULL, species = NULL,
                    per_CD = FALSE, per_species = FALSE) {

    .get_groups(df, province, com_aut, species, per_CD, per_species) |>
        summarise(Height = weighted.mean(Ht,N, na.rm = T))

}


#get_N(PCMayores_IFN4, com_aut = "Catalunya", per_species = T)
#get_diam(PCMayores_IFN4, com_aut = "Euskadi", per_species = T)

