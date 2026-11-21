#' Inventory data
#'
#' A subset of tree-level data used to demonstrate the
#' application of allometric equations with [calculate_allom()].
#'
#' @format A data frame with 2,157 rows and 8 columns:
#' \describe{
#'   \item{Codigo}{Code of the inventory plot}
#'   \item{nArbol}{Tree order within the plot}
#'   \item{Sps_name}{Tree species name}
#'   \item{Dn}{Tree diameter, in cm}
#'   \item{Ht}{Tree height, in m}
#'   \item{N}{Tree density (trees per hectare)}
#'   \item{Prov_Name}{Province in which the plot is located}
#'   \item{CCAA}{Autonomous region in which the plot is located}
#' }
#'
#' @source Spanish National Forest Inventory (IFN4), Catalonia.
#' \url{https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html}
#'
#' @keywords datasets
"inventory"


#' IFN3 example data
#'
#' A subset of tree-level data from the 3rd Spanish National Forest
#' Inventory (IFN3), used to demonstrate the functions for calculating
#' forest stand attributes.
#'
#' The dataset contains all tree records from a selection of inventory
#' plots in Catalonia.
#'
#' @format A data frame containing tree-level inventory data.
#' It includes the variables required by the functions
#' [get_BA()], [get_N()], [get_diam()] and [get_hei()].
#'
#' @source Spanish National Forest Inventory (IFN3), Ministry for
#' Ecological Transition and Demographic Challenge (MITECO).
#' \url{https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html}
#'
#' @keywords datasets
"IFN3_example"


#' IFN4 example data
#'
#' A subset of tree-level data from the 4th Spanish National Forest
#' Inventory (IFN4), used to demonstrate the functions for calculating
#' forest stand attributes.
#'
#' The dataset contains all tree records from a selection of inventory
#' plots in Catalonia.
#'
#' @format A data frame containing tree-level inventory data.
#' It includes the variables required by the functions
#' [get_BA()], [get_N()], [get_diam()] and [get_hei()].
#'
#' @source Spanish National Forest Inventory (IFN4), Ministry for
#' Ecological Transition and Demographic Challenge (MITECO).
#' \url{https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html}
#'
#' @keywords datasets
"IFN4_example"
