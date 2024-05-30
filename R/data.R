#' Inventory data
#'
#' A subset of data from the 4th National Forest Inventory in Catalonia
#'
#'
#' @format  data frame with data from 100 random inventory plots randomly
#'  selected from the National Forest Inventory plots in Catalonia
#' A data frame with 3,434 rows and 8 columns:
#' \describe{
#'   \item{Codigo}{Code of the plot}
#'   \item{nArbol}{tree order within the plot}
#'   \item{Sps_name}{Tree pecies}
#'   \item{Dn}{Tree diameter, in cm}
#'   \item{Ht}{Tree height, in m}
#'   \item{N}{Tree density (i.e. trees per hectare of that species and size)}
#'   \item{Prov_Name}{Province in which the plot is located}
#'   \item{CCAA}{Autonomous region in which the plot is located}
#'   ...
#' }
#' @source <https://www.miteco.gob.es/es/biodiversidad/temas/inventarios-nacionales/inventario-forestal-nacional.html>
#'
#' @examples
#' data(inventory)    # Lazy loading. Data becomes visible as soon as needed
"inventory"
