#' Calculate tree biomass or volume over bark based on data from tree inventories and allometries from AllometrApp
#'
#' @param df The dataframe that contains the data
#' @param variable The dependent variable that we want to calculate. "BAT" stands for aerial biomass, and "VOB" for volume over bark
#' @param equation The allometric equation we want to use. Options include "INIA", "IEFC_1", "IEFC_2", "RUIZ", and "VOB"
#' @param dbh The name of the variable that identifies diameter in the dataset
#' @param height The name of the variable that identifies tree height in the dataset. If `height` is missing, the funcion will calculate it from allometries and warn about it.
#' @param sps A variable containing the names of the species
#' @param frac Whether we want to obtain the biomass values by tree fraction (leaves, small and large branches, stem, bark). By default, `frac = FALSE`

#' @return
#' * If `frac = FALSE`, it will return a numeric vector containing the BAT or VOB values for each observation in `df`.
#' * If `frac = TRUE`, it will return a data frame identical to `df` but with some extra columns containing the biomass weights per tree fraction
#'
#' @examples
#' trees <- data.frame(species = c("Abies alba", "Pinus sylvestris", "Pinus sylvestris", "Pinus pinea", "Quercus ilex"),
#'                     dbh = 12, 15, 26, 52, 10,
#'                     height = 10, 12, 20, 24, 9)
#' calculate_allom(trees, "BAT", equation = "INIA", dbh = "dbh", height = "height", sps = "species")  # including height
#' calculate_allom(trees, "BAT", equation = "INIA", dbh = "dbh", sps = "species")  # without height
#' calculate_allom(trees, "BAT", equation = "INIA", dbh = "dbh", sps = "species", frac = T)  # biomass per fractions
#'
#'
#'
calculate_allom <- function (df, variable, equation = NULL, dbh = "dbh", height = NULL, sps, frac = FALSE) {

    species <- enquo(sps)
    by = set_names("species", quo_name(species))

    # Check that variables exist
    if(missing(variable)) stop("You must define which variable to calculate.
                               Options are aerial biomass (BAT), tree height (Ht), or volume over bark (VOB)")
    if(variable == "BAT" & is.null(equation))
        stop ("You must define one BAT equation. Options are `IEFC_1`, `IEFC_2`, `VOB`, INIA`, or `RUIZ`")


    try(df$dbh <- pull(df[,dbh]),
        stop("No diameter data found in your data frame. Please use parameter `dbh` to specify the variable containing
                                       tree diameters"))

    # If height is not defined, calculate
    if(is.null(height)) {

        warning("No height data was found in your data frame. If the allometric equation you chose requires height values,
                              we will use the allometries by IEFC")
        df$Height <- df %>%
            left_join( filter(data_allom, dependent_var == "Ht"), by = by) %>%
            mutate(Ht = param_a * dbh ^ param_b) %>% pull(Ht)

    } else {
        df$Height <- pull(df[,height])

    }

    if(variable == "VOB") {
        eq_id = "VOB_IEFC_1"

        foo <- data_allom %>%
            filter(dependent_var == variable,
                   eq_id == !!eq_id) %>%
            dplyr::select(species, param_a:param_d, BEFD)

        output <- left_join(df,foo, by = by) %>%
            mutate(VOB = param_a*dbh^param_b)

        output %>%
            pull(VOB)

    } else {


        # Check the value fo equation

        if(equation == "VOB") {
            eq_id = "VOB_IEFC_1"

            foo <- data_allom %>%
                filter(dependent_var == "VOB",
                       eq_id == !!eq_id) %>%
                dplyr::select(species, param_a:param_d, BEFD)

            output <- left_join(df,foo, by = by) %>%
                mutate(VOB = param_a * dbh ^ param_b,
                       BAT = VOB * BEFD)
            output%>%
                pull(BAT)
        } else {

        if(equation == "RUIZ") {
            eq_id = c("BF_RUIZ_1", "BR7_RUIZ_1", "BR2_7_RUIZ_1", "BR2_RUIZ_1")
            df$Species <- pull(df[,sps])

            foo <- data_allom %>%
                filter(eq_id %in% !!eq_id) %>%
                pivot_wider(c(species, eq_id,param_a: param_d),
                            names_from = eq_id,
                            values_from =  param_a:param_d,
                            names_glue = "{eq_id}_{.value}")

            output <- left_join(df,foo, by = by) %>%
                mutate(
                    BF_RUIZ = case_when(
                        Species == "Fagus sylvatica" ~
                            BF_RUIZ_1_param_a * dbh^2 + BF_RUIZ_1_param_b * dbh^2 * Height,
                        TRUE ~ (BF_RUIZ_1_param_a * dbh ^ BF_RUIZ_1_param_b) * (Height ^ BF_RUIZ_1_param_c) + BF_RUIZ_1_param_d * Height),
                    BR7_RUIZ = case_when(
                        Species %in% c("Alnus glutinosa", "Quercus canariensis", "Quercus faginea", "Quercus suber",
                                       "Abies alba", "Pinus pinaster","Pinus uncinata") ~
                            BR7_RUIZ_1_param_a*dbh ^ BR7_RUIZ_1_param_b * Height ^ BR7_RUIZ_1_param_c,
                        Species %nin% c("Alnus glutinosa", "Quercus canariensis", "Quercus faginea", "Quercus suber",
                                        "Abies alba", "Pinus pinaster","Pinus uncinata") & dbh <= BR7_RUIZ_1_param_b ~
                            0,
                        Species %nin% c("Alnus glutinosa", "Quercus canariensis", "Quercus faginea", "Quercus suber",
                                        "Abies alba", "Pinus pinaster","Pinus uncinata") & dbh > BR7_RUIZ_1_param_b ~
                            BR7_RUIZ_1_param_a * (dbh - BR7_RUIZ_1_param_b)^BR7_RUIZ_1_param_c +
                            BR7_RUIZ_1_param_d*(dbh - BR7_RUIZ_1_param_b)^2*Height),
                    BR2_7_RUIZ = case_when(
                        Species == "Pinus halepensis" ~
                            BR2_7_RUIZ_1_param_a + BR2_7_RUIZ_1_param_b * dbh^2*Height + BR2_7_RUIZ_1_param_c*dbh*Height,
                        Species %in% c("Quercus faginea", "Quercus pyrenaica") ~
                            BR2_7_RUIZ_1_param_a * dbh^BR2_7_RUIZ_1_param_b + BR2_7_RUIZ_1_param_c * Height +
                            BR2_7_RUIZ_1_param_d * dbh^2 * Height,
                        TRUE ~ BR2_7_RUIZ_1_param_a * dbh ^ BR2_7_RUIZ_1_param_b * Height ^ BR2_7_RUIZ_1_param_c),
                    BR2_RUIZ = case_when(
                        Species %in% c("Pinus pinea", "Pinus uncinata") ~
                            BR2_RUIZ_1_param_a + BR2_RUIZ_1_param_b * dbh ^ BR2_RUIZ_1_param_c - BR2_RUIZ_1_param_d * Height,
                        Species == "Pinus halepensis" ~
                            BR2_RUIZ_1_param_a + BR2_RUIZ_1_param_b * dbh^2 * Height + BR2_RUIZ_1_param_c * dbh * Height,
                        Species == "Eucalyptus globulus" ~
                            BR2_RUIZ_1_param_a *dbh^BR2_RUIZ_1_param_b + BR2_RUIZ_1_param_c * dbh^BR2_RUIZ_1_param_d *Height,
                        Species %in% c("Pinus nigra", "Pinus pinaster", "Alnus glutinosa", "Castanea sativa", "Juniperus thurifera",
                                       "Pinus sylvestris", "Fraxinus angustifolia") ~
                            BR2_RUIZ_1_param_a * dbh ^ BR2_RUIZ_1_param_b * BR2_RUIZ_1_param_c * Height ^BR2_RUIZ_1_param_d,
                        TRUE ~
                            BR2_RUIZ_1_param_a * dbh ^ BR2_RUIZ_1_param_b + BR2_RUIZ_1_param_c * dbh ^BR2_RUIZ_1_param_d * Height)) %>%
                rowwise() %>%
                mutate(BAT = sum(BF_RUIZ, BR7_RUIZ, BR2_7_RUIZ, BR2_RUIZ, na.rm = T)) %>%
                dplyr::select(-contains("param"))

            if(isTRUE(frac)) {
                output
            } else {
                output %>%
                    pull(BAT)
            }



        } else{
        if (equation == "INIA") {
            eq_id = c("BF_INIA_1", "BH_INIA_1", "BR2_7_INIA_1", "BR2_INIA_1", "BR7_INIA_1", "BAT_INIA_1")
            df$Species <- pull(df[,sps] )

            foo <- data_allom %>%
                filter(eq_id %in% !!eq_id) %>%
                pivot_wider(c(species, eq_id,param_a: param_d),
                            names_from = eq_id,
                            values_from =  param_a:param_d,
                            names_glue = "{eq_id}_{.value}")

            output <- left_join(df,foo, by = by) %>%
                mutate(BAT = BAT_INIA_1_param_a * dbh ^ BAT_INIA_1_param_b,
                       BF = BF_INIA_1_param_a * dbh ^ BF_INIA_1_param_b,
                       BH = BH_INIA_1_param_a * dbh ^ BH_INIA_1_param_b,
                       BR2_7 = BR2_7_INIA_1_param_a * dbh ^ BR2_7_INIA_1_param_b,
                       BR2 = BR2_INIA_1_param_a * dbh ^ BR2_INIA_1_param_b,
                       BR7 = BR7_INIA_1_param_a * dbh ^ BR7_INIA_1_param_b) %>%
                rowwise() %>%
                mutate(BAT_fr = sum(BF, BH, BR2_7, BR2, BR7, na.rm = T)) %>%
                dplyr::select(-contains("param"))
            if(isTRUE(frac)) {
                output

            } else {
                output %>%
                    pull(BAT)
            }

            } else {
                # if(equation == "INIA") {
                # eq_id = paste0(variable, "_", equation, "_1")
                #  } else {
                eq_id = paste0(variable, "_", equation)

                foo <- data_allom %>%
                    filter(dependent_var == variable,
                           eq_id == !!eq_id) %>%
                    dplyr::select(species, param_a:param_d)

                left_join(df,foo, by = by) %>%
                    mutate(BAT = case_when(equation == "IEFC_1" ~ param_a * dbh ^ param_b,
                                           equation == "IEFC_2" ~ param_a * dbh ^ param_b * Height ^ param_c,
                                           equation == "INIA" ~ param_a * dbh ^ param_b,
                                           TRUE ~ NA_real_)) %>%
                    pull(BAT)
                }
        }
            }}
}
