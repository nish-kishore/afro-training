#' Function to create AFP map
#' @description This function does something great
#' @import ggplot2 dplyr
#' @param afp.data tibble: contains all AFP information
#' @param dist.shape sf: all districts of interest
#' @param ctry str: specify country of interest
#' @param year int: specify year of interest
#' @param out_file str: location of image to be saved
#' @return ggplot map
create_afp_map <- function(afp.data, dist.shape, ctry, year){

  # afp.data <- data$afp
  # dist.shape <- data$global.dist
  # ctry <- "NIGERIA"
  # year <- 2023

  #read AFP data
  # provided by inputs of user
  #subset to ctry of interest and year
  dat <- afp.data |>
    dplyr::filter(place.admin.0 == ctry,
                  yronset == year)

  #process shape files
  dist <- dist.shape |>
    dplyr::filter(
      ADM0_NAME == ctry,
      year >= yr.st,
      year <= yr.end
      )

  #process AFP data
  dat <- dat |>
    dplyr::group_by(adm2guid) |>
    dplyr::summarize(count = dplyr::n())

  dat.sp <- dplyr::left_join(
    dist, dat, by = c("GUID" = "adm2guid")
  )

  #plot afp cases
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = dat.sp, ggplot2::aes(fill = count))

  #write out figure
  ggplot2::ggsave(filename = out_file, plot = plot)

}
