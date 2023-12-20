#' Imports specified TALOS data
#'
#' @param token api token
#'
#' @return tibble of REDCap exported data
#' @examples
#' data <- import_talos()
#' skimr::skim(data)
import_talos <- function(key = "TALOS_REDCAP_API") {
  REDCapR::redcap_read(
    redcap_uri = "https://redcap.au.dk/api/", token = keyring::key_get(key),
    fields = c("record_id", "inkl_rnumb", "cpr", "talos_inkl03x", "talos_basis02a", "talos_basis02b", "basis_sys_site")
  )$data
}

#' Filter TALOS by site
#'
#' @param data
#' @param site default is "10015", which is Aarhus
#'
#' @return
#' @export
#'
#' @examples
#' aarhus <- import_talos() |> filter_talos_site(site = "10015")
filter_talos_site <- function(data, site = "10015") {
  if (is.null(site)) {
    data
  } else {
    data |> dplyr::filter(basis_sys_site %in% site)
  }
}

next_id <- function(key = "SVD_REDCAP_API"){
suppressMessages(
    REDCapR::redcap_next_free_record_name(redcap_uri = "https://redcap.au.dk/api/",
                                          token = keyring::key_get(key))
    ) |>
        as.numeric()
    }

#' Modifies exported talos data to new database format
#'
#' @param data tibbled REDCap data
#' @param trial source trial name. TALOS or RESIST
#' @param key destination project API key name
#'
#' @return tibble with modified data
#' data |> modify_data()
modify_data <- function(data, trial = "TALOS",key="SVD_REDCAP_API") {
    ids <- seq_len(nrow(data))+(next_id(key=key)-1)

    data |> dplyr::mutate(ids=ids) |>
      dplyr::transmute(
    record_id = glue::glue("svd_{ids}"),
    trial_id = inkl_rnumb,
    cpr = cpr,
    name = talos_inkl03x,
    index_date = lubridate::ymd(talos_basis02a),
    index_time = hms::as_hms(talos_basis02b),
    trial_name = dplyr::case_match(
      trial,
      "TALOS" ~ 1,
      "RESIST" ~ 2
    )
  )
}


#' Write data to REDCap db
#'
#' @param key project key set in `keyring`
#' @param data data to write
#'
#' @return
write2new_db <- function(data, key = "SVD_REDCAP_API") {
  REDCapR::redcap_write(
    redcap_uri = "https://redcap.au.dk/api/",
    token = keyring::key_get(key),
    ds_to_write = data
  )
}


# import_talos() |> filter_talos_site() |> modify_data() |> write2new_db()
