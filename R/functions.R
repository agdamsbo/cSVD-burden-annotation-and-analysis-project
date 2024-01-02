#' Imports specified TALOS data
#'
#' @param token api token
#' @param vars variable names to retrieve (fields in REDCap lingo)
#'
#' @return tibble of REDCap exported data
#' @examples
#' data <- import_talos()
#' skimr::skim(data)
#' ds <- import_talos()
import_talos <- function(key = "TALOS_REDCAP_API",
                         vars = c("record_id", "inkl_rnumb", "cpr", "talos_inkl03x", "talos_basis02a", "talos_basis02b", "basis_sys_site")) {
  REDCapR::redcap_read(
    redcap_uri = "https://redcap.au.dk/api/", token = keyring::key_get(key),
    fields = vars
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

#' Quick number of subjects in project
#'
#' @param key API key name for ekyring key retrieval
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' redcap_get_n()
redcap_get_n <- function(key = "SVD_REDCAP_API") {
  REDCapR::redcap_read_oneshot(
    redcap_uri = "https://redcap.au.dk/api/",
    token = keyring::key_get(key),
    fields = "record_id"
  ) |>
    purrr::pluck("data") |>
    nrow()
}

#' Modifies exported talos data to new database format
#'
#' @param data tibbled REDCap data
#' @param trial source trial name. TALOS or RESIST
#' @param key destination project API key name
#'
#' @return tibble with modified data
#' ds |> modify_data(index=0)
modify_data <- function(data,
                        index = redcap_get_n(),
                        trial = "TALOS",
                        key = "SVD_REDCAP_API",
                        id.var = inkl_rnumb,
                        cpr.var = cpr,
                        name.var = talos_inkl03x,
                        date.var = talos_basis02a,
                        time.var = talos_basis02b) {
  ids <- seq_len(nrow(data)) + (index)

  data |>
    dplyr::mutate(ids = ids) |>
    dplyr::transmute(
      record_id = glue::glue("svd_{ids}"),
      trial_id = {{ id.var }},
      cpr = {{ cpr.var }},
      name = {{ name.var }},
      index_date = lubridate::ymd({{ date.var }}),
      index_time = format(as.POSIXct({{ time.var }}), "%H:%M"),
      trial_name = dplyr::case_match(
        trial,
        "TALOS" ~ 1,
        "RESIST" ~ 2
      ),
      basis_complete = 2
    )
}


#' Write data to REDCap db
#'
#' @param key project key set in `keyring`
#' @param data data to write
#'
#' @return
#' @examples
#' ds |>
#'   filter_talos_site() |>
#'   modify_data() |>
#'   write2new_db()
write2new_db <- function(data, key = "SVD_REDCAP_API") {
  REDCapR::redcap_write(
    redcap_uri = "https://redcap.au.dk/api/",
    token = keyring::key_get(key),
    ds_to_write = data
  )
}
