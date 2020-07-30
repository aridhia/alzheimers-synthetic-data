
#' @title FAIR JSON Templates
#' @description Create FAIR JSON templates from existing data
#' @param df A data.frame
#' @param dataset_name Name of the dataset to be shown in the library
#' @param lookups Indexes of columns to create lookups for
#' @import jsonlite
#' @export
fair_template <- function(df, dataset_name = as.character(substitute(df)), lookups = c(), title = "", description = "", creator = "", contactPoint = "") {

  field_names <- names(df)
  field_lookups <- field_names[lookups]

  fields <- lapply(field_names, function(field_name) {
    if(field_name %in% field_lookups) {
      constraints <- toupper(field_name)
    } else {
      constraints <- ""
    }

    l <- list(description = unbox(field_description_template(field_name)),
              name = unbox(field_name),
              label =  unbox(field_label_template(field_name)),
              type = unbox(""),
              constraints = unbox(constraints))
    return(l)
  })

  if(length(field_lookups) == 0) {
    lookups <- NULL
  } else {
    lookups <- lapply(field_lookups, function(field_lookup) {
      unique_values <- unique(df[[field_lookup]])
      unique_values <- unique_values[!is.na(unique_values)]

      lapply(unique_values, function(x) {
        list(description = unbox(x), name = unbox(x))
      })
    })
    names(lookups) <- toupper(field_lookups)
  }

  l <- list(
    datasets = list(
      catalogue = list(id = unbox(dataset_name), title = unbox(title), description = unbox(description),
                       creator = unbox(creator), contactPoint = unbox(contactPoint), licence = unbox(""),
                       versionInfo = unbox(""), keyword = "",
                       publisher = list(
                         name = unbox(""),
                         url = unbox("")
                       )),
      dictionaries = list(id = unbox(dataset_name),
                          fields = fields,
                          lookups = lookups)
    )
  )

  toJSON(l, pretty = TRUE)
}

field_label_template <- function(field_name) {
  return(field_name)
}

field_description_template <- function(field_name) {
  return(field_name)
}
