# Use ALA4R with servers from gbifswedens.se

server_config <- list(
  max_occurrence_records = 500000,
  server_max_url_length = 8150, 
  brand = "ALA4R", 
  notify = "Please use https://github.com/AtlasOfLivingAustralia/ALA4R/issues/ or email to support@ala.org.au", 
  support_email = "support@ala.org.au", 
  reasons_function = "ala_reasons", 
  fields_function = "ala_fields", 
  occurrences_function = "occurrences", 
  config_function = "ala_config", 
  base_url_spatial = "http://gbifsweden.se/spatial/", 
  base_url_bie = "http://gbifsweden.se/bie/", 
  base_url_biocache = "http://gbifsweden.se/biocache/", 
  base_url_alaspatial = "http://gbifsweden.se/alaspatial/", 
  base_url_images = "http://gbifsweden.se/images/", 
  base_url_logger = "http://gbifsweden.se/logger/", 
  base_url_fieldguide = "http://gbifsweden.se/fieldguide/",
  base_url_lists = "http://gbifsweden.se/lists/"
)

if (!"ALA4R_server_config" %in% names(options())) {
  message("No existing ALA4R server config, setting new...")
  options(ALA4R_server_config = server_config)
} else {
  message("Overwriting existing ALA server config with new...")
  options(ALA4R_server_config = server_config)
}

