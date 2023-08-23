dir_name <- paste0("data/output/cos_sims_mastun/")
tunisia_source <- readRDS(paste0(dir_name, "turess", "/cos_simsdf_all_bysource.rds"))
egypt_source <- readRDS(paste0(dir_name, "masress", "/cos_simsdf_all_bysource.rds"))

## Note do this in conjunction with Stata .do file "24_synth_did"

#Bring in source dataframes by country
# tunisia_source <- readRDS("C:/Users/neilke/Dropbox/arabress_medcrit/turess_medcrit/data/output/cos_sims/cos_sims_nws_by_source.rds")
# egypt_source <- readRDS("C:/Users/neilke/Dropbox/arabress_medcrit/masress_medcrit/data/output/cos_sims/cos_sims_nws_by_source.rds")

# Get a country unit ID
tunisia_source$country_ID <- "Tunisia"
egypt_source$country_ID <- "Egypt"

#Append together
appended_df <- rbind(tunisia_source, egypt_source)

#Export to csv for wrangling in Stata .do file "24_synth_did"
write.csv(appended_df, file = "data/output/cos_sims_mastun/diffindiffdata.csv", row.names = FALSE)