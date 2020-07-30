library(alzheimersSyntheticData)

N <- 10000
out_dir <- "inst/fair_json"

apoe_sample <- apoe(N)
apoe_fair <- fair_template(apoe_sample, title = "APOE", creator = "Harry Peaker", contactPoint = "harry.peaker@aridhia.com", lookups = c(3, 4, 6))
writeLines(apoe_fair, file.path(out_dir, "apoe.json"))

aiadl_sample <- aiadl(N)
aiadl_fair <- fair_template(aiadl_sample, lookups = c(8:217, 219))
writeLines(aiadl_fair, file.path(out_dir, "aiadl.json"))

cdr_sample <- cdr(N)
cdr_fair <- fair_template(cdr_sample, lookups = c(4, 6:12, 14))
writeLines(cdr_fair, file.path(out_dir, "cdr.json"))

csf_sample <- csf(N)
csf_fair <- fair_template(csf_sample, lookups = c(8, 10:13))
writeLines(csf_fair, file.path(out_dir, "csf.json"))

dot_counting_sample <- dot_counting(N)
dot_counting_fair <- fair_template(dot_counting_sample)
writeLines(dot_counting_fair, file.path(out_dir, "dot_counting.json"))

family_history_sample <- family_history(N)
family_history_fair <- fair_template(family_history_sample)
writeLines(family_history_fair, file.path(out_dir, "family_history.json"))

favourites_sample <- favourites(N)
favourites_fair <- fair_template(favourites_sample)
writeLines(favourites_fair, file.path(out_dir, "favourites.json"))

gds_sample <- gds(N)
gds_fair <- fair_template(gds_sample)
writeLines(gds_fair, file.path(out_dir, "gds.json"))

hatice_sample <- hatice(N)
hatice_fair <- fair_template(hatice_sample)
writeLines(hatice_fair, file.path(out_dir, "hatice.json"))

lacunes_infarcts_sample <- lacunes_infarcts(N)
lacunes_infarcts_fair <- fair_template(lacunes_infarcts_sample)
writeLines(lacunes_infarcts_fair, file.path(out_dir, "lacunes_infarcts.json"))

life_sample <- life(N)
life_fair <- fair_template(life_sample)
writeLines(life_fair, file.path(out_dir, "life.json"))

mmse_sample <- mmse(N)
mmse_fair <- fair_template(mmse_sample)
writeLines(mmse_fair, file.path(out_dir, "mmse.json"))

psqi_sample <- psqi(N)
psqi_fair <- fair_template(psqi_sample)
writeLines(psqi_fair, file.path(out_dir, "psqi.json"))

radiological_read_sample <- radiological_read(N)
radiological_read_fair <- fair_template(radiological_read_sample)
writeLines(radiological_read_fair, file.path(out_dir, "radiological_read.json"))

rbans_sample <- rbans(N)
rbans_fair <- fair_template(rbans_sample)
writeLines(rbans_fair, file.path(out_dir, "rbans.json"))

socio_demographics_sample <- socio_demographics(N)
socio_demographics_fair <- fair_template(socio_demographics_sample)
writeLines(socio_demographics_fair, file.path(out_dir, "socio_demographics.json"))

stai_40_sample <- stai_40(N)
stai_40_fair <- fair_template(stai_40_sample)
writeLines(stai_40_fair, file.path(out_dir, "stai_40.json"))

vital_signs_sample <- vital_signs(N)
vital_signs_fair <- fair_template(vital_signs_sample)
writeLines(vital_signs_fair, file.path(out_dir, "vital_signs.json"))

volumetric_sample <- volumetric(N)
volumetric_fair <- fair_template(volumetric_sample)
writeLines(volumetric_fair, file.path(out_dir, "volumetric.json"))

vr_supermarket_trolley_sample <- vr_supermarket_trolley(N)
vr_supermarket_trolley_fair <- fair_template(vr_supermarket_trolley_sample)
writeLines(vr_supermarket_trolley_fair, file.path(out_dir, "vr_supermarket_trolley.json"))


