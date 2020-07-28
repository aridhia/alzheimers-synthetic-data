library(alzheimersSyntheticData)

apoe_sample <- apoe(10000)
apoe_fair <- fair_template(apoe_sample, title = "APOE", creator = "Harry Peaker", contactPoint = "harry.peaker@aridhia.com", lookups = c(3, 4, 6))
writeLines(apoe_fair, "inst/fair_json/apoe.json")

aiadl_sample <- aiadl(10000)
aiadl_fair <- fair_template(aiadl_sample)
writeLines(aiadl_fair, "inst/fair_json/aiadl.json")
