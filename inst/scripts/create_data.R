library(dplyr)
library(readr)

N <- 1000

patient_ids <- 1:N
visit_1_ids <- 1:N
visit_2_ids <- (N+1):(2*N)
visit_3_ids <- (2*N + 1):(3*N)

mockup_socio_demographics <- socio_demographics(N, ids = patient_ids)

mockup_cdr_1 <- cdr(N, visit = "V1", ids = patient_ids, visit_ids = visit_1_ids)
mockup_cdr_2 <- cdr(N, visit = "V2", ids = patient_ids, visit_ids = visit_2_ids)
mockup_cdr_3 <- cdr(N, visit = "V3", ids = patient_ids, visit_ids = visit_3_ids)
mockup_cdr <- bind_rows(mockup_cdr_1, mockup_cdr_2, mockup_cdr_3)


mockup_rbans_1 <- rbans(N, visit = "V1", ids = patient_ids, visit_ids = visit_1_ids)
mockup_rbans_2 <- rbans(N, visit = "V2", ids = patient_ids, visit_ids = visit_2_ids)
mockup_rbans_3 <- rbans(N, visit = "V3", ids = patient_ids, visit_ids = visit_3_ids)
mockup_rbans <- bind_rows(mockup_rbans_1, mockup_rbans_2, mockup_rbans_3)

mockup_apoe <- apoe(N, visit = "V1", ids = patient_ids)

mockup_csf <- csf(N, visit = "V1", ids = patient_ids, visit_ids = visit_1_ids)

mockup_four_mountains <- four_mountains(N, visit = "V1", ids = patient_ids, visit_ids = visit_ids_1)

mockup_flanker <- flanker(N, visit = "V1", ids = patient_ids, visit_ids = visit_ids_1)

mockup_dot_counting <- dot_counting(N, visit = "V1", ids = patient_ids, visit_ids = visit_ids_1)


write_csv(mockup_socio_demographics, "inst/mockup_data/socio_demographics.csv")
write_csv(mockup_cdr, "inst/mockup_data/cdr.csv")

