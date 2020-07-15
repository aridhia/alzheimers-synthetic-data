#' @export
psqi <- function(n, visit = "V1", missing = 0.1, ids = NULL, visit_ids = NULL) {
  ids <- handle_ids(n, ids)
  visit_ids <- handle_ids(n, visit_ids)

  df <- data.frame(
    patient_id = ids,
    visit_id = visit_ids,
    visit = visit,
    psqi_total,
    psqi_component_1_score,
    psqi_component_2_score,
    psqi_component_3_score,
    psqi_component_4_score,
    psqi_component_5_score,
    psqi_component_6_score,
    psqi_component_7_score,
    psqi_item1,
    psqi_item2,
    psqi_item3,
    psqi_item4,
    psqi_item5_a,
    psqi_item5_b,
    psqi_item5_c,
    psqi_item5_d,
    psqi_item5_e,
    psqi_item5_f,
    psqi_item5_g,
    psqi_item5_h,
    psqi_item5_i,
    psqi_item5_j,
    psqi_item5_yesno,
    psqi_item6,
    psqi_item7,
    psqi_item8,
    psqi_item9,
    psqi_item10,
    psqi_item10_a,
    psqi_item10_b,
    psqi_item10_c,
    psqi_item10_d,
    psqi_item10_e,
    psqi_item10_yesno,
    assessment_date,
    assessment_performed,
    reason_not_done

  )
  return(df)
}
