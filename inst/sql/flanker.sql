CREATE TABLE flanker(
  patient_id int,
  visit_id int,
  visit text,
  flanker_total_trials int,
  flanker_score numeric,
  flanker_error_diff int,
  flanker_total_corr int,
  flanker_total_mean numeric,
  flanker_total_median numeric,
  flanker_total_stdev numeric,
  flanker_congr_corr int,
  flanker_congr_mean numeric,
  flanker_congr_median numeric,
  flanker_congr_stdev numeric,
  flanker_incongr_corr int,
  flanker_incongr_mean numeric,
  flanker_incongr_median numeric,
  flanker_incongr_stdev numeric,
  flanker_left_corr int,
  flanker_left_mean numeric,
  flanker_left_median numeric,
  flanker_left_stdev numeric,
  flanker_right_corr int,
  flanker_right_mean numeric,
  flanker_right_median numeric,
  flanker_right_stdev numeric,
  flanker_up_corr int,
  flanker_up_mean numeric,
  flanker_up_median numeric,
  flanker_up_stdev numeric,
  flanker_down_corr int,
  flanker_down_mean numeric,
  flanker_down_median numeric,
  flanker_down_stdev numeric,
  assessment_performed text,
  assessment_date date,
  reason_not_performed text
);
