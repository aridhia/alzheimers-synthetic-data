CREATE TABLE cdr(
  patient_id int,
  visit_id int,
  visit text,
  cdr_global_score numeric,
  cdr_sum_of_box numeric,
  cdr_community_affairs numeric,
  cdr_home_hobbies numeric,
  cdr_judgement numeric,
  cdr_memory numeric,
  cdr_orientation numeric,
  cdr_personal_care numeric,
  assessment_performed text,
  assessment_date date,
  reason_not_performed text
);
