CREATE TABLE vital_signs(
  patient_id int,
  visit_id int,
  visit text,
  vital_signs_collected text,
  reason_not_collected text,
  date_collected date,
  height numeric,
  weight numeric,
  hip_circumference numeric,
  waist_circumference numeric,
  systolic_bp numeric,
  diastolic_bp numeric,
  pulse numeric
);
