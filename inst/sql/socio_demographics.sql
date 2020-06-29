CREATE TABLE socio_demographics(
  patient_id integer PRIMARY KEY,
  site_name text,
  site_id text,
  sex = text,
  age_years integer,
  age_months integer,
  handedness integer,
  years_education integer,
  marital_status text,
  ethnicity text
);
