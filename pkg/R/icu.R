#' Data from ICU admissions.
#'
#' A dataset containing the data from some ICU admissions and its outcomes at the year 2010.
#' The variables are as follows:
#'
#' \itemize{
#'   \item \code{Unit} The name of the ICU unit.
#'   \item \code{Age} Patient age.
#'   \item \code{Gender} Patient sex.
#'   \item \code{BMI} Body Mass Index.
#'   \item \code{UnitAdmissionDate} ICU unit admission date.
#'   \item \code{UnitAdmissionTime} ICU unit admission time.
#'   \item \code{UnitDischargeDate} ICU unit discharge date.
#'   \item \code{UnitDischargeTime} ICU unit discharge time.
#'   \item \code{UnitDischargeName} Death or discharge.
#'   \item \code{UnitDestinationName} ICU unit destination after discharge.
#'   \item \code{HospitalAdmissionDate} Hospital admission date.
#'   \item \code{HospitalDischargeDate} Hospital discharge date.
#'   \item \code{HospitalDischargeName} Hospital destination discharge.
#'   \item \code{LengthHospitalStayPriorUnitAdmission} Hospital length of stay before unit admission.
#'   \item \code{AdmissionSourceName} The origin of the patient before ICU unit admission.
#'   \item \code{AdmissionTypeName_pri} Admission for Elective surgery, Urgent surgery or Clinical treatment.
#'   \item \code{AdmissionReasonName_pri} Main diagnosis groups.
#'   \item \code{InfectionIsAtAdmission}  ICU admission with infection?
#'   \item \code{IsMechanicalVentilation1h} Required mechanichal ventilation at 1 hour of admission.
#'   \item \code{CharlsonComorbidityIndex} Charlso comorbidity index.
#'   \item \code{Saps3Points} SAPS 3 score
#'   \item \code{Saps3DeathProbabilityStandardEquation} SAPS 3 estimated probability
#'   \item \code{SofaScore} SOFA score.
#' }
#'
#' @format A data frame with 11549 rows and 24 variables
"icu"
#> [1] "icu"
