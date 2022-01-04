#' Goal metrics for the Balance of State
#'
#' Thresholds for Balance of State key performance metrics pertaining to specific project types (as column headers)
#'
#' @format A tibble with 14 rows and 11 variables:
#' \describe{
#'   \item{SummaryMeasure}{ The KPM description}
#'   \item{Measure}{ The KPM short name}
#'   \item{Operator}{ The Operator pertaining to successful performance for this metric}
#'   \item{1,2,8,4,3,13,12,9}{ Thresholds for the specific Project Types}
#' }
"BoSGoals"

#' Geocodes for the Balance of State
#'
#' Maps geocodes to the County/City's of Ohio
#'
#' @format A tibble with 123 rows and 5 variables:
#' \describe{
#'   \item{GeographicCode}{ Geocode}
#'   \item{State}{ State}
#'   \item{Name}{ the City/County Name}
#'   \item{Type}{ the type (City/County)}
#'   \item{County}{ the County (derived column)}
#' }
#' @source \href{https://www.hud.gov/sites/dfiles/CPD/documents/FY-2021-GeoCodes-with-PPRN_Final.pdf}{HUD Geocodes PDF FY 2021}. See \link[hud.extract]{hud_geocodes} for instructions on extracting these in subsequent years.
"geocodes"

#' HUD_specs
#'
#' A table of the HUD CSV variable specifications
#'
#' @format A tibble with 728 rows and 4 variables:
#' \describe{
#'   \item{DataElementID}{ The ID number}
#'   \item{DataElement}{ The column name for the variable}
#'   \item{ReferenceNo}{ The value pertaining to the variable}
#'   \item{Description}{ The description of that value}
#' }
#' @seealso hud.extract::hud_translations
#' @source \href{https://hudhdx.info/Resources/Vendors/HMIS_CSV_Specifications_FY2022_v1.2_clean.pdf}{HUD Specs PDF FY 2021}. See \link[hud.extract]{hud_value_tables} for instructions on extracting these in subsequent years.

"HUD_specs"

#' Regions
#'
#' A table of the Balance of State Regions
#'
#' @format A tibble with 82 rows and 3 variables:
#' \describe{
#'   \item{County}{ The county name}
#'   \item{Region}{ The Region number}
#'   \item{RegionName}{ The Region name}
#' }
#' @source \href{http://hmis.cohhio.org/index.php?pg=kb.page&id=77}{BoS Regions}.

"Regions"

#' The Scoring Rubric
#'
#' Key Performance measures for evaluating projects
#'
#' @format A tibble with 85 rows and 6 variables:
#' \describe{
#'   \item{metric}{ the metric name}
#'   \item{ProjectType}{ The Project Type code to which the metric pertains}
#'   \item{goal_type}{ the direction of the goal}
#'   \item{minimum}{ the minimum cut-off to score}
#'   \item{maximum}{ the maximum value possible}
#'   \item{points}{ the points possible for the metric}
#' }

"scoring_rubric"

#' Service Areas
#'
#' Details about each Service area in the Balance of State
#'
#' @format A tibble with 88 rows and 13 variables:
#' \describe{
#'   \item{State}{}
#'   \item{County}{}
#'   \item{SSVFServiceArea}{ the SSVF program for the area}
#'   \item{VAMC}{ the VAMC program for the area}
#'   \item{HomelessPlanningRegion}{ The BoS Region}
#'   \item{SOAR}{ the SOAR program for the region}
#'   \item{Bridges}{ the Bridges region}
#'   \item{BridgesRegCoordinator}{ the Bridges regional coordinator contact details}
#'   \item{CoC}{ the Continuum of Care for the area}
#'   \item{YHDP}{ the YHDP program for the area}
#'   \item{RHY}{ the RHY program for the area}
#'   \item{Unknown}{}
#'   \item{HNHF}{ the HNHF program for the area}
#' }

"ServiceAreas"
