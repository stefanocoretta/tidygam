#' Number of gestures by infants at 10, 11 and 12 months
#'
#' This data table contains counts of three type of gestures performed by 60
#' infants  from Bengali, Chinese and British backgrounds.
#'
#' @format A tibble with 540 observations and 5 variables: \describe{
#'   \item{dyad}{Unique parent/infant dyad ID.}
#'   \item{background}{Cultural background of dyad.}
#'   \item{months}{Time point in infant months.}
#'   \item{gesture}{Type of gesture.}
#'   \item{count}{Number of gestures.}
#'   }
#'
#' @source \doi{10.1111/cdev.13406}
"gest"

#' ERP to structural violation in music and language
#'
#' This data table contains ERP amplitude data from 39 subjects listening to speech and music.
#'
#' @format A tibble with 17160 observations and 6 variables: \describe{
#'   \item{t}{Time from stimulus onset in milliseconds.}
#'   \item{electrode}{Electrode number.}
#'   \item{voltage}{Electrode voltage at time t.}
#'   \item{stimulus.condition}{Language vs music.}
#'   \item{grammar.condition}{Structural type (grammatical vs ungrammatical).}
#'   }
#'
#' @source \doi{10.31234/osf.io/e9w3v}
"struct"
