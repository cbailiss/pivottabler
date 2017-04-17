#' Birmingham Trains, Dec 2016-Feb 2017.
#'
#' A dataset containing all of the trains that either
#' originated at, passed through or terminated at
#' Birmingham New Street railway station in the UK
#' between 1st December 2016 and 28th February 2017
#' inclusive.
#'
#' @format A data frame with 83710 rows and 16 variables:
#' \describe{
#'   \item{ServiceId}{Unique train identifier}
#'   \item{Status}{Train status: A=Active, C=Cancelled, R=Reinstated}
#'   \item{TOC}{Train operating company}
#'   \item{TrainCategory}{Express Passenger or Ordinary Passenger}
#'   \item{PowerType}{DMU=Diesel Multiple Unit, EMU=Electrical Multiple Unit,
#'   HST=High Speed Train}
#'   \item{SchedSpeedMPH}{Scheduled maximum speed (in miles per hour)}
#'   \item{Origin}{3-letter code denoting the scheduled origin of the train}
#'   \item{OriginGbttDeparture}{Scheduled departure time in the Great Britain
#'   Train Timetable (GBTT) from the origin station}
#'   \item{OriginActualDeparture}{Actual departure time from the origin station}
#'   \item{GbttArrival}{Scheduled arrival time in Birmingham in the GBTT}
#'   \item{ActualArrival}{Actual arrival time in Birmingham in the GBTT}
#'   \item{GbttDeparture}{Scheduled departture time from Birmingham in the GBTT}
#'   \item{ActualDeparture}{Actual departure time from Birmingham in the GBTT}
#'   \item{Destination}{3-letter code denoting the scheduled destination of the train}
#'   \item{DestinationGbttArrival}{Scheduled arrival time in the GBTT at the
#'   destination station}
#'   \item{DestinationActualArrival}{Actual arrival time at the destination station}
#' }
#' @source \url{http://www.recenttraintimes.co.uk/}
"bhmtrains"



#' Birmingham Train Disruptions, Dec 2016-Feb 2017.
#'
#' A dataset containing details of the trains in the
#' bhmtrains dataset that were partially/wholly
#' cancelled and/or reinstated.
#'
#' @format A data frame with 2982 rows and  10 variables:
#' \describe{
#'   \item{ServiceId}{Unique train identifier}
#'   \item{LastCancellationAt}{Time of the last cancellation}
#'   \item{LastCancellationLocation}{3-letter code denoting the location of the
#'   last cancellation}
#'   \item{LastCancellationReasonCategory}{The broad reason why the train was
#'   cancelled}
#'   \item{LastCancellationReasonDesc}{A more specific reason why the train was
#'   cancelled}
#'   \item{LastReinstatedAt}{Time of the last reinstatement}
#'   \item{LastChangeOfOriginAt}{Time of the last change of origin}
#'   \item{LastChangeOfOriginLocation}{3-letter code denoting the location of
#'   the last change of origin}
#'   \item{LastChangeOfOriginReasonCategory}{The broad reason why the origin was
#'   changed}
#'   \item{LastChangeOfOriginReasonDesc}{A more specific reason why the origin
#'   was changed}
#' }
#' @source \url{http://www.recenttraintimes.co.uk/}
"bhmtraindisruption"



#' Train Stations
#'
#' A reference dataset listing the codes, names and locations
#' of trains stations in Great Britain.
#'
#' @format A data frame with 2568 rows and  7 variables:
#' \describe{
#'   \item{CrsCode}{3-letter code for the station}
#'   \item{StationName}{The name of the station}
#'   \item{OsEasting}{The UK Ordnance Survey Easting coordinate for the station}
#'   \item{OsNorthing}{The UK Ordnance Survey Northing coordinate for the
#'   station}
#'   \item{GridReference}{Grid reference for the station}
#'   \item{Latitude}{Latitude of the station location}
#'   \item{Longitude}{Longitude of the station location}
#' }
#' @source \url{http://www.recenttraintimes.co.uk/}
"trainstations"
