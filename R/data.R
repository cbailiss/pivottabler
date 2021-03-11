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
#'   \item{GbttDeparture}{Scheduled departure time from Birmingham in the GBTT}
#'   \item{ActualDeparture}{Actual departure time from Birmingham in the GBTT}
#'   \item{Destination}{3-letter code denoting the scheduled destination of the train}
#'   \item{DestinationGbttArrival}{Scheduled arrival time in the GBTT at the
#'   destination station}
#'   \item{DestinationActualArrival}{Actual arrival time at the destination station}
#' }
#' @source \url{https://www.recenttraintimes.co.uk/}
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
#' @source \url{https://www.recenttraintimes.co.uk/}
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
#' @source \url{https://www.recenttraintimes.co.uk/}
"trainstations"




#' Performance Comparison Results
#'
#' A reference dataset containing the full results of an example performance
#' comparison for different pivot table test cases.
#'
#' @format A data frame with 216 rows and  11 variables:
#' \describe{
#'   \item{id}{A unique identifier for this test case.}
#'   \item{evaluationMode}{The pivot table evaluation mode used for this test case.}
#'   \item{rowCount}{The number of rows in the data frame used for this test case.}
#'   \item{cellCount}{The number of cells in the pivot table used for this test case.}
#'   \item{argumentCheckMode}{The pivot table argument check mode used this test case.}
#'   \item{processingLibrary}{The pivot table processing library used this test case.}
#'   \item{description}{A description of this test case.}
#'   \item{completed}{A logical value indicating whether this test case completed.}
#'   \item{userTime}{The user time for this test case.}
#'   \item{systemTime}{The system time for this test case.}
#'   \item{elapsedTime}{The elapsed time for this test case.}
#' }
"pvtperfresults"




#' Performance Comparison Summary
#'
#' A reference dataset containing summary results of an example performance
#' comparison for different pivot table test cases.
#'
#' @format A data frame with 36 rows and  18 variables:
#' \describe{
#'   \item{id}{A unique identifier for this test case.}
#'   \item{evaluationMode}{The pivot table evaluation mode used for this test case.}
#'   \item{rowCount}{The number of rows in the data frame used for this test case.}
#'   \item{cellCount}{The number of cells in the pivot table used for this test case.}
#'   \item{argumentCheckMode}{The pivot table argument check mode used this test case.}
#'   \item{processingLibrary}{The pivot table processing library used this test case.}
#'   \item{description}{A description of this test case.}
#'   \item{userTimeAvg}{The average user time for this test case.}
#'   \item{systemTimeAvg}{The average system time for this test case.}
#'   \item{elapsedTimeAvg}{The average elapsed time for this test case.}
#'   \item{userTimeMin}{The minimum user time for this test case.}
#'   \item{userTimeMax}{The maximum user time for this test case.}
#'   \item{systemTimeMin}{The minimum system time for this test case.}
#'   \item{systemTimeMax}{The maximum system time for this test case.}
#'   \item{elapsedTimeMin}{The minimum elapsed time for this test case.}
#'   \item{elapsedTimeMax}{The maximum elapsed time for this test case.}
#'   \item{testName}{A short name for this test case.}
#'   \item{testIndex}{An index for this test case.}
#' }
"pvtperfsummary"

