#' IBM Cognos Mashup Service interface for R
#'
#' @name cognosMashup
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for communication with IBM Cognos Mashup Service
#' @format An \code{\link{R6Class}} generator object
#' @section Public Members:
#'   \describe{
#'     \item{\code{dispatcher_url}}{Stores IBM Cognos Mashup Service dispatcher URL}
#'     \item{\code{credential_template}}{Stores IBM Cognos Mashup Service authentication credential template file path}
#'   }
#' @section Private Members:
#'   \describe{
#'     \item{\code{namespace}}{Stores authentication namespace}
#'     \item{\code{username}}{Stures authentication username}
#'     \item{\code{password}}{Stores authentication password}
#'   }
#' @section Public Methods:
#'   \describe{
#'     \item{\code{logon()}}{Logon to the IBM Cognos Mashup Service using the provided credentials (\code{namespace, username, password}).}
#'     \item{\code{logoff()}}{Logoff from  the IBM Cognos Mashup Servce.}
#'     \item{\code{get_report_data(report_id, dataset_id, col_names)}}{Retrieve report dataset (\code{dataset_id}) for the specified report (\code{report_id}).}
#'   }

library(R6)
library(xml2)
library(httr)

cognosMashup <- R6Class("cognosMashup",
     public = list(
          dispatcher_url = NULL,
          credential_template = NULL,
          initialize = function(dispatcher_url = NA, credential_template = NA,
                                namespace = NA, username = NA, password = NA) {
               self$dispatcher_url <- dispatcher_url
               self$credential_template <- credential_template
               private$namespace <- namespace
               private$username <- username
               private$password <- password
          },
          logon = function() {
               # read credentials template file
               credentials <- read_xml(paste(readLines(self$credential_template), collapse=""))

               # find placeholder fields
               credential_namespace <- xml_find_all(credentials, ".//namespaceValue")
               credential_username <- xml_find_all(credentials, ".//usernameValue")
               credential_password <- xml_find_all(credentials, ".//passwordValue")

               # set field values
               xml_text(credential_namespace) <- private$namespace
               xml_text(credential_username) <- private$username
               xml_text(credential_password) <- private$password

               # rename placeholder fields
               xml_name(credential_namespace) <- "actualValue"
               xml_name(credential_username) <- "actualValue"
               xml_name(credential_password) <- "actualValue"

               # convert to character string and drop xml declaration line
               credentials <- strsplit(as.character(credentials), "\n\\s*", perl=TRUE)[[1]]
               credentials <- url_escape(paste(credentials[2:length(credentials)], collapse=""))

               # build logon url and log on
               req_logon <- GET(paste0(self$dispatcher_url, "/rds/auth/logon?xmlData=", credentials))
          },
          logoff = function () {
               # logoff
               req_logout <- GET(paste0(self$dispatcher_url, "/rds/auth/logoff"))
          },
          get_report_data = function(report_id, dataset_id, col_names) {
               # build report url
               report_url <- paste0(self$dispatcher_url, "/rds/reportData/report/")
               report_options <- "?fmt=DataSetJSON"
               report_url <- paste0(report_url, report_id, report_options)

               # get report content
               req_report <- GET(report_url)

               # get list data from report content
               list <- content(req_report)$dataSet$dataTable[[dataset_id]]$row

               # convert list to dataframe
               df <- as.data.frame(matrix(unlist(list), nrow=length(list), byrow=TRUE), stringsAsFactors=FALSE)
               colnames(df) <- col_names
               rownames(df) <- NULL

               # return dataframe
               return(df)
          }
     ),
     private = list (
          namespace = NULL,
          username = NULL,
          password = NULL
     )
)
