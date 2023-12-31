% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ExtractData.R
\name{createDatabaseDetails}
\alias{createDatabaseDetails}
\title{Create a setting that holds the details about the cdmDatabase connection for data extraction}
\usage{
createDatabaseDetails(
  connectionDetails,
  cdmDatabaseSchema,
  cdmDatabaseName,
  tempEmulationSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cdmDatabaseSchema,
  cohortTable = "cohort",
  outcomeDatabaseSchema = cdmDatabaseSchema,
  outcomeTable = "cohort",
  cohortId = NULL,
  outcomeIds = NULL,
  cdmVersion = 5
)
}
\arguments{
\item{connectionDetails}{An R object of type \code{connectionDetails} created using the
function \code{createConnectionDetails} in the
\code{DatabaseConnector} package.}

\item{cdmDatabaseSchema}{The name of the database schema that contains the OMOP CDM
instance. Requires read permissions to this database. On SQL
Server, this should specifiy both the database and the schema,
so for example 'cdm_instance.dbo'.}

\item{cdmDatabaseName}{A string with a shareable name of the database (this will be shown to OHDSI researchers if the results get transported)}

\item{tempEmulationSchema}{For dmbs like Oracle only: the name of the database schema where you
want all temporary tables to be managed. Requires
create/insert permissions to this database.}

\item{cohortDatabaseSchema}{The name of the database schema that is the location where the
target cohorts are available.  Requires read
permissions to this database.}

\item{cohortTable}{The tablename that contains the target cohorts.  Expectation is cohortTable
has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
COHORT_START_DATE, COHORT_END_DATE.}

\item{outcomeDatabaseSchema}{The name of the database schema that is the location where the
data used to define the outcome cohorts is available. Requires read permissions to
this database.}

\item{outcomeTable}{The tablename that contains the outcome cohorts.  Expectation is
outcomeTable has format of COHORT table: COHORT_DEFINITION_ID,
SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.}

\item{cohortId}{An integer specifying the cohort id for the target cohort}

\item{outcomeIds}{A single integer or vector of integers specifying the cohort ids for the outcome cohorts}

\item{cdmVersion}{Define the OMOP CDM version used: currently support "4" and
"5".}
}
\value{
A list with the the database specific settings (this is used by the runMultiplePlp function and the skeleton packages)
}
\description{
Create a setting that holds the details about the cdmDatabase connection for data extraction
}
\details{
This function simply stores the settings for communicating with the cdmDatabase when extracting 
the target cohort and outcomes
}
