% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Methods-ViralPop.R
\name{createAViralPop}
\alias{createAViralPop}
\title{createAViralPop}
\usage{
createAViralPop(host_id, obs_time, seq, id_seq = "seq_ID",
  seq_value = "seq", prop = "prop", compact = FALSE)
}
\arguments{
\item{host_id}{host ID which viral pop is observed}

\item{obs_time}{time of the observation (numeric or date)}

\item{seq}{a data.frame of sequences ID, sequences and counts}

\item{id_seq}{column name containing the sequences ID}

\item{seq_value}{column name containing the sequences}

\item{prop}{column name containing the count of each sequences}

\item{compact}{boolean, default FALSE, if TRUE will try group identicals sequences (not implemented yet)}
}
\description{
Create a new ViralPop object
}
