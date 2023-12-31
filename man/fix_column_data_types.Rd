% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_column_data_types.R
\name{fix_column_data_types}
\alias{fix_column_data_types}
\title{fix_column_data_types}
\usage{
fix_column_data_types(df_in)
}
\arguments{
\item{df_in}{The `data.frame` object to fix.}
}
\value{
A `data.frame` object containing the data from `df_in` but with
fixed column data types.
}
\description{
Freshly exported data has all item-data columns as type
"character".
This function converts these columns to ideal types (e.g. integer).
Currently simply converts all multiple-choice columns to integer.
Future task: Add conversion to other data types as needed.
}
