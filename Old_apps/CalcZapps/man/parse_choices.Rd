% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readQfile.R
\name{parse_choices}
\alias{parse_choices}
\title{Accepted syntax
Line starts with [a] or [+] means correct, with [a] meaning "keep order" and "+"
meaning "randomize order" [] or [ ] or [-] means incorrect
Next characters until open bracket are the choice text itself
Optionally [feedback] {param=whatever, param2=whatever}}
\usage{
parse_choices(lines)
}
\description{
Accepted syntax
Line starts with [a] or [+] means correct, with [a] meaning "keep order" and "+"
meaning "randomize order" [] or [ ] or [-] means incorrect
Next characters until open bracket are the choice text itself
Optionally [feedback] {param=whatever, param2=whatever}
}
