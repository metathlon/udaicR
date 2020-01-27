\name{freq}
\alias{freq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
freq
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
It will do freqs
}
\usage{
freq(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Load default dataset HairEyeColor
data(HairEyeColor)
df <- as.data.frame(HairEyeColor)

## The function is currently defined as

freq(df,Sex) # frequency for Sex
freq(df,Sex, Eye) # frequency for Sex AND Eye
freq(df,Eye, group_by=Sex) # frequency for Sex AND Eye

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
