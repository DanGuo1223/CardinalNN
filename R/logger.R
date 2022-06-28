
#### Logging and flushing log to output files ####
## -----------------------------------------------

.log <- function(...) {
	msg <- paste0(c(date(), ": ", ...), collapse="")
	.Cardinal$log <- append(.Cardinal$log, list(msg))
	flushtime <- getOption("Cardinal.flush")
	if ( is.numeric(flushtime) && is.finite(flushtime) ) {
		elapsed <- proc.time()[3] - .Cardinal$time$flush
		if ( elapsed > flushtime )
			.log.flush()
	}
}

.log.collapse <- function(...) {
	.log(paste0(c(...), collapse="\n"))
}

.log.flush <- function(e=.Cardinal) {
	log <- getOption("Cardinal.log")
	if ( isTRUE(log) || is.character(log) ) {
		if ( is.character(log) ) {
			path <- normalizePath(log, mustWork=FALSE)
		} else {
			path <- file.path(tempdir(), "Cardinal.log")
		}
		res <- tryCatch({
			e$time$flush <- proc.time()[3]
			con <- file(path, open="at")
			for ( m in e$log )
				writeLines(c(m, "\n"), con=con)
			close(con)
			e$log <- list()
			sink()
			TRUE
		}, error=function(e) FALSE)
	} else {
		res <- FALSE
	}
	invisible(res)
}

#### User messages ####
## --------------------

.message <- function(...) {
	.log(...)
	for ( f in .Cardinal$message )
		f(...)
}

.console <- function(...) {
	if ( getCardinalVerbose() ) {
		message(...)
		flush.console()
	}
}

.time.start <- function() {
	.Cardinal$time$start <- proc.time()
}

.time.stop <- function() {
	.Cardinal$time$stop <- proc.time()
	time <- .Cardinal$time$stop - .Cardinal$time$start
	.log("took ", round(time[[3]], digits=2), " seconds.")
}

#### Generate R console history ####
## ---------------------------------

.history <- function(toString = FALSE) {
	file <- tempfile(fileext=".Rhistory")
	savehistory(file)
	history <- readLines(file)
	if ( toString ) {
		history <- paste0(history, collapse="\n")
		history <- c("Session history:\n", history)
		history <- paste0(history, collapse="")
	}
	history
}

#### Capture session info ####
## ---------------------------

.session <- function() {
	info <- capture.output(sessionInfo())
	paste0(info, collapse="\n")
}

#### Errors and warnings ####
## --------------------------

.stop <- function(...) {
	.log("Error:", ...)
	stop(..., call.=FALSE)
}

.warning <- function(...) {
	.log("Warning:", ...)
	warning(..., call.=FALSE)
}

