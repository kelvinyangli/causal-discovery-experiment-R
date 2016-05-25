# analyse output.out file for profiling
proftable <- function(file, lines = 10) {
  profdata <- readLines(file)
  interval <- as.numeric(strsplit(profdata[1L], "=")[[1L]][2L]) / 1e+06
  filelines <- grep("#File", profdata)
  files <- profdata[filelines]
  profdata <- profdata[-c(1, filelines)]
  total.time <- interval * length(profdata)
  ncalls <- length(profdata)
  profdata <- gsub("\\\"| $", "", profdata)
  calls <- lapply(profdata, function(x) rev(unlist(strsplit(x, " "))))
  stacktable <- as.data.frame(table(sapply(calls, function(x) paste(x, collapse = " > "))) / ncalls * 100, stringsAsFactors = FALSE)
  stacktable <- stacktable[order(stacktable$Freq[], decreasing = TRUE), 2:1]
  colnames(stacktable) <- c("PctTime", "Call")
  stacktable <- head(stacktable, lines)
  shortcalls = strsplit(stacktable$Call, " > ")
  shortcalls.len <- range(sapply(shortcalls, length))
  parent.call <- unlist(lapply(seq(shortcalls.len[1]), function(i) Reduce(intersect, lapply(shortcalls,"[[", i))))
  shortcalls <- lapply(shortcalls, function(x) setdiff(x, parent.call))
  stacktable$Call = sapply(shortcalls, function(x) paste(x, collapse = " > "))
  if (length(parent.call) > 0) {
    parent.call <- paste(paste(parent.call, collapse = " > "), "> ...")
  } else {
    parent.call <- "None"
  }
  frac <- sum(stacktable$PctTime)
  attr(stacktable, "total.time") <- total.time
  attr(stacktable, "parent.call") <- parent.call
  attr(stacktable, "files") <- files
  attr(stacktable, "total.pct.time") <- frac
  print(stacktable, row.names=FALSE, right=FALSE, digits=3)
  if(length(files) > 0) {
    cat("\n")
    cat(paste(files, collapse="\n"))
    cat("\n")
  }
  cat(paste("\nParent Call:", parent.call))
  cat(paste("\n\nTotal Time:", total.time, "seconds\n"))
  cat(paste0("Percent of run time represented: ", format(frac, digits=3)), "%")
  
  invisible(stacktable)
}