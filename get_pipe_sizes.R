#### GATHER JOB PIPE SIZES ####

swSizes <- data.frame(
  size = ifelse(is.na(ex_mm(sw$ACTIVITY_TEXT)), ex_inches(sw$ACTIVITY_TEXT), ex_mm(sw$ACTIVITY_TEXT)),
  unit = ifelse(is.na(ex_mm(sw$ACTIVITY_TEXT, TRUE)), ex_inches(sw$ACTIVITY_TEXT, TRUE), ex_mm(sw$ACTIVITY_TEXT, TRUE)))
swSizes$unit <- trimws(gsub("\\d|\\.", "", swSizes$unit))
swSizes <- unique(na.omit(swSizes))

# job text
jobTextSizes <- data.frame(
  size = ifelse(is.na(ex_mm(jobtext$TEXT)), ex_inches(jobtext$TEXT), ex_mm(jobtext$TEXT)),
  unit = ifelse(is.na(ex_mm(jobtext$TEXT, TRUE)), ex_inches(jobtext$TEXT, TRUE), ex_mm(jobtext$TEXT, TRUE)))
jobTextSizes$unit <- trimws(gsub("\\d|\\.", "", jobTextSizes$unit))
jobTextSizes <- unique(na.omit(jobTextSizes))

# ptwp1
ptwp1Sizes <- data.frame(
  size = ifelse(is.na(ex_mm(ptwp1$assetNum)), ex_inches(ptwp1$assetNum), ex_mm(ptwp1$assetNum)),
  unit = ifelse(is.na(ex_mm(ptwp1$assetNum, TRUE)), ex_inches(ptwp1$assetNum, TRUE), ex_mm(ptwp1$assetNum, TRUE)))
ptwp1Sizes$unit <- trimws(gsub("\\d|\\.", "", ptwp1Sizes$unit))
ptwp1Sizes <- unique(na.omit(ptwp1Sizes))



# rewrite the ex_pipeSize function to consider units
ps <- function(x) {
  x <- gsub('"', '^', x)
  x <- gsub("\\\\", "^", x)
  x <- stringr::str_replace_all(x,"[^[:graph:]]", " ")
  r <- ifelse(!is.na(ex_mm(x, units = TRUE)), ex_mm(x, units = TRUE), ex_inches(x, units = TRUE))
  ifelse(grepl("MM", r) == TRUE, as.double(stringr::str_extract(r, "\\d+")) / 25.4, as.double(stringr::str_extract(r, "\\d+")))
}

ps("lkjlkja 30mm lkasdklj")

if the result has mm then result numbers / 25.4 else result numbers


jobSizeCheck <- inner_join(jobPipeSize, jobtext, by = "operation")
jobSizeCheck$inches <- ex_inches(jobSizeCheck$text, units = FALSE)
jobSizeCheck$mm <- ex_mm(jobSizeCheck$text, units = FALSE)
jobSizeCheck <- jobSizeCheck %>% filter(!is.na(inches) | !is.na(mm))
jobSizeCheck$text <- cleanFreeText(jobSizeCheck$text)
jobSizeCheck$checkUnit <- ifelse(!is.na(jobSizeCheck$inches) & !is.na(jobSizeCheck$mm), "both",
                                 ifelse(!is.na(jobSizeCheck$inches), "inches", "mm"))
xlo(jobSizeCheck %>% count(jobPipeSize, checkUnit) %>% tidyr::spread(checkUnit, n))



write.table(jobSizeCheck, "F:/2/jobSizeCheck.txt", sep = "\t", row.names = F, col.names = T)

