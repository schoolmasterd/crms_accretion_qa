

#place name of current file to be QA'd in the the quotes on line 10
#place name of historical file in the the quotes on line 11

#set path to folder
path <- "path/to/crms_accretion_qa-main"
setwd(path)
in_path <- "Data/"
out_path <- "Output/"
input_file <- "CRMS_Acc_Sp23_CES_Jan.csv"
db_file <- "CRMS_Acc_Hist_LRO.csv"
input_df <- read.csv(paste0(in_path, input_file), check.names = F)
db_df <- read.csv(paste0(in_path, db_file), check.names = F)

#load some useful functions
library(xtable)

#Tools
tool_path <- "Tools/"
preamb <-
  paste(readLines(paste0(tool_path, "html_preamble.txt")), collapse = "\n")

#check for 3 stations per establishment date
#get site list
stations <- sapply(strsplit(input_df$`Station ID`, "-"), "[", 1)

#create a subset of the input_df with just what we need
df_temp <-
  data.frame(site = stations[!duplicated(input_df$`Station ID`)], input_df[!duplicated(input_df$`Station ID`),
                                                                           c("Station ID", "Establishment Date (mm/dd/yyyy)")],
             check.names = F)

#create table of number of stations in a site that have the same establishment date (new data only)
ans <- table(df_temp$site, df_temp$`Establishment Date (mm/dd/yyyy)`)
#make a list of all of those that are not equal to 3 (or 0)
bad_est_dat <- list()
k = 1
for (i in 1:9)
  for(j in 1:6) {
    if (ans[j, i] > 0 & ans[j, i] != 3) {
      bad_est_dat[[k]] <-
        data.frame(Site = rownames(ans)[j], Estab_date = colnames(ans)[i])
      k = k + 1
    }
  }
#create output
if (length(bad_est_dat) > 0) {
  tab_1 <-
    xtable(do.call("rbind", bad_est_dat), caption = "Stations per Establishmet Date not equal to 3")
} else {
  tab_1 <-
    xtable(data.frame(Site = NA, Estab_date = NA), caption = "Stations per Establishmet Date not equal to 3")
}

#check that each station only has 1 establishment date (new and old data)

#combine new data with corresponding station data from database
df_temp <-
  rbind(input_df[, c("Station ID", "Establishment Date (mm/dd/yyyy)")],
        db_df[which(db_df$`Station ID` %in% input_df$`Station ID`),
              c("Station ID", "Establishment Date (mm/dd/yyyy)")])

ans <-
  tapply(df_temp$`Establishment Date (mm/dd/yyyy)`, df_temp$`Station ID`, function(x)
    length(unique(x)))

too_many_est_dat <- which(ans > 1)
if (length(too_many_est_dat) > 0) {
  tab_2 <-
    xtable(
      data.frame(
        "Station ID" = names(too_many_est_dat),
        "Num Estab Dates" = as.integer(ans[too_many_est_dat]),
        check.names = F
      ),
      caption = "Stations with multiple establishment dates"
    )
} else {
  tab_2 <-
    xtable(data.frame(
      "Station ID" = NA,
      "Num Estab Dates" = NA,
      check.names = F
    ),
    caption = "Stations with multiple establishment dates")
}

#check that each coordinate has only been used once per_station

#combine new data with corresponding station data from database
df_temp <-
  rbind(input_df[, c("Station ID", "Sample Date (mm/dd/yyyy)", "Core X:Y")], db_df[which(db_df$`Station ID` %in%
                                                                                           input_df$`Station ID`), c("Station ID", "Sample Date (mm/dd/yyyy)", "Core X:Y")])
num_stat <- table(df_temp$`Station ID`)
num_cord <-
  tapply(df_temp$`Core X:Y`, df_temp$`Station ID`, function(x)
    length(unique(x)))
ans <- num_cord == num_stat
coord_used_more_than_once <- ans[which(!ans)]
dup_coords <- list()
if (length(coord_used_more_than_once) > 0)
  for(i in 1:length(coord_used_more_than_once)) {
    foo <-
      df_temp[which(df_temp$`Station ID` %in% names(coord_used_more_than_once)[i]), ]
    dup_list <- foo$`Core X:Y`[duplicated(foo$`Core X:Y`)]
    dup_coords[[i]] <- foo[foo$`Core X:Y` %in% dup_list, ]
  }

if (length(dup_coords) > 0) {
  tab_3 <-
    xtable(do.call("rbind", dup_coords), caption = "Duplicated coordinates")
} else {
  tab_3 <-
    xtable(
      data.frame(
        "Station ID" = NA,
        "Sample Date (mm/dd/yyyy)" = NA,
        "Core X:Y" = NA,
        check.rows = F
      ),
      caption = "Duplicated coordinates"
    )
}

#print out all accretion == 0

who <- grep("Measurement", names(input_df))
get_em <- which(apply(input_df[, who], 1, function(x)
  any(x == 0)))
tab_4 <-
  xtable(input_df[get_em, c(1, 8, 9:13)], caption = "Accretion is exactly zero")

out_file <- strsplit(input_file, "\\.")[[1]][1]
#write report
#create the html output
sink(paste0(out_path, out_file, ".html"))
cat(preamb)
cat(paste0('<h1>', out_file, '</h1>'), sep = "\n")
print(
  tab_1,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_2,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_3,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_4,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat('</body>
    </html>')
sink()
