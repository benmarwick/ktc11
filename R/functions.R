#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



####              ####
###  Data ingest   ###
####              ####


#' read_in_the_data
#
#
#
#' @importFrom readr read_csv
#' @export
#'
read_in_the_data <- function(){

 # assuming we are in the /analysis/paper directory...

 # read in the c14 data
 ktc11_radiocarbon_dates <- read.csv("../data/ktc11_radiocarbon_dates.csv")

 # read in the summary geoarch data
 ktc11_summary_geoarch_data <- read_csv("../data/ktc11_summary_geoarch_data.csv")

 # read in the carbon isotope data
 ktc11_d13C_data <- read_csv("../data/ktc11_d13C_data.csv")

 # read in the particle size data
 ktc11_particle_size_data <- read.csv("../data/ktc11_LPSA_data.txt", header=FALSE, stringsAsFactors = FALSE)

 # read in the xrd data
 ktc11_summary_xrd_data <- read_csv("../data/ktc11_summary_xrd_data.csv")

 # read in the icp data
 ktc11_raw_ICP_data <- read.csv("../data/ktc11_raw_ICP_data.csv", header=TRUE, stringsAsFactors = FALSE)

 # read in the ceramic data
 ktc11_ceramic_data <- read.csv("../data/ktc11_ceramic_data.csv")

 # read in the lithic data
 ktc11_lithic_data <- read.csv("../data/ktc11_lithic_data.csv")

 # read in faunal data
 ktc11_fauna_nonmollusc_data <- read.csv("../data/ktc11_summary_faunal_nonmollusc_data.csv")
 ktc11_mollusc_data <- read.csv("../data/ktc11_summary_mollusc_data.csv")

 ktc11_fauna_nonmollusc_MNI_data <- read.csv("../data/ktc11_summary_faunal_nonmollusc_data_MNI.csv")
 ktc11_mollusc_MNI_data <- read.csv("../data/ktc11_summary_mollusc_data_MNI.csv")
 ktc11_fauna_ntaxa_data <- read.csv("../data/ktc11_faunal_ntaxa_data.csv")


 # return a list of data frames
 return(list(ktc11_radiocarbon_dates = ktc11_radiocarbon_dates,
             ktc11_summary_geoarch_data = ktc11_summary_geoarch_data,
             ktc11_d13C_data = ktc11_d13C_data,
             ktc11_particle_size_data = ktc11_particle_size_data,
             ktc11_summary_xrd_data = ktc11_summary_xrd_data,
             ktc11_raw_ICP_data = ktc11_raw_ICP_data,
             ktc11_ceramic_data = ktc11_ceramic_data,
             ktc11_lithic_data = ktc11_lithic_data,
             ktc11_fauna_nonmollusc_data = ktc11_fauna_nonmollusc_data,
             ktc11_mollusc_data = ktc11_mollusc_data,
             ktc11_fauna_nonmollusc_MNI_data = ktc11_fauna_nonmollusc_MNI_data,
             ktc11_mollusc_MNI_data = ktc11_mollusc_MNI_data,
             ktc11_fauna_ntaxa_data = ktc11_fauna_ntaxa_data))
}




####              ####
###   Chronology   ###
####              ####


# calibrate_the_dates
#
# This uses the Bchron package to calibrate
# the radiocarbon dates
#

#' @importFrom Bchron BchronCalibrate
#' @importFrom plyr ldply
#' @export

calibrate_the_dates <- function(dates) {


  dates_ <-  dates[-2,] # something wrong with the second date
  # Bchron summary doesn't work well when range is >0 BP
  set.seed(007)
  ages <- BchronCalibrate(ages = dates_$RCAge,
                          ageSds = dates_$RCAge_1s_error,
                          positions = dates_$depth_below_surface,
                          calCurves = rep("intcal13", length(dates_$RCAge)))

  # First create age samples for each date
  age_samples = lapply(ages, function(x) sample(x$ageGrid,size=2000,replace=TRUE,prob=x$densities))
  # Now summaries them with quantiles - this gives a 95% credible interval
  df <- ldply(age_samples,quantile,prob=c(0.025,0.975))


  # for our table we want
  dates_table <- dates[, c("DAMS_Sample_code",  "RCAge", "RCAge_1s_error",  "Material", "spit", "context", "depth_below_surface")]
  # add on calibrated ranges
  # date 2 had a problem so let's do it by hand (from OxCal website)
  df_ <- rbind( df[1,], c(291, 0), df[ 2:nrow(df),] )
  dates_table <- cbind(dates_table, df_[,-1])

  dates_table$`2.5%` <- round(dates_table$`2.5%`, 0)
  dates_table$`97.5%`  <- round(dates_table$`97.5%`, 0)

  row.names(dates_table) <- NULL

  # change column names
  names(dates_table) <- c("Sample code", "Age in years BP", "1 sd error", "Material dated", "Excavation unit", "Context", "Depth below surface (m)", "Calibrated upper 95%", "Calibrated lower 95%")

  # add a midpoint of calibrated dates for plotting
  dates_table$midpoint <- with(dates_table, (`Calibrated upper 95%` - `Calibrated lower 95%`)/2 + `Calibrated lower 95%`)

  # use basic linear regression on ages
  charcoal_midpoints <- dates_table[dates_table$`Material dated` == 'charcoal', ]

  summary_lm <- summary(lm(charcoal_midpoints$midpoint ~ charcoal_midpoints$`Depth below surface (m)`))
  #intercept
  intcp <- coef(summary_lm)[1]
  slp <-  coef(summary_lm)[2]
  # function to interpolate age from charcoal dates
  age <- function(d){slp * d + intcp }

  return(list(dates_table = dates_table,
              summary_lm = summary_lm,
              intcp = intcp,
              slp = slp,
              age = age))
}




# mean_difference_offset
#
# This computes an approximate mean difference in
# the offset of the charcoal and shell dates
#
#' @importFrom Bchron Bchronology
#' @importFrom dplyr arrange filter %>% inner_join
#' @export

mean_difference_offset <- function(dates) {
  #
  # we have to put in order to let the calibration work...
  dates <- dates %>%
    arrange(depth_below_surface)

  # separate shell and charcoal dates to do separate interpolations
  shell    <- dates %>% filter(Material == 'shell')
  charcoal <- dates %>% filter(Material == 'charcoal')

  # interpolate calibrated ages... rather slow
  calibrated_dates_shell = Bchronology(ages = shell$RCAge,
                                 ageSds = shell$RCAge_1s_error,
                                 positions = shell$depth_below_surface,
                                 positionThicknesses = 0.1,
                                 ids = shell$depth_below_surface,
                                 calCurves = rep("intcal13", length(shell$RCAge)))

  # interpolate calibrated ages... still slow
  calibrated_dates_charcoal = Bchronology(ages = charcoal$RCAge,
                                       ageSds = charcoal$RCAge_1s_error,
                                       positions = charcoal$depth_below_surface,
                                       positionThicknesses = 0.1,
                                       ids = charcoal$depth_below_surface,
                                       calCurves = rep("intcal13", length(charcoal$RCAge)))

  # get the first and last shell dates, since they have a shorter range than the
  # charcoal dates
  shell_first_last <- summary(calibrated_dates_shell)[c(1, nrow(summary(calibrated_dates_shell))),]

  # get the charcoal ages from the same depths
  calibrated_dates_charcoal_df <- summary(calibrated_dates_charcoal)
  calibrated_dates_charcoal_df$Depth_rounded <- round(calibrated_dates_charcoal_df$Depth, 2)

  # join so we have interpolated dates for charcoal to
  # match the first and last shell date
  shell_charcoal_offset <- inner_join(shell_first_last, calibrated_dates_charcoal_df,
             by = c('Depth' = 'Depth_rounded'))

  # compute the difference
  mean_difference <- mean(shell_charcoal_offset$`50%.x` - shell_charcoal_offset$`50%.y`)

  # get a table of interpolated ages from the charcoal dates that we can use later
  charcoal_dates_interp <- summary(calibrated_dates_charcoal)
  # compute a mid-point of the 97.5% range
  charcoal_dates_interp$midpoint <- charcoal_dates_interp$`2.5%` + ((charcoal_dates_interp$`97.5%` - charcoal_dates_interp$`2.5%`) / 2 )



return(list(mean_difference = mean_difference,
            charcoal_dates_interp = charcoal_dates_interp))

}

####              ####
###   Geoarch      ###
####              ####

# join carbon isotope data to summary geoarch data
#
#
#
#' @importFrom dplyr group_by summarise %>%
#' @export

join_carbon_to_summary <- function(ktc11_d13C_data, ktc11_summary_geoarch_data){

  # compute means of measurements
  ktc11_d13C_data_means <- ktc11_d13C_data %>%
    group_by(SampleID) %>%
    summarise(mean(`d13C vs VPDB (permil)`))

  # row order must be B1, A2, A3, A4, A5, A6, A7H, 8, A7L
  ktc11_d13C_data_means <- ktc11_d13C_data_means[c(9,1,2,3,4,5,6,8,7),]

  # join to summary geoarch data
  ktc11_summary_geoarch_data_dC13 <- cbind(ktc11_summary_geoarch_data, ktc11_d13C_data_means[,-1])

  return(ktc11_summary_geoarch_data_dC13)



}

# particle_size_data_summary
#
#
#
#' @importFrom stringr str_extract
#' @importFrom reshape2 dcast
#' @importFrom G2Sd granstat
#' @importFrom dplyr select %>% mutate_each funs
#' @export

particle_size_data_summary <- function(ktc11_particle_size_data){

  # This object "ktc11_particle_size_data" has the raw output from the instrument
  # which means it has a lot of stuff we don't need. Let's
  # delete the first row and first 23 columns to get only sample
  # names, size classes and sample data. We're also removing
  # column 25 and the very last column because they are empty
  KTC_LPSA <- ktc11_particle_size_data[-1,-c(1:22, 25, ncol(ktc11_particle_size_data))]
  #
  # Fill first column with numbers and give it a name, this
  # will be useful later
  #
  KTC_LPSA[,1] <- c(1:nrow(KTC_LPSA))
  names(KTC_LPSA)[1]<-c("num")
  #
  # Convert a few errant characters to numbers. This is a bug
  # of the import process where some numbers are not
  # formatted as numbers.
  KTC_LPSA[,c(3:ncol(KTC_LPSA))] <- as.numeric(as.character(unlist(KTC_LPSA[,c(3:ncol(KTC_LPSA))])))
  #
  # Reshape to long form to prepare for statistical analysis,
  # this may take a few seconds...
  KTC_l <- reshape(KTC_LPSA, idvar=1:2,
                   varying=list(size=colnames(KTC_LPSA[seq(from=3,
                                                           to=ncol(KTC_LPSA), by=3)]),
                                meas=colnames(KTC_LPSA[seq(from=4,
                                                           to=ncol(KTC_LPSA), by=3)])),
                   direction="long")
  #
  # strip off the numbers indicating replicates of the same sample
  # so we can use the sample ID as the group label for doing
  # stats by groups
  KTC_l$V24 <-  str_extract(KTC_l$V24, ".+[^[:punct:]{1}[:digit:]{1}[:punct:]{1}][^rs]")
  #
  #
  # get averages of multiple runs on the same sample
  KTC_l_a <- aggregate(KTC_l$V27, list(sample=KTC_l$V24, size=KTC_l$V26), mean)
  KTC_l_a <- subset(KTC_l_a, KTC_l_a$x != "NA")
  #
  # Rename measurement variable for consistency, ready for plots
  # and tables
  KTC_l_a$q <- KTC_l_a$x
  #
  # Put the sample names in order, first inspect the levels
  # levels(as.factor(KTC_l_a$sample))
  #
  # now reorder them, I've already done the hard work for you
  # here, just click through...
  KTC_l_a$sample <- factor(as.factor(KTC_l_a$sample),
                           levels(as.factor(KTC_l_a$sample))
                           [c(9,1,2,3,4,5,7,8,6)])
  #
  # Check that it worked ok, the sample names should be in
  # order now
  # levels(KTC_l_a$sample)
  #
  # Now we are ready to visualise the LPSA data. We'll look at
  # how to do all samples together, then just one by itself.
  #
  # Make a plot to have a quick look, all samples overlaid
  # library(ggplot2)
  # ggplot(KTC_l_a, aes(group = sample))+ geom_line(aes(x=size, y=q, colour = sample))
  #
  # And for a different perspective, plot each sample
  # separately, one above the other
  # ggplot(KTC_l_a, aes(size, q)) + geom_line() + facet_grid(sample ~ .)
  #
  # Plot an individual sample, go ahead and change the
  # sample name to get different samples
  # ggplot(subset(KTC_l_a, KTC_l_a == "KTC_A6"),
  #       aes(size, q)) + geom_line()
  #
  # Now we can do some basic statistical analysis of the LPSA
  # data. We will use a package dedicated to granulometry to
  # calculate typical summary statistics of particle sizes.
  #
  # First we need to get the data in the shape that the grain
  # size stats package is expecting. Let's cast into a wide
  # table of sample by size class.
  #
  #
  KTC_l_a <- KTC_l_a[ order(-KTC_l_a$size), ]
  KTC_cast <- t(dcast(KTC_l_a, sample ~ size, value.var="q"))
  colnames(KTC_cast) <- KTC_cast[1,]
  KTC_cast <- KTC_cast[-1,]
  #
  # We need to add an empty row at the bottom to
  # make the stats work
  z <- c(rep(0, ncol(KTC_cast)))
  KTC_cast <- data.frame(rbind(KTC_cast,z))
  rownames(KTC_cast)[nrow(KTC_cast)] <- c("0")
  KTC_cast[,1:ncol(KTC_cast)] <- as.numeric(as.character(unlist(KTC_cast[,1:ncol(KTC_cast)])))
  #
  #
  # This next line will make a table of commonly used
  # grain size stats. Note that when modes is set to TRUE,
  # then you have to use mouse to click on the modal
  # point for each plot, then press escape to move on to
  # the next sample's plot. Currently I've set the function
  # to modes=FALSE so you don't have to worry about that.
  # After the last sample all the data will be generated.
  # Definitions of the  terms used can be found here
  # http://cran.r-project.org/web/packages/G2Sd/G2Sd.pdf or by
  # typing ?granstat at the R prompt
  KTC_granstat <- as.data.frame(t(granstat(KTC_cast, statistic = "arithmetic", aggr = TRUE, modes = FALSE)))

  # Let's get just the mean and standard deviation from the
  # particle size data ready to attach it to the table of
  # basic sediment data to make a stratigraphic plot

  particle_size_data_summary_subset <- KTC_granstat %>%
    # subset only mean and sd cols
    dplyr::select(mean.arith.um, sd.arith.um) %>%
    # convert from factor to numeric
    mutate_each(dplyr::funs(as.character), mean.arith.um, sd.arith.um) %>%
    mutate_each(dplyr::funs(as.numeric), mean.arith.um, sd.arith.um)
  particle_size_data_summary_subset$`Sample ID`  <- row.names(particle_size_data_summary)

  return(list(KTC_granstat = KTC_granstat,
              particle_size_data_summary_subset = particle_size_data_summary_subset))

}



#' icp_aes_data_summary
#'
#'
#'
#' @importFrom tidyr spread
#' @export
#'
icp_aes_data_summary <- function(ktc11_raw_ICP_data){

  # ktc11_raw_ICP_data  <- the_data$ktc11_raw_ICP_data

  KTC_ICP <- ktc11_raw_ICP_data
  #
  # This object "KTC_ICP" that we just made has data for both KTC
  # and BN1. Let's split it up so you can choose which site
  # you want to do the rest of the analysis on. This line
  # returns only those rows that have "KTC" in the Sample.ID
  # We'll come back to the replicates and blanks later
  #


  KTConly <- KTC_ICP[grepl("KTC", KTC_ICP$Sample.ID) &
                       !grepl("BLANK|Blank", KTC_ICP$Sample.ID) &
                       !grepl("replica", KTC_ICP$Sample.ID), ]

  # KTConly <- KTC_ICP %>%
  #             filter(grepl("KTC", Sample.ID)) %>%
  #             filter(!grepl("BLANK|Blank", Sample.ID)) %>%
  #             filter(!grepl("replica", Sample.ID))

  # KTConly <- subset(KTC_ICP, (grepl("KTC", KTC_ICP$Sample.ID)))
  # # but still has blanks
  # KTConly <- subset(KTConly, !(grepl("BLANK|Blank", KTConly$Sample.ID)))
  # # no blanks now
  # KTConly <- subset(KTConly, !(grepl("replica", KTConly$Sample.ID)))
  # # no replicates
  #
  #
  # And let's put all the blanks in one data object
  # to look at together
  blanks <- KTC_ICP[grepl("BLANK|Blank", KTC_ICP$Sample.ID), ]

  replicates <- KTC_ICP[grepl("replica", KTC_ICP$Sample.ID), ]

  # blanks <- subset(KTC_ICP, grepl("BLANK|Blank", KTC_ICP$Sample.ID))
  # # and the replicates, ready for later
  # replicates <- subset(KTC_ICP, (grepl("replica", KTC_ICP$Sample.ID)))

  #
  #
  # convert the mg/L values, first make change them ppm to
  # ppb, then make sure they're all +ve, then take the
  # base 10 log, and put that new value into a new column
  KTConly$LogConc <- log10(abs(KTConly$Conc..Samp.1*1000))
  #
  # # plot all the samples on top of each other, may take 5 seconds...
  # # a fairly pointless plot, hard to see differences...
  # ggplot(data=KTConly, aes(group = Sample.ID)) +
  #   geom_line(aes(x=Elem, y=LogConc, colour = Sample.ID))
  #
  # get ready to plot each sample separately by making a small table of just
  # sample ID, element and log concentration, and then exlude rows with NA
  KTCconcs <- as.data.frame(na.omit(cbind(KTConly$Sample.ID,
                                          KTConly$Elem, KTConly$LogConc)))


  # KTCconcs <- as.data.frame(na.omit(cbind(as.matrix(KTConly$Sample.ID),
  #                                         as.matrix(KTConly$Elem), KTConly$LogConc)))
  # give column names back again
  colnames(KTCconcs) <- c("Sample.ID", "Element", "log_ugL")
  #
  # convert mg.L from factor to numeric, ready to operate on
  KTCconcs$log_ugL <- abs(as.numeric(as.character(KTCconcs$log_ugL)))
  #
  # convert "NA" to zeros, "NA" comes from elements that were so high
  # in concentration that they saturated the detector and we have no
  # data on them at all. We can re-run the sample with different
  # settings to get data if necessary.
  KTCconcs$log_ugL <- as.numeric(ifelse(KTCconcs$log_ugL=="NA",
                                        "0", KTCconcs$log_ugL))
  #
  # convert "Inf" to zero. These come from the log of a
  # negative number. The negative numbers result from
  # spectral interference during calibration
  KTCconcs$log_ugL <- as.numeric(ifelse(KTCconcs$log_ugL=="Inf",
                                        "0", KTCconcs$log_ugL))
  #
  # re-order the sample IDs to get them in stratigraphic order
  # so they plot in that order...
  # first, find out what order they are in
  # levels(KTCconcs$Sample.ID)
  # second, reorder them, I have already determined the correct
  # order here, so it should just work when you click though
  KTCconcs$Sample.ID <- factor(KTCconcs$Sample.ID,
                               levels(KTCconcs$Sample.ID)[c(9,2,3,1,4,5,6,8,7)])
  # check that you reordered them correctly
  # levels(KTCconcs$Sample.ID)
  #
  # Now we are going to go through a few visualisation methods
  # to try and get a look at the data and see if we can spot
  # trends and interesting patterns.
  #
  # First, plot all of the samples in a giant grid of plots...
  # this may be very slow! Could be a couple of mins...
  # ggplot(KTCconcs, aes(Element, log_ugL)) + geom_point() +
  #   facet_wrap( ~ Sample.ID)
  # #
  # # Second, let's try a single column of plots
  # ggplot(KTCconcs, aes(Element, log_ugL)) + geom_point() +
  #   facet_grid(Sample.ID ~ .)
  #
  # Third, let's rotate the plot to make a stratigraphic plot
  # with the samples in order from top to bottom
  #
  # We need to convert data to wide format for this plot
  # KTCconcs$row <- 1:nrow(KTCconcs)
  # KTCconcs$row <- NULL
  KTCcast <-  spread(KTCconcs, Element, log_ugL)


  # KTCcast_dcast <- dcast(KTCconcs, as.character(Sample.ID) ~ as.character(Element), value.var="log_ugL")
  #
  # get rid of sample IDs by putting them into rownames
  row.names(KTCcast) <- KTCcast$Sample.ID
  #
  # delete sample ID column
  KTCcast <- KTCcast[,-1]
  #
  # delete columns with zeros, these elements with problematic data
  # resulting from calibration problems or saturation of the
  # detector
  KTCcast <- KTCcast[,!(colSums(abs(KTCcast)) == 0)]
  #
  # load the packages we need to make the stratigraphic plot
  # library(analogue)
  #
  # make a data object that contains that sample depth
  # values in m below the surface
  depths <- c(0.02, 0.09, 0.22, 0.30, 0.40, 0.53, 0.72, 0.92, 1.10)
  #
  KTCcast.strat <- KTCcast
  #
  # And now draw the plot...
  # Stratiplot(KTCcast.strat, y = depths,
  #            type = c("h", "o"), ylab = "depth below surface (m)",
  #            varTypes = "absolute")
  # #
  # This is a bit of a silly plot, there's too much going on.
  # But you might be able to spot some trends that you want to
  # make a note of to explore futher by plotting just a subset
  # of all the elements.
  #
  # To plot just a subset of certain elements, try this.
  # you can add as many elements as you like by
  # inserting more 'which(colnames(KTCcast.strat)=="Na")'
  # and just changing the element name. Don't forget the
  # comma after the close-bracket! You should go ahead and
  # replace the element names here with ones that you've found
  # to have interesting patterns.
  # Stratiplot(KTCcast.strat[,c(which(colnames(KTCcast.strat)=="Sc"), # delete or copy
  #                             which(colnames(KTCcast.strat)=="V"), # these lines to
  #                             which(colnames(KTCcast.strat)=="Li"),  # customise as
  #                             which(colnames(KTCcast.strat)=="Cr"),  # customise as
  #                             which(colnames(KTCcast.strat)=="Ni"),  # customise as
  #                             which(colnames(KTCcast.strat)=="Na")   # you like
  # ) ],
  # y = depths, type = c("h", "o"), ylab = "depth below surface (m)",
  # varTypes = "absolute")
  # #

# get table of ppm of many elements (cols) for each context (rows)

  # subset elements of interest
  subset_elements <- c("Ca", "Sr", "Mn", "Fe", "Zn",  "Na", "K", "Mg", "Ti")
  KTCcast.strat.subset <- KTCcast.strat[ , names(KTCcast.strat) %in% subset_elements]

  row.names(KTCcast.strat.subset) <-
              c("B1", "A2", "A3", "A4", "A5", "A6", "A7U", "A8", "A7L")

return(KTCcast.strat.subset)

}

####                  ####
###   Archaeology      ###
####                  ####


#' table_ceramics_lithics
#'
#' @importFrom dplyr group_by %>% full_join
#' @export
#'
table_ceramics_lithics <- function(lithics, ceramics){

  # lithics <- the_data$ktc11_lithic_data
  # ceramics <- the_data$ktc11_ceramic_data


# aggregate counts and masses per context

  li <- lithics %>%
    group_by(Context) %>%
    dplyr::summarise(lithic_counts = sum(Counts),
                     lithic_mass = sum(Mass))

  ce <- ceramics %>%
    group_by(Context) %>%
    dplyr::summarise(ceramic_counts = sum(Counts),
                     ceramic_mass = sum(Mass))

   ktc11_summary_ceramics_lithics <-  full_join(li, ce, by  = "Context")

   # meaningful column names
   names(ktc11_summary_ceramics_lithics) <- c('Context', "Lithic count (n)", "Lithic mass (g)", "Ceramic count (n)", "Ceramic mass (g)")

   # reorder so that it's in the correct order
   ktc11_summary_ceramics_lithics_reordered <- ktc11_summary_ceramics_lithics[c(1,8,9,2,3,4,6,7,5), ]

   return(ktc11_summary_ceramics_lithics_reordered)


}

#' plot_ceramic_and_stone_artefact_mass
#'
#'
#'
#' @importFrom dplyr group_by %>% summarise full_join left_join select
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot
#' @export
#'
#'
plot_ceramic_and_stone_artefact_mass <- function(the_data, calibrated_dates){

  # summarise by excavation unit
  li <- the_data$ktc11_lithic_data
  ce <- the_data$ktc11_ceramic_data

  li_mass <- li %>%
    group_by(EU) %>%
    dplyr::summarise(lithic_counts = sum(Counts),
                     lithic_mass = sum(Mass))

  ce_mass <- ce %>%
    group_by(EU) %>%
    dplyr::summarise(ceramic_counts = sum(Counts),
                     ceramic_mass = sum(Mass))

  ktc11_summary_ceramics_lithics_units <-  full_join(li_mass, ce_mass, by  = "EU")

  depths <- read.table(header = TRUE, text = "EU  depth
                     1	0.0
                       2	0.1
                       3	0.15
                       4	0.2
                       5	0.3
                       6	0.4
                       7	0.5
                       8	0.6
                       9	0.65
                       10	0.7
                       11	0.75
                       12	0.8
                       13	0.85
                       14	0.9
                       15	0.95
                       16	1
                       17	1.05
                       18	1.1
                       19	1.15
                       20	1.25
                       ")



  ktc11_summary_ceramics_lithics_units_depths <- full_join(depths, ktc11_summary_ceramics_lithics_units, by = "EU")

  slp <-  calibrated_dates$slp
  intcp  <- calibrated_dates$intcp
  d <- ktc11_summary_ceramics_lithics_units_depths$depth
  ktc11_summary_ceramics_lithics_units_depths$age <- calibrated_dates$age(d)

  # stop ceramics over 10k BP from showing as we believe these are not in situ
  ktc11_summary_ceramics_lithics_units_depths$ceramic_mass <- ifelse(ktc11_summary_ceramics_lithics_units_depths$age > 10000, (ktc11_summary_ceramics_lithics_units_depths$ceramic_mass == NA), ktc11_summary_ceramics_lithics_units_depths$ceramic_mass)



  plot_artefact_over_time <- ktc11_summary_ceramics_lithics_units_depths %>%
    select(age, lithic_mass, ceramic_mass) %>%
  gather(artefact, mass, -age)

  # rename to make better plot labels
  plot_artefact_over_time$artefact <- ifelse(plot_artefact_over_time$artefact == "lithic_mass", "Stone artefacts", "Ceramics")

  ggplot(plot_artefact_over_time, aes(age, mass)) +
    geom_bar(stat = "identity") +
    xlab("years cal. BP") +
    ylab("mass (g)") +
    theme_bw(base_size = 14) +
    facet_grid(artefact ~ ., scales = "free_y")
}


#' table_fauna_and_molluscs
#'
#'
#' @export
#'
table_fauna_and_molluscs <- function(the_data){

  ntaxa <- the_data$ktc11_fauna_ntaxa_data
  bones <- the_data$ktc11_fauna_nonmollusc_data
  shells <- the_data$ktc11_mollusc_data

  bones_MNI <- the_data$ktc11_fauna_nonmollusc_MNI_data
  shells_MNI <- the_data$ktc11_mollusc_MNI_data

  names(bones)[1] <- "Taxon"
  names(shells)[1] <- "Taxon"

  # Bones, NISP with MNI in parentheses
  # clean up a bit, NISP first
  bones_num <-  data.frame(sapply(bones[,2:ncol(bones)], function(i) as.numeric(as.character(i))))
  # change NA to zero
  bones_num[is.na(bones_num)] <- 0
  bones_rowsums <- c(rowSums(bones_num))
  bones_colsums <- colSums(bones_num)
  # get taxon names
  bones_num$taxon <- bones[,1]
  bones_num <- bones_num[, c(ncol(bones_num), 1:ncol(bones_num)-1) ]
  names(bones_num) <- c("Taxon", "1", "2", "3", "4", "5", "6", "7U", "8", "7L")

  # now MNI
  bones_MNI_num <-  data.frame(sapply(bones_MNI[,2:ncol(bones_MNI)], function(i) as.numeric(as.character(i))))
  # change NA to zero
  bones_MNI_num[is.na(bones_MNI_num)] <- 0
  bones_MNI_rowsums <- rowSums(bones_MNI_num)
  bones_MNI_colsums <- colSums(bones_MNI_num)
  # get taxon names
  bones_MNI_num$taxon <- bones_MNI[,1]
  # reorder
  bones_MNI_num <- bones_MNI_num[, c( (ncol(bones_MNI_num)), 1:(ncol(bones_MNI_num)-1) ) ]
  names(bones_MNI_num) <- c("Taxon", "1", "2", "3", "4", "5", "6", "7U", "8", "7L")

  # now combine into compact table
  bones_NISP_MNI <- as.data.frame(do.call(
                                      cbind,
                                        lapply(2:ncol(bones_num), function(i) paste0(bones_num[ , i],
                                                                                     " (",
                                                                                     bones_MNI_num[ , i],
                                                                                     ")"  ) )))
  # put taxa back on
  bones_NISP_MNI <- cbind(bones_num$Taxon, bones_NISP_MNI)
  names(bones_NISP_MNI) <- names(bones_num)

  # add on row and col totals
  col_totals <- sapply(1:length(bones_colsums), function(i) paste0(bones_colsums[i], " (", bones_MNI_colsums[i], ")"))
  row_totals <- sapply(1:length(bones_rowsums), function(i) paste0(bones_rowsums[i], " (", bones_MNI_rowsums[i], ")"))
  # need to have characters to rbind
  bones_NISP_MNI <- sapply(bones_NISP_MNI, as.character)
  bones_NISP_MNI_total <- cbind(bones_NISP_MNI, Total = row_totals)
  bones_NISP_MNI_total <- rbind(bones_NISP_MNI_total, c("Total", col_totals, ""))
  # fill in that last bottom corner cell
  bones_NISP_MNI_total[nrow(bones_NISP_MNI_total), ncol(bones_NISP_MNI_total)] <-
    paste0(sum(bones_num[,-1]), " (", sum(bones_MNI_num[,-1]), ")")

############
  # Shells, NISP with MNI in parentheses
  # clean up a bit, NISP first
  shells_num <-  data.frame(sapply(shells[,2:ncol(shells)], function(i) as.numeric(as.character(i))))
  # change NA to zero
  shells_num[is.na(shells_num)] <- 0
  shells_rowsums <- rowSums(shells_num)
  shells_colsums <- colSums(shells_num)
  # get taxon names
  shells_num$taxon <- shells[,1]
  shells_num <- shells_num[, c(ncol(shells_num), 1:ncol(shells_num)-1) ]
  names(shells_num) <- c("Taxon", "1", "2", "3", "4", "5", "6", "7U", "8", "7L")

  # now MNI
  shells_MNI_num <-  data.frame(sapply(shells_MNI[,2:ncol(shells_MNI)], function(i) as.numeric(as.character(i))))
  # change NA to zero
  shells_MNI_num[is.na(shells_MNI_num)] <- 0
  shells_MNI_rowsums <- rowSums(shells_MNI_num)
  shells_MNI_colsums <- colSums(shells_MNI_num)
  # get taxon names
  shells_MNI_num$taxon <- shells_MNI[,1]
  # reorder
  shells_MNI_num <- shells_MNI_num[, c( (ncol(shells_MNI_num)), 1:(ncol(shells_MNI_num)-2))   ]
  names(shells_MNI_num) <- c("Taxon", "1", "2", "3", "4", "5", "6", "7U", "8", "7L")

  # now combine into compact table
  # they don't have the same taxa in both tables
  shells_num <- shells_num[order(shells_num$Taxon),]
  shells_MNI_num <- shells_MNI_num[order(shells_MNI_num$Taxon),]

  shells_NISP_MNI <- as.data.frame(do.call(cbind, lapply(2:ncol(shells_num), function(i) paste0(shells_num[ , i], " (", shells_MNI_num[ , i], ")"  ) )))
  # put taxa back on
  shells_NISP_MNI <- cbind(shells_num$Taxon, shells_NISP_MNI)
  names(shells_NISP_MNI) <- names(shells_num)

  # add on row and col totals
  col_totals <- sapply(1:length(shells_colsums), function(i) paste0(shells_colsums[i], " (", shells_MNI_colsums[i], ")"))
  row_totals <- sapply(1:length(shells_rowsums), function(i) paste0(shells_rowsums[i], " (", shells_MNI_rowsums[i], ")"))
  # need to have characters to rbind
  shells_NISP_MNI <- sapply(shells_NISP_MNI, as.character)
  shells_NISP_MNI_total <- cbind(shells_NISP_MNI, Total = row_totals)
  shells_NISP_MNI_total <- rbind(shells_NISP_MNI_total, c("Total", col_totals, ""))
  # fill in that last bottom corner cell
  shells_NISP_MNI_total[nrow(shells_NISP_MNI_total), ncol(shells_NISP_MNI_total)] <- paste0(sum(shells_num[,-1]), " (", sum(shells_MNI_num[,-1]), ")")




  return(list(bones_NISP_MNI_total = bones_NISP_MNI_total,
              shells_NISP_MNI_total = shells_NISP_MNI_total,
              bones_num = bones_num,
              bones_MNI_num = bones_MNI_num,
              shells_num = shells_num,
              shells_MNI_num = shells_MNI_num,
              ntaxa = ntaxa))
}

#' mni_log_nisp
#'
#' @export
mni_log_nisp <- function(fauna_and_molluscs_table){

   mni_nisp <- data.frame(
   nisp = colSums(fauna_and_molluscs_table$bones_num[, -1]) +
          colSums(fauna_and_molluscs_table$shells_num[, -1]),
   mni = colSums(fauna_and_molluscs_table$bones_MNI_num[, -1]) +
         colSums(fauna_and_molluscs_table$shells_MNI_num[, -1])[1:9]
  )
    # can't take a log of zero...
    mni_nisp[mni_nisp == 0] <- 1

  summary_lm <- summary(lm(log(nisp, 10) ~ mni, data = mni_nisp))
  corr_output <- with(mni_nisp, cor.test(log(nisp, 10), mni))


  return(list(summary_lm = summary_lm,
              corr_output = corr_output,
              mni_nisp = mni_nisp))

}

#' ecological_indices
#'
#' @importFrom vegan diversity specnumber
#' @export
#'
ecological_indices <- function(fauna_and_molluscs_table){

  # combine bones and shell
  fauna <- rbind(fauna_and_molluscs_table$bones_MNI_num, fauna_and_molluscs_table$shells_MNI_num)

  mni <- t(fauna[, -1])
  # simpson's diversity per context
  simp <- diversity(mni, "simpson")
  H <- diversity(mni)

  ## Species richness (S) and Pielou's evenness (J):
  S <- specnumber(mni) ## rowSums(BCI > 0) does the same...
  J <- H/log(S)
  J[is.nan(J)] <- 0

  ecological_indices <- data.frame(Context = row.names(mni),
             NTAXA = S,
             Simpson = round(simp, 3),
             #`Shannon-Weaver` = round(H, 3),
             Pielou = round(J, 3) ,
             row.names = NULL,
             check.names = FALSE)

  return(ecological_indices)

}


#'  long_corr_matrix
#
#
# from http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
#
#' @importFrom Hmisc rcorr
#' @export
#'
long_corr_matrix <- function(df) {
  res <- rcorr(as.matrix(df))
  cormat <- res$r
  pmat <- res$P
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut],
    p = pmat[ut]
  )
}

#' my_trunc
#'
#' from http://stackoverflow.com/a/23158178/1036500
#'
#' @export
my_trunc <- function(x, ..., prec = 0) {
  base::trunc(x * 10^prec, ...) / 10^prec
}




#' my_maps
#'
#' @return saves a PNG of the map

#'
#' @importFrom ggmap    ggmap    get_googlemap
#' @import legendMap
#' @import maptools
#' @importFrom gridExtra    grid.arrange
#' @importFrom grDevices    dev.off    png
#' @importFrom stats    aggregate    coef    lm    na.omit    quantile    reshape
#' @importFrom utils    read.csv    read.table
#'
#'@export
my_maps <- function(){
# from https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
# library(ggmap)
# library(legendMap) # for scale and N arrow
require(maptools) # to fix knit error
require(ggplot2) # to fix knit error


myLocation <-  c(lon = 98.883060, lat = 8.167072)
myMap <- get_googlemap(center = myLocation,
                       zoom = 11,
                       scale = 2,
                       maptype = "terrain"
                       )

close_up <-
  ggmap(myMap) +
   geom_point(data = data.frame(t(myLocation)),
              aes(lon,
                  lat),
              size = 10,
              colour = "white") +
  geom_point(data = data.frame(t(myLocation)),
             aes(lon,
                 lat),
             size = 6,
             colour = "black")


close_up <- close_up +
  legendMap::scale_bar(lon = 98.68,
            lat = 7.975,
            distance_lon = 5,
            distance_lat = 1,
            distance_legend = 2,
            dist_unit = "km",
            orientation = TRUE,
            arrow_length = 5,
            arrow_distance = 7,
            arrow_north_size = 5)


# ggsave("close_up.png")

myMap <- get_googlemap(center = myLocation,
                       zoom = 5,
                       scale = 2,
                       maptype = "roadmap"
                       )

region <-
  ggmap(myMap) +
  geom_point(data = data.frame(t(myLocation)),
             aes(lon,
                 lat),
             size = 10,
             colour = "white") +
  geom_point(data = data.frame(t(myLocation)),
             aes(lon,
                 lat),
             size = 6,
             colour = "black")


region <- region +
  legendMap::scale_bar(lon = 87,
            lat = -4,
            distance_lon = 500,
            distance_lat = 50,
            distance_legend = 100,
            dist_unit = "km",
            orientation = TRUE,
            arrow_length = 200,
            arrow_distance = 250,
            arrow_north_size = 5)

# ggsave("region.png")


# library(gridExtra)
png("../figures/location_map.png", height = 600, width = 1200)
grid.arrange(region,
             close_up,
             ncol = 2)
dev.off()

# g <- arrangeGrob(region,
#                  close_up,
#                  nrow = 1) #generates g
# ggsave(file="location_map.png", g) #saves g
}
