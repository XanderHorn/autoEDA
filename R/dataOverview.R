#' Automated tabular exploratory data analysis
#'
#' Performs automated tabular exploratory data analysis. Summary statistics per feature is also calculated along with common data issues which will be flagged. Imputation values are also calculated per feature.
#' @param x [data.frame | Required]   Dataset which should contain all relevant features. If x is not a data.frame object it will be converted to one.
#' @param outlierMethod [character | Optional] Determines how outliers are identified. Two possible methods are available, tukey and percentile.   When specifying percentile based outlier detection, it is recommended to manually set the lower and upper percentile values for detection. Defaults to tukey.
#' @param lowPercentile [numeric | Optional] The lower percentile value that will be used to flag any values less than the calculated percentile as lower outliers. Recommended to set values between 0.01 and 0.05. Defaults to 0.01.
#' @param upPercentile [numeric | Optional] The upper percentile value that will be used to flag any values greater than the calculated percentile as upper outliers. Recommended to set values between 0.95 and 0.99. Defaults to 0.99.
#' @param minLevelPercentage [numeric | Optional] The minimum percentage data representation per level required for a categorical feature. Categorical features should ideally exhibit levels which contains adequate data proportions and levels with low proportions should require data cleaning. If a categorical feature has levels lower than the specified percentage, these levels will be used to determine the imputation value used. If the cumulative sum of the minimum levels are less than the specified minimum level, the imputation value is simply the mode of the feature, else all minimum levels are combined into a new level called ALL_OTHER. Defaults to 0.025.
#' @return Object of type data.frame containing exploratory information of all features passed on in x.
#' @export
#' @examples
#' # Tukey outlier detection example:
#' overview <-  dataOverview(x = iris,
#'                           outlierMethod = "tukey",
#'                           minLevelPercentage = 0.025)
#'
#' # Percentile outlier detection example:
#' overview <-  dataOverview(x = iris,
#'                           outlierMethod = "percentile",
#'                           lowPercentile = 0.025,
#'                           upPercentile = 0.975,
#'                           minLevelPercentage = 0.025)
#'
#' @author
#' Xander Horn

# FUNCTION TO PROVIDE AN OVERVIEW OF DATA
dataOverview <- function(x,
                         outlierMethod = "tukey", # tukey or percentile
                         lowPercentile = 0.01,
                         upPercentile = 0.99,
                         minLevelPercentage = 0.025){

  if(missing(x)){
    stop("Please supply a data.frame object")
  }

  if(outlierMethod %in% c("tukey","percentile")){

  } else {
    warning("Invalid 'outlierMethod' value, defaulting")
    outlierMethod <- "tukey"
  }

  if(lowPercentile < 0 | lowPercentile > 1 | upPercentile < 0 | upPercentile > 1){
    warning("Invalid 'lowPercentile' or 'upPercentile' value, defaulting")
    lowPercentile <- 0.01
    upPercentile <- 0.99
  }

  if(minLevelPercentage < 0 | minLevelPercentage > 1){
    warning("'minLevelPercentage' is limited between 0 and 1, defaulting")
    minLevelPercentage <- 0.025
  }

  for(i in 1:ncol(x)){
    if(class(x[,i]) %in% c("factor","character")){
      x[,i] <- toupper(x[,i])
    }
  }

  x <- as.data.frame(x)

  overview <- data.frame(Feature = names(x),
                         Observations = nrow(x),
                         FeatureClass = sapply(x, class),
                         FeatureType = NA,
                         PercentageMissing = sapply(x, function(x) sum(is.na(x))),
                         PercentageUnique = sapply(x, function(x) length(unique(x))),
                         ConstantFeature = NA,
                         ZeroSpreadFeature = NA,
                         LowerOutliers = NA,
                         UpperOutliers = NA,
                         ImputationValue = NA,
                         MinValue = NA,
                         FirstQuartile = NA,
                         Median = NA,
                         Mean = NA,
                         Mode = NA,
                         ThirdQuartile = NA,
                         MaxValue = NA,
                         LowerOutlierValue = NA,
                         UpperOutlierValue = NA)

  overview$Feature <- as.character(overview$Feature)
  overview$FeatureClass <- as.character(overview$FeatureClass)
  overview$ConstantFeature <- as.character(ifelse(overview$PercentageUnique == 1,"Yes","No"))
  overview$FeatureType <- as.character(ifelse(overview$FeatureClass %in% c("factor","character"),"Categorical",
                                              ifelse(overview$FeatureClass == "integer","Discrete","Continuous")))
  overview$PercentageMissing <- round(overview$PercentageMissing / overview$Observations,4) * 100
  overview$PercentageUnique <- round(overview$PercentageUnique / overview$Observations,4) * 100

  for(i in 1:ncol(x)){

    if(overview[i,"FeatureClass"] %in% c("integer","numeric")){

      x[,i] <- as.numeric(x[,i])

    } else if(overview[i,"FeatureClass"] %in% c("character","factor")){

      x[,i] <- as.character(x[,i])

    }

    # ZERO SPREAD FEATURES
    if(overview[i,"FeatureClass"] %in% c("integer","numeric")){

      overview[i,"ZeroSpreadFeature"] <- IQR(x[,i], na.rm = TRUE)[[1]]

    }

    # IMPUTATION VALUE
    if(overview[i,"FeatureClass"] %in% c("integer","numeric")){

      overview[i,"ImputationValue"] <- as.character(median(x[,i], na.rm = TRUE))

    } else if(overview[i,"FeatureClass"] %in% c("factor","character")){

      props <- as.data.frame(prop.table(table(x[,i])))
      levels <- as.character(props[which(props$Freq < minLevelPercentage),1])
      cumsumLevels <- sum(props[which(props$Freq < minLevelPercentage),2])

      if(cumsumLevels < minLevelPercentage){
        overview[i,"ImputationValue"] <- as.character(props[which.max(props$Freq),1])
      } else {
        overview[i,"ImputationValue"] <- "ALL_OTHER"
      }
    }

    # MODE
    levels <- data.frame(table(x[,i]))
    overview[i,"Mode"] <- as.character(levels[which.max(levels$Freq),1])

    # SUMMARY STATISTICS
    if(overview[i,"FeatureClass"] %in% c("integer","numeric")){
      overview[i,"MinValue"] <- min(x[,i], na.rm = TRUE)
      overview[i,"FirstQuartile"] <- quantile(x[,i], probs = 0.25, na.rm = TRUE)[[1]]
      overview[i,"Median"] <- round(median(x[,i], na.rm = TRUE),2)
      overview[i,"Mean"] <- round(mean(x[,i], na.rm = TRUE),2)
      overview[i,"ThirdQuartile"] <- quantile(x[,i], probs = 0.75, na.rm = TRUE)[[1]]
      overview[i,"MaxValue"] <- max(x[,i], na.rm = TRUE)
    }

    # OUTLIERS
    if(overview[i,"FeatureClass"] %in% c("integer","numeric")){
      if(outlierMethod == "tukey"){

        overview[i,"LowerOutlierValue"] <- quantile(x[,i], probs = 0.25, na.rm = TRUE)[[1]] - (1.5*IQR(x[,i], na.rm = TRUE))
        overview[i,"UpperOutlierValue"] <- quantile(x[,i], probs = 0.75, na.rm = TRUE)[[1]] + (1.5*IQR(x[,i], na.rm = TRUE))

        overview[i,"LowerOutliers"] <- length(which(x[,i] < overview[i,"LowerOutlierValue"]))
        overview[i,"UpperOutliers"] <- length(which(x[,i] > overview[i,"UpperOutlierValue"]))

      } else {

        overview[i,"LowerOutlierValue"] <- quantile(x[,i], probs = lowPercentile, na.rm = TRUE)[[1]]
        overview[i,"UpperOutlierValue"] <- quantile(x[,i], probs = upPercentile, na.rm = TRUE)[[1]]

        overview[i,"LowerOutliers"] <- length(which(x[,i] < overview[i,"LowerOutlierValue"]))
        overview[i,"UpperOutliers"] <- length(which(x[,i] > overview[i,"UpperOutlierValue"]))
      }
    }
  }

  overview$ZeroSpreadFeature <- as.character(ifelse(overview$ZeroSpreadFeature == 0,"Yes","No"))
  overview$ZeroSpreadFeature <- ifelse(is.na(overview$ZeroSpreadFeature) == TRUE,"No",overview$ZeroSpreadFeature)
  overview$ImputationValue <- ifelse(overview$FeatureClass %in% c("character","factor") & overview$PercentageMissing > minLevelPercentage,"MISSING",overview$ImputationValue)

  for(i in 1:ncol(overview)){
    overview[,i] <- ifelse(is.na(overview[,i]) == TRUE,0,overview[,i])
  }
  row.names(overview) <- NULL

  return(overview)
}
