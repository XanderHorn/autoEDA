#' Calcualtes feature predictive power
#'
#' Feature predictive power will be calculated for all features contained in a dataset along with the outcome feature. Works for binary classification, multi-class classification and regression problems. Can also be used when exploring a feature of interest to determine correlations of independent features with the outcome feature. When the outcome feature is continuous of nature or is a regression problem, correlation calculations are performed. When the outcome feature is categorical of nature or is a classification problem, the Kolmogorov Smirnov distance measure is used to determine predictive power. For multi-class classification outcomes, a one vs all approach is taken which is then averaged to arrive at the mean KS distance measure. The predictive power is sensitive towards the manner in which the data has been prepared and will differ should the manner in which the data has been prepared changes.
#' @param x [data.frame | Required]   Dataset which should contain the outcome feature. If x is not a data.frame object it will be converted to one.
#' @param y [character | Required] The name of the outcome feature contained in the dataset specified in x.
#' @param outcomeType [character | Optional] The outcome type of the outcome feature specified in y. Available options are: automatic, binary (Binary classification), multi (Multi-class classification) and regression. For high cardinal categorical outcomes (>= 15), it is recommended to specify the outcome type manually. Defaults to automatic.
#' @return Object of type data.frame containing all features and their respective predictive power.
#' @examples.-
#' # Classification example:
#' power <-  predictivePower(x = iris,
#'                           y = "Species",
#'                           outcomeType = "automatic")
#'
#' # Regression example:
#' power <-  predictivePower(x = iris,
#'                           y = "Sepal.Length",
#'                           outcomeType = "automatic")
#'
#' # Manually specifying outcome type example:
#' power <-  predictivePower(x = iris,
#'                           y = "Sepal.Length",
#'                           outcomeType = "regression")
#'
#' power <-  predictivePower(x = iris,
#'                           y = "Species",
#'                           outcomeType = "multi")
#'
#' @author
#' Xander Horn

# PREDICTIVE POWER FUNCTION
predictivePower <- function(x,
                            y,
                            outcomeType = "automatic"){

options(cipen = 999)
set.seed(1991)

if(missing(x)){
  stop("Invalid 'x' value, provide a dataset")
}

if(missing(y)){
  stop("Invalid 'y' value, speficy outcome variable name as a character string")
}

if(outcomeType %in% c("automatic","binary","multi","regression")){
  err <- ""
} else {
  warning("'outcomeType' must be a valid value, see help document, defaulting")
  outcomeType <- "automatic"
}

x <- as.data.frame(x)

# DETERMINE OUTCOME TYPE
if(outcomeType == "automatic"){
  if(is.null(y) == FALSE){
    if(length(unique(x[,y])) == 2){
      outcomeType <- "binary"
      x[,y] <- as.factor(x[,y])
    } else if(length(unique(x[,y])) > 2 & length(unique(x[,y])) <= 15){
      outcomeType <- "multi"
      x[,y] <- as.factor(x[,y])
    } else {
      outcomeType <- "regression"
      x[,y] <- as.numeric(x[,y])
    }
  }
} else {
  outcomeType <- outcomeType
}

# CREATE FRAME THAT WILL HOUSE RESULTS
temp <- data.frame(Feature = names(x),
                   PredictivePowerPercentage = NA,
                   PredictivePower = NA)
options(warn = -1)
for(i in 1:ncol(x)){
  if(outcomeType == "regression"){
    feat <- x[,i]
    outcome <- x[,y]
    if(class(feat) == "numeric"){
      power <- abs(round(cor(outcome,feat, use = "complete.obs"),2))
    } else {
      feat <- as.numeric(as.factor(x[,i]))
      power <- abs(round(cor(outcome,feat, use = "complete.obs"),2))
    }
    temp[i,"PredictivePowerPercentage"] <- power
    temp[i,"PredictivePower"] <- ifelse(power <= 0.3,"Low",
                                        ifelse(power > 0.3 & power <= 0.5,"Medium","High"))
  } else {
    classes <- unique(x[,y])
    temp1 <- list()
    temp2 <- list()

    if(class(x[,i]) %in% c("integer","numeric")){
      if(names(x)[i] != y){
        for(j in 1:length(classes)){
          df <- subset(x,x[,y] == classes[j])
          temp2[[j]] <- df[,c(names(x)[i],y)]
        }

        df <- as.data.frame(do.call(rbind,temp2))
        class1 <- subset(df,df[,y] == classes[1])[[names(x)[i]]]
        rest <- subset(df,df[,y] != classes[1])
        classes <- unique(rest[,y])

        for(k in 1:length(classes)){
          compare <- subset(rest,rest[,y] == classes[k])[[names(x)[i]]]
          ks <- ks.test(class1, compare)
          temp1[[i]] <- ks$statistic[[1]]
        }

        power <- as.numeric(do.call(rbind,temp1))
        power <- round(mean(power),2)

        temp[i,"PredictivePowerPercentage"] <- power
        temp[i,"PredictivePower"] <- ifelse(power <= 0.3,"Low",
                                            ifelse(power > 0.3 & power <= 0.5,"Medium","High"))
      }
    } else {
      if(names(x)[i] != y){
        x[,i] <- as.numeric(as.factor(x[,i]))

        for(j in 1:length(classes)){
          df <- subset(x,x[,y] == classes[j])
          temp2[[j]] <- df[,c(names(x)[i],y)]
        }

        df <- as.data.frame(do.call(rbind,temp2))
        class1 <- subset(df,df[,y] == classes[1])[[names(x)[i]]]
        rest <- subset(df,df[,y] != classes[1])
        classes <- unique(rest[,y])

        for(k in 1:length(classes)){
          compare <- subset(rest,rest[,y] == classes[k])[[names(x)[i]]]
          ks <- ks.test(class1, compare)
          temp1[[i]] <- ks$statistic[[1]]
        }

        power <- as.numeric(do.call(rbind,temp1))
        power <- round(mean(power),2)

        temp[i,"PredictivePowerPercentage"] <- power
        temp[i,"PredictivePower"] <- ifelse(power <= 0.3,"Low",
                                            ifelse(power > 0.3 & power <= 0.5,"Medium","High"))
      }

    }

  }

}

temp$PredictivePowerPercentage <- temp$PredictivePowerPercentage * 100
temp$PredictivePowerPercentage <- ifelse(temp$Feature == y, NA, temp$PredictivePowerPercentage)
temp$PredictivePower <- ifelse(temp$Feature == y, NA, temp$PredictivePower)
temp <- subset(temp, temp$Feature != y)
options(warn = 0)
return(temp)
}
