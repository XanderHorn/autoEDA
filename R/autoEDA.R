#' Automated visual exploratory data analysis
#'
#' Automated visual exploratory analysis in a univariate or bivariate manner. Utilizes the other functions in the package should that be specified. Plots are produced using the ggplot2 library and themes are designed partly from the inspiration of the RColorBrewer library. Ability to customize plots are available. Data cleaning options are available which is essential before plotting. When data cleaning is not used, it can serve the purpose to identify areas in the data where attention needs to be paid to. Ability to output all plots to a PDF file.
#'
#' @param x [data.frame | Required]   Dataset which should contain the outcome feature. If x is not a data.frame object it will be converted to one.
#' @param y [character | Optional] The name of the outcome feature contained in the dataset specified in x. If y is NULL, a univariate analysis will be performed, else a bivariate analysis will take place with respect to the type of feature y is. Continuous features will be identified as a regression outcome else a classification outcome will be identified. Defaults to NULL.
#' @param IDFeats [character | Optional] A name or vector of names relating to ID features contained in the dataset that will be removed for plotting purposes. Defaults to NULL.
#' @param sampleRate [numeric | Optional] A value between 0 and 1 specifying if a simple random sample of the data should be taken to improve plotting speed. A value of 1 means that the entire dataset will be utilized. Defaults to 1.
#' @param outcomeType [character | Optional] The outcome type of the outcome feature specified in y. Available options are: automatic, binary (Binary classification), multi (Multi-class classification) and regression. For high cardinal categorical outcomes (>= 15), it is recommended to specify the outcome type manually. Defaults to automatic.
#' @param maxUniques [integer | Optional] The maximum allowed number of unique values a feature can have before it is converted to a numeric object type. Features with unique values <= the specified value will be converted to factor features. Defaults to 15.
#' @param maxLevels [integer | Optional] The maximum number of allowed levels a categorical feature is allowed to have before it is removed from plotting. High cardinal categorical features can pose problems with visualization techniques. Defaults to 25.
#' @param removeConstant [logical | Optional] Features containing a constant value or single unique value will be removed. Defaults to TRUE.
#' @param removeZeroSpread [logical | Optional] Features which exhibit zero spread will be removed. Zero spread is calculated by using the IQR values of features and only applies to continuous and discrete features. Defaults to TRUE.
#' @param removeMajorityMissing [logical | Optional] Features where more than half of the observations are missing will be removed. Defaults to TRUE.
#' @param imputeMissing [logical | Optional] Features containing missing values will be imputed. Imputation for continuous and discrete variables will be imputed using the median value. Categorical features will be replaced by a level called MISSING. Defaults to TRUE.
#' @param clipOutliers [logical | Optional] Features containing outliers as flagged by the specified outlier method will be clipped by using median replacement and only applies to continuous and discrete features. Defaults to TRUE.
#' @param minLevelPercentage [numeric | Optional] The minimum percentage data representation per level required for a categorical feature. Categorical features should ideally exhibit levels which contains adequate data proportions and levels with low proportions should require data cleaning. If a categorical feature has levels lower than the specified percentage, these levels will be used to determine the imputation value used. If the cumulative sum of the minimum levels are less than the specified minimum level, the imputation value is simply the mode of the feature, else all minimum levels are combined into a new level called ALL_OTHER. Defaults to 0.025.
#' @param predictivePower [logical | Optional] Should the predictive power be calculated per feature using the predictivePower function. Defaults to TRUE.
#' @param outlierMethod [character | Optional] Determines how outliers are identified. Two possible methods are available, tukey and percentile.   When specifying percentile based outlier detection, it is recommended to manually set the lower and upper percentile values for detection. Defaults to tukey.
#' @param lowPercentile [numeric | Optional] The lower percentile value that will be used to flag any values less than the calculated percentile as lower outliers. Recommended to set values between 0.01 and 0.05. Defaults to 0.01.
#' @param upPercentile [numeric | Optional] The upper percentile value that will be used to flag any values greater than the calculated percentile as upper outliers. Recommended to set values between 0.95 and 0.99. Defaults to 0.99.
#' @param plotCategorical [character | Optional] Specifies the type of plot to use when encountering categorical features. Available categorical plot types include: bar, stackedBar, groupedBar. When using groupedBar as a plot type, it is recommended to specify rotateLabels as TRUE. All bar plots are displayed in a relative frequency manner. Only applies to situations where a univariate analysis is being performed and categorical features are present or when a categorical outcome is specified and categorical features are present. Defaults to stackedBar.
#' @param plotContinuous [character | Optional] Specifies the type of plot to use when encountering continuous/discrete features. Available plot types for continuous features include: boxplot, qqplot, density, histogram. When specifying density as the desired plot type, transparency is automatically reduced. For continuous/discrete outcomes, continuous plots will be used when a categorical feature is present. Defaults to histogram.
#' @param bins [integer | Optional] The number of bins to use when histograms are the chosen plot type. Defaults to 20.
#' @param rotateLabels [logical | Optional] Should x-axis labels be rotated by 90 degrees. Defaults to FALSE.
#' @param colorTheme [integer | Optional] Specifies the color theme to use for plots when an outcome feature has been provided. Available values range from 1 to 4. Alternatively a vector of color names or hash codes can be provided to create a custom theme. Only applicable to univariate analyses. Defaults to 1.
#' @param theme [integer | Optional] Specifies the plot theme to use. Available options range from 1 to 2. Defaults to 2.
#' @param color [character | Optional] Specifies the color to use when performing univariate analyses. Defaults to "#26A69A".
#' @param transparency [numeric | Optional] Specifies the color transparency for plots. Lower values means more transparency and higher values means no transparency. Defaults to 1.
#' @param outputPath [character | Optional] The destination path where the output plots will be contained in a PDF file format. Should the path be left as NULL, all plotting will occur in R, else a valid path should be provided to create a PDF document containing all plots. Defaults to NULL.
#' @param filename [character | Optional] The filename of the PDF file that will consists of the plots should the output path be specified. Defaults to ExploratoryPlots.
#' @param returnPlotList [logical | Optional] Should plots generated be returned as a list 
#' @param verbose [logical | Optional] Should the function be chatty and provide feedback or not. Defaults to TRUE.
#' @return Object of type data.frame containing exploratory information and if specified predictive power per feature. Output is the same as the output generated from dataOverview. Output will change to object of type list when argument 'returnPlotList' is TRUE.
#' @export
#' @examples
#' # Bivariate classification example:
#' overview <-  autoEDA(x = iris,
#'                      y = "Species")
#'
#' # Bivariate regression example:
#' overview <-  autoEDA(x = iris,
#'                      y = "Sepal.Length")
#'
#' # Univariate example:
#' overview <-  autoEDA(x = iris)
#'
#' @author
#' Xander Horn

autoEDA <- function(x,
                    y = NULL,
                    IDFeats = NULL,
                    sampleRate = 1,
                    outcomeType = "automatic",
                    maxUniques = 15,
                    maxLevels = 25,
                    removeConstant = TRUE,
                    removeZeroSpread = TRUE,
                    removeMajorityMissing = TRUE,
                    imputeMissing = TRUE,
                    clipOutliers = TRUE,
                    minLevelPercentage = 0.025,
                    predictivePower = TRUE,
                    outlierMethod = "tukey",
                    lowPercentile = 0.01,
                    upPercentile = 0.99,
                    plotCategorical = "stackedBar",
                    plotContinuous = "histogram",
                    bins = 20,
                    rotateLabels = FALSE,
                    colorTheme = 1,
                    theme = 2,
                    color = "#26A69A",
                    transparency = 1,
                    outputPath = NULL,
                    filename = "ExploratoryPlots",
                    returnPlotList = FALSE,
                    verbose = TRUE){
  
  # LIBRARIES
  libExists <- function(x){
    if(!require(x,character.only = TRUE)){
      install.packages(x,dep = TRUE)
    }
    
    if(!require(x,character.only = TRUE)){
      stop("Package not found")
    }
  }
  libExists("ggplot2")
  libExists("RColorBrewer")
  
  library(RColorBrewer)
  library(ggplot2)
  
  # SETTINGS
  options(scipen = 999)
  set.seed(1991)
  
  x <- as.data.frame(x)
  
  # ERROR HANDLING
  if (is.null(outputPath) == FALSE){
    PDFPath = paste(outputPath, "/", filename,".pdf", sep = "")
    pdf(file = PDFPath)
  }
  
  if(maxLevels < 5){
    warning("Invalid 'maxLevels' value, defaulting")
    maxLevels <- 25
  }
  
  if(is.null(y) == FALSE & class(y) != "character"){
    stop("Outcome variable should be a character value of a variable name that exists in data")
  }
  
  if(is.null(y) == FALSE & length(which(y %in% names(x))) == 0){
    stop("Outcome variable does not exist in data")
  }
  
  if(transparency < 0 | transparency > 1){
    warning("Invalid 'transparency' value, defaulting")
    transparency <- 1
  }
  
  if(plotCategorical %in% c("stackedBar","bar","groupedBar")){
    err <- ""
  } else {
    warning("Invalid 'plotCategorical' value, defaulting")
    plotCategorical <- "stackedBar"
  }
  
  if(plotContinuous %in% c("boxplot","histogram","density","qqplot")){
    err <- ""
  } else {
    warning("Invalid 'plotContinuous' value, defaulting")
    plotContinuous <- "histogram"
  }
  
  if(bins < 0){
    warning("Invalid 'bins' value, defaulting")
    bins <- 20
  }
  
  if(theme %in% c(1,2)){
    err <- ""
  } else {
    warning("Invalid 'theme' value, defaulting")
    theme <- 2
  }
  
  if(class(color) != "character"){
    warning("Invalid 'color' value, needs to be a character string, defaulting")
    color <- "#26A69A"
  }
  
  if(sampleRate < 0 | sampleRate > 1){
    warning("Invalid 'sampleRate' value, defaulting")
    sampleRate <- 1
  }
  
  if(plotContinuous == "density" & is.null(y) == FALSE){
    transparency <- 0.7
  }
  
  # DECLARE GLOBAL VARIABLES
  plots <- list()
  
  # DECLARE FUNCTIONS
  boxplot_XY <- function(df, x, y, rotateLabels = FALSE, alpha = 0.5, theme = 1){
    p <- ggplot(data = df, aes(x = df[,y], y = df[,x], color = df[,y])) +
      geom_boxplot(lwd = 1,outlier.colour = "black", outlier.shape = 16, outlier.size = 2, alpha = alpha) +
      labs(x = y, y = x) +
      ggtitle(paste0("Distribution: ",x," By ",y)) +
      guides(fill = FALSE, colour = FALSE) +
      scale_color_manual(values = colors) +
      stat_summary(fun.y = mean, geom="point",colour="gray43", size=3)
    
    if(rotateLabels == TRUE){
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    if(theme == 2){
      p <- p + theme_classic()
      
      if(rotateLabels == TRUE){
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    }
    
    return(p)
  }
  
  density_XY <- function(df, x, y, alpha = 0.5, theme = 1){
    p <- ggplot(data = df, aes(x = df[,x], fill = df[,y], color = df[,y])) +
      geom_density(alpha = alpha) +
      labs(x = x, y = "Density") +
      ggtitle(paste0("Distribution: ",x," By ",y)) +
      guides(colour=FALSE, fill = guide_legend(title=y)) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    if(theme == 2){
      p <- p + theme_classic()
    }
    
    return(p)
  }
  
  histogram_XY <- function(df, x, y, alpha = 0.5, nrBins = 15, theme = 1){
    p <- ggplot(data = df, aes(x = df[,x], fill = df[,y])) +
      geom_histogram(bins = nrBins, alpha = alpha, color = "gray") +
      labs(x = x, y = "Frequency") +
      ggtitle(paste0("Distribution: ",x," By ",y)) +
      guides(colour=FALSE, fill = guide_legend(title=y)) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors)
    
    if(theme == 2){
      p <- p + theme_classic()
    }
    
    return(p)
  }
  
  qqplot_groupedXY <- function(df, x, y, alpha = 0.5, theme = 1){
    p <- ggplot(data = df, aes(x = seq(1:nrow(df)), y = df[,x], color = df[,y])) +
      geom_point(alpha = alpha) +
      labs(x = "Observation", y = x) +
      ggtitle(paste0("Distribution: ", x ," By ",y)) +
      guides(colour=guide_legend(title=y), fill = FALSE) +
      scale_color_manual(values = colors)
    
    if(theme == 2){
      p <- p + theme_classic()
    }
    
    return(p)
  }
  
  qqplot_XY <- function(df,x,y,alpha = 0.5, theme = 1, color = "steelblue"){
    p <- ggplot(data = df, aes(x = df[,x], y = df[,y])) +
      geom_point(color = color,alpha = alpha) +
      labs(x = x, y = y) +
      ggtitle(paste0("Distribution: ", x ," By ",y))
    
    if(theme == 2){
      p <- p + theme_classic()
    }
    
    return(p)
  }
  
  percentageXY_bar <- function(df, x, y, rotateLabels = FALSE, alpha = 0.5, theme = 1){
    p <- ggplot(data = df, aes(x = df[,x], fill = df[,y])) +
      geom_bar(aes(y = (..count..)/sum(..count..)), alpha = alpha) +
      scale_y_continuous(labels=scales::percent) +
      labs(x = x, y = "Relative Frequency") +
      ggtitle(paste0("Distribution: ",x," By ",y)) +
      guides(fill=guide_legend(title=y)) +
      scale_fill_manual(values = colors)
    
    if(rotateLabels == TRUE){
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    if(theme == 2){
      p <- p + theme_classic()
      
      if(rotateLabels == TRUE){
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
      
    }
    
    return(p)
  }
  
  percentageXY_groupedBar <- function(df, x, y, rotateLabels = FALSE, alpha = 0.5, theme = 1){
    p <- ggplot(data = df, aes(x = df[,x], fill = df[,x])) +
      geom_bar(aes(y = (..count..)/sum(..count..)), alpha = alpha) +
      scale_y_continuous(labels=scales::percent) +
      labs(x = x, y = "Relative Frequency") +
      ggtitle(paste0("Distribution: ",x," By ",y)) +
      theme(legend.position="none") +
      facet_grid(~df[,y]) +
      scale_fill_manual(values = colors)
    
    if(rotateLabels == TRUE){
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    if(theme == 2){
      p <- p + theme_classic() + theme(legend.position="none")
      
      if(rotateLabels == TRUE){
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    }
    
    return(p)
  }
  
  percentageXY_stackedBar <- function(df, x, y, rotateLabels = FALSE, alpha = 0.5, theme = 1){
    p <- ggplot(data = df, aes(x = df[,x], fill = df[,y])) +
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "fill", alpha = alpha) +
      scale_y_continuous(labels=scales::percent) +
      labs(x = x, y = "Relative Frequency") +
      ggtitle(paste0("Distribution: ",x," By ",y)) +
      guides(fill=guide_legend(title=y)) +
      scale_fill_manual(values = colors)
    
    if(rotateLabels == TRUE){
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    
    if(theme == 2){
      p <- p + theme_classic()
      
      if(rotateLabels == TRUE){
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    }
    
    return(p)
  }
  
  percentageX_bar <- function(df,x,rotateLabels = FALSE,alpha = 0.5,color = "steelblue", theme = 1){
    p <- ggplot(data = df, aes(x = df[,x])) +
      geom_bar(aes(y = (..count..)/sum(..count..)), fill = color, alpha = alpha) +
      scale_y_continuous(labels=scales::percent) +
      labs(x = x, y = "Relative Frequency") +
      ggtitle(paste0("Distribution: ",x))
    
    if(rotateLabels == TRUE){
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + guides(fill=guide_legend(title=y))
    }
    
    if(theme == 2){
      p <- p + theme_classic()
      
      if(rotateLabels == TRUE){
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    }
    
    return(p)
  }
  
  # 1.2 CONTINUOUS VARIABLES
  histogram_X <- function(df,x,color = "steelblue",nrBins = 15,alpha = 0.5, theme = 1,rotateLabels = FALSE){
    p <- ggplot(data = df, aes(x = df[,x])) +
      geom_histogram(bins = nrBins, alpha = alpha, fill = color, color = "gray") +
      ggtitle(paste0("Distribution: ",x)) +
      labs(x = x, y = "Frequency") +
      theme(legend.title=element_blank())
    
    if(rotateLabels == TRUE){
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + guides(fill=guide_legend(title=y))
    }
    
    if(theme == 2){
      p <- p + theme_classic()
      
      if(rotateLabels == TRUE){
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    }
    
    return(p)
  }
  
  density_X <- function(df,x,color = "steelblue",alpha = 0.5, theme = 1,rotateLabels = FALSE){
    p <- ggplot(data = df, aes(x = df[,x])) +
      geom_density(alpha = alpha, fill = color, color = color) +
      ggtitle(paste0("Distribution: ",x)) +
      labs(x = x, y = "Density") +
      theme(legend.title=element_blank())
    
    if(rotateLabels == TRUE){
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + guides(fill=guide_legend(title=y))
    }
    
    if(theme == 2){
      p <- p + theme_classic()
      
      if(rotateLabels == TRUE){
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    }
    
    return(p)
  }
  
  qqplot_X <- function(df, x, alpha = 0.5, theme = 1, color = "steelblue",rotateLabels = FALSE){
    p <- ggplot(data = df, aes(x = seq(1:nrow(df)), y = df[,x])) +
      geom_point(color = color,alpha = alpha) +
      labs(x = "Observation", y = x) +
      ggtitle(paste0("Distribution: ", x))
    
    if(rotateLabels == TRUE){
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + guides(fill=guide_legend(title=y))
    }
    
    if(theme == 2){
      p <- p + theme_classic()
      
      if(rotateLabels == TRUE){
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    }
    
    return(p)
  }
  
  boxplot_X <- function(df,x,color = "steelblue",alpha = 0.5, theme = 1,rotateLabels = FALSE){
    p <- ggplot(data = df, aes(x = factor(0), y = df[,x])) +
      geom_boxplot(lwd = 1,outlier.colour = "black", outlier.shape = 16, outlier.size = 2, color = color, alpha = alpha) +
      theme(axis.text.x = element_blank()) +
      ggtitle(paste0("Distribution: ",x)) +
      labs(x = "", y = x)
    
    if(rotateLabels == TRUE){
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + guides(fill=guide_legend(title=y))
    }
    
    if(theme == 2){
      p <- p + theme_classic()
      
      if(rotateLabels == TRUE){
        p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    }
    
    return(p)
  }
  
  # COLOR THEMES
  if(verbose == TRUE){
    cat("autoEDA | Setting color theme \n")
  }
  
  tol8qualitative <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#CC6677","#AA4499")
  set8equal <- c("#66C2A5", "#8DA0CB", "#A6D854", "#B3B3B3", "#E5C494", "#E78AC3", "#FC8D62", "#FFD92F")
  redmono = c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272", "#FCBBA1", "#FEE0D2")
  greenmono = c("#005A32", "#238B45", "#41AB5D", "#74C476", "#A1D99B", "#C7E9C0", "#E5F5E0")
  bluemono = c("#084594", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF", "#DEEBF7")
  greymono = c("#000000","#252525", "#525252", "#737373", "#969696", "#BDBDBD", "#D9D9D9")
  
  theme1 <- c(brewer.pal(9,"Set1"),brewer.pal(12,"Paired"))
  theme2 <- c(brewer.pal(12,"Paired"),brewer.pal(8,"Accent"))
  theme3 <- c(brewer.pal(8,"Dark2"),set8equal,tol8qualitative)
  theme4 <- c("#4527A0","#B39DDB",bluemono,redmono,greenmono,greymono)
  
  if(colorTheme == 1){
    colors <- theme1
  } else if(colorTheme == 2){
    colors <- theme2
  } else if(colorTheme == 3){
    colors <- theme3
  } else if(colorTheme == 4){
    colors <- theme4
  } else {
    colors <- colorTheme
  }
  
  # SAMPLE DATA
  ind <- sample(nrow(x),sampleRate*nrow(x),replace = F)
  x <- x[ind,]
  
  # REMOVE ID FEATURES
  if(is.null(IDFeats) == FALSE){
    if(verbose == TRUE){
      cat("autoEDA | Removing ID features \n")
    }
    x <- x[,setdiff(names(x),IDFeats)]
  }
  
  # GET OVERVIEW OF DATA
  overview <- dataOverview(x,
                           outlierMethod = outlierMethod,
                           upPercentile = upPercentile,
                           lowPercentile = lowPercentile,
                           minLevelPercentage = minLevelPercentage)
  
  # REMOVE CONSTANT FEATURES
  if(removeConstant == TRUE){
    if(verbose == TRUE){
      cat("autoEDA | Removing constant features \n")
    }
    remove <- as.character(overview[which(overview$ConstantFeature == "Yes"),1])
    x <- x[,setdiff(names(x),remove)]
    if(verbose == TRUE){
      if(length(remove) == 1){
        cat(paste0("autoEDA | ",length(remove)," constant feature removed \n"))
      } else {
        cat(paste0("autoEDA | ",length(remove)," constant features removed \n"))
      }
    }
  }
  
  # REMOVE ZERO SPREAD FEATURES
  if(removeZeroSpread == TRUE){
    remove <- as.character(overview[which(overview$ZeroSpreadFeature == "Yes"),1])
    if(is.null(y) == FALSE){
      if(verbose == TRUE){
        cat("autoEDA | Removing zero spread features \n")
      }
      remove <- setdiff(remove,y)
    }
    if(verbose == TRUE){
      if(length(remove) == 1){
        cat(paste0("autoEDA | ",length(remove)," zero spread feature removed \n"))
      } else {
        cat(paste0("autoEDA | ",length(remove)," zero spread features removed \n"))
      }
    }
    x <- x[,setdiff(names(x),remove)]
  }
  
  # REMOVE FEATURES CONTAINING MAJORITY MISSING INFORMATION
  if(removeMajorityMissing == TRUE){
    if(verbose == TRUE){
      cat("autoEDA | Removing features containing majority missing values \n")
    }
    remove <- as.character(overview[which(overview$PercentageMissing > 50),1])
    if(verbose == TRUE){
      if(length(remove) == 1){
        cat(paste0("autoEDA | ",length(remove)," majority missing feature removed \n"))
      } else {
        cat(paste0("autoEDA | ",length(remove)," majority missing features removed \n"))
      }
    }
    x <- x[,setdiff(names(x),remove)]
  }
  
  # IF FEATURES WERE REMOVED SUBSET OVERVIEW AND ORDER FRAME
  overview <- subset(overview,overview$Feature %in% names(x))
  x <- x[,names(x)]
  
  # FORMAT FEATURES
  if(is.null(y) == TRUE){
    
    for(i in 1:ncol(x)){
      if(class(x[,i]) %in% c("integer","numeric") & length(unique(x[,i])) > maxUniques){
        x[,i] <- as.numeric(x[,i])
      } else if(class(x[,i]) %in% c("integer","numeric") & length(unique(x[,i])) <= maxUniques){
        x[,i] <- as.character(x[,i])
      } else {
        x[,i] <- toupper(as.character(x[,i]))
      }
    }
    
  } else {
    
    for(i in 1:ncol(x)){
      if(class(x[,i]) %in% c("integer","numeric") & length(unique(x[,i])) > maxUniques & names(x)[i] != y){
        x[,i] <- as.numeric(x[,i])
      } else if(class(x[,i]) %in% c("integer","numeric") & length(unique(x[,i])) <= maxUniques & names(x)[i] != y){
        x[,i] <- as.character(x[,i])
      } else if(class(x[,i]) %in% c("factor","character") & names(x)[i] != y){
        x[,i] <- toupper(as.character(x[,i]))
      }
    }
    
  }
  
  # GET OVERVIEW OF DATA
  overview <- dataOverview(x,
                           outlierMethod = outlierMethod,
                           upPercentile = upPercentile,
                           lowPercentile = lowPercentile,
                           minLevelPercentage = minLevelPercentage)
  
  # CLEAN DATA ACCORDING TO OVERVIEW
  if(verbose == TRUE){
    cat("autoEDA | Cleaning data \n")
  }
  
  if(is.null(y) == TRUE){
    for(i in 1:ncol(x)){
      
      if(clipOutliers == TRUE & class(x[,i]) == "numeric"){
        x[,i] <- ifelse(x[,i] < overview[i,"LowerOutlierValue"], as.numeric(overview[i,"ImputationValue"]), x[,i])
        x[,i] <- ifelse(x[,i] > overview[i,"UpperOutlierValue"], as.numeric(overview[i,"ImputationValue"]), x[,i])
      }
      
      if(imputeMissing == TRUE){
        if(class(x[,i]) == "numeric"){
          x[,i] <- ifelse(is.na(x[,i]) == TRUE, as.numeric(overview[i,"ImputationValue"]), x[,i])
        } else {
          x[,i] <- ifelse(is.na(x[,i]) == TRUE, overview[i,"ImputationValue"], x[,i])
        }
      }
    }
  } else {
    for(i in 1:ncol(x)){
      
      if(clipOutliers == TRUE & class(x[,i]) == "numeric" & names(x)[i] != y){
        x[,i] <- ifelse(x[,i] < overview[i,"LowerOutlierValue"], as.numeric(overview[i,"ImputationValue"]), x[,i])
        x[,i] <- ifelse(x[,i] > overview[i,"UpperOutlierValue"], as.numeric(overview[i,"ImputationValue"]), x[,i])
      }
      
      if(imputeMissing == TRUE){
        if(class(x[,i]) == "numeric"){
          x[,i] <- ifelse(is.na(x[,i]) == TRUE, as.numeric(overview[i,"ImputationValue"]), x[,i])
        } else {
          x[,i] <- ifelse(is.na(x[,i]) == TRUE, overview[i,"ImputationValue"], x[,i])
        }
      }
    }
  }
  
  # FORMAT FEATURES
  if(is.null(y) == TRUE){
    
    for(i in 1:ncol(x)){
      if(class(x[,i]) %in% c("integer","numeric") & length(unique(x[,i])) > maxUniques){
        x[,i] <- as.numeric(x[,i])
      } else if(class(x[,i]) %in% c("integer","numeric") & length(unique(x[,i])) <= maxUniques){
        x[,i] <- as.factor(as.character(x[,i]))
        levels <- as.character(sort(as.numeric(levels(factor(x[,i])))))
        x[,i] <- factor(x[,i], levels = levels)
      } else {
        x[,i] <- as.factor(as.character(x[,i]))
        levels <- as.character(sort(levels(factor(x[,i]))))
        x[,i] <- factor(x[,i], levels = levels)
      }
    }
    
  } else {
    
    for(i in 1:ncol(x)){
      if(class(x[,i]) %in% c("integer","numeric") & length(unique(x[,i])) > maxUniques & names(x)[i] != y){
        x[,i] <- as.numeric(x[,i])
      } else if(class(x[,i]) %in% c("integer","numeric") & length(unique(x[,i])) <= maxUniques & names(x)[i] != y){
        x[,i] <- as.factor(as.character(x[,i]))
        levels <- as.character(sort(as.numeric(levels(factor(x[,i])))))
        x[,i] <- factor(x[,i], levels = levels)
      } else if(class(x[,i]) %in% c("factor","character") & names(x)[i] != y){
        x[,i] <- as.factor(as.character(x[,i]))
        levels <- as.character(sort(levels(factor(x[,i]))))
        x[,i] <- factor(x[,i], levels = levels)
      }
    }
  }
  
  # SPARSE LEVELS OF CATEGORICAL FEATURES
  if(minLevelPercentage > 0 & is.null(y) == TRUE){
    if(verbose == TRUE){
      cat("autoEDA | Correcting sparse categorical feature levels \n")
    }
    
    for(i in 1:ncol(x)){
      if(class(x[,i]) %in% c("factor","character")){
        
        x[,i] <- as.character(x[,i])
        props <- as.data.frame(prop.table(table(x[,i])))
        levels <- as.character(props[which(props$Freq < minLevelPercentage),1])
        cumsumLevels <- sum(props[which(props$Freq < minLevelPercentage),2])
        
        if(cumsumLevels < minLevelPercentage){
          x[,i] <- ifelse(x[,i] %in% levels, overview[i,"Mode"], x[,i])
          x[,i] <- as.factor(as.character(x[,i]))
          levels <- as.character(sort(levels(factor(x[,i]))))
          x[,i] <- factor(x[,i], levels = levels)
        } else {
          x[,i] <- ifelse(x[,i] %in% levels, "ALL_OTHER", x[,i])
          x[,i] <- as.factor(as.character(x[,i]))
          levels <- as.character(sort(levels(factor(x[,i]))))
          x[,i] <- factor(x[,i], levels = levels)
        }
      }
    }
  } else if(minLevelPercentage > 0 & is.null(y) == FALSE){
    if(verbose == TRUE){
      cat("autoEDA | Correcting sparse categorical feature levels \n")
    }
    
    for(i in 1:ncol(x)){
      if(class(x[,i]) %in% c("factor","character") & names(x)[i] != y){
        
        x[,i] <- as.character(x[,i])
        props <- as.data.frame(prop.table(table(x[,i])))
        levels <- as.character(props[which(props$Freq < minLevelPercentage),1])
        cumsumLevels <- sum(props[which(props$Freq < minLevelPercentage),2])
        
        if(cumsumLevels < minLevelPercentage){
          x[,i] <- ifelse(x[,i] %in% levels, overview[i,"Mode"], x[,i])
          x[,i] <- as.factor(as.character(x[,i]))
          levels <- as.character(sort(levels(factor(x[,i]))))
          x[,i] <- factor(x[,i], levels = levels)
        } else {
          x[,i] <- ifelse(x[,i] %in% levels, "ALL_OTHER", x[,i])
          x[,i] <- as.factor(as.character(x[,i]))
          levels <- as.character(sort(levels(factor(x[,i]))))
          x[,i] <- factor(x[,i], levels = levels)
        }
      }
    }
  }
  
  # COMBINING CATEGORICAL LEVELS CAN CAUSE CONSTANT FEATURES
  if(removeConstant == TRUE){
    temp <- data.frame(Feature = names(x),
                       Unique = sapply(x, function(x) length(unique(x))))
    remove <- as.character(temp[which(temp$Unique == 1),1])
    if(length(remove) > 0){
      overview <- subset(overview,overview$Feature %in% names(x))
      x <- x[,names(x)]
    }
  }
  
  # REMOVE CATEGORICAL FEATURES ABOVE maxLevels VALUE
  if(is.null(y) == FALSE){
    temp <- data.frame(Feature = names(x),
                       Class = sapply(x,class),
                       Unique = sapply(x, function(x) length(unique(x))))
    temp <- subset(temp,temp$Feature != y)
    temp <- subset(temp,temp$Class %in% c("character","factor"))
    remove <- as.character(temp[which(temp$Unique > maxLevels),1])
    if(length(remove) > 0){
      overview <- subset(overview,overview$Feature %in% names(x))
      x <- x[,names(x)]
    }
  } else {
    temp <- data.frame(Feature = names(x),
                       Class = sapply(x,class),
                       Unique = sapply(x, function(x) length(unique(x))))
    temp <- subset(temp,temp$Class %in% c("character","factor"))
    remove <- as.character(temp[which(temp$Unique > maxLevels),1])
    if(length(remove) > 0){
      overview <- subset(overview,overview$Feature %in% names(x))
      x <- x[,names(x)]
    }
  }
  
  # RE-ORDER THAT OUTCOME IS FIRST
  if(is.null(y) == FALSE){
    if(verbose == TRUE){
      cat("autoEDA | Sorting features \n")
    }
    
    feats <- setdiff(names(x),y)
    feats <- c(y,feats)
    x <- x[,feats]
  }
  
  # DETERMINE OUTCOME TYPE IF Y IS GIVEN
  if(outcomeType == "automatic"){
    if(is.null(y) == FALSE){
      if(length(unique(x[,y])) == 2){
        outcomeType <- "binary"
        x[,y] <- as.factor(x[,y])
        if(verbose == TRUE){
          cat("autoEDA | Binary classification outcome detected \n")
        }
        
      } else if(length(unique(x[,y])) > 2 & length(unique(x[,y])) <= 15){
        outcomeType <- "multi"
        x[,y] <- as.factor(x[,y])
        if(verbose == TRUE){
          cat("autoEDA | Multi-class classification outcome detected \n")
        }
        
      } else {
        outcomeType <- "regression"
        x[,y] <- as.numeric(x[,y])
        if(verbose == TRUE){
          cat("autoEDA | Regression outcome detected \n")
        }
        
      }
    } else {
      outcomeType <- "none"
      if(verbose == TRUE){
        cat("autoEDA | Performing univariate analysis \n")
      }
      
    }
  } else {
    outcomeType <- outcomeType
  }
  
  if(color == "#26A69A" & outcomeType != "none"){
    color <- colors[1]
  }
  
  #CALCULATE PREDICTIVE POWER OF FEATURES
  options(warn = -1)
  if(predictivePower == TRUE & is.null(y) == FALSE){
    if(verbose == TRUE){
      cat("autoEDA | Calculating feature predictive power \n")
    }
    pp <- predictivePower(x = x,
                          y = y,
                          outcomeType = outcomeType)
    
    overview <- merge(x = overview,
                      y = pp,
                      by.x = "Feature",
                      all.x = TRUE)
    overview$PredictivePower <- ifelse(is.na(overview$PredictivePower) == TRUE, "Low", overview$PredictivePower)
    overview$PredictivePower <- factor(overview$PredictivePower, levels = c("Low","Medium","High"))
    
    pp_plot <- percentageXY_bar(df = overview, x = "PredictivePower", y = "PredictivePower", rotateLabels = rotateLabels, alpha = transparency, theme = theme) +
      ggtitle("Predictive power of features")
  }
  options(warn = 0)
  
  # VISUALIZE DATA
  if(verbose == TRUE){
    cat("autoEDA | Visualizing data \n")
  }
  
  if(outcomeType %in% c("binary","multi")){
    for(i in 1:ncol(x)){
      
      if(names(x)[i] == y){
        plots[[length(plots) + 1]] <- percentageXY_bar(df = x, x = y, y = y, rotateLabels = rotateLabels, alpha = transparency, theme = theme) +
          ggtitle("Outcome distribution")
        names(plots)[[length(plots)]] <- names(x)[i]
      }
      
      if(class(x[,i]) %in% c("character","factor") & names(x)[i] != y){
        if(plotCategorical == "bar"){
          plots[[length(plots) + 1]] <- percentageXY_bar(df = x, x = y, y = names(x)[i], rotateLabels = rotateLabels, alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotCategorical == "groupedBar"){
          plots[[length(plots) + 1]] <- percentageXY_groupedBar(df = x, x = names(x)[i], y = y, rotateLabels = rotateLabels, alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotCategorical == "stackedBar"){
          plots[[length(plots) + 1]] <- percentageXY_stackedBar(df = x, x = y, y = names(x)[i], rotateLabels = rotateLabels, alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        }
        
      } else if(class(x[,i]) %in% c("integer","numeric") & names(x)[i] != y){
        if(plotContinuous == "boxplot"){
          plots[[length(plots) + 1]] <- boxplot_XY(df = x, x = names(x)[i], y = y, rotateLabels = rotateLabels, alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotContinuous == "density"){
          plots[[length(plots) + 1]] <- density_XY(df = x, x = names(x)[i], y = y, alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotContinuous == "histogram"){
          plots[[length(plots) + 1]] <- histogram_XY(df = x, x = names(x)[i], y = y, nrBins = bins, alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotContinuous == "qqplot"){
          plots[[length(plots) + 1]] <- qqplot_groupedXY(df = x, x =  names(x)[i], y = y, alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        }
      }
      
    }
  }
  
  if(outcomeType == "regression"){
    for(i in 1:ncol(x)){
      
      if(names(x)[i] == y){
        plots[[length(plots) + 1]] <- histogram_X(df = x, x = names(x)[i], nrBins = bins, color = color, alpha = transparency, theme = theme) +
          ggtitle("Outcome distribution")
        names(plots)[[length(plots)]] <- names(x)[i]
        plots[[length(plots) + 1]] <- qqplot_X(df = x, x = names(x)[i], color = color, alpha = transparency, theme = theme) +
          ggtitle("Outcome distribution")
        names(plots)[[length(plots)]] <- names(x)[i]
      }
      
      if(class(x[,i]) %in% c("character","factor") & names(x)[i] != y){
        if(plotContinuous == "boxplot"){
          plots[[length(plots) + 1]] <- boxplot_XY(df = x, x = y, y = names(x)[i], rotateLabels = rotateLabels, alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotContinuous == "qqplot"){
          plots[[length(plots) + 1]] <- qqplot_groupedXY(df = x, x =  y, y = names(x)[i], alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotContinuous == "histogram"){
          plots[[length(plots) + 1]] <- histogram_XY(df = x, x = y, y = names(x)[i], alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotContinuous == "density"){
          plots[[length(plots) + 1]] <- density_XY(df = x, x = y, y = names(x)[i], alpha = transparency, theme = theme)
          names(plots)[[length(plots)]] <- names(x)[i]
        }
        
      } else if(class(x[,i]) %in% c("integer","numeric") & names(x)[i] != y){
        plots[[length(plots) + 1]] <- qqplot_XY(df = x, x = names(x)[i], y = y, alpha = transparency, theme = theme, color = color)
        names(plots)[[length(plots)]] <- names(x)[i]
      }
      
    }
  }
  
  if(outcomeType == "none"){
    for(i in 1:ncol(x)){
      
      if(class(x[,i]) %in% c("character","factor")){
        plots[[length(plots) + 1]] <- percentageX_bar(df = x, x = names(x)[i], color = color, alpha = transparency, theme = theme, rotateLabels = rotateLabels)
        names(plots)[[length(plots)]] <- names(x)[i]
        
      } else if(class(x[,i]) %in% c("integer","numeric")){
        if(plotContinuous == "boxplot"){
          plots[[length(plots) + 1]] <- boxplot_X(df = x, x = names(x)[i], color = color, alpha = transparency, theme = theme, rotateLabels = rotateLabels)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotContinuous == "density"){
          plots[[length(plots) + 1]] <- density_X(df = x, x = names(x)[i], color = color, alpha = transparency, theme = theme, rotateLabels = rotateLabels)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotContinuous == "histogram"){
          plots[[length(plots) + 1]] <- histogram_X(df = x, x = names(x)[i], nrBins = bins, color = color, alpha = transparency, theme = theme, rotateLabels = rotateLabels)
          names(plots)[[length(plots)]] <- names(x)[i]
        } else if(plotContinuous == "qqplot"){
          plots[[length(plots) + 1]] <- qqplot_X(df = x, x = names(x)[i], color = color, alpha = transparency, theme = theme, rotateLabels = rotateLabels)
          names(plots)[[length(plots)]] <- names(x)[i]
        }
      }
    }
  }
  
  # ADD OVERVIEW PLOTS TO PLOT LIST BEFORE PLOTTING
  if(predictivePower == TRUE & is.null(y) == FALSE){
    plots[[length(plots) + 1]] <- pp_plot
    names(plots)[[length(plots)]] <- "PredictivePower"
  }
  
  if(returnPlotList == FALSE){
    invisible(lapply(plots,function(x) plot(x,main="some plot")))
  }
  
  for(i in 1:ncol(overview)){
    if(class(overview[,i]) %in% c("integer","numeric")){
      overview[,i] <- ifelse(is.na(overview[,i]) == TRUE,0,overview[,i])
    }
  }
  
  if (is.null(outputPath) == FALSE){
    dev.off()
    if(verbose == TRUE){
      cat(paste0("autoEDA | Output generated to ",outputPath," \n"))
      cat(paste0("autoEDA | Output contained in file name ",filename,".pdf \n"))
    }
  }
  
  if(returnPlotList == FALSE){
    rm(list = setdiff(ls(),c("overview")))
    invisible(gc())
    return(overview)
  } else {
    rm(list = setdiff(ls(),c("overview","plots","colors","color")))
    
    invisible(gc())
    return(list(overview = overview, plots = plots))
  }
}
