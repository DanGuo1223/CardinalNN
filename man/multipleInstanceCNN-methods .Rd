\name{multipleInstanceCNN-methods}
\docType{methods}
\alias{multipleInstanceCNN}
\alias{class:MiCNN_classification}
\alias{MiCNN_classification-class}
\alias{multipleInstanceCNN-methods}


\alias{multipleInstanceCNN,SparseImagingExperiment,ANY-method}
\alias{predict,MiCNN_classification-method}
%\alias{fitted,SpatialShrunkenCentroids2-method}
%\alias{summary,SpatialShrunkenCentroids2-method}
%\alias{logLik,SpatialShrunkenCentroids2-method}

%\alias{spatialShrunkenCentroids,SImageSet,missing-method}
%\alias{spatialShrunkenCentroids,SImageSet,factor-method}
%\alias{spatialShrunkenCentroids,SImageSet,character-method}
%\alias{predict,SpatialShrunkenCentroids-method}
%\alias{logLik,SpatialShrunkenCentroids-method}
%\alias{summary,SpatialShrunkenCentroids-method}
%\alias{print.summary.SpatialShrunkenCentroids}
%\alias{plot.summary.SpatialShrunkenCentroids}

\title{Multiple instance based deep learning method for image classification}
\description{
    Performs multiple instance based or fully supervised convolutional neural network to classify tissues using spectra information.
}
\usage{


\S4method{multipleInstanceCNN}{SparseImagingExperiment,ANY}(x, y, model_name = 'classifier', groups  = run(x), method = c('multiple_instance', 'fully_supervised'), batch_size = 32, optim = 'Adam', lr = 0.00001, num_epochs =10, \dots)

\S4method{predict}{MiCNN_classification}(object, newx, newy, \dots)

%\S4method{fitted}{SpatialShrunkenCentroids2}(object, \dots)

%\S4method{summary}{MiCNN_classification}(object, \dots)

%\S4method{spatialShrunkenCentroids}{SImageSet,missing}(x, r = 1, k = 3, s = 0,
%    method = c("gaussian", "adaptive"),
%    iter.max=10, \dots)

%\S4method{spatialShrunkenCentroids}{SImageSet,factor}(x, y, r = 1, s = 0,
%    method = c("gaussian", "adaptive"),
%    priors = table(y), \dots)

%\S4method{spatialShrunkenCentroids}{SImageSet,character}(x, y, \dots)

%\S4method{predict}{SpatialShrunkenCentroids}(object, newx, newy, \dots)
}
\arguments{
    \item{x}{The imaging dataset to segment or classify.}
    \item{y}{A \code{factor} or \code{character} response.}
    \item{model_name}{The name of the CNN classifier.}
    \item{method}{The method to train the classifier. 'multiple_instance' stands for multiple instance based learning and it is used when only the tissue-level labels are available; 'fully_supervised' stands for standard supervised training and it is used when pixel-level labels are available.}
    \item{batch_size}{The batch size used during training.}
    \item{lr}{The learning rate to train the classifier.}
    \item{optim}{The optimizer to train the classifier.}
    \item{num_epochs}{The number of epochs to train the classifier.}
    \item{\dots}{Passed to internal methods.}

    \item{object}{The result of a previous call to \code{\link{multipleinstanceCNN}}.}
    \item{newx}{An imaging dataset for which to calculate the predicted response from shrunken centroids.}
    \item{newy}{Optionally, a new response from which residuals should be calculated.}
}
\value{
    An object of class \code{MiCNN_classification}, which is a \code{ImagingResult}, or an object of class \code{MiCNN_classification}, which is a \code{ResultSet}. Each element of the \code{resultData} slot contains at least the following components:

    \describe{
        \item{\code{class, classes}:}{A factor indicating the predicted class for each pixel in the dataset.}

    }
}
\author{
    Dan Guo
}
\references{
Guo, Dan, et al. "Deep multiple instance learning classifies subtissue locations in mass spectrometry images from tissue-level annotations." Bioinformatics 36.Supplement_1 (2020): i300-i308.
}

\examples{

res <- multipleInstanceCNN(x, y, method="multiple_instance")

summary(res)

image(res)
}

\keyword{multiple instance}
\keyword{CNN}
\keyword{classif}

