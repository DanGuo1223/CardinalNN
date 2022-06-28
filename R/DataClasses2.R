

.SimpleImageArrayList_nn <- setRefClass("SimpleImageArrayList_nn",
                                     contains = c("SimpleImageList", "ImageArrayList"))







.MiCNN_classification <- setClass("MiCNN_classification", contains="SparseImagingResult")

.CrossValidated2 <- setClass("CrossValidated2", contains="SparseImagingResult")

setClass("SummaryMiCNN_classification", contains="SummaryDataFrame")
