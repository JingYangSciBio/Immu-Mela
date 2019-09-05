waterfall_calcMutFreq <- function(x)
{
  message("Calculating frequency of mutations...")
  # Change trv_type calls to either synonymous or non synonymous,
  # for use in the mutation per Mb plot
  x$trv_type <- as.character(x$trv_type)
  x$trv_type[toupper(x$trv_type) != toupper('silent')] <- 'Non Synonymous'
  x$trv_type[toupper(x$trv_type) == toupper('silent')] <- 'Synonymous'
  x$trv_type <- factor(x$trv_type, levels=c('Synonymous', 'Non Synonymous'))
  
  # Obtain a data frame of mutation counts on the sample level
  mutation_counts <- table(x[,c('sample', 'trv_type')])
  mutation_counts <- as.data.frame(reshape2::melt(mutation_counts))
  colnames(mutation_counts) <- c('sample', 'trv_type', 'mutation_total')
  
  return(mutation_counts)
}
library(GenVisR)
waterfall_qual <- function(x, y, z, file_type, label_col)
{
  # print message statement
  message("Checking if input is properly formatted...")
  
  # Check input data to x
  if(!is.data.frame(x))
  {
    stop("Did not detect a data frame for input to x")
  }
  
  # Convert file type to internal format
  if(toupper(file_type) == toupper("MAF"))
  {
    x <- waterfall_MAF2anno(x, label_col)
  } else if(toupper(file_type) == toupper("MGI")) {
    x <- waterfall_MGI2anno(x, label_col)
  } else if(toupper(file_type) == toupper("Custom")) {
    x <- waterfall_Custom2anno(x, label_col)
  } else {
    stop("Unrecognized file_type: ", file_type)
  }
  
  # drop unused levels in x
  x$sample <- as.factor(x$sample)
  x$gene <- as.factor(x$gene)
  x$trv_type <- as.factor(x$trv_type)
  x <- droplevels(x)
  
  # Check input data to clinDat
  if(!is.null(y))
  {
    if(!is.data.frame(y))
    {
      stop("Did not detect a data frame for input to clinDat")
    }
    y <- droplevels(y)
    
    if(!all(c('sample', 'variable', 'value') %in% colnames(y)))
    {
      stop("Did not detect correct sample names in clinDat")
    }
    
    # make sure clinical data columns are of expected class
    y$sample <- as.factor(y$sample)
    y$variable <- as.factor(y$variable)
    y$value <- as.character(y$value)
  }
  
  # check input data to mutBurden
  if(!is.null(z))
  {
    if(!is.data.frame(z))
    {
      stop("Did not detect a data frame for input to mutBurden")
    }
    z <- droplevels(z)
    
    if(!all(c('sample', 'mut_burden') %in% colnames(z)))
    {
      stop("Did not detect correct sample names in mutBurden")
    }
    
    # Make sure mutation burden columns are of proper class
    z$sample <- as.factor(z$sample)
    z$mut_burden <- as.numeric(as.character(z$mut_burden))
  }
  
  return(list(x, y, z))
}
waterfall_MAF2anno <- function(x, label_col)
{
  # Check that correct column names are present and convert to internal format
  expec_col <- c('Tumor_Sample_Barcode', 'Hugo_Symbol',
                 'Variant_Classification')
  
  if(!is.null(label_col))
  {
    expec_col <- c(expec_col, label_col)
  }
  
  if(!all(expec_col %in% colnames(x)))
  {
    memo <- paste0("Did not detect correct column names, column names
                       should be: ", toString(expec_col))
    stop(memo)
  }
  
  x <- x[,c('Tumor_Sample_Barcode', 'Hugo_Symbol', 'Variant_Classification',
            label_col)]
  
  if(!is.null(label_col))
  {
    colnames(x) <- c('sample', 'gene', 'trv_type', 'label')
  } else {
    colnames(x) <- c('sample', 'gene', 'trv_type')
  }
  return(x)
}
waterfall_Custom2anno <- function(x, label_col)
{
  # message statement
  memo <- paste0("Detected \"Custom\" file_type flag, ",
                 "looking for correct column names...")
  message(memo)
  
  # define expected columns
  expec_col <- c("sample", "gene", "variant_class")
  if(!is.null(label_col))
  {
    expec_col <- c(expec_col, label_col)
  }
  
  # check expected columns are present
  if(!all(expec_col %in% colnames(x)))
  {
    memo <- paste0("Did not detect correct column names, column names
                       should be: ", toString(expec_col))
    stop(memo)
  }
  
  x <- x[,c('sample', 'gene', 'variant_class', label_col)]
  
  if(!is.null(label_col))
  {
    colnames(x) <- c('sample', 'gene', 'trv_type', 'label')
  } else {
    colnames(x) <- c('sample', 'gene', 'trv_type')
  }
  
  # if no silent mutations are present warn the user
  if(all(!toupper(x$trv_type) %in% toupper("silent")))
  {
    warning("Did not detect silent mutations in input, is this expected?")
  }
  return(x)
}
waterfall_sampSort <- function(x)
{
  # recast the data going from long format to wide format, values in this data
  # are counts of a mutation call
  wide_data <- reshape2::dcast(x, sample ~ gene, fun.aggregate = length,
                               value.var="trv_type")
  
  # apply a boolean function to convert the data frame values to 1's and 0's
  values <- wide_data[,-1, drop=FALSE]
  sample <- wide_data[,1]
  values <- data.frame(apply(values, 2,
                             function(x) as.numeric(as.logical(x))))
  wide_boolean <- cbind(sample, values)
  
  # reverse the columns so that genes with highest mutation's are listed first
  # (assumes gene_sort has been run on the data frame)
  wide_boolean <- wide_boolean[,c(1, rev(2:ncol(wide_boolean)))]
  
  # if there are any NA values present in a sample at the gene put that
  # remove that sample and save (at this stage it should be samples)
  if(any(grepl("^NA.$", colnames(wide_boolean))))
  {
    # Find which column has the NA header
    NA_index <- which(grepl("^NA.$", colnames(wide_boolean)))
    
    # Append NA column to end of data frame
    NA_gene <- wide_boolean[,NA_index]
    wide_boolean <- wide_boolean[,-NA_index]
    
    # Save copy and remove samples with no mutations,
    # these will be added to the end
    samp_no_mut <- wide_boolean[rowSums(wide_boolean[2:ncol(wide_boolean)]) == 0,]$sample
    samp_no_mut <- as.character(samp_no_mut)
    wide_boolean <- wide_boolean[!wide_boolean$sample %in% samp_no_mut,]
  } else {
    samp_no_mut <- NULL
  }
  
  # hiearchial sort on all column's (i.e. genes) such that samples are
  # rearranged if there is a mutation in that gene
  sample_order <- wide_boolean[do.call(order, as.list(-wide_boolean[2:ncol(wide_boolean)])),]$sample
  
  # Put those samples not in sample order in from the original levels of the
  # data (these are samples with no mutations)
  not_in <- as.character(levels(sample_order)[which(!levels(sample_order) %in% sample_order)])
  not_in <- not_in[!not_in %in% samp_no_mut]
  sample_order <- c(as.character(sample_order), as.character(not_in))
  
  # Put those samples with no mutations back in
  if(!is.null(samp_no_mut))
  {
    sample_order <- c(sample_order, samp_no_mut)
  }
  
  return(sample_order)
}
waterfall_buildMain <- function(data_frame, grid=TRUE, label_x=FALSE,
                                gene_label_size=8, file_type='MGI',
                                drop_mutation=FALSE, plot_x_title=TRUE,
                                plot_label=FALSE, plot_label_size=4,
                                plot_palette=NULL, layers=NULL,
                                plot_label_angle=0, user_x_label=NULL)
{
  # NOTE: scale_x_discret(drop=FALSE), added to ggplot2 call to ensure samples
  # remove by 'mutation_recurrence_subset' are still plotted as empty tiles
  
  # Define layers
  
  # grid overlay
  vertical_grid <- geom_vline(xintercept = seq(.5, nlevels(data_frame$sample),
                                               by=1),
                              linetype='solid', colour='grey80', size=.01)
  
  if(length(unique(data_frame$gene)) == 1)
  {
    horizontal_grid <- geom_blank()
  } else {
    horizontal_grid <- geom_hline(yintercept = seq(1.5, length(unique(data_frame$gene)),
                                                   by=1),
                                  linetype='solid', colour='grey80',
                                  size=.01)
  }
  
  # Declare the appropriate palette
  if(!is.null(plot_palette))
  {
    palette <- plot_palette
  } else if(toupper(file_type) == toupper('MGI')) {
    palette <- c('#4f00A8', '#A80100', '#CF5A59', '#A80079', '#BC2D94',
                 '#CF59AE', '#000000', '#006666', '#00A8A8', '#009933',
                 '#ace7b9', '#cdf0d5', '#59CF74', '#002AA8', '#5977CF',
                 '#F37812', '#F2B079', '#888811', '#FDF31C', '#8C8C8C')
  } else if(toupper(file_type) == toupper('MAF')) {
    palette <- c("grey", '#A80100', '#CF5A59', '#A80079', '#CF59AE', '#000000',
                 '#9159CF', '#4f00A8', '#59CF74', '#00A8A8', '#79F2F2',
                 '#006666', '#002AA8', '#5977CF', '#F37812', '#F2B079',
                 '#888811', '#FDF31C')
  } else if(toupper(file_type) == toupper('Custom')) {
    memo <- paste0("Defining a palette in main.pallete is recommended ",
                   "when file_type is set to \"Custom\", defaulting to ",
                   "a predefined palette with 20 levels")
    warning(memo)
    palette <- c('#4f00A8', '#A80100', '#CF5A59', '#A80079', '#BC2D94',
                 '#CF59AE', '#000000', '#006666', '#00A8A8', '#009933',
                 '#ace7b9', '#cdf0d5', '#59CF74', '#002AA8', '#5977CF',
                 '#F37812', '#F2B079', '#888811', '#FDF31C', '#8C8C8C')
  }
  
  # Create breaks specific and labels for specified file type
  if(toupper(file_type) == toupper('MGI'))
  {
    # Create Legend labels
    breaks <- c("nonsense", "frame_shift_del", "frame_shift_ins",
                "splice_site_del", "splice_site_ins", "splice_site",
                "nonstop", "in_frame_del", "in_frame_ins", "missense",
                "splice_region_del", "splice_region_ins",
                "splice_region", "5_prime_flanking_region",
                "3_prime_flanking_region", "3_prime_untranslated_region",
                "5_prime_untranslated_region", "rna", "intronic", "silent")
    labels <- c("Nonsense", "Frame Shift Deletion", "Frame Shift Insertion",
                "Splice Site Deletion", "Splice Site Insertion",
                "Splice Site", "Stop Loss", "In Frame Deletion",
                "In Frame Insertion", "Missense", "Splice Region Insertion",
                "Splice Region Deletion", "Splice Region",
                "5' Flank", "3' Flank", "3' UTR", "5' UTR", "RNA",
                "Intronic", "Silent")
  } else if(toupper(file_type) == toupper('MAF')) {
    # Create Legend Labels
    breaks <- c("Nonsense_Mutation", "Frame_Shift_Ins", "Frame_Shift_Del",
                "In_Frame_Ins", "In_Frame_Del", "Nonstop_Mutation",
                "Translation_Start_Site", "Splice_Site", "Missense_Mutation",
                "5\'Flank", "3\'Flank", "5\'UTR", "3\'UTR", "RNA", "Intron",
                "IGR", "Silent", "Targeted_Region")
    labels <- c("Nonsense", "Frame Shift Insertion", "Frame Shift Deletion",
                "In Frame Insertion", "In Frame Deletion", "Nonstop",
                "Translation Start Site", "Splice Site", "Missense",
                "5' Flank", "3' Flank", "5' UTR", "3' UTR", "RNA", "Intron",
                "Intergenic Region", "Silent", "Targeted Region")
  } else if(toupper(file_type) == toupper('Custom')) {
    breaks <- levels(unique(data_frame$trv_type))
    labels <- breaks
  }
  
  if(drop_mutation == TRUE)
  {
    # Create Legend
    legend <- scale_fill_manual(name="", values=palette,
                                breaks=breaks, labels=labels, drop=TRUE)
  } else if (drop_mutation == FALSE) {
    # Create Legend
    legend <- scale_fill_manual(name="", values=palette,
                                breaks=breaks, labels=labels, drop=FALSE)
  }
  
  # X Label
  x_label <- xlab(paste0(user_x_label, ' samples (n=', nlevels(data_frame$sample), ')' ))
  
  if(plot_label == TRUE)
  {
    label <- geom_text(data=data_frame,
                       mapping=aes_string(x='sample', y='gene',
                                          label='label'),
                       size=plot_label_size, colour='white',
                       angle=plot_label_angle)
  } else {
    label <- geom_blank()
  }
  
  # Theme, Boolean, if specified to plot x labels, define theme such that
  # labels are plotted
  if(label_x == TRUE & plot_x_title == TRUE)
  {
    theme <-  theme(axis.ticks=element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    panel.background=element_rect(fill='white',
                                                  colour='white'),
                    axis.text.x=element_text(size=1,angle=50, hjust=1),
                    axis.text.y=element_text(size=gene_label_size,
                                             colour='black', face='italic'),
                    axis.title.y=element_blank(),
                    axis.title.x=element_text(size=11),
                    legend.title=element_text(size=11),
                    plot.title=element_blank())
  } else if(label_x == FALSE & plot_x_title == TRUE) {
    theme <-  theme(axis.ticks=element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    panel.background=element_rect(fill='white',
                                                  colour='white'),
                    axis.text.x=element_blank(),
                    axis.text.y=element_text(size=gene_label_size,
                                             colour='black', face='italic'),
                    axis.title.y=element_blank(),
                    axis.title.x=element_text(size=11),
                    legend.title=element_text(size=11),
                    plot.title=element_blank(),
                    panel.border=element_rect(colour='grey80', fill=NA,
                                              size=.1),
                    legend.position=("right"))
  } else if(label_x == TRUE & plot_x_title == FALSE) {
    theme <-  theme(axis.ticks=element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    panel.background=element_rect(fill='white',
                                                  colour='white'),
                    axis.text.x=element_text(angle=50, hjust=1),
                    axis.text.y=element_text(size=gene_label_size,
                                             colour='black', face='italic'),
                    axis.title.y=element_blank(),
                    axis.title.x=element_text(size=11),
                    legend.title=element_text(size=11),
                    plot.title=element_blank())
  } else if(label_x == FALSE & plot_x_title == FALSE) {
    theme <-  theme(axis.ticks=element_blank(),
                    panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank(),
                    panel.background=element_rect(fill='white',
                                                  colour='white'),
                    axis.text.x=element_blank(),
                    axis.text.y=element_text(size=gene_label_size,
                                             colour='black', face='italic'),
                    axis.title.y=element_blank(),
                    axis.title.x=element_text(size=11),
                    legend.title=element_text(size=11),
                    plot.title=element_blank(),
                    panel.border=element_rect(colour='grey80', fill=NA,
                                              size=.1),
                    legend.position=("right"))
  }
  # additional parameters
  if(!is.null(layers))
  {
    layers <- layers
  } else {
    layers <- geom_blank()
  }
  
  # ggplot call
  if(grid == TRUE)
  {
    p1 <- ggplot(data_frame, aes(x=sample,y=gene)) +
      geom_tile(aes_string(fill='trv_type'), position="identity") +
      theme + legend + x_label + vertical_grid +
      horizontal_grid + scale_x_discrete(drop=FALSE) + label +
      layers
  } else {
    p1 <- ggplot(data_frame, aes(x=sample,y=gene)) +
      geom_tile(aes_string(fill='trv_type'), position="identity") +
      theme + legend + x_label +
      scale_x_discrete(drop=FALSE) + label + layers
  }
  
  return(p1)
}
waterfall <- function(x, clinData=NULL, clinLegCol=1, clinVarCol=NULL,
                      clinVarOrder=NULL, clinLayer=NULL, mutBurden=NULL,
                      mainRecurCutoff=0, mainGrid=TRUE,
                      mainXlabel=FALSE, main_geneLabSize=8,
                      coverageSpace=44100000, fileType='MAF', plotGenes=NULL,
                      plotSamples=NULL, mainDropMut=FALSE, rmvSilent=FALSE,
                      mainLabelCol=NULL, mainLabelSize=4,
                      mainPalette=NULL, sampRecurLayer=NULL,
                      mainLayer=NULL, mutBurdenLayer=NULL,
                      mainLabelAngle=0, variant_class_order=NULL, dataOut=FALSE,
                      plotMutBurden=TRUE, maxGenes=NULL,my_x_label=NULL,returnp="p1")
{
  # Perform data quality checks and conversions
  inputDat <- waterfall_qual(x, clinData, mutBurden, file_type=fileType,
                             label_col=mainLabelCol)
  data_frame <- inputDat[[1]]
  clinData <- inputDat[[2]]
  mutBurden <- inputDat[[3]]
  
  # Set flag if it is desirable to plot cell text
  if(!is.null(mainLabelCol))
  {
    main.plot_label_flag <- TRUE
  } else {
    main.plot_label_flag <- FALSE
  }
  
  # If it is requested subset the input data on a sample list
  if(!is.null(plotSamples))
  {
    data_frame <- waterfall_sampAlt(data_frame, plotSamples)
  }
  
  # add in a count of mutations at the sample level before anything is
  # stripped out and save for mutation recurrence plot
  data_frame2 <- waterfall_calcMutFreq(data_frame[,c('sample', 'gene',
                                                     'trv_type')])
  
  # Subset the data to remove silent mutations if specified
  if(rmvSilent==TRUE)
  {
    data_frame <- waterfall_rmvSilent(data_frame)
  }
  
  # Subset the data based on a vector of genes if supplied
  if(!is.null(plotGenes))
  {
    data_frame <- waterfall_geneAlt(data_frame, plotGenes)
  }
  
  # Remove trv_type that are not the most deleterious for a given gene/sample
  data_frame <- waterfall_hierarchyTRV(data_frame, fileType,
                                       variant_class_order)
  
  # Subset the data based on the recurrence of mutations at the gene level
  data_frame <- waterfall_geneRecurCutoff(data_frame, mainRecurCutoff)
  
  # Use the max genes parameter to limit the number of genes plotted
  # and then reorder genes based on the frequency of mutations
  gene_sorted <- waterfall_geneSort(data_frame)
  data_frame$gene <- factor(data_frame$gene, levels=gene_sorted)
  if(!is.null(maxGenes))
  {
    max_gene_list <- tail(gene_sorted, maxGenes)
    data_frame <- data_frame[data_frame$gene %in% max_gene_list,]
  }
  
  # reorder the samples based on hiearchial sort on ordered gene list
  sample_order <- waterfall_sampSort(data_frame)
  data_frame$sample <- factor(data_frame$sample, levels=sample_order)
  
  # Reorder the sample levels in data_frame2 to match the main plot's levels,
  # and then plot the top margin plot
  if(isTRUE(plotMutBurden))
  {
    if(!is.null(mutBurden))
    {
      if(!setequal(sample_order, mutBurden$sample))
      {
        stop("levels in the sample column of mutBurden does not match
             either: the samples given in x, or plotSamples")
      }
      
      mutBurden$sample <- factor(mutBurden$sample, levels=sample_order)
      p3 <- waterfall_buildMutBurden_B(mutBurden, layers=mutBurdenLayer)
    } else {
      data_frame2$sample <- factor(data_frame2$sample,
                                   levels=sample_order)
      p3 <- waterfall_buildMutBurden_A(data_frame2, coverageSpace,
                                       layers=mutBurdenLayer)
    }        
  } else {
    # create a blank ggplot object
    df <- data.frame()  
    p3 <- ggplot2::ggplot(df) + ggplot2::geom_point() +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
      ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                     axis.text.y=ggplot2::element_blank(),
                     axis.ticks.x=ggplot2::element_blank(),
                     axis.ticks.y=ggplot2::element_blank(),
                     panel.background=ggplot2::element_blank(),
                     panel.grid=ggplot2::element_blank())
  }
  
  
  # Plot the Left Bar Chart
  p2 <- waterfall_buildGenePrevelance(data_frame, layers=sampRecurLayer)
  
  # if there are any NA values in the data frame for a gene, give these NA
  # values a gene name so they are plotted properly
  data_frame <- waterfall_NA2gene(data_frame)
  
  # Plot the Heatmap
  if(is.null(clinData))
  {
    p1 <- waterfall_buildMain(data_frame, grid=mainGrid,
                              label_x=mainXlabel,
                              gene_label_size=main_geneLabSize,
                              file_type=fileType,
                              drop_mutation=mainDropMut,
                              plot_x_title=TRUE,
                              plot_label=main.plot_label_flag,
                              plot_label_size=mainLabelSize,
                              plot_palette=mainPalette, layers=mainLayer,
                              plot_label_angle=mainLabelAngle,user_x_label=my_x_label)
  } else if(!is.null(clinData)) {
    p1 <- waterfall_buildMain(data_frame, grid=mainGrid,
                              label_x=mainXlabel,
                              gene_label_size=main_geneLabSize,
                              file_type=fileType,
                              drop_mutation=mainDropMut,
                              plot_x_title=FALSE,
                              plot_label=main.plot_label_flag,
                              plot_label_size=mainLabelSize,
                              plot_palette=mainPalette, layers=mainLayer,
                              plot_label_angle=mainLabelAngle,user_x_label=my_x_label)
  }
#  return(list(p1=p1,p2=p2))
if (returnp=="p1") {
	return(p1)
} else if (returnp=="p2") {
	return(p2)
}
  
}
waterfall_hierarchyTRV <- function(x, file_type, variant_class_order)
{
  message("setting mutation hierarchy...")
  # if variant_class_order is null use predefined values
  if(is.null(variant_class_order))
  {
    if(toupper(file_type) == toupper('MGI'))
    {
      mutation_order <- c("nonsense", "frame_shift_del",
                          "frame_shift_ins", "splice_site_del",
                          "splice_site_ins", "splice_site",
                          "nonstop", "in_frame_del", "in_frame_ins",
                          "missense", "splice_region_del",
                          "splice_region_ins", "splice_region",
                          "5_prime_flanking_region",
                          "3_prime_flanking_region",
                          "3_prime_untranslated_region",
                          "5_prime_untranslated_region", "rna",
                          "intronic", "silent", NA)
    } else if(toupper(file_type) == toupper('MAF')) {
      mutation_order <- c("Nonsense_Mutation", "Frame_Shift_Ins",
                          "Frame_Shift_Del", "Translation_Start_Site",
                          "Splice_Site", "Nonstop_Mutation",
                          "In_Frame_Ins", "In_Frame_Del",
                          "Missense_Mutation", "5\'Flank",
                          "3\'Flank", "5\'UTR", "3\'UTR", "RNA", "Intron",
                          "IGR", "Silent", "Targeted_Region", NA)
    } else if(toupper(file_type) == toupper('Custom')) {
      memo <- paste0("Detected NULL in variant_class_order, ",
                     "this parameter is required if file_type is set ",
                     "to \"Custom\"")
      stop(memo)
    }
  } else {
    mutation_order <- variant_class_order
  }
  
  # Check that elements in trv_type are in the mutation order
  if(any(!x$trv_type %in% mutation_order))
  {
    memo <- paste0("Detected an invalid mutation type, valid values for ",
                   file_type, " are: ", toString(mutation_order))
    stop(memo)
  }
  # refactor the data frame
  x$trv_type <- factor(x$trv_type, levels=mutation_order)
  
  # sort the data frame so that the duplicated call will remove the
  # proper trv_type
  x <- x[order(x$sample, x$gene, x$trv_type),]
  
  # collapse the data on sample/gene
  x <- x[!duplicated(x[, c("sample", "gene")]), ]
  
  return(x)
}
waterfall_geneAlt <- function(x, genes)
{
  message("Removing genes not in: ", toString(genes))
  # Perform quality checks
  if(typeof(genes) != 'character' & class(genes) != 'character')
  {
    memo <- paste0("argument supplied to main.genes is not a character ",
                   "vector, attempting to coerce")
    warning(memo)
    genes <- as.character(genes)
  }
  
  if(!all(toupper(genes) %in% toupper(x$gene)))
  {
    memo <- paste0("genes supplied in main.genes contains an element not ",
                   "found in x or it's subsequent subsets")
    warning(memo)
  }
  genes <- c(genes, NA)
  x <- x[(toupper(x$gene) %in% toupper(genes)), ]
  
  return(x)
}
waterfall_geneRecurCutoff <- function(x, recurrence_cutoff)
{
  if(recurrence_cutoff != 0)
  {
    message("Performing recurrence cutoff...")
  }
  mutRecur <- plyr::count(unique(x), vars=c("gene"))
  mutRecur <- na.omit(mutRecur)
  mutRecur$prop <- mutRecur$freq/nlevels(x$sample)
  
  # If recurrence cutoff specified exceeds upper limit such that no plot
  # useful would be generated, reset recurrence cutoff
  maxRecur <- max(mutRecur$prop)
  if(maxRecur < recurrence_cutoff)
  {
    memo <- paste0("The recurrence cutoff specified exceeds the recurrence",
                   " seen in the data, resetting this value to equal max ",
                   "recurrence:", maxRecur)
    warning(memo)
    recurrence_cutoff <- maxRecur
  }
  
  gene_above_recur <- mutRecur[mutRecur$prop >= recurrence_cutoff,]$gene
  # add NA to the end of 'gene_above_recurrence' vector, allowing for all
  # samples having NA as a gene name to be retained in the subset below
  gene_above_recur <- c(as.character(gene_above_recur), NA)
  
  # subset the original data frame based on the following: keep gene if it is
  # in the gene vector in "mutation_recurrence_subset"
  x <- x[(x$gene %in% gene_above_recur), ]
  
  return(x)
}
waterfall_geneSort <- function(x)
{
  gene_mutation_table <- table(x[,c('gene', 'trv_type')])
  gene_order <- names(sort(rowSums(gene_mutation_table)))
  return(gene_order)
}
waterfall_buildMutBurden_A <- function(x, coverage_space, layers=NULL)
{
  # Add in mutations per MB calculation
  x$mutation_per_MB <-
    x$mutation_total/coverage_space * 1000000
  
  # Alter GGplot2 Theme
  theme <- theme(axis.ticks.x=element_blank(),
                 axis.text.x=element_blank(),
                 axis.title.x=element_blank(),
                 legend.title=element_text(size=14))
  
  # Add Legend
  legend <- scale_fill_manual(name="Translational Effect",
                              values=c("red", "blue"),
                              breaks=c("Synonymous", "Non Synonymous"),
                              drop=FALSE)
  
  # add y label
  y_label <- ylab('Mutations per MB')
  
  # additional parameters
  if(!is.null(layers))
  {
    layers <- layers
  } else {
    layers <- geom_blank()
  }
  
  # ggplot2 call
  p1 <- ggplot(x, aes_string(x='sample', y='mutation_per_MB',
                             fill='trv_type')) +
    geom_bar(stat='identity', alpha=.75, width=1) +
    theme_bw() + theme + y_label + legend + layers
  
  return(p1)
}
waterfall_buildGenePrevelance <- function(data_frame, layers=NULL)
{
  # Convert all silent mutations to Synonymous, and all else to non-synonymous
  data_frame$trv_type <- as.character(data_frame$trv_type)
  data_frame$trv_type[toupper(data_frame$trv_type) != toupper('silent')] <-
    'Non Synonymous'
  data_frame$trv_type[toupper(data_frame$trv_type) == toupper('silent')] <-
    'Synonymous'
  data_frame$trv_type <- factor(data_frame$trv_type,
                                levels=c('Synonymous', 'Non Synonymous'))
  
  # Define the number of samples for the Percentage calculation
  # (Note: to pass a variable outside of aes into aes it needs to be
  # defined again)
  total_number_sample <- nlevels(data_frame$sample)
  
  # Make a count column
  data_frame <- plyr::count(data_frame, c("gene", "trv_type"))
  data_frame$prop <- data_frame$freq/total_number_sample * 100
  
  # Define Theme and various other layers to be passed to ggplot
  theme <- theme(axis.text.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.title.y=element_blank(),
                 legend.position=('none'))
  y_limits <- ylim(100,0)
  y_label <- ylab('% Samples With Mutation')
  legend <- scale_fill_manual(name="Translational Effect",
                              values=c("red", "blue"),
                              breaks=c('Synonymous', 'Non Synonymous'),
                              drop=FALSE)
  
  if(!is.null(layers))
  {
    layers <- layers
  } else {
    layers <- geom_blank()
  }
  
  # Plotting call
  p1 <- ggplot(na.omit(data_frame),
               aes_string(x='gene', y='prop', fill='trv_type')) +
    geom_bar(position='stack', alpha=.75, width=1, stat='identity') +
    theme_bw() + coord_flip() + theme + y_label + y_limits + 
    legend + layers
  
  return(p1)
}
waterfall_NA2gene <- function(x)
{
  # Get The gene with the most Mutations and add the NA samples to that gene
  # (ensures that the NAs are added in as gene with most mutations will always
  # be plotted)
  top_gene <- na.omit(rev(x$gene))[1]
  x$gene <- replace(x$gene, is.na(x$gene), top_gene)
  return(x)
}
