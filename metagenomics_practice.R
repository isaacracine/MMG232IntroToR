# Isaac Racine
# Metagenomics
# 25 Mar 2021
#------------------------------------------------

dat <- read.table("bracken_output.txt", sep = "\t", check.names = F, quote = "")

head(dat)

colnames(dat) <- c("OTU", "SRR12588591","SRR13200939","SRR13200940","SRR13200941",  "SRR13200945","SRR13867563", "taxonomy")

library(tidyr)

mat <- dat %>% separate(taxonomy, sep = ";", into = c("Kingdom", "Phylum", "Class","Order", "Family", "Genus", "Species"))
head(mat)

#install.packages("remotes")
#remotes::install_github("MadsAlbertsen/ampvis2")

library(ampvis2)


## Load in counts matrix
#otutable <- read.csv("outs/bracken_out.csv", check.names = F)

## Load in metadata
metadata <- read.csv("metadata.csv", check.names = F)

otu <- amp_load(mat, metadata)
otu


##heat map
amp_heatmap(otu)
amp_heatmap(otu,
            group_by = "Platform",
            tax_aggregate = "Species",
            tax_add = "Genus",
            tax_show = 5)

##boxplot
amp_boxplot(otu,
            group_by = "Platform",
            tax_aggregate = "Species",
            tax_add = "Genus",
            tax_show = 5)

##alpha diversity
alphas <- amp_alphadiv(otu)
head(alphas)


library(ggthemes)
library(ggplot2)

ggplot(alphas, aes(x = Sample, y = Shannon, color = Platform)) +
  geom_point(size = 2) + 
  geom_line(size = 0.8) + theme_clean() + theme(axis.text.x = element_text(angle = 90))

##ordination
amp_ordinate(otu,
             type="pca",
             transform = "hellinger",
             sample_color_by="Platform",
             sample_label_by= "Sample")

##filtering and subsetting
novaseq <- amp_subset_samples(otu,Platform %in% "Illumina NovaSeq 6000" )
bacteria <- amp_subset_samples(otu, Kingdom %in% "Bacteria")
    #bacteria command will not run???????????????????????????????

##phyloseq
#source('http://bioconductor.org/biocLite.R')
#BiocManager::install("phyloseq")
library(phyloseq)
## Create input files from ampis2 object
otumat <- otu$abund
taxa <- otu$tax
meta <- otu$metadata
taxmat <- as.matrix(taxa)

# Convert them to phyloseq format
OTU = otu_table(otumat, taxa_are_rows = TRUE)
TAX = tax_table(taxmat)
sampledata <- sample_data(metadata)
rownames(sampledata)<- metadata$Sample

# Create the phyloseq object
physeq <- merge_phyloseq(phyloseq(OTU, TAX), sampledata)
physeq


##MOre alpha diversity
plot_richness(physeq, measures=c("Shannon", "Simpson", "InvSimpson"), color="Platform")

##STAKCED bar chart
plot_bar(physeq, fill="Family")


## Filter to a mean depth of at least 10
physeq2 = filter_taxa(physeq, function(x) mean(x) > 15, TRUE)
physeq2

## Transform these counts to relative abundances (%)
physeq3 = transform_sample_counts(physeq2, function(x) x / sum(x) )
head(otu_table(physeq3))

species <- physeq3 %>% 
  tax_glom(taxrank = "Species", NArm=FALSE) %>%
  psmelt()

library(ggsci)
ggplot(species, aes(x = Sample, y = Abundance, fill = Species)) +
  geom_bar(stat = "identity") +
  xlab("Sample ID") + scale_fill_npg() + theme(axis.text.x = element_text(angle = 90))

## FILTERED STacked bar
#install.packages("magrittr")
library(magrittr)
library(dplyr)

ps_species <- physeq %>%
  tax_glom(taxrank = "Species") %>%                     # agglomerate at phylum level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
  dplyr::filter(Abundance >0.02) %>%                         # Filter out low abundance taxa
  dplyr::arrange(Species)                                      # Sort data frame alphabetically by Species

######CAN NOT FIND ABUNDANCE IN ps_species :))))))))))))))))))))))))))

head(ps_species)

# Transform to relative abundance
physeq_norm <- transform_sample_counts(physeq, function(x) 100 * x/sum(x))

# Compile taxa by Order (filtering out low abundance taxa)
ps_species <- physeq_norm  %>%
  tax_glom(taxrank = "Species") %>%                     # agglomerate taxa at order level
  psmelt()%>%                                        # Melt phyloseq object to long format for producing graphics with ggplot2
  dplyr::filter(Abundance > 1.0)  %>%                        # Filter out orders below 1% in each sample
  dplyr::arrange(desc(Species))

# Sum remaining taxa with a relative abundance < 1% and make a new dataframe
Remainders <- (ps_species) %>%
  dplyr::group_by(Sample,Platform) %>% 
  dplyr::summarise(Abundance = (100-sum(Abundance))) %>% 
  as.data.frame()

Remainders$Species<-"Species < 1%"


# Join dataframes
ps_barchart <- dplyr::full_join(ps_species,Remainders)


# Make your new plot
ggplot(ps_barchart, aes(x = Sample, y = Abundance, fill = Species)) +
  geom_bar(stat = "identity") +
  xlab("Sample ID") + scale_fill_npg() + theme(axis.text.x = element_text(angle = 90))

#############################################  
#-------------- Day 2 ---------------

#DIABIMMUNE Cohort

library(ampvis2)

## Load in counts matrix
counts <- read.csv("T1D_bracken_output.csv", check.names = F)

## Load in metadata
meta <- read.csv("T1D_metadata.csv", check.names = F)

t1d <- amp_load(counts, meta)
t1d

#-------------- Heatmap ---------------
#Starting with a heat map is a quick way to see if there are any major shifts in the top taxa between your groups of interest. We can look at it many different ways, such as just case vs control:
amp_heatmap(t1d,
            facet_by = "Case_Control",
            tax_aggregate = "Genus",
            tax_show = 20)

#Or adding in Gender as a feature, which is a common source of variability in microbiome studies:

amp_heatmap(t1d,
            group_by = "Gender",
            facet_by = "Case_Control",
            tax_aggregate = "Species",
            tax_add = "Genus",
            tax_show = 20)

library(ggsci)
library(ggthemes)
library(ggplot2)

heat_gender <- amp_heatmap(t1d,
                           group_by = "Gender",
                           facet_by = "Case_Control",
                           tax_aggregate = "Species",
                           tax_add = "Genus",
                           tax_show = 20)
heat_gender + ggtitle("Top 20 Species by Status and Gender") + theme_classic()

#-------------- Boxplot ---------------
#Much like a heat map, box plots can be a great way to start getting an overview of the top taxa in a data set, with the added component of intra-group variation. Let’s look at the same example as the last slide:
  
amp_boxplot(t1d,
            group_by = "Case_Control",
            tax_aggregate = "Species",
            tax_add = "Genus",
            tax_show = 20)

#Another common metric is the Firmicutes:Bacteroidetes ratio. This is a bit of a holdover from amplicon sequencing, when taxonomic resolution was lower, but there have been many studies indicating that it can be a useful marker, particularly in people with metabolic syndromes. Together they represent ~90% of gut flora. Using the subset taxa function, we can look at this ratio using a boxplot:

BF_ratio <- amp_subset_taxa(t1d, tax_vector = c("Bacteroidetes", "Firmicutes"))

amp_boxplot(BF_ratio,
            group_by = "Case_Control",
            tax_aggregate = "Phylum",
            tax_show = 20)

#-------------- Alpha Diversity ---------------
#We’ve already seen how the top 20 taxa look, but there are 6,988 unique Species detected across all of these samples. Often in microbiome studies, we need ways to look at the data compositionally. The first way we’re going to do this is looking at alpha diversity, or to see how

alpha <- amp_alphadiv(t1d)
head(alpha)


ggplot(alpha, aes(x =Case_Control , y = Shannon, color = Case_Control)) +
  geom_boxplot() + facet_wrap(~Gender) + theme_pander() + theme(axis.text.x = element_text(angle = 90))

#-------------- Testing Significance ---------------
#We can see that there are differences between our groups, but for your own research you will often need to test the significance of these shifts. A quick way to do paired tests like this (case vs control), is a simple T-test of the Alpha diversity scores, followed by a post hoc Tukey test. We will get into more later, but statistical testing of microbiome data itself (ex. your full counts matrix) needs to be treated in a different way due to sparsity & zero-inflation.

#The first thing we need to do is reshape the data frame in order to test all measures of alpha diversity at once:
  
library(tidyverse)

colnames(alpha)

alpha2 <- alpha[,c(1,3,4,17,18,19,20 )]
alpha3<- gather(alpha2, measure, value, -Sample_ID, -Case_Control, -Gender)

head(alpha3)

#Now we can use the rstatix package to perform our tests:

library(rstatix)

pairwise_case <- alpha3 %>%
  group_by(measure) %>%
  pairwise_t_test(value ~ Case_Control, p.adjust.method = "bonferroni")

pairwise_case

case_tukey <- alpha3 %>%
  group_by(measure) %>%
  tukey_hsd(value ~ Case_Control)

case_tukey

#-------------- Ordination ---------------
#Now that we have meaningful metadata, we can begin to use this in our ordinations. In addition to the PCA and PCoA included in ampvis2, there are several methods for constrained ordinations, such as CCA and RDA. These can be helpful if there is a feature that you think might be explanatory, such as T1D status in this study. Let’s start with a simple PCA for now so that we can see the difference in ordination

amp_ordinate(t1d,
             type="pca",
             transform = "hellinger",
             sample_color_by="T1D_Status",
             sample_label_by= "Gender")

#-------------- COnstrained Ordination ---------------
#There are several options, but as an example we will use Redundancy Analysis (RDA), which is like a constrained version of the PCA we just performed. There is more on RDA here, but in essence it is a method to summarize variation in a set of “response” variables based on hypothetical explanatory variables (in our case T1D status). It is an extension of a multiple linear regression, and attempts to identify variables (species and their abundances) that are “redundant” within our explanatory groups. Most simply, it’s a regression, filtered through PCA as dimensionality reduction, that identifies species that are most representative of each sample type.

amp_ordinate(t1d,
             type="RDA",
             transform = "hellinger",
             constrain = "T1D_Status",
             sample_color_by="T1D_Status",
             sample_label_by= "Gender",
             sample_colorframe = TRUE,
             sample_colorframe_label = "T1D_Status")

#-------------- Filtering by Kingdom ---------------
#As we’ve seen in our plots so far, Bacteria dominate the most abundant taxa. However, often other Kingdoms are of interest for specific functions. We can use the same subsetting command as before to generate new ampvis2 objects for each Kingdom and investigate them seperately:

table(t1d$tax$Kingdom)

bacteria <- amp_subset_taxa(t1d, tax_vector = "Bacteria")
viruses <- amp_subset_taxa(t1d, tax_vector = "Viruses")
archaea <- amp_subset_taxa(t1d, tax_vector = "Archaea")

#-------------- Archaea ---------------
amp_heatmap(archaea,
            group_by = "Gender",
            facet_by = "Case_Control",
            tax_aggregate = "Species",
            tax_add = "Genus",
            tax_show = 5)

amp_boxplot(archaea,
            group_by = "Case_Control",
            tax_aggregate = "Species",
            tax_add = "Genus",
            tax_show = 20)

#-------------- Cpre Microbiota ---------------
#Another type of analysis is to examine the “core” microbiome of different groups, i.e. what taxa are shared among all samples of a defined group. In this case, we can look at any Species that are core at a level of 80% of the reads in a sample.

amp_core(
  t1d,
  group_by="T1D_Status",
  core_pct = 80,
  margin_plots = "xy",
  margin_plot_values_size = 3,
  widths = c(5, 1),
  heights = c(1, 5)
)

#-------------- Ven Diagram of Core Taxa ---------------
#Another way to visualize this data is with a Venn Diagram, with the number of taxa (Species in this example) shared at a defined threshold.

amp_venn(
  t1d,
  group_by = "T1D_Status",
  cut_a = 0.1,
  cut_f = 80,
  text_size = 5,
  normalise = TRUE,
  detailed_output = FALSE
)

#-------------- Phyloseq ---------------
#We can again convert our ampvis2 object to phyloseq format in order to unlock those additional features. This can also be useful if you want to start using some of the other R packages from the resources page, such as the microbiome package.

library(phyloseq)
## Create input files from ampis2 object
otumat <- t1d$abund
taxa <- t1d$tax
meta <- t1d$metadata
taxmat <- as.matrix(taxa)

# Convert them to phyloseq format
OTU = otu_table(otumat, taxa_are_rows = TRUE)
TAX = tax_table(taxmat)
sampledata <- sample_data(meta)
rownames(sampledata)<- meta$Sample

# Create the phyloseq object
physeq <- merge_phyloseq(phyloseq(OTU, TAX), sampledata)

#-------------- Microbiome R package ---------------
#This package contains many useful features for statistical testing, including a tutorial for univariate testing of two groups like we have here. Feel free to explore, but here are a few quick examples of functions that we can immediately perform on our converted phyloseq object:

#Table of F/B Ratio we plotted earlier:
  
library(microbiome)

bfratio(physeq) ########ERRRRRROR########### wants int, get doub

library(DESeq2)

ds2 <- phyloseq_to_deseq2(physeq, ~ Case_Control)

# Run DESeq2 analysis (all taxa at once!)
dds <- DESeq(ds2)

# Investigate results
res <- results(dds)
summary(res)

deseq.results <- as.data.frame(res)
df <- deseq.results
df$taxon <- rownames(df)
df <- df %>% arrange(log2FoldChange, padj)

# Print the results; flitered and sorted by pvalue and effectsize
library(knitr)
df <- df %>% filter(pvalue < 0.05 & log2FoldChange > 1.5) %>%
  arrange(pvalue, log2FoldChange)
kable(df, digits = 5)

#-------------- Microbiome Explorer ---------------
#For the rest of the class we’re going to explore the Microbiome Explorer R Shiny app. This requires three input files:
  #Counts
  #Taxonomy
  #Metadata

#We can use the existing metadata file, and we simply need to export the objects we created while re-formatting for phyloseq:
  
write.csv(taxmat, "T1D_taxonomy.csv")
write.csv(otumat, "T1D_counts.csv")

#Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true") ## running this helps converting warning to error and complete package installation

#Installation:
#BiocManager::install("zoecastillo/microbiomeExplorer", 
 #                    ref = "master") ##########ERROR nono-zeroexit status

#Starting the Shiny App:
library(microbiomeExplorer)
runMicrobiomeExplorer()


#-------------- Day 3 ---------------
#In order to examine these results at the experimental level, we are going to load them into R. The two main files we are working with are going to be humann2_filtered_pathabundance.tsv and metaphlan2_counts.tsv:
  
library(tidyverse)
## Load HUMAnN2 results into R

# Load metadata file (same as last week)
df.meta <- read.csv("T1D_metadata.csv")
rownames(df.meta) <- df.meta$Sample_ID

# metaphlan2 counts
counts <- read_tsv("metaphlan2_counts.tsv")
counts <- as.data.frame(counts)

# Filtered and normalized pathway abundances
# Be careful to specify the column separation and comment.char variables

df.humann2 <- read.table('humann2_filtered_pathabundance.tsv',
                         row.names=1, header=T, sep='\t', 
                         comment.char='', quote='')

# Remove the suffix from the sample labels
names(df.humann2) <- gsub('_Abundance', '', names(df.humann2))

# Re-order the columns to that they correspond to the order of samples in df.meta
df.humann2 <- df.humann2[,row.names(df.meta)]


# Take a look at our current objects:

# taxonomy
DT::datatable(counts, options = list(pageLength = 20))
