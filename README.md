# Abstract

The statistical atlases published by the Census Bureau in the late 1800s utilized a number of novel methods for displaying data. In this paper, we examine the use of framed spine and mosaic plots used in two plates of the Statistical Atlas of 1874. We use forensic statistics to recreate the data using available census information, and then use that data to create framed charts using modern plotting methods. We then examine the effectiveness of the framed charts compared to other alternatives with a user study.

# Repository Structure

- [data](./data) contains the following folders:
    - [atlas-data-raw](./data/atlas-data-raw): code and data files for "Statistical Forensics" - reading data from the charts in the statistical atlas and re-creating numerical data using IPUMS results. IPUMS data cannot be released due to licensing agreements, so the code file dealing with that data will not work out of the box.
    - [atlas-data-clean](./data/atlas-data-clean): CSV files containing aggregated data used to re-create the framed plots in ggplot2. 
        - ages-ipums.csv: Aggregated population numbers from the IPUMS 1% microsample, by age
        - ages-sex-ipums.csv: Aggregated population numbers from the IPUMS 1% microsample, by age and sex
        - church_pixel.csv: pixel measurements of framed spine plots with denominational information
        - denominations-1874.csv: accommodation information by denomination and state/territory
        - occ2.csv: occupation information by gender and state
        - occ3.csv: expanded occupation information using reconstructed age/sex information from ages-sex-ipums.csv
        - px2.csv: pixel measurements of mosaic plots with occupation and sex information
    - [user-data-anon](./data/user-data-anon): CSV files containing anonymized data from amazon and reddit, along with demographic information
    - [study-setup](./data/study-setup): PlotLabels.csv file containing the key matching Qualtrics questions to actual images (images are in the [images/test-images](./images/test-images) folder in the main directory)
    - [Format_Data.R](./data/Format_Data.R): Script to anonymize user data, format cleaned atlas data, and read in all data needed to compile the paper. Outputs Processed_Data.Rdata.
    - Processed_Data.Rdata: Rdata file containing all data sets needed to compile the paper.
    
- [images](./images): Contains all plots generated in the process of designing the user study
    - [all-images](./images/all-images): Framed and unframed pie, spine, and mosaic charts for every state (generated using framed-plots-code)
    - [test-images](./images/test-images): Images actually used in the user study, with "A" marking estimated areas.
    - [framed-plots-code](./framed-plots-code): Code to create framed plots, generating images in all-images, as well as test images for all states (placed in all-test-images, if run)
    
- [writeup](./writeup): Contains the Rnw file and supporting image files which are compiled into the pdf of the paper.
