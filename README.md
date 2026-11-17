# Interactive PDF-Parser Creator

This repository contains tools to assist in generating a parser to capture text data from a structured PDF report

### Description

The goal of a parser is to gather structured data from unstructured (or less structured) data. In this case, extracting text data from PDF 
files and returning it in a tabular data format for further analyses. This is intended to fit into a workflow where one frequently works with 
PDF reports with the exact same report format and fields, and is looking to quickly and automatically convert some or all of the fields from 
these PDF reports into an observation-level dataset. 

To achieve this, we construct a parser template. Assigning regions of the PDF (boxes in a report, signature lines, etc.) to what we expect to find there,
we can then extract what appears in that region across every PDF, and with some light cleaning (mostly stringr), and derived datas, extract insights.
Because this template uses page locations rather than text recognition, it is important that every page of a report has the same format. 

The process contains two components:

app.R - An interactive Shiny app for constructing a template from a copy of a PDF report and tuning capture parameters

TEMPLATE_PDF_PARSER.R - A template for writing a parsing script that uses the template and parameters from app.R 


# Getting Started

### Requirements:

1) This repository, downloaded or cloned from github

2) A sample pdf report of the format that you want to develop a parser for

3) Packages:
- shiny
- pdftools
- png
- zip
- tidyverse
*Running app.R will automatically attempt to install these packages if not present*

##  Template Construction:

Sourcing [app.R](app.R) will launch the interactive Shiny app in your browser (Change line 15 to view elsewhere). This begins the process of constructing a template for this report format.

1) Select your sample PDF report with the file browser. It will render in the main tab of the shiny app

2) Type the name of the field you are looking to extract. For example: date of birth, or last name. 

3) Click and drag on the image of your PDF report to completely capture the area where text for this field **could** appear. 

4) Click 'Add Field', and the field name and location data should populate in 'Saved Fields' under the viewer.

