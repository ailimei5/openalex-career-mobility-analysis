# Career Mobility and Scientific Impact: An OpenAlex Reproduction Study

## Overview

This undergraduate Computational Biology project examines the relationship between researchers' institutional mobility and scientific impact. Using publication records retrieved from OpenAlex, I developed an R workflow to reproduce and compare a figure from a published study.

## Dataset

The analysis included approximately 55,600 publications associated with 651 researchers, following data collection and filtering.

## Methods

- Retrieved publication data using the OpenAlex API.
- Processed nested JSON records and cleaned publication data using R.
- Ranked institutions based on citation information.
- Identified researchers' institutional affiliations and transitions.
- Calculated changes in institutional rank and normalized citation impact.
- Generated figures for comparison with the original study.

## Tools

R, OpenAlex API, httr2, jsonlite, dplyr, purrr, readr, tidyr, and stringr.

## Project Files

- `Download_OpenAlex.R` — Data retrieval and preparation.
- `Fig6_Analysis.R` — Mobility analysis and figure generation.

## Limitations

The original study used historical citation measures that were not directly available in the retrieved data. Current citation counts were therefore used as an approximation, which may affect comparisons with the original findings.

## Project Context

Completed as part of an undergraduate Computational Biology course at Concordia University.# openalex-career-mobility-analysis
Reproducing a published study of researcher career mobility and scientific impact using R and OpenAlex data
