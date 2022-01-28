---
geometry: margin=1in
month: "January"
year: "2022"
preamble: |
output:
  sa4ss::techreport_pdf:
    default
  bookdown::pdf_document2:
    keep_tex: true
lang: en
papersize: letter
---



<!--chapter:end:00a.Rmd-->

---
author:
  - name: Chantel R. Wetzel
    code: 1
    first: C
    middle: R
    family: Wetzel
  - name: Jim Hastie
    code: 1
    first: J
    middle: ''
    family: Hastie
author_list: Wetzel, C.R., J. Hastie
affiliation:
  - code: 1
    address: Northwest Fisheries Science Center, U.S. Department of Commerce, National
      Oceanic and Atmospheric Administration, National Marine Fisheries Service, 2725
      Montlake Boulevard East, Seattle, Washington 98112
address:
  - ^1^Northwest Fisheries Science Center, U.S. Department of Commerce, National Oceanic
    and Atmospheric Administration, National Marine Fisheries Service, 2725 Montlake
    Boulevard East, Seattle, Washington 98112
---

<!--chapter:end:00authors.Rmd-->

---
bibliography:
  - sa4ss.bib
---

<!--chapter:end:00bibliography.Rmd-->

---
title: Detailed summary of avaible data to support West Coast groundfish stock assessments in 2023
---

<!--chapter:end:00title.Rmd-->

\pagebreak
\pagenumbering{roman}
\setcounter{page}{1}

\renewcommand{\thetable}{\roman{table}}
\renewcommand{\thefigure}{\roman{figure}}


\setlength\parskip{0.5em plus 0.1em minus 0.2em}

<!--chapter:end:01a.Rmd-->


<!--chapter:end:01executive.Rmd-->

\pagebreak
\setlength{\parskip}{5mm plus1mm minus1mm}
\pagenumbering{arabic}
\setcounter{page}{1}
\renewcommand{\thefigure}{\arabic{figure}}
\renewcommand{\thetable}{\arabic{table}}
\setcounter{table}{0}
\setcounter{figure}{0}

<!--chapter:end:10a.Rmd-->

# Introduction

This document provides a detailed summary of available data that may be used to support assessments in 2023. In previous assessment prioritizations, data summaries have been available in a sheet in main Excel workbook where average sample summaries available across select years and data sources were detailed by species.  This summary attempts to provide the same data but in more detail.  

Data from Pacific Fisheries Information Network (PacFIN), Recreational Fisheries Information Network (RecFIN), and the Northwest Fisheries Science Center (NWFSC) West Coast Groundfish Bottom Trawl Survey (WCGBT) and Hook-and-Line (HKL) surveys are summarized.  Additional data, not summarized here, may be available to support stock assessments of specific species (e.g., California Collaborative Fisheries Research Program). 

Commercial data summaries were obtained and downloaded from PacFIN on November 22nd, 2021 via the BDS501 report. The number of sexed and unsexed fish, length samples, read ages, and otoliths samples by species, state, and year were summarized.  Otolith collection data was summarized using separately provided state sample summaries generally for more recent years and then combined with otolith sample records available in PacFIN (e.g, AGE_STRUCTURE_CODE) for earlier years. Otolith records were provide directly from the Washington Department of Fish and Wildlife (WDFW) for 2002-2021 and California Department of Fish and Wildlife (CDFW) for 1960 - 2021 (only samples from 1980 - 2021 are summarize). Oregon Department of Fish and Wildlife (ODFW) confirmed that otolith records in PacFIN were correct and recommended using these data directly. Summaries of otoliths from early data years (e.g., 1980 - 2001) for Oregon and Washington were based on PacFIN records. Summaries of otolith collections across all years were provided for completeness.  However, there are a number of reasons why otolith counts in early years (e.g., 1980) may not reflect actual otoliths that could be effectively aged (e.g., unable to locate, otolith degradation).

Recreational data summaries were obtained and downloaded from RecFIN on December 2, 2021 via the BDS501 report which includes data for the following range or years by state: Oregon 1999-2021, and California 2003-2021. Recreational sample data for Washington was provided directly by WDFW with data ranging from 2002 - 2021. The number of sexed and unsexed fish, length samples, and read ages samples by species, state, and available years were summarized from  each data source. Otolith collection counts by species and year were provided separately by each state: WDFW 2002-2021, ODFW 2001-2021, and CDFW 2016-2021. 

Finally, data collected by two surveys conducted by the NWFSC are summarized: the WCGBT and HKL survey.  Data collected during the 2021 sample season are not available yet but should be available for discussion during the next scheduled Assessment Prioritization agenda item at the June Pacific Fishery Management Council (the Council) meeting. Data collected between 2003-2019 by the NWFSC WCGBT survey and between 2004-2019 by the NWFSC HKL survey by species are summarized. Similar to the summaries provided for the commercial and recreational fisheries; sexed and unsexed fish, lengths, read ages, and otoliths collected are available by year.  Additionally, the number of tows (WCGBT survey) or the sites that observed each species by year are provided. 

In the 2020 Assessment Prioritization a summary of length compositions and indices from the [NWFSC WCGBT data](https://www.pcouncil.org/documents/2020/05/f-2-attachment-1-summary-of-the-nwfsc-west-coast-groundfish-bottom-trawl-survey-data-for-select-species-from-2003-2019.pdf/) were provided.  Since data from the 2021 sample season is not yet available for summary, no new length compositions or indices were calculate with the summaries from 2020 representing all available data (i.e., no 2020 sampling season due to COVID-19).  Depending upon data availability, similar data summaries could be provided at the June Council meeting.

<!-- =====================================================================
A brief summary of the NWFSC West Coast Groundfish Bottom Trawl (WCGBT) and NWFSC Hook & Line Surveys is presented here for data available from 2003 - `endyr`.  These analyses are meant to provide additional information and guidance for the 2022 assessment prioritization process.  The selection of the species from the WCGBTS was based on having an average of 20 or more positive tows by the survey per year.  

The indices were calculated using VAST and the biomass estimates were aggregated by a generalized approach using state boundaries based on the areas where observations were present. Future species specific assessments may select a more tailored approach for summarizing the NWFSC WCGBTS data by area.  

The length data also were expanded using a generalized stratification. The composition data were expanded using a design-based approach with stratas based on state latitudes with two depth stratas: 55 - 183 m and 183 - 549 m, for all species except for three.  The three exceptions were species with considerable biomass at depths greater than 549 m: Dover sole, longspine thornyhead, and shortspine thornyhead.  These three species had an additional depth strata that included deeper waters, 549 - 1280 m, for each state area. The expanded length composition data were summarized using either a 2 or 4 cm bin structure depending upon the range between maximum and minimum lengths observed within the survey data.  Species where the range between the maximum and minimum lengths observed by the survey were less than 60 cm, 2 cm data bins were used, and for species where the range was 60 cm or greater the data bins were set at 4 cm.  The generalized stratification and bin structure selected here provides a simple summary of the data that can be useful for decision making, but will likely differ from a species specific approach that would be selected in a future assessment.  
-->

\newpage



<!--chapter:end:11introduction.Rmd-->


# Arrowtooth flounder 
 The most recent assessment of Arrowtooth flounder was a update assessment conducted in 2017. Across available data, Arrowtooth flounder have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 54,741 length observations, 913 age readings, and 23,472 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 39,919 length observations, 763 age readings, and 17,250 otoliths that are available to be aged. In California, since 2000, a total of 7,507 length observations, 0 age readings, and 689 otoliths have been collected In Oregon, since 2000, a total of 19,320 length observations, 0 age readings, and 16,561 otoliths have been collected In Washington, since 2000, a total of 13,092 length observations, 763 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 52,254 length observations, 4,324 age readings, and 10,799 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 129 & 24 & 153 & 0 & 0\\
C & 2005 & 160 & 1 & 161 & 0 & 0\\
C & 2006 & 549 & 0 & 549 & 0 & 0\\
C & 2007 & 362 & 6 & 368 & 0 & 0\\
C & 2008 & 320 & 1 & 321 & 0 & 0\\
C & 2009 & 288 & 16 & 304 & 0 & 0\\
C & 2010 & 326 & 11 & 337 & 0 & 0\\
C & 2011 & 647 & 104 & 750 & 0 & 215\\
C & 2012 & 862 & 120 & 982 & 0 & 141\\
C & 2013 & 833 & 29 & 862 & 0 & 44\\
C & 2014 & 539 & 42 & 581 & 0 & 47\\
C & 2015 & 573 & 63 & 636 & 0 & 135\\
C & 2016 & 563 & 139 & 702 & 0 & 105\\
C & 2017 & 368 & 21 & 389 & 0 & 2\\
C & 2018 & 126 & 0 & 126 & 0 & 0\\
C & 2019 & 127 & 35 & 162 & 0 & 0\\
C & 2020 & 102 & 22 & 124 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1987 & 150 & 0 & 150 & 0 & 0\\
O & 1990 & 374 & 0 & 374 & 0 & 374\\
O & 1991 & 550 & 0 & 550 & 0 & 550\\
O & 1992 & 650 & 0 & 650 & 0 & 650\\
O & 2006 & 534 & 0 & 534 & 0 & 490\\
O & 2007 & 1561 & 0 & 1561 & 0 & 1231\\
O & 2008 & 1488 & 1 & 1489 & 0 & 1189\\
O & 2009 & 1419 & 2 & 1420 & 0 & 1269\\
O & 2010 & 2225 & 7 & 2232 & 0 & 1412\\
O & 2011 & 1893 & 0 & 1893 & 0 & 1863\\
O & 2012 & 1219 & 0 & 1219 & 0 & 1149\\
O & 2013 & 1025 & 4 & 1029 & 0 & 999\\
O & 2014 & 1259 & 1 & 1260 & 0 & 1230\\
O & 2015 & 1257 & 0 & 1257 & 0 & 1062\\
O & 2016 & 1384 & 0 & 1384 & 0 & 1258\\
O & 2017 & 1307 & 0 & 1307 & 0 & 1162\\
O & 2018 & 1228 & 3 & 1231 & 0 & 1061\\
O & 2019 & 1065 & 1 & 1066 & 0 & 838\\
O & 2020 & 438 & 0 & 438 & 0 & 348\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1986 & 950 & 0 & 950 & 0 & 847\\
W & 1987 & 1050 & 0 & 1050 & 0 & 995\\
W & 1988 & 800 & 0 & 800 & 0 & 729\\
W & 1989 & 850 & 0 & 850 & 0 & 778\\
W & 1990 & 600 & 0 & 600 & 0 & 599\\
W & 1991 & 1100 & 0 & 1100 & 0 & 550\\
W & 1992 & 849 & 1 & 850 & 0 & 0\\
W & 1993 & 900 & 0 & 900 & 0 & 0\\
W & 1994 & 1000 & 0 & 1000 & 0 & 0\\
W & 1995 & 1098 & 0 & 1098 & 0 & 0\\
W & 1996 & 900 & 0 & 900 & 0 & 0\\
W & 1997 & 895 & 5 & 900 & 0 & 0\\
W & 1998 & 999 & 2 & 1001 & 150 & 150\\
W & 1999 & 1098 & 1 & 1099 & 0 & 0\\
W & 2000 & 1050 & 0 & 1050 & 0 & 0\\
W & 2001 & 800 & 0 & 800 & 0 & 0\\
W & 2002 & 499 & 1 & 500 & 0 & 0\\
W & 2003 & 300 & 0 & 300 & 299 & 0\\
W & 2004 & 300 & 0 & 300 & 266 & 0\\
W & 2005 & 199 & 1 & 200 & 198 & 0\\
W & 2006 & 604 & 1 & 605 & 0 & 0\\
W & 2007 & 1050 & 0 & 1050 & 0 & 0\\
W & 2008 & 900 & 0 & 900 & 0 & 0\\
W & 2009 & 1365 & 0 & 1365 & 0 & 0\\
W & 2010 & 833 & 0 & 833 & 0 & 0\\
W & 2011 & 899 & 0 & 899 & 0 & 0\\
W & 2012 & 1098 & 2 & 1100 & 0 & 0\\
W & 2013 & 500 & 0 & 500 & 0 & 0\\
W & 2014 & 600 & 0 & 600 & 0 & 0\\
W & 2015 & 599 & 1 & 600 & 0 & 0\\
W & 2016 & 177 & 25 & 202 & 0 & 0\\
W & 2017 & 250 & 0 & 250 & 0 & 0\\
W & 2018 & 703 & 0 & 703 & 0 & 0\\
W & 2019 & 259 & 0 & 259 & 0 & 0\\
W & 2020 & 76 & 0 & 76 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 191 & 4502 & 0 & 4502 & 512 & 857\\
2004 & 168 & 2713 & 0 & 2713 & 485 & 202\\
2005 & 215 & 3932 & 3 & 3935 & 853 & 4\\
2006 & 189 & 3030 & 6 & 3036 & 475 & 260\\
2007 & 227 & 3520 & 32 & 3552 & 0 & 894\\
2008 & 228 & 3221 & 6 & 3227 & 0 & 874\\
2009 & 232 & 3448 & 21 & 3469 & 0 & 956\\
2010 & 265 & 3697 & 6 & 3703 & 0 & 1134\\
2011 & 264 & 3057 & 3 & 3060 & 0 & 1043\\
2012 & 248 & 3027 & 18 & 3045 & 399 & 627\\
2013 & 184 & 2519 & 15 & 2534 & 400 & 285\\
2014 & 272 & 3537 & 124 & 3650 & 400 & 523\\
2015 & 277 & 3863 & 5 & 3851 & 400 & 567\\
2016 & 285 & 3257 & 0 & 3257 & 400 & 574\\
2017 & 266 & 2200 & 5 & 2205 & 0 & 862\\
2018 & 232 & 1761 & 0 & 1761 & 0 & 761\\
2019 & 123 & 754 & 0 & 754 & 0 & 376\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Aurora rockfish 
 The most recent assessment of Aurora rockfish was a full assessment conducted in 2013. Across available data, Aurora rockfish have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 42,495 length observations, 1,361 age readings, and 18,891 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 35,181 length observations, 1,361 age readings, and 16,925 otoliths that are available to be aged. In California, since 2000, a total of 21,606 length observations, 881 age readings, and 5,280 otoliths have been collected In Oregon, since 2000, a total of 12,395 length observations, 480 age readings, and 11,645 otoliths have been collected In Washington, since 2000, a total of 1,180 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 26,586 length observations, 3,089 age readings, and 8,334 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 34 & 1 & 35 & 0 & 33\\
C & 1981 & 83 & 0 & 83 & 0 & 39\\
C & 1982 & 94 & 0 & 94 & 0 & 43\\
C & 1983 & 542 & 0 & 542 & 0 & 524\\
C & 1984 & 415 & 0 & 415 & 0 & 414\\
C & 1985 & 791 & 7 & 798 & 0 & 847\\
C & 1986 & 574 & 1 & 575 & 0 & 66\\
C & 1987 & 178 & 1 & 179 & 0 & 0\\
C & 1988 & 215 & 3 & 218 & 0 & 0\\
C & 1989 & 231 & 34 & 265 & 0 & 0\\
C & 1990 & 282 & 204 & 486 & 0 & 0\\
C & 1991 & 115 & 1 & 116 & 0 & 0\\
C & 1992 & 105 & 264 & 369 & 0 & 0\\
C & 1993 & 158 & 86 & 244 & 0 & 0\\
C & 1994 & 343 & 78 & 421 & 0 & 0\\
C & 1995 & 441 & 48 & 489 & 0 & 0\\
C & 1996 & 421 & 350 & 771 & 0 & 0\\
C & 1997 & 330 & 224 & 554 & 0 & 0\\
C & 1998 & 235 & 61 & 296 & 0 & 0\\
C & 1999 & 233 & 77 & 310 & 0 & 0\\
C & 2000 & 245 & 40 & 285 & 0 & 22\\
C & 2001 & 246 & 254 & 489 & 0 & 0\\
C & 2002 & 1030 & 353 & 1352 & 0 & 620\\
C & 2003 & 1453 & 635 & 2075 & 497 & 413\\
C & 2004 & 337 & 169 & 482 & 0 & 235\\
C & 2005 & 682 & 279 & 960 & 0 & 382\\
C & 2006 & 725 & 608 & 1330 & 0 & 381\\
C & 2007 & 745 & 186 & 930 & 0 & 240\\
C & 2008 & 1493 & 152 & 1645 & 229 & 0\\
C & 2009 & 1087 & 268 & 1355 & 155 & 196\\
C & 2010 & 780 & 188 & 942 & 0 & 69\\
C & 2011 & 1428 & 1270 & 2145 & 0 & 795\\
C & 2012 & 939 & 1234 & 1459 & 0 & 744\\
C & 2013 & 431 & 890 & 734 & 0 & 364\\
C & 2014 & 178 & 954 & 533 & 0 & 115\\
C & 2015 & 550 & 1267 & 879 & 0 & 458\\
C & 2016 & 524 & 1205 & 1276 & 0 & 161\\
C & 2017 & 1276 & 526 & 1495 & 0 & 63\\
C & 2018 & 509 & 94 & 603 & 0 & 22\\
C & 2019 & 380 & 141 & 437 & 0 & 0\\
C & 2020 & 162 & 38 & 200 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 154 & 0 & 154 & 0 & 114\\
O & 2003 & 90 & 0 & 90 & 85 & 3\\
O & 2004 & 260 & 0 & 260 & 0 & 258\\
O & 2005 & 176 & 0 & 176 & 0 & 176\\
O & 2006 & 316 & 0 & 316 & 0 & 301\\
O & 2007 & 765 & 0 & 765 & 0 & 713\\
O & 2008 & 621 & 0 & 621 & 196 & 423\\
O & 2009 & 754 & 0 & 754 & 199 & 551\\
O & 2010 & 591 & 2 & 593 & 0 & 536\\
O & 2011 & 1282 & 0 & 1282 & 0 & 1272\\
O & 2012 & 1265 & 1 & 1266 & 0 & 1238\\
O & 2013 & 1154 & 0 & 1154 & 0 & 1113\\
O & 2014 & 1251 & 0 & 1251 & 0 & 1251\\
O & 2015 & 834 & 0 & 834 & 0 & 834\\
O & 2016 & 783 & 0 & 783 & 0 & 783\\
O & 2017 & 853 & 0 & 853 & 0 & 853\\
O & 2018 & 755 & 1 & 756 & 0 & 741\\
O & 2019 & 390 & 0 & 390 & 0 & 388\\
O & 2020 & 96 & 1 & 97 & 0 & 97\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 10 & 10 & 0 & 0\\
W & 1997 & 2 & 16 & 18 & 0 & 0\\
W & 1998 & 13 & 3 & 16 & 0 & 0\\
W & 1999 & 7 & 3 & 10 & 0 & 0\\
W & 2000 & 3 & 10 & 13 & 0 & 0\\
W & 2001 & 11 & 1 & 12 & 0 & 0\\
W & 2002 & 11 & 0 & 11 & 0 & 0\\
W & 2003 & 76 & 7 & 83 & 0 & 0\\
W & 2004 & 118 & 0 & 118 & 0 & 0\\
W & 2005 & 51 & 0 & 51 & 0 & 0\\
W & 2006 & 30 & 0 & 30 & 0 & 0\\
W & 2007 & 15 & 0 & 15 & 0 & 0\\
W & 2008 & 18 & 0 & 18 & 0 & 0\\
W & 2009 & 36 & 4 & 40 & 0 & 0\\
W & 2010 & 16 & 0 & 16 & 0 & 0\\
W & 2011 & 134 & 0 & 134 & 0 & 0\\
W & 2012 & 180 & 6 & 186 & 0 & 0\\
W & 2013 & 24 & 0 & 24 & 0 & 0\\
W & 2014 & 73 & 5 & 78 & 0 & 0\\
W & 2015 & 101 & 5 & 106 & 0 & 0\\
W & 2016 & 31 & 1 & 32 & 0 & 0\\
W & 2017 & 53 & 0 & 53 & 0 & 0\\
W & 2018 & 101 & 1 & 102 & 0 & 0\\
W & 2019 & 57 & 0 & 57 & 0 & 0\\
W & 2020 & 1 & 0 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 67 & 1105 & 15 & 1120 & 404 & 155\\
2004 & 55 & 1087 & 0 & 1087 & 0 & 354\\
2005 & 89 & 1669 & 16 & 1685 & 428 & 139\\
2006 & 87 & 1713 & 3 & 1716 & 0 & 599\\
2007 & 92 & 1679 & 15 & 1694 & 395 & 191\\
2008 & 118 & 1702 & 6 & 1708 & 0 & 706\\
2009 & 87 & 1889 & 8 & 1897 & 403 & 184\\
2010 & 91 & 1605 & 29 & 1634 & 487 & 318\\
2011 & 94 & 1481 & 41 & 1522 & 502 & 282\\
2012 & 101 & 1659 & 25 & 1684 & 470 & 408\\
2013 & 58 & 849 & 2 & 851 & 0 & 515\\
2014 & 83 & 1500 & 8 & 1497 & 0 & 665\\
2015 & 89 & 2050 & 9 & 2059 & 0 & 793\\
2016 & 91 & 1947 & 26 & 1973 & 0 & 944\\
2017 & 90 & 2062 & 8 & 2070 & 0 & 843\\
2018 & 100 & 1756 & 3 & 1759 & 0 & 906\\
2019 & 36 & 629 & 1 & 630 & 0 & 332\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Bank rockfish 
 The most recent assessment of Bank rockfish was a data-limited assessment conducted in 2010. Across available data, Bank rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 48,625 length observations, 5,064 age readings, and 24,837 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 11,598 length observations, 0 age readings, and 4,675 otoliths that are available to be aged. In California, since 2000, a total of 10,052 length observations, 0 age readings, and 3,394 otoliths have been collected In Oregon, since 2000, a total of 1,318 length observations, 0 age readings, and 1,281 otoliths have been collected In Washington, since 2000, a total of 228 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 601 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 601 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 601 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 2,024 length observations, 0 age readings, and 1,295 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 2,532 length observations, 0 age readings, and 2,520 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 20 & 8 & 28 & 23 & 1\\
C & 1981 & 334 & 36 & 370 & 235 & 0\\
C & 1982 & 556 & 17 & 573 & 513 & 292\\
C & 1983 & 913 & 373 & 1283 & 0 & 226\\
C & 1984 & 1979 & 1337 & 3315 & 249 & 1592\\
C & 1985 & 3819 & 1989 & 5808 & 93 & 4231\\
C & 1986 & 5962 & 332 & 6294 & 457 & 2385\\
C & 1987 & 3237 & 391 & 3628 & 219 & 1017\\
C & 1988 & 1904 & 120 & 2024 & 486 & 1633\\
C & 1989 & 1486 & 62 & 1548 & 379 & 461\\
C & 1990 & 1610 & 64 & 1674 & 399 & 1260\\
C & 1991 & 1713 & 91 & 1804 & 405 & 1897\\
C & 1992 & 981 & 386 & 1367 & 6 & 1081\\
C & 1993 & 501 & 406 & 907 & 382 & 770\\
C & 1994 & 508 & 408 & 916 & 292 & 435\\
C & 1995 & 272 & 532 & 804 & 271 & 281\\
C & 1996 & 638 & 615 & 1253 & 238 & 606\\
C & 1997 & 1175 & 495 & 1670 & 415 & 1063\\
C & 1998 & 822 & 520 & 1342 & 2 & 671\\
C & 1999 & 342 & 77 & 419 & 0 & 260\\
C & 2000 & 392 & 243 & 635 & 0 & 140\\
C & 2001 & 469 & 788 & 1201 & 0 & 372\\
C & 2002 & 651 & 654 & 1300 & 0 & 510\\
C & 2003 & 748 & 146 & 821 & 0 & 573\\
C & 2004 & 455 & 279 & 731 & 0 & 368\\
C & 2005 & 112 & 114 & 226 & 0 & 79\\
C & 2006 & 103 & 131 & 234 & 0 & 127\\
C & 2007 & 132 & 98 & 230 & 0 & 100\\
C & 2008 & 347 & 452 & 799 & 0 & 250\\
C & 2009 & 122 & 237 & 359 & 0 & 58\\
C & 2010 & 210 & 64 & 274 & 0 & 81\\
C & 2011 & 82 & 144 & 225 & 0 & 74\\
C & 2012 & 66 & 301 & 367 & 0 & 63\\
C & 2013 & 177 & 240 & 412 & 0 & 176\\
C & 2014 & 49 & 120 & 169 & 0 & 46\\
C & 2015 & 148 & 272 & 420 & 0 & 134\\
C & 2016 & 84 & 484 & 567 & 0 & 81\\
C & 2017 & 141 & 186 & 327 & 0 & 126\\
C & 2018 & 36 & 282 & 318 & 0 & 36\\
C & 2019 & 140 & 68 & 208 & 0 & 0\\
C & 2020 & 55 & 174 & 229 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 2 & 0 & 2 & 0 & 2\\
O & 2004 & 21 & 0 & 21 & 0 & 21\\
O & 2005 & 26 & 0 & 26 & 0 & 26\\
O & 2006 & 8 & 0 & 8 & 0 & 8\\
O & 2007 & 61 & 0 & 61 & 0 & 59\\
O & 2008 & 132 & 0 & 131 & 0 & 105\\
O & 2009 & 43 & 0 & 43 & 0 & 43\\
O & 2010 & 44 & 0 & 44 & 0 & 43\\
O & 2011 & 65 & 0 & 65 & 0 & 65\\
O & 2012 & 42 & 0 & 42 & 0 & 42\\
O & 2013 & 97 & 0 & 97 & 0 & 97\\
O & 2014 & 91 & 0 & 91 & 0 & 90\\
O & 2015 & 61 & 0 & 61 & 0 & 61\\
O & 2016 & 102 & 0 & 102 & 0 & 102\\
O & 2017 & 92 & 0 & 92 & 0 & 86\\
O & 2018 & 159 & 0 & 159 & 0 & 158\\
O & 2019 & 145 & 0 & 145 & 0 & 145\\
O & 2020 & 128 & 0 & 128 & 0 & 128\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2004 & 2 & 0 & 2 & 0 & 0\\
W & 2011 & 2 & 0 & 2 & 0 & 0\\
W & 2014 & 2 & 0 & 2 & 0 & 0\\
W & 2015 & 4 & 0 & 4 & 0 & 0\\
W & 2016 & 2 & 0 & 2 & 0 & 0\\
W & 2017 & 1 & 0 & 1 & 0 & 0\\
W & 2019 & 193 & 0 & 193 & 0 & 0\\
W & 2020 & 22 & 0 & 22 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 1 & 1 & 0 & 0\\
C & 2004 & 0 & 50 & 50 & 0 & 0\\
C & 2005 & 0 & 38 & 38 & 0 & 0\\
C & 2006 & 0 & 3 & 3 & 0 & 0\\
C & 2007 & 0 & 19 & 19 & 0 & 0\\
C & 2008 & 0 & 13 & 13 & 0 & 0\\
C & 2009 & 0 & 11 & 11 & 0 & 0\\
C & 2010 & 0 & 10 & 10 & 0 & 0\\
C & 2011 & 0 & 51 & 51 & 0 & 0\\
C & 2012 & 0 & 19 & 19 & 0 & 0\\
C & 2013 & 0 & 27 & 27 & 0 & 0\\
C & 2014 & 0 & 30 & 30 & 0 & 0\\
C & 2015 & 0 & 193 & 193 & 0 & 0\\
C & 2016 & 0 & 95 & 95 & 0 & 0\\
C & 2017 & 0 & 19 & 19 & 0 & 0\\
C & 2018 & 0 & 6 & 6 & 0 & 0\\
C & 2019 & 0 & 16 & 16 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 4 & 6 & 0 & 6 & 0 & 4\\
2004 & 8 & 106 & 0 & 106 & 0 & 60\\
2005 & 11 & 70 & 26 & 96 & 0 & 50\\
2006 & 11 & 255 & 0 & 255 & 0 & 62\\
2007 & 14 & 59 & 16 & 75 & 0 & 55\\
2008 & 17 & 31 & 0 & 31 & 0 & 31\\
2009 & 12 & 82 & 2 & 84 & 0 & 74\\
2010 & 13 & 169 & 1 & 170 & 0 & 71\\
2011 & 16 & 134 & 4 & 138 & 0 & 52\\
2012 & 9 & 31 & 0 & 31 & 0 & 30\\
2013 & 14 & 32 & 0 & 32 & 0 & 32\\
2014 & 18 & 142 & 3 & 145 & 0 & 111\\
2015 & 18 & 60 & 0 & 60 & 0 & 60\\
2016 & 16 & 68 & 132 & 200 & 0 & 155\\
2017 & 14 & 39 & 0 & 39 & 0 & 39\\
2018 & 13 & 295 & 16 & 311 & 0 & 187\\
2019 & 6 & 243 & 2 & 245 & 0 & 222\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 8 & 12 & 0 & 12 & 0 & 12\\
2005 & 10 & 24 & 0 & 24 & 0 & 23\\
2006 & 12 & 49 & 0 & 49 & 0 & 49\\
2007 & 8 & 15 & 0 & 15 & 0 & 15\\
2008 & 10 & 40 & 0 & 39 & 0 & 40\\
2009 & 11 & 30 & 0 & 30 & 0 & 30\\
2010 & 13 & 75 & 0 & 75 & 0 & 75\\
2011 & 8 & 32 & 0 & 32 & 0 & 31\\
2012 & 16 & 25 & 0 & 25 & 0 & 25\\
2013 & 11 & 38 & 0 & 38 & 0 & 38\\
2014 & 30 & 132 & 1 & 133 & 0 & 131\\
2015 & 48 & 385 & 3 & 387 & 0 & 387\\
2016 & 43 & 312 & 1 & 312 & 0 & 312\\
2017 & 42 & 382 & 8 & 383 & 0 & 378\\
2018 & 55 & 560 & 5 & 560 & 0 & 560\\
2019 & 49 & 418 & 3 & 418 & 0 & 414\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Big skate 
 The most recent assessment of Big skate was a full assessment conducted in 2019. Across available data, Big skate have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 8,575 length observations, 654 age readings, and 1,086 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 8,488 length observations, 654 age readings, and 1,086 otoliths that are available to be aged. In California, since 2000, a total of 1,459 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 5,653 length observations, 654 age readings, and 960 otoliths have been collected In Washington, since 2000, a total of 1,376 length observations, 0 age readings, and 126 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 5,410 length observations, 1,034 age readings, and 112 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2007 & 1 & 0 & 1 & 0 & 0\\
C & 2009 & 32 & 10 & 32 & 0 & 0\\
C & 2010 & 8 & 0 & 8 & 0 & 0\\
C & 2011 & 2 & 0 & 2 & 0 & 0\\
C & 2012 & 43 & 0 & 43 & 0 & 0\\
C & 2013 & 201 & 6 & 207 & 0 & 0\\
C & 2014 & 217 & 1 & 218 & 0 & 0\\
C & 2015 & 237 & 0 & 237 & 0 & 0\\
C & 2016 & 181 & 0 & 181 & 0 & 0\\
C & 2017 & 239 & 0 & 239 & 0 & 0\\
C & 2018 & 157 & 0 & 157 & 0 & 0\\
C & 2019 & 98 & 0 & 98 & 0 & 0\\
C & 2020 & 33 & 3 & 36 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1995 & 55 & 0 & 55 & 0 & 0\\
O & 1996 & 8 & 0 & 8 & 0 & 0\\
O & 1997 & 14 & 0 & 14 & 0 & 0\\
O & 1998 & 2 & 0 & 2 & 0 & 0\\
O & 1999 & 8 & 0 & 8 & 0 & 0\\
O & 2001 & 43 & 0 & 43 & 0 & 0\\
O & 2002 & 199 & 0 & 199 & 0 & 0\\
O & 2003 & 202 & 0 & 202 & 0 & 0\\
O & 2004 & 27 & 0 & 27 & 0 & 0\\
O & 2005 & 123 & 0 & 123 & 0 & 0\\
O & 2006 & 310 & 0 & 310 & 0 & 0\\
O & 2007 & 127 & 0 & 127 & 23 & 29\\
O & 2008 & 95 & 0 & 94 & 80 & 0\\
O & 2009 & 235 & 0 & 235 & 87 & 0\\
O & 2010 & 187 & 0 & 186 & 103 & 3\\
O & 2011 & 419 & 0 & 419 & 202 & 0\\
O & 2012 & 477 & 0 & 477 & 120 & 7\\
O & 2013 & 254 & 0 & 252 & 0 & 0\\
O & 2014 & 237 & 0 & 237 & 0 & 0\\
O & 2015 & 412 & 0 & 411 & 0 & 0\\
O & 2016 & 444 & 0 & 444 & 0 & 0\\
O & 2017 & 668 & 0 & 668 & 0 & 57\\
O & 2018 & 554 & 0 & 553 & 39 & 423\\
O & 2019 & 550 & 0 & 550 & 0 & 394\\
O & 2020 & 96 & 0 & 96 & 0 & 47\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2004 & 15 & 0 & 15 & 0 & 14\\
W & 2005 & 87 & 0 & 87 & 0 & 0\\
W & 2006 & 191 & 1 & 192 & 0 & 0\\
W & 2007 & 173 & 0 & 173 & 0 & 0\\
W & 2008 & 94 & 0 & 94 & 0 & 0\\
W & 2009 & 18 & 0 & 18 & 0 & 0\\
W & 2010 & 15 & 0 & 15 & 0 & 0\\
W & 2011 & 14 & 0 & 9 & 0 & 0\\
W & 2012 & 38 & 0 & 38 & 0 & 0\\
W & 2013 & 168 & 0 & 168 & 0 & 0\\
W & 2014 & 249 & 0 & 249 & 0 & 0\\
W & 2015 & 8 & 2 & 10 & 0 & 0\\
W & 2016 & 107 & 0 & 107 & 0 & 0\\
W & 2017 & 56 & 0 & 56 & 0 & 0\\
W & 2018 & 121 & 0 & 121 & 0 & 112\\
W & 2019 & 20 & 0 & 20 & 0 & 0\\
W & 2020 & 4 & 0 & 4 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 60 & 195 & 2 & 197 & 0 & 0\\
2004 & 81 & 259 & 3 & 262 & 0 & 0\\
2005 & 99 & 328 & 0 & 328 & 0 & 0\\
2006 & 67 & 152 & 3 & 0 & 0 & 0\\
2007 & 76 & 191 & 1 & 95 & 0 & 0\\
2008 & 53 & 159 & 0 & 159 & 0 & 0\\
2009 & 82 & 302 & 3 & 305 & 230 & 0\\
2010 & 130 & 466 & 0 & 466 & 333 & 3\\
2011 & 99 & 360 & 0 & 360 & 0 & 0\\
2012 & 104 & 395 & 0 & 395 & 0 & 0\\
2013 & 84 & 316 & 0 & 316 & 0 & 0\\
2014 & 149 & 552 & 0 & 552 & 0 & 0\\
2015 & 135 & 549 & 0 & 546 & 0 & 0\\
2016 & 105 & 422 & 0 & 422 & 138 & 2\\
2017 & 125 & 496 & 0 & 496 & 164 & 23\\
2018 & 123 & 331 & 0 & 331 & 169 & 2\\
2019 & 56 & 180 & 0 & 180 & 0 & 82\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Black rockfish 
 The most recent assessment of Black rockfish was a full assessment conducted in 2015. Across available data, Black rockfish have been observed and sampled generally by both commercial and recreational fisheries.  
 
 Across all years of available data, commercial fisheries have collected a total of 116,789 length observations, 20,281 age readings, and 20,602 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 89,906 length observations, 11,041 age readings, and 19,376 otoliths that are available to be aged. In California, since 2000, a total of 12,240 length observations, 287 age readings, and 668 otoliths have been collected In Oregon, since 2000, a total of 77,573 length observations, 10,735 age readings, and 18,708 otoliths have been collected In Washington, since 2000, a total of 93 length observations, 19 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 404,735 length observations, 55,734 age readings, and 14,600 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 377,243 length observations, 47,291 age readings, and 14,230 otoliths that are available to be aged. In California, since 2003, a total of 130,782 length observations, 0 age readings, and 462 otoliths have been collected In Oregon, since 2003, a total of 197,304 length observations, 17,262 age readings, and 10,107 otoliths have been collected In Washington, since 2003, a total of 49,157 length observations, 30,029 age readings, and 3,661 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 121 & 11 & 132 & 28 & 71\\
C & 1981 & 130 & 0 & 130 & 129 & 130\\
C & 1982 & 368 & 1 & 368 & 16 & 261\\
C & 1983 & 283 & 0 & 283 & 0 & 288\\
C & 1984 & 233 & 0 & 233 & 226 & 226\\
C & 1985 & 188 & 0 & 188 & 144 & 172\\
C & 1986 & 27 & 0 & 27 & 0 & 0\\
C & 1987 & 184 & 0 & 184 & 0 & 0\\
C & 1988 & 125 & 0 & 125 & 0 & 0\\
C & 1989 & 80 & 0 & 80 & 0 & 0\\
C & 1990 & 5 & 0 & 5 & 0 & 0\\
C & 1991 & 36 & 0 & 36 & 0 & 0\\
C & 1992 & 138 & 875 & 1013 & 0 & 0\\
C & 1993 & 3 & 2410 & 2413 & 0 & 0\\
C & 1994 & 0 & 2836 & 2836 & 0 & 0\\
C & 1995 & 0 & 2145 & 2145 & 0 & 0\\
C & 1996 & 25 & 1953 & 1978 & 0 & 0\\
C & 1997 & 52 & 997 & 1049 & 0 & 0\\
C & 1998 & 6 & 300 & 306 & 0 & 0\\
C & 1999 & 25 & 2360 & 2385 & 0 & 0\\
C & 2000 & 25 & 599 & 596 & 0 & 0\\
C & 2001 & 47 & 952 & 999 & 32 & 32\\
C & 2002 & 27 & 574 & 601 & 13 & 13\\
C & 2003 & 19 & 123 & 142 & 19 & 19\\
C & 2004 & 9 & 264 & 266 & 9 & 9\\
C & 2005 & 1 & 219 & 220 & 1 & 1\\
C & 2006 & 0 & 641 & 641 & 0 & 0\\
C & 2007 & 37 & 531 & 559 & 28 & 28\\
C & 2008 & 0 & 290 & 283 & 0 & 0\\
C & 2009 & 136 & 554 & 683 & 96 & 97\\
C & 2010 & 0 & 174 & 174 & 0 & 0\\
C & 2011 & 44 & 308 & 349 & 44 & 44\\
C & 2012 & 44 & 652 & 695 & 44 & 44\\
C & 2013 & 1 & 620 & 590 & 1 & 1\\
C & 2014 & 0 & 1212 & 1172 & 0 & 0\\
C & 2015 & 12 & 1706 & 1712 & 0 & 12\\
C & 2016 & 9 & 915 & 924 & 0 & 9\\
C & 2017 & 0 & 478 & 478 & 0 & 0\\
C & 2018 & 39 & 547 & 568 & 0 & 39\\
C & 2019 & 55 & 137 & 89 & 0 & 320\\
C & 2020 & 498 & 1 & 499 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1985 & 355 & 0 & 355 & 0 & 0\\
O & 1992 & 203 & 0 & 203 & 143 & 0\\
O & 1994 & 41 & 0 & 41 & 41 & 0\\
O & 1995 & 434 & 5 & 439 & 0 & 0\\
O & 1996 & 228 & 0 & 228 & 0 & 0\\
O & 1997 & 441 & 0 & 441 & 0 & 28\\
O & 1998 & 381 & 0 & 381 & 194 & 50\\
O & 1999 & 152 & 0 & 152 & 0 & 0\\
O & 2000 & 603 & 0 & 603 & 287 & 5\\
O & 2001 & 1049 & 0 & 1049 & 205 & 0\\
O & 2002 & 1239 & 93 & 1332 & 316 & 14\\
O & 2003 & 1368 & 46 & 1414 & 489 & 8\\
O & 2004 & 3566 & 0 & 3566 & 453 & 162\\
O & 2005 & 2261 & 0 & 2261 & 310 & 1\\
O & 2006 & 4531 & 49 & 4580 & 772 & 30\\
O & 2007 & 3824 & 24 & 3843 & 635 & 13\\
O & 2008 & 2916 & 5 & 2921 & 619 & 53\\
O & 2009 & 2868 & 76 & 2944 & 839 & 47\\
O & 2010 & 4034 & 224 & 4257 & 864 & 151\\
O & 2011 & 4592 & 0 & 4590 & 880 & 500\\
O & 2012 & 3536 & 53 & 3583 & 789 & 738\\
O & 2013 & 4243 & 61 & 4304 & 440 & 1484\\
O & 2014 & 6819 & 57 & 6876 & 935 & 2298\\
O & 2015 & 6406 & 18 & 6424 & 951 & 1920\\
O & 2016 & 6293 & 11 & 6303 & 951 & 1894\\
O & 2017 & 5290 & 4 & 5294 & 0 & 2909\\
O & 2018 & 5072 & 22 & 5094 & 0 & 2976\\
O & 2019 & 4626 & 31 & 4657 & 0 & 2541\\
O & 2020 & 1665 & 13 & 1678 & 0 & 964\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 100 & 96 & 196 & 99 & 0\\
W & 1981 & 400 & 0 & 400 & 394 & 0\\
W & 1982 & 400 & 29 & 429 & 295 & 0\\
W & 1983 & 900 & 24 & 924 & 894 & 0\\
W & 1984 & 400 & 0 & 400 & 397 & 0\\
W & 1986 & 849 & 0 & 849 & 846 & 0\\
W & 1987 & 1122 & 2 & 1123 & 1121 & 0\\
W & 1988 & 524 & 0 & 524 & 515 & 0\\
W & 1989 & 524 & 0 & 524 & 521 & 0\\
W & 1990 & 349 & 0 & 349 & 349 & 0\\
W & 1991 & 777 & 25 & 802 & 801 & 0\\
W & 1992 & 573 & 2 & 575 & 575 & 0\\
W & 1993 & 665 & 1 & 666 & 664 & 0\\
W & 1994 & 444 & 0 & 444 & 443 & 0\\
W & 1995 & 406 & 1 & 406 & 405 & 0\\
W & 1997 & 0 & 31 & 31 & 0 & 0\\
W & 1998 & 85 & 0 & 85 & 0 & 0\\
W & 2000 & 3 & 0 & 3 & 0 & 0\\
W & 2001 & 0 & 1 & 1 & 0 & 0\\
W & 2002 & 50 & 0 & 50 & 0 & 0\\
W & 2003 & 3 & 0 & 3 & 0 & 0\\
W & 2004 & 14 & 1 & 15 & 0 & 0\\
W & 2005 & 1 & 0 & 1 & 0 & 0\\
W & 2006 & 20 & 0 & 20 & 19 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 37 & 37 & 0 & 0\\
C & 2004 & 0 & 3402 & 3402 & 0 & 0\\
C & 2005 & 0 & 5449 & 5449 & 0 & 0\\
C & 2006 & 0 & 5366 & 5366 & 0 & 0\\
C & 2007 & 0 & 6950 & 6950 & 0 & 0\\
C & 2008 & 0 & 8416 & 8416 & 0 & 0\\
C & 2009 & 2 & 11549 & 11551 & 0 & 0\\
C & 2010 & 0 & 5859 & 5859 & 0 & 0\\
C & 2011 & 0 & 8124 & 8124 & 0 & 0\\
C & 2012 & 1 & 9914 & 9915 & 0 & 0\\
C & 2013 & 20 & 17724 & 17743 & 0 & 0\\
C & 2014 & 0 & 12088 & 12088 & 0 & 0\\
C & 2015 & 1 & 11818 & 11818 & 0 & 0\\
C & 2016 & 0 & 9077 & 9077 & 0 & 0\\
C & 2017 & 0 & 5883 & 5883 & 0 & 0\\
C & 2018 & 1 & 5422 & 5423 & 0 & 212\\
C & 2019 & 1 & 3668 & 3669 & 0 & 250\\
C & 2020 & 0 & 12 & 12 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1999 & 3608 & 158 & 3766 & 0 & 0\\
O & 2000 & 4833 & 23 & 4856 & 0 & 0\\
O & 2001 & 3101 & 5671 & 8772 & 3086 & 71\\
O & 2002 & 3757 & 3712 & 7469 & 3463 & 294\\
O & 2003 & 3461 & 3637 & 7098 & 2230 & 1214\\
O & 2004 & 3261 & 2754 & 6015 & 2287 & 1020\\
O & 2005 & 3125 & 3954 & 7079 & 1787 & 1331\\
O & 2006 & 2173 & 7295 & 9468 & 2247 & 0\\
O & 2007 & 2052 & 10741 & 12793 & 1988 & 40\\
O & 2008 & 1 & 11764 & 11765 & 0 & 0\\
O & 2009 & 1093 & 11344 & 12437 & 969 & 113\\
O & 2010 & 1161 & 11901 & 13062 & 1177 & 0\\
O & 2011 & 1107 & 11750 & 12857 & 861 & 263\\
O & 2012 & 1067 & 12394 & 13461 & 543 & 544\\
O & 2013 & 1038 & 12652 & 13690 & 526 & 520\\
O & 2014 & 1083 & 11708 & 12791 & 549 & 543\\
O & 2015 & 1078 & 12784 & 13862 & 1074 & 0\\
O & 2016 & 1098 & 10596 & 11694 & 530 & 554\\
O & 2017 & 991 & 10233 & 11224 & 494 & 485\\
O & 2018 & 1203 & 11825 & 13028 & 0 & 1203\\
O & 2019 & 1154 & 11727 & 12881 & 0 & 1159\\
O & 2020 & 1110 & 989 & 2099 & 0 & 1118\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 1850 & 779 & 2629 & 1894 & 5\\
W & 2003 & 1853 & 470 & 2323 & 1841 & 12\\
W & 2004 & 1657 & 343 & 2000 & 1645 & 12\\
W & 2005 & 1664 & 563 & 2227 & 1653 & 11\\
W & 2006 & 1608 & 1244 & 2852 & 1484 & 16\\
W & 2007 & 2311 & 612 & 2923 & 2300 & 13\\
W & 2008 & 1987 & 505 & 2492 & 1920 & 69\\
W & 2009 & 1696 & 1034 & 2730 & 1671 & 16\\
W & 2010 & 1598 & 982 & 2580 & 1586 & 12\\
W & 2011 & 1181 & 1302 & 2483 & 1171 & 10\\
W & 2012 & 1263 & 1122 & 2385 & 1038 & 52\\
W & 2013 & 1767 & 1090 & 2857 & 1762 & 5\\
W & 2014 & 1608 & 678 & 2286 & 1564 & 25\\
W & 2015 & 2666 & 484 & 3150 & 2152 & 7\\
W & 2016 & 1732 & 439 & 2171 & 1713 & 20\\
W & 2017 & 2299 & 1047 & 3346 & 2254 & 9\\
W & 2018 & 1827 & 966 & 2793 & 1761 & 59\\
W & 2019 & 2552 & 1321 & 3873 & 2514 & 15\\
W & 2020 & 1374 & 75 & 1449 & 0 & 1374\\
W & 2021 & 1992 & 245 & 2237 & 0 & 1924\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Blackgill rockfish 
 The most recent assessment of Blackgill rockfish was a update assessment conducted in 2017. Across available data, Blackgill rockfish have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 58,511 length observations, 1,858 age readings, and 13,035 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 27,372 length observations, 1,309 age readings, and 10,256 otoliths that are available to be aged. In California, since 2000, a total of 20,058 length observations, 1,309 age readings, and 3,764 otoliths have been collected In Oregon, since 2000, a total of 6,622 length observations, 0 age readings, and 6,492 otoliths have been collected In Washington, since 2000, a total of 692 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 8,490 length observations, 1,948 age readings, and 4,053 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 36 & 4 & 40 & 0 & 30\\
C & 1981 & 33 & 149 & 182 & 0 & 33\\
C & 1982 & 157 & 0 & 156 & 17 & 39\\
C & 1983 & 270 & 367 & 637 & 185 & 227\\
C & 1984 & 343 & 794 & 1137 & 127 & 325\\
C & 1985 & 1751 & 1239 & 2987 & 86 & 1613\\
C & 1986 & 4216 & 937 & 5153 & 134 & 309\\
C & 1987 & 2961 & 548 & 3509 & 0 & 9\\
C & 1988 & 2327 & 312 & 2639 & 0 & 42\\
C & 1989 & 628 & 531 & 1159 & 0 & 0\\
C & 1990 & 953 & 65 & 1018 & 0 & 0\\
C & 1991 & 929 & 13 & 942 & 0 & 0\\
C & 1992 & 734 & 1631 & 2365 & 0 & 108\\
C & 1993 & 509 & 82 & 591 & 0 & 0\\
C & 1994 & 245 & 642 & 887 & 0 & 0\\
C & 1995 & 653 & 750 & 1403 & 0 & 23\\
C & 1996 & 778 & 1494 & 2272 & 0 & 0\\
C & 1997 & 749 & 1046 & 1795 & 0 & 0\\
C & 1998 & 695 & 482 & 1177 & 0 & 10\\
C & 1999 & 507 & 98 & 605 & 0 & 11\\
C & 2000 & 463 & 245 & 704 & 0 & 0\\
C & 2001 & 324 & 502 & 819 & 64 & 92\\
C & 2002 & 638 & 757 & 1342 & 169 & 225\\
C & 2003 & 335 & 788 & 1020 & 157 & 390\\
C & 2004 & 162 & 170 & 328 & 21 & 84\\
C & 2005 & 275 & 349 & 623 & 176 & 324\\
C & 2006 & 230 & 573 & 802 & 221 & 357\\
C & 2007 & 150 & 367 & 515 & 101 & 193\\
C & 2008 & 445 & 1140 & 1560 & 230 & 242\\
C & 2009 & 238 & 1165 & 1402 & 131 & 306\\
C & 2010 & 280 & 1007 & 1284 & 39 & 175\\
C & 2011 & 365 & 1017 & 1365 & 0 & 242\\
C & 2012 & 365 & 1022 & 1360 & 0 & 323\\
C & 2013 & 262 & 433 & 682 & 0 & 203\\
C & 2014 & 192 & 509 & 675 & 0 & 154\\
C & 2015 & 213 & 1143 & 1305 & 0 & 178\\
C & 2016 & 158 & 1445 & 1543 & 0 & 93\\
C & 2017 & 220 & 579 & 797 & 0 & 100\\
C & 2018 & 118 & 424 & 542 & 0 & 0\\
C & 2019 & 259 & 462 & 721 & 0 & 83\\
C & 2020 & 306 & 363 & 669 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1997 & 32 & 27 & 59 & 0 & 0\\
O & 2002 & 6 & 0 & 6 & 0 & 5\\
O & 2003 & 82 & 0 & 82 & 0 & 76\\
O & 2004 & 22 & 0 & 22 & 0 & 22\\
O & 2005 & 19 & 0 & 19 & 0 & 19\\
O & 2006 & 124 & 0 & 124 & 0 & 123\\
O & 2007 & 204 & 0 & 204 & 0 & 190\\
O & 2008 & 341 & 0 & 341 & 0 & 328\\
O & 2009 & 472 & 0 & 472 & 0 & 468\\
O & 2010 & 594 & 0 & 594 & 0 & 582\\
O & 2011 & 435 & 0 & 435 & 0 & 428\\
O & 2012 & 823 & 0 & 823 & 0 & 791\\
O & 2013 & 665 & 0 & 665 & 0 & 627\\
O & 2014 & 818 & 0 & 818 & 0 & 817\\
O & 2015 & 531 & 0 & 531 & 0 & 531\\
O & 2016 & 253 & 0 & 253 & 0 & 253\\
O & 2017 & 490 & 0 & 490 & 0 & 490\\
O & 2018 & 402 & 0 & 402 & 0 & 401\\
O & 2019 & 288 & 0 & 288 & 0 & 288\\
O & 2020 & 53 & 0 & 53 & 0 & 53\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 31 & 31 & 0 & 0\\
W & 1997 & 0 & 182 & 182 & 0 & 0\\
W & 1998 & 125 & 12 & 137 & 0 & 0\\
W & 1999 & 55 & 21 & 76 & 0 & 0\\
W & 2000 & 93 & 60 & 153 & 0 & 0\\
W & 2001 & 21 & 14 & 35 & 0 & 0\\
W & 2002 & 18 & 9 & 27 & 0 & 0\\
W & 2003 & 65 & 9 & 74 & 0 & 0\\
W & 2004 & 29 & 3 & 32 & 0 & 0\\
W & 2005 & 39 & 1 & 40 & 0 & 0\\
W & 2006 & 22 & 0 & 22 & 0 & 0\\
W & 2007 & 28 & 0 & 28 & 0 & 0\\
W & 2008 & 27 & 1 & 28 & 0 & 0\\
W & 2009 & 30 & 0 & 30 & 0 & 0\\
W & 2010 & 22 & 0 & 22 & 0 & 0\\
W & 2011 & 47 & 1 & 48 & 0 & 0\\
W & 2012 & 48 & 0 & 48 & 0 & 0\\
W & 2013 & 26 & 0 & 26 & 0 & 0\\
W & 2014 & 29 & 2 & 31 & 0 & 0\\
W & 2015 & 5 & 0 & 5 & 0 & 0\\
W & 2016 & 16 & 0 & 16 & 0 & 0\\
W & 2017 & 11 & 0 & 11 & 0 & 0\\
W & 2018 & 3 & 0 & 3 & 0 & 0\\
W & 2019 & 13 & 0 & 13 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 21 & 129 & 0 & 129 & 0 & 89\\
2004 & 20 & 449 & 0 & 449 & 0 & 175\\
2005 & 28 & 397 & 1 & 398 & 0 & 191\\
2006 & 35 & 750 & 2 & 752 & 0 & 200\\
2007 & 29 & 298 & 0 & 298 & 0 & 194\\
2008 & 44 & 339 & 0 & 339 & 0 & 243\\
2009 & 40 & 535 & 18 & 553 & 0 & 430\\
2010 & 43 & 514 & 8 & 522 & 0 & 461\\
2011 & 44 & 362 & 3 & 365 & 311 & 0\\
2012 & 43 & 503 & 5 & 508 & 419 & 0\\
2013 & 31 & 415 & 4 & 419 & 0 & 348\\
2014 & 38 & 847 & 7 & 854 & 638 & 0\\
2015 & 39 & 717 & 2 & 719 & 580 & 0\\
2016 & 34 & 379 & 0 & 379 & 0 & 361\\
2017 & 38 & 594 & 2 & 596 & 0 & 442\\
2018 & 44 & 968 & 2 & 970 & 0 & 693\\
2019 & 16 & 239 & 1 & 240 & 0 & 226\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Blue/Deacon rockfish 
 The most recent assessment of Blue/Deacon rockfish was a full assessment conducted in 2017. Across available data, Blue/Deacon rockfish have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC HKL survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 22,413 length observations, 2,297 age readings, and 2,453 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 10,853 length observations, 2,297 age readings, and 2,271 otoliths that are available to be aged. In California, since 2000, a total of 5,593 length observations, 119 age readings, and 673 otoliths have been collected In Oregon, since 2000, a total of 5,260 length observations, 2,178 age readings, and 1,598 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 239,895 length observations, 3,959 age readings, and 16,757 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 231,630 length observations, 3,959 age readings, and 14,625 otoliths that are available to be aged. In California, since 2003, a total of 146,007 length observations, 0 age readings, and 445 otoliths have been collected In Oregon, since 2003, a total of 78,685 length observations, 3,959 age readings, and 10,066 otoliths have been collected In Washington, since 2003, a total of 6,938 length observations, 0 age readings, and 4,114 otoliths have been collected 

Across all years of available data, the NWFSC HKL survey has collected a total of 675 length observations, 0 age readings, and 656 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 3 & 0 & 3 & 0 & 0\\
C & 1981 & 1 & 7 & 8 & 0 & 0\\
C & 1982 & 16 & 0 & 16 & 0 & 0\\
C & 1983 & 9 & 2 & 11 & 0 & 0\\
C & 1984 & 3 & 0 & 3 & 0 & 150\\
C & 1985 & 44 & 32 & 76 & 0 & 0\\
C & 1986 & 17 & 6 & 23 & 0 & 19\\
C & 1987 & 0 & 2 & 2 & 0 & 0\\
C & 1988 & 3 & 0 & 3 & 0 & 0\\
C & 1989 & 16 & 8 & 24 & 0 & 0\\
C & 1990 & 4 & 9 & 13 & 0 & 0\\
C & 1991 & 35 & 54 & 89 & 0 & 0\\
C & 1992 & 103 & 1205 & 1308 & 0 & 0\\
C & 1993 & 55 & 3640 & 3695 & 0 & 0\\
C & 1994 & 0 & 1833 & 1833 & 0 & 0\\
C & 1995 & 0 & 638 & 638 & 0 & 0\\
C & 1996 & 0 & 1103 & 1103 & 0 & 0\\
C & 1997 & 71 & 948 & 1019 & 0 & 0\\
C & 1998 & 32 & 511 & 543 & 0 & 0\\
C & 1999 & 0 & 1052 & 1052 & 0 & 0\\
C & 2000 & 0 & 128 & 126 & 0 & 0\\
C & 2001 & 0 & 135 & 135 & 0 & 0\\
C & 2002 & 3 & 260 & 258 & 0 & 3\\
C & 2003 & 0 & 41 & 41 & 0 & 0\\
C & 2004 & 0 & 202 & 110 & 0 & 0\\
C & 2005 & 32 & 141 & 173 & 0 & 32\\
C & 2006 & 0 & 146 & 140 & 0 & 0\\
C & 2007 & 4 & 304 & 308 & 0 & 27\\
C & 2008 & 0 & 157 & 154 & 0 & 0\\
C & 2009 & 2 & 185 & 185 & 0 & 2\\
C & 2010 & 0 & 95 & 95 & 0 & 0\\
C & 2011 & 70 & 289 & 359 & 67 & 59\\
C & 2012 & 48 & 778 & 816 & 50 & 25\\
C & 2013 & 0 & 485 & 482 & 0 & 0\\
C & 2014 & 0 & 338 & 310 & 0 & 0\\
C & 2015 & 2 & 816 & 807 & 2 & 1\\
C & 2016 & 1 & 233 & 233 & 0 & 1\\
C & 2017 & 0 & 193 & 193 & 0 & 0\\
C & 2018 & 0 & 121 & 121 & 0 & 0\\
C & 2019 & 116 & 292 & 291 & 0 & 523\\
C & 2020 & 198 & 58 & 256 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1992 & 64 & 0 & 64 & 0 & 13\\
O & 1999 & 13 & 0 & 13 & 0 & 0\\
O & 2000 & 243 & 0 & 243 & 32 & 0\\
O & 2001 & 97 & 0 & 97 & 0 & 0\\
O & 2002 & 78 & 0 & 78 & 0 & 15\\
O & 2003 & 172 & 0 & 172 & 56 & 8\\
O & 2004 & 227 & 0 & 227 & 0 & 81\\
O & 2005 & 166 & 3 & 169 & 32 & 0\\
O & 2006 & 183 & 3 & 186 & 0 & 54\\
O & 2007 & 375 & 0 & 375 & 159 & 1\\
O & 2008 & 128 & 0 & 128 & 0 & 75\\
O & 2009 & 163 & 2 & 165 & 145 & 5\\
O & 2010 & 425 & 2 & 427 & 0 & 349\\
O & 2011 & 531 & 41 & 572 & 265 & 200\\
O & 2012 & 478 & 18 & 496 & 0 & 440\\
O & 2013 & 672 & 3 & 673 & 637 & 12\\
O & 2014 & 624 & 1 & 625 & 592 & 14\\
O & 2015 & 265 & 2 & 267 & 260 & 4\\
O & 2016 & 68 & 0 & 68 & 0 & 68\\
O & 2017 & 45 & 0 & 45 & 0 & 45\\
O & 2018 & 65 & 0 & 65 & 0 & 65\\
O & 2019 & 147 & 0 & 147 & 0 & 138\\
O & 2020 & 35 & 0 & 35 & 0 & 24\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 0 & 21 & 21 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 284 & 284 & 0 & 0\\
C & 2004 & 0 & 9735 & 9734 & 0 & 0\\
C & 2005 & 1 & 10919 & 10920 & 0 & 0\\
C & 2006 & 0 & 14801 & 14801 & 0 & 0\\
C & 2007 & 0 & 10528 & 10528 & 0 & 0\\
C & 2008 & 0 & 8015 & 8015 & 0 & 0\\
C & 2009 & 1 & 4254 & 4254 & 0 & 0\\
C & 2010 & 0 & 4351 & 4351 & 0 & 0\\
C & 2011 & 0 & 4773 & 4773 & 0 & 0\\
C & 2012 & 0 & 5090 & 5087 & 0 & 0\\
C & 2013 & 3 & 9700 & 9702 & 0 & 0\\
C & 2014 & 1 & 10795 & 10796 & 0 & 0\\
C & 2015 & 0 & 12867 & 12857 & 0 & 0\\
C & 2016 & 4 & 9680 & 9683 & 0 & 0\\
C & 2017 & 3 & 10150 & 10150 & 0 & 0\\
C & 2018 & 3 & 9803 & 9803 & 0 & 255\\
C & 2019 & 4 & 10211 & 10214 & 0 & 190\\
C & 2020 & 0 & 55 & 55 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2008 & 66 & 0 & 66 & 65 & 0\\
O & 2009 & 78 & 0 & 78 & 77 & 0\\
O & 2010 & 64 & 1 & 65 & 65 & 0\\
O & 2011 & 58 & 4 & 62 & 62 & 0\\
O & 2012 & 116 & 5 & 121 & 119 & 0\\
O & 2013 & 88 & 3 & 91 & 91 & 0\\
O & 2014 & 132 & 0 & 132 & 132 & 0\\
O & 2015 & 124 & 2 & 126 & 125 & 0\\
O & 2016 & 98 & 1136 & 1234 & 0 & 98\\
O & 2017 & 53 & 816 & 869 & 0 & 53\\
O & 2018 & 77 & 607 & 684 & 0 & 77\\
O & 2019 & 146 & 969 & 1115 & 0 & 149\\
O & 2020 & 67 & 86 & 153 & 0 & 70\\
O & 1999 & 711 & 15 & 726 & 0 & 0\\
O & 2000 & 566 & 0 & 566 & 0 & 0\\
O & 2001 & 1359 & 2849 & 4208 & 0 & 1393\\
O & 2002 & 737 & 1944 & 2681 & 0 & 739\\
O & 2003 & 784 & 2547 & 3331 & 0 & 785\\
O & 2004 & 501 & 1769 & 2270 & 146 & 352\\
O & 2005 & 478 & 3061 & 3539 & 0 & 477\\
O & 2006 & 898 & 3475 & 4373 & 0 & 899\\
O & 2007 & 863 & 4170 & 5033 & 0 & 863\\
O & 2008 & 34 & 4804 & 4838 & 0 & 34\\
O & 2009 & 30 & 4351 & 4381 & 0 & 29\\
O & 2010 & 43 & 5302 & 5345 & 0 & 47\\
O & 2011 & 6 & 4498 & 4504 & 0 & 7\\
O & 2012 & 9 & 4980 & 4989 & 0 & 10\\
O & 2013 & 3 & 3973 & 3976 & 0 & 3\\
O & 2014 & 2 & 3266 & 3268 & 0 & 2\\
O & 2015 & 0 & 4057 & 4057 & 0 & 0\\
O & 2016 & 0 & 168 & 168 & 0 & 0\\
O & 2008 & 826 & 0 & 826 & 461 & 361\\
O & 2009 & 940 & 4 & 944 & 384 & 555\\
O & 2010 & 832 & 10 & 842 & 344 & 495\\
O & 2011 & 767 & 17 & 784 & 374 & 398\\
O & 2012 & 838 & 27 & 865 & 400 & 457\\
O & 2013 & 616 & 5 & 621 & 367 & 246\\
O & 2014 & 617 & 4 & 621 & 361 & 250\\
O & 2015 & 669 & 2 & 671 & 386 & 281\\
O & 2016 & 645 & 2555 & 3200 & 0 & 649\\
O & 2017 & 544 & 2526 & 3070 & 0 & 544\\
O & 2018 & 557 & 2097 & 2654 & 0 & 558\\
O & 2019 & 740 & 3140 & 3880 & 0 & 743\\
O & 2020 & 572 & 267 & 839 & 0 & 574\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2019 & 17 & 0 & 17 & 0 & 17\\
W & 2020 & 24 & 0 & 24 & 0 & 24\\
W & 2021 & 8 & 0 & 8 & 0 & 8\\
W & 2002 & 10 & 32 & 42 & 0 & \vphantom{1} 0\\
W & 2003 & 4 & 25 & 29 & 0 & \vphantom{1} 0\\
W & 2004 & 155 & 16 & 171 & 0 & \vphantom{1} 144\\
W & 2005 & 297 & 186 & 483 & 0 & \vphantom{1} 202\\
W & 2006 & 81 & 147 & 228 & 0 & \vphantom{1} 33\\
W & 2007 & 138 & 27 & 165 & 0 & \vphantom{1} 89\\
W & 2008 & 89 & 39 & 128 & 0 & \vphantom{1} 59\\
W & 2009 & 88 & 35 & 123 & 0 & \vphantom{1} 39\\
W & 2010 & 132 & 38 & 170 & 0 & \vphantom{1} 60\\
W & 2011 & 42 & 99 & 141 & 0 & \vphantom{1} 36\\
W & 2012 & 19 & 38 & 57 & 0 & \vphantom{1} 17\\
W & 2013 & 13 & 60 & 73 & 0 & \vphantom{1} 23\\
W & 2014 & 169 & 1 & 170 & 0 & \vphantom{1} 169\\
W & 2015 & 236 & 8 & 244 & 0 & \vphantom{1} 236\\
W & 2016 & 0 & 4 & 4 & 0 & \vphantom{1} 0\\
W & 2017 & 51 & 110 & 161 & 0 & \vphantom{1} 51\\
W & 2018 & 6 & 79 & 85 & 0 & \vphantom{1} 6\\
W & 2019 & 74 & 134 & 208 & 0 & \vphantom{1} 74\\
W & 2020 & 0 & 3 & 3 & 0 & \vphantom{1} 0\\
W & 2021 & 0 & 7 & 7 & 0 & \vphantom{1} 0\\
W & 2016 & 612 & 1 & 613 & 0 & 612\\
W & 2017 & 334 & 0 & 333 & 0 & 333\\
W & 2018 & 105 & 0 & 105 & 0 & 105\\
W & 2019 & 204 & 0 & 202 & 0 & 203\\
W & 2020 & 183 & 0 & 183 & 0 & 183\\
W & 2021 & 153 & 0 & 153 & 0 & 153\\
W & 2002 & 10 & 32 & 42 & 0 & 0\\
W & 2003 & 4 & 25 & 29 & 0 & 0\\
W & 2004 & 155 & 16 & 171 & 0 & 144\\
W & 2005 & 297 & 186 & 483 & 0 & 202\\
W & 2006 & 81 & 147 & 228 & 0 & 33\\
W & 2007 & 138 & 27 & 165 & 0 & 89\\
W & 2008 & 89 & 39 & 128 & 0 & 59\\
W & 2009 & 88 & 35 & 123 & 0 & 39\\
W & 2010 & 132 & 38 & 170 & 0 & 60\\
W & 2011 & 42 & 99 & 141 & 0 & 36\\
W & 2012 & 19 & 38 & 57 & 0 & 17\\
W & 2013 & 13 & 60 & 73 & 0 & 23\\
W & 2014 & 169 & 1 & 170 & 0 & 169\\
W & 2015 & 236 & 8 & 244 & 0 & 236\\
W & 2016 & 0 & 4 & 4 & 0 & 0\\
W & 2017 & 51 & 110 & 161 & 0 & 51\\
W & 2018 & 6 & 79 & 85 & 0 & 6\\
W & 2019 & 74 & 134 & 208 & 0 & 74\\
W & 2020 & 0 & 3 & 3 & 0 & 0\\
W & 2021 & 0 & 7 & 7 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 6 & 47 & 0 & 47 & 0 & 47\\
2005 & 9 & 64 & 1 & 64 & 0 & 65\\
2006 & 9 & 40 & 1 & 40 & 0 & 40\\
2007 & 5 & 21 & 0 & 21 & 0 & 21\\
2008 & 7 & 39 & 0 & 39 & 0 & 39\\
2009 & 6 & 17 & 0 & 17 & 0 & 15\\
2010 & 4 & 27 & 0 & 27 & 0 & 27\\
2011 & 5 & 36 & 0 & 36 & 0 & 36\\
2012 & 3 & 4 & 0 & 4 & 0 & 3\\
2013 & 6 & 10 & 1 & 10 & 0 & 10\\
2014 & 14 & 38 & 2 & 38 & 0 & 36\\
2015 & 16 & 61 & 1 & 61 & 0 & 60\\
2016 & 19 & 64 & 0 & 64 & 0 & 61\\
2017 & 14 & 92 & 0 & 92 & 0 & 89\\
2018 & 16 & 65 & 2 & 65 & 0 & 62\\
2019 & 15 & 48 & 2 & 50 & 0 & 45\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Bocaccio 
 The most recent assessment of Bocaccio was a update assessment conducted in 2017. Across available data, Bocaccio have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 76,324 length observations, 6,421 age readings, and 46,113 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 16,184 length observations, 181 age readings, and 4,805 otoliths that are available to be aged. In California, since 2000, a total of 10,975 length observations, 181 age readings, and 1,559 otoliths have been collected In Oregon, since 2000, a total of 3,356 length observations, 0 age readings, and 3,246 otoliths have been collected In Washington, since 2000, a total of 1,853 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 44,876 length observations, 0 age readings, and 343 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 44,672 length observations, 0 age readings, and 343 otoliths that are available to be aged. In California, since 2003, a total of 43,900 length observations, 0 age readings, and 103 otoliths have been collected In Oregon, since 2003, a total of 395 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 377 length observations, 0 age readings, and 240 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 8,878 length observations, 2,855 age readings, and 2,725 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 16,429 length observations, 0 age readings, and 11,983 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 1836 & 274 & 2107 & 252 & 1490\\
C & 1981 & 1427 & 266 & 1693 & 0 & 1341\\
C & 1982 & 2687 & 245 & 2930 & 0 & 1851\\
C & 1983 & 2862 & 536 & 3396 & 0 & 2478\\
C & 1984 & 2792 & 1209 & 4000 & 0 & 2577\\
C & 1985 & 2774 & 1317 & 4089 & 434 & 3210\\
C & 1986 & 4037 & 640 & 4677 & 506 & 3576\\
C & 1987 & 4231 & 370 & 4601 & 577 & 3396\\
C & 1988 & 2961 & 338 & 3299 & 416 & 3044\\
C & 1989 & 2737 & 453 & 3190 & 499 & 3309\\
C & 1990 & 3072 & 229 & 3301 & 254 & 2734\\
C & 1991 & 3066 & 348 & 3414 & 636 & 2879\\
C & 1992 & 3187 & 629 & 3812 & 554 & 3104\\
C & 1993 & 2123 & 1385 & 3508 & 639 & 1856\\
C & 1994 & 1110 & 1565 & 2675 & 630 & 1024\\
C & 1995 & 688 & 1030 & 1718 & 0 & 651\\
C & 1996 & 847 & 968 & 1815 & 0 & 831\\
C & 1997 & 787 & 946 & 1733 & 0 & 761\\
C & 1998 & 705 & 472 & 1177 & 0 & 631\\
C & 1999 & 715 & 56 & 771 & 598 & 448\\
C & 2000 & 212 & 70 & 282 & 0 & 135\\
C & 2001 & 619 & 282 & 901 & 0 & 331\\
C & 2002 & 306 & 169 & 475 & 0 & 219\\
C & 2003 & 2 & 0 & 2 & 0 & 2\\
C & 2004 & 195 & 5 & 200 & 181 & 122\\
C & 2005 & 14 & 5 & 19 & 0 & 17\\
C & 2006 & 35 & 45 & 80 & 0 & 16\\
C & 2007 & 33 & 43 & 76 & 0 & 29\\
C & 2008 & 50 & 21 & 71 & 0 & 17\\
C & 2009 & 17 & 57 & 74 & 0 & 14\\
C & 2010 & 16 & 52 & 68 & 0 & 16\\
C & 2011 & 2 & 68 & 70 & 0 & 15\\
C & 2012 & 123 & 233 & 356 & 0 & 75\\
C & 2013 & 51 & 382 & 433 & 0 & 18\\
C & 2014 & 46 & 445 & 459 & 0 & 28\\
C & 2015 & 232 & 709 & 940 & 0 & 225\\
C & 2016 & 353 & 623 & 955 & 0 & 156\\
C & 2017 & 454 & 663 & 1090 & 0 & 124\\
C & 2018 & 405 & 1222 & 1627 & 0 & 0\\
C & 2019 & 557 & 809 & 1366 & 0 & 0\\
C & 2020 & 612 & 819 & 1431 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1983 & 100 & 0 & 100 & 0 & 100\\
O & 1992 & 1 & 0 & 1 & 0 & 1\\
O & 1993 & 1 & 0 & 1 & 0 & 1\\
O & 1997 & 15 & 0 & 15 & 0 & 15\\
O & 1998 & 47 & 0 & 47 & 0 & 0\\
O & 1999 & 12 & 0 & 12 & 0 & 0\\
O & 2000 & 15 & 0 & 15 & 0 & 15\\
O & 2002 & 5 & 0 & 5 & 0 & 5\\
O & 2004 & 1 & 0 & 1 & 0 & 1\\
O & 2005 & 16 & 0 & 16 & 0 & 15\\
O & 2006 & 5 & 0 & 5 & 0 & 5\\
O & 2007 & 7 & 0 & 7 & 0 & 6\\
O & 2008 & 36 & 0 & 36 & 0 & 36\\
O & 2009 & 24 & 0 & 24 & 0 & 24\\
O & 2010 & 22 & 0 & 22 & 0 & 22\\
O & 2011 & 25 & 0 & 25 & 0 & 25\\
O & 2012 & 34 & 0 & 34 & 0 & 34\\
O & 2013 & 30 & 0 & 30 & 0 & 30\\
O & 2014 & 39 & 0 & 39 & 0 & 39\\
O & 2015 & 22 & 0 & 22 & 0 & 22\\
O & 2016 & 73 & 0 & 73 & 0 & 73\\
O & 2017 & 673 & 1 & 674 & 0 & 670\\
O & 2018 & 820 & 1 & 821 & 0 & 821\\
O & 2019 & 1095 & 0 & 1095 & 0 & 999\\
O & 2020 & 412 & 0 & 412 & 0 & 404\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 6 & 0 & 6 & 0 & 0\\
W & 1995 & 245 & 0 & 245 & 245 & 0\\
W & 1996 & 275 & 166 & 441 & 0 & 0\\
W & 1997 & 380 & 246 & 626 & 0 & 0\\
W & 1998 & 482 & 36 & 518 & 0 & 0\\
W & 1999 & 222 & 0 & 222 & 0 & 0\\
W & 2000 & 198 & 2 & 200 & 0 & 0\\
W & 2001 & 117 & 7 & 124 & 0 & 0\\
W & 2002 & 298 & 0 & 298 & 0 & 0\\
W & 2003 & 272 & 0 & 272 & 0 & 0\\
W & 2004 & 75 & 1 & 74 & 0 & 0\\
W & 2005 & 88 & 0 & 88 & 0 & 0\\
W & 2006 & 15 & 0 & 15 & 0 & 0\\
W & 2008 & 1 & 0 & 1 & 0 & 0\\
W & 2009 & 1 & 0 & 1 & 0 & 0\\
W & 2011 & 4 & 0 & 4 & 0 & 0\\
W & 2012 & 13 & 0 & 13 & 0 & 0\\
W & 2013 & 2 & 0 & 2 & 0 & 0\\
W & 2014 & 12 & 0 & 12 & 0 & 0\\
W & 2016 & 5 & 0 & 5 & 0 & 0\\
W & 2017 & 144 & 0 & 144 & 0 & 0\\
W & 2018 & 171 & 0 & 171 & 0 & 0\\
W & 2019 & 272 & 0 & 272 & 0 & 0\\
W & 2020 & 157 & 0 & 157 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 2 & 36 & 38 & 0 & 0\\
C & 2004 & 1 & 1029 & 1030 & 0 & 0\\
C & 2005 & 0 & 1783 & 1783 & 0 & 0\\
C & 2006 & 0 & 2271 & 2271 & 0 & 0\\
C & 2007 & 0 & 2450 & 2450 & 0 & 0\\
C & 2008 & 0 & 1987 & 1987 & 0 & 0\\
C & 2009 & 0 & 2336 & 2336 & 0 & 0\\
C & 2010 & 1 & 2116 & 2117 & 0 & 0\\
C & 2011 & 1 & 3479 & 3480 & 0 & 0\\
C & 2012 & 0 & 4336 & 4336 & 0 & 0\\
C & 2013 & 0 & 4676 & 4676 & 0 & 0\\
C & 2014 & 0 & 3330 & 3330 & 0 & 0\\
C & 2015 & 0 & 2772 & 2772 & 0 & 0\\
C & 2016 & 2 & 1771 & 1773 & 0 & 0\\
C & 2017 & 1 & 2567 & 2567 & 0 & 0\\
C & 2018 & 0 & 3083 & 3083 & 0 & 71\\
C & 2019 & 2 & 3730 & 3732 & 0 & 32\\
C & 2020 & 0 & 139 & 139 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 47 & 47 & 0 & 0\\
O & 2002 & 0 & 157 & 157 & 0 & 0\\
O & 2003 & 0 & 64 & 64 & 0 & 0\\
O & 2004 & 0 & 20 & 20 & 0 & 0\\
O & 2005 & 0 & 14 & 14 & 0 & 0\\
O & 2006 & 0 & 11 & 11 & 0 & 0\\
O & 2007 & 0 & 16 & 16 & 0 & 0\\
O & 2008 & 0 & 7 & 7 & 0 & 0\\
O & 2009 & 0 & 11 & 11 & 0 & 0\\
O & 2011 & 0 & 8 & 8 & 0 & 0\\
O & 2012 & 0 & 14 & 14 & 0 & 0\\
O & 2013 & 0 & 7 & 7 & 0 & 0\\
O & 2014 & 0 & 8 & 8 & 0 & 0\\
O & 2015 & 0 & 1 & 1 & 0 & 0\\
O & 2016 & 0 & 2 & 2 & 0 & 0\\
O & 2017 & 0 & 5 & 5 & 0 & 0\\
O & 2018 & 0 & 130 & 130 & 0 & 0\\
O & 2019 & 0 & 75 & 75 & 0 & 0\\
O & 2020 & 0 & 2 & 2 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2003 & 0 & 1 & 1 & 0 & 0\\
W & 2004 & 0 & 14 & 14 & 0 & 0\\
W & 2005 & 1 & 0 & 1 & 0 & 0\\
W & 2009 & 0 & 7 & 7 & 0 & 0\\
W & 2011 & 0 & 24 & 24 & 0 & 0\\
W & 2012 & 0 & 18 & 18 & 0 & 0\\
W & 2014 & 12 & 0 & 12 & 0 & 12\\
W & 2015 & 2 & 1 & 3 & 0 & 2\\
W & 2016 & 7 & 2 & 9 & 0 & 7\\
W & 2017 & 11 & 3 & 14 & 0 & 11\\
W & 2018 & 25 & 11 & 36 & 0 & 25\\
W & 2019 & 95 & 50 & 145 & 0 & 95\\
W & 2020 & 2 & 1 & 3 & 0 & 2\\
W & 2021 & 86 & 4 & 90 & 0 & 86\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 38 & 102 & 5 & 107 & 105 & 2\\
2004 & 29 & 480 & 0 & 480 & 209 & 0\\
2005 & 40 & 270 & 0 & 270 & 146 & 1\\
2006 & 38 & 262 & 0 & 262 & 127 & 8\\
2007 & 30 & 157 & 0 & 157 & 94 & 4\\
2008 & 25 & 109 & 1 & 110 & 85 & 0\\
2009 & 24 & 100 & 0 & 100 & 95 & 5\\
2010 & 37 & 162 & 110 & 272 & 188 & 1\\
2011 & 22 & 105 & 0 & 105 & 105 & 0\\
2012 & 48 & 816 & 1 & 817 & 513 & 3\\
2013 & 46 & 539 & 199 & 738 & 493 & 2\\
2014 & 97 & 995 & 25 & 1020 & 695 & 16\\
2015 & 58 & 687 & 9 & 696 & 0 & 383\\
2016 & 81 & 1338 & 109 & 1447 & 0 & 695\\
2017 & 74 & 952 & 0 & 952 & 0 & 621\\
2018 & 76 & 806 & 0 & 806 & 0 & 601\\
2019 & 42 & 539 & 0 & 539 & 0 & 383\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 59 & 780 & 1 & 780 & 0 & 725\\
2005 & 69 & 659 & 8 & 664 & 0 & 665\\
2006 & 73 & 732 & 5 & 730 & 0 & 733\\
2007 & 79 & 641 & 5 & 643 & 0 & 640\\
2008 & 91 & 653 & 0 & 652 & 0 & 649\\
2009 & 80 & 586 & 0 & 586 & 0 & 583\\
2010 & 64 & 268 & 4 & 271 & 0 & 268\\
2011 & 87 & 767 & 11 & 771 & 0 & 765\\
2012 & 103 & 1072 & 5 & 1072 & 0 & 1067\\
2013 & 95 & 1126 & 3 & 1127 & 0 & 1123\\
2014 & 138 & 1790 & 6 & 1789 & 0 & 1790\\
2015 & 159 & 1950 & 5 & 1947 & 0 & 806\\
2016 & 152 & 1255 & 22 & 1256 & 0 & 536\\
2017 & 161 & 1351 & 4 & 1349 & 0 & 554\\
2018 & 161 & 1420 & 4 & 1416 & 0 & 581\\
2019 & 165 & 1369 & 14 & 1376 & 0 & 498\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Brown rockfish 
 The most recent assessment of Brown rockfish was a data-moderate assessment conducted in 2013. Across available data, Brown rockfish have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 11,374 length observations, 0 age readings, and 543 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 4,711 length observations, 0 age readings, and 474 otoliths that are available to be aged. In California, since 2000, a total of 4,708 length observations, 0 age readings, and 472 otoliths have been collected In Oregon, since 2000, a total of 3 length observations, 0 age readings, and 2 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 72,668 length observations, 13 age readings, and 89 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 72,650 length observations, 13 age readings, and 89 otoliths that are available to be aged. In California, since 2003, a total of 72,284 length observations, 0 age readings, and 25 otoliths have been collected In Oregon, since 2003, a total of 310 length observations, 13 age readings, and 25 otoliths have been collected In Washington, since 2003, a total of 56 length observations, 0 age readings, and 39 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 524 length observations, 0 age readings, and 386 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 20 & 0 & 20 & 0 & 25\\
C & 1981 & 10 & 0 & 10 & 0 & 1\\
C & 1982 & 17 & 0 & 17 & 0 & 0\\
C & 1983 & 6 & 0 & 6 & 0 & 7\\
C & 1984 & 0 & 15 & 15 & 0 & 2\\
C & 1985 & 2 & 9 & 11 & 0 & 34\\
C & 1986 & 2 & 0 & 2 & 0 & 0\\
C & 1988 & 1 & 0 & 1 & 0 & 0\\
C & 1990 & 1 & 0 & 1 & 0 & 0\\
C & 1991 & 0 & 51 & 51 & 0 & 0\\
C & 1992 & 0 & 1852 & 1852 & 0 & 0\\
C & 1993 & 0 & 515 & 515 & 0 & 0\\
C & 1994 & 0 & 623 & 623 & 0 & 0\\
C & 1995 & 1 & 405 & 406 & 0 & 0\\
C & 1996 & 1 & 848 & 849 & 0 & 0\\
C & 1997 & 1 & 1115 & 1115 & 0 & 0\\
C & 1998 & 0 & 133 & 133 & 0 & 0\\
C & 1999 & 0 & 1036 & 1036 & 0 & 0\\
C & 2000 & 7 & 622 & 601 & 0 & 0\\
C & 2001 & 24 & 971 & 891 & 0 & 23\\
C & 2002 & 48 & 443 & 358 & 0 & 47\\
C & 2003 & 0 & 81 & 81 & 0 & 0\\
C & 2004 & 4 & 101 & 75 & 0 & 4\\
C & 2005 & 0 & 321 & 60 & 0 & 0\\
C & 2006 & 0 & 564 & 7 & 0 & 0\\
C & 2007 & 20 & 478 & 49 & 0 & 19\\
C & 2008 & 2 & 511 & 27 & 0 & 1\\
C & 2009 & 23 & 191 & 155 & 0 & 16\\
C & 2010 & 0 & 491 & 465 & 0 & 0\\
C & 2011 & 0 & 248 & 244 & 0 & 0\\
C & 2012 & 0 & 206 & 203 & 0 & 0\\
C & 2013 & 0 & 101 & 101 & 0 & 0\\
C & 2014 & 7 & 78 & 85 & 0 & 7\\
C & 2015 & 0 & 199 & 197 & 0 & 0\\
C & 2016 & 0 & 192 & 192 & 0 & 0\\
C & 2017 & 0 & 236 & 234 & 0 & 0\\
C & 2018 & 2 & 204 & 204 & 0 & 0\\
C & 2019 & 0 & 233 & 228 & 0 & 355\\
C & 2020 & 5 & 249 & 251 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2012 & 1 & 0 & 1 & 0 & 1\\
O & 2014 & 1 & 0 & 1 & 0 & 1\\
O & 2017 & 1 & 0 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 2 & 65 & 67 & 0 & 0\\
C & 2004 & 0 & 1794 & 1794 & 0 & 0\\
C & 2005 & 0 & 3841 & 3841 & 0 & 0\\
C & 2006 & 0 & 4910 & 4910 & 0 & 0\\
C & 2007 & 2 & 4158 & 4160 & 0 & 0\\
C & 2008 & 0 & 4113 & 4113 & 0 & 0\\
C & 2009 & 0 & 4572 & 4572 & 0 & 0\\
C & 2010 & 7 & 4805 & 4812 & 0 & 0\\
C & 2011 & 0 & 4390 & 4390 & 0 & 0\\
C & 2012 & 0 & 3913 & 3913 & 0 & 0\\
C & 2013 & 1 & 5534 & 5535 & 0 & 0\\
C & 2014 & 0 & 7824 & 7824 & 0 & 0\\
C & 2015 & 0 & 6151 & 6150 & 0 & 0\\
C & 2016 & 1 & 4840 & 4841 & 0 & 0\\
C & 2017 & 1 & 3691 & 3691 & 0 & 0\\
C & 2018 & 1 & 4031 & 4031 & 0 & 24\\
C & 2019 & 2 & 3622 & 3622 & 0 & 1\\
C & 2020 & 0 & 18 & 18 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 11 & 11 & 0 & 0\\
O & 2002 & 0 & 7 & 7 & 0 & 0\\
O & 2003 & 0 & 10 & 10 & 0 & 0\\
O & 2004 & 0 & 2 & 2 & 0 & 0\\
O & 2005 & 2 & 11 & 13 & 2 & 0\\
O & 2006 & 0 & 21 & 21 & 0 & 0\\
O & 2007 & 0 & 9 & 9 & 0 & 0\\
O & 2008 & 4 & 18 & 22 & 4 & 0\\
O & 2009 & 1 & 20 & 21 & 1 & 0\\
O & 2010 & 1 & 44 & 45 & 1 & 0\\
O & 2011 & 1 & 34 & 35 & 1 & 0\\
O & 2012 & 1 & 5 & 6 & 1 & 0\\
O & 2013 & 3 & 9 & 12 & 3 & 0\\
O & 2014 & 0 & 7 & 7 & 0 & 0\\
O & 2015 & 0 & 10 & 10 & 0 & 0\\
O & 2016 & 0 & 7 & 7 & 0 & 0\\
O & 2017 & 1 & 13 & 14 & 0 & 1\\
O & 2018 & 0 & 11 & 11 & 0 & 0\\
O & 2019 & 3 & 35 & 38 & 0 & 3\\
O & 2020 & 21 & 6 & 27 & 0 & 21\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2011 & 0 & 1 & 1 & 0 & 0\\
W & 2012 & 0 & 1 & 1 & 0 & 0\\
W & 2013 & 0 & 1 & 1 & 0 & 0\\
W & 2016 & 0 & 3 & 3 & 0 & 0\\
W & 2017 & 8 & 2 & 10 & 0 & 8\\
W & 2018 & 0 & 3 & 3 & 0 & 0\\
W & 2019 & 12 & 3 & 15 & 0 & 12\\
W & 2020 & 10 & 0 & 10 & 0 & 10\\
W & 2021 & 9 & 3 & 12 & 0 & 9\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 8 & 46 & 2 & 48 & 0 & 0\\
2004 & 6 & 28 & 0 & 28 & 0 & 28\\
2005 & 5 & 42 & 1 & 43 & 0 & 20\\
2006 & 4 & 13 & 0 & 13 & 0 & 13\\
2007 & 1 & 1 & 0 & 1 & 0 & 1\\
2008 & 2 & 3 & 1 & 4 & 0 & 4\\
2009 & 2 & 9 & 0 & 9 & 0 & 9\\
2010 & 3 & 15 & 0 & 15 & 0 & 14\\
2011 & 7 & 13 & 2 & 15 & 0 & 15\\
2012 & 8 & 39 & 0 & 39 & 0 & 29\\
2013 & 4 & 39 & 0 & 39 & 0 & 30\\
2014 & 5 & 83 & 0 & 83 & 0 & 37\\
2015 & 7 & 51 & 0 & 51 & 0 & 50\\
2016 & 6 & 7 & 0 & 7 & 0 & 7\\
2017 & 6 & 35 & 0 & 35 & 0 & 35\\
2018 & 12 & 62 & 0 & 62 & 0 & 62\\
2019 & 4 & 32 & 0 & 32 & 0 & 32\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Cabezon 
 The most recent assessment of Cabezon was a full assessment conducted in 2019. Across available data, Cabezon have been observed and sampled generally by both commercial and recreational fisheries.  
 
 Across all years of available data, commercial fisheries have collected a total of 28,821 length observations, 366 age readings, and 205 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 23,055 length observations, 366 age readings, and 205 otoliths that are available to be aged. In California, since 2000, a total of 7,534 length observations, 0 age readings, and 25 otoliths have been collected In Oregon, since 2000, a total of 15,521 length observations, 366 age readings, and 180 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 45,151 length observations, 2,328 age readings, and 2,795 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 43,287 length observations, 2,328 age readings, and 2,771 otoliths that are available to be aged. In California, since 2003, a total of 14,873 length observations, 0 age readings, and 8 otoliths have been collected In Oregon, since 2003, a total of 24,383 length observations, 2,328 age readings, and 812 otoliths have been collected In Washington, since 2003, a total of 4,031 length observations, 0 age readings, and 1,951 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1993 & 0 & 30 & 30 & 0 & 0\\
C & 1994 & 0 & 9 & 9 & 0 & 0\\
C & 1995 & 0 & 206 & 206 & 0 & 0\\
C & 1996 & 0 & 1696 & 1696 & 0 & 0\\
C & 1997 & 0 & 911 & 904 & 0 & 0\\
C & 1998 & 0 & 1345 & 1345 & 0 & 0\\
C & 1999 & 0 & 1479 & 1479 & 0 & 0\\
C & 2000 & 35 & 2511 & 2500 & 0 & 0\\
C & 2001 & 11 & 1082 & 1080 & 0 & 0\\
C & 2002 & 0 & 297 & 297 & 0 & 0\\
C & 2003 & 0 & 83 & 83 & 0 & 0\\
C & 2004 & 0 & 288 & 228 & 0 & 0\\
C & 2005 & 0 & 184 & 129 & 0 & 0\\
C & 2006 & 1 & 426 & 220 & 0 & 1\\
C & 2007 & 0 & 469 & 283 & 0 & 0\\
C & 2008 & 0 & 340 & 256 & 0 & 0\\
C & 2009 & 0 & 232 & 185 & 0 & 0\\
C & 2010 & 0 & 288 & 280 & 0 & 0\\
C & 2011 & 0 & 160 & 148 & 0 & 0\\
C & 2012 & 0 & 301 & 247 & 0 & 0\\
C & 2013 & 0 & 205 & 102 & 0 & 0\\
C & 2014 & 0 & 194 & 55 & 0 & 0\\
C & 2015 & 18 & 321 & 318 & 0 & 0\\
C & 2016 & 140 & 207 & 347 & 0 & 0\\
C & 2017 & 4 & 185 & 188 & 0 & 0\\
C & 2018 & 0 & 105 & 105 & 0 & 0\\
C & 2019 & 164 & 90 & 247 & 0 & 24\\
C & 2020 & 234 & 3 & 236 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1998 & 0 & 57 & 57 & 0 & 0\\
O & 1999 & 31 & 9 & 40 & 0 & 0\\
O & 2000 & 184 & 618 & 802 & 0 & 0\\
O & 2001 & 14 & 1214 & 1228 & 0 & 0\\
O & 2002 & 50 & 1245 & 1295 & 0 & 0\\
O & 2003 & 11 & 770 & 777 & 8 & 0\\
O & 2004 & 0 & 775 & 775 & 0 & 0\\
O & 2005 & 1 & 598 & 599 & 0 & 0\\
O & 2006 & 0 & 595 & 595 & 0 & 0\\
O & 2007 & 1 & 813 & 813 & 1 & 0\\
O & 2008 & 9 & 391 & 400 & 1 & 0\\
O & 2009 & 342 & 73 & 415 & 20 & 1\\
O & 2010 & 626 & 153 & 778 & 9 & 0\\
O & 2011 & 820 & 21 & 841 & 32 & 8\\
O & 2012 & 492 & 180 & 665 & 43 & 12\\
O & 2013 & 426 & 175 & 601 & 24 & 10\\
O & 2014 & 641 & 40 & 678 & 29 & 11\\
O & 2015 & 521 & 85 & 606 & 16 & 3\\
O & 2016 & 535 & 218 & 751 & 56 & 8\\
O & 2017 & 787 & 157 & 944 & 68 & 10\\
O & 2018 & 659 & 85 & 742 & 59 & 4\\
O & 2019 & 917 & 2 & 919 & 0 & 95\\
O & 2020 & 288 & 9 & 297 & 0 & 18\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 49 & 49 & 0 & 0\\
C & 2004 & 1 & 715 & 716 & 0 & 0\\
C & 2005 & 1 & 1085 & 1086 & 0 & 0\\
C & 2006 & 0 & 944 & 944 & 0 & 0\\
C & 2007 & 10 & 743 & 753 & 0 & 0\\
C & 2008 & 11 & 672 & 683 & 0 & 0\\
C & 2009 & 12 & 882 & 894 & 0 & 0\\
C & 2010 & 0 & 740 & 740 & 0 & 0\\
C & 2011 & 13 & 943 & 956 & 0 & 0\\
C & 2012 & 57 & 889 & 945 & 0 & 0\\
C & 2013 & 244 & 710 & 949 & 0 & 0\\
C & 2014 & 639 & 402 & 1041 & 0 & 0\\
C & 2015 & 1238 & 227 & 1465 & 0 & 0\\
C & 2016 & 881 & 239 & 1120 & 0 & 0\\
C & 2017 & 844 & 100 & 944 & 0 & 0\\
C & 2018 & 788 & 99 & 887 & 0 & 3\\
C & 2019 & 619 & 76 & 695 & 0 & 5\\
C & 2020 & 3 & 3 & 6 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 520 & 520 & 0 & 0\\
O & 2002 & 0 & 1260 & 1260 & 0 & 0\\
O & 2003 & 0 & 1199 & 1199 & 0 & 0\\
O & 2004 & 0 & 1019 & 1019 & 0 & 0\\
O & 2005 & 73 & 1480 & 1553 & 68 & 0\\
O & 2006 & 337 & 1598 & 1935 & 313 & 1\\
O & 2007 & 229 & 1512 & 1741 & 177 & 0\\
O & 2008 & 383 & 1902 & 2285 & 327 & 0\\
O & 2009 & 431 & 1967 & 2398 & 423 & 0\\
O & 2010 & 349 & 1672 & 2021 & 6 & 345\\
O & 2011 & 332 & 1403 & 1735 & 329 & 0\\
O & 2012 & 278 & 1280 & 1558 & 5 & 273\\
O & 2013 & 152 & 916 & 1068 & 148 & 0\\
O & 2014 & 73 & 640 & 713 & 71 & 0\\
O & 2015 & 92 & 691 & 783 & 92 & 0\\
O & 2016 & 119 & 880 & 999 & 118 & 0\\
O & 2017 & 137 & 1165 & 1302 & 136 & 0\\
O & 2018 & 117 & 894 & 1011 & 115 & 0\\
O & 2019 & 115 & 866 & 981 & 0 & 115\\
O & 2020 & 78 & 4 & 82 & 0 & 78\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 48 & 36 & 84 & 0 & 24\\
W & 2003 & 21 & 72 & 93 & 0 & 0\\
W & 2004 & 30 & 73 & 103 & 0 & 0\\
W & 2005 & 38 & 174 & 212 & 0 & 0\\
W & 2006 & 44 & 86 & 130 & 0 & 0\\
W & 2007 & 33 & 74 & 107 & 0 & 0\\
W & 2008 & 8 & 41 & 49 & 0 & 0\\
W & 2009 & 39 & 65 & 104 & 0 & 0\\
W & 2010 & 37 & 85 & 122 & 0 & 2\\
W & 2011 & 11 & 146 & 157 & 0 & 0\\
W & 2012 & 13 & 75 & 88 & 0 & 0\\
W & 2013 & 20 & 42 & 62 & 0 & 0\\
W & 2014 & 139 & 68 & 207 & 0 & 153\\
W & 2015 & 78 & 36 & 114 & 0 & 78\\
W & 2016 & 259 & 23 & 282 & 0 & 259\\
W & 2017 & 253 & 191 & 444 & 0 & 248\\
W & 2018 & 310 & 197 & 507 & 0 & 300\\
W & 2019 & 509 & 303 & 812 & 0 & 505\\
W & 2020 & 180 & 9 & 189 & 0 & 180\\
W & 2021 & 228 & 21 & 249 & 0 & 226\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# California scorpionfish 
 The most recent assessment of California scorpionfish was a full assessment conducted in 2017. Across available data, California scorpionfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 847 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 768 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2000, a total of 768 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 56,642 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 56,642 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 56,642 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 2,892 length observations, 883 age readings, and 362 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 20 length observations, 0 age readings, and 4 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1999 & 0 & 79 & 79 & 0 & 0\\
C & 2006 & 0 & 33 & 33 & 0 & 0\\
C & 2012 & 0 & 1 & 1 & 0 & 0\\
C & 2013 & 0 & 244 & 244 & 0 & 0\\
C & 2014 & 0 & 51 & 51 & 0 & 0\\
C & 2015 & 0 & 164 & 164 & 0 & 0\\
C & 2016 & 0 & 252 & 252 & 0 & 0\\
C & 2018 & 0 & 23 & 23 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 136 & 136 & 0 & 0\\
C & 2004 & 0 & 1926 & 1926 & 0 & 0\\
C & 2005 & 2 & 2209 & 2210 & 0 & 0\\
C & 2006 & 0 & 4073 & 4073 & 0 & 0\\
C & 2007 & 0 & 4910 & 4910 & 0 & 0\\
C & 2008 & 1 & 6193 & 6193 & 0 & 0\\
C & 2009 & 0 & 5652 & 5653 & 0 & 0\\
C & 2010 & 5 & 6342 & 6346 & 0 & 0\\
C & 2011 & 0 & 3196 & 3196 & 0 & 0\\
C & 2012 & 0 & 3683 & 3683 & 0 & 0\\
C & 2013 & 0 & 3675 & 3675 & 0 & 0\\
C & 2014 & 0 & 2835 & 2835 & 0 & 0\\
C & 2015 & 0 & 1871 & 1871 & 0 & 0\\
C & 2016 & 0 & 2032 & 2032 & 0 & 0\\
C & 2017 & 0 & 2398 & 2398 & 0 & 0\\
C & 2018 & 0 & 2125 & 2125 & 0 & 0\\
C & 2019 & 1 & 2576 & 2577 & 0 & 0\\
C & 2020 & 0 & 803 & 803 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 7 & 69 & 5 & 74 & 0 & 0\\
2004 & 12 & 115 & 41 & 156 & 0 & 0\\
2005 & 8 & 169 & 0 & 169 & 75 & 0\\
2006 & 11 & 46 & 1 & 47 & 45 & 1\\
2007 & 12 & 191 & 0 & 191 & 68 & 0\\
2008 & 12 & 50 & 0 & 50 & 49 & 1\\
2009 & 11 & 346 & 1 & 347 & 130 & 0\\
2010 & 10 & 58 & 0 & 58 & 42 & 0\\
2011 & 16 & 239 & 0 & 239 & 116 & 1\\
2012 & 9 & 94 & 0 & 94 & 66 & 0\\
2013 & 7 & 259 & 0 & 259 & 67 & 0\\
2014 & 6 & 91 & 2 & 93 & 73 & 0\\
2015 & 14 & 160 & 3 & 163 & 57 & 1\\
2016 & 12 & 127 & 12 & 139 & 95 & 2\\
2017 & 13 & 275 & 34 & 309 & 0 & 140\\
2018 & 23 & 287 & 2 & 289 & 0 & 128\\
2019 & 13 & 215 & 0 & 215 & 0 & 88\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2005 & 1 & 0 & 1 & 1 & 0 & 0\\
2006 & 2 & 0 & 2 & 1 & 0 & 1\\
2007 & 1 & 0 & 1 & 1 & 0 & 0\\
2010 & 1 & 0 & 1 & 1 & 0 & 0\\
2014 & 2 & 0 & 2 & 2 & 0 & 0\\
2015 & 5 & 0 & 7 & 7 & 0 & 0\\
2017 & 2 & 0 & 3 & 3 & 0 & 0\\
2018 & 2 & 3 & 1 & 4 & 0 & 3\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Canary rockfish 
 The most recent assessment of Canary rockfish was a full assessment conducted in 2015. Across available data, Canary rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 112,492 length observations, 69,363 age readings, and 18,620 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 38,283 length observations, 19,074 age readings, and 10,885 otoliths that are available to be aged. In California, since 2000, a total of 6,561 length observations, 346 age readings, and 1,184 otoliths have been collected In Oregon, since 2000, a total of 20,910 length observations, 9,478 age readings, and 9,701 otoliths have been collected In Washington, since 2000, a total of 10,812 length observations, 9,250 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 48,446 length observations, 1,569 age readings, and 6,394 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 44,925 length observations, 1,569 age readings, and 6,376 otoliths that are available to be aged. In California, since 2003, a total of 15,864 length observations, 0 age readings, and 163 otoliths have been collected In Oregon, since 2003, a total of 23,555 length observations, 0 age readings, and 3,565 otoliths have been collected In Washington, since 2003, a total of 5,506 length observations, 1,569 age readings, and 2,648 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 12,080 length observations, 5,421 age readings, and 2,131 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 303 length observations, 192 age readings, and 110 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 324 & 40 & 363 & 263 & 332\\
C & 1981 & 198 & 74 & 271 & 166 & 202\\
C & 1982 & 478 & 29 & 507 & 222 & 420\\
C & 1983 & 427 & 30 & 457 & 402 & 498\\
C & 1984 & 377 & 32 & 406 & 364 & 376\\
C & 1985 & 484 & 52 & 536 & 430 & 567\\
C & 1986 & 410 & 86 & 496 & 0 & 28\\
C & 1987 & 420 & 6 & 426 & 1 & 0\\
C & 1988 & 333 & 33 & 366 & 0 & 0\\
C & 1989 & 489 & 88 & 577 & 0 & 0\\
C & 1990 & 321 & 112 & 433 & 0 & 7\\
C & 1991 & 175 & 223 & 398 & 0 & 0\\
C & 1992 & 189 & 1881 & 2070 & 0 & 0\\
C & 1993 & 45 & 1622 & 1667 & 0 & 0\\
C & 1994 & 87 & 2121 & 2208 & 0 & 0\\
C & 1995 & 213 & 1383 & 1596 & 0 & 0\\
C & 1996 & 218 & 1614 & 1832 & 0 & 0\\
C & 1997 & 165 & 1297 & 1462 & 0 & 0\\
C & 1998 & 129 & 468 & 597 & 0 & 0\\
C & 1999 & 339 & 754 & 1093 & 0 & 0\\
C & 2000 & 105 & 130 & 235 & 0 & 0\\
C & 2001 & 110 & 336 & 446 & 28 & 31\\
C & 2002 & 256 & 60 & 316 & 98 & 105\\
C & 2003 & 47 & 0 & 47 & 44 & 44\\
C & 2004 & 37 & 1 & 38 & 17 & 12\\
C & 2005 & 162 & 13 & 175 & 64 & 54\\
C & 2006 & 96 & 20 & 116 & 41 & 55\\
C & 2007 & 63 & 46 & 109 & 27 & 40\\
C & 2008 & 7 & 16 & 23 & 0 & 2\\
C & 2009 & 27 & 79 & 106 & 27 & 41\\
C & 2010 & 8 & 49 & 57 & 0 & 10\\
C & 2011 & 0 & 12 & 12 & 0 & 0\\
C & 2012 & 1 & 166 & 167 & 0 & 0\\
C & 2013 & 165 & 145 & 310 & 0 & 156\\
C & 2014 & 90 & 189 & 202 & 0 & 78\\
C & 2015 & 169 & 303 & 426 & 0 & 176\\
C & 2016 & 109 & 252 & 361 & 0 & 41\\
C & 2017 & 322 & 663 & 985 & 0 & 144\\
C & 2018 & 413 & 375 & 788 & 0 & 46\\
C & 2019 & 379 & 291 & 670 & 0 & 149\\
C & 2020 & 383 & 589 & 972 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1981 & 603 & 0 & 603 & 428 & 175\\
O & 1982 & 1142 & 0 & 1142 & 457 & 685\\
O & 1983 & 2153 & 0 & 2153 & 2041 & 112\\
O & 1984 & 1364 & 0 & 1364 & 1257 & 5\\
O & 1985 & 1588 & 0 & 1588 & 1053 & 100\\
O & 1986 & 1152 & 0 & 1152 & 607 & 545\\
O & 1987 & 1751 & 0 & 1751 & 1448 & 303\\
O & 1988 & 1435 & 0 & 1435 & 459 & 976\\
O & 1989 & 1130 & 0 & 1130 & 1055 & 75\\
O & 1990 & 1199 & 0 & 1199 & 998 & 101\\
O & 1991 & 869 & 0 & 869 & 850 & 19\\
O & 1992 & 1364 & 0 & 1364 & 1280 & 84\\
O & 1993 & 1113 & 0 & 1113 & 1110 & 3\\
O & 1994 & 750 & 0 & 750 & 200 & 45\\
O & 1995 & 847 & 0 & 847 & 794 & 2\\
O & 1996 & 1199 & 0 & 1199 & 1093 & 69\\
O & 1997 & 2083 & 46 & 2129 & 1554 & 8\\
O & 1998 & 1895 & 0 & 1895 & 1641 & 59\\
O & 1999 & 1685 & 0 & 1685 & 1516 & 1\\
O & 2000 & 671 & 1 & 672 & 491 & 5\\
O & 2001 & 1070 & 0 & 1070 & 772 & 16\\
O & 2002 & 1173 & 0 & 1173 & 1008 & 4\\
O & 2003 & 272 & 1 & 273 & 241 & 0\\
O & 2004 & 354 & 2 & 354 & 333 & 3\\
O & 2005 & 350 & 0 & 350 & 342 & 1\\
O & 2006 & 358 & 2 & 360 & 240 & 79\\
O & 2007 & 121 & 1 & 122 & 108 & 8\\
O & 2008 & 203 & 0 & 202 & 195 & 8\\
O & 2009 & 483 & 4 & 487 & 485 & 1\\
O & 2010 & 365 & 0 & 365 & 340 & 0\\
O & 2011 & 418 & 0 & 418 & 390 & 7\\
O & 2012 & 494 & 0 & 494 & 493 & 1\\
O & 2013 & 1174 & 2 & 1175 & 1150 & 7\\
O & 2014 & 1323 & 0 & 1322 & 1319 & 4\\
O & 2015 & 1909 & 3 & 1912 & 1571 & 69\\
O & 2016 & 1289 & 1 & 1290 & 0 & 1237\\
O & 2017 & 3037 & 0 & 3037 & 0 & 2849\\
O & 2018 & 2335 & 7 & 2342 & 0 & 2117\\
O & 2019 & 2350 & 1 & 2351 & 0 & 2179\\
O & 2020 & 1138 & 3 & 1141 & 0 & 1106\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 2107 & 0 & 2106 & 921 & 1140\\
W & 1981 & 2232 & 0 & 2232 & 1633 & 594\\
W & 1982 & 1461 & 0 & 1461 & 1337 & 104\\
W & 1983 & 1833 & 0 & 1833 & 1775 & 0\\
W & 1984 & 2719 & 0 & 2719 & 2660 & 0\\
W & 1985 & 1892 & 0 & 1892 & 1689 & 100\\
W & 1986 & 2644 & 2 & 2646 & 2644 & 0\\
W & 1987 & 1684 & 0 & 1684 & 1631 & 0\\
W & 1988 & 1418 & 0 & 1418 & 1416 & 0\\
W & 1989 & 1016 & 0 & 1016 & 1003 & 0\\
W & 1990 & 902 & 0 & 902 & 902 & 0\\
W & 1991 & 2618 & 0 & 2618 & 2502 & 0\\
W & 1992 & 1508 & 1 & 1509 & 1509 & 0\\
W & 1993 & 1854 & 0 & 1854 & 1848 & 0\\
W & 1994 & 750 & 0 & 750 & 749 & 0\\
W & 1995 & 1100 & 0 & 1100 & 1100 & 0\\
W & 1996 & 788 & 1 & 789 & 787 & 0\\
W & 1997 & 847 & 23 & 870 & 846 & 0\\
W & 1998 & 921 & 1 & 922 & 905 & 0\\
W & 1999 & 749 & 10 & 759 & 743 & 0\\
W & 2000 & 239 & 3 & 242 & 237 & 0\\
W & 2001 & 320 & 136 & 456 & 453 & 0\\
W & 2002 & 671 & 40 & 711 & 708 & 0\\
W & 2003 & 297 & 0 & 297 & 297 & 0\\
W & 2004 & 393 & 1 & 394 & 378 & 0\\
W & 2005 & 426 & 0 & 426 & 424 & 0\\
W & 2006 & 490 & 0 & 490 & 488 & 0\\
W & 2007 & 504 & 0 & 504 & 502 & 0\\
W & 2008 & 437 & 6 & 441 & 441 & 0\\
W & 2009 & 401 & 0 & 401 & 396 & 0\\
W & 2010 & 315 & 0 & 315 & 310 & 0\\
W & 2011 & 372 & 0 & 372 & 343 & 0\\
W & 2012 & 441 & 16 & 455 & 436 & 0\\
W & 2013 & 396 & 0 & 396 & 344 & 0\\
W & 2014 & 127 & 0 & 127 & 81 & 0\\
W & 2015 & 551 & 3 & 554 & 547 & 0\\
W & 2016 & 444 & 0 & 444 & 444 & 0\\
W & 2017 & 1615 & 2 & 1617 & 1581 & 0\\
W & 2018 & 1029 & 1 & 1030 & 840 & 0\\
W & 2019 & 769 & 1 & 770 & 0 & 0\\
W & 2020 & 370 & 0 & 370 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 35 & 35 & 0 & 0\\
C & 2004 & 0 & 243 & 243 & 0 & 0\\
C & 2005 & 0 & 406 & 406 & 0 & 0\\
C & 2006 & 0 & 606 & 606 & 0 & 0\\
C & 2007 & 0 & 357 & 357 & 0 & 0\\
C & 2008 & 0 & 176 & 176 & 0 & 0\\
C & 2009 & 0 & 407 & 407 & 0 & 0\\
C & 2010 & 0 & 389 & 389 & 0 & 0\\
C & 2011 & 1 & 894 & 895 & 0 & 0\\
C & 2012 & 0 & 548 & 548 & 0 & 0\\
C & 2013 & 0 & 575 & 575 & 0 & 0\\
C & 2014 & 0 & 488 & 488 & 0 & 0\\
C & 2015 & 0 & 829 & 829 & 0 & 0\\
C & 2016 & 0 & 577 & 577 & 0 & 0\\
C & 2017 & 0 & 3395 & 3393 & 0 & 0\\
C & 2018 & 0 & 2902 & 2902 & 0 & 122\\
C & 2019 & 1 & 3038 & 3038 & 0 & 41\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1999 & 393 & 0 & 393 & 0 & 0\\
O & 2000 & 367 & 0 & 367 & 0 & 0\\
O & 2001 & 18 & 1005 & 1023 & 0 & 18\\
O & 2002 & 0 & 1540 & 1540 & 0 & 0\\
O & 2003 & 0 & 1885 & 1885 & 0 & 0\\
O & 2004 & 0 & 139 & 139 & 0 & 0\\
O & 2005 & 0 & 207 & 207 & 0 & 0\\
O & 2006 & 1 & 216 & 217 & 0 & 0\\
O & 2007 & 1 & 138 & 139 & 0 & 0\\
O & 2008 & 0 & 206 & 206 & 0 & 0\\
O & 2009 & 1 & 218 & 219 & 0 & 0\\
O & 2010 & 0 & 194 & 194 & 0 & 0\\
O & 2011 & 0 & 137 & 137 & 0 & 0\\
O & 2012 & 1 & 213 & 214 & 0 & 0\\
O & 2013 & 0 & 202 & 202 & 0 & 0\\
O & 2014 & 0 & 244 & 244 & 0 & 0\\
O & 2015 & 615 & 2291 & 2906 & 0 & 618\\
O & 2016 & 500 & 1875 & 2375 & 0 & 506\\
O & 2017 & 699 & 3423 & 4122 & 0 & 703\\
O & 2018 & 559 & 4114 & 4673 & 0 & 559\\
O & 2019 & 584 & 3958 & 4542 & 0 & 592\\
O & 2020 & 575 & 359 & 934 & 0 & 587\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 142 & 56 & 198 & 0 & 0\\
W & 2003 & 201 & 28 & 229 & 0 & 0\\
W & 2004 & 40 & 4 & 44 & 40 & 0\\
W & 2005 & 44 & 7 & 51 & 44 & 0\\
W & 2006 & 4 & 11 & 15 & 4 & 0\\
W & 2007 & 7 & 3 & 10 & 7 & 0\\
W & 2008 & 10 & 3 & 13 & 9 & 1\\
W & 2009 & 8 & 5 & 13 & 8 & 0\\
W & 2010 & 11 & 1 & 12 & 11 & 0\\
W & 2011 & 1 & 10 & 11 & 1 & 0\\
W & 2012 & 7 & 2 & 9 & 7 & 0\\
W & 2013 & 0 & 6 & 6 & 0 & 0\\
W & 2014 & 8 & 0 & 8 & 8 & 0\\
W & 2015 & 5 & 32 & 37 & 5 & 0\\
W & 2016 & 4 & 27 & 31 & 4 & 0\\
W & 2017 & 1194 & 109 & 1303 & 1193 & 1\\
W & 2018 & 436 & 267 & 703 & 228 & 204\\
W & 2019 & 935 & 520 & 1455 & 0 & 934\\
W & 2020 & 450 & 12 & 462 & 0 & 449\\
W & 2021 & 1066 & 28 & 1094 & 0 & 1059\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 50 & 423 & 0 & 423 & 262 & 16\\
2004 & 37 & 482 & 0 & 482 & 254 & 1\\
2005 & 53 & 529 & 1 & 530 & 231 & 0\\
2006 & 32 & 622 & 1 & 623 & 247 & 0\\
2007 & 48 & 672 & 1 & 673 & 497 & 0\\
2008 & 35 & 701 & 91 & 792 & 452 & 0\\
2009 & 32 & 301 & 5 & 306 & 239 & 1\\
2010 & 51 & 432 & 63 & 495 & 397 & 0\\
2011 & 45 & 569 & 4 & 573 & 364 & 1\\
2012 & 56 & 799 & 53 & 852 & 596 & 1\\
2013 & 38 & 480 & 32 & 512 & 376 & 1\\
2014 & 75 & 1457 & 10 & 1467 & 901 & 9\\
2015 & 77 & 1061 & 3 & 1064 & 0 & 738\\
2016 & 80 & 1046 & 9 & 1055 & 605 & 0\\
2017 & 84 & 955 & 15 & 970 & 0 & 528\\
2018 & 65 & 716 & 3 & 719 & 0 & 475\\
2019 & 40 & 544 & 0 & 544 & 0 & 360\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 12 & 21 & 0 & 21 & 21 & 0\\
2005 & 6 & 20 & 0 & 20 & 20 & 0\\
2006 & 7 & 11 & 0 & 11 & 11 & 0\\
2007 & 8 & 21 & 1 & 21 & 21 & 0\\
2008 & 3 & 4 & 0 & 4 & 4 & 0\\
2009 & 8 & 20 & 0 & 20 & 20 & 0\\
2010 & 7 & 16 & 1 & 16 & 16 & 0\\
2011 & 5 & 11 & 0 & 11 & 10 & 1\\
2012 & 4 & 12 & 0 & 12 & 12 & 0\\
2013 & 4 & 12 & 0 & 12 & 12 & 0\\
2014 & 7 & 26 & 0 & 26 & 26 & 0\\
2015 & 9 & 21 & 0 & 21 & 19 & 1\\
2016 & 7 & 11 & 0 & 11 & 0 & 11\\
2017 & 9 & 15 & 0 & 15 & 0 & 15\\
2018 & 12 & 41 & 0 & 41 & 0 & 41\\
2019 & 9 & 41 & 0 & 41 & 0 & 41\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Chilipepper rockfish 
 The most recent assessment of Chilipepper rockfish was a update assessment conducted in 2015. Across available data, Chilipepper rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 151,592 length observations, 53,454 age readings, and 2,689 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 41,931 length observations, 8,784 age readings, and 2,689 otoliths that are available to be aged. In California, since 2000, a total of 39,100 length observations, 8,784 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 2,785 length observations, 0 age readings, and 2,689 otoliths have been collected In Washington, since 2000, a total of 46 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 6,786 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 6,775 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 6,755 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 19 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 1 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 31,590 length observations, 8,654 age readings, and 2,804 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 2,110 length observations, 0 age readings, and 1,898 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 1649 & 100 & 1749 & 1079 & 0\\
C & 1981 & 981 & 115 & 1093 & 701 & 0\\
C & 1982 & 2206 & 227 & 2433 & 1220 & 0\\
C & 1983 & 2709 & 287 & 2991 & 2384 & 0\\
C & 1984 & 5164 & 810 & 5940 & 3618 & 0\\
C & 1985 & 8151 & 471 & 8599 & 3640 & 0\\
C & 1986 & 5018 & 99 & 5113 & 2598 & 0\\
C & 1987 & 5080 & 190 & 5268 & 2932 & 0\\
C & 1988 & 5144 & 198 & 5342 & 2653 & 0\\
C & 1989 & 5237 & 279 & 5516 & 2906 & 0\\
C & 1990 & 5898 & 161 & 6059 & 2152 & 0\\
C & 1991 & 9242 & 674 & 9916 & 2120 & 0\\
C & 1992 & 6032 & 1692 & 7724 & 3232 & 0\\
C & 1993 & 4777 & 4403 & 9179 & 2650 & 0\\
C & 1994 & 2969 & 5475 & 8444 & 1248 & 0\\
C & 1995 & 2541 & 2789 & 5330 & 1712 & 0\\
C & 1996 & 2801 & 2006 & 4807 & 1029 & 0\\
C & 1997 & 3322 & 3021 & 6343 & 1990 & 0\\
C & 1998 & 3440 & 824 & 4264 & 2550 & 0\\
C & 1999 & 3115 & 350 & 3465 & 2256 & 0\\
C & 2000 & 1620 & 450 & 2070 & 1159 & 0\\
C & 2001 & 1686 & 778 & 2397 & 896 & 0\\
C & 2002 & 1734 & 163 & 1897 & 1068 & 0\\
C & 2003 & 406 & 180 & 539 & 312 & 0\\
C & 2004 & 1439 & 410 & 1839 & 1045 & 0\\
C & 2005 & 405 & 72 & 477 & 349 & 0\\
C & 2006 & 555 & 166 & 720 & 0 & 0\\
C & 2007 & 1839 & 347 & 2186 & 725 & 0\\
C & 2008 & 1412 & 434 & 1841 & 567 & 0\\
C & 2009 & 2370 & 780 & 3131 & 892 & 0\\
C & 2010 & 1405 & 686 & 2091 & 491 & 0\\
C & 2011 & 9 & 565 & 574 & 8 & 0\\
C & 2012 & 499 & 860 & 1359 & 427 & 0\\
C & 2013 & 709 & 508 & 1215 & 487 & 0\\
C & 2014 & 786 & 700 & 1264 & 358 & 0\\
C & 2015 & 367 & 1188 & 1524 & 0 & 0\\
C & 2016 & 330 & 1133 & 1385 & 0 & 0\\
C & 2017 & 687 & 621 & 1308 & 0 & 0\\
C & 2018 & 1823 & 1198 & 2992 & 0 & 0\\
C & 2019 & 2903 & 1743 & 4631 & 0 & 0\\
C & 2020 & 2480 & 1300 & 3660 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1998 & 82 & 0 & 82 & 0 & 0\\
O & 2001 & 18 & 0 & 18 & 0 & 18\\
O & 2003 & 15 & 0 & 15 & 0 & 15\\
O & 2005 & 30 & 0 & 30 & 0 & 30\\
O & 2007 & 3 & 0 & 3 & 0 & 1\\
O & 2009 & 224 & 0 & 224 & 0 & 224\\
O & 2010 & 77 & 0 & 77 & 0 & 77\\
O & 2011 & 36 & 0 & 36 & 0 & 5\\
O & 2012 & 46 & 0 & 46 & 0 & 46\\
O & 2013 & 42 & 0 & 42 & 0 & 42\\
O & 2014 & 41 & 0 & 41 & 0 & 41\\
O & 2015 & 162 & 0 & 162 & 0 & 162\\
O & 2016 & 140 & 0 & 140 & 0 & 140\\
O & 2017 & 678 & 0 & 678 & 0 & 678\\
O & 2018 & 452 & 0 & 452 & 0 & 438\\
O & 2019 & 645 & 0 & 645 & 0 & 596\\
O & 2020 & 176 & 0 & 176 & 0 & 176\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1997 & 0 & 1 & 1 & 0 & 0\\
W & 1999 & 0 & 3 & 3 & 0 & 0\\
W & 2017 & 33 & 0 & 33 & 0 & 0\\
W & 2018 & 5 & 0 & 5 & 0 & 0\\
W & 2019 & 4 & 0 & 4 & 0 & 0\\
W & 2020 & 4 & 0 & 4 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 298 & 298 & 0 & 0\\
C & 2005 & 0 & 293 & 293 & 0 & 0\\
C & 2006 & 0 & 333 & 333 & 0 & 0\\
C & 2007 & 0 & 487 & 487 & 0 & 0\\
C & 2008 & 1 & 300 & 301 & 0 & 0\\
C & 2009 & 0 & 231 & 231 & 0 & 0\\
C & 2010 & 0 & 358 & 358 & 0 & 0\\
C & 2011 & 0 & 635 & 635 & 0 & 0\\
C & 2012 & 0 & 707 & 707 & 0 & 0\\
C & 2013 & 0 & 738 & 738 & 0 & 0\\
C & 2014 & 0 & 669 & 669 & 0 & 0\\
C & 2015 & 0 & 442 & 442 & 0 & 0\\
C & 2016 & 0 & 435 & 435 & 0 & 0\\
C & 2017 & 0 & 204 & 204 & 0 & 0\\
C & 2018 & 0 & 239 & 239 & 0 & 0\\
C & 2019 & 0 & 331 & 331 & 0 & 0\\
C & 2020 & 0 & 54 & 54 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 8 & 8 & 0 & 0\\
O & 2002 & 0 & 3 & 3 & 0 & 0\\
O & 2003 & 0 & 2 & 2 & 0 & 0\\
O & 2004 & 0 & 1 & 1 & 0 & 0\\
O & 2007 & 0 & 2 & 2 & 0 & 0\\
O & 2008 & 0 & 2 & 2 & 0 & 0\\
O & 2009 & 0 & 2 & 2 & 0 & 0\\
O & 2012 & 0 & 1 & 1 & 0 & 0\\
O & 2013 & 0 & 3 & 3 & 0 & 0\\
O & 2018 & 0 & 6 & 6 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2019 & 0 & 1 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 85 & 2472 & 136 & 2608 & 677 & 17\\
2004 & 80 & 3214 & 251 & 3465 & 758 & 5\\
2005 & 86 & 3577 & 128 & 3704 & 833 & 1\\
2006 & 70 & 2630 & 66 & 2696 & 611 & 0\\
2007 & 68 & 2472 & 23 & 2495 & 590 & 1\\
2008 & 80 & 2193 & 17 & 2209 & 698 & 2\\
2009 & 79 & 1757 & 358 & 2115 & 619 & 0\\
2010 & 106 & 1666 & 425 & 2091 & 806 & 15\\
2011 & 81 & 980 & 78 & 1058 & 647 & 2\\
2012 & 102 & 1145 & 132 & 1277 & 847 & 0\\
2013 & 94 & 891 & 193 & 1084 & 684 & 0\\
2014 & 126 & 1619 & 121 & 1740 & 884 & 2\\
2015 & 103 & 912 & 140 & 1052 & 0 & 619\\
2016 & 111 & 1176 & 269 & 1445 & 0 & 720\\
2017 & 93 & 867 & 38 & 905 & 0 & 540\\
2018 & 94 & 980 & 7 & 987 & 0 & 530\\
2019 & 52 & 658 & 1 & 659 & 0 & 350\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 13 & 67 & 0 & 67 & 0 & 67\\
2005 & 14 & 71 & 3 & 74 & 0 & 74\\
2006 & 12 & 44 & 0 & 44 & 0 & 44\\
2007 & 14 & 82 & 1 & 83 & 0 & 80\\
2008 & 16 & 89 & 1 & 89 & 0 & 83\\
2009 & 13 & 86 & 1 & 86 & 0 & 82\\
2010 & 15 & 50 & 0 & 50 & 0 & 0\\
2011 & 22 & 122 & 12 & 134 & 0 & 56\\
2012 & 23 & 75 & 4 & 77 & 0 & 77\\
2013 & 13 & 72 & 1 & 73 & 0 & 73\\
2014 & 32 & 167 & 5 & 172 & 0 & 172\\
2015 & 34 & 169 & 0 & 169 & 0 & 168\\
2016 & 30 & 143 & 1 & 143 & 0 & 142\\
2017 & 43 & 307 & 3 & 306 & 0 & 296\\
2018 & 34 & 263 & 1 & 263 & 0 & 261\\
2019 & 36 & 265 & 15 & 280 & 0 & 223\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# China rockfish 
 The most recent assessment of China rockfish was a full assessment conducted in 2015. Across available data, China rockfish have been observed and sampled generally by both commercial and recreational fisheries.  
 
 Across all years of available data, commercial fisheries have collected a total of 19,055 length observations, 1,395 age readings, and 1,005 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 16,842 length observations, 1,395 age readings, and 1,005 otoliths that are available to be aged. In California, since 2000, a total of 1,643 length observations, 0 age readings, and 3 otoliths have been collected In Oregon, since 2000, a total of 15,199 length observations, 1,395 age readings, and 1,002 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 33,361 length observations, 2,254 age readings, and 2,563 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 32,275 length observations, 2,243 age readings, and 2,563 otoliths that are available to be aged. In California, since 2003, a total of 16,105 length observations, 0 age readings, and 19 otoliths have been collected In Oregon, since 2003, a total of 11,963 length observations, 1,042 age readings, and 1,226 otoliths have been collected In Washington, since 2003, a total of 4,207 length observations, 1,201 age readings, and 1,318 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1985 & 0 & 2 & 2 & 0 & 0\\
C & 1991 & 0 & 2 & 2 & 0 & 0\\
C & 1992 & 0 & 314 & 314 & 0 & 0\\
C & 1993 & 0 & 185 & 185 & 0 & 0\\
C & 1994 & 0 & 352 & 352 & 0 & 0\\
C & 1995 & 0 & 142 & 142 & 0 & 0\\
C & 1996 & 1 & 172 & 173 & 0 & 0\\
C & 1997 & 0 & 181 & 181 & 0 & 0\\
C & 1998 & 0 & 47 & 47 & 0 & 0\\
C & 1999 & 0 & 324 & 324 & 0 & 0\\
C & 2000 & 0 & 167 & 152 & 0 & 0\\
C & 2001 & 0 & 164 & 164 & 0 & 0\\
C & 2002 & 1 & 97 & 97 & 0 & 0\\
C & 2003 & 0 & 26 & 26 & 0 & 0\\
C & 2004 & 0 & 102 & 95 & 0 & 0\\
C & 2005 & 0 & 103 & 103 & 0 & 0\\
C & 2006 & 0 & 81 & 73 & 0 & 0\\
C & 2007 & 0 & 257 & 247 & 0 & 0\\
C & 2008 & 2 & 189 & 187 & 0 & 0\\
C & 2009 & 1 & 233 & 209 & 0 & 1\\
C & 2010 & 2 & 201 & 129 & 0 & 1\\
C & 2011 & 0 & 21 & 18 & 0 & 0\\
C & 2012 & 0 & 31 & 13 & 0 & 0\\
C & 2013 & 0 & 39 & 8 & 0 & 0\\
C & 2014 & 0 & 73 & 3 & 0 & 0\\
C & 2015 & 0 & 40 & 23 & 0 & 0\\
C & 2016 & 0 & 67 & 67 & 0 & 0\\
C & 2017 & 1 & 14 & 15 & 0 & 1\\
C & 2018 & 0 & 10 & 10 & 0 & 0\\
C & 2019 & 0 & 2 & 2 & 0 & 0\\
C & 2020 & 0 & 2 & 2 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1995 & 10 & 92 & 102 & 0 & 0\\
O & 1996 & 118 & 0 & 118 & 0 & 0\\
O & 1998 & 138 & 0 & 138 & 0 & 0\\
O & 1999 & 130 & 0 & 130 & 0 & 0\\
O & 2000 & 1232 & 0 & 1232 & 0 & 1\\
O & 2001 & 2053 & 0 & 2053 & 63 & 9\\
O & 2002 & 1588 & 6 & 1592 & 123 & 2\\
O & 2003 & 987 & 0 & 987 & 181 & 4\\
O & 2004 & 701 & 0 & 701 & 55 & 3\\
O & 2005 & 217 & 0 & 217 & 14 & 2\\
O & 2006 & 430 & 8 & 438 & 29 & 1\\
O & 2007 & 724 & 0 & 724 & 40 & 0\\
O & 2008 & 376 & 0 & 376 & 26 & 8\\
O & 2009 & 430 & 0 & 430 & 80 & 0\\
O & 2010 & 528 & 1 & 529 & 65 & 2\\
O & 2011 & 993 & 0 & 993 & 307 & 2\\
O & 2012 & 602 & 1 & 603 & 152 & 1\\
O & 2013 & 676 & 12 & 688 & 260 & 8\\
O & 2014 & 520 & 0 & 520 & 0 & 166\\
O & 2015 & 473 & 2 & 475 & 0 & 97\\
O & 2016 & 473 & 0 & 473 & 0 & 84\\
O & 2017 & 441 & 1 & 442 & 0 & 83\\
O & 2018 & 520 & 0 & 520 & 0 & 125\\
O & 2019 & 826 & 2 & 828 & 0 & 321\\
O & 2020 & 374 & 6 & 378 & 0 & 83\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 0 & 3 & 3 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 6 & 6 & 0 & 0\\
C & 2004 & 0 & 498 & 498 & 0 & 0\\
C & 2005 & 0 & 747 & 747 & 0 & 0\\
C & 2006 & 0 & 957 & 957 & 0 & 0\\
C & 2007 & 0 & 1076 & 1076 & 0 & 0\\
C & 2008 & 0 & 1445 & 1445 & 0 & 0\\
C & 2009 & 0 & 1601 & 1601 & 0 & 0\\
C & 2010 & 0 & 1308 & 1308 & 0 & 0\\
C & 2011 & 0 & 1408 & 1408 & 0 & 0\\
C & 2012 & 0 & 1137 & 1137 & 0 & 0\\
C & 2013 & 2 & 773 & 775 & 0 & 0\\
C & 2014 & 0 & 772 & 772 & 0 & 0\\
C & 2015 & 0 & 1006 & 1006 & 0 & 0\\
C & 2016 & 1 & 1084 & 1085 & 0 & 0\\
C & 2017 & 0 & 855 & 855 & 0 & 0\\
C & 2018 & 0 & 750 & 749 & 0 & 8\\
C & 2019 & 0 & 680 & 680 & 0 & 11\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 373 & 373 & 0 & 0\\
O & 2002 & 0 & 644 & 644 & 0 & 0\\
O & 2003 & 0 & 685 & 685 & 0 & 0\\
O & 2004 & 0 & 398 & 398 & 0 & 0\\
O & 2005 & 56 & 621 & 677 & 110 & 0\\
O & 2006 & 190 & 719 & 909 & 248 & 0\\
O & 2007 & 264 & 901 & 1165 & 60 & 204\\
O & 2008 & 266 & 913 & 1179 & 60 & 209\\
O & 2009 & 180 & 658 & 838 & 62 & 121\\
O & 2010 & 173 & 744 & 917 & 60 & 115\\
O & 2011 & 232 & 892 & 1124 & 236 & 1\\
O & 2012 & 208 & 848 & 1056 & 60 & 148\\
O & 2013 & 147 & 792 & 939 & 146 & 1\\
O & 2014 & 73 & 322 & 395 & 0 & 73\\
O & 2015 & 0 & 28 & 28 & 0 & 0\\
O & 2016 & 0 & 13 & 13 & 0 & 0\\
O & 2017 & 67 & 320 & 387 & 0 & 68\\
O & 2018 & 107 & 476 & 583 & 0 & 108\\
O & 2019 & 131 & 454 & 585 & 0 & 132\\
O & 2020 & 46 & 39 & 85 & 0 & 46\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 35 & 34 & 69 & 11 & 0\\
W & 2003 & 18 & 42 & 60 & 0 & 0\\
W & 2004 & 181 & 42 & 223 & 171 & 4\\
W & 2005 & 211 & 152 & 363 & 206 & 3\\
W & 2006 & 103 & 174 & 277 & 89 & 0\\
W & 2007 & 151 & 69 & 220 & 119 & 0\\
W & 2008 & 78 & 65 & 143 & 73 & 0\\
W & 2009 & 38 & 80 & 118 & 22 & 0\\
W & 2010 & 36 & 42 & 78 & 22 & 0\\
W & 2011 & 53 & 129 & 182 & 50 & 0\\
W & 2012 & 14 & 63 & 77 & 24 & 1\\
W & 2013 & 22 & 150 & 172 & 11 & 0\\
W & 2014 & 439 & 2 & 441 & 414 & 2\\
W & 2015 & 260 & 10 & 270 & 0 & 260\\
W & 2016 & 236 & 2 & 238 & 0 & 236\\
W & 2017 & 114 & 163 & 277 & 0 & 112\\
W & 2018 & 191 & 151 & 342 & 0 & 189\\
W & 2019 & 304 & 201 & 505 & 0 & 302\\
W & 2020 & 85 & 1 & 86 & 0 & 85\\
W & 2021 & 124 & 11 & 135 & 0 & 124\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Copper rockfish 
 The most recent assessment of Copper rockfish was a data-moderate assessment conducted in 2021. Across available data, Copper rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 8,760 length observations, 354 age readings, and 157 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 3,739 length observations, 354 age readings, and 70 otoliths that are available to be aged. In California, since 2000, a total of 2,338 length observations, 0 age readings, and 14 otoliths have been collected In Oregon, since 2000, a total of 1,394 length observations, 352 age readings, and 56 otoliths have been collected In Washington, since 2000, a total of 7 length observations, 2 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 76,409 length observations, 3,992 age readings, and 422 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 75,489 length observations, 3,973 age readings, and 422 otoliths that are available to be aged. In California, since 2003, a total of 59,418 length observations, 0 age readings, and 41 otoliths have been collected In Oregon, since 2003, a total of 12,836 length observations, 2,298 age readings, and 89 otoliths have been collected In Washington, since 2003, a total of 3,235 length observations, 1,675 age readings, and 292 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 1,050 length observations, 187 age readings, and 503 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 1,107 length observations, 0 age readings, and 1,079 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 7 & 27 & 34 & 0 & 14\\
C & 1981 & 0 & 4 & 4 & 0 & 0\\
C & 1982 & 6 & 0 & 6 & 0 & 7\\
C & 1983 & 7 & 8 & 15 & 0 & 12\\
C & 1984 & 25 & 18 & 43 & 0 & 28\\
C & 1985 & 1 & 27 & 28 & 0 & 23\\
C & 1986 & 5 & 31 & 36 & 0 & 2\\
C & 1987 & 8 & 14 & 22 & 0 & 1\\
C & 1988 & 2 & 25 & 27 & 0 & 0\\
C & 1989 & 0 & 24 & 24 & 0 & 0\\
C & 1990 & 1 & 1 & 2 & 0 & 0\\
C & 1991 & 1 & 125 & 126 & 0 & 0\\
C & 1992 & 0 & 664 & 664 & 0 & 0\\
C & 1993 & 11 & 798 & 808 & 0 & 0\\
C & 1994 & 0 & 354 & 354 & 0 & 0\\
C & 1995 & 0 & 450 & 450 & 0 & 0\\
C & 1996 & 0 & 479 & 479 & 0 & 0\\
C & 1997 & 0 & 525 & 525 & 0 & 0\\
C & 1998 & 1 & 578 & 579 & 0 & 0\\
C & 1999 & 29 & 538 & 567 & 0 & 0\\
C & 2000 & 0 & 92 & 91 & 0 & 0\\
C & 2001 & 0 & 245 & 245 & 0 & 0\\
C & 2002 & 0 & 80 & 76 & 0 & 0\\
C & 2003 & 5 & 99 & 90 & 0 & 1\\
C & 2004 & 14 & 31 & 36 & 0 & 6\\
C & 2005 & 0 & 28 & 19 & 0 & 0\\
C & 2006 & 0 & 34 & 19 & 0 & 0\\
C & 2007 & 5 & 106 & 74 & 0 & 5\\
C & 2008 & 0 & 94 & 72 & 0 & 0\\
C & 2009 & 0 & 59 & 52 & 0 & 0\\
C & 2010 & 0 & 84 & 83 & 0 & 0\\
C & 2011 & 0 & 67 & 67 & 0 & 0\\
C & 2012 & 1 & 72 & 73 & 0 & 1\\
C & 2013 & 0 & 47 & 47 & 0 & 0\\
C & 2014 & 1 & 80 & 80 & 0 & 1\\
C & 2015 & 0 & 235 & 234 & 0 & 0\\
C & 2016 & 0 & 265 & 265 & 0 & 0\\
C & 2017 & 0 & 323 & 323 & 0 & 0\\
C & 2018 & 5 & 267 & 170 & 0 & 0\\
C & 2019 & 45 & 139 & 133 & 0 & 0\\
C & 2020 & 40 & 49 & 89 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1999 & 9 & 0 & 9 & 0 & 0\\
O & 2000 & 85 & 0 & 85 & 0 & 0\\
O & 2001 & 92 & 0 & 92 & 0 & 0\\
O & 2002 & 28 & 0 & 28 & 1 & 0\\
O & 2003 & 39 & 0 & 39 & 9 & 0\\
O & 2004 & 52 & 0 & 52 & 26 & 0\\
O & 2005 & 11 & 0 & 11 & 0 & 0\\
O & 2006 & 41 & 0 & 41 & 1 & 0\\
O & 2007 & 31 & 1 & 32 & 1 & 1\\
O & 2008 & 19 & 0 & 19 & 1 & 0\\
O & 2009 & 14 & 0 & 14 & 0 & 1\\
O & 2010 & 42 & 0 & 42 & 6 & 0\\
O & 2011 & 79 & 0 & 79 & 18 & 0\\
O & 2012 & 59 & 0 & 59 & 11 & 0\\
O & 2013 & 63 & 0 & 63 & 31 & 0\\
O & 2014 & 74 & 1 & 75 & 25 & 0\\
O & 2015 & 26 & 0 & 26 & 10 & 0\\
O & 2016 & 78 & 0 & 78 & 25 & 0\\
O & 2017 & 101 & 1 & 102 & 40 & 1\\
O & 2018 & 113 & 0 & 112 & 45 & 1\\
O & 2019 & 218 & 1 & 219 & 102 & 3\\
O & 2020 & 126 & 0 & 126 & 0 & 49\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1982 & 0 & 1 & 1 & 0 & 0\\
W & 1989 & 0 & 118 & 118 & 0 & 0\\
W & 1990 & 0 & 100 & 100 & 0 & 0\\
W & 2004 & 1 & 0 & 1 & 0 & 0\\
W & 2006 & 4 & 0 & 4 & 0 & 0\\
W & 2017 & 2 & 0 & 2 & 2 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 9 & 9 & 0 & 0\\
C & 2004 & 0 & 977 & 977 & 0 & 0\\
C & 2005 & 0 & 2050 & 2050 & 0 & 0\\
C & 2006 & 1 & 3010 & 3011 & 0 & 0\\
C & 2007 & 0 & 3760 & 3760 & 0 & 0\\
C & 2008 & 0 & 3310 & 3310 & 0 & 0\\
C & 2009 & 1 & 2781 & 2782 & 0 & 0\\
C & 2010 & 0 & 2200 & 2200 & 0 & 0\\
C & 2011 & 0 & 2864 & 2864 & 0 & 0\\
C & 2012 & 0 & 3963 & 3962 & 0 & 0\\
C & 2013 & 2 & 5630 & 5632 & 0 & 0\\
C & 2014 & 1 & 4107 & 4108 & 0 & 0\\
C & 2015 & 0 & 5114 & 5114 & 0 & 0\\
C & 2016 & 0 & 4974 & 4973 & 0 & 0\\
C & 2017 & 2 & 5706 & 5704 & 0 & 0\\
C & 2018 & 3 & 4622 & 4624 & 0 & 27\\
C & 2019 & 3 & 4242 & 4243 & 0 & 14\\
C & 2020 & 0 & 95 & 95 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 196 & 196 & 0 & 0\\
O & 2002 & 0 & 641 & 641 & 0 & 0\\
O & 2003 & 0 & 518 & 518 & 0 & 0\\
O & 2004 & 0 & 325 & 325 & 0 & 0\\
O & 2005 & 59 & 696 & 755 & 58 & 0\\
O & 2006 & 149 & 764 & 913 & 150 & 0\\
O & 2007 & 189 & 799 & 988 & 188 & 0\\
O & 2008 & 217 & 836 & 1053 & 217 & 0\\
O & 2009 & 156 & 569 & 725 & 156 & 0\\
O & 2010 & 274 & 790 & 1064 & 273 & 0\\
O & 2011 & 233 & 867 & 1100 & 235 & 0\\
O & 2012 & 216 & 944 & 1160 & 216 & 0\\
O & 2013 & 158 & 570 & 728 & 158 & 0\\
O & 2014 & 121 & 338 & 459 & 121 & 0\\
O & 2015 & 0 & 32 & 32 & 0 & 0\\
O & 2016 & 0 & 28 & 28 & 0 & 0\\
O & 2017 & 176 & 566 & 742 & 177 & 0\\
O & 2018 & 175 & 983 & 1158 & 175 & 0\\
O & 2019 & 173 & 792 & 965 & 174 & 0\\
O & 2020 & 89 & 34 & 123 & 0 & 89\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 61 & 22 & 83 & 19 & 0\\
W & 2003 & 18 & 28 & 46 & 0 & 0\\
W & 2004 & 203 & 41 & 244 & 188 & 0\\
W & 2005 & 265 & 178 & 443 & 225 & 1\\
W & 2006 & 96 & 73 & 169 & 65 & 0\\
W & 2007 & 110 & 42 & 152 & 86 & 0\\
W & 2008 & 71 & 20 & 91 & 65 & 0\\
W & 2009 & 52 & 19 & 71 & 35 & 0\\
W & 2010 & 38 & 19 & 57 & 24 & 0\\
W & 2011 & 28 & 99 & 127 & 27 & 0\\
W & 2012 & 38 & 43 & 81 & 35 & 0\\
W & 2013 & 14 & 57 & 71 & 8 & 2\\
W & 2014 & 132 & 4 & 136 & 123 & 9\\
W & 2015 & 83 & 1 & 84 & 74 & 4\\
W & 2016 & 158 & 21 & 179 & 169 & 5\\
W & 2017 & 110 & 102 & 212 & 101 & 7\\
W & 2018 & 190 & 125 & 315 & 176 & 3\\
W & 2019 & 275 & 188 & 463 & 274 & 1\\
W & 2020 & 76 & 1 & 77 & 0 & 76\\
W & 2021 & 184 & 33 & 217 & 0 & 184\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 7 & 25 & 0 & 25 & 0 & 0\\
2004 & 5 & 71 & 0 & 71 & 20 & 51\\
2005 & 5 & 19 & 3 & 22 & 1 & 21\\
2006 & 3 & 10 & 0 & 10 & 2 & 8\\
2007 & 5 & 13 & 0 & 13 & 8 & 5\\
2008 & 11 & 44 & 0 & 44 & 14 & 30\\
2009 & 7 & 27 & 0 & 27 & 20 & 7\\
2010 & 10 & 17 & 1 & 18 & 1 & 17\\
2011 & 4 & 12 & 0 & 12 & 9 & 3\\
2012 & 19 & 234 & 7 & 241 & 42 & 61\\
2013 & 9 & 98 & 0 & 98 & 2 & 32\\
2014 & 8 & 40 & 0 & 40 & 4 & 29\\
2015 & 9 & 111 & 2 & 113 & 16 & 23\\
2016 & 9 & 53 & 43 & 96 & 28 & 49\\
2017 & 12 & 125 & 1 & 126 & 20 & 82\\
2018 & 11 & 62 & 0 & 62 & 0 & 53\\
2019 & 7 & 32 & 0 & 32 & 0 & 32\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 11 & 33 & 0 & 33 & 0 & 33\\
2005 & 14 & 70 & 0 & 70 & 0 & 69\\
2006 & 12 & 58 & 2 & 58 & 0 & 58\\
2007 & 18 & 77 & 3 & 77 & 0 & 76\\
2008 & 22 & 67 & 0 & 67 & 0 & 67\\
2009 & 21 & 104 & 2 & 104 & 0 & 101\\
2010 & 14 & 24 & 1 & 24 & 0 & 23\\
2011 & 23 & 56 & 0 & 56 & 0 & 53\\
2012 & 22 & 63 & 0 & 63 & 0 & 62\\
2013 & 29 & 46 & 0 & 46 & 0 & 46\\
2014 & 29 & 52 & 1 & 53 & 0 & 48\\
2015 & 38 & 99 & 0 & 99 & 0 & 98\\
2016 & 39 & 108 & 1 & 109 & 0 & 108\\
2017 & 31 & 75 & 0 & 75 & 0 & 69\\
2018 & 30 & 108 & 0 & 108 & 0 & 105\\
2019 & 32 & 64 & 3 & 65 & 0 & 63\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Cowcod 
 The most recent assessment of Cowcod was a full assessment conducted in 2019. Across available data, Cowcod have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 4,026 length observations, 108 age readings, and 33 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 918 length observations, 0 age readings, and 33 otoliths that are available to be aged. In California, since 2000, a total of 882 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 35 length observations, 0 age readings, and 33 otoliths have been collected In Washington, since 2000, a total of 1 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 181 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 181 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 181 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 795 length observations, 468 age readings, and 316 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 662 length observations, 440 age readings, and 199 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 1 & 40 & 41 & 0 & 0\\
C & 1981 & 4 & 15 & 19 & 0 & 0\\
C & 1982 & 6 & 2 & 8 & 4 & 0\\
C & 1983 & 13 & 222 & 235 & 3 & 0\\
C & 1984 & 56 & 430 & 486 & 25 & 0\\
C & 1985 & 49 & 483 & 532 & 45 & 0\\
C & 1986 & 172 & 278 & 450 & 31 & 0\\
C & 1987 & 99 & 91 & 190 & 0 & 0\\
C & 1988 & 33 & 42 & 75 & 0 & 0\\
C & 1989 & 12 & 54 & 66 & 0 & 0\\
C & 1990 & 21 & 16 & 37 & 0 & 0\\
C & 1991 & 80 & 25 & 105 & 0 & 0\\
C & 1992 & 23 & 150 & 173 & 0 & 0\\
C & 1993 & 8 & 65 & 73 & 0 & 0\\
C & 1994 & 2 & 53 & 55 & 0 & 0\\
C & 1995 & 1 & 125 & 126 & 0 & 0\\
C & 1996 & 6 & 142 & 148 & 0 & 0\\
C & 1997 & 28 & 131 & 159 & 0 & 0\\
C & 1998 & 25 & 32 & 57 & 0 & 0\\
C & 1999 & 21 & 52 & 73 & 0 & 0\\
C & 2000 & 0 & 7 & 7 & 0 & 0\\
C & 2001 & 12 & 2 & 14 & 0 & 0\\
C & 2002 & 10 & 0 & 10 & 0 & 0\\
C & 2003 & 4 & 0 & 4 & 0 & 0\\
C & 2004 & 27 & 2 & 29 & 0 & 0\\
C & 2005 & 2 & 0 & 2 & 0 & 0\\
C & 2007 & 2 & 1 & 3 & 0 & 0\\
C & 2011 & 0 & 1 & 1 & 0 & 0\\
C & 2012 & 32 & 14 & 46 & 0 & 0\\
C & 2013 & 19 & 5 & 24 & 0 & 0\\
C & 2014 & 26 & 28 & 54 & 0 & 0\\
C & 2015 & 58 & 105 & 161 & 0 & 0\\
C & 2016 & 59 & 21 & 80 & 0 & 0\\
C & 2017 & 16 & 39 & 55 & 0 & 0\\
C & 2018 & 31 & 90 & 121 & 0 & 0\\
C & 2019 & 12 & 102 & 113 & 0 & 0\\
C & 2020 & 7 & 151 & 158 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2000 & 2 & 0 & 2 & 0 & 0\\
O & 2012 & 1 & 0 & 1 & 0 & 1\\
O & 2018 & 7 & 0 & 7 & 0 & 7\\
O & 2019 & 12 & 0 & 12 & 0 & 12\\
O & 2020 & 13 & 0 & 13 & 0 & 13\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2009 & 1 & 0 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 8 & 8 & 0 & 0\\
C & 2005 & 0 & 4 & 4 & 0 & 0\\
C & 2006 & 0 & 10 & 10 & 0 & 0\\
C & 2007 & 0 & 3 & 3 & 0 & 0\\
C & 2008 & 0 & 6 & 6 & 0 & 0\\
C & 2009 & 0 & 12 & 12 & 0 & 0\\
C & 2010 & 0 & 8 & 8 & 0 & 0\\
C & 2011 & 0 & 32 & 32 & 0 & 0\\
C & 2012 & 0 & 11 & 11 & 0 & 0\\
C & 2013 & 0 & 18 & 18 & 0 & 0\\
C & 2014 & 0 & 4 & 4 & 0 & 0\\
C & 2015 & 0 & 7 & 7 & 0 & 0\\
C & 2016 & 0 & 12 & 12 & 0 & 0\\
C & 2017 & 0 & 18 & 18 & 0 & 0\\
C & 2018 & 0 & 14 & 14 & 0 & 0\\
C & 2019 & 1 & 11 & 12 & 0 & 0\\
C & 2020 & 0 & 2 & 2 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 7 & 13 & 0 & 13 & 13 & 0\\
2004 & 20 & 64 & 0 & 64 & 24 & 40\\
2005 & 21 & 27 & 3 & 30 & 25 & 5\\
2006 & 11 & 25 & 0 & 25 & 25 & 0\\
2007 & 9 & 25 & 0 & 25 & 21 & 4\\
2008 & 11 & 13 & 3 & 16 & 16 & 0\\
2009 & 14 & 20 & 3 & 23 & 22 & 1\\
2010 & 29 & 45 & 15 & 60 & 58 & 2\\
2011 & 20 & 26 & 3 & 29 & 29 & 0\\
2012 & 24 & 72 & 1 & 73 & 73 & 0\\
2013 & 12 & 15 & 11 & 26 & 23 & 1\\
2014 & 22 & 74 & 3 & 77 & 75 & 2\\
2015 & 17 & 30 & 0 & 30 & 30 & 0\\
2016 & 32 & 62 & 3 & 65 & 0 & 65\\
2017 & 17 & 31 & 3 & 34 & 34 & 0\\
2018 & 32 & 113 & 3 & 116 & 0 & 107\\
2019 & 16 & 89 & 0 & 89 & 0 & 89\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 4 & 5 & 0 & 5 & 5 & 0\\
2005 & 10 & 17 & 0 & 17 & 17 & 0\\
2006 & 8 & 10 & 0 & 10 & 10 & 0\\
2007 & 13 & 23 & 0 & 23 & 23 & 0\\
2008 & 12 & 22 & 0 & 22 & 21 & 0\\
2009 & 17 & 30 & 0 & 30 & 30 & 0\\
2010 & 10 & 21 & 0 & 21 & 21 & 0\\
2011 & 8 & 24 & 0 & 24 & 22 & 2\\
2012 & 15 & 35 & 1 & 36 & 35 & 1\\
2013 & 10 & 31 & 0 & 31 & 31 & 0\\
2014 & 20 & 30 & 0 & 30 & 24 & 1\\
2015 & 43 & 110 & 0 & 110 & 95 & 0\\
2016 & 27 & 48 & 0 & 48 & 46 & 0\\
2017 & 29 & 62 & 0 & 62 & 60 & 1\\
2018 & 41 & 100 & 2 & 100 & 0 & 101\\
2019 & 33 & 93 & 0 & 93 & 0 & 93\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Curlfin sole 
 To date, no assessment or analysis has been conducted on Curlfin sole. Across available data, Curlfin sole have been observed and sampled generally by  
 
 Across all years of available data, commercial fisheries have collected a total of 1,725 length observations, 0 age readings, and 289 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 1,724 length observations, 0 age readings, and 289 otoliths that are available to be aged. In California, since 2000, a total of 1,350 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 374 length observations, 0 age readings, and 289 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1999 & 0 & 1 & 1 & 0 & 0\\
C & 2003 & 4 & 0 & 4 & 0 & 0\\
C & 2004 & 0 & 55 & 27 & 0 & 0\\
C & 2005 & 1 & 1 & 2 & 0 & 0\\
C & 2006 & 79 & 0 & 79 & 0 & 0\\
C & 2007 & 15 & 0 & 15 & 0 & 0\\
C & 2008 & 9 & 0 & 9 & 0 & 0\\
C & 2009 & 23 & 0 & 23 & 0 & 0\\
C & 2010 & 32 & 14 & 46 & 0 & 0\\
C & 2011 & 2 & 2 & 4 & 0 & 0\\
C & 2013 & 19 & 0 & 19 & 0 & 0\\
C & 2014 & 140 & 119 & 153 & 0 & 0\\
C & 2015 & 67 & 214 & 145 & 0 & 0\\
C & 2016 & 155 & 63 & 218 & 0 & 0\\
C & 2017 & 83 & 250 & 333 & 0 & 0\\
C & 2018 & 228 & 0 & 228 & 0 & 0\\
C & 2019 & 22 & 5 & 27 & 0 & 0\\
C & 2020 & 1 & 17 & 18 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2004 & 44 & 0 & 44 & 0 & 31\\
O & 2009 & 73 & 0 & 73 & 0 & 43\\
O & 2010 & 32 & 0 & 32 & 0 & 0\\
O & 2011 & 5 & 0 & 5 & 0 & 2\\
O & 2012 & 54 & 0 & 54 & 0 & 54\\
O & 2013 & 44 & 0 & 44 & 0 & 44\\
O & 2014 & 55 & 0 & 55 & 0 & 55\\
O & 2015 & 3 & 0 & 3 & 0 & 3\\
O & 2016 & 24 & 0 & 24 & 0 & 24\\
O & 2017 & 7 & 0 & 7 & 0 & 0\\
O & 2018 & 30 & 0 & 30 & 0 & 30\\
O & 2019 & 3 & 0 & 3 & 0 & 3\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Darkblotched rockfish 
 The most recent assessment of Darkblotched rockfish was a update assessment conducted in 2017. Across available data, Darkblotched rockfish have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 107,759 length observations, 48,775 age readings, and 42,573 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 78,815 length observations, 36,471 age readings, and 20,984 otoliths that are available to be aged. In California, since 2000, a total of 24,951 length observations, 7,662 age readings, and 6,897 otoliths have been collected In Oregon, since 2000, a total of 41,907 length observations, 24,288 age readings, and 13,937 otoliths have been collected In Washington, since 2000, a total of 11,957 length observations, 4,521 age readings, and 150 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 31,089 length observations, 11,727 age readings, and 1,621 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 222 & 19 & 241 & 199 & 121\\
C & 1981 & 232 & 3 & 234 & 198 & 197\\
C & 1982 & 473 & 1 & 474 & 414 & 440\\
C & 1983 & 792 & 0 & 792 & 527 & 765\\
C & 1984 & 1925 & 0 & 1925 & 1 & 1798\\
C & 1985 & 3555 & 7 & 3562 & 3450 & 4185\\
C & 1986 & 2490 & 1 & 2491 & 223 & 2373\\
C & 1987 & 2645 & 60 & 2705 & 1072 & 2225\\
C & 1988 & 1339 & 4 & 1343 & 376 & 1673\\
C & 1989 & 1098 & 9 & 1107 & 0 & 1082\\
C & 1990 & 927 & 11 & 938 & 865 & 818\\
C & 1991 & 809 & 8 & 817 & 407 & 287\\
C & 1992 & 421 & 8 & 429 & 0 & 431\\
C & 1993 & 550 & 57 & 607 & 510 & 473\\
C & 1994 & 450 & 160 & 610 & 436 & 423\\
C & 1995 & 787 & 48 & 835 & 396 & 559\\
C & 1996 & 1052 & 41 & 1093 & 829 & 781\\
C & 1997 & 979 & 38 & 1017 & 861 & 810\\
C & 1998 & 1312 & 121 & 1433 & 934 & 927\\
C & 1999 & 761 & 61 & 822 & 549 & 500\\
C & 2000 & 869 & 37 & 906 & 575 & 570\\
C & 2001 & 1932 & 215 & 2145 & 625 & 479\\
C & 2002 & 990 & 133 & 1122 & 773 & 645\\
C & 2003 & 494 & 155 & 593 & 379 & 293\\
C & 2004 & 566 & 57 & 623 & 289 & 438\\
C & 2005 & 772 & 4 & 776 & 699 & 494\\
C & 2006 & 1582 & 180 & 1762 & 1309 & 834\\
C & 2007 & 1629 & 362 & 1991 & 668 & 556\\
C & 2008 & 1877 & 395 & 2272 & 394 & 310\\
C & 2009 & 1304 & 250 & 1554 & 537 & 452\\
C & 2010 & 793 & 353 & 1144 & 284 & 238\\
C & 2011 & 633 & 323 & 956 & 443 & 330\\
C & 2012 & 782 & 506 & 1127 & 511 & 426\\
C & 2013 & 380 & 303 & 540 & 176 & 176\\
C & 2014 & 405 & 455 & 667 & 0 & 110\\
C & 2015 & 364 & 1208 & 989 & 0 & 149\\
C & 2016 & 848 & 796 & 1255 & 0 & 289\\
C & 2017 & 1537 & 469 & 1957 & 0 & 108\\
C & 2018 & 945 & 169 & 1114 & 0 & 0\\
C & 2019 & 464 & 176 & 640 & 0 & 0\\
C & 2020 & 566 & 252 & 818 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1982 & 150 & 0 & 150 & 0 & 150\\
O & 1984 & 70 & 0 & 70 & 0 & 70\\
O & 1985 & 0 & 201 & 201 & 0 & 201\\
O & 1990 & 100 & 0 & 100 & 0 & 100\\
O & 1991 & 200 & 0 & 200 & 0 & 200\\
O & 1994 & 200 & 0 & 200 & 0 & 0\\
O & 1995 & 188 & 0 & 188 & 0 & 0\\
O & 1996 & 833 & 0 & 833 & 0 & 0\\
O & 1997 & 802 & 0 & 802 & 33 & 0\\
O & 1998 & 541 & 0 & 541 & 0 & 0\\
O & 1999 & 430 & 0 & 430 & 24 & 0\\
O & 2000 & 224 & 0 & 224 & 183 & 3\\
O & 2001 & 1005 & 0 & 1005 & 843 & 1\\
O & 2002 & 611 & 0 & 611 & 610 & 1\\
O & 2003 & 1398 & 49 & 1447 & 1211 & 5\\
O & 2004 & 1305 & 0 & 1305 & 753 & 302\\
O & 2005 & 1275 & 0 & 1275 & 912 & 1\\
O & 2006 & 1457 & 0 & 1457 & 1219 & 87\\
O & 2007 & 2155 & 0 & 2155 & 1773 & 34\\
O & 2008 & 2689 & 0 & 2689 & 2349 & 6\\
O & 2009 & 2828 & 1 & 2829 & 2622 & 9\\
O & 2010 & 2855 & 1 & 2856 & 2302 & 50\\
O & 2011 & 2570 & 0 & 2570 & 2434 & 34\\
O & 2012 & 2309 & 0 & 2309 & 2263 & 11\\
O & 2013 & 2319 & 0 & 2319 & 927 & 1327\\
O & 2014 & 2470 & 3 & 2473 & 2369 & 4\\
O & 2015 & 3189 & 0 & 3189 & 1406 & 1530\\
O & 2016 & 2467 & 3 & 2470 & 112 & 2274\\
O & 2017 & 2621 & 1 & 2621 & 0 & 2452\\
O & 2018 & 2492 & 6 & 2498 & 0 & 2373\\
O & 2019 & 2305 & 3 & 2308 & 0 & 2268\\
O & 2020 & 1297 & 0 & 1297 & 0 & 1165\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 370 & 370 & 0 & 0\\
W & 1997 & 0 & 586 & 586 & 0 & 0\\
W & 1998 & 317 & 139 & 456 & 0 & 0\\
W & 1999 & 332 & 10 & 342 & 0 & 0\\
W & 2000 & 652 & 1 & 653 & 0 & 0\\
W & 2001 & 660 & 232 & 892 & 0 & 0\\
W & 2002 & 1124 & 5 & 1129 & 389 & 0\\
W & 2003 & 580 & 0 & 580 & 369 & 0\\
W & 2004 & 605 & 11 & 616 & 365 & 50\\
W & 2005 & 117 & 0 & 117 & 103 & 0\\
W & 2006 & 397 & 108 & 505 & 294 & 0\\
W & 2007 & 529 & 50 & 579 & 423 & 0\\
W & 2008 & 350 & 0 & 350 & 243 & 0\\
W & 2009 & 350 & 9 & 359 & 281 & 0\\
W & 2010 & 206 & 3 & 209 & 120 & 0\\
W & 2011 & 869 & 0 & 869 & 535 & 0\\
W & 2012 & 739 & 29 & 768 & 466 & 0\\
W & 2013 & 701 & 0 & 701 & 300 & 100\\
W & 2014 & 409 & 1 & 410 & 237 & 0\\
W & 2015 & 577 & 1 & 578 & 396 & 0\\
W & 2016 & 487 & 0 & 487 & 0 & 0\\
W & 2017 & 723 & 0 & 723 & 0 & 0\\
W & 2018 & 543 & 0 & 543 & 0 & 0\\
W & 2019 & 618 & 1 & 619 & 0 & 0\\
W & 2020 & 263 & 7 & 270 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 100 & 2371 & 4 & 2375 & 748 & 24\\
2004 & 90 & 1056 & 7 & 1062 & 595 & 0\\
2005 & 110 & 1972 & 11 & 1983 & 804 & 0\\
2006 & 130 & 1910 & 15 & 1925 & 940 & 4\\
2007 & 132 & 2060 & 26 & 2086 & 987 & 0\\
2008 & 111 & 1632 & 15 & 1647 & 762 & 0\\
2009 & 126 & 2268 & 30 & 2298 & 1159 & 0\\
2010 & 117 & 2224 & 15 & 2239 & 912 & 0\\
2011 & 108 & 1824 & 4 & 1828 & 796 & 2\\
2012 & 102 & 2166 & 39 & 2205 & 791 & 2\\
2013 & 89 & 1543 & 5 & 1548 & 687 & 3\\
2014 & 114 & 1420 & 97 & 1517 & 767 & 0\\
2015 & 132 & 2391 & 67 & 2458 & 1066 & 2\\
2016 & 118 & 2094 & 3 & 2097 & 713 & 1\\
2017 & 124 & 1696 & 34 & 1730 & 0 & 669\\
2018 & 105 & 1268 & 2 & 1269 & 0 & 585\\
2019 & 57 & 822 & 0 & 822 & 0 & 329\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Dover sole 
 The most recent assessment of Dover sole was a full assessment conducted in 2021. Across available data, Dover sole have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 219,111 length observations, 87,738 age readings, and 53,248 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 130,130 length observations, 30,358 age readings, and 39,850 otoliths that are available to be aged. In California, since 2000, a total of 46,413 length observations, 11,604 age readings, and 8,454 otoliths have been collected In Oregon, since 2000, a total of 58,569 length observations, 10,194 age readings, and 31,396 otoliths have been collected In Washington, since 2000, a total of 25,148 length observations, 8,560 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 165,963 length observations, 16,922 age readings, and 7,717 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1989 & 672 & 1 & 673 & 0 & 1525\\
C & 1990 & 4070 & 0 & 4069 & 859 & 461\\
C & 1991 & 5564 & 82 & 5646 & 1241 & 872\\
C & 1992 & 5081 & 0 & 5081 & 1562 & 1174\\
C & 1993 & 3316 & 10 & 3326 & 1105 & 946\\
C & 1994 & 2887 & 42 & 2929 & 1083 & 1076\\
C & 1995 & 3508 & 28 & 3535 & 1353 & 1460\\
C & 1996 & 3591 & 29 & 3620 & 1649 & 871\\
C & 1997 & 3537 & 16 & 3553 & 1742 & 1376\\
C & 1998 & 3635 & 24 & 3659 & 1979 & 1244\\
C & 1999 & 3366 & 23 & 3389 & 1779 & 1228\\
C & 2000 & 2754 & 120 & 2874 & 2470 & 0\\
C & 2001 & 2945 & 43 & 2988 & 2118 & 0\\
C & 2002 & 4124 & 37 & 4159 & 2396 & 0\\
C & 2003 & 3943 & 53 & 3987 & 2178 & 0\\
C & 2004 & 3174 & 28 & 3202 & 388 & 0\\
C & 2005 & 3258 & 32 & 3286 & 0 & 0\\
C & 2006 & 2426 & 115 & 2541 & 423 & 0\\
C & 2007 & 2347 & 149 & 2494 & 1010 & 1042\\
C & 2008 & 2497 & 223 & 2719 & 270 & 975\\
C & 2009 & 2509 & 426 & 2933 & 351 & 917\\
C & 2010 & 986 & 209 & 1192 & 0 & 348\\
C & 2011 & 1296 & 233 & 1529 & 0 & 349\\
C & 2012 & 2180 & 248 & 2428 & 0 & 962\\
C & 2013 & 2508 & 49 & 2557 & 0 & 772\\
C & 2014 & 1863 & 133 & 1996 & 0 & 703\\
C & 2015 & 1416 & 89 & 1505 & 0 & 732\\
C & 2016 & 1007 & 172 & 1179 & 0 & 848\\
C & 2017 & 532 & 41 & 573 & 0 & 339\\
C & 2018 & 666 & 38 & 704 & 0 & 467\\
C & 2019 & 562 & 130 & 692 & 0 & 0\\
C & 2020 & 783 & 92 & 875 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1987 & 3926 & 0 & 3926 & 3706 & 220\\
O & 1988 & 3090 & 0 & 3090 & 2990 & 100\\
O & 1989 & 3165 & 0 & 3165 & 3063 & 102\\
O & 1990 & 3250 & 0 & 3250 & 3099 & 151\\
O & 1991 & 4687 & 0 & 4687 & 4675 & 12\\
O & 1992 & 4124 & 0 & 4124 & 4026 & 49\\
O & 1993 & 1608 & 0 & 1608 & 1602 & 6\\
O & 1994 & 1887 & 1 & 1888 & 1880 & 8\\
O & 1995 & 1702 & 0 & 1702 & 1615 & 87\\
O & 1996 & 1204 & 38 & 1242 & 1229 & 13\\
O & 1997 & 1823 & 0 & 1795 & 1650 & 78\\
O & 1998 & 2130 & 0 & 2129 & 1721 & 312\\
O & 1999 & 2197 & 0 & 2197 & 1923 & 27\\
O & 2000 & 2137 & 1 & 2138 & 1711 & 20\\
O & 2001 & 1684 & 0 & 1684 & 310 & 1301\\
O & 2002 & 2264 & 0 & 2264 & 350 & 1218\\
O & 2003 & 2789 & 0 & 2789 & 2163 & 125\\
O & 2004 & 2889 & 0 & 2889 & 1188 & 1213\\
O & 2005 & 2928 & 0 & 2928 & 624 & 1274\\
O & 2006 & 3792 & 0 & 3792 & 749 & 1573\\
O & 2007 & 3060 & 1 & 3060 & 899 & 949\\
O & 2008 & 4004 & 1 & 4005 & 782 & 1783\\
O & 2009 & 3146 & 1 & 3146 & 743 & 1584\\
O & 2010 & 3163 & 31 & 3192 & 0 & 2007\\
O & 2011 & 3099 & 0 & 3099 & 0 & 1894\\
O & 2012 & 3173 & 0 & 3173 & 0 & 2337\\
O & 2013 & 2825 & 0 & 2825 & 0 & 2015\\
O & 2014 & 2712 & 4 & 2716 & 0 & 1885\\
O & 2015 & 2488 & 0 & 2488 & 219 & 1659\\
O & 2016 & 3069 & 30 & 3099 & 0 & 2098\\
O & 2017 & 3302 & 0 & 3302 & 202 & 2077\\
O & 2018 & 2469 & 1 & 2470 & 49 & 1765\\
O & 2019 & 2093 & 0 & 2093 & 205 & 1507\\
O & 2020 & 1417 & 0 & 1417 & 0 & 1112\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1985 & 1100 & 0 & 1100 & 589 & 0\\
W & 1986 & 1100 & 0 & 1099 & 690 & 0\\
W & 1987 & 949 & 1 & 950 & 693 & 0\\
W & 1988 & 1100 & 0 & 1100 & 827 & 0\\
W & 1989 & 999 & 0 & 999 & 554 & 0\\
W & 1990 & 799 & 1 & 800 & 593 & 0\\
W & 1991 & 900 & 0 & 900 & 434 & 0\\
W & 1992 & 849 & 0 & 849 & 838 & 0\\
W & 1993 & 850 & 0 & 850 & 745 & 0\\
W & 1994 & 848 & 2 & 850 & 843 & 0\\
W & 1995 & 1049 & 1 & 1050 & 1045 & 0\\
W & 1996 & 999 & 1 & 1000 & 993 & 0\\
W & 1997 & 994 & 6 & 1000 & 396 & 0\\
W & 1998 & 951 & 1 & 952 & 302 & 0\\
W & 1999 & 1198 & 2 & 1199 & 307 & 0\\
W & 2000 & 1150 & 0 & 1150 & 300 & 0\\
W & 2001 & 950 & 1 & 950 & 597 & 0\\
W & 2002 & 899 & 3 & 901 & 440 & 0\\
W & 2003 & 1131 & 0 & 1131 & 629 & 0\\
W & 2004 & 949 & 1 & 950 & 919 & 0\\
W & 2005 & 850 & 0 & 850 & 344 & 0\\
W & 2006 & 800 & 350 & 1150 & 573 & 0\\
W & 2007 & 2100 & 50 & 2150 & 637 & 0\\
W & 2008 & 2130 & 4 & 2134 & 0 & 0\\
W & 2009 & 1000 & 0 & 1000 & 0 & 0\\
W & 2010 & 1193 & 7 & 1200 & 0 & 0\\
W & 2011 & 1628 & 24 & 1652 & 889 & 0\\
W & 2012 & 954 & 1 & 955 & 552 & 0\\
W & 2013 & 1807 & 0 & 1807 & 898 & 0\\
W & 2014 & 598 & 1 & 599 & 347 & 0\\
W & 2015 & 1569 & 1 & 1570 & 417 & 0\\
W & 2016 & 807 & 2 & 809 & 208 & 0\\
W & 2017 & 2051 & 1 & 2052 & 278 & 0\\
W & 2018 & 1223 & 0 & 1223 & 532 & 0\\
W & 2019 & 803 & 5 & 808 & 0 & 0\\
W & 2020 & 107 & 0 & 107 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 440 & 22866 & 32 & 22898 & 957 & 2024\\
2004 & 402 & 17312 & 40 & 17349 & 952 & 1546\\
2005 & 547 & 17659 & 5 & 17664 & 989 & 1513\\
2006 & 528 & 13496 & 23 & 13519 & 970 & 1011\\
2007 & 577 & 11255 & 10 & 11265 & 984 & 1157\\
2008 & 553 & 6230 & 4 & 6234 & 948 & 124\\
2009 & 541 & 3566 & 7 & 3573 & 1034 & 20\\
2010 & 600 & 3206 & 33 & 3239 & 996 & 176\\
2011 & 570 & 8396 & 27 & 8423 & 1075 & 10\\
2012 & 559 & 8761 & 15 & 8776 & 1088 & 6\\
2013 & 413 & 7316 & 12 & 7328 & 809 & 9\\
2014 & 576 & 10204 & 25 & 10177 & 1123 & 7\\
2015 & 567 & 10029 & 60 & 10070 & 1088 & 9\\
2016 & 580 & 10126 & 34 & 10160 & 1120 & 6\\
2017 & 586 & 6210 & 13 & 6223 & 1141 & 15\\
2018 & 592 & 6015 & 16 & 6031 & 1161 & 5\\
2019 & 291 & 3033 & 1 & 3034 & 487 & 79\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# English sole 
 The most recent assessment of English sole was a data-moderate assessment conducted in 2013. Across available data, English sole have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 129,492 length observations, 7,970 age readings, and 26,433 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 77,288 length observations, 5,404 age readings, and 20,897 otoliths that are available to be aged. In California, since 2000, a total of 22,846 length observations, 0 age readings, and 1,630 otoliths have been collected In Oregon, since 2000, a total of 29,492 length observations, 0 age readings, and 13,742 otoliths have been collected In Washington, since 2000, a total of 24,950 length observations, 5,404 age readings, and 5,525 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 79,439 length observations, 898 age readings, and 14,902 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 1 & 0 & 1 & 0 & 3365\\
C & 1982 & 1 & 0 & 1 & 0 & 1521\\
C & 1991 & 62 & 0 & 62 & 0 & 0\\
C & 2001 & 157 & 77 & 234 & 0 & 0\\
C & 2002 & 116 & 26 & 116 & 0 & 0\\
C & 2003 & 622 & 66 & 630 & 0 & 74\\
C & 2004 & 991 & 1 & 992 & 0 & 56\\
C & 2005 & 1135 & 133 & 1197 & 0 & 280\\
C & 2006 & 1723 & 200 & 1922 & 0 & 60\\
C & 2007 & 1774 & 87 & 1861 & 0 & 0\\
C & 2008 & 1656 & 273 & 1927 & 0 & 187\\
C & 2009 & 1251 & 258 & 1507 & 0 & 71\\
C & 2010 & 641 & 303 & 944 & 0 & 20\\
C & 2011 & 252 & 50 & 302 & 0 & 72\\
C & 2012 & 604 & 273 & 877 & 0 & 32\\
C & 2013 & 1243 & 265 & 1508 & 0 & 149\\
C & 2014 & 821 & 100 & 921 & 0 & 142\\
C & 2015 & 1714 & 302 & 2016 & 0 & 293\\
C & 2016 & 1494 & 137 & 1631 & 0 & 134\\
C & 2017 & 1750 & 133 & 1883 & 0 & 60\\
C & 2018 & 728 & 94 & 822 & 0 & 0\\
C & 2019 & 553 & 136 & 689 & 0 & 0\\
C & 2020 & 675 & 192 & 867 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1987 & 1558 & 0 & 1558 & 1194 & 364\\
O & 1988 & 954 & 0 & 954 & 668 & 286\\
O & 1989 & 1301 & 0 & 1301 & 0 & 0\\
O & 1990 & 1049 & 0 & 1049 & 0 & 0\\
O & 1991 & 949 & 0 & 949 & 0 & 0\\
O & 1992 & 803 & 0 & 803 & 0 & 0\\
O & 1993 & 845 & 0 & 845 & 0 & 0\\
O & 1994 & 838 & 0 & 838 & 0 & 0\\
O & 1995 & 587 & 0 & 587 & 0 & 0\\
O & 1996 & 863 & 0 & 863 & 0 & 0\\
O & 1997 & 2170 & 0 & 2170 & 0 & 0\\
O & 1998 & 1756 & 0 & 1756 & 0 & 0\\
O & 1999 & 1775 & 0 & 1775 & 0 & 0\\
O & 2000 & 1469 & 0 & 1469 & 0 & 0\\
O & 2001 & 2412 & 0 & 2412 & 0 & 0\\
O & 2002 & 2533 & 0 & 2533 & 0 & 0\\
O & 2003 & 1589 & 0 & 1589 & 0 & 0\\
O & 2004 & 1496 & 0 & 1496 & 0 & 0\\
O & 2005 & 1954 & 0 & 1954 & 0 & 0\\
O & 2006 & 2261 & 0 & 2261 & 0 & 60\\
O & 2007 & 1079 & 0 & 1079 & 0 & 1019\\
O & 2008 & 840 & 0 & 840 & 0 & 840\\
O & 2009 & 1045 & 0 & 1045 & 0 & 955\\
O & 2010 & 1378 & 2 & 1379 & 0 & 990\\
O & 2011 & 990 & 0 & 990 & 0 & 960\\
O & 2012 & 750 & 0 & 750 & 0 & 750\\
O & 2013 & 1109 & 1 & 1110 & 0 & 1110\\
O & 2014 & 1236 & 1 & 1235 & 0 & 1237\\
O & 2015 & 1409 & 0 & 1409 & 0 & 1200\\
O & 2016 & 1400 & 0 & 1400 & 0 & 1160\\
O & 2017 & 1668 & 0 & 1668 & 0 & 1301\\
O & 2018 & 1331 & 0 & 1330 & 0 & 1011\\
O & 2019 & 1039 & 0 & 1039 & 0 & 759\\
O & 2020 & 504 & 0 & 504 & 0 & 390\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 4749 & 0 & 4749 & 0 & 0\\
W & 1981 & 2188 & 107 & 2295 & 0 & 0\\
W & 1982 & 2939 & 100 & 3037 & 0 & 0\\
W & 1983 & 1834 & 100 & 1934 & 0 & 0\\
W & 1984 & 1645 & 0 & 1645 & 0 & 0\\
W & 1985 & 2210 & 0 & 2210 & 0 & 0\\
W & 1986 & 783 & 0 & 783 & 0 & 0\\
W & 1987 & 1226 & 0 & 1226 & 0 & 0\\
W & 1988 & 1121 & 0 & 1121 & 0 & 0\\
W & 1989 & 1435 & 1 & 1436 & 0 & 0\\
W & 1990 & 2078 & 93 & 2171 & 0 & 0\\
W & 1991 & 2898 & 0 & 2898 & 0 & 0\\
W & 1992 & 2144 & 100 & 2244 & 2 & 0\\
W & 1993 & 2092 & 0 & 2092 & 454 & 0\\
W & 1994 & 1149 & 1 & 1149 & 138 & 0\\
W & 1995 & 1228 & 2 & 1228 & 18 & 0\\
W & 1996 & 729 & 108 & 836 & 39 & 0\\
W & 1997 & 1237 & 98 & 1335 & 43 & 0\\
W & 1998 & 1303 & 2 & 1305 & 10 & 0\\
W & 1999 & 998 & 2 & 998 & 0 & 0\\
W & 2000 & 999 & 1 & 1000 & 0 & 0\\
W & 2001 & 1008 & 1 & 1009 & 10 & 0\\
W & 2002 & 400 & 0 & 400 & 0 & 0\\
W & 2003 & 850 & 0 & 850 & 0 & 0\\
W & 2004 & 762 & 0 & 762 & 21 & 0\\
W & 2005 & 975 & 0 & 975 & 0 & 0\\
W & 2006 & 1510 & 89 & 1599 & 10 & 0\\
W & 2007 & 2791 & 0 & 2791 & 1141 & 1144\\
W & 2008 & 2144 & 0 & 2144 & 746 & 747\\
W & 2009 & 2041 & 150 & 2191 & 741 & 693\\
W & 2010 & 1195 & 0 & 1195 & 395 & 395\\
W & 2011 & 2022 & 0 & 2022 & 672 & 700\\
W & 2012 & 1014 & 16 & 1030 & 349 & 350\\
W & 2013 & 1878 & 1 & 1879 & 522 & 699\\
W & 2014 & 1489 & 1 & 1490 & 497 & 497\\
W & 2015 & 1100 & 0 & 1100 & 300 & 300\\
W & 2016 & 450 & 0 & 450 & 0 & 0\\
W & 2017 & 1330 & 0 & 1330 & 0 & 0\\
W & 2018 & 375 & 50 & 425 & 0 & 0\\
W & 2019 & 308 & 0 & 308 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 221 & 8124 & 0 & 8124 & 182 & 1584\\
2004 & 210 & 8209 & 4 & 8210 & 360 & 563\\
2005 & 286 & 8366 & 0 & 8366 & 356 & 873\\
2006 & 229 & 5383 & 6 & 5389 & 0 & 980\\
2007 & 239 & 3935 & 10 & 3945 & 0 & 926\\
2008 & 235 & 3477 & 1 & 3478 & 0 & 834\\
2009 & 242 & 3346 & 21 & 3367 & 0 & 893\\
2010 & 286 & 2476 & 1 & 2477 & 0 & 1046\\
2011 & 299 & 4448 & 6 & 4454 & 0 & 1062\\
2012 & 299 & 4637 & 0 & 4637 & 0 & 1078\\
2013 & 204 & 3542 & 3 & 3545 & 0 & 748\\
2014 & 311 & 5263 & 1 & 5242 & 0 & 1138\\
2015 & 305 & 5135 & 0 & 5105 & 0 & 1091\\
2016 & 310 & 5223 & 0 & 5223 & 0 & 622\\
2017 & 321 & 3542 & 7 & 3549 & 0 & 623\\
2018 & 285 & 2914 & 1 & 2915 & 0 & 559\\
2019 & 148 & 1413 & 0 & 1413 & 0 & 282\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Flag rockfish 
 The most recent assessment of Flag rockfish was a data-limited assessment conducted in 2010. Across available data, Flag rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 1,259 length observations, 0 age readings, and 44 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 289 length observations, 0 age readings, and 13 otoliths that are available to be aged. In California, since 2000, a total of 289 length observations, 0 age readings, and 13 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 11,029 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 11,029 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 11,021 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 8 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 455 length observations, 3 age readings, and 327 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 182 length observations, 0 age readings, and 160 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 1 & 2 & 3 & 0 & 1\\
C & 1981 & 2 & 3 & 5 & 0 & 1\\
C & 1982 & 2 & 1 & 3 & 0 & 0\\
C & 1983 & 6 & 35 & 41 & 0 & 9\\
C & 1984 & 3 & 90 & 93 & 0 & 8\\
C & 1985 & 6 & 48 & 54 & 0 & 9\\
C & 1986 & 20 & 28 & 48 & 0 & 3\\
C & 1987 & 12 & 37 & 49 & 0 & 0\\
C & 1988 & 16 & 7 & 23 & 0 & 0\\
C & 1989 & 8 & 16 & 24 & 0 & 0\\
C & 1990 & 4 & 0 & 4 & 0 & 0\\
C & 1991 & 2 & 0 & 2 & 0 & 0\\
C & 1992 & 0 & 49 & 49 & 0 & 0\\
C & 1993 & 1 & 68 & 69 & 0 & 0\\
C & 1994 & 1 & 58 & 59 & 0 & 0\\
C & 1995 & 0 & 61 & 61 & 0 & 0\\
C & 1996 & 0 & 117 & 117 & 0 & 0\\
C & 1997 & 0 & 78 & 78 & 0 & 0\\
C & 1998 & 1 & 134 & 135 & 0 & 0\\
C & 1999 & 2 & 51 & 53 & 0 & 0\\
C & 2000 & 0 & 16 & 16 & 0 & 0\\
C & 2001 & 1 & 7 & 7 & 0 & 0\\
C & 2002 & 0 & 14 & 14 & 0 & 0\\
C & 2003 & 1 & 0 & 1 & 0 & 1\\
C & 2004 & 6 & 1 & 7 & 0 & 3\\
C & 2006 & 0 & 13 & 13 & 0 & 0\\
C & 2008 & 15 & 22 & 37 & 0 & 7\\
C & 2009 & 0 & 18 & 18 & 0 & 0\\
C & 2010 & 0 & 17 & 17 & 0 & 0\\
C & 2011 & 0 & 27 & 27 & 0 & 0\\
C & 2012 & 0 & 14 & 14 & 0 & 0\\
C & 2013 & 0 & 7 & 7 & 0 & 0\\
C & 2014 & 0 & 9 & 9 & 0 & 0\\
C & 2015 & 0 & 25 & 25 & 0 & 0\\
C & 2016 & 1 & 49 & 50 & 0 & 2\\
C & 2017 & 1 & 2 & 3 & 0 & 0\\
C & 2018 & 0 & 12 & 8 & 0 & 0\\
C & 2019 & 0 & 2 & 2 & 0 & 0\\
C & 2020 & 0 & 14 & 14 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 324 & 324 & 0 & 0\\
C & 2005 & 1 & 564 & 565 & 0 & 0\\
C & 2006 & 0 & 641 & 641 & 0 & 0\\
C & 2007 & 2 & 849 & 851 & 0 & 0\\
C & 2008 & 0 & 885 & 885 & 0 & 0\\
C & 2009 & 1 & 781 & 782 & 0 & 0\\
C & 2010 & 2 & 594 & 596 & 0 & 0\\
C & 2011 & 1 & 792 & 793 & 0 & 0\\
C & 2012 & 0 & 885 & 885 & 0 & 0\\
C & 2013 & 1 & 1187 & 1188 & 0 & 0\\
C & 2014 & 0 & 607 & 607 & 0 & 0\\
C & 2015 & 0 & 634 & 634 & 0 & 0\\
C & 2016 & 1 & 521 & 522 & 0 & 0\\
C & 2017 & 0 & 581 & 581 & 0 & 0\\
C & 2018 & 0 & 510 & 510 & 0 & 0\\
C & 2019 & 3 & 628 & 631 & 0 & 0\\
C & 2020 & 0 & 26 & 26 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2005 & 0 & 1 & 1 & 0 & 0\\
O & 2010 & 0 & 3 & 3 & 0 & 0\\
O & 2013 & 0 & 1 & 1 & 0 & 0\\
O & 2019 & 0 & 3 & 3 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 6 & 77 & 0 & 77 & 0 & 0\\
2004 & 8 & 14 & 0 & 14 & 0 & 14\\
2005 & 5 & 6 & 1 & 7 & 0 & 7\\
2006 & 9 & 25 & 0 & 25 & 0 & 25\\
2007 & 13 & 54 & 1 & 55 & 0 & 49\\
2008 & 8 & 8 & 0 & 8 & 0 & 8\\
2009 & 11 & 29 & 1 & 30 & 3 & 27\\
2010 & 8 & 16 & 0 & 16 & 0 & 16\\
2011 & 4 & 5 & 0 & 5 & 0 & 5\\
2012 & 6 & 69 & 0 & 69 & 0 & 35\\
2013 & 4 & 11 & 0 & 11 & 0 & 11\\
2014 & 5 & 19 & 0 & 19 & 0 & 19\\
2015 & 5 & 18 & 0 & 18 & 0 & 17\\
2016 & 8 & 11 & 1 & 12 & 0 & 12\\
2017 & 7 & 36 & 0 & 36 & 0 & 29\\
2018 & 9 & 34 & 0 & 34 & 0 & 34\\
2019 & 3 & 19 & 0 & 19 & 0 & 19\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 8 & 10 & 0 & 10 & 0 & 10\\
2005 & 4 & 6 & 0 & 6 & 0 & 6\\
2006 & 9 & 11 & 0 & 11 & 0 & 11\\
2007 & 10 & 12 & 0 & 12 & 0 & 11\\
2008 & 11 & 14 & 0 & 14 & 0 & 13\\
2009 & 9 & 14 & 0 & 14 & 0 & 12\\
2010 & 11 & 15 & 2 & 17 & 0 & 13\\
2011 & 11 & 14 & 0 & 14 & 0 & 13\\
2012 & 7 & 8 & 0 & 8 & 0 & 6\\
2013 & 4 & 4 & 0 & 4 & 0 & 4\\
2014 & 12 & 17 & 0 & 17 & 0 & 13\\
2015 & 13 & 21 & 0 & 21 & 0 & 18\\
2016 & 6 & 6 & 0 & 6 & 0 & 6\\
2017 & 6 & 8 & 0 & 8 & 0 & 8\\
2018 & 9 & 11 & 0 & 11 & 0 & 10\\
2019 & 8 & 9 & 0 & 9 & 0 & 6\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Flathead sole 
 To date, no assessment or analysis has been conducted on Flathead sole. Across available data, Flathead sole have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 1,008 length observations, 0 age readings, and 859 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 980 length observations, 0 age readings, and 859 otoliths that are available to be aged. In California, since 2000, a total of 1 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 979 length observations, 0 age readings, and 859 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 10,276 length observations, 0 age readings, and 2,764 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2017 & 1 & 0 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2007 & 47 & 0 & 47 & 0 & 47\\
O & 2010 & 10 & 0 & 10 & 0 & 10\\
O & 2012 & 30 & 0 & 30 & 0 & 30\\
O & 2013 & 129 & 0 & 129 & 0 & 129\\
O & 2014 & 32 & 0 & 32 & 0 & 32\\
O & 2015 & 120 & 0 & 120 & 0 & 90\\
O & 2016 & 90 & 0 & 90 & 0 & 90\\
O & 2017 & 130 & 0 & 130 & 0 & 130\\
O & 2018 & 151 & 0 & 151 & 0 & 121\\
O & 2019 & 116 & 0 & 116 & 0 & 86\\
O & 2020 & 124 & 0 & 124 & 0 & 94\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 0 & 28 & 28 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 52 & 1521 & 0 & 1521 & 0 & 0\\
2004 & 41 & 507 & 0 & 507 & 0 & 0\\
2005 & 42 & 593 & 0 & 593 & 0 & 0\\
2006 & 50 & 421 & 0 & 421 & 0 & 0\\
2007 & 27 & 137 & 0 & 137 & 0 & 0\\
2008 & 25 & 156 & 1 & 157 & 0 & 0\\
2009 & 38 & 191 & 0 & 191 & 0 & 0\\
2010 & 55 & 393 & 13 & 406 & 0 & 0\\
2011 & 52 & 549 & 5 & 554 & 0 & 203\\
2012 & 49 & 253 & 3 & 256 & 0 & 136\\
2013 & 33 & 289 & 2 & 291 & 0 & 160\\
2014 & 77 & 945 & 35 & 980 & 0 & 519\\
2015 & 100 & 1649 & 5 & 1654 & 0 & 659\\
2016 & 75 & 1236 & 1 & 1237 & 0 & 517\\
2017 & 76 & 653 & 0 & 653 & 0 & 256\\
2018 & 65 & 515 & 1 & 516 & 0 & 229\\
2019 & 30 & 202 & 0 & 202 & 0 & 85\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Gopher/Black and yellow rockfish 
 The most recent assessment of Gopher/Black and yellow rockfish was a full assessment conducted in 2019. Across available data, Gopher/Black and yellow rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 39,905 length observations, 45 age readings, and 76 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 23,995 length observations, 45 age readings, and 75 otoliths that are available to be aged. In California, since 2000, a total of 23,844 length observations, 45 age readings, and 60 otoliths have been collected In Oregon, since 2000, a total of 151 length observations, 0 age readings, and 15 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 90,825 length observations, 0 age readings, and 41 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 90,824 length observations, 0 age readings, and 41 otoliths that are available to be aged. In California, since 2003, a total of 90,698 length observations, 0 age readings, and 26 otoliths have been collected In Oregon, since 2003, a total of 126 length observations, 0 age readings, and 15 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 17 length observations, 0 age readings, and 15 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 8 length observations, 0 age readings, and 6 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1985 & 0 & 8 & 8 & 0 & 1\\
C & 1987 & 0 & 82 & 82 & 0 & 0\\
C & 1992 & 0 & 67 & 67 & 0 & 0\\
C & 1993 & 0 & 167 & 165 & 0 & 0\\
C & 1994 & 0 & 241 & 241 & 0 & 0\\
C & 1995 & 0 & 736 & 736 & 0 & 0\\
C & 1996 & 0 & 732 & 732 & 0 & 0\\
C & 1997 & 0 & 715 & 715 & 0 & 0\\
C & 1998 & 0 & 1399 & 1399 & 0 & 0\\
C & 1999 & 0 & 1611 & 1587 & 0 & 0\\
C & 2000 & 0 & 1791 & 1768 & 0 & 0\\
C & 2001 & 0 & 598 & 552 & 0 & 0\\
C & 2002 & 0 & 368 & 356 & 0 & 0\\
C & 2003 & 0 & 342 & 326 & 0 & 0\\
C & 2004 & 0 & 389 & 371 & 0 & 0\\
C & 2005 & 0 & 500 & 404 & 0 & 0\\
C & 2006 & 0 & 527 & 156 & 0 & 0\\
C & 2007 & 0 & 640 & 488 & 0 & 0\\
C & 2008 & 0 & 319 & 167 & 0 & 0\\
C & 2009 & 0 & 325 & 250 & 0 & 0\\
C & 2010 & 1 & 336 & 300 & 0 & 0\\
C & 2011 & 1 & 275 & 274 & 0 & 0\\
C & 2012 & 0 & 342 & 289 & 0 & 0\\
C & 2013 & 0 & 278 & 238 & 0 & 0\\
C & 2014 & 0 & 152 & 67 & 0 & 0\\
C & 2015 & 0 & 224 & 222 & 0 & 0\\
C & 2016 & 0 & 405 & 405 & 0 & 0\\
C & 2017 & 0 & 575 & 574 & 0 & 0\\
C & 2018 & 2 & 595 & 596 & 0 & 0\\
C & 2019 & 5 & 535 & 540 & 0 & 0\\
C & 2020 & 3 & 620 & 620 & 0 & 0\\
C & 1988 & 1 & 0 & 1 & 0 & 0\\
C & 1992 & 0 & 700 & 700 & 0 & 0\\
C & 1993 & 0 & 1488 & 1484 & 0 & 0\\
C & 1994 & 0 & 1151 & 1151 & 0 & 0\\
C & 1995 & 0 & 811 & 811 & 0 & 0\\
C & 1996 & 0 & 2551 & 2551 & 0 & 0\\
C & 1997 & 0 & 611 & 610 & 0 & 0\\
C & 1998 & 0 & 1166 & 1166 & 0 & 0\\
C & 1999 & 0 & 1725 & 1704 & 0 & 0\\
C & 2000 & 18 & 3280 & 3155 & 0 & 0\\
C & 2001 & 1 & 1733 & 1724 & 0 & 0\\
C & 2002 & 0 & 695 & 665 & 0 & 0\\
C & 2003 & 0 & 302 & 299 & 0 & 0\\
C & 2004 & 0 & 466 & 399 & 0 & 0\\
C & 2005 & 0 & 468 & 296 & 0 & 0\\
C & 2006 & 1 & 644 & 322 & 0 & 0\\
C & 2007 & 0 & 1028 & 677 & 0 & 0\\
C & 2008 & 54 & 619 & 393 & 0 & 0\\
C & 2009 & 20 & 753 & 604 & 8 & 8\\
C & 2010 & 61 & 880 & 625 & 7 & 15\\
C & 2011 & 3 & 644 & 584 & 2 & 3\\
C & 2012 & 1 & 488 & 420 & 0 & 0\\
C & 2013 & 0 & 391 & 343 & 0 & 0\\
C & 2014 & 0 & 216 & 117 & 0 & 0\\
C & 2015 & 0 & 381 & 356 & 0 & 0\\
C & 2016 & 2 & 526 & 524 & 2 & 2\\
C & 2017 & 0 & 1015 & 1009 & 0 & 0\\
C & 2018 & 30 & 587 & 615 & 26 & 27\\
C & 2019 & 121 & 844 & 964 & 0 & 5\\
C & 2020 & 29 & 765 & 790 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 4 & 0 & 4 & 0 & 0\\
O & 2002 & 3 & 0 & 2 & 0 & 0\\
O & 2003 & 1 & 0 & 1 & 0 & 0\\
O & 2004 & 6 & 0 & 6 & 0 & 0\\
O & 2006 & 4 & 0 & 4 & 0 & 0\\
O & 2007 & 6 & 0 & 6 & 0 & 0\\
O & 2008 & 3 & 0 & 3 & 0 & 0\\
O & 2009 & 1 & 0 & 1 & 0 & 0\\
O & 2012 & 2 & 0 & 2 & 0 & 0\\
O & 2014 & 3 & 0 & 3 & 0 & 0\\
O & 2015 & 4 & 0 & 4 & 0 & 0\\
O & 2016 & 1 & 0 & 1 & 0 & 0\\
O & 2018 & 1 & 0 & 1 & 0 & 0\\
O & 2000 & 3 & 0 & 3 & 0 & 0\\
O & 2001 & 5 & 0 & 5 & 0 & 0\\
O & 2002 & 11 & 0 & 11 & 0 & 0\\
O & 2003 & 10 & 0 & 10 & 0 & 0\\
O & 2004 & 4 & 0 & 4 & 0 & 2\\
O & 2005 & 2 & 0 & 2 & 0 & 0\\
O & 2006 & 7 & 0 & 7 & 0 & 0\\
O & 2007 & 7 & 0 & 7 & 0 & 1\\
O & 2008 & 10 & 0 & 10 & 0 & 0\\
O & 2009 & 4 & 0 & 4 & 0 & 1\\
O & 2010 & 8 & 0 & 8 & 0 & 1\\
O & 2011 & 5 & 0 & 5 & 0 & 1\\
O & 2012 & 7 & 0 & 7 & 0 & 0\\
O & 2013 & 2 & 0 & 2 & 0 & 2\\
O & 2014 & 3 & 0 & 3 & 0 & 3\\
O & 2015 & 3 & 0 & 3 & 0 & 1\\
O & 2016 & 6 & 0 & 6 & 0 & 1\\
O & 2017 & 4 & 0 & 4 & 0 & 1\\
O & 2018 & 5 & 0 & 5 & 0 & 0\\
O & 2019 & 3 & 0 & 3 & 0 & 1\\
O & 2020 & 4 & 0 & 4 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 228 & 227 & 0 & 0\\
C & 2004 & 0 & 3364 & 3364 & 0 & 0\\
C & 2005 & 0 & 3912 & 3912 & 0 & 0\\
C & 2006 & 2 & 4661 & 4663 & 0 & 0\\
C & 2007 & 0 & 4376 & 4376 & 0 & 0\\
C & 2008 & 3 & 5478 & 5481 & 0 & 0\\
C & 2009 & 2 & 5996 & 5998 & 0 & 0\\
C & 2010 & 1 & 7517 & 7518 & 0 & 0\\
C & 2011 & 1 & 6663 & 6664 & 0 & 0\\
C & 2012 & 1 & 5502 & 5503 & 0 & 0\\
C & 2013 & 0 & 4783 & 4782 & 0 & 0\\
C & 2014 & 0 & 4754 & 4754 & 0 & 0\\
C & 2015 & 0 & 5549 & 5547 & 0 & 0\\
C & 2016 & 3 & 6008 & 6010 & 0 & 0\\
C & 2017 & 2 & 4373 & 4373 & 0 & 0\\
C & 2018 & 0 & 3442 & 3441 & 0 & 5\\
C & 2019 & 3 & 4044 & 4041 & 0 & 21\\
C & 2020 & 0 & 19 & 19 & 0 & 0\\
C & 2003 & 0 & 7 & 7 & 0 & 0\\
C & 2004 & 1 & 275 & 276 & 0 & 0\\
C & 2005 & 0 & 480 & 480 & 0 & 0\\
C & 2006 & 0 & 468 & 468 & 0 & 0\\
C & 2007 & 0 & 319 & 319 & 0 & 0\\
C & 2008 & 0 & 625 & 625 & 0 & 0\\
C & 2009 & 0 & 1034 & 1034 & 0 & 0\\
C & 2010 & 0 & 896 & 896 & 0 & 0\\
C & 2011 & 1 & 1040 & 1041 & 0 & 0\\
C & 2012 & 1 & 597 & 598 & 0 & 0\\
C & 2013 & 1 & 565 & 566 & 0 & 0\\
C & 2014 & 1 & 651 & 652 & 0 & 0\\
C & 2015 & 0 & 710 & 710 & 0 & 0\\
C & 2016 & 0 & 716 & 716 & 0 & 0\\
C & 2017 & 1 & 819 & 818 & 0 & 0\\
C & 2018 & 0 & 462 & 462 & 0 & 0\\
C & 2019 & 2 & 352 & 353 & 0 & 0\\
C & 2020 & 0 & 4 & 4 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2002 & 0 & 1 & 1 & 0 & 0\\
O & 2003 & 0 & 13 & 13 & 0 & 0\\
O & 2004 & 0 & 3 & 3 & 0 & 0\\
O & 2005 & 0 & 6 & 6 & 0 & 0\\
O & 2006 & 0 & 4 & 4 & 0 & 0\\
O & 2007 & 0 & 3 & 3 & 0 & 0\\
O & 2008 & 2 & 1 & 3 & 0 & 2\\
O & 2009 & 0 & 6 & 6 & 0 & 0\\
O & 2010 & 1 & 4 & 5 & 0 & 1\\
O & 2011 & 1 & 6 & 7 & 0 & 1\\
O & 2012 & 0 & 4 & 4 & 0 & 0\\
O & 2013 & 1 & 16 & 17 & 0 & 1\\
O & 2014 & 2 & 4 & 6 & 0 & 2\\
O & 2016 & 0 & 1 & 1 & 0 & 0\\
O & 2017 & 0 & 1 & 1 & 0 & \vphantom{1} 0\\
O & 2018 & 2 & 6 & 8 & 0 & 2\\
O & 2019 & 0 & 8 & 8 & 0 & 0\\
O & 2020 & 0 & 2 & 2 & 0 & 0\\
O & 2004 & 0 & 2 & 2 & 0 & 0\\
O & 2005 & 0 & 1 & 1 & 0 & 0\\
O & 2010 & 3 & 9 & 12 & 0 & 3\\
O & 2012 & 0 & 1 & 1 & 0 & 0\\
O & 2013 & 1 & 3 & 4 & 0 & 1\\
O & 2014 & 2 & 2 & 4 & 0 & 2\\
O & 2017 & 0 & 1 & 1 & 0 & 0\\
O & 2018 & 0 & 1 & 1 & 0 & 0\\
O & 2019 & 0 & 3 & 3 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2005 & 1 & 1 & 0 & 1 & 0 & 1\\
2006 & 2 & 2 & 0 & 2 & 0 & 2\\
2009 & 1 & 1 & 0 & 1 & 0 & 1\\
2010 & 2 & 3 & 0 & 3 & 0 & 3\\
2011 & 1 & 0 & 1 & 1 & 0 & 1\\
2012 & 4 & 3 & 2 & 5 & 0 & 3\\
2013 & 1 & 1 & 1 & 2 & 0 & 2\\
2017 & 1 & 0 & 1 & 1 & 0 & 1\\
2018 & 1 & 1 & 0 & 1 & 0 & 1\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2005 & 1 & 1 & 0 & 1 & 0 & 1\\
2007 & 2 & 2 & 0 & 2 & 0 & 2\\
2011 & 1 & 1 & 0 & 1 & 0 & 1\\
2014 & 1 & 2 & 0 & 2 & 0 & 0\\
2017 & 1 & 1 & 0 & 1 & 0 & 1\\
2018 & 1 & 1 & 0 & 1 & 0 & 1\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Grass rockfish 
 The most recent assessment of Grass rockfish was a data-limited assessment conducted in 2010. Across available data, Grass rockfish have been observed and sampled generally by both commercial and recreational fisheries.  
 
 Across all years of available data, commercial fisheries have collected a total of 7,776 length observations, 0 age readings, and 15 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 5,386 length observations, 0 age readings, and 15 otoliths that are available to be aged. In California, since 2000, a total of 4,560 length observations, 0 age readings, and 8 otoliths have been collected In Oregon, since 2000, a total of 826 length observations, 0 age readings, and 7 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 5,218 length observations, 0 age readings, and 16 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 5,209 length observations, 0 age readings, and 16 otoliths that are available to be aged. In California, since 2003, a total of 5,080 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 118 length observations, 0 age readings, and 7 otoliths have been collected In Washington, since 2003, a total of 11 length observations, 0 age readings, and 9 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1982 & 3 & 0 & 3 & 0 & 0\\
C & 1985 & 0 & 5 & 5 & 0 & 0\\
C & 1987 & 0 & 2 & 2 & 0 & 0\\
C & 1992 & 0 & 16 & 16 & 0 & 0\\
C & 1993 & 0 & 8 & 8 & 0 & 0\\
C & 1994 & 0 & 17 & 17 & 0 & 0\\
C & 1995 & 0 & 98 & 98 & 0 & 0\\
C & 1996 & 0 & 523 & 523 & 0 & 0\\
C & 1997 & 0 & 501 & 494 & 0 & 0\\
C & 1998 & 0 & 515 & 515 & 0 & 0\\
C & 1999 & 0 & 709 & 709 & 0 & 0\\
C & 2000 & 0 & 834 & 818 & 0 & 0\\
C & 2001 & 0 & 477 & 439 & 0 & 0\\
C & 2002 & 0 & 164 & 158 & 0 & 0\\
C & 2003 & 0 & 121 & 121 & 0 & 0\\
C & 2004 & 0 & 88 & 74 & 0 & 0\\
C & 2005 & 0 & 65 & 35 & 0 & 0\\
C & 2006 & 0 & 370 & 116 & 0 & 0\\
C & 2007 & 0 & 276 & 172 & 0 & 0\\
C & 2008 & 0 & 344 & 242 & 0 & 0\\
C & 2009 & 0 & 169 & 151 & 0 & 0\\
C & 2010 & 1 & 361 & 358 & 0 & 1\\
C & 2011 & 0 & 243 & 242 & 0 & 0\\
C & 2012 & 0 & 312 & 305 & 0 & 0\\
C & 2013 & 0 & 153 & 143 & 0 & 0\\
C & 2014 & 0 & 118 & 104 & 0 & 0\\
C & 2015 & 0 & 286 & 284 & 0 & 0\\
C & 2016 & 0 & 245 & 243 & 0 & 0\\
C & 2017 & 0 & 188 & 188 & 0 & 0\\
C & 2018 & 0 & 79 & 78 & 0 & 0\\
C & 2019 & 0 & 206 & 127 & 0 & 7\\
C & 2020 & 0 & 164 & 162 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2000 & 165 & 6 & 171 & 0 & 0\\
O & 2001 & 71 & 0 & 71 & 0 & 0\\
O & 2002 & 188 & 0 & 188 & 0 & 0\\
O & 2003 & 27 & 0 & 27 & 0 & 6\\
O & 2004 & 44 & 0 & 44 & 0 & 1\\
O & 2005 & 80 & 0 & 80 & 0 & 0\\
O & 2006 & 85 & 6 & 91 & 0 & 0\\
O & 2007 & 29 & 0 & 29 & 0 & 0\\
O & 2008 & 28 & 0 & 28 & 0 & 0\\
O & 2009 & 19 & 0 & 19 & 0 & 0\\
O & 2010 & 15 & 0 & 15 & 0 & 0\\
O & 2011 & 2 & 0 & 2 & 0 & 0\\
O & 2012 & 9 & 0 & 9 & 0 & 0\\
O & 2013 & 13 & 0 & 13 & 0 & 0\\
O & 2014 & 9 & 0 & 9 & 0 & 0\\
O & 2015 & 20 & 0 & 20 & 0 & 0\\
O & 2016 & 1 & 0 & 1 & 0 & 0\\
O & 2017 & 4 & 0 & 4 & 0 & 0\\
O & 2018 & 1 & 0 & 1 & 0 & 0\\
O & 2020 & 4 & 0 & 4 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 5 & 5 & 0 & 0\\
C & 2004 & 0 & 180 & 180 & 0 & 0\\
C & 2005 & 0 & 302 & 302 & 0 & 0\\
C & 2006 & 0 & 467 & 467 & 0 & 0\\
C & 2007 & 0 & 322 & 322 & 0 & 0\\
C & 2008 & 0 & 446 & 446 & 0 & 0\\
C & 2009 & 0 & 439 & 439 & 0 & 0\\
C & 2010 & 0 & 342 & 342 & 0 & 0\\
C & 2011 & 0 & 413 & 413 & 0 & 0\\
C & 2012 & 0 & 313 & 313 & 0 & 0\\
C & 2013 & 1 & 451 & 452 & 0 & 0\\
C & 2014 & 0 & 352 & 352 & 0 & 0\\
C & 2015 & 1 & 229 & 230 & 0 & 0\\
C & 2016 & 0 & 248 & 248 & 0 & 0\\
C & 2017 & 1 & 243 & 244 & 0 & 0\\
C & 2018 & 0 & 191 & 190 & 0 & 0\\
C & 2019 & 0 & 124 & 124 & 0 & 0\\
C & 2020 & 0 & 11 & 11 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 6 & 6 & 0 & 0\\
O & 2002 & 0 & 2 & 2 & 0 & 0\\
O & 2003 & 0 & 5 & 5 & 0 & 0\\
O & 2004 & 0 & 3 & 3 & 0 & 0\\
O & 2005 & 1 & 10 & 11 & 0 & 1\\
O & 2006 & 0 & 3 & 3 & 0 & 0\\
O & 2007 & 0 & 2 & 2 & 0 & 0\\
O & 2008 & 1 & 3 & 4 & 0 & 1\\
O & 2009 & 0 & 9 & 9 & 0 & 0\\
O & 2010 & 0 & 13 & 13 & 0 & 0\\
O & 2011 & 0 & 7 & 7 & 0 & 0\\
O & 2012 & 0 & 4 & 4 & 0 & 0\\
O & 2013 & 2 & 3 & 5 & 0 & 2\\
O & 2014 & 3 & 6 & 9 & 0 & 3\\
O & 2015 & 0 & 1 & 1 & 0 & 0\\
O & 2016 & 0 & 5 & 5 & 0 & 0\\
O & 2017 & 0 & 7 & 7 & 0 & 0\\
O & 2018 & 0 & 6 & 6 & 0 & 0\\
O & 2019 & 0 & 22 & 22 & 0 & 0\\
O & 2020 & 0 & 2 & 2 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 1 & 0 & 1 & 0 & 0\\
W & 2004 & 1 & 0 & 1 & 0 & 0\\
W & 2015 & 1 & 0 & 1 & 0 & 1\\
W & 2018 & 6 & 1 & 7 & 0 & 6\\
W & 2021 & 2 & 0 & 2 & 0 & 2\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Greenspotted rockfish 
 The most recent assessment of Greenspotted rockfish was a full assessment conducted in 2011. Across available data, Greenspotted rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 14,477 length observations, 0 age readings, and 1,749 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 3,904 length observations, 0 age readings, and 802 otoliths that are available to be aged. In California, since 2000, a total of 3,407 length observations, 0 age readings, and 323 otoliths have been collected In Oregon, since 2000, a total of 487 length observations, 0 age readings, and 479 otoliths have been collected In Washington, since 2000, a total of 10 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 17,660 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 17,660 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 17,640 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 20 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 7,568 length observations, 701 age readings, and 3,559 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 4,409 length observations, 843 age readings, and 3,483 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 88 & 56 & 144 & 0 & 95\\
C & 1981 & 84 & 123 & 207 & 0 & 83\\
C & 1982 & 83 & 41 & 124 & 0 & 103\\
C & 1983 & 115 & 179 & 293 & 0 & 112\\
C & 1984 & 133 & 174 & 307 & 0 & 138\\
C & 1985 & 274 & 234 & 508 & 0 & 370\\
C & 1986 & 145 & 407 & 552 & 0 & 43\\
C & 1987 & 223 & 140 & 363 & 0 & 1\\
C & 1988 & 164 & 111 & 275 & 0 & 0\\
C & 1989 & 123 & 161 & 284 & 0 & 0\\
C & 1990 & 95 & 97 & 192 & 0 & 0\\
C & 1991 & 74 & 422 & 496 & 0 & 1\\
C & 1992 & 67 & 966 & 1033 & 0 & 0\\
C & 1993 & 29 & 925 & 954 & 0 & 0\\
C & 1994 & 39 & 780 & 819 & 0 & 0\\
C & 1995 & 24 & 680 & 704 & 0 & 0\\
C & 1996 & 35 & 849 & 884 & 0 & 1\\
C & 1997 & 36 & 827 & 863 & 0 & 0\\
C & 1998 & 40 & 1361 & 1401 & 0 & 0\\
C & 1999 & 45 & 125 & 170 & 0 & 0\\
C & 2000 & 90 & 47 & 137 & 0 & 0\\
C & 2001 & 163 & 221 & 327 & 0 & 55\\
C & 2002 & 72 & 31 & 103 & 0 & 71\\
C & 2003 & 73 & 0 & 72 & 0 & 47\\
C & 2004 & 64 & 3 & 66 & 0 & 52\\
C & 2005 & 1 & 0 & 1 & 0 & 1\\
C & 2006 & 1 & 27 & 28 & 0 & 1\\
C & 2007 & 3 & 19 & 22 & 0 & 4\\
C & 2008 & 4 & 141 & 145 & 0 & 0\\
C & 2009 & 3 & 202 & 205 & 0 & 1\\
C & 2010 & 1 & 293 & 294 & 0 & 1\\
C & 2011 & 0 & 208 & 208 & 0 & 0\\
C & 2012 & 15 & 87 & 102 & 0 & 13\\
C & 2013 & 5 & 8 & 13 & 0 & 4\\
C & 2014 & 4 & 100 & 104 & 0 & 4\\
C & 2015 & 20 & 208 & 223 & 0 & 25\\
C & 2016 & 11 & 455 & 465 & 0 & 22\\
C & 2017 & 21 & 105 & 126 & 0 & 22\\
C & 2018 & 16 & 316 & 325 & 0 & 0\\
C & 2019 & 19 & 207 & 224 & 0 & 0\\
C & 2020 & 102 & 115 & 217 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2000 & 3 & 0 & 3 & 0 & 0\\
O & 2001 & 34 & 0 & 34 & 0 & 34\\
O & 2002 & 11 & 0 & 11 & 0 & 11\\
O & 2003 & 4 & 0 & 4 & 0 & 4\\
O & 2004 & 1 & 0 & 1 & 0 & 1\\
O & 2005 & 9 & 0 & 9 & 0 & 9\\
O & 2006 & 4 & 0 & 4 & 0 & 0\\
O & 2007 & 10 & 0 & 10 & 0 & 10\\
O & 2008 & 1 & 0 & 1 & 0 & 1\\
O & 2009 & 64 & 0 & 64 & 0 & 64\\
O & 2010 & 39 & 0 & 39 & 0 & 39\\
O & 2011 & 31 & 0 & 31 & 0 & 31\\
O & 2012 & 11 & 0 & 11 & 0 & 11\\
O & 2013 & 18 & 0 & 18 & 0 & 18\\
O & 2014 & 3 & 0 & 3 & 0 & 3\\
O & 2015 & 4 & 0 & 4 & 0 & 4\\
O & 2016 & 6 & 0 & 6 & 0 & 6\\
O & 2017 & 100 & 0 & 100 & 0 & 99\\
O & 2018 & 58 & 0 & 58 & 0 & 58\\
O & 2019 & 65 & 0 & 65 & 0 & 65\\
O & 2020 & 11 & 0 & 11 & 0 & 11\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2001 & 0 & 1 & 1 & 0 & 0\\
W & 2002 & 1 & 1 & 2 & 0 & 0\\
W & 2003 & 2 & 0 & 2 & 0 & 0\\
W & 2018 & 2 & 0 & 2 & 0 & 0\\
W & 2019 & 3 & 0 & 3 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 3 & 3 & 0 & 0\\
C & 2004 & 1 & 808 & 809 & 0 & 0\\
C & 2005 & 1 & 948 & 949 & 0 & 0\\
C & 2006 & 1 & 1426 & 1427 & 0 & 0\\
C & 2007 & 0 & 1317 & 1317 & 0 & 0\\
C & 2008 & 4 & 1427 & 1431 & 0 & 0\\
C & 2009 & 2 & 1603 & 1605 & 0 & 0\\
C & 2010 & 5 & 1343 & 1348 & 0 & 0\\
C & 2011 & 1 & 1750 & 1751 & 0 & 0\\
C & 2012 & 1 & 1258 & 1259 & 0 & 0\\
C & 2013 & 0 & 1011 & 1011 & 0 & 0\\
C & 2014 & 0 & 731 & 731 & 0 & 0\\
C & 2015 & 0 & 639 & 639 & 0 & 0\\
C & 2016 & 0 & 536 & 536 & 0 & 0\\
C & 2017 & 0 & 813 & 813 & 0 & 0\\
C & 2018 & 4 & 826 & 830 & 0 & 0\\
C & 2019 & 7 & 1097 & 1102 & 0 & 0\\
C & 2020 & 0 & 79 & 79 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2004 & 0 & 10 & 10 & 0 & 0\\
O & 2019 & 0 & 10 & 10 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 33 & 420 & 0 & 420 & 0 & 0\\
2004 & 26 & 553 & 9 & 562 & 0 & 278\\
2005 & 29 & 216 & 9 & 225 & 85 & 42\\
2006 & 43 & 459 & 4 & 463 & 0 & 261\\
2007 & 40 & 622 & 31 & 653 & 265 & 42\\
2008 & 41 & 512 & 45 & 557 & 0 & 244\\
2009 & 49 & 650 & 28 & 678 & 351 & 26\\
2010 & 37 & 670 & 41 & 711 & 0 & 395\\
2011 & 34 & 259 & 9 & 268 & 0 & 225\\
2012 & 35 & 393 & 8 & 401 & 0 & 235\\
2013 & 25 & 245 & 1 & 246 & 0 & 218\\
2014 & 36 & 290 & 6 & 296 & 0 & 221\\
2015 & 26 & 243 & 6 & 249 & 0 & 209\\
2016 & 40 & 883 & 16 & 899 & 0 & 464\\
2017 & 28 & 333 & 1 & 334 & 0 & 213\\
2018 & 36 & 377 & 21 & 398 & 0 & 322\\
2019 & 17 & 207 & 1 & 208 & 0 & 164\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 38 & 223 & 0 & 223 & 200 & 0\\
2005 & 45 & 125 & 4 & 128 & 0 & 129\\
2006 & 51 & 209 & 3 & 210 & 206 & 6\\
2007 & 45 & 190 & 0 & 189 & 0 & 187\\
2008 & 58 & 232 & 3 & 234 & 230 & 2\\
2009 & 59 & 272 & 3 & 273 & 0 & 273\\
2010 & 55 & 208 & 0 & 208 & 207 & 0\\
2011 & 47 & 244 & 2 & 244 & 0 & 242\\
2012 & 48 & 222 & 2 & 223 & 0 & 222\\
2013 & 52 & 271 & 2 & 271 & 0 & 268\\
2014 & 70 & 446 & 4 & 449 & 0 & 446\\
2015 & 74 & 378 & 1 & 378 & 0 & 375\\
2016 & 68 & 272 & 2 & 274 & 0 & 268\\
2017 & 75 & 377 & 4 & 377 & 0 & 362\\
2018 & 83 & 382 & 1 & 381 & 0 & 372\\
2019 & 78 & 345 & 5 & 347 & 0 & 331\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Greenstriped rockfish 
 The most recent assessment of Greenstriped rockfish was a full assessment conducted in 2009. Across available data, Greenstriped rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 21,228 length observations, 0 age readings, and 7,024 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 14,219 length observations, 0 age readings, and 6,043 otoliths that are available to be aged. In California, since 2000, a total of 2,798 length observations, 0 age readings, and 335 otoliths have been collected In Oregon, since 2000, a total of 6,329 length observations, 0 age readings, and 5,708 otoliths have been collected In Washington, since 2000, a total of 5,092 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 2,777 length observations, 0 age readings, and 67 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 2,589 length observations, 0 age readings, and 67 otoliths that are available to be aged. In California, since 2003, a total of 2,148 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 334 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 107 length observations, 0 age readings, and 67 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 39,364 length observations, 3,413 age readings, and 7,513 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 686 length observations, 0 age readings, and 674 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 52 & 7 & 58 & 0 & 42\\
C & 1981 & 24 & 5 & 29 & 0 & 26\\
C & 1982 & 122 & 0 & 122 & 0 & 58\\
C & 1983 & 195 & 7 & 202 & 0 & 194\\
C & 1984 & 163 & 31 & 194 & 0 & 161\\
C & 1985 & 418 & 35 & 453 & 0 & 428\\
C & 1986 & 147 & 10 & 157 & 0 & 58\\
C & 1987 & 159 & 23 & 182 & 0 & 6\\
C & 1988 & 115 & 17 & 132 & 0 & 0\\
C & 1989 & 144 & 120 & 264 & 0 & 2\\
C & 1990 & 139 & 8 & 147 & 0 & 1\\
C & 1991 & 135 & 8 & 143 & 0 & 2\\
C & 1992 & 45 & 77 & 122 & 0 & 0\\
C & 1993 & 62 & 63 & 125 & 0 & 0\\
C & 1994 & 82 & 187 & 269 & 0 & 0\\
C & 1995 & 149 & 96 & 245 & 0 & 0\\
C & 1996 & 89 & 238 & 327 & 0 & 0\\
C & 1997 & 263 & 143 & 406 & 0 & 0\\
C & 1998 & 246 & 231 & 477 & 0 & 0\\
C & 1999 & 198 & 62 & 260 & 0 & 1\\
C & 2000 & 401 & 3 & 404 & 0 & 0\\
C & 2001 & 297 & 23 & 320 & 0 & 110\\
C & 2002 & 122 & 7 & 129 & 0 & 44\\
C & 2003 & 10 & 70 & 80 & 0 & 9\\
C & 2004 & 37 & 240 & 218 & 0 & 36\\
C & 2005 & 37 & 1 & 38 & 0 & 36\\
C & 2006 & 44 & 5 & 49 & 0 & 22\\
C & 2007 & 41 & 1 & 42 & 0 & 4\\
C & 2008 & 1 & 18 & 19 & 0 & 0\\
C & 2009 & 5 & 67 & 72 & 0 & 5\\
C & 2010 & 0 & 21 & 21 & 0 & 0\\
C & 2011 & 1 & 25 & 26 & 0 & 1\\
C & 2012 & 0 & 141 & 8 & 0 & 0\\
C & 2013 & 29 & 120 & 40 & 0 & 24\\
C & 2014 & 24 & 409 & 77 & 0 & 7\\
C & 2015 & 16 & 577 & 96 & 0 & 7\\
C & 2016 & 263 & 100 & 363 & 0 & 25\\
C & 2017 & 108 & 206 & 274 & 0 & 5\\
C & 2018 & 225 & 62 & 257 & 0 & 0\\
C & 2019 & 107 & 60 & 113 & 0 & 0\\
C & 2020 & 20 & 132 & 152 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1995 & 2 & 0 & 2 & 0 & 2\\
O & 1996 & 264 & 0 & 264 & 0 & 0\\
O & 1997 & 368 & 0 & 368 & 0 & 0\\
O & 1998 & 121 & 0 & 121 & 0 & 0\\
O & 1999 & 359 & 0 & 359 & 0 & 0\\
O & 2000 & 95 & 0 & 95 & 0 & 78\\
O & 2001 & 275 & 0 & 275 & 0 & 98\\
O & 2002 & 35 & 0 & 35 & 0 & 30\\
O & 2003 & 114 & 0 & 114 & 0 & 114\\
O & 2004 & 56 & 0 & 56 & 0 & 50\\
O & 2005 & 30 & 0 & 30 & 0 & 30\\
O & 2006 & 192 & 0 & 192 & 0 & 155\\
O & 2007 & 86 & 0 & 86 & 0 & 85\\
O & 2008 & 109 & 0 & 109 & 0 & 109\\
O & 2009 & 347 & 0 & 347 & 0 & 324\\
O & 2010 & 162 & 0 & 162 & 0 & 102\\
O & 2011 & 310 & 3 & 313 & 0 & 309\\
O & 2012 & 491 & 7 & 498 & 0 & 485\\
O & 2013 & 451 & 7 & 458 & 0 & 450\\
O & 2014 & 576 & 0 & 576 & 0 & 543\\
O & 2015 & 476 & 2 & 477 & 0 & 448\\
O & 2016 & 572 & 0 & 572 & 0 & 506\\
O & 2017 & 608 & 0 & 608 & 0 & 546\\
O & 2018 & 514 & 1 & 514 & 0 & 438\\
O & 2019 & 612 & 1 & 613 & 0 & 609\\
O & 2020 & 198 & 1 & 199 & 0 & 199\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 539 & 539 & 0 & 0\\
W & 1997 & 0 & 386 & 386 & 0 & 0\\
W & 1998 & 514 & 3 & 517 & 0 & 0\\
W & 1999 & 137 & 2 & 139 & 0 & 0\\
W & 2000 & 175 & 3 & 178 & 0 & 0\\
W & 2001 & 216 & 166 & 382 & 0 & 0\\
W & 2002 & 2187 & 65 & 2252 & 0 & 0\\
W & 2003 & 435 & 21 & 456 & 0 & 0\\
W & 2004 & 300 & 2 & 302 & 0 & 0\\
W & 2005 & 86 & 0 & 86 & 0 & 0\\
W & 2006 & 90 & 0 & 90 & 0 & 0\\
W & 2007 & 3 & 0 & 3 & 0 & 0\\
W & 2009 & 62 & 0 & 62 & 0 & 0\\
W & 2010 & 21 & 0 & 21 & 0 & 0\\
W & 2011 & 12 & 0 & 12 & 0 & 0\\
W & 2012 & 184 & 1 & 185 & 0 & 0\\
W & 2013 & 161 & 1 & 162 & 0 & 0\\
W & 2014 & 151 & 9 & 160 & 0 & 0\\
W & 2015 & 28 & 0 & 28 & 0 & 0\\
W & 2016 & 2 & 1 & 3 & 0 & 0\\
W & 2017 & 105 & 0 & 105 & 0 & 0\\
W & 2018 & 187 & 0 & 187 & 0 & 0\\
W & 2019 & 267 & 1 & 268 & 0 & 0\\
W & 2020 & 143 & 7 & 150 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 42 & 42 & 0 & 0\\
C & 2005 & 0 & 53 & 53 & 0 & 0\\
C & 2006 & 0 & 100 & 100 & 0 & 0\\
C & 2007 & 0 & 129 & 129 & 0 & 0\\
C & 2008 & 0 & 142 & 142 & 0 & 0\\
C & 2009 & 0 & 263 & 263 & 0 & 0\\
C & 2010 & 1 & 165 & 166 & 0 & 0\\
C & 2011 & 1 & 254 & 255 & 0 & 0\\
C & 2012 & 0 & 129 & 129 & 0 & 0\\
C & 2013 & 0 & 164 & 164 & 0 & 0\\
C & 2014 & 0 & 120 & 120 & 0 & 0\\
C & 2015 & 0 & 88 & 88 & 0 & 0\\
C & 2016 & 0 & 145 & 145 & 0 & 0\\
C & 2017 & 0 & 98 & 98 & 0 & 0\\
C & 2018 & 0 & 82 & 82 & 0 & 0\\
C & 2019 & 0 & 165 & 165 & 0 & 0\\
C & 2020 & 0 & 7 & 7 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 79 & 79 & 0 & 0\\
O & 2002 & 0 & 109 & 109 & 0 & 0\\
O & 2003 & 0 & 141 & 141 & 0 & 0\\
O & 2004 & 0 & 45 & 45 & 0 & 0\\
O & 2005 & 0 & 24 & 24 & 0 & 0\\
O & 2006 & 0 & 22 & 22 & 0 & 0\\
O & 2007 & 0 & 2 & 2 & 0 & 0\\
O & 2008 & 0 & 3 & 3 & 0 & 0\\
O & 2009 & 0 & 3 & 3 & 0 & 0\\
O & 2011 & 0 & 8 & 8 & 0 & 0\\
O & 2012 & 0 & 3 & 3 & 0 & 0\\
O & 2013 & 0 & 10 & 10 & 0 & 0\\
O & 2015 & 0 & 1 & 1 & 0 & 0\\
O & 2016 & 0 & 3 & 3 & 0 & 0\\
O & 2017 & 0 & 12 & 12 & 0 & 0\\
O & 2018 & 0 & 14 & 14 & 0 & 0\\
O & 2019 & 0 & 40 & 40 & 0 & 0\\
O & 2020 & 0 & 3 & 3 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2003 & 4 & 0 & 4 & 0 & 0\\
W & 2004 & 2 & 3 & 5 & 0 & 0\\
W & 2005 & 0 & 1 & 1 & 0 & 0\\
W & 2011 & 1 & 1 & 2 & 0 & 1\\
W & 2012 & 0 & 1 & 1 & 0 & 0\\
W & 2014 & 2 & 0 & 2 & 0 & 2\\
W & 2016 & 4 & 0 & 4 & 0 & 4\\
W & 2017 & 29 & 1 & 30 & 0 & 29\\
W & 2018 & 5 & 4 & 9 & 0 & 5\\
W & 2019 & 16 & 23 & 39 & 0 & 16\\
W & 2020 & 1 & 0 & 1 & 0 & 1\\
W & 2021 & 9 & 0 & 9 & 0 & 9\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 131 & 3489 & 37 & 3526 & 577 & 33\\
2004 & 136 & 2654 & 13 & 2667 & 522 & 7\\
2005 & 175 & 3871 & 27 & 3898 & 568 & 131\\
2006 & 179 & 3116 & 16 & 3132 & 524 & 155\\
2007 & 167 & 2325 & 34 & 2359 & 657 & 5\\
2008 & 149 & 1805 & 60 & 1865 & 565 & 3\\
2009 & 164 & 1718 & 82 & 1800 & 0 & 614\\
2010 & 189 & 2156 & 166 & 2322 & 0 & 747\\
2011 & 184 & 1810 & 123 & 1933 & 0 & 646\\
2012 & 186 & 2099 & 117 & 2216 & 0 & 753\\
2013 & 125 & 1449 & 23 & 1472 & 0 & 494\\
2014 & 180 & 2259 & 15 & 2274 & 0 & 682\\
2015 & 174 & 1988 & 30 & 2018 & 0 & 693\\
2016 & 164 & 2302 & 17 & 2319 & 0 & 736\\
2017 & 169 & 1890 & 29 & 1919 & 0 & 646\\
2018 & 198 & 2395 & 83 & 2477 & 0 & 786\\
2019 & 96 & 1163 & 4 & 1167 & 0 & 382\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 6 & 8 & 0 & 8 & 0 & 7\\
2005 & 7 & 8 & 0 & 8 & 0 & 8\\
2006 & 8 & 15 & 0 & 15 & 0 & 15\\
2007 & 11 & 19 & 0 & 19 & 0 & 19\\
2008 & 12 & 22 & 0 & 22 & 0 & 22\\
2009 & 16 & 26 & 0 & 26 & 0 & 24\\
2010 & 15 & 31 & 0 & 31 & 0 & 31\\
2011 & 10 & 19 & 0 & 19 & 0 & 17\\
2012 & 13 & 24 & 0 & 24 & 0 & 24\\
2013 & 16 & 27 & 1 & 27 & 0 & 27\\
2014 & 28 & 46 & 1 & 46 & 0 & 46\\
2015 & 33 & 66 & 0 & 66 & 0 & 65\\
2016 & 23 & 41 & 1 & 41 & 0 & 41\\
2017 & 43 & 111 & 0 & 111 & 0 & 111\\
2018 & 44 & 124 & 0 & 124 & 0 & 124\\
2019 & 44 & 99 & 0 & 99 & 0 & 93\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Honeycomb rockfish 
 The most recent assessment of Honeycomb rockfish was a data-limited assessment conducted in 2010. Across available data, Honeycomb rockfish have been observed and sampled generally by recreational fisheries and the both the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, recreational fisheries have collected a total of 14,042 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 14,042 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 14,042 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 289 length observations, 0 age readings, and 176 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 203 length observations, 0 age readings, and 176 otoliths that are available to be aged. 

## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 54 & 54 & 0 & 0\\
C & 2004 & 0 & 492 & 492 & 0 & 0\\
C & 2005 & 6 & 907 & 913 & 0 & 0\\
C & 2006 & 1 & 812 & 813 & 0 & 0\\
C & 2007 & 4 & 957 & 961 & 0 & 0\\
C & 2008 & 1 & 1100 & 1101 & 0 & 0\\
C & 2009 & 15 & 1221 & 1236 & 0 & 0\\
C & 2010 & 16 & 1613 & 1629 & 0 & 0\\
C & 2011 & 11 & 1490 & 1501 & 0 & 0\\
C & 2012 & 0 & 849 & 849 & 0 & 0\\
C & 2013 & 0 & 1340 & 1340 & 0 & 0\\
C & 2014 & 1 & 652 & 653 & 0 & 0\\
C & 2015 & 0 & 559 & 558 & 0 & 0\\
C & 2016 & 0 & 496 & 496 & 0 & 0\\
C & 2017 & 0 & 606 & 606 & 0 & 0\\
C & 2018 & 0 & 366 & 366 & 0 & 0\\
C & 2019 & 4 & 443 & 447 & 0 & 0\\
C & 2020 & 0 & 27 & 27 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 1 & 1 & 0 & 1 & 0 & 0\\
2005 & 1 & 14 & 0 & 14 & 0 & 14\\
2006 & 2 & 14 & 0 & 14 & 0 & 14\\
2007 & 3 & 68 & 0 & 68 & 0 & 31\\
2008 & 1 & 1 & 0 & 1 & 0 & 1\\
2009 & 1 & 90 & 0 & 90 & 0 & 15\\
2010 & 1 & 22 & 0 & 22 & 0 & 22\\
2012 & 3 & 6 & 0 & 6 & 0 & 6\\
2013 & 3 & 36 & 0 & 36 & 0 & 36\\
2014 & 1 & 1 & 0 & 1 & 0 & 1\\
2016 & 1 & 2 & 2 & 4 & 0 & 4\\
2017 & 2 & 4 & 0 & 4 & 0 & 4\\
2018 & 1 & 14 & 0 & 14 & 0 & 14\\
2019 & 1 & 14 & 0 & 14 & 0 & 14\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 3 & 3 & 0 & 3 & 0 & 3\\
2005 & 4 & 7 & 0 & 7 & 0 & 7\\
2006 & 4 & 5 & 1 & 6 & 0 & 6\\
2007 & 4 & 9 & 0 & 9 & 0 & 9\\
2008 & 8 & 28 & 0 & 28 & 0 & 23\\
2009 & 11 & 20 & 0 & 20 & 0 & 18\\
2010 & 11 & 19 & 1 & 20 & 0 & 13\\
2011 & 7 & 12 & 1 & 13 & 0 & 12\\
2012 & 3 & 3 & 0 & 3 & 0 & 2\\
2013 & 9 & 25 & 1 & 26 & 0 & 25\\
2014 & 4 & 7 & 0 & 7 & 0 & 7\\
2015 & 7 & 17 & 1 & 18 & 0 & 14\\
2016 & 7 & 16 & 0 & 16 & 0 & 13\\
2017 & 6 & 11 & 0 & 11 & 0 & 8\\
2018 & 8 & 9 & 0 & 9 & 0 & 9\\
2019 & 4 & 7 & 0 & 7 & 0 & 7\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Kelp greenling 
 The most recent assessment of Kelp greenling was a full assessment conducted in 2015. Across available data, Kelp greenling have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 42,794 length observations, 282 age readings, and 560 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 40,518 length observations, 282 age readings, and 560 otoliths that are available to be aged. In California, since 2000, a total of 2,986 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 37,531 length observations, 282 age readings, and 560 otoliths have been collected In Washington, since 2000, a total of 1 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 48,473 length observations, 4,094 age readings, and 3,920 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 46,576 length observations, 4,094 age readings, and 3,920 otoliths that are available to be aged. In California, since 2003, a total of 10,357 length observations, 0 age readings, and 7 otoliths have been collected In Oregon, since 2003, a total of 31,014 length observations, 2,786 age readings, and 2,929 otoliths have been collected In Washington, since 2003, a total of 5,205 length observations, 1,308 age readings, and 984 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 800 length observations, 0 age readings, and 618 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1992 & 0 & 9 & 9 & 0 & 0\\
C & 1993 & 0 & 24 & 24 & 0 & 0\\
C & 1994 & 0 & 18 & 18 & 0 & 0\\
C & 1995 & 0 & 90 & 90 & 0 & 0\\
C & 1996 & 0 & 266 & 266 & 0 & 0\\
C & 1997 & 83 & 271 & 346 & 0 & 0\\
C & 1998 & 0 & 148 & 148 & 0 & 0\\
C & 1999 & 14 & 1004 & 1018 & 0 & 0\\
C & 2000 & 754 & 490 & 1234 & 0 & 0\\
C & 2001 & 328 & 242 & 566 & 0 & 0\\
C & 2002 & 154 & 18 & 172 & 0 & 0\\
C & 2003 & 1 & 40 & 41 & 0 & 0\\
C & 2004 & 69 & 1 & 65 & 0 & 0\\
C & 2005 & 55 & 43 & 84 & 0 & 0\\
C & 2006 & 63 & 1 & 35 & 0 & 0\\
C & 2007 & 120 & 12 & 94 & 0 & 0\\
C & 2008 & 65 & 0 & 47 & 0 & 0\\
C & 2009 & 70 & 6 & 60 & 0 & 0\\
C & 2010 & 74 & 24 & 71 & 0 & 0\\
C & 2011 & 51 & 4 & 52 & 0 & 0\\
C & 2012 & 50 & 16 & 47 & 0 & 0\\
C & 2013 & 54 & 37 & 38 & 0 & 0\\
C & 2014 & 38 & 5 & 17 & 0 & 0\\
C & 2015 & 69 & 15 & 69 & 0 & 0\\
C & 2016 & 119 & 11 & 119 & 0 & 0\\
C & 2017 & 67 & 5 & 72 & 0 & 0\\
C & 2018 & 20 & 10 & 30 & 0 & 0\\
C & 2019 & 39 & 1 & 39 & 0 & 0\\
C & 2020 & 34 & 0 & 34 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1998 & 165 & 0 & 165 & 0 & 0\\
O & 1999 & 192 & 0 & 192 & 0 & 0\\
O & 2000 & 1442 & 0 & 1442 & 0 & 0\\
O & 2001 & 2898 & 0 & 2898 & 0 & 0\\
O & 2002 & 3868 & 0 & 3868 & 0 & 0\\
O & 2003 & 1695 & 0 & 1695 & 20 & 11\\
O & 2004 & 2561 & 0 & 2561 & 2 & 0\\
O & 2005 & 1638 & 0 & 1638 & 0 & 0\\
O & 2006 & 1992 & 1 & 1993 & 0 & 0\\
O & 2007 & 2068 & 0 & 2068 & 10 & 7\\
O & 2008 & 1539 & 0 & 1539 & 4 & 0\\
O & 2009 & 1146 & 0 & 1146 & 20 & 3\\
O & 2010 & 1823 & 5 & 1828 & 29 & 13\\
O & 2011 & 2546 & 0 & 2546 & 56 & 35\\
O & 2012 & 1598 & 0 & 1597 & 84 & 16\\
O & 2013 & 2365 & 17 & 2382 & 57 & 42\\
O & 2014 & 1904 & 0 & 1904 & 0 & 115\\
O & 2015 & 1463 & 0 & 1463 & 0 & 56\\
O & 2016 & 1242 & 0 & 1242 & 0 & 57\\
O & 2017 & 977 & 0 & 977 & 0 & 99\\
O & 2018 & 1239 & 0 & 1239 & 0 & 28\\
O & 2019 & 899 & 0 & 899 & 0 & 67\\
O & 2020 & 606 & 0 & 606 & 0 & 11\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2018 & 1 & 0 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 13 & 18 & 31 & 0 & 0\\
C & 2004 & 120 & 400 & 520 & 0 & 0\\
C & 2005 & 153 & 298 & 451 & 0 & 0\\
C & 2006 & 352 & 255 & 607 & 0 & 0\\
C & 2007 & 224 & 190 & 414 & 0 & 0\\
C & 2008 & 309 & 353 & 663 & 0 & 0\\
C & 2009 & 455 & 492 & 948 & 0 & 0\\
C & 2010 & 413 & 341 & 754 & 0 & 0\\
C & 2011 & 616 & 272 & 888 & 0 & 0\\
C & 2012 & 490 & 250 & 740 & 0 & 0\\
C & 2013 & 655 & 105 & 760 & 0 & 0\\
C & 2014 & 574 & 66 & 640 & 0 & 0\\
C & 2015 & 908 & 59 & 967 & 0 & 0\\
C & 2016 & 634 & 44 & 678 & 0 & 0\\
C & 2017 & 512 & 28 & 540 & 0 & 0\\
C & 2018 & 442 & 29 & 471 & 0 & 6\\
C & 2019 & 259 & 12 & 270 & 0 & 1\\
C & 2020 & 15 & 0 & 15 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 515 & 515 & 0 & 0\\
O & 2002 & 0 & 1280 & 1280 & 0 & 0\\
O & 2003 & 2 & 1369 & 1371 & 0 & 0\\
O & 2004 & 2 & 1092 & 1094 & 0 & 0\\
O & 2005 & 153 & 1560 & 1713 & 294 & 0\\
O & 2006 & 381 & 1355 & 1736 & 467 & 0\\
O & 2007 & 338 & 1314 & 1652 & 334 & 0\\
O & 2008 & 480 & 1835 & 2315 & 470 & 0\\
O & 2009 & 520 & 1825 & 2345 & 257 & 259\\
O & 2010 & 559 & 2583 & 3142 & 271 & 273\\
O & 2011 & 703 & 2499 & 3202 & 289 & 405\\
O & 2012 & 663 & 2431 & 3094 & 220 & 441\\
O & 2013 & 560 & 2308 & 2868 & 184 & 367\\
O & 2014 & 270 & 1050 & 1320 & 0 & 269\\
O & 2015 & 171 & 961 & 1132 & 0 & 170\\
O & 2016 & 164 & 783 & 947 & 0 & 164\\
O & 2017 & 125 & 708 & 833 & 0 & 123\\
O & 2018 & 131 & 865 & 996 & 0 & 122\\
O & 2019 & 187 & 848 & 1035 & 0 & 183\\
O & 2020 & 153 & 66 & 219 & 0 & 153\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 9 & 93 & 102 & 0 & 0\\
W & 2003 & 7 & 147 & 154 & 0 & 0\\
W & 2004 & 279 & 164 & 443 & 55 & 14\\
W & 2005 & 270 & 282 & 552 & 232 & 20\\
W & 2006 & 123 & 226 & 349 & 105 & 5\\
W & 2007 & 102 & 67 & 169 & 96 & 5\\
W & 2008 & 96 & 109 & 205 & 92 & 4\\
W & 2009 & 83 & 131 & 214 & 51 & 0\\
W & 2010 & 78 & 117 & 195 & 44 & 18\\
W & 2011 & 50 & 172 & 222 & 39 & 1\\
W & 2012 & 30 & 162 & 192 & 22 & 0\\
W & 2013 & 31 & 114 & 145 & 10 & 0\\
W & 2014 & 165 & 120 & 285 & 165 & 0\\
W & 2015 & 117 & 40 & 157 & 106 & 11\\
W & 2016 & 319 & 40 & 359 & 291 & 28\\
W & 2017 & 212 & 192 & 404 & 0 & 210\\
W & 2018 & 229 & 190 & 419 & 0 & 227\\
W & 2019 & 205 & 264 & 469 & 0 & 204\\
W & 2020 & 115 & 8 & 123 & 0 & 114\\
W & 2021 & 123 & 26 & 149 & 0 & 123\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 10 & 61 & 0 & 61 & 0 & 0\\
2004 & 12 & 97 & 0 & 97 & 0 & 0\\
2005 & 9 & 42 & 0 & 42 & 0 & 41\\
2006 & 7 & 23 & 0 & 23 & 0 & 23\\
2007 & 7 & 39 & 0 & 39 & 0 & 39\\
2008 & 4 & 19 & 0 & 19 & 0 & 19\\
2009 & 9 & 27 & 0 & 27 & 0 & 27\\
2010 & 16 & 46 & 0 & 46 & 0 & 46\\
2011 & 21 & 105 & 0 & 105 & 0 & 103\\
2012 & 11 & 28 & 0 & 28 & 0 & 27\\
2013 & 14 & 25 & 1 & 26 & 0 & 26\\
2014 & 14 & 42 & 0 & 42 & 0 & 39\\
2015 & 8 & 38 & 0 & 38 & 0 & 37\\
2016 & 10 & 80 & 0 & 80 & 0 & 72\\
2017 & 13 & 49 & 0 & 49 & 0 & 45\\
2018 & 16 & 72 & 0 & 72 & 0 & 68\\
2019 & 2 & 6 & 0 & 6 & 0 & 6\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Kelp rockfish 
 The most recent assessment of Kelp rockfish was a data-limited assessment conducted in 2010. Across available data, Kelp rockfish have been observed and sampled generally by both commercial and recreational fisheries.  
 
 Across all years of available data, commercial fisheries have collected a total of 1,537 length observations, 0 age readings, and 1 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 685 length observations, 0 age readings, and 1 otoliths that are available to be aged. In California, since 2000, a total of 685 length observations, 0 age readings, and 1 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 13,134 length observations, 0 age readings, and 1 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 13,134 length observations, 0 age readings, and 1 otoliths that are available to be aged. In California, since 2003, a total of 13,134 length observations, 0 age readings, and 1 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1987 & 0 & 19 & 19 & 0 & 0\\
C & 1992 & 0 & 28 & 28 & 0 & 0\\
C & 1993 & 0 & 148 & 148 & 0 & 0\\
C & 1994 & 0 & 92 & 92 & 0 & 0\\
C & 1995 & 0 & 114 & 114 & 0 & 0\\
C & 1996 & 0 & 95 & 95 & 0 & 0\\
C & 1997 & 0 & 66 & 66 & 0 & 0\\
C & 1998 & 0 & 140 & 140 & 0 & 0\\
C & 1999 & 0 & 150 & 150 & 0 & 0\\
C & 2000 & 1 & 73 & 74 & 0 & 0\\
C & 2001 & 0 & 17 & 17 & 0 & 0\\
C & 2002 & 0 & 13 & 13 & 0 & 0\\
C & 2003 & 0 & 48 & 47 & 0 & 0\\
C & 2004 & 0 & 10 & 5 & 0 & 0\\
C & 2005 & 0 & 7 & 3 & 0 & 0\\
C & 2006 & 0 & 8 & 4 & 0 & 0\\
C & 2007 & 0 & 26 & 22 & 0 & 0\\
C & 2008 & 0 & 3 & 1 & 0 & 0\\
C & 2009 & 0 & 31 & 30 & 0 & 0\\
C & 2010 & 0 & 44 & 44 & 0 & 0\\
C & 2011 & 0 & 26 & 26 & 0 & 0\\
C & 2012 & 0 & 2 & 2 & 0 & 0\\
C & 2013 & 0 & 23 & 23 & 0 & 0\\
C & 2014 & 0 & 3 & 3 & 0 & 0\\
C & 2015 & 0 & 47 & 47 & 0 & 0\\
C & 2016 & 0 & 85 & 85 & 0 & 0\\
C & 2017 & 1 & 84 & 85 & 0 & 1\\
C & 2018 & 0 & 82 & 82 & 0 & 0\\
C & 2019 & 2 & 17 & 19 & 0 & 0\\
C & 2020 & 0 & 53 & 53 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 14 & 14 & 0 & 0\\
C & 2004 & 0 & 393 & 393 & 0 & 0\\
C & 2005 & 0 & 521 & 521 & 0 & 0\\
C & 2006 & 1 & 740 & 741 & 0 & 0\\
C & 2007 & 2 & 499 & 501 & 0 & 0\\
C & 2008 & 0 & 650 & 650 & 0 & 0\\
C & 2009 & 0 & 601 & 601 & 0 & 0\\
C & 2010 & 0 & 666 & 666 & 0 & 0\\
C & 2011 & 3 & 1154 & 1157 & 0 & 0\\
C & 2012 & 1 & 1332 & 1333 & 0 & 0\\
C & 2013 & 1 & 1383 & 1384 & 0 & 0\\
C & 2014 & 1 & 1284 & 1285 & 0 & 0\\
C & 2015 & 0 & 931 & 931 & 0 & 0\\
C & 2016 & 1 & 779 & 780 & 0 & 0\\
C & 2017 & 0 & 1131 & 1130 & 0 & 0\\
C & 2018 & 3 & 624 & 626 & 0 & 0\\
C & 2019 & 0 & 421 & 416 & 0 & 1\\
C & 2020 & 0 & 5 & 5 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Leopard shark 
 The most recent assessment of Leopard shark was a data-limited assessment conducted in 2010. Across available data, Leopard shark have been observed and sampled generally by  
 
 Across all years of available data, recreational fisheries have collected a total of 2,094 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 2,094 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 2,094 length observations, 0 age readings, and 0 otoliths have been collected 



## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 2 & 1 & 3 & 0 & 0\\
C & 2004 & 4 & 178 & 182 & 0 & 0\\
C & 2005 & 4 & 155 & 159 & 0 & 0\\
C & 2006 & 7 & 308 & 315 & 0 & 0\\
C & 2007 & 19 & 173 & 192 & 0 & 0\\
C & 2008 & 13 & 83 & 96 & 0 & 0\\
C & 2009 & 159 & 100 & 259 & 0 & 0\\
C & 2010 & 66 & 62 & 128 & 0 & 0\\
C & 2011 & 8 & 68 & 76 & 0 & 0\\
C & 2012 & 1 & 74 & 75 & 0 & 0\\
C & 2013 & 7 & 82 & 88 & 0 & 0\\
C & 2014 & 41 & 43 & 84 & 0 & 0\\
C & 2015 & 60 & 18 & 78 & 0 & 0\\
C & 2016 & 73 & 30 & 103 & 0 & 0\\
C & 2017 & 101 & 14 & 114 & 0 & 0\\
C & 2018 & 64 & 24 & 88 & 0 & 0\\
C & 2019 & 50 & 5 & 53 & 0 & 0\\
C & 2020 & 1 & 0 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Lingcod 
 The most recent assessment of Lingcod was a full assessment conducted in 2021. Across available data, Lingcod have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 94,009 length observations, 14,008 age readings, and 3,838 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 51,013 length observations, 6,436 age readings, and 1,882 otoliths that are available to be aged. In California, since 2000, a total of 10,414 length observations, 681 age readings, and 113 otoliths have been collected In Oregon, since 2000, a total of 30,534 length observations, 5,302 age readings, and 1,769 otoliths have been collected In Washington, since 2000, a total of 10,065 length observations, 453 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 249,990 length observations, 24,540 age readings, and 14,150 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 237,413 length observations, 21,969 age readings, and 12,982 otoliths that are available to be aged. In California, since 2003, a total of 91,576 length observations, 0 age readings, and 328 otoliths have been collected In Oregon, since 2003, a total of 118,214 length observations, 7,176 age readings, and 10,234 otoliths have been collected In Washington, since 2003, a total of 27,623 length observations, 14,793 age readings, and 2,420 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 25,277 length observations, 9,056 age readings, and 4,499 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 825 length observations, 0 age readings, and 50 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 1617 & 9 & 1616 & 0 & 0\\
C & 1981 & 1835 & 0 & 1835 & 0 & 0\\
C & 1982 & 412 & 0 & 412 & 0 & 0\\
C & 1983 & 369 & 14 & 383 & 0 & 0\\
C & 1984 & 238 & 0 & 238 & 0 & 0\\
C & 1985 & 70 & 0 & 70 & 0 & 0\\
C & 1986 & 84 & 1 & 85 & 0 & 0\\
C & 1987 & 146 & 0 & 146 & 0 & 0\\
C & 1988 & 261 & 0 & 261 & 0 & 0\\
C & 1989 & 106 & 12 & 118 & 0 & 0\\
C & 1990 & 0 & 3 & 3 & 0 & 0\\
C & 1992 & 117 & 2 & 119 & 0 & 0\\
C & 1993 & 1339 & 242 & 1579 & 1070 & 0\\
C & 1994 & 881 & 62 & 943 & 791 & 0\\
C & 1995 & 559 & 51 & 610 & 345 & 0\\
C & 1996 & 481 & 265 & 746 & 417 & 0\\
C & 1997 & 984 & 180 & 1164 & 873 & 0\\
C & 1998 & 348 & 78 & 426 & 319 & 0\\
C & 1999 & 295 & 322 & 617 & 0 & 0\\
C & 2000 & 181 & 100 & 274 & 0 & 0\\
C & 2001 & 214 & 175 & 386 & 183 & 0\\
C & 2002 & 290 & 66 & 346 & 247 & 0\\
C & 2003 & 155 & 34 & 178 & 98 & 0\\
C & 2004 & 320 & 33 & 341 & 153 & 0\\
C & 2005 & 168 & 16 & 175 & 0 & 0\\
C & 2006 & 304 & 75 & 355 & 0 & 0\\
C & 2007 & 567 & 14 & 568 & 0 & 0\\
C & 2008 & 512 & 11 & 494 & 0 & 0\\
C & 2009 & 266 & 29 & 289 & 0 & 0\\
C & 2010 & 349 & 28 & 375 & 0 & 0\\
C & 2011 & 137 & 88 & 225 & 0 & 0\\
C & 2012 & 167 & 72 & 230 & 0 & 0\\
C & 2013 & 421 & 109 & 459 & 0 & 0\\
C & 2014 & 418 & 194 & 510 & 0 & 0\\
C & 2015 & 867 & 129 & 923 & 0 & 0\\
C & 2016 & 1272 & 14 & 1281 & 0 & 0\\
C & 2017 & 949 & 47 & 996 & 0 & 0\\
C & 2018 & 730 & 70 & 783 & 0 & 0\\
C & 2019 & 760 & 17 & 774 & 0 & 113\\
C & 2020 & 445 & 7 & 452 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1992 & 1278 & 0 & 1278 & 1259 & 19\\
O & 1993 & 1358 & 51 & 1409 & 1306 & 10\\
O & 1994 & 1751 & 0 & 1751 & 494 & 2\\
O & 1995 & 506 & 0 & 506 & 330 & 75\\
O & 1996 & 339 & 0 & 339 & 306 & 0\\
O & 1997 & 722 & 52 & 774 & 0 & 744\\
O & 1998 & 501 & 15 & 516 & 0 & 443\\
O & 1999 & 679 & 4 & 683 & 0 & 663\\
O & 2000 & 540 & 2 & 542 & 118 & 265\\
O & 2001 & 757 & 0 & 757 & 389 & 28\\
O & 2002 & 712 & 9 & 721 & 335 & 125\\
O & 2003 & 607 & 1 & 608 & 341 & 1\\
O & 2004 & 900 & 14 & 914 & 328 & 24\\
O & 2005 & 488 & 0 & 488 & 260 & 5\\
O & 2006 & 674 & 9 & 683 & 343 & 15\\
O & 2007 & 1401 & 16 & 1417 & 401 & 53\\
O & 2008 & 1099 & 15 & 1114 & 309 & 76\\
O & 2009 & 1014 & 35 & 1048 & 207 & 34\\
O & 2010 & 1152 & 11 & 1163 & 179 & 37\\
O & 2011 & 1102 & 10 & 1112 & 216 & 25\\
O & 2012 & 1446 & 51 & 1494 & 208 & 7\\
O & 2013 & 1706 & 3 & 1709 & 265 & 62\\
O & 2014 & 1855 & 3 & 1858 & 282 & 42\\
O & 2015 & 2821 & 1 & 2822 & 99 & 152\\
O & 2016 & 2352 & 13 & 2364 & 54 & 116\\
O & 2017 & 3101 & 2 & 3101 & 295 & 149\\
O & 2018 & 2381 & 12 & 2393 & 370 & 106\\
O & 2019 & 2802 & 17 & 2819 & 303 & 148\\
O & 2020 & 1405 & 2 & 1407 & 0 & 299\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 2180 & 119 & 2299 & 0 & 0\\
W & 1981 & 1497 & 14 & 1477 & 0 & 0\\
W & 1982 & 1284 & 81 & 1365 & 0 & 0\\
W & 1983 & 890 & 0 & 890 & 0 & 0\\
W & 1984 & 756 & 0 & 756 & 0 & 0\\
W & 1985 & 912 & 0 & 912 & 0 & 0\\
W & 1986 & 1292 & 2 & 1294 & 0 & 0\\
W & 1987 & 1138 & 46 & 1184 & 0 & 0\\
W & 1988 & 1163 & 1 & 1163 & 0 & 0\\
W & 1989 & 1251 & 73 & 1323 & 0 & 0\\
W & 1990 & 1291 & 1 & 1292 & 0 & 0\\
W & 1991 & 1210 & 18 & 1228 & 0 & 0\\
W & 1992 & 1156 & 62 & 1217 & 0 & 0\\
W & 1993 & 1336 & 11 & 1346 & 0 & 0\\
W & 1994 & 1362 & 14 & 1374 & 12 & 0\\
W & 1995 & 1381 & 1 & 1381 & 0 & 0\\
W & 1996 & 1150 & 1 & 1150 & 0 & 0\\
W & 1997 & 945 & 65 & 1010 & 50 & 0\\
W & 1998 & 858 & 0 & 858 & 0 & 0\\
W & 1999 & 825 & 25 & 850 & 0 & 0\\
W & 2000 & 469 & 0 & 469 & 75 & 0\\
W & 2001 & 446 & 0 & 446 & 74 & 0\\
W & 2002 & 431 & 0 & 431 & 0 & 0\\
W & 2003 & 514 & 0 & 514 & 0 & 0\\
W & 2004 & 276 & 0 & 276 & 10 & 0\\
W & 2005 & 410 & 0 & 410 & 77 & 0\\
W & 2006 & 460 & 0 & 460 & 80 & 0\\
W & 2007 & 634 & 0 & 634 & 137 & 0\\
W & 2008 & 496 & 0 & 496 & 0 & 0\\
W & 2009 & 386 & 0 & 386 & 0 & 0\\
W & 2010 & 199 & 2 & 201 & 0 & 0\\
W & 2011 & 462 & 3 & 465 & 0 & 0\\
W & 2012 & 479 & 0 & 479 & 0 & 0\\
W & 2013 & 726 & 105 & 831 & 0 & 0\\
W & 2014 & 276 & 6 & 282 & 0 & 0\\
W & 2015 & 269 & 13 & 282 & 0 & 0\\
W & 2016 & 412 & 16 & 427 & 0 & 0\\
W & 2017 & 1004 & 20 & 1024 & 0 & 0\\
W & 2018 & 777 & 27 & 804 & 0 & 0\\
W & 2019 & 519 & 10 & 529 & 0 & 0\\
W & 2020 & 187 & 32 & 219 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 421 & 421 & 0 & 0\\
C & 2004 & 51 & 1375 & 1426 & 0 & 0\\
C & 2005 & 223 & 4432 & 4655 & 0 & 0\\
C & 2006 & 508 & 3975 & 4483 & 0 & 0\\
C & 2007 & 652 & 2697 & 3349 & 0 & 0\\
C & 2008 & 850 & 1848 & 2698 & 0 & 0\\
C & 2009 & 990 & 1769 & 2759 & 0 & 0\\
C & 2010 & 864 & 1045 & 1909 & 0 & 0\\
C & 2011 & 1579 & 3016 & 4595 & 0 & 0\\
C & 2012 & 1680 & 4090 & 5770 & 0 & 0\\
C & 2013 & 4907 & 3037 & 7943 & 0 & 0\\
C & 2014 & 8088 & 940 & 9027 & 0 & 0\\
C & 2015 & 12155 & 679 & 12834 & 0 & 0\\
C & 2016 & 9887 & 450 & 10337 & 0 & 0\\
C & 2017 & 8281 & 286 & 8564 & 0 & 51\\
C & 2018 & 6175 & 221 & 6392 & 0 & 225\\
C & 2019 & 4170 & 233 & 4402 & 0 & 52\\
C & 2020 & 8 & 4 & 12 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1999 & 1649 & 73 & 1722 & 0 & 0\\
O & 2000 & 2246 & 15 & 2261 & 0 & 0\\
O & 2001 & 1948 & 1465 & 3413 & 791 & 1163\\
O & 2002 & 860 & 2933 & 3793 & 858 & 3\\
O & 2003 & 891 & 3886 & 4777 & 807 & 12\\
O & 2004 & 942 & 2238 & 3180 & 653 & 45\\
O & 2005 & 775 & 3285 & 4060 & 541 & 22\\
O & 2006 & 1392 & 4812 & 6204 & 799 & 371\\
O & 2007 & 1149 & 5477 & 6626 & 788 & 250\\
O & 2008 & 1180 & 5641 & 6821 & 740 & 294\\
O & 2009 & 1138 & 4970 & 6108 & 258 & 771\\
O & 2010 & 1153 & 5879 & 7032 & 260 & 759\\
O & 2011 & 1217 & 6641 & 7858 & 258 & 799\\
O & 2012 & 1216 & 8139 & 9355 & 260 & 786\\
O & 2013 & 1157 & 8136 & 9293 & 258 & 774\\
O & 2014 & 1090 & 6456 & 7546 & 259 & 754\\
O & 2015 & 1129 & 7064 & 8193 & 259 & 769\\
O & 2016 & 1087 & 5430 & 6517 & 260 & 730\\
O & 2017 & 1010 & 5632 & 6642 & 260 & 691\\
O & 2018 & 1162 & 7530 & 8692 & 258 & 814\\
O & 2019 & 1081 & 6645 & 7726 & 258 & 737\\
O & 2020 & 858 & 726 & 1584 & 0 & 856\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 1224 & 164 & 1388 & 922 & 2\\
W & 2003 & 1641 & 397 & 2038 & 1167 & 10\\
W & 2004 & 1369 & 215 & 1584 & 849 & 0\\
W & 2005 & 1701 & 332 & 2033 & 911 & 70\\
W & 2006 & 1215 & 254 & 1469 & 701 & 8\\
W & 2007 & 1290 & 144 & 1434 & 811 & 25\\
W & 2008 & 841 & 127 & 968 & 812 & 10\\
W & 2009 & 1070 & 184 & 1254 & 698 & 59\\
W & 2010 & 535 & 287 & 822 & 401 & 12\\
W & 2011 & 418 & 652 & 1070 & 302 & 8\\
W & 2012 & 431 & 621 & 1052 & 269 & 3\\
W & 2013 & 362 & 291 & 653 & 352 & 0\\
W & 2014 & 711 & 332 & 1043 & 701 & 10\\
W & 2015 & 515 & 247 & 762 & 501 & 15\\
W & 2016 & 870 & 214 & 1084 & 805 & 65\\
W & 2017 & 1568 & 770 & 2338 & 1627 & 76\\
W & 2018 & 919 & 802 & 1721 & 1002 & 47\\
W & 2019 & 1702 & 1200 & 2902 & 1623 & 175\\
W & 2020 & 1340 & 257 & 1597 & 1261 & 214\\
W & 2021 & 1322 & 477 & 1799 & 0 & 1613\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 185 & 1244 & 86 & 1330 & 875 & 17\\
2004 & 170 & 1343 & 24 & 1367 & 827 & 25\\
2005 & 196 & 1067 & 30 & 1097 & 840 & 24\\
2006 & 171 & 1011 & 1 & 1012 & 697 & 14\\
2007 & 169 & 631 & 14 & 645 & 483 & 69\\
2008 & 190 & 883 & 277 & 1160 & 838 & 19\\
2009 & 221 & 933 & 174 & 1107 & 484 & 483\\
2010 & 235 & 1623 & 307 & 1930 & 518 & 516\\
2011 & 266 & 1768 & 85 & 1853 & 519 & 526\\
2012 & 250 & 1940 & 247 & 2187 & 410 & 418\\
2013 & 189 & 1295 & 48 & 1343 & 365 & 262\\
2014 & 263 & 2914 & 69 & 2924 & 478 & 438\\
2015 & 245 & 1797 & 51 & 1838 & 437 & 323\\
2016 & 230 & 1716 & 37 & 1753 & 366 & 359\\
2017 & 250 & 1565 & 60 & 1625 & 286 & 478\\
2018 & 224 & 1537 & 17 & 1554 & 399 & 319\\
2019 & 124 & 551 & 1 & 552 & 234 & 209\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 19 & 30 & 4 & 32 & 0 & 0\\
2005 & 19 & 27 & 14 & 39 & 0 & 0\\
2006 & 13 & 13 & 5 & 14 & 0 & 0\\
2007 & 19 & 24 & 6 & 26 & 0 & 0\\
2008 & 9 & 12 & 0 & 12 & 0 & 0\\
2009 & 11 & 19 & 1 & 19 & 0 & 0\\
2010 & 5 & 14 & 1 & 15 & 0 & 0\\
2011 & 27 & 28 & 7 & 31 & 0 & 0\\
2012 & 36 & 27 & 43 & 66 & 0 & 0\\
2013 & 36 & 71 & 25 & 94 & 0 & 2\\
2014 & 40 & 50 & 41 & 88 & 0 & 0\\
2015 & 52 & 51 & 42 & 83 & 0 & 0\\
2016 & 48 & 103 & 12 & 105 & 0 & 15\\
2017 & 47 & 78 & 4 & 78 & 0 & 31\\
2018 & 42 & 79 & 6 & 79 & 0 & 0\\
2019 & 30 & 44 & 0 & 44 & 0 & 2\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Longnose skate 
 The most recent assessment of Longnose skate was a full assessment conducted in 2019. Across available data, Longnose skate have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 33,762 length observations, 0 age readings, and 5,149 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 32,618 length observations, 0 age readings, and 5,149 otoliths that are available to be aged. In California, since 2000, a total of 9,598 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 15,826 length observations, 0 age readings, and 5,149 otoliths have been collected In Washington, since 2000, a total of 7,194 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 49,205 length observations, 649 age readings, and 1,647 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2009 & 727 & 2 & 727 & 0 & 0\\
C & 2010 & 638 & 0 & 638 & 0 & 0\\
C & 2011 & 1272 & 0 & 1272 & 0 & 0\\
C & 2012 & 1158 & 39 & 1196 & 0 & 0\\
C & 2013 & 948 & 0 & 948 & 0 & 0\\
C & 2014 & 606 & 56 & 662 & 0 & 0\\
C & 2015 & 806 & 47 & 831 & 0 & 0\\
C & 2016 & 952 & 58 & 969 & 0 & 0\\
C & 2017 & 1037 & 2 & 1039 & 0 & 0\\
C & 2018 & 546 & 8 & 554 & 0 & 0\\
C & 2019 & 496 & 16 & 512 & 0 & 0\\
C & 2020 & 250 & 0 & 250 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1995 & 174 & 0 & 174 & 0 & 0\\
O & 1996 & 99 & 0 & 99 & 0 & 0\\
O & 1997 & 492 & 0 & 492 & 0 & 0\\
O & 1998 & 84 & 0 & 84 & 0 & 0\\
O & 1999 & 295 & 0 & 295 & 0 & 0\\
O & 2000 & 356 & 0 & 356 & 0 & 0\\
O & 2001 & 332 & 0 & 332 & 0 & 0\\
O & 2002 & 235 & 0 & 235 & 0 & 0\\
O & 2003 & 521 & 0 & 521 & 0 & 0\\
O & 2004 & 92 & 0 & 92 & 0 & 0\\
O & 2005 & 233 & 0 & 233 & 0 & 0\\
O & 2006 & 870 & 0 & 870 & 0 & 254\\
O & 2007 & 1079 & 0 & 1079 & 0 & 702\\
O & 2008 & 693 & 0 & 693 & 0 & 573\\
O & 2009 & 685 & 0 & 685 & 0 & 477\\
O & 2010 & 1110 & 0 & 1110 & 0 & 539\\
O & 2011 & 889 & 0 & 889 & 0 & 527\\
O & 2012 & 1118 & 0 & 1118 & 0 & 623\\
O & 2013 & 942 & 1 & 943 & 0 & 30\\
O & 2014 & 993 & 0 & 991 & 0 & 0\\
O & 2015 & 918 & 0 & 917 & 0 & 0\\
O & 2016 & 892 & 0 & 892 & 0 & 0\\
O & 2017 & 1240 & 0 & 1240 & 0 & 80\\
O & 2018 & 865 & 0 & 865 & 0 & 504\\
O & 2019 & 1285 & 0 & 1285 & 0 & 611\\
O & 2020 & 480 & 0 & 480 & 0 & 229\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2004 & 49 & 0 & 49 & 0 & 0\\
W & 2005 & 15 & 1 & 15 & 0 & 0\\
W & 2006 & 255 & 0 & 255 & 0 & 0\\
W & 2007 & 381 & 0 & 381 & 0 & 0\\
W & 2008 & 972 & 0 & 972 & 0 & 0\\
W & 2009 & 456 & 0 & 456 & 0 & 0\\
W & 2010 & 147 & 3 & 150 & 0 & 0\\
W & 2011 & 735 & 0 & 735 & 0 & 0\\
W & 2012 & 549 & 51 & 600 & 0 & 0\\
W & 2013 & 1013 & 0 & 1012 & 0 & 0\\
W & 2014 & 401 & 0 & 401 & 0 & 0\\
W & 2015 & 448 & 0 & 448 & 0 & 0\\
W & 2016 & 722 & 24 & 746 & 0 & 0\\
W & 2017 & 543 & 0 & 543 & 0 & 0\\
W & 2018 & 260 & 0 & 260 & 0 & 0\\
W & 2019 & 147 & 1 & 148 & 0 & 0\\
W & 2020 & 23 & 0 & 23 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 289 & 2657 & 2 & 2655 & 0 & 0\\
2004 & 273 & 2598 & 1 & 2599 & 0 & 0\\
2005 & 382 & 3259 & 2 & 3259 & 0 & 0\\
2006 & 385 & 3304 & 3 & 0 & 0 & 0\\
2007 & 413 & 3834 & 9 & 861 & 0 & 0\\
2008 & 395 & 3383 & 0 & 3383 & 0 & 0\\
2009 & 364 & 3113 & 3 & 3116 & 0 & 0\\
2010 & 408 & 3462 & 0 & 3462 & 0 & 0\\
2011 & 423 & 2987 & 4 & 2991 & 321 & 104\\
2012 & 427 & 3647 & 3 & 3650 & 328 & 40\\
2013 & 297 & 2491 & 1 & 2492 & 0 & 0\\
2014 & 421 & 3722 & 0 & 3722 & 0 & 0\\
2015 & 429 & 4053 & 14 & 4067 & 0 & 0\\
2016 & 428 & 4003 & 1 & 4004 & 0 & 420\\
2017 & 437 & 3679 & 0 & 3679 & 0 & 442\\
2018 & 426 & 3610 & 0 & 3610 & 0 & 421\\
2019 & 223 & 1669 & 0 & 1655 & 0 & 220\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Longspine thornyhead 
 The most recent assessment of Longspine thornyhead was a full assessment conducted in 2013. Across available data, Longspine thornyhead have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 180,299 length observations, 30 age readings, and 37,194 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 110,054 length observations, 30 age readings, and 21,583 otoliths that are available to be aged. In California, since 2000, a total of 76,415 length observations, 0 age readings, and 54 otoliths have been collected In Oregon, since 2000, a total of 27,824 length observations, 30 age readings, and 21,529 otoliths have been collected In Washington, since 2000, a total of 5,815 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 104,769 length observations, 0 age readings, and 13,832 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 716 & 13 & 729 & 0 & 101\\
C & 1981 & 587 & 1 & 588 & 0 & 20\\
C & 1982 & 341 & 1 & 341 & 0 & 284\\
C & 1983 & 300 & 0 & 300 & 0 & 294\\
C & 1984 & 806 & 4 & 810 & 0 & 795\\
C & 1985 & 1442 & 2 & 1443 & 0 & 1430\\
C & 1986 & 599 & 14 & 613 & 0 & 122\\
C & 1987 & 591 & 1 & 592 & 0 & 0\\
C & 1988 & 56 & 0 & 56 & 0 & 0\\
C & 1989 & 1159 & 75 & 1234 & 0 & 0\\
C & 1990 & 1512 & 66 & 1578 & 0 & 67\\
C & 1991 & 2350 & 135 & 2469 & 0 & 0\\
C & 1992 & 2539 & 153 & 2692 & 0 & 0\\
C & 1993 & 1038 & 1753 & 2791 & 0 & 0\\
C & 1994 & 1171 & 2704 & 3875 & 0 & 0\\
C & 1995 & 3636 & 4296 & 7931 & 0 & 0\\
C & 1996 & 2766 & 4032 & 6798 & 0 & 0\\
C & 1997 & 1429 & 4549 & 5978 & 0 & 0\\
C & 1998 & 1591 & 2662 & 4252 & 0 & 0\\
C & 1999 & 1156 & 2261 & 3417 & 0 & 0\\
C & 2000 & 1602 & 1736 & 3338 & 0 & 0\\
C & 2001 & 1306 & 2028 & 3245 & 0 & 0\\
C & 2002 & 2870 & 3119 & 5440 & 0 & 0\\
C & 2003 & 1529 & 2512 & 3381 & 0 & 0\\
C & 2004 & 528 & 1902 & 2423 & 0 & 0\\
C & 2005 & 1247 & 1571 & 2498 & 0 & 0\\
C & 2006 & 2392 & 2245 & 4596 & 0 & 0\\
C & 2007 & 1370 & 1773 & 2953 & 0 & 0\\
C & 2008 & 2035 & 2741 & 4774 & 0 & 0\\
C & 2009 & 2903 & 1368 & 4269 & 0 & 11\\
C & 2010 & 2269 & 2236 & 4502 & 0 & 0\\
C & 2011 & 2552 & 2305 & 4857 & 0 & 3\\
C & 2012 & 2157 & 2720 & 4876 & 0 & 40\\
C & 2013 & 1794 & 2825 & 4617 & 0 & 0\\
C & 2014 & 607 & 4250 & 4857 & 0 & 0\\
C & 2015 & 0 & 4159 & 4156 & 0 & 0\\
C & 2016 & 0 & 4653 & 4646 & 0 & 0\\
C & 2017 & 0 & 3172 & 3172 & 0 & 0\\
C & 2018 & 266 & 1215 & 1481 & 0 & 0\\
C & 2019 & 1023 & 595 & 1618 & 0 & 0\\
C & 2020 & 414 & 302 & 716 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1990 & 3812 & 0 & 3812 & 0 & 3812\\
O & 1991 & 2162 & 1 & 2163 & 0 & 2149\\
O & 1992 & 4158 & 1 & 4159 & 0 & 3427\\
O & 1993 & 1272 & 0 & 1272 & 0 & 1099\\
O & 1994 & 150 & 0 & 150 & 0 & 0\\
O & 1996 & 1925 & 47 & 1972 & 0 & 0\\
O & 1997 & 0 & 6189 & 6189 & 0 & 0\\
O & 1998 & 879 & 21 & 900 & 0 & 900\\
O & 1999 & 1134 & 7 & 1141 & 0 & 1111\\
O & 2000 & 805 & 5 & 810 & 30 & 630\\
O & 2001 & 1193 & 6 & 1199 & 0 & 870\\
O & 2002 & 1256 & 5 & 1261 & 0 & 961\\
O & 2003 & 1341 & 9 & 1350 & 0 & 990\\
O & 2004 & 1174 & 25 & 1199 & 0 & 899\\
O & 2005 & 868 & 42 & 910 & 0 & 820\\
O & 2006 & 1194 & 36 & 1230 & 0 & 990\\
O & 2007 & 1671 & 190 & 1861 & 0 & 1501\\
O & 2008 & 1789 & 251 & 2040 & 0 & 1650\\
O & 2009 & 1700 & 160 & 1860 & 0 & 1590\\
O & 2010 & 2208 & 102 & 2309 & 0 & 1620\\
O & 2011 & 1512 & 78 & 1590 & 0 & 1380\\
O & 2012 & 1683 & 57 & 1740 & 0 & 1500\\
O & 2013 & 1828 & 134 & 1962 & 0 & 1452\\
O & 2014 & 1232 & 67 & 1299 & 0 & 879\\
O & 2015 & 914 & 78 & 992 & 0 & 752\\
O & 2016 & 978 & 53 & 1030 & 0 & 791\\
O & 2017 & 1055 & 67 & 1122 & 0 & 851\\
O & 2018 & 853 & 35 & 888 & 0 & 620\\
O & 2019 & 673 & 46 & 719 & 0 & 510\\
O & 2020 & 409 & 44 & 453 & 0 & 273\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2001 & 132 & 255 & 387 & 0 & 0\\
W & 2002 & 78 & 55 & 133 & 0 & 0\\
W & 2003 & 390 & 147 & 537 & 0 & 0\\
W & 2004 & 84 & 59 & 143 & 0 & 0\\
W & 2005 & 54 & 15 & 69 & 0 & 0\\
W & 2006 & 50 & 3 & 53 & 0 & 0\\
W & 2007 & 211 & 105 & 316 & 0 & 0\\
W & 2008 & 266 & 104 & 370 & 0 & 0\\
W & 2009 & 344 & 110 & 454 & 0 & 0\\
W & 2010 & 506 & 110 & 616 & 0 & 0\\
W & 2011 & 448 & 348 & 796 & 0 & 0\\
W & 2012 & 285 & 67 & 352 & 0 & 0\\
W & 2013 & 509 & 96 & 605 & 0 & 0\\
W & 2014 & 315 & 36 & 351 & 0 & 0\\
W & 2015 & 47 & 3 & 50 & 0 & 0\\
W & 2016 & 0 & 17 & 17 & 0 & 0\\
W & 2017 & 93 & 51 & 144 & 0 & 0\\
W & 2018 & 167 & 44 & 211 & 0 & 0\\
W & 2019 & 187 & 24 & 211 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 195 & 0 & 15475 & 15475 & 0 & 955\\
2004 & 151 & 2790 & 8383 & 11173 & 0 & 742\\
2005 & 228 & 9044 & 4486 & 13530 & 0 & 1103\\
2006 & 237 & 6407 & 2674 & 9081 & 0 & 930\\
2007 & 250 & 4567 & 1631 & 6198 & 0 & 958\\
2008 & 261 & 2775 & 860 & 3635 & 0 & 989\\
2009 & 242 & 2333 & 777 & 3110 & 0 & 956\\
2010 & 264 & 2297 & 785 & 3082 & 0 & 1032\\
2011 & 254 & 4074 & 1114 & 5188 & 0 & 1002\\
2012 & 248 & 3572 & 1388 & 4960 & 0 & 980\\
2013 & 162 & 2643 & 642 & 3285 & 0 & 621\\
2014 & 240 & 3938 & 947 & 4885 & 0 & 922\\
2015 & 238 & 4051 & 771 & 4822 & 0 & 919\\
2016 & 245 & 3837 & 1026 & 4863 & 0 & 529\\
2017 & 236 & 3912 & 728 & 4640 & 0 & 478\\
2018 & 248 & 3841 & 1039 & 4880 & 0 & 495\\
2019 & 112 & 1664 & 298 & 1962 & 0 & 221\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Olive rockfish 
 The most recent assessment of Olive rockfish was a data-limited assessment conducted in 2010. Across available data, Olive rockfish have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC HKL survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 1,976 length observations, 0 age readings, and 138 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 338 length observations, 0 age readings, and 65 otoliths that are available to be aged. In California, since 2000, a total of 332 length observations, 0 age readings, and 61 otoliths have been collected In Oregon, since 2000, a total of 6 length observations, 0 age readings, and 4 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 37,205 length observations, 0 age readings, and 137 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 37,205 length observations, 0 age readings, and 137 otoliths that are available to be aged. In California, since 2003, a total of 37,168 length observations, 0 age readings, and 137 otoliths have been collected In Oregon, since 2003, a total of 37 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC HKL survey has collected a total of 537 length observations, 0 age readings, and 508 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 1 & 1 & 2 & 0 & 0\\
C & 1981 & 0 & 1 & 1 & 0 & 0\\
C & 1982 & 2 & 26 & 28 & 0 & 2\\
C & 1983 & 6 & 1 & 7 & 0 & 4\\
C & 1984 & 0 & 5 & 5 & 0 & 6\\
C & 1985 & 18 & 30 & 48 & 0 & 35\\
C & 1986 & 68 & 44 & 112 & 0 & 23\\
C & 1987 & 25 & 1 & 26 & 0 & 3\\
C & 1988 & 9 & 0 & 9 & 0 & 0\\
C & 1989 & 0 & 33 & 33 & 0 & 0\\
C & 1990 & 0 & 1 & 1 & 0 & 0\\
C & 1991 & 12 & 2 & 14 & 0 & 0\\
C & 1992 & 23 & 128 & 151 & 0 & 0\\
C & 1993 & 15 & 385 & 400 & 0 & 0\\
C & 1994 & 0 & 270 & 270 & 0 & 0\\
C & 1995 & 4 & 178 & 182 & 0 & 0\\
C & 1996 & 1 & 172 & 173 & 0 & 0\\
C & 1997 & 3 & 65 & 68 & 0 & 0\\
C & 1998 & 9 & 19 & 28 & 0 & 0\\
C & 1999 & 3 & 77 & 80 & 0 & 0\\
C & 2000 & 0 & 26 & 26 & 0 & 0\\
C & 2001 & 0 & 18 & 3 & 0 & 0\\
C & 2002 & 1 & 10 & 9 & 0 & 0\\
C & 2004 & 0 & 2 & 1 & 0 & 0\\
C & 2005 & 26 & 10 & 36 & 0 & 26\\
C & 2006 & 14 & 8 & 21 & 0 & 13\\
C & 2007 & 0 & 69 & 67 & 0 & 16\\
C & 2009 & 0 & 25 & 24 & 0 & 0\\
C & 2010 & 0 & 9 & 9 & 0 & 0\\
C & 2011 & 0 & 9 & 9 & 0 & 0\\
C & 2012 & 6 & 27 & 31 & 0 & 6\\
C & 2013 & 0 & 24 & 24 & 0 & 0\\
C & 2014 & 0 & 5 & 5 & 0 & 0\\
C & 2015 & 0 & 3 & 3 & 0 & 0\\
C & 2016 & 0 & 12 & 12 & 0 & 0\\
C & 2017 & 0 & 11 & 11 & 0 & 0\\
C & 2018 & 0 & 23 & 23 & 0 & 0\\
C & 2019 & 10 & 4 & 14 & 0 & 0\\
C & 2020 & 3 & 1 & 4 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2010 & 1 & 0 & 1 & 0 & 0\\
O & 2011 & 1 & 0 & 1 & 0 & 0\\
O & 2014 & 1 & 0 & 1 & 0 & 1\\
O & 2017 & 3 & 0 & 3 & 0 & 3\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 51 & 51 & 0 & 0\\
C & 2004 & 0 & 2786 & 2786 & 0 & 0\\
C & 2005 & 0 & 3451 & 3451 & 0 & 0\\
C & 2006 & 0 & 3331 & 3331 & 0 & 0\\
C & 2007 & 0 & 2953 & 2953 & 0 & 0\\
C & 2008 & 0 & 2644 & 2644 & 0 & 0\\
C & 2009 & 0 & 1401 & 1401 & 0 & 0\\
C & 2010 & 0 & 737 & 737 & 0 & 0\\
C & 2011 & 0 & 1321 & 1321 & 0 & 0\\
C & 2012 & 0 & 2147 & 2147 & 0 & 0\\
C & 2013 & 0 & 1517 & 1517 & 0 & 0\\
C & 2014 & 0 & 2101 & 2101 & 0 & 0\\
C & 2015 & 0 & 3013 & 3013 & 0 & 0\\
C & 2016 & 0 & 2675 & 2675 & 0 & 0\\
C & 2017 & 0 & 1935 & 1935 & 0 & 0\\
C & 2018 & 0 & 2281 & 2280 & 0 & 100\\
C & 2019 & 1 & 2791 & 2791 & 0 & 37\\
C & 2020 & 0 & 34 & 34 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2008 & 0 & 1 & 1 & 0 & 0\\
O & 2009 & 0 & 2 & 2 & 0 & 0\\
O & 2010 & 0 & 3 & 3 & 0 & 0\\
O & 2011 & 0 & 2 & 2 & 0 & 0\\
O & 2012 & 0 & 2 & 2 & 0 & 0\\
O & 2013 & 0 & 9 & 9 & 0 & 0\\
O & 2014 & 0 & 2 & 2 & 0 & 0\\
O & 2015 & 0 & 6 & 6 & 0 & 0\\
O & 2016 & 0 & 9 & 9 & 0 & 0\\
O & 2017 & 0 & 1 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 8 & 15 & 0 & 15 & 0 & 15\\
2005 & 2 & 3 & 0 & 3 & 0 & 3\\
2006 & 7 & 21 & 0 & 21 & 0 & 21\\
2007 & 7 & 20 & 0 & 20 & 0 & 19\\
2008 & 12 & 52 & 0 & 52 & 0 & 52\\
2009 & 4 & 8 & 0 & 8 & 0 & 8\\
2010 & 3 & 5 & 0 & 5 & 0 & 5\\
2011 & 5 & 7 & 0 & 7 & 0 & 6\\
2012 & 9 & 13 & 1 & 13 & 0 & 13\\
2013 & 7 & 12 & 0 & 12 & 0 & 9\\
2014 & 14 & 60 & 0 & 60 & 0 & 60\\
2015 & 20 & 64 & 0 & 64 & 0 & 61\\
2016 & 20 & 46 & 0 & 46 & 0 & 44\\
2017 & 28 & 80 & 0 & 80 & 0 & 78\\
2018 & 18 & 59 & 1 & 59 & 0 & 57\\
2019 & 22 & 72 & 0 & 72 & 0 & 57\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Pacific cod 
 To date, no assessment or analysis has been conducted on Pacific cod. Across available data, Pacific cod have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 65,416 length observations, 943 age readings, and 3,642 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 20,227 length observations, 943 age readings, and 3,642 otoliths that are available to be aged. In California, since 2000, a total of 28 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 3,850 length observations, 0 age readings, and 3,642 otoliths have been collected In Washington, since 2000, a total of 16,349 length observations, 943 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 386 length observations, 0 age readings, and 82 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 376 length observations, 0 age readings, and 82 otoliths that are available to be aged. In Oregon, since 2003, a total of 49 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 327 length observations, 0 age readings, and 82 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 3,850 length observations, 0 age readings, and 1,266 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2008 & 9 & 0 & 9 & 0 & 0\\
C & 2014 & 1 & 0 & 1 & 0 & 0\\
C & 2015 & 17 & 1 & 18 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2003 & 26 & 0 & 26 & 0 & 26\\
O & 2008 & 17 & 0 & 16 & 0 & 16\\
O & 2009 & 36 & 0 & 36 & 0 & 34\\
O & 2010 & 370 & 4 & 374 & 0 & 342\\
O & 2011 & 349 & 0 & 349 & 0 & 349\\
O & 2012 & 420 & 0 & 420 & 0 & 420\\
O & 2013 & 493 & 0 & 493 & 0 & 433\\
O & 2014 & 315 & 0 & 315 & 0 & 315\\
O & 2015 & 407 & 0 & 407 & 0 & 375\\
O & 2016 & 432 & 0 & 432 & 0 & 372\\
O & 2017 & 410 & 0 & 410 & 0 & 394\\
O & 2018 & 222 & 0 & 222 & 0 & 216\\
O & 2019 & 212 & 1 & 213 & 0 & 213\\
O & 2020 & 137 & 0 & 137 & 0 & 137\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 4983 & 41 & 5024 & 0 & 0\\
W & 1981 & 3979 & 343 & 4322 & 0 & 0\\
W & 1982 & 1599 & 1362 & 2961 & 0 & 0\\
W & 1983 & 1203 & 1102 & 2305 & 0 & 0\\
W & 1984 & 1200 & 600 & 1800 & 0 & 0\\
W & 1985 & 1757 & 0 & 1757 & 0 & 0\\
W & 1986 & 2300 & 0 & 2300 & 0 & 0\\
W & 1987 & 4100 & 0 & 4100 & 0 & 0\\
W & 1988 & 3963 & 0 & 3963 & 0 & 0\\
W & 1989 & 2499 & 127 & 2626 & 0 & 0\\
W & 1990 & 1251 & 294 & 1545 & 0 & 0\\
W & 1991 & 2214 & 50 & 2264 & 0 & 0\\
W & 1992 & 2025 & 5 & 2030 & 0 & 0\\
W & 1993 & 1470 & 180 & 1650 & 0 & 0\\
W & 1994 & 735 & 65 & 800 & 0 & 0\\
W & 1995 & 1097 & 301 & 1398 & 0 & 0\\
W & 1996 & 790 & 0 & 790 & 0 & 0\\
W & 1997 & 1097 & 3 & 1100 & 0 & 0\\
W & 1998 & 1400 & 0 & 1400 & 0 & 0\\
W & 1999 & 1054 & 0 & 1054 & 0 & 0\\
W & 2000 & 900 & 0 & 900 & 0 & 0\\
W & 2001 & 1097 & 0 & 1097 & 0 & 0\\
W & 2002 & 1079 & 0 & 1079 & 100 & 0\\
W & 2003 & 1497 & 2 & 1499 & 200 & 0\\
W & 2004 & 1020 & 1 & 1021 & 123 & 0\\
W & 2005 & 1181 & 0 & 1181 & 50 & 0\\
W & 2006 & 920 & 0 & 920 & 0 & 0\\
W & 2007 & 699 & 1 & 700 & 0 & 0\\
W & 2008 & 300 & 0 & 300 & 0 & 0\\
W & 2009 & 898 & 2 & 900 & 0 & 0\\
W & 2010 & 1300 & 0 & 1300 & 0 & 0\\
W & 2011 & 800 & 0 & 800 & 0 & 0\\
W & 2012 & 478 & 0 & 478 & 0 & 0\\
W & 2013 & 400 & 0 & 400 & 0 & 0\\
W & 2014 & 453 & 4 & 457 & 0 & 0\\
W & 2015 & 227 & 9 & 236 & 0 & 0\\
W & 2016 & 143 & 168 & 311 & 0 & 0\\
W & 2017 & 986 & 105 & 1091 & 470 & 0\\
W & 2018 & 818 & 3 & 821 & 0 & 0\\
W & 2019 & 704 & 69 & 773 & 0 & 0\\
W & 2020 & 70 & 15 & 85 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2002 & 0 & 7 & 7 & 0 & 0\\
O & 2003 & 0 & 20 & 20 & 0 & 0\\
O & 2004 & 0 & 1 & 1 & 0 & 0\\
O & 2009 & 0 & 2 & 2 & 0 & 0\\
O & 2011 & 0 & 1 & 1 & 0 & 0\\
O & 2012 & 0 & 20 & 20 & 0 & 0\\
O & 2017 & 0 & 3 & 3 & 0 & 0\\
O & 2019 & 0 & 2 & 2 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 0 & 3 & 3 & 0 & 0\\
W & 2003 & 1 & 18 & 19 & 0 & 0\\
W & 2004 & 159 & 5 & 164 & 0 & 0\\
W & 2005 & 17 & 14 & 31 & 0 & 0\\
W & 2006 & 11 & 7 & 18 & 0 & 0\\
W & 2011 & 0 & 2 & 2 & 0 & 0\\
W & 2013 & 0 & 1 & 1 & 0 & 0\\
W & 2018 & 33 & 8 & 41 & 0 & 32\\
W & 2019 & 6 & 1 & 7 & 0 & 6\\
W & 2020 & 10 & 0 & 10 & 0 & 10\\
W & 2021 & 34 & 0 & 34 & 0 & 34\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 68 & 783 & 0 & 783 & 0 & 0\\
2004 & 46 & 290 & 0 & 290 & 0 & 0\\
2005 & 27 & 199 & 0 & 199 & 0 & 0\\
2006 & 15 & 166 & 0 & 166 & 0 & 0\\
2007 & 24 & 177 & 0 & 177 & 0 & 0\\
2008 & 19 & 72 & 0 & 72 & 0 & 0\\
2009 & 17 & 123 & 0 & 123 & 0 & 0\\
2010 & 48 & 404 & 0 & 404 & 0 & 221\\
2011 & 34 & 294 & 7 & 301 & 0 & 150\\
2012 & 21 & 154 & 0 & 154 & 0 & 89\\
2013 & 27 & 173 & 2 & 175 & 0 & 33\\
2014 & 39 & 240 & 0 & 240 & 0 & 185\\
2015 & 62 & 328 & 0 & 328 & 0 & 270\\
2016 & 31 & 231 & 0 & 231 & 0 & 148\\
2017 & 25 & 93 & 0 & 93 & 0 & 92\\
2018 & 10 & 45 & 0 & 45 & 0 & 45\\
2019 & 10 & 69 & 0 & 69 & 0 & 33\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Pacific ocean perch 
 The most recent assessment of Pacific ocean perch was a full assessment conducted in 2017. Across available data, Pacific ocean perch have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 93,923 length observations, 35,040 age readings, and 31,641 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 45,756 length observations, 18,977 age readings, and 17,267 otoliths that are available to be aged. In California, since 2000, a total of 789 length observations, 0 age readings, and 435 otoliths have been collected In Oregon, since 2000, a total of 32,414 length observations, 11,949 age readings, and 16,640 otoliths have been collected In Washington, since 2000, a total of 12,553 length observations, 7,028 age readings, and 192 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 13,824 length observations, 5,881 age readings, and 2,739 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1981 & 2 & 0 & 2 & 0 & 2\\
C & 1982 & 1 & 0 & 1 & 0 & 133\\
C & 1985 & 1 & 0 & 1 & 0 & 277\\
C & 2007 & 36 & 0 & 22 & 0 & 14\\
C & 2008 & 53 & 4 & 57 & 0 & 35\\
C & 2009 & 62 & 2 & 64 & 0 & 60\\
C & 2010 & 45 & 2 & 47 & 0 & 26\\
C & 2011 & 63 & 18 & 81 & 0 & 60\\
C & 2012 & 33 & 24 & 56 & 0 & 26\\
C & 2013 & 36 & 4 & 37 & 0 & 32\\
C & 2014 & 34 & 16 & 46 & 0 & 20\\
C & 2015 & 52 & 6 & 55 & 0 & 48\\
C & 2016 & 85 & 15 & 96 & 0 & 84\\
C & 2017 & 85 & 11 & 94 & 0 & 30\\
C & 2018 & 18 & 1 & 19 & 0 & 0\\
C & 2019 & 47 & 0 & 47 & 0 & 0\\
C & 2020 & 61 & 7 & 68 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1981 & 1306 & 0 & 1306 & 149 & 1157\\
O & 1982 & 2219 & 0 & 2219 & 1343 & 876\\
O & 1983 & 1637 & 0 & 1637 & 1430 & 207\\
O & 1984 & 401 & 0 & 401 & 289 & 10\\
O & 1985 & 1710 & 0 & 1710 & 0 & 1710\\
O & 1986 & 1703 & 0 & 1703 & 0 & 1703\\
O & 1987 & 1855 & 0 & 1855 & 0 & 1855\\
O & 1988 & 402 & 0 & 402 & 0 & 402\\
O & 1989 & 798 & 0 & 798 & 0 & 798\\
O & 1990 & 599 & 0 & 599 & 0 & 599\\
O & 1991 & 216 & 0 & 216 & 0 & 216\\
O & 1994 & 898 & 0 & 898 & 238 & 0\\
O & 1995 & 856 & 0 & 856 & 0 & 0\\
O & 1996 & 1193 & 0 & 1193 & 0 & 23\\
O & 1997 & 1543 & 24 & 1567 & 0 & 0\\
O & 1998 & 1069 & 0 & 1069 & 0 & 0\\
O & 1999 & 1496 & 0 & 1496 & 39 & 0\\
O & 2000 & 1301 & 0 & 1301 & 414 & 4\\
O & 2001 & 1052 & 0 & 1052 & 724 & 114\\
O & 2002 & 811 & 0 & 811 & 683 & 2\\
O & 2003 & 839 & 0 & 839 & 534 & 0\\
O & 2004 & 864 & 0 & 864 & 554 & 0\\
O & 2005 & 957 & 0 & 957 & 720 & 1\\
O & 2006 & 1092 & 0 & 1092 & 1041 & 0\\
O & 2007 & 1339 & 21 & 1339 & 1221 & 37\\
O & 2008 & 2186 & 0 & 2186 & 667 & 1511\\
O & 2009 & 2127 & 0 & 2127 & 963 & 989\\
O & 2010 & 2442 & 0 & 2442 & 1068 & 903\\
O & 2011 & 1483 & 2 & 1484 & 929 & 547\\
O & 2012 & 1300 & 0 & 1300 & 1008 & 247\\
O & 2013 & 1645 & 1 & 1645 & 589 & 1026\\
O & 2014 & 2234 & 0 & 2234 & 295 & 1837\\
O & 2015 & 2069 & 0 & 2068 & 309 & 1610\\
O & 2016 & 1879 & 0 & 1879 & 230 & 1604\\
O & 2017 & 1739 & 0 & 1739 & 0 & 1636\\
O & 2018 & 1891 & 2 & 1892 & 0 & 1742\\
O & 2019 & 1881 & 0 & 1881 & 0 & 1710\\
O & 2020 & 1282 & 0 & 1282 & 0 & 1120\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 3301 & 0 & 3301 & 0 & 3109\\
W & 1981 & 1984 & 0 & 1984 & 1753 & 100\\
W & 1982 & 2194 & 3 & 2196 & 1291 & 897\\
W & 1983 & 1898 & 0 & 1898 & 1482 & 300\\
W & 1984 & 1900 & 0 & 1900 & 1859 & 0\\
W & 1985 & 2099 & 0 & 2099 & 2097 & 0\\
W & 1986 & 1799 & 0 & 1799 & 1795 & 0\\
W & 1987 & 1200 & 1 & 1201 & 1196 & 0\\
W & 1988 & 200 & 0 & 200 & 200 & 0\\
W & 1994 & 1677 & 33 & 1710 & 0 & 0\\
W & 1995 & 2304 & 1 & 2305 & 0 & 0\\
W & 1996 & 1800 & 92 & 1892 & 0 & 0\\
W & 1997 & 1898 & 105 & 2003 & 0 & 0\\
W & 1998 & 2359 & 22 & 2381 & 0 & 0\\
W & 1999 & 1357 & 12 & 1369 & 902 & 0\\
W & 2000 & 723 & 2 & 725 & 285 & 0\\
W & 2001 & 722 & 1 & 723 & 704 & 0\\
W & 2002 & 887 & 56 & 943 & 819 & 0\\
W & 2003 & 792 & 54 & 846 & 799 & 0\\
W & 2004 & 331 & 7 & 338 & 300 & 0\\
W & 2005 & 323 & 0 & 323 & 300 & 0\\
W & 2006 & 345 & 50 & 395 & 218 & 0\\
W & 2007 & 809 & 101 & 910 & 605 & 0\\
W & 2008 & 870 & 2 & 872 & 462 & 0\\
W & 2009 & 1178 & 0 & 1178 & 634 & 96\\
W & 2010 & 522 & 0 & 522 & 240 & 96\\
W & 2011 & 459 & 1 & 460 & 303 & 0\\
W & 2012 & 573 & 0 & 573 & 331 & 0\\
W & 2013 & 507 & 16 & 523 & 266 & 0\\
W & 2014 & 609 & 7 & 616 & 295 & 0\\
W & 2015 & 425 & 11 & 436 & 213 & 0\\
W & 2016 & 311 & 0 & 311 & 254 & 0\\
W & 2017 & 570 & 0 & 570 & 0 & 0\\
W & 2018 & 351 & 0 & 351 & 0 & 0\\
W & 2019 & 600 & 1 & 601 & 0 & 0\\
W & 2020 & 336 & 1 & 337 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 46 & 1425 & 1 & 1426 & 432 & 2\\
2004 & 34 & 565 & 0 & 565 & 219 & 0\\
2005 & 38 & 525 & 1 & 526 & 257 & 0\\
2006 & 33 & 659 & 0 & 659 & 254 & 1\\
2007 & 50 & 627 & 1 & 628 & 439 & 0\\
2008 & 39 & 505 & 34 & 539 & 328 & 0\\
2009 & 46 & 435 & 36 & 471 & 331 & 2\\
2010 & 53 & 903 & 4 & 907 & 579 & 0\\
2011 & 53 & 849 & 72 & 921 & 674 & 3\\
2012 & 50 & 1166 & 9 & 1175 & 0 & 699\\
2013 & 45 & 730 & 2 & 732 & 199 & 354\\
2014 & 52 & 902 & 89 & 991 & 626 & 0\\
2015 & 69 & 1143 & 22 & 1165 & 840 & 5\\
2016 & 50 & 1150 & 0 & 1150 & 703 & 67\\
2017 & 56 & 968 & 8 & 976 & 0 & 778\\
2018 & 40 & 642 & 6 & 648 & 0 & 537\\
2019 & 19 & 345 & 0 & 345 & 0 & 291\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Pacific sanddab 
 The most recent assessment of Pacific sanddab was a data-limited assessment conducted in 2010. Across available data, Pacific sanddab have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 38,299 length observations, 159 age readings, and 16,382 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 36,350 length observations, 0 age readings, and 14,759 otoliths that are available to be aged. In California, since 2000, a total of 23,128 length observations, 0 age readings, and 2,552 otoliths have been collected In Oregon, since 2000, a total of 13,222 length observations, 0 age readings, and 12,207 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 46,530 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 46,494 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 44,994 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 1,492 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 8 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 82,948 length observations, 7,986 age readings, and 5,262 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2002 & 0 & 46 & 46 & 0 & 0\\
C & 2003 & 1304 & 0 & 1304 & 0 & 409\\
C & 2004 & 757 & 125 & 882 & 0 & 72\\
C & 2005 & 994 & 134 & 1024 & 0 & 112\\
C & 2006 & 1979 & 125 & 2104 & 0 & 184\\
C & 2007 & 1516 & 116 & 1632 & 0 & 770\\
C & 2008 & 1319 & 203 & 1522 & 0 & 860\\
C & 2009 & 759 & 190 & 949 & 0 & 0\\
C & 2010 & 688 & 390 & 1078 & 0 & 60\\
C & 2011 & 246 & 163 & 409 & 0 & 0\\
C & 2012 & 355 & 230 & 585 & 0 & 5\\
C & 2013 & 1077 & 302 & 1379 & 0 & 0\\
C & 2014 & 961 & 278 & 1239 & 0 & 0\\
C & 2015 & 1672 & 216 & 1823 & 0 & 0\\
C & 2016 & 2203 & 100 & 2303 & 0 & 80\\
C & 2017 & 1607 & 16 & 1623 & 0 & 0\\
C & 2018 & 1454 & 10 & 1464 & 0 & 0\\
C & 2019 & 829 & 548 & 1376 & 0 & 0\\
C & 2020 & 231 & 155 & 386 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1994 & 147 & 0 & 147 & 145 & 2\\
O & 1995 & 215 & 0 & 215 & 9 & 206\\
O & 1996 & 160 & 0 & 160 & 0 & 61\\
O & 1997 & 585 & 0 & 585 & 5 & 515\\
O & 1998 & 588 & 0 & 588 & 0 & 588\\
O & 1999 & 251 & 0 & 251 & 0 & 251\\
O & 2000 & 414 & 0 & 414 & 0 & 414\\
O & 2001 & 399 & 104 & 503 & 0 & 400\\
O & 2002 & 539 & 0 & 538 & 0 & 379\\
O & 2003 & 340 & 0 & 340 & 0 & 340\\
O & 2004 & 481 & 0 & 481 & 0 & 481\\
O & 2005 & 566 & 0 & 566 & 0 & 566\\
O & 2006 & 804 & 0 & 804 & 0 & 804\\
O & 2007 & 630 & 0 & 630 & 0 & 540\\
O & 2008 & 470 & 0 & 470 & 0 & 410\\
O & 2009 & 930 & 0 & 930 & 0 & 830\\
O & 2010 & 834 & 0 & 834 & 0 & 804\\
O & 2011 & 830 & 0 & 830 & 0 & 730\\
O & 2012 & 709 & 0 & 709 & 0 & 709\\
O & 2013 & 852 & 0 & 851 & 0 & 852\\
O & 2014 & 835 & 0 & 835 & 0 & 835\\
O & 2015 & 759 & 1 & 760 & 0 & 670\\
O & 2016 & 649 & 1 & 650 & 0 & 530\\
O & 2017 & 699 & 2 & 701 & 0 & 685\\
O & 2018 & 664 & 2 & 666 & 0 & 578\\
O & 2019 & 550 & 0 & 550 & 0 & 490\\
O & 2020 & 160 & 0 & 160 & 0 & 160\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 0 & 3 & 3 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 21 & 21 & 0 & 0\\
C & 2004 & 0 & 1742 & 1742 & 0 & 0\\
C & 2005 & 1 & 1833 & 1834 & 0 & 0\\
C & 2006 & 0 & 2864 & 2864 & 0 & 0\\
C & 2007 & 0 & 2105 & 2105 & 0 & 0\\
C & 2008 & 0 & 4007 & 4007 & 0 & 0\\
C & 2009 & 11 & 2971 & 2982 & 0 & 0\\
C & 2010 & 0 & 3739 & 3739 & 0 & 0\\
C & 2011 & 70 & 3372 & 3442 & 0 & 0\\
C & 2012 & 211 & 4480 & 4691 & 0 & 0\\
C & 2013 & 1038 & 3891 & 4929 & 0 & 0\\
C & 2014 & 1393 & 2461 & 3854 & 0 & 0\\
C & 2015 & 779 & 1007 & 1786 & 0 & 0\\
C & 2016 & 980 & 971 & 1951 & 0 & 0\\
C & 2017 & 867 & 790 & 1657 & 0 & 0\\
C & 2018 & 782 & 833 & 1615 & 0 & 0\\
C & 2019 & 651 & 533 & 1184 & 0 & 0\\
C & 2020 & 270 & 322 & 591 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 18 & 18 & 0 & 0\\
O & 2002 & 0 & 18 & 18 & 0 & 0\\
O & 2003 & 0 & 56 & 56 & 0 & 0\\
O & 2004 & 0 & 339 & 339 & 0 & 0\\
O & 2005 & 0 & 82 & 82 & 0 & 0\\
O & 2006 & 0 & 29 & 29 & 0 & 0\\
O & 2007 & 0 & 7 & 7 & 0 & 0\\
O & 2008 & 0 & 46 & 46 & 0 & 0\\
O & 2009 & 0 & 13 & 13 & 0 & 0\\
O & 2010 & 0 & 85 & 85 & 0 & 0\\
O & 2011 & 0 & 49 & 49 & 0 & 0\\
O & 2012 & 0 & 151 & 151 & 0 & 0\\
O & 2013 & 0 & 69 & 69 & 0 & 0\\
O & 2014 & 0 & 82 & 82 & 0 & 0\\
O & 2015 & 0 & 32 & 32 & 0 & 0\\
O & 2016 & 0 & 39 & 39 & 0 & 0\\
O & 2017 & 0 & 277 & 277 & 0 & 0\\
O & 2018 & 0 & 59 & 59 & 0 & 0\\
O & 2019 & 0 & 65 & 65 & 0 & 0\\
O & 2020 & 0 & 12 & 12 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2008 & 0 & 1 & 1 & 0 & 0\\
W & 2019 & 0 & 1 & 1 & 0 & 0\\
W & 2021 & 0 & 6 & 6 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 133 & 8853 & 74 & 8927 & 787 & 8\\
2004 & 165 & 10755 & 0 & 10755 & 1419 & 8\\
2005 & 219 & 10126 & 0 & 10126 & 993 & 14\\
2006 & 178 & 5902 & 2 & 5904 & 704 & 15\\
2007 & 190 & 4293 & 26 & 4319 & 725 & 10\\
2008 & 202 & 4527 & 9 & 4536 & 768 & 6\\
2009 & 216 & 2815 & 6 & 2821 & 0 & 813\\
2010 & 242 & 1501 & 11 & 1512 & 1018 & 133\\
2011 & 240 & 4486 & 90 & 4576 & 752 & 96\\
2012 & 241 & 4536 & 78 & 4614 & 820 & 106\\
2013 & 160 & 3275 & 52 & 3327 & 0 & 618\\
2014 & 242 & 4992 & 47 & 5039 & 0 & 929\\
2015 & 246 & 4997 & 34 & 5026 & 0 & 926\\
2016 & 223 & 4565 & 6 & 4571 & 0 & 463\\
2017 & 242 & 3057 & 6 & 3063 & 0 & 466\\
2018 & 225 & 2590 & 7 & 2597 & 0 & 436\\
2019 & 110 & 1230 & 5 & 1235 & 0 & 215\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Pacific spiny dogfish 
 The most recent assessment of Pacific spiny dogfish was a full assessment conducted in 2021. Across available data, Pacific spiny dogfish have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 16,282 length observations, 2,956 age readings, and 459 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 15,369 length observations, 2,956 age readings, and 459 otoliths that are available to be aged. In California, since 2000, a total of 330 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 2,846 length observations, 0 age readings, and 459 otoliths have been collected In Washington, since 2000, a total of 12,193 length observations, 2,956 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 1,130 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 1,128 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 1,076 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 52 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 30,890 length observations, 591 age readings, and 8,911 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2009 & 176 & 0 & 176 & 0 & 0\\
C & 2011 & 1 & 0 & 1 & 0 & 0\\
C & 2012 & 3 & 0 & 3 & 0 & 0\\
C & 2014 & 97 & 0 & 97 & 0 & 0\\
C & 2015 & 2 & 30 & 32 & 0 & 0\\
C & 2017 & 3 & 0 & 3 & 0 & 0\\
C & 2018 & 17 & 0 & 17 & 0 & 0\\
C & 2020 & 1 & 0 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2008 & 4 & 0 & 4 & 0 & 4\\
O & 2009 & 6 & 0 & 6 & 0 & 6\\
O & 2010 & 113 & 0 & 113 & 0 & 50\\
O & 2011 & 113 & 0 & 113 & 0 & 30\\
O & 2012 & 143 & 0 & 143 & 0 & 30\\
O & 2013 & 96 & 0 & 96 & 0 & 0\\
O & 2014 & 194 & 0 & 194 & 0 & 0\\
O & 2015 & 377 & 0 & 377 & 0 & 0\\
O & 2016 & 501 & 0 & 501 & 0 & 0\\
O & 2017 & 283 & 0 & 283 & 0 & 1\\
O & 2018 & 285 & 0 & 285 & 0 & 21\\
O & 2019 & 266 & 0 & 266 & 0 & 10\\
O & 2020 & 465 & 0 & 465 & 0 & 307\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 200 & 0 & 200 & 0 & 0\\
W & 1981 & 532 & 0 & 532 & 0 & 0\\
W & 1991 & 181 & 0 & 181 & 0 & 0\\
W & 2003 & 125 & 0 & 125 & 0 & 0\\
W & 2004 & 93 & 1 & 94 & 0 & 0\\
W & 2005 & 388 & 0 & 388 & 188 & 0\\
W & 2006 & 1701 & 91 & 1792 & 220 & 0\\
W & 2007 & 3210 & 35 & 3243 & 1154 & 0\\
W & 2008 & 1809 & 3 & 1809 & 824 & 0\\
W & 2009 & 800 & 0 & 800 & 399 & 0\\
W & 2010 & 649 & 0 & 649 & 171 & 0\\
W & 2011 & 748 & 1 & 749 & 0 & 0\\
W & 2012 & 280 & 0 & 280 & 0 & 0\\
W & 2013 & 419 & 0 & 419 & 0 & 0\\
W & 2014 & 399 & 26 & 425 & 0 & 0\\
W & 2015 & 674 & 0 & 674 & 0 & 0\\
W & 2016 & 208 & 0 & 208 & 0 & 0\\
W & 2017 & 200 & 0 & 200 & 0 & 0\\
W & 2018 & 237 & 1 & 238 & 0 & 0\\
W & 2019 & 75 & 0 & 75 & 0 & 0\\
W & 2020 & 25 & 0 & 25 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 1 & 1 & 2 & 0 & \vphantom{1} 0\\
C & 2004 & 0 & 23 & 23 & 0 & \vphantom{1} 0\\
C & 2005 & 4 & 50 & 54 & 0 & \vphantom{1} 0\\
C & 2006 & 18 & 83 & 101 & 0 & \vphantom{1} 0\\
C & 2007 & 17 & 36 & 53 & 0 & \vphantom{1} 0\\
C & 2008 & 20 & 23 & 43 & 0 & \vphantom{1} 0\\
C & 2009 & 22 & 16 & 38 & 0 & \vphantom{1} 0\\
C & 2010 & 15 & 9 & 24 & 0 & \vphantom{1} 0\\
C & 2011 & 3 & 36 & 39 & 0 & \vphantom{1} 0\\
C & 2012 & 4 & 17 & 21 & 0 & \vphantom{1} 0\\
C & 2013 & 8 & 17 & 25 & 0 & \vphantom{1} 0\\
C & 2014 & 11 & 9 & 20 & 0 & \vphantom{1} 0\\
C & 2015 & 10 & 9 & 19 & 0 & \vphantom{1} 0\\
C & 2016 & 5 & 2 & 7 & 0 & \vphantom{1} 0\\
C & 2017 & 26 & 2 & 28 & 0 & \vphantom{1} 0\\
C & 2018 & 13 & 4 & 17 & 0 & \vphantom{1} 0\\
C & 2019 & 22 & 2 & 24 & 0 & \vphantom{1} 0\\
C & 2003 & 1 & 1 & 2 & 0 & 0\\
C & 2004 & 0 & 23 & 23 & 0 & 0\\
C & 2005 & 4 & 50 & 54 & 0 & 0\\
C & 2006 & 18 & 83 & 101 & 0 & 0\\
C & 2007 & 17 & 36 & 53 & 0 & 0\\
C & 2008 & 20 & 23 & 43 & 0 & 0\\
C & 2009 & 22 & 16 & 38 & 0 & 0\\
C & 2010 & 15 & 9 & 24 & 0 & 0\\
C & 2011 & 3 & 36 & 39 & 0 & 0\\
C & 2012 & 4 & 17 & 21 & 0 & 0\\
C & 2013 & 8 & 17 & 25 & 0 & 0\\
C & 2014 & 11 & 9 & 20 & 0 & 0\\
C & 2015 & 10 & 9 & 19 & 0 & 0\\
C & 2016 & 5 & 2 & 7 & 0 & 0\\
C & 2017 & 26 & 2 & 28 & 0 & 0\\
C & 2018 & 13 & 4 & 17 & 0 & 0\\
C & 2019 & 22 & 2 & 24 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2002 & 0 & 1 & 1 & 0 & \vphantom{1} 0\\
O & 2003 & 0 & 3 & 3 & 0 & \vphantom{1} 0\\
O & 2004 & 0 & 2 & 2 & 0 & \vphantom{1} 0\\
O & 2005 & 0 & 1 & 1 & 0 & \vphantom{1} 0\\
O & 2006 & 0 & 2 & 2 & 0 & \vphantom{1} 0\\
O & 2007 & 0 & 2 & 2 & 0 & \vphantom{1} 0\\
O & 2009 & 0 & 5 & 5 & 0 & \vphantom{1} 0\\
O & 2010 & 0 & 2 & 2 & 0 & \vphantom{1} 0\\
O & 2012 & 0 & 4 & 4 & 0 & \vphantom{1} 0\\
O & 2014 & 0 & 2 & 2 & 0 & \vphantom{1} 0\\
O & 2015 & 0 & 1 & 1 & 0 & \vphantom{1} 0\\
O & 2017 & 0 & 1 & 1 & 0 & \vphantom{1} 0\\
O & 2018 & 0 & 1 & 1 & 0 & \vphantom{1} 0\\
O & 2002 & 0 & 1 & 1 & 0 & 0\\
O & 2003 & 0 & 3 & 3 & 0 & 0\\
O & 2004 & 0 & 2 & 2 & 0 & 0\\
O & 2005 & 0 & 1 & 1 & 0 & 0\\
O & 2006 & 0 & 2 & 2 & 0 & 0\\
O & 2007 & 0 & 2 & 2 & 0 & 0\\
O & 2009 & 0 & 5 & 5 & 0 & 0\\
O & 2010 & 0 & 2 & 2 & 0 & 0\\
O & 2012 & 0 & 4 & 4 & 0 & 0\\
O & 2014 & 0 & 2 & 2 & 0 & 0\\
O & 2015 & 0 & 1 & 1 & 0 & 0\\
O & 2017 & 0 & 1 & 1 & 0 & 0\\
O & 2018 & 0 & 1 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 178 & 3789 & 2 & 3789 & 0 & 673\\
2004 & 160 & 2482 & 1 & 2481 & 0 & 540\\
2005 & 251 & 3565 & 1 & 3566 & 0 & 864\\
2006 & 224 & 3882 & 18 & 3900 & 0 & 786\\
2007 & 224 & 2419 & 2 & 2421 & 0 & 749\\
2008 & 249 & 2847 & 27 & 2874 & 0 & 887\\
2009 & 205 & 1658 & 3 & 1661 & 0 & 632\\
2010 & 226 & 1723 & 4 & 1727 & 591 & 440\\
2011 & 200 & 1635 & 2 & 1637 & 0 & 639\\
2012 & 173 & 1507 & 2 & 1509 & 0 & 563\\
2013 & 95 & 616 & 0 & 613 & 0 & 287\\
2014 & 154 & 1496 & 2 & 1476 & 0 & 470\\
2015 & 145 & 669 & 1 & 670 & 0 & 350\\
2016 & 119 & 771 & 0 & 771 & 0 & 291\\
2017 & 100 & 532 & 0 & 532 & 0 & 223\\
2018 & 135 & 774 & 0 & 774 & 0 & 330\\
2019 & 65 & 489 & 0 & 489 & 0 & 187\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Petrale sole 
 The most recent assessment of Petrale sole was a update assessment conducted in 2019. Across available data, Petrale sole have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 136,238 length observations, 18,591 age readings, and 49,662 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 122,133 length observations, 15,752 age readings, and 39,821 otoliths that are available to be aged. In California, since 2000, a total of 54,489 length observations, 2,163 age readings, and 2,593 otoliths have been collected In Oregon, since 2000, a total of 40,755 length observations, 7,345 age readings, and 27,269 otoliths have been collected In Washington, since 2000, a total of 26,889 length observations, 6,244 age readings, and 9,959 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 68,578 length observations, 12,551 age readings, and 7,313 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1990 & 484 & 0 & 484 & 321 & 200\\
C & 1991 & 467 & 0 & 467 & 294 & 200\\
C & 1992 & 88 & 0 & 88 & 0 & 0\\
C & 1994 & 0 & 1 & 1 & 0 & 0\\
C & 2001 & 478 & 116 & 594 & 0 & 0\\
C & 2002 & 305 & 156 & 461 & 0 & 0\\
C & 2003 & 719 & 30 & 749 & 116 & 196\\
C & 2004 & 894 & 105 & 965 & 182 & 273\\
C & 2005 & 1330 & 125 & 1407 & 300 & 274\\
C & 2006 & 1988 & 297 & 2284 & 231 & 206\\
C & 2007 & 4708 & 269 & 4977 & 210 & 177\\
C & 2008 & 4285 & 578 & 4862 & 440 & 403\\
C & 2009 & 2034 & 468 & 2502 & 78 & 78\\
C & 2010 & 1620 & 391 & 2011 & 0 & 6\\
C & 2011 & 1543 & 236 & 1778 & 253 & 239\\
C & 2012 & 2024 & 350 & 2374 & 183 & 154\\
C & 2013 & 3589 & 234 & 3823 & 170 & 148\\
C & 2014 & 2859 & 157 & 2964 & 0 & 97\\
C & 2015 & 2166 & 978 & 3088 & 0 & 0\\
C & 2016 & 2599 & 1018 & 3617 & 0 & 176\\
C & 2017 & 3108 & 670 & 3778 & 0 & 166\\
C & 2018 & 3105 & 381 & 3486 & 0 & 0\\
C & 2019 & 3556 & 940 & 4496 & 0 & 0\\
C & 2020 & 3655 & 619 & 4273 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1987 & 1105 & 0 & 1105 & 348 & 757\\
O & 1988 & 900 & 0 & 900 & 91 & 809\\
O & 1989 & 803 & 0 & 803 & 651 & 152\\
O & 1990 & 803 & 0 & 803 & 410 & 393\\
O & 1991 & 633 & 0 & 633 & 8 & 625\\
O & 1992 & 741 & 0 & 741 & 55 & 686\\
O & 1993 & 532 & 0 & 532 & 0 & 532\\
O & 1994 & 629 & 0 & 629 & 46 & 583\\
O & 1995 & 296 & 0 & 296 & 0 & 296\\
O & 1996 & 235 & 0 & 235 & 0 & 235\\
O & 1997 & 748 & 0 & 748 & 6 & 742\\
O & 1998 & 555 & 0 & 555 & 344 & 211\\
O & 1999 & 466 & 0 & 466 & 265 & 130\\
O & 2000 & 777 & 0 & 777 & 0 & 750\\
O & 2001 & 504 & 0 & 504 & 0 & 426\\
O & 2002 & 919 & 0 & 919 & 0 & 781\\
O & 2003 & 1092 & 0 & 1092 & 0 & 987\\
O & 2004 & 939 & 0 & 939 & 0 & 873\\
O & 2005 & 945 & 0 & 945 & 0 & 810\\
O & 2006 & 2045 & 0 & 2045 & 0 & 1634\\
O & 2007 & 1940 & 0 & 1940 & 455 & 1114\\
O & 2008 & 2409 & 0 & 2409 & 483 & 1506\\
O & 2009 & 2952 & 0 & 2952 & 537 & 1134\\
O & 2010 & 2692 & 61 & 2753 & 506 & 1503\\
O & 2011 & 1748 & 1 & 1749 & 529 & 1068\\
O & 2012 & 1872 & 0 & 1872 & 621 & 1161\\
O & 2013 & 2537 & 0 & 2537 & 715 & 1597\\
O & 2014 & 3380 & 0 & 3379 & 745 & 2455\\
O & 2015 & 2984 & 0 & 2984 & 718 & 1861\\
O & 2016 & 1916 & 0 & 1916 & 523 & 1273\\
O & 2017 & 2985 & 1 & 2986 & 761 & 1894\\
O & 2018 & 2336 & 0 & 2336 & 752 & 1224\\
O & 2019 & 2348 & 37 & 2385 & 0 & 2032\\
O & 2020 & 1336 & 0 & 1336 & 0 & 1186\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 2858 & 2 & 2860 & 0 & 1747\\
W & 1981 & 400 & 0 & 400 & 0 & 195\\
W & 1998 & 639 & 11 & 650 & 0 & 648\\
W & 1999 & 708 & 1 & 709 & 0 & 700\\
W & 2000 & 878 & 3 & 878 & 0 & 829\\
W & 2001 & 743 & 7 & 750 & 0 & 736\\
W & 2002 & 887 & 4 & 891 & 0 & 697\\
W & 2003 & 1186 & 1 & 1187 & 0 & 1139\\
W & 2004 & 1179 & 0 & 1179 & 0 & 1174\\
W & 2005 & 1333 & 0 & 1333 & 0 & 1325\\
W & 2006 & 1847 & 2 & 1849 & 0 & 1043\\
W & 2007 & 2138 & 4 & 2142 & 0 & 1099\\
W & 2008 & 1872 & 1 & 1873 & 0 & 1031\\
W & 2009 & 1671 & 0 & 1671 & 543 & 547\\
W & 2010 & 995 & 0 & 995 & 389 & 339\\
W & 2011 & 1123 & 0 & 1123 & 643 & 0\\
W & 2012 & 1141 & 0 & 1141 & 599 & 0\\
W & 2013 & 1906 & 2 & 1908 & 840 & 0\\
W & 2014 & 1200 & 0 & 1200 & 549 & 0\\
W & 2015 & 1430 & 1 & 1431 & 725 & 0\\
W & 2016 & 220 & 0 & 220 & 115 & 0\\
W & 2017 & 2095 & 0 & 2095 & 755 & 0\\
W & 2018 & 1722 & 50 & 1772 & 813 & 0\\
W & 2019 & 1112 & 28 & 1140 & 273 & 0\\
W & 2020 & 111 & 0 & 111 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 197 & 2833 & 4 & 2837 & 765 & 835\\
2004 & 212 & 3345 & 1 & 3346 & 723 & 1133\\
2005 & 278 & 4539 & 16 & 4555 & 752 & 1093\\
2006 & 247 & 3664 & 4 & 3668 & 774 & 201\\
2007 & 257 & 3403 & 6 & 3409 & 690 & 289\\
2008 & 257 & 3042 & 5 & 3047 & 746 & 279\\
2009 & 277 & 3385 & 2 & 3387 & 777 & 283\\
2010 & 325 & 6049 & 3 & 6052 & 801 & 593\\
2011 & 320 & 6172 & 4 & 6176 & 799 & 561\\
2012 & 295 & 5366 & 6 & 5372 & 777 & 510\\
2013 & 218 & 3440 & 5 & 3445 & 843 & 1\\
2014 & 332 & 4805 & 17 & 4822 & 766 & 472\\
2015 & 312 & 4253 & 4 & 4236 & 751 & 406\\
2016 & 309 & 4383 & 2 & 4385 & 893 & 23\\
2017 & 314 & 4260 & 1 & 4261 & 884 & 4\\
2018 & 291 & 3782 & 1 & 3783 & 810 & 6\\
2019 & 155 & 1795 & 2 & 1797 & 0 & 624\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Quillback rockfish 
 The most recent assessment of Quillback rockfish was a data-moderate assessment conducted in 2021. Across available data, Quillback rockfish have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 6,579 length observations, 185 age readings, and 1,106 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 4,275 length observations, 185 age readings, and 1,106 otoliths that are available to be aged. In California, since 2000, a total of 1,046 length observations, 0 age readings, and 36 otoliths have been collected In Oregon, since 2000, a total of 3,041 length observations, 138 age readings, and 1,070 otoliths have been collected In Washington, since 2000, a total of 188 length observations, 47 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 29,344 length observations, 2,895 age readings, and 3,715 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 28,002 length observations, 2,886 age readings, and 3,715 otoliths that are available to be aged. In California, since 2003, a total of 5,057 length observations, 0 age readings, and 29 otoliths have been collected In Oregon, since 2003, a total of 18,366 length observations, 783 age readings, and 3,344 otoliths have been collected In Washington, since 2003, a total of 4,579 length observations, 2,103 age readings, and 342 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 219 length observations, 175 age readings, and 1 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1984 & 1 & 0 & 1 & 0 & 0\\
C & 1987 & 1 & 0 & 1 & 0 & 0\\
C & 1991 & 3 & 155 & 158 & 0 & 0\\
C & 1992 & 0 & 260 & 260 & 0 & 0\\
C & 1993 & 0 & 97 & 97 & 0 & 0\\
C & 1994 & 0 & 295 & 295 & 0 & 0\\
C & 1995 & 2 & 124 & 126 & 0 & 0\\
C & 1996 & 0 & 132 & 132 & 0 & 0\\
C & 1997 & 0 & 150 & 150 & 0 & 0\\
C & 1998 & 0 & 16 & 16 & 0 & 0\\
C & 1999 & 1 & 579 & 580 & 0 & 0\\
C & 2000 & 0 & 41 & 41 & 0 & 0\\
C & 2001 & 1 & 321 & 322 & 0 & 0\\
C & 2002 & 0 & 17 & 17 & 0 & 0\\
C & 2004 & 4 & 10 & 14 & 0 & 4\\
C & 2005 & 0 & 16 & 16 & 0 & 0\\
C & 2006 & 0 & 19 & 19 & 0 & 0\\
C & 2007 & 27 & 111 & 138 & 0 & 27\\
C & 2008 & 0 & 108 & 108 & 0 & 0\\
C & 2009 & 0 & 39 & 39 & 0 & 0\\
C & 2010 & 0 & 16 & 16 & 0 & 0\\
C & 2011 & 2 & 5 & 7 & 0 & 1\\
C & 2012 & 5 & 13 & 15 & 0 & 4\\
C & 2013 & 0 & 13 & 13 & 0 & 0\\
C & 2014 & 0 & 5 & 5 & 0 & 0\\
C & 2015 & 0 & 20 & 20 & 0 & 0\\
C & 2016 & 0 & 16 & 16 & 0 & 0\\
C & 2017 & 0 & 49 & 49 & 0 & 0\\
C & 2018 & 0 & 117 & 31 & 0 & 0\\
C & 2019 & 75 & 68 & 86 & 0 & 0\\
C & 2020 & 74 & 0 & 74 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1998 & 4 & 0 & 4 & 0 & 0\\
O & 1999 & 25 & 0 & 25 & 0 & 0\\
O & 2000 & 200 & 0 & 200 & 0 & 0\\
O & 2001 & 214 & 0 & 214 & 0 & 0\\
O & 2002 & 59 & 0 & 59 & 2 & 1\\
O & 2003 & 48 & 0 & 48 & 9 & 0\\
O & 2004 & 134 & 0 & 134 & 63 & 0\\
O & 2005 & 20 & 0 & 20 & 1 & 0\\
O & 2006 & 140 & 0 & 140 & 63 & 0\\
O & 2007 & 127 & 0 & 127 & 0 & 2\\
O & 2008 & 55 & 2 & 57 & 0 & 12\\
O & 2009 & 64 & 0 & 64 & 0 & 17\\
O & 2010 & 69 & 0 & 69 & 0 & 14\\
O & 2011 & 191 & 0 & 191 & 0 & 97\\
O & 2012 & 151 & 0 & 151 & 0 & 102\\
O & 2013 & 214 & 0 & 214 & 0 & 117\\
O & 2014 & 184 & 6 & 190 & 0 & 120\\
O & 2015 & 102 & 0 & 102 & 0 & 59\\
O & 2016 & 75 & 2 & 77 & 0 & 35\\
O & 2017 & 181 & 33 & 214 & 0 & 112\\
O & 2018 & 198 & 1 & 199 & 0 & 85\\
O & 2019 & 354 & 2 & 355 & 0 & 194\\
O & 2020 & 216 & 0 & 216 & 0 & 103\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 0 & 3 & 3 & 0 & 0\\
W & 1982 & 275 & 38 & 313 & 0 & 0\\
W & 1983 & 22 & 0 & 22 & 0 & 0\\
W & 1989 & 0 & 20 & 20 & 0 & 0\\
W & 1990 & 0 & 100 & 100 & 0 & 0\\
W & 1996 & 0 & 1 & 1 & 0 & 0\\
W & 2000 & 0 & 4 & 4 & 0 & 0\\
W & 2002 & 10 & 6 & 16 & 0 & 0\\
W & 2003 & 4 & 0 & 4 & 0 & 0\\
W & 2004 & 2 & 0 & 2 & 0 & 0\\
W & 2005 & 1 & 0 & 1 & 0 & 0\\
W & 2006 & 105 & 0 & 105 & 0 & 0\\
W & 2014 & 19 & 0 & 19 & 15 & 0\\
W & 2017 & 9 & 0 & 9 & 9 & 0\\
W & 2018 & 9 & 0 & 9 & 4 & 0\\
W & 2019 & 19 & 0 & 19 & 19 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 120 & 120 & 0 & 0\\
C & 2005 & 0 & 215 & 215 & 0 & 0\\
C & 2006 & 0 & 418 & 418 & 0 & 0\\
C & 2007 & 0 & 552 & 552 & 0 & 0\\
C & 2008 & 1 & 330 & 331 & 0 & 0\\
C & 2009 & 0 & 321 & 321 & 0 & 0\\
C & 2010 & 0 & 144 & 144 & 0 & 0\\
C & 2011 & 0 & 207 & 207 & 0 & 0\\
C & 2012 & 0 & 271 & 271 & 0 & 0\\
C & 2013 & 3 & 186 & 189 & 0 & 0\\
C & 2014 & 0 & 129 & 129 & 0 & 0\\
C & 2015 & 0 & 376 & 376 & 0 & 0\\
C & 2016 & 0 & 440 & 440 & 0 & 0\\
C & 2017 & 0 & 457 & 457 & 0 & 0\\
C & 2018 & 0 & 423 & 423 & 0 & 11\\
C & 2019 & 0 & 464 & 464 & 0 & 18\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 321 & 321 & 0 & 0\\
O & 2002 & 0 & 757 & 757 & 0 & 0\\
O & 2003 & 0 & 876 & 876 & 0 & 0\\
O & 2004 & 0 & 500 & 500 & 0 & 0\\
O & 2005 & 91 & 931 & 1022 & 91 & 0\\
O & 2006 & 343 & 1033 & 1376 & 336 & 3\\
O & 2007 & 309 & 1075 & 1384 & 0 & 311\\
O & 2008 & 363 & 1120 & 1483 & 356 & 0\\
O & 2009 & 245 & 825 & 1070 & 0 & 245\\
O & 2010 & 372 & 919 & 1291 & 0 & 374\\
O & 2011 & 333 & 1048 & 1381 & 0 & 337\\
O & 2012 & 475 & 1241 & 1716 & 0 & 475\\
O & 2013 & 283 & 753 & 1036 & 0 & 284\\
O & 2014 & 193 & 484 & 677 & 0 & 193\\
O & 2015 & 0 & 43 & 43 & 0 & 0\\
O & 2016 & 0 & 27 & 27 & 0 & 0\\
O & 2017 & 227 & 737 & 964 & 0 & 228\\
O & 2018 & 349 & 1352 & 1701 & 0 & 349\\
O & 2019 & 392 & 1235 & 1627 & 0 & 392\\
O & 2020 & 153 & 39 & 192 & 0 & 153\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 235 & 29 & 264 & 9 & 0\\
W & 2003 & 260 & 30 & 290 & 0 & 0\\
W & 2004 & 287 & 35 & 322 & 157 & 1\\
W & 2005 & 337 & 74 & 411 & 181 & 0\\
W & 2006 & 235 & 91 & 326 & 52 & 0\\
W & 2007 & 218 & 48 & 266 & 89 & 4\\
W & 2008 & 131 & 45 & 176 & 59 & 0\\
W & 2009 & 135 & 25 & 160 & 27 & 1\\
W & 2010 & 70 & 3 & 73 & 6 & 7\\
W & 2011 & 63 & 45 & 108 & 32 & 0\\
W & 2012 & 55 & 29 & 84 & 14 & 0\\
W & 2013 & 72 & 65 & 137 & 8 & 0\\
W & 2014 & 240 & 76 & 316 & 288 & 2\\
W & 2015 & 208 & 35 & 243 & 236 & 0\\
W & 2016 & 325 & 14 & 339 & 273 & 52\\
W & 2017 & 223 & 108 & 331 & 222 & 0\\
W & 2018 & 200 & 88 & 288 & 199 & 0\\
W & 2019 & 260 & 168 & 428 & 260 & 0\\
W & 2020 & 161 & 1 & 162 & 0 & 161\\
W & 2021 & 114 & 5 & 119 & 0 & 114\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 2 & 3 & 0 & 3 & 0 & 0\\
2005 & 2 & 2 & 0 & 2 & 2 & 0\\
2006 & 2 & 3 & 0 & 3 & 3 & 0\\
2007 & 2 & 20 & 0 & 20 & 16 & 0\\
2008 & 3 & 37 & 0 & 37 & 22 & 0\\
2009 & 2 & 3 & 0 & 3 & 3 & 0\\
2010 & 2 & 2 & 0 & 2 & 2 & 0\\
2011 & 4 & 6 & 0 & 6 & 6 & 0\\
2012 & 4 & 46 & 0 & 46 & 26 & 0\\
2013 & 2 & 1 & 1 & 2 & 1 & 0\\
2014 & 9 & 25 & 0 & 25 & 24 & 1\\
2015 & 6 & 7 & 1 & 8 & 8 & 0\\
2016 & 4 & 9 & 0 & 9 & 9 & 0\\
2017 & 7 & 15 & 1 & 16 & 16 & 0\\
2018 & 8 & 21 & 0 & 21 & 21 & 0\\
2019 & 3 & 16 & 0 & 16 & 16 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Redbanded rockfish 
 The most recent assessment of Redbanded rockfish was a data-limited assessment conducted in 2010. Across available data, Redbanded rockfish have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 32,088 length observations, 279 age readings, and 12,855 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 29,211 length observations, 279 age readings, and 12,283 otoliths that are available to be aged. In California, since 2000, a total of 4,824 length observations, 1 age readings, and 1,564 otoliths have been collected In Oregon, since 2000, a total of 11,364 length observations, 278 age readings, and 10,719 otoliths have been collected In Washington, since 2000, a total of 13,023 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 3,120 length observations, 0 age readings, and 2,932 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 34 & 8 & 42 & 0 & 34\\
C & 1981 & 48 & 2 & 50 & 0 & 48\\
C & 1982 & 38 & 1 & 39 & 0 & 27\\
C & 1983 & 117 & 1 & 118 & 0 & 115\\
C & 1984 & 253 & 0 & 253 & 0 & 0\\
C & 1985 & 327 & 9 & 336 & 0 & 342\\
C & 1986 & 102 & 0 & 102 & 0 & 5\\
C & 1987 & 48 & 0 & 48 & 0 & 0\\
C & 1988 & 38 & 0 & 38 & 0 & 0\\
C & 1989 & 56 & 10 & 66 & 0 & 0\\
C & 1990 & 29 & 2 & 31 & 0 & 0\\
C & 1991 & 37 & 7 & 44 & 0 & 0\\
C & 1992 & 48 & 32 & 80 & 0 & 0\\
C & 1993 & 12 & 44 & 56 & 0 & 0\\
C & 1994 & 46 & 8 & 54 & 0 & 0\\
C & 1995 & 32 & 37 & 69 & 0 & 0\\
C & 1996 & 109 & 128 & 237 & 0 & 0\\
C & 1997 & 32 & 18 & 50 & 0 & 0\\
C & 1998 & 37 & 24 & 61 & 0 & 0\\
C & 1999 & 34 & 18 & 52 & 0 & 0\\
C & 2000 & 71 & 3 & 74 & 0 & 0\\
C & 2001 & 24 & 26 & 50 & 0 & 9\\
C & 2002 & 48 & 45 & 93 & 0 & 53\\
C & 2003 & 46 & 13 & 58 & 0 & 44\\
C & 2004 & 17 & 6 & 23 & 1 & 11\\
C & 2005 & 77 & 15 & 92 & 0 & 52\\
C & 2006 & 258 & 11 & 269 & 0 & 141\\
C & 2007 & 240 & 3 & 243 & 0 & 110\\
C & 2008 & 343 & 46 & 389 & 0 & 118\\
C & 2009 & 128 & 64 & 192 & 0 & 51\\
C & 2010 & 249 & 71 & 319 & 0 & 115\\
C & 2011 & 320 & 62 & 374 & 0 & 280\\
C & 2012 & 152 & 73 & 198 & 0 & 92\\
C & 2013 & 131 & 79 & 171 & 0 & 100\\
C & 2014 & 115 & 160 & 246 & 0 & 54\\
C & 2015 & 216 & 335 & 496 & 0 & 141\\
C & 2016 & 258 & 216 & 432 & 0 & 148\\
C & 2017 & 270 & 160 & 411 & 0 & 45\\
C & 2018 & 115 & 162 & 277 & 0 & 0\\
C & 2019 & 115 & 43 & 158 & 0 & 0\\
C & 2020 & 154 & 105 & 259 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1995 & 1 & 0 & 1 & 0 & 1\\
O & 2000 & 14 & 0 & 14 & 0 & 14\\
O & 2001 & 24 & 0 & 24 & 0 & 24\\
O & 2002 & 44 & 0 & 44 & 0 & 0\\
O & 2003 & 120 & 0 & 120 & 6 & 103\\
O & 2004 & 142 & 0 & 142 & 0 & 128\\
O & 2005 & 95 & 0 & 95 & 24 & 71\\
O & 2006 & 158 & 0 & 158 & 0 & 150\\
O & 2007 & 442 & 0 & 442 & 38 & 379\\
O & 2008 & 453 & 14 & 467 & 51 & 359\\
O & 2009 & 349 & 0 & 349 & 6 & 343\\
O & 2010 & 647 & 1 & 648 & 7 & 610\\
O & 2011 & 1007 & 3 & 1010 & 22 & 980\\
O & 2012 & 1205 & 1 & 1206 & 58 & 1148\\
O & 2013 & 733 & 0 & 733 & 21 & 712\\
O & 2014 & 686 & 0 & 686 & 45 & 639\\
O & 2015 & 957 & 0 & 957 & 0 & 957\\
O & 2016 & 1000 & 0 & 1000 & 0 & 943\\
O & 2017 & 1023 & 0 & 1023 & 0 & 946\\
O & 2018 & 940 & 1 & 941 & 0 & 915\\
O & 2019 & 857 & 2 & 859 & 0 & 852\\
O & 2020 & 446 & 0 & 446 & 0 & 446\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 240 & 240 & 0 & 0\\
W & 1997 & 0 & 314 & 314 & 0 & 0\\
W & 1998 & 165 & 13 & 178 & 0 & 0\\
W & 1999 & 290 & 28 & 318 & 0 & 0\\
W & 2000 & 227 & 298 & 525 & 0 & 0\\
W & 2001 & 278 & 115 & 393 & 0 & 0\\
W & 2002 & 448 & 53 & 501 & 0 & 0\\
W & 2003 & 574 & 30 & 604 & 0 & 0\\
W & 2004 & 306 & 38 & 344 & 0 & 0\\
W & 2005 & 102 & 0 & 102 & 0 & 0\\
W & 2006 & 264 & 0 & 264 & 0 & 0\\
W & 2007 & 400 & 19 & 419 & 0 & 0\\
W & 2008 & 374 & 25 & 399 & 0 & 0\\
W & 2009 & 342 & 4 & 346 & 0 & 0\\
W & 2010 & 300 & 6 & 306 & 0 & 0\\
W & 2011 & 500 & 14 & 514 & 0 & 0\\
W & 2012 & 431 & 12 & 443 & 0 & 0\\
W & 2013 & 598 & 5 & 603 & 0 & 0\\
W & 2014 & 688 & 18 & 706 & 0 & 0\\
W & 2015 & 724 & 63 & 787 & 0 & 0\\
W & 2016 & 1169 & 44 & 1213 & 0 & 0\\
W & 2017 & 995 & 2 & 997 & 0 & 0\\
W & 2018 & 1369 & 2 & 1371 & 0 & 0\\
W & 2019 & 1552 & 1 & 1553 & 0 & 0\\
W & 2020 & 632 & 1 & 633 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 62 & 272 & 0 & 272 & 0 & 179\\
2004 & 36 & 156 & 2 & 158 & 0 & 144\\
2005 & 54 & 168 & 3 & 171 & 0 & 171\\
2006 & 48 & 145 & 7 & 152 & 0 & 145\\
2007 & 52 & 166 & 12 & 178 & 0 & 178\\
2008 & 52 & 142 & 4 & 146 & 0 & 146\\
2009 & 53 & 135 & 7 & 142 & 0 & 142\\
2010 & 37 & 166 & 1 & 167 & 0 & 167\\
2011 & 52 & 208 & 3 & 211 & 0 & 203\\
2012 & 61 & 143 & 6 & 149 & 0 & 149\\
2013 & 49 & 180 & 9 & 189 & 0 & 189\\
2014 & 58 & 219 & 4 & 223 & 0 & 203\\
2015 & 55 & 187 & 2 & 189 & 0 & 189\\
2016 & 48 & 111 & 3 & 114 & 0 & 114\\
2017 & 68 & 262 & 8 & 270 & 0 & 268\\
2018 & 53 & 266 & 7 & 273 & 0 & 236\\
2019 & 30 & 114 & 2 & 116 & 0 & 109\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Rex sole 
 The most recent assessment of Rex sole was a data-moderate assessment conducted in 2013. Across available data, Rex sole have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 49,884 length observations, 0 age readings, and 19,130 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 49,883 length observations, 0 age readings, and 19,130 otoliths that are available to be aged. In California, since 2000, a total of 26,674 length observations, 0 age readings, and 1,162 otoliths have been collected In Oregon, since 2000, a total of 21,879 length observations, 0 age readings, and 17,968 otoliths have been collected In Washington, since 2000, a total of 1,330 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 134,477 length observations, 0 age readings, and 10,122 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2007 & 2639 & 50 & 2688 & 0 & 80\\
C & 2008 & 2945 & 162 & 3106 & 0 & 302\\
C & 2009 & 1410 & 158 & 1564 & 0 & 118\\
C & 2010 & 1460 & 82 & 1542 & 0 & 2\\
C & 2011 & 2348 & 13 & 2361 & 0 & 235\\
C & 2012 & 1948 & 98 & 2046 & 0 & 172\\
C & 2013 & 2231 & 58 & 2289 & 0 & 37\\
C & 2014 & 1441 & 5 & 1446 & 0 & 12\\
C & 2015 & 1470 & 1 & 1471 & 0 & 11\\
C & 2016 & 2042 & 3 & 2045 & 0 & 153\\
C & 2017 & 2187 & 42 & 2229 & 0 & 40\\
C & 2018 & 906 & 99 & 1005 & 0 & 0\\
C & 2019 & 1341 & 2 & 1343 & 0 & 0\\
C & 2020 & 1455 & 84 & 1539 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2006 & 218 & 0 & 218 & 0 & 218\\
O & 2007 & 1613 & 7 & 1620 & 0 & 1260\\
O & 2008 & 1438 & 1 & 1439 & 0 & 1140\\
O & 2009 & 1809 & 1 & 1810 & 0 & 1570\\
O & 2010 & 2281 & 1 & 2281 & 0 & 1710\\
O & 2011 & 2037 & 0 & 2037 & 0 & 1350\\
O & 2012 & 1575 & 0 & 1575 & 0 & 1455\\
O & 2013 & 1449 & 7 & 1456 & 0 & 1456\\
O & 2014 & 1490 & 0 & 1490 & 0 & 1490\\
O & 2015 & 1371 & 0 & 1371 & 0 & 1221\\
O & 2016 & 1390 & 0 & 1390 & 0 & 1240\\
O & 2017 & 1535 & 0 & 1535 & 0 & 1175\\
O & 2018 & 1503 & 0 & 1503 & 0 & 979\\
O & 2019 & 1442 & 3 & 1445 & 0 & 1115\\
O & 2020 & 709 & 0 & 709 & 0 & 589\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 0 & 1 & 1 & 0 & 0\\
W & 2017 & 430 & 0 & 430 & 0 & 0\\
W & 2018 & 450 & 100 & 550 & 0 & 0\\
W & 2019 & 300 & 0 & 300 & 0 & 0\\
W & 2020 & 50 & 0 & 50 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 171 & 10843 & 0 & 10843 & 0 & 0\\
2004 & 305 & 13884 & 48 & 13932 & 0 & 0\\
2005 & 425 & 15980 & 27 & 16007 & 0 & 0\\
2006 & 396 & 11580 & 0 & 11580 & 0 & 0\\
2007 & 423 & 9608 & 13 & 9621 & 0 & 801\\
2008 & 399 & 7166 & 61 & 7227 & 0 & 763\\
2009 & 395 & 4088 & 26 & 4114 & 0 & 780\\
2010 & 442 & 2611 & 47 & 2658 & 0 & 885\\
2011 & 424 & 6921 & 67 & 6988 & 0 & 838\\
2012 & 432 & 7399 & 27 & 7426 & 0 & 850\\
2013 & 305 & 5720 & 9 & 5729 & 0 & 600\\
2014 & 432 & 8688 & 15 & 8682 & 0 & 852\\
2015 & 418 & 8535 & 17 & 8517 & 0 & 818\\
2016 & 426 & 8643 & 24 & 8667 & 0 & 841\\
2017 & 427 & 5203 & 28 & 5231 & 0 & 829\\
2018 & 431 & 4822 & 9 & 4831 & 0 & 850\\
2019 & 210 & 2423 & 1 & 2424 & 0 & 415\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Rock sole 
 The most recent assessment of Rock sole was a data-limited assessment conducted in 2010. Across available data, Rock sole have been observed and sampled generally by both commercial and recreational fisheries.  
 
 Across all years of available data, commercial fisheries have collected a total of 2,236 length observations, 0 age readings, and 812 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 1,767 length observations, 0 age readings, and 812 otoliths that are available to be aged. In California, since 2000, a total of 1,054 length observations, 0 age readings, and 107 otoliths have been collected In Oregon, since 2000, a total of 713 length observations, 0 age readings, and 705 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 926 length observations, 0 age readings, and 15 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 925 length observations, 0 age readings, and 15 otoliths that are available to be aged. In California, since 2003, a total of 836 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 65 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 24 length observations, 0 age readings, and 15 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 55 & 29 & 84 & 0 & 2\\
C & 2004 & 1 & 3 & 4 & 0 & 0\\
C & 2006 & 0 & 8 & 0 & 0 & 0\\
C & 2007 & 57 & 0 & 57 & 0 & 0\\
C & 2008 & 73 & 1 & 74 & 0 & 93\\
C & 2009 & 89 & 43 & 132 & 0 & 0\\
C & 2010 & 31 & 71 & 102 & 0 & 0\\
C & 2011 & 0 & 27 & 27 & 0 & 0\\
C & 2012 & 42 & 99 & 141 & 0 & 0\\
C & 2013 & 26 & 67 & 93 & 0 & 12\\
C & 2014 & 0 & 2 & 2 & 0 & 0\\
C & 2015 & 0 & 24 & 24 & 0 & 0\\
C & 2016 & 7 & 59 & 66 & 0 & 0\\
C & 2017 & 94 & 23 & 117 & 0 & 0\\
C & 2018 & 39 & 53 & 92 & 0 & 0\\
C & 2019 & 2 & 24 & 26 & 0 & 0\\
C & 2020 & 1 & 12 & 13 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2004 & 51 & 0 & 51 & 0 & 51\\
O & 2005 & 26 & 0 & 26 & 0 & 26\\
O & 2008 & 16 & 0 & 16 & 0 & 16\\
O & 2009 & 72 & 0 & 72 & 0 & 72\\
O & 2011 & 12 & 0 & 12 & 0 & 12\\
O & 2012 & 60 & 0 & 60 & 0 & 60\\
O & 2013 & 43 & 0 & 43 & 0 & 43\\
O & 2014 & 16 & 0 & 16 & 0 & 16\\
O & 2015 & 91 & 0 & 91 & 0 & 91\\
O & 2016 & 71 & 0 & 71 & 0 & 71\\
O & 2017 & 97 & 0 & 97 & 0 & 91\\
O & 2018 & 79 & 0 & 79 & 0 & 79\\
O & 2019 & 55 & 0 & 55 & 0 & 53\\
O & 2020 & 24 & 0 & 24 & 0 & 24\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1982 & 50 & 0 & 50 & 0 & 0\\
W & 1983 & 177 & 0 & 177 & 0 & 0\\
W & 1984 & 242 & 0 & 242 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 38 & 38 & 0 & 0\\
C & 2005 & 0 & 33 & 33 & 0 & 0\\
C & 2006 & 0 & 32 & 32 & 0 & 0\\
C & 2007 & 0 & 43 & 43 & 0 & 0\\
C & 2008 & 0 & 33 & 33 & 0 & 0\\
C & 2009 & 0 & 43 & 43 & 0 & 0\\
C & 2010 & 0 & 40 & 40 & 0 & 0\\
C & 2011 & 1 & 54 & 55 & 0 & 0\\
C & 2012 & 0 & 59 & 59 & 0 & 0\\
C & 2013 & 2 & 65 & 67 & 0 & 0\\
C & 2014 & 2 & 51 & 53 & 0 & 0\\
C & 2015 & 2 & 55 & 57 & 0 & 0\\
C & 2016 & 0 & 55 & 55 & 0 & 0\\
C & 2017 & 1 & 70 & 71 & 0 & 0\\
C & 2018 & 0 & 76 & 76 & 0 & 0\\
C & 2019 & 0 & 79 & 79 & 0 & 0\\
C & 2020 & 0 & 2 & 2 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 1 & 1 & 0 & 0\\
O & 2004 & 0 & 4 & 4 & 0 & 0\\
O & 2005 & 0 & 15 & 15 & 0 & 0\\
O & 2006 & 0 & 5 & 5 & 0 & 0\\
O & 2007 & 0 & 2 & 2 & 0 & 0\\
O & 2008 & 0 & 1 & 1 & 0 & 0\\
O & 2009 & 0 & 2 & 2 & 0 & 0\\
O & 2010 & 0 & 5 & 5 & 0 & 0\\
O & 2011 & 0 & 2 & 2 & 0 & 0\\
O & 2012 & 0 & 3 & 3 & 0 & 0\\
O & 2013 & 0 & 2 & 2 & 0 & 0\\
O & 2014 & 0 & 3 & 3 & 0 & 0\\
O & 2015 & 0 & 5 & 5 & 0 & 0\\
O & 2016 & 0 & 1 & 1 & 0 & 0\\
O & 2017 & 0 & 7 & 7 & 0 & 0\\
O & 2018 & 0 & 3 & 3 & 0 & 0\\
O & 2019 & 0 & 5 & 5 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2005 & 0 & 2 & 2 & 0 & 0\\
W & 2006 & 0 & 1 & 1 & 0 & 0\\
W & 2012 & 0 & 1 & 1 & 0 & 0\\
W & 2018 & 1 & 4 & 5 & 0 & 0\\
W & 2019 & 6 & 0 & 6 & 0 & 6\\
W & 2020 & 6 & 0 & 6 & 0 & 6\\
W & 2021 & 3 & 0 & 3 & 0 & 3\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Rosethorn rockfish 
 The most recent assessment of Rosethorn rockfish was a data-limited assessment conducted in 2010. Across available data, Rosethorn rockfish have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC HKL survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 4,614 length observations, 0 age readings, and 2,093 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 3,447 length observations, 0 age readings, and 1,947 otoliths that are available to be aged. In California, since 2000, a total of 201 length observations, 0 age readings, and 66 otoliths have been collected In Oregon, since 2000, a total of 1,962 length observations, 0 age readings, and 1,881 otoliths have been collected In Washington, since 2000, a total of 1,284 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 548 length observations, 0 age readings, and 5 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 364 length observations, 0 age readings, and 5 otoliths that are available to be aged. In California, since 2003, a total of 20 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 337 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 7 length observations, 0 age readings, and 5 otoliths have been collected 

Across all years of available data, the NWFSC HKL survey has collected a total of 36 length observations, 0 age readings, and 27 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 0 & 1 & 1 & 0 & 0\\
C & 1982 & 15 & 0 & 15 & 0 & 7\\
C & 1983 & 10 & 0 & 10 & 0 & 56\\
C & 1984 & 15 & 0 & 15 & 0 & 34\\
C & 1985 & 56 & 4 & 60 & 0 & 46\\
C & 1986 & 37 & 1 & 38 & 0 & 3\\
C & 1987 & 18 & 0 & 18 & 0 & 0\\
C & 1988 & 19 & 1 & 20 & 0 & 0\\
C & 1989 & 13 & 76 & 89 & 0 & 0\\
C & 1990 & 18 & 7 & 25 & 0 & 0\\
C & 1991 & 12 & 102 & 114 & 0 & 0\\
C & 1992 & 7 & 18 & 25 & 0 & 0\\
C & 1993 & 12 & 17 & 29 & 0 & 0\\
C & 1994 & 9 & 45 & 54 & 0 & 0\\
C & 1995 & 16 & 79 & 95 & 0 & 0\\
C & 1996 & 50 & 151 & 201 & 0 & 0\\
C & 1997 & 9 & 71 & 80 & 0 & 0\\
C & 1998 & 14 & 3 & 17 & 0 & 0\\
C & 1999 & 62 & 3 & 65 & 0 & 0\\
C & 2000 & 3 & 0 & 3 & 0 & 0\\
C & 2001 & 2 & 0 & 2 & 0 & 0\\
C & 2002 & 5 & 1 & 6 & 0 & 0\\
C & 2003 & 2 & 0 & 2 & 0 & 0\\
C & 2004 & 1 & 0 & 1 & 0 & 1\\
C & 2005 & 2 & 0 & 2 & 0 & 9\\
C & 2006 & 1 & 0 & 1 & 0 & 1\\
C & 2007 & 1 & 0 & 1 & 0 & 0\\
C & 2008 & 0 & 4 & 4 & 0 & 0\\
C & 2009 & 0 & 1 & 1 & 0 & 0\\
C & 2010 & 3 & 0 & 3 & 0 & 2\\
C & 2011 & 0 & 5 & 5 & 0 & 0\\
C & 2012 & 29 & 84 & 113 & 0 & 30\\
C & 2013 & 4 & 4 & 8 & 0 & 3\\
C & 2014 & 2 & 2 & 4 & 0 & 1\\
C & 2015 & 15 & 15 & 28 & 0 & 15\\
C & 2016 & 1 & 4 & 5 & 0 & 0\\
C & 2017 & 4 & 4 & 8 & 0 & 4\\
C & 2018 & 0 & 1 & 1 & 0 & 0\\
C & 2020 & 3 & 60 & 3 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1997 & 90 & 0 & 90 & 0 & 0\\
O & 2000 & 51 & 0 & 51 & 0 & 0\\
O & 2003 & 7 & 0 & 7 & 0 & 7\\
O & 2004 & 7 & 0 & 7 & 0 & 4\\
O & 2006 & 2 & 0 & 2 & 0 & 2\\
O & 2007 & 38 & 0 & 38 & 0 & 34\\
O & 2008 & 70 & 0 & 70 & 0 & 68\\
O & 2009 & 64 & 0 & 64 & 0 & 64\\
O & 2010 & 52 & 0 & 52 & 0 & 50\\
O & 2011 & 101 & 0 & 101 & 0 & 98\\
O & 2012 & 96 & 2 & 97 & 0 & 98\\
O & 2013 & 242 & 1 & 243 & 0 & 243\\
O & 2014 & 148 & 0 & 148 & 0 & 146\\
O & 2015 & 92 & 0 & 92 & 0 & 92\\
O & 2016 & 96 & 1 & 97 & 0 & 96\\
O & 2017 & 316 & 1 & 317 & 0 & 317\\
O & 2018 & 180 & 1 & 181 & 0 & 169\\
O & 2019 & 277 & 2 & 279 & 0 & 279\\
O & 2020 & 116 & 0 & 116 & 0 & 114\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 51 & 51 & 0 & 0\\
W & 1997 & 0 & 23 & 23 & 0 & 0\\
W & 1998 & 25 & 0 & 25 & 0 & 0\\
W & 1999 & 7 & 0 & 7 & 0 & 0\\
W & 2000 & 3 & 17 & 20 & 0 & 0\\
W & 2001 & 0 & 3 & 3 & 0 & 0\\
W & 2002 & 233 & 4 & 237 & 0 & 0\\
W & 2003 & 132 & 3 & 135 & 0 & 0\\
W & 2004 & 7 & 0 & 7 & 0 & 0\\
W & 2005 & 9 & 0 & 9 & 0 & 0\\
W & 2006 & 8 & 0 & 8 & 0 & 0\\
W & 2008 & 3 & 0 & 3 & 0 & 0\\
W & 2009 & 3 & 1 & 4 & 0 & 0\\
W & 2010 & 2 & 0 & 2 & 0 & 0\\
W & 2011 & 50 & 1 & 51 & 0 & 0\\
W & 2012 & 50 & 1 & 51 & 0 & 0\\
W & 2013 & 56 & 0 & 56 & 0 & 0\\
W & 2014 & 65 & 5 & 70 & 0 & 0\\
W & 2015 & 59 & 0 & 59 & 0 & 0\\
W & 2016 & 60 & 2 & 62 & 0 & 0\\
W & 2017 & 178 & 1 & 179 & 0 & 0\\
W & 2018 & 160 & 0 & 160 & 0 & 0\\
W & 2019 & 142 & 0 & 142 & 0 & 0\\
W & 2020 & 25 & 1 & 26 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 4 & 4 & 0 & 0\\
C & 2005 & 0 & 2 & 2 & 0 & 0\\
C & 2008 & 0 & 1 & 1 & 0 & 0\\
C & 2010 & 0 & 11 & 11 & 0 & 0\\
C & 2013 & 0 & 1 & 1 & 0 & 0\\
C & 2019 & 0 & 1 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 63 & 63 & 0 & 0\\
O & 2002 & 0 & 121 & 121 & 0 & 0\\
O & 2003 & 0 & 58 & 58 & 0 & 0\\
O & 2004 & 0 & 16 & 16 & 0 & 0\\
O & 2005 & 0 & 24 & 24 & 0 & 0\\
O & 2006 & 0 & 12 & 12 & 0 & 0\\
O & 2007 & 0 & 20 & 20 & 0 & 0\\
O & 2008 & 0 & 6 & 6 & 0 & 0\\
O & 2009 & 0 & 17 & 17 & 0 & 0\\
O & 2011 & 0 & 6 & 6 & 0 & 0\\
O & 2012 & 0 & 15 & 15 & 0 & 0\\
O & 2013 & 0 & 38 & 38 & 0 & 0\\
O & 2014 & 0 & 15 & 15 & 0 & 0\\
O & 2015 & 0 & 16 & 16 & 0 & 0\\
O & 2016 & 0 & 24 & 24 & 0 & 0\\
O & 2017 & 0 & 12 & 12 & 0 & 0\\
O & 2018 & 0 & 11 & 11 & 0 & 0\\
O & 2019 & 0 & 45 & 45 & 0 & 0\\
O & 2020 & 0 & 2 & 2 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2005 & 1 & 0 & 1 & 0 & 0\\
W & 2017 & 3 & 0 & 3 & 0 & 3\\
W & 2018 & 1 & 0 & 1 & 0 & 1\\
W & 2019 & 0 & 1 & 1 & 0 & 0\\
W & 2021 & 1 & 0 & 1 & 0 & 1\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 3 & 33 & 0 & 33 & 0 & 25\\
2012 & 1 & 1 & 0 & 1 & 0 & 1\\
2019 & 2 & 2 & 0 & 2 & 0 & 1\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Rosy rockfish 
 The most recent assessment of Rosy rockfish was a data-limited assessment conducted in 2010. Across available data, Rosy rockfish have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC HKL survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 2,414 length observations, 0 age readings, and 213 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 329 length observations, 0 age readings, and 12 otoliths that are available to be aged. In California, since 2000, a total of 290 length observations, 0 age readings, and 2 otoliths have been collected In Oregon, since 2000, a total of 39 length observations, 0 age readings, and 10 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 19,061 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 18,924 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 18,843 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 81 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC HKL survey has collected a total of 763 length observations, 0 age readings, and 699 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 0 & 7 & 7 & 0 & 104\\
C & 1981 & 0 & 4 & 4 & 0 & 19\\
C & 1982 & 0 & 1 & 1 & 0 & 0\\
C & 1983 & 0 & 1 & 1 & 0 & 0\\
C & 1984 & 0 & 6 & 6 & 0 & 14\\
C & 1985 & 0 & 12 & 12 & 0 & 25\\
C & 1986 & 4 & 1 & 5 & 0 & 3\\
C & 1987 & 5 & 0 & 5 & 0 & 0\\
C & 1988 & 6 & 31 & 37 & 0 & 0\\
C & 1989 & 9 & 6 & 15 & 0 & 0\\
C & 1990 & 3 & 13 & 16 & 0 & 0\\
C & 1991 & 1 & 174 & 175 & 0 & 0\\
C & 1992 & 2 & 652 & 654 & 0 & 0\\
C & 1993 & 1 & 138 & 139 & 0 & 0\\
C & 1994 & 0 & 459 & 459 & 0 & 0\\
C & 1995 & 1 & 72 & 73 & 0 & 0\\
C & 1996 & 5 & 231 & 236 & 0 & 0\\
C & 1997 & 6 & 115 & 121 & 0 & 0\\
C & 1998 & 1 & 20 & 21 & 0 & 0\\
C & 1999 & 1 & 31 & 31 & 0 & 0\\
C & 2000 & 0 & 1 & 1 & 0 & 0\\
C & 2002 & 0 & 1 & 1 & 0 & 0\\
C & 2008 & 0 & 25 & 25 & 0 & 0\\
C & 2009 & 0 & 36 & 36 & 0 & 0\\
C & 2010 & 0 & 24 & 24 & 0 & 0\\
C & 2011 & 0 & 3 & 3 & 0 & 0\\
C & 2012 & 0 & 14 & 14 & 0 & 0\\
C & 2013 & 0 & 17 & 17 & 0 & 0\\
C & 2014 & 0 & 41 & 41 & 0 & 0\\
C & 2015 & 0 & 4 & 4 & 0 & 0\\
C & 2016 & 1 & 12 & 13 & 0 & 2\\
C & 2017 & 0 & 12 & 12 & 0 & 0\\
C & 2018 & 0 & 27 & 27 & 0 & 0\\
C & 2019 & 2 & 12 & 14 & 0 & 0\\
C & 2020 & 0 & 58 & 58 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1992 & 36 & 0 & 36 & 0 & 36\\
O & 1993 & 31 & 0 & 31 & 0 & 0\\
O & 2001 & 29 & 0 & 29 & 0 & 0\\
O & 2003 & 2 & 0 & 2 & 0 & 2\\
O & 2011 & 1 & 0 & 1 & 0 & 1\\
O & 2014 & 1 & 0 & 1 & 0 & 1\\
O & 2017 & 1 & 0 & 1 & 0 & 1\\
O & 2018 & 2 & 0 & 2 & 0 & 2\\
O & 2019 & 3 & 0 & 3 & 0 & 3\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 1 & 16 & 17 & 0 & 0\\
C & 2004 & 1 & 604 & 605 & 0 & 0\\
C & 2005 & 1 & 663 & 664 & 0 & 0\\
C & 2006 & 0 & 1039 & 1039 & 0 & 0\\
C & 2007 & 0 & 1295 & 1295 & 0 & 0\\
C & 2008 & 0 & 1166 & 1166 & 0 & 0\\
C & 2009 & 0 & 1658 & 1657 & 0 & 0\\
C & 2010 & 0 & 1469 & 1469 & 0 & 0\\
C & 2011 & 0 & 1295 & 1294 & 0 & 0\\
C & 2012 & 0 & 1041 & 1041 & 0 & 0\\
C & 2013 & 1 & 1185 & 1186 & 0 & 0\\
C & 2014 & 0 & 655 & 655 & 0 & 0\\
C & 2015 & 0 & 1009 & 1009 & 0 & 0\\
C & 2016 & 1 & 1044 & 1045 & 0 & 0\\
C & 2017 & 0 & 1329 & 1329 & 0 & 0\\
C & 2018 & 0 & 1696 & 1696 & 0 & 0\\
C & 2019 & 0 & 1633 & 1633 & 0 & 0\\
C & 2020 & 0 & 43 & 43 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 96 & 96 & 0 & 0\\
O & 2002 & 0 & 41 & 41 & 0 & 0\\
O & 2003 & 0 & 23 & 23 & 0 & 0\\
O & 2004 & 0 & 3 & 3 & 0 & 0\\
O & 2005 & 0 & 7 & 7 & 0 & 0\\
O & 2006 & 0 & 6 & 6 & 0 & 0\\
O & 2007 & 0 & 3 & 3 & 0 & 0\\
O & 2009 & 0 & 2 & 2 & 0 & 0\\
O & 2011 & 0 & 4 & 4 & 0 & 0\\
O & 2012 & 0 & 9 & 9 & 0 & 0\\
O & 2013 & 0 & 4 & 4 & 0 & 0\\
O & 2015 & 0 & 2 & 2 & 0 & 0\\
O & 2017 & 0 & 6 & 6 & 0 & 0\\
O & 2018 & 0 & 4 & 4 & 0 & 0\\
O & 2019 & 0 & 8 & 8 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 9 & 21 & 0 & 21 & 0 & 21\\
2005 & 7 & 11 & 0 & 11 & 0 & 11\\
2006 & 8 & 13 & 0 & 13 & 0 & 13\\
2007 & 14 & 48 & 5 & 53 & 0 & 45\\
2008 & 24 & 50 & 1 & 51 & 0 & 50\\
2009 & 27 & 62 & 0 & 62 & 0 & 44\\
2010 & 12 & 31 & 0 & 31 & 0 & 31\\
2011 & 11 & 28 & 0 & 28 & 0 & 26\\
2012 & 9 & 15 & 0 & 15 & 0 & 10\\
2013 & 8 & 21 & 0 & 21 & 0 & 18\\
2014 & 23 & 53 & 8 & 61 & 0 & 58\\
2015 & 28 & 68 & 0 & 68 & 0 & 66\\
2016 & 27 & 44 & 0 & 44 & 0 & 43\\
2017 & 29 & 77 & 1 & 77 & 0 & 72\\
2018 & 36 & 104 & 0 & 104 & 0 & 94\\
2019 & 36 & 103 & 0 & 103 & 0 & 97\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Rougheye/Blackspotted rockfish 
 The most recent assessment of Rougheye/Blackspotted rockfish was a full assessment conducted in 2013. Across available data, Rougheye/Blackspotted rockfish have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 49,275 length observations, 2,487 age readings, and 15,062 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 45,399 length observations, 2,487 age readings, and 15,039 otoliths that are available to be aged. In California, since 2000, a total of 1,020 length observations, 0 age readings, and 386 otoliths have been collected In Oregon, since 2000, a total of 16,225 length observations, 724 age readings, and 14,653 otoliths have been collected In Washington, since 2000, a total of 28,154 length observations, 1,763 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 1,958 length observations, 962 age readings, and 675 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1981 & 0 & 1 & 1 & 0 & 0\\
C & 1985 & 1 & 0 & 1 & 0 & 1\\
C & 1986 & 1 & 0 & 1 & 0 & 0\\
C & 1987 & 6 & 0 & 6 & 0 & 1\\
C & 1990 & 1 & 0 & 1 & 0 & 0\\
C & 1991 & 4 & 0 & 4 & 0 & 0\\
C & 1992 & 2 & 15 & 17 & 0 & 0\\
C & 1994 & 15 & 0 & 15 & 0 & 0\\
C & 1995 & 4 & 5 & 9 & 0 & 0\\
C & 1996 & 15 & 7 & 22 & 0 & 0\\
C & 1997 & 1 & 2 & 3 & 0 & 0\\
C & 1998 & 0 & 11 & 11 & 0 & 0\\
C & 1999 & 3 & 0 & 3 & 0 & 0\\
C & 2000 & 20 & 0 & 20 & 0 & 0\\
C & 2001 & 1 & 6 & 7 & 0 & 1\\
C & 2002 & 16 & 4 & 20 & 0 & 16\\
C & 2003 & 13 & 2 & 15 & 0 & 11\\
C & 2004 & 1 & 0 & 1 & 0 & 1\\
C & 2005 & 58 & 0 & 58 & 0 & 56\\
C & 2006 & 118 & 0 & 118 & 0 & 5\\
C & 2007 & 26 & 0 & 26 & 0 & 9\\
C & 2008 & 79 & 0 & 79 & 0 & 26\\
C & 2009 & 108 & 3 & 111 & 0 & 68\\
C & 2010 & 105 & 17 & 121 & 0 & 28\\
C & 2011 & 58 & 6 & 64 & 0 & 42\\
C & 2012 & 33 & 6 & 35 & 0 & 24\\
C & 2013 & 34 & 4 & 36 & 0 & 29\\
C & 2014 & 10 & 14 & 24 & 0 & 7\\
C & 2015 & 16 & 37 & 49 & 0 & 16\\
C & 2016 & 30 & 84 & 114 & 0 & 27\\
C & 2017 & 34 & 7 & 38 & 0 & 20\\
C & 2018 & 12 & 6 & 18 & 0 & 0\\
C & 2019 & 57 & 4 & 61 & 0 & 0\\
C & 2020 & 4 & 1 & 5 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1995 & 43 & 0 & 43 & 0 & 21\\
O & 1996 & 167 & 0 & 167 & 0 & 0\\
O & 1997 & 24 & 31 & 55 & 0 & 0\\
O & 1998 & 44 & 0 & 44 & 0 & 0\\
O & 1999 & 102 & 0 & 102 & 0 & 0\\
O & 2000 & 63 & 0 & 63 & 0 & 63\\
O & 2001 & 135 & 0 & 135 & 0 & 80\\
O & 2002 & 5 & 0 & 5 & 0 & 5\\
O & 2003 & 48 & 0 & 48 & 0 & 46\\
O & 2004 & 318 & 0 & 318 & 0 & 299\\
O & 2005 & 280 & 0 & 280 & 0 & 238\\
O & 2006 & 431 & 0 & 431 & 0 & 408\\
O & 2007 & 903 & 0 & 903 & 0 & 764\\
O & 2008 & 793 & 1 & 794 & 329 & 399\\
O & 2009 & 1148 & 0 & 1148 & 0 & 1086\\
O & 2010 & 1322 & 1 & 1323 & 0 & 1209\\
O & 2011 & 951 & 0 & 951 & 395 & 550\\
O & 2012 & 1188 & 0 & 1188 & 0 & 1187\\
O & 2013 & 1040 & 1 & 1041 & 0 & 1040\\
O & 2014 & 705 & 0 & 705 & 0 & 703\\
O & 2015 & 1444 & 0 & 1444 & 0 & 1323\\
O & 2016 & 1227 & 0 & 1227 & 0 & 1188\\
O & 2017 & 1167 & 0 & 1167 & 0 & 1102\\
O & 2018 & 1171 & 3 & 1174 & 0 & 1134\\
O & 2019 & 1133 & 1 & 1133 & 0 & 1087\\
O & 2020 & 747 & 0 & 747 & 0 & 742\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 163 & 163 & 0 & 0\\
W & 1997 & 0 & 828 & 828 & 0 & 0\\
W & 1998 & 1187 & 82 & 1269 & 0 & 0\\
W & 1999 & 796 & 315 & 1111 & 0 & 0\\
W & 2000 & 573 & 815 & 1388 & 0 & 0\\
W & 2001 & 398 & 463 & 861 & 0 & 0\\
W & 2002 & 646 & 155 & 801 & 0 & 0\\
W & 2003 & 1761 & 91 & 1852 & 0 & 0\\
W & 2004 & 1042 & 73 & 1115 & 0 & 0\\
W & 2005 & 1351 & 6 & 1357 & 0 & 0\\
W & 2006 & 1620 & 15 & 1635 & 0 & 0\\
W & 2007 & 1463 & 5 & 1468 & 0 & 0\\
W & 2008 & 1626 & 37 & 1663 & 0 & 0\\
W & 2009 & 1563 & 18 & 1581 & 0 & 0\\
W & 2010 & 1096 & 1 & 1097 & 0 & 0\\
W & 2011 & 1423 & 169 & 1592 & 430 & 0\\
W & 2012 & 1517 & 250 & 1767 & 791 & 0\\
W & 2013 & 1393 & 36 & 1428 & 542 & 0\\
W & 2014 & 995 & 10 & 1005 & 0 & 0\\
W & 2015 & 1296 & 41 & 1337 & 0 & 0\\
W & 2016 & 1380 & 68 & 1448 & 0 & 0\\
W & 2017 & 1149 & 1 & 1150 & 0 & 0\\
W & 2018 & 1463 & 1 & 1464 & 0 & 0\\
W & 2019 & 1548 & 6 & 1554 & 0 & 0\\
W & 2020 & 590 & 1 & 591 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 34 & 112 & 0 & 112 & 56 & 0\\
2004 & 27 & 113 & 2 & 115 & 78 & 0\\
2005 & 27 & 259 & 0 & 259 & 139 & 1\\
2006 & 36 & 101 & 1 & 102 & 94 & 6\\
2007 & 37 & 107 & 1 & 108 & 107 & 0\\
2008 & 36 & 120 & 2 & 122 & 121 & 1\\
2009 & 27 & 126 & 0 & 126 & 91 & 5\\
2010 & 29 & 89 & 0 & 89 & 88 & 1\\
2011 & 30 & 115 & 0 & 115 & 109 & 6\\
2012 & 25 & 85 & 2 & 87 & 79 & 6\\
2013 & 21 & 67 & 1 & 68 & 0 & 68\\
2014 & 22 & 40 & 2 & 42 & 0 & 23\\
2015 & 37 & 182 & 3 & 185 & 0 & 174\\
2016 & 32 & 103 & 2 & 105 & 0 & 105\\
2017 & 28 & 165 & 3 & 168 & 0 & 124\\
2018 & 25 & 97 & 0 & 97 & 0 & 97\\
2019 & 10 & 57 & 1 & 58 & 0 & 58\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Sablefish 
 The most recent assessment of Sablefish was a update assessment conducted in 2021. Across available data, Sablefish have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 345,620 length observations, 42,665 age readings, and 85,845 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 224,210 length observations, 24,012 age readings, and 58,874 otoliths that are available to be aged. In California, since 2000, a total of 90,163 length observations, 7,833 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 73,411 length observations, 12,698 age readings, and 57,928 otoliths have been collected In Washington, since 2000, a total of 60,636 length observations, 3,481 age readings, and 946 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 75,104 length observations, 20,775 age readings, and 11,220 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 0 & 2724 & 2724 & 0 & 0\\
C & 1981 & 0 & 1739 & 1739 & 0 & 0\\
C & 1985 & 2 & 0 & 2 & 0 & 0\\
C & 1986 & 2453 & 344 & 2794 & 747 & 0\\
C & 1987 & 8101 & 450 & 8551 & 5530 & 0\\
C & 1988 & 4047 & 209 & 4256 & 2294 & 0\\
C & 1989 & 4214 & 616 & 4830 & 1793 & 0\\
C & 1990 & 4094 & 451 & 4544 & 1589 & 0\\
C & 1991 & 2745 & 219 & 2964 & 417 & 0\\
C & 1993 & 2761 & 2591 & 5352 & 349 & 0\\
C & 1994 & 1307 & 3112 & 4419 & 306 & 0\\
C & 1995 & 1257 & 1642 & 2899 & 372 & 0\\
C & 1996 & 1068 & 1175 & 2243 & 738 & 0\\
C & 1997 & 1077 & 777 & 1854 & 982 & 0\\
C & 1998 & 964 & 199 & 1163 & 230 & 0\\
C & 1999 & 1557 & 946 & 2503 & 0 & 0\\
C & 2000 & 1161 & 1144 & 2305 & 741 & 0\\
C & 2001 & 1200 & 1313 & 2513 & 598 & 0\\
C & 2002 & 1555 & 1206 & 2760 & 503 & 0\\
C & 2003 & 1694 & 1615 & 3309 & 249 & 0\\
C & 2004 & 1217 & 739 & 1955 & 580 & 0\\
C & 2005 & 2181 & 1078 & 3226 & 616 & 0\\
C & 2006 & 2609 & 1018 & 3627 & 501 & 0\\
C & 2007 & 1669 & 1433 & 3083 & 1482 & 0\\
C & 2008 & 1563 & 3039 & 4601 & 0 & 0\\
C & 2009 & 1176 & 3344 & 4520 & 520 & 0\\
C & 2010 & 847 & 4086 & 4933 & 521 & 0\\
C & 2011 & 1166 & 3709 & 4875 & 484 & 0\\
C & 2012 & 1171 & 5722 & 6893 & 462 & 0\\
C & 2013 & 1004 & 4883 & 5887 & 113 & 0\\
C & 2014 & 669 & 5861 & 6530 & 0 & 0\\
C & 2015 & 613 & 8194 & 8772 & 0 & 0\\
C & 2016 & 1297 & 7139 & 8398 & 187 & 0\\
C & 2017 & 1113 & 4137 & 5250 & 144 & 0\\
C & 2018 & 484 & 2820 & 3304 & 132 & 0\\
C & 2019 & 591 & 1544 & 2135 & 0 & 0\\
C & 2020 & 230 & 1057 & 1287 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1980 & 383 & 184 & 567 & 0 & 567\\
O & 1981 & 100 & 449 & 549 & 0 & 549\\
O & 1982 & 159 & 0 & 159 & 0 & 159\\
O & 1983 & 50 & 0 & 50 & 0 & 50\\
O & 1985 & 0 & 966 & 966 & 0 & 966\\
O & 1986 & 1087 & 0 & 1087 & 12 & 1075\\
O & 1987 & 2247 & 1 & 2248 & 19 & 2229\\
O & 1988 & 1380 & 0 & 1380 & 168 & 1212\\
O & 1989 & 2138 & 93 & 2231 & 10 & 2128\\
O & 1990 & 2119 & 297 & 2416 & 10 & 2406\\
O & 1991 & 2083 & 700 & 2783 & 0 & 2743\\
O & 1992 & 0 & 179 & 179 & 0 & 0\\
O & 1993 & 1504 & 1974 & 3478 & 371 & 1075\\
O & 1994 & 1948 & 994 & 2942 & 425 & 1275\\
O & 1995 & 1809 & 779 & 2588 & 0 & 1728\\
O & 1996 & 1475 & 134 & 1609 & 0 & 1405\\
O & 1997 & 2731 & 157 & 2838 & 109 & 2552\\
O & 1998 & 2108 & 73 & 2177 & 0 & 2041\\
O & 1999 & 2152 & 31 & 2183 & 75 & 1971\\
O & 2000 & 2472 & 22 & 2494 & 96 & 2325\\
O & 2001 & 2025 & 83 & 2108 & 78 & 2030\\
O & 2002 & 1614 & 6 & 1620 & 0 & 1620\\
O & 2003 & 1827 & 20 & 1847 & 34 & 1752\\
O & 2004 & 1834 & 0 & 1834 & 0 & 1814\\
O & 2005 & 1581 & 1 & 1582 & 707 & 833\\
O & 2006 & 2059 & 1 & 2059 & 92 & 1945\\
O & 2007 & 3048 & 1 & 3049 & 1097 & 1692\\
O & 2008 & 4320 & 0 & 4318 & 120 & 4165\\
O & 2009 & 3737 & 174 & 3911 & 1205 & 2446\\
O & 2010 & 4235 & 19 & 4253 & 1062 & 2682\\
O & 2011 & 4798 & 32 & 4829 & 791 & 3671\\
O & 2012 & 4511 & 6 & 4517 & 398 & 3963\\
O & 2013 & 3977 & 6 & 3982 & 803 & 2995\\
O & 2014 & 3832 & 6 & 3838 & 787 & 3021\\
O & 2015 & 5022 & 6 & 5028 & 1222 & 3666\\
O & 2016 & 4818 & 21 & 4839 & 537 & 4137\\
O & 2017 & 4878 & 17 & 4895 & 1141 & 3664\\
O & 2018 & 4971 & 3 & 4973 & 1354 & 3459\\
O & 2019 & 4937 & 4 & 4941 & 561 & 4167\\
O & 2020 & 2494 & 0 & 2494 & 613 & 1881\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 700 & 500 & 1200 & 0 & 0\\
W & 1981 & 700 & 100 & 800 & 0 & 0\\
W & 1983 & 80 & 2168 & 2248 & 0 & 0\\
W & 1984 & 100 & 0 & 100 & 0 & 0\\
W & 1986 & 1148 & 269 & 1417 & 0 & 275\\
W & 1987 & 680 & 347 & 1025 & 0 & 159\\
W & 1988 & 186 & 347 & 533 & 23 & 67\\
W & 1989 & 286 & 687 & 973 & 0 & 0\\
W & 1990 & 129 & 500 & 629 & 0 & 36\\
W & 1991 & 702 & 1201 & 1903 & 593 & 45\\
W & 1992 & 669 & 1363 & 2032 & 694 & 0\\
W & 1993 & 409 & 2305 & 2712 & 192 & 57\\
W & 1994 & 134 & 2300 & 2434 & 77 & 0\\
W & 1995 & 34 & 3736 & 3770 & 0 & 0\\
W & 1996 & 0 & 3005 & 3005 & 33 & 0\\
W & 1997 & 348 & 3359 & 3706 & 264 & 70\\
W & 1998 & 268 & 3182 & 3450 & 20 & 75\\
W & 1999 & 399 & 3807 & 4206 & 211 & 56\\
W & 2000 & 190 & 3989 & 4179 & 126 & 0\\
W & 2001 & 282 & 2677 & 2959 & 88 & 60\\
W & 2002 & 193 & 2688 & 2881 & 41 & 140\\
W & 2003 & 203 & 2920 & 3123 & 175 & 21\\
W & 2004 & 129 & 2843 & 2959 & 92 & 0\\
W & 2005 & 469 & 2068 & 2537 & 311 & 131\\
W & 2006 & 840 & 1763 & 2603 & 463 & 334\\
W & 2007 & 629 & 1255 & 1884 & 597 & 25\\
W & 2008 & 629 & 1110 & 1739 & 122 & 0\\
W & 2009 & 623 & 1337 & 1960 & 132 & 113\\
W & 2010 & 272 & 1817 & 2089 & 104 & 103\\
W & 2011 & 731 & 2173 & 2904 & 344 & 19\\
W & 2012 & 590 & 1565 & 2155 & 182 & 0\\
W & 2013 & 677 & 1469 & 2146 & 656 & 0\\
W & 2014 & 691 & 1980 & 2671 & 0 & 0\\
W & 2015 & 409 & 2230 & 2639 & 0 & 0\\
W & 2016 & 449 & 2919 & 3367 & 0 & 0\\
W & 2017 & 839 & 3347 & 4186 & 48 & 0\\
W & 2018 & 872 & 3437 & 4309 & 0 & 0\\
W & 2019 & 619 & 4868 & 5487 & 0 & 0\\
W & 2020 & 150 & 1709 & 1859 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 420 & 5779 & 20 & 5799 & 1389 & 992\\
2004 & 329 & 4536 & 4 & 4540 & 1086 & 1053\\
2005 & 445 & 5543 & 24 & 5567 & 1575 & 1295\\
2006 & 398 & 4831 & 2 & 4833 & 1363 & 1306\\
2007 & 422 & 4461 & 10 & 4470 & 1259 & 857\\
2008 & 420 & 3430 & 543 & 3973 & 1190 & 995\\
2009 & 419 & 3659 & 29 & 3688 & 1181 & 669\\
2010 & 458 & 3828 & 404 & 4232 & 1271 & 876\\
2011 & 455 & 4624 & 50 & 4674 & 1193 & 600\\
2012 & 428 & 4367 & 15 & 4381 & 1091 & 606\\
2013 & 307 & 3146 & 134 & 3280 & 992 & 212\\
2014 & 461 & 4337 & 7 & 4319 & 1200 & 600\\
2015 & 420 & 4923 & 21 & 4910 & 1197 & 528\\
2016 & 438 & 4425 & 119 & 4544 & 1213 & 256\\
2017 & 459 & 4866 & 17 & 4883 & 1219 & 354\\
2018 & 435 & 4780 & 5 & 4785 & 1482 & 11\\
2019 & 226 & 2223 & 3 & 2226 & 874 & 10\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Sand sole 
 The most recent assessment of Sand sole was a data-limited assessment conducted in 2010. Across available data, Sand sole have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 8,031 length observations, 0 age readings, and 3,340 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 7,916 length observations, 0 age readings, and 3,340 otoliths that are available to be aged. In California, since 2000, a total of 4,396 length observations, 0 age readings, and 50 otoliths have been collected In Oregon, since 2000, a total of 3,520 length observations, 0 age readings, and 3,290 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 1,459 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 1,421 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 596 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 823 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 2 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 909 length observations, 0 age readings, and 484 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2001 & 36 & 0 & 36 & 0 & 0\\
C & 2002 & 16 & 0 & 16 & 0 & 0\\
C & 2003 & 86 & 16 & 102 & 0 & 0\\
C & 2004 & 0 & 16 & 1 & 0 & 0\\
C & 2005 & 31 & 57 & 65 & 0 & 5\\
C & 2006 & 31 & 9 & 40 & 0 & 0\\
C & 2007 & 58 & 0 & 58 & 0 & 1\\
C & 2008 & 89 & 2 & 91 & 0 & 44\\
C & 2009 & 111 & 85 & 196 & 0 & 0\\
C & 2010 & 41 & 71 & 112 & 0 & 0\\
C & 2011 & 58 & 223 & 281 & 0 & 0\\
C & 2012 & 23 & 387 & 410 & 0 & 0\\
C & 2013 & 125 & 373 & 498 & 0 & 0\\
C & 2014 & 36 & 213 & 249 & 0 & 0\\
C & 2015 & 175 & 63 & 238 & 0 & 0\\
C & 2016 & 356 & 118 & 363 & 0 & 0\\
C & 2017 & 276 & 257 & 533 & 0 & 0\\
C & 2018 & 375 & 0 & 375 & 0 & 0\\
C & 2019 & 167 & 274 & 439 & 0 & 0\\
C & 2020 & 272 & 21 & 293 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 42 & 42 & 0 & 0\\
O & 2003 & 30 & 0 & 30 & 0 & 30\\
O & 2004 & 21 & 0 & 21 & 0 & 21\\
O & 2005 & 29 & 0 & 29 & 0 & 29\\
O & 2006 & 0 & 2 & 2 & 0 & 0\\
O & 2007 & 360 & 1 & 361 & 0 & 360\\
O & 2008 & 360 & 0 & 360 & 0 & 360\\
O & 2009 & 440 & 0 & 440 & 0 & 440\\
O & 2010 & 661 & 0 & 661 & 0 & 601\\
O & 2011 & 273 & 0 & 273 & 0 & 273\\
O & 2012 & 150 & 0 & 150 & 0 & 150\\
O & 2013 & 135 & 0 & 135 & 0 & 135\\
O & 2014 & 176 & 0 & 176 & 0 & 146\\
O & 2015 & 345 & 0 & 345 & 0 & 315\\
O & 2016 & 210 & 0 & 210 & 0 & 180\\
O & 2017 & 152 & 0 & 152 & 0 & 152\\
O & 2018 & 117 & 0 & 117 & 0 & 82\\
O & 2019 & 16 & 0 & 16 & 0 & 16\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1984 & 115 & 0 & 115 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 22 & 22 & 0 & 0\\
C & 2005 & 0 & 19 & 19 & 0 & 0\\
C & 2006 & 0 & 11 & 11 & 0 & 0\\
C & 2007 & 0 & 12 & 12 & 0 & 0\\
C & 2008 & 0 & 8 & 8 & 0 & 0\\
C & 2009 & 0 & 32 & 32 & 0 & 0\\
C & 2010 & 0 & 48 & 48 & 0 & 0\\
C & 2011 & 0 & 105 & 105 & 0 & 0\\
C & 2012 & 0 & 93 & 93 & 0 & 0\\
C & 2013 & 6 & 84 & 90 & 0 & 0\\
C & 2014 & 1 & 64 & 65 & 0 & 0\\
C & 2015 & 0 & 24 & 24 & 0 & 0\\
C & 2016 & 0 & 13 & 13 & 0 & 0\\
C & 2017 & 0 & 12 & 12 & 0 & 0\\
C & 2018 & 0 & 23 & 23 & 0 & 0\\
C & 2019 & 0 & 18 & 18 & 0 & 0\\
C & 2020 & 0 & 1 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 7 & 7 & 0 & 0\\
O & 2002 & 0 & 31 & 31 & 0 & 0\\
O & 2003 & 0 & 20 & 20 & 0 & 0\\
O & 2004 & 0 & 42 & 42 & 0 & 0\\
O & 2005 & 0 & 38 & 38 & 0 & 0\\
O & 2006 & 0 & 15 & 15 & 0 & 0\\
O & 2007 & 0 & 11 & 11 & 0 & 0\\
O & 2008 & 0 & 20 & 20 & 0 & 0\\
O & 2009 & 0 & 61 & 61 & 0 & 0\\
O & 2010 & 0 & 109 & 109 & 0 & 0\\
O & 2011 & 0 & 67 & 67 & 0 & 0\\
O & 2012 & 0 & 37 & 37 & 0 & 0\\
O & 2013 & 0 & 50 & 50 & 0 & 0\\
O & 2014 & 0 & 51 & 51 & 0 & 0\\
O & 2015 & 0 & 61 & 61 & 0 & 0\\
O & 2016 & 0 & 81 & 81 & 0 & 0\\
O & 2017 & 0 & 89 & 89 & 0 & 0\\
O & 2018 & 0 & 41 & 41 & 0 & 0\\
O & 2019 & 0 & 30 & 30 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2009 & 0 & 1 & 1 & 0 & 0\\
W & 2019 & 0 & 1 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 6 & 79 & 0 & 79 & 0 & 0\\
2004 & 5 & 29 & 0 & 29 & 0 & 0\\
2005 & 6 & 31 & 0 & 31 & 0 & 0\\
2006 & 3 & 11 & 1 & 12 & 0 & 0\\
2007 & 7 & 37 & 0 & 37 & 0 & 0\\
2008 & 12 & 62 & 0 & 62 & 0 & 62\\
2009 & 9 & 48 & 0 & 48 & 0 & 38\\
2010 & 10 & 93 & 0 & 93 & 0 & 93\\
2011 & 18 & 123 & 0 & 123 & 0 & 122\\
2012 & 9 & 37 & 0 & 37 & 0 & 34\\
2013 & 13 & 47 & 0 & 47 & 0 & 43\\
2014 & 9 & 50 & 0 & 50 & 0 & 50\\
2015 & 9 & 74 & 0 & 74 & 0 & 0\\
2016 & 10 & 26 & 0 & 26 & 0 & 0\\
2017 & 16 & 83 & 0 & 83 & 0 & 0\\
2018 & 7 & 36 & 0 & 36 & 0 & 0\\
2019 & 6 & 42 & 0 & 42 & 0 & 42\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Sharpchin rockfish 
 The most recent assessment of Sharpchin rockfish was a data-moderate assessment conducted in NA. Across available data, Sharpchin rockfish have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 11,098 length observations, 0 age readings, and 2,285 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 3,994 length observations, 0 age readings, and 1,792 otoliths that are available to be aged. In California, since 2000, a total of 180 length observations, 0 age readings, and 48 otoliths have been collected In Oregon, since 2000, a total of 1,792 length observations, 0 age readings, and 1,744 otoliths have been collected In Washington, since 2000, a total of 2,022 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 17,653 length observations, 0 age readings, and 7,309 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1982 & 13 & 0 & 13 & 0 & 13\\
C & 1983 & 144 & 0 & 144 & 0 & 135\\
C & 1984 & 90 & 0 & 90 & 0 & 90\\
C & 1985 & 213 & 0 & 213 & 0 & 224\\
C & 1986 & 134 & 0 & 134 & 0 & 26\\
C & 1987 & 291 & 0 & 291 & 0 & 0\\
C & 1988 & 356 & 0 & 356 & 0 & 0\\
C & 1989 & 101 & 0 & 101 & 0 & 0\\
C & 1990 & 280 & 0 & 280 & 0 & 0\\
C & 1991 & 188 & 4 & 192 & 0 & 1\\
C & 1992 & 130 & 17 & 147 & 0 & 0\\
C & 1993 & 111 & 14 & 125 & 0 & 0\\
C & 1994 & 520 & 42 & 562 & 0 & 0\\
C & 1995 & 368 & 13 & 381 & 0 & 0\\
C & 1996 & 425 & 33 & 458 & 0 & 0\\
C & 1997 & 357 & 20 & 377 & 0 & 0\\
C & 1998 & 212 & 3 & 215 & 0 & 0\\
C & 1999 & 159 & 3 & 162 & 0 & 0\\
C & 2000 & 35 & 0 & 35 & 0 & 0\\
C & 2001 & 41 & 0 & 41 & 0 & 1\\
C & 2002 & 45 & 0 & 45 & 0 & 2\\
C & 2005 & 26 & 1 & 27 & 0 & 27\\
C & 2007 & 4 & 0 & 4 & 0 & 2\\
C & 2013 & 4 & 0 & 4 & 0 & 4\\
C & 2014 & 11 & 1 & 11 & 0 & 10\\
C & 2015 & 2 & 2 & 3 & 0 & 2\\
C & 2016 & 1 & 6 & 7 & 0 & 0\\
C & 2017 & 0 & 2 & 2 & 0 & 0\\
C & 2018 & 1 & 0 & 1 & 0 & 0\\
C & 2020 & 0 & 19 & 0 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1995 & 26 & 0 & 26 & 0 & 4\\
O & 1996 & 283 & 0 & 283 & 0 & 0\\
O & 1997 & 526 & 0 & 526 & 0 & 0\\
O & 1999 & 127 & 0 & 127 & 0 & 0\\
O & 2001 & 19 & 0 & 19 & 0 & 0\\
O & 2003 & 15 & 0 & 15 & 0 & 15\\
O & 2004 & 146 & 0 & 146 & 0 & 146\\
O & 2005 & 46 & 0 & 46 & 0 & 46\\
O & 2007 & 22 & 0 & 22 & 0 & 22\\
O & 2008 & 30 & 0 & 30 & 0 & 30\\
O & 2009 & 30 & 0 & 30 & 0 & 30\\
O & 2010 & 46 & 0 & 46 & 0 & 46\\
O & 2011 & 32 & 0 & 32 & 0 & 32\\
O & 2012 & 4 & 0 & 4 & 0 & 4\\
O & 2013 & 113 & 0 & 113 & 0 & 113\\
O & 2014 & 193 & 1 & 194 & 0 & 194\\
O & 2015 & 196 & 0 & 196 & 0 & 166\\
O & 2016 & 37 & 0 & 37 & 0 & 37\\
O & 2017 & 203 & 1 & 202 & 0 & 203\\
O & 2018 & 163 & 1 & 163 & 0 & 163\\
O & 2019 & 349 & 0 & 349 & 0 & 349\\
O & 2020 & 148 & 0 & 148 & 0 & 148\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 612 & 612 & 0 & 0\\
W & 1997 & 0 & 685 & 685 & 0 & 0\\
W & 1998 & 453 & 22 & 475 & 0 & 0\\
W & 1999 & 117 & 12 & 129 & 0 & 0\\
W & 2000 & 77 & 20 & 97 & 0 & 0\\
W & 2001 & 165 & 4 & 169 & 0 & 0\\
W & 2002 & 646 & 5 & 651 & 0 & 0\\
W & 2003 & 334 & 1 & 335 & 0 & 0\\
W & 2004 & 6 & 0 & 6 & 0 & 0\\
W & 2005 & 2 & 0 & 2 & 0 & 0\\
W & 2006 & 9 & 0 & 9 & 0 & 0\\
W & 2007 & 11 & 0 & 11 & 0 & 0\\
W & 2008 & 2 & 0 & 2 & 0 & 0\\
W & 2009 & 2 & 0 & 2 & 0 & 0\\
W & 2010 & 4 & 0 & 4 & 0 & 0\\
W & 2011 & 4 & 29 & 33 & 0 & 0\\
W & 2012 & 344 & 32 & 376 & 0 & 0\\
W & 2013 & 94 & 1 & 95 & 0 & 0\\
W & 2014 & 34 & 2 & 36 & 0 & 0\\
W & 2015 & 8 & 1 & 9 & 0 & 0\\
W & 2016 & 22 & 0 & 22 & 0 & 0\\
W & 2017 & 50 & 0 & 50 & 0 & 0\\
W & 2018 & 25 & 0 & 25 & 0 & 0\\
W & 2019 & 85 & 0 & 85 & 0 & 0\\
W & 2020 & 3 & 0 & 3 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 51 & 2351 & 5 & 2355 & 0 & 553\\
2004 & 33 & 1173 & 3 & 1176 & 0 & 199\\
2005 & 35 & 864 & 7 & 871 & 0 & 153\\
2006 & 43 & 1283 & 0 & 1283 & 0 & 239\\
2007 & 35 & 975 & 0 & 975 & 0 & 204\\
2008 & 26 & 919 & 4 & 923 & 0 & 365\\
2009 & 39 & 934 & 4 & 938 & 0 & 593\\
2010 & 43 & 568 & 6 & 574 & 0 & 381\\
2011 & 48 & 1304 & 3 & 1307 & 0 & 781\\
2012 & 41 & 954 & 43 & 997 & 0 & 478\\
2013 & 38 & 874 & 0 & 874 & 0 & 375\\
2014 & 54 & 1359 & 64 & 1421 & 0 & 606\\
2015 & 53 & 1052 & 41 & 1093 & 0 & 620\\
2016 & 56 & 844 & 10 & 854 & 0 & 501\\
2017 & 52 & 830 & 16 & 846 & 0 & 535\\
2018 & 47 & 720 & 3 & 723 & 0 & 437\\
2019 & 33 & 443 & 0 & 443 & 0 & 289\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Shortraker rockfish 
 The most recent assessment of Shortraker rockfish was a data-limited assessment conducted in 2010. Across available data, Shortraker rockfish have been observed and sampled generally by  
 
 Across all years of available data, commercial fisheries have collected a total of 6,269 length observations, 20 age readings, and 2,895 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 5,638 length observations, 20 age readings, and 2,815 otoliths that are available to be aged. In California, since 2000, a total of 56 length observations, 0 age readings, and 17 otoliths have been collected In Oregon, since 2000, a total of 2,843 length observations, 20 age readings, and 2,798 otoliths have been collected In Washington, since 2000, a total of 2,739 length observations, 0 age readings, and 0 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1983 & 1 & 0 & 1 & 0 & 1\\
C & 1996 & 4 & 0 & 4 & 0 & 0\\
C & 1997 & 1 & 0 & 1 & 0 & 0\\
C & 1999 & 2 & 0 & 2 & 0 & 0\\
C & 2002 & 1 & 0 & 1 & 0 & 0\\
C & 2003 & 2 & 0 & 2 & 0 & 2\\
C & 2007 & 1 & 0 & 1 & 0 & 0\\
C & 2008 & 2 & 0 & 2 & 0 & 2\\
C & 2009 & 1 & 7 & 8 & 0 & 1\\
C & 2010 & 6 & 2 & 8 & 0 & 0\\
C & 2011 & 2 & 0 & 2 & 0 & 0\\
C & 2012 & 3 & 0 & 3 & 0 & 1\\
C & 2013 & 1 & 0 & 1 & 0 & 0\\
C & 2014 & 3 & 0 & 3 & 0 & 0\\
C & 2015 & 4 & 0 & 4 & 0 & 3\\
C & 2016 & 4 & 1 & 5 & 0 & 4\\
C & 2017 & 5 & 0 & 5 & 0 & 4\\
C & 2018 & 2 & 1 & 3 & 0 & 0\\
C & 2019 & 3 & 0 & 3 & 0 & 0\\
C & 2020 & 5 & 0 & 5 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1983 & 16 & 0 & 16 & 0 & 16\\
O & 1987 & 30 & 0 & 30 & 0 & 30\\
O & 1990 & 30 & 0 & 30 & 0 & 30\\
O & 1996 & 5 & 0 & 5 & 0 & 0\\
O & 1999 & 3 & 0 & 3 & 0 & 3\\
O & 2000 & 2 & 0 & 2 & 0 & 2\\
O & 2001 & 10 & 0 & 10 & 1 & 9\\
O & 2002 & 1 & 0 & 1 & 0 & 1\\
O & 2003 & 7 & 0 & 7 & 4 & 3\\
O & 2004 & 10 & 0 & 10 & 0 & 10\\
O & 2005 & 64 & 0 & 64 & 15 & 49\\
O & 2006 & 40 & 0 & 40 & 0 & 38\\
O & 2007 & 95 & 0 & 95 & 0 & 95\\
O & 2008 & 140 & 0 & 140 & 0 & 138\\
O & 2009 & 194 & 0 & 194 & 0 & 194\\
O & 2010 & 140 & 0 & 140 & 0 & 140\\
O & 2011 & 156 & 4 & 160 & 0 & 160\\
O & 2012 & 113 & 1 & 113 & 0 & 114\\
O & 2013 & 258 & 1 & 259 & 0 & 259\\
O & 2014 & 307 & 0 & 306 & 0 & 307\\
O & 2015 & 309 & 0 & 309 & 0 & 309\\
O & 2016 & 138 & 0 & 138 & 0 & 136\\
O & 2017 & 225 & 0 & 225 & 0 & 205\\
O & 2018 & 290 & 0 & 289 & 0 & 288\\
O & 2019 & 190 & 0 & 190 & 0 & 190\\
O & 2020 & 151 & 0 & 151 & 0 & 151\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 91 & 91 & 0 & 0\\
W & 1997 & 0 & 88 & 88 & 0 & 0\\
W & 1998 & 157 & 3 & 160 & 0 & 0\\
W & 1999 & 191 & 9 & 200 & 0 & 0\\
W & 2000 & 127 & 90 & 217 & 0 & 0\\
W & 2001 & 74 & 29 & 103 & 0 & 0\\
W & 2002 & 61 & 37 & 98 & 0 & 0\\
W & 2003 & 202 & 35 & 237 & 0 & 0\\
W & 2004 & 49 & 4 & 53 & 0 & 0\\
W & 2005 & 60 & 0 & 60 & 0 & 0\\
W & 2006 & 50 & 2 & 51 & 0 & 0\\
W & 2007 & 99 & 0 & 99 & 0 & 0\\
W & 2008 & 151 & 0 & 151 & 0 & 0\\
W & 2009 & 129 & 2 & 131 & 0 & 0\\
W & 2010 & 124 & 0 & 124 & 0 & 0\\
W & 2011 & 207 & 1 & 208 & 0 & 0\\
W & 2012 & 128 & 0 & 128 & 0 & 0\\
W & 2013 & 60 & 0 & 60 & 0 & 0\\
W & 2014 & 87 & 3 & 90 & 0 & 0\\
W & 2015 & 163 & 4 & 167 & 0 & 0\\
W & 2016 & 125 & 0 & 125 & 0 & 0\\
W & 2017 & 141 & 0 & 141 & 0 & 0\\
W & 2018 & 271 & 3 & 274 & 0 & 0\\
W & 2019 & 187 & 0 & 187 & 0 & 0\\
W & 2020 & 34 & 1 & 35 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Shortspine thornyhead 
 The most recent assessment of Shortspine thornyhead was a full assessment conducted in 2013. Across available data, Shortspine thornyhead have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 145,968 length observations, 0 age readings, and 29,699 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 112,117 length observations, 0 age readings, and 25,310 otoliths that are available to be aged. In California, since 2000, a total of 61,653 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 32,838 length observations, 0 age readings, and 25,310 otoliths have been collected In Washington, since 2000, a total of 17,626 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 85,165 length observations, 0 age readings, and 19,568 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 1059 & 76 & 1135 & 0 & 0\\
C & 1981 & 736 & 4 & 740 & 0 & 0\\
C & 1982 & 717 & 8 & 723 & 0 & 0\\
C & 1983 & 1227 & 3 & 1230 & 0 & 0\\
C & 1984 & 2743 & 15 & 2755 & 0 & 0\\
C & 1985 & 3135 & 45 & 3179 & 0 & 0\\
C & 1986 & 947 & 40 & 987 & 0 & 0\\
C & 1987 & 390 & 7 & 397 & 0 & 0\\
C & 1988 & 68 & 80 & 148 & 0 & 0\\
C & 1989 & 693 & 66 & 759 & 0 & 0\\
C & 1990 & 468 & 73 & 541 & 0 & 0\\
C & 1991 & 480 & 52 & 532 & 0 & 0\\
C & 1992 & 428 & 95 & 523 & 0 & 0\\
C & 1993 & 400 & 596 & 996 & 0 & 0\\
C & 1994 & 356 & 1057 & 1413 & 0 & 0\\
C & 1995 & 788 & 1496 & 2284 & 0 & 0\\
C & 1996 & 416 & 1688 & 2104 & 0 & 0\\
C & 1997 & 530 & 1226 & 1756 & 0 & 0\\
C & 1998 & 168 & 1092 & 1260 & 0 & 0\\
C & 1999 & 195 & 2478 & 2673 & 0 & 0\\
C & 2000 & 505 & 966 & 1471 & 0 & 0\\
C & 2001 & 724 & 800 & 1515 & 0 & 0\\
C & 2002 & 1582 & 2026 & 3346 & 0 & 0\\
C & 2003 & 1137 & 2275 & 2743 & 0 & 0\\
C & 2004 & 352 & 1368 & 1205 & 0 & 0\\
C & 2005 & 931 & 1792 & 2013 & 0 & 0\\
C & 2006 & 2651 & 1135 & 3703 & 0 & 0\\
C & 2007 & 1317 & 977 & 2171 & 0 & 0\\
C & 2008 & 1496 & 2502 & 3998 & 0 & 0\\
C & 2009 & 838 & 2467 & 3305 & 0 & 0\\
C & 2010 & 360 & 2497 & 2856 & 0 & 0\\
C & 2011 & 906 & 4816 & 5712 & 0 & 0\\
C & 2012 & 423 & 3881 & 4304 & 0 & 0\\
C & 2013 & 890 & 3198 & 4082 & 0 & 0\\
C & 2014 & 217 & 3870 & 4087 & 0 & 0\\
C & 2015 & 83 & 3996 & 4075 & 0 & 0\\
C & 2016 & 549 & 3740 & 4286 & 0 & 0\\
C & 2017 & 370 & 2696 & 3066 & 0 & 0\\
C & 2018 & 416 & 1266 & 1682 & 0 & 0\\
C & 2019 & 557 & 420 & 977 & 0 & 0\\
C & 2020 & 196 & 860 & 1056 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1981 & 30 & 0 & 30 & 0 & 30\\
O & 1982 & 150 & 0 & 150 & 0 & 150\\
O & 1990 & 510 & 0 & 510 & 0 & 510\\
O & 1991 & 1059 & 0 & 1059 & 0 & 1059\\
O & 1992 & 1227 & 0 & 1227 & 0 & 898\\
O & 1993 & 281 & 0 & 281 & 0 & 233\\
O & 1994 & 40 & 0 & 40 & 0 & 0\\
O & 1995 & 7 & 17 & 24 & 0 & 0\\
O & 1996 & 375 & 115 & 490 & 0 & 0\\
O & 1997 & 0 & 2321 & 2321 & 0 & 0\\
O & 1998 & 738 & 15 & 753 & 0 & 690\\
O & 1999 & 819 & 0 & 819 & 0 & 819\\
O & 2000 & 659 & 0 & 659 & 0 & 507\\
O & 2001 & 1063 & 0 & 1063 & 0 & 883\\
O & 2002 & 1000 & 0 & 1000 & 0 & 760\\
O & 2003 & 1451 & 1 & 1452 & 0 & 973\\
O & 2004 & 1256 & 0 & 1256 & 0 & 835\\
O & 2005 & 1229 & 41 & 1270 & 0 & 940\\
O & 2006 & 1466 & 9 & 1475 & 0 & 1146\\
O & 2007 & 1876 & 14 & 1890 & 0 & 1410\\
O & 2008 & 2409 & 28 & 2437 & 0 & 1867\\
O & 2009 & 2031 & 13 & 2044 & 0 & 1624\\
O & 2010 & 2416 & 32 & 2448 & 0 & 1848\\
O & 2011 & 1758 & 8 & 1766 & 0 & 1555\\
O & 2012 & 1797 & 6 & 1803 & 0 & 1547\\
O & 2013 & 1899 & 63 & 1962 & 0 & 1537\\
O & 2014 & 1604 & 79 & 1683 & 0 & 1285\\
O & 2015 & 1569 & 46 & 1615 & 0 & 1328\\
O & 2016 & 1508 & 23 & 1531 & 0 & 1095\\
O & 2017 & 1598 & 53 & 1651 & 0 & 1377\\
O & 2018 & 1496 & 26 & 1522 & 0 & 1167\\
O & 2019 & 1346 & 19 & 1364 & 0 & 1010\\
O & 2020 & 933 & 14 & 947 & 0 & 616\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 7 & 7 & 0 & 0\\
W & 1997 & 0 & 1 & 1 & 0 & 0\\
W & 1998 & 2 & 2 & 4 & 0 & 0\\
W & 2000 & 1 & 3 & 4 & 0 & 0\\
W & 2001 & 409 & 190 & 599 & 0 & 0\\
W & 2002 & 1266 & 57 & 1323 & 0 & 0\\
W & 2003 & 955 & 24 & 979 & 0 & 0\\
W & 2004 & 193 & 61 & 254 & 0 & 0\\
W & 2005 & 377 & 2 & 379 & 0 & 0\\
W & 2006 & 100 & 0 & 100 & 0 & 0\\
W & 2007 & 696 & 11 & 707 & 0 & 0\\
W & 2008 & 328 & 149 & 475 & 0 & 0\\
W & 2009 & 915 & 25 & 940 & 0 & 0\\
W & 2010 & 875 & 17 & 892 & 0 & 0\\
W & 2011 & 947 & 183 & 1130 & 0 & 0\\
W & 2012 & 765 & 232 & 997 & 0 & 0\\
W & 2013 & 618 & 372 & 990 & 0 & 0\\
W & 2014 & 325 & 714 & 1039 & 0 & 0\\
W & 2015 & 482 & 329 & 811 & 0 & 0\\
W & 2016 & 67 & 1060 & 1127 & 0 & 0\\
W & 2017 & 918 & 598 & 1516 & 0 & 0\\
W & 2018 & 522 & 661 & 1183 & 0 & 0\\
W & 2019 & 416 & 1224 & 1640 & 0 & 0\\
W & 2020 & 29 & 512 & 541 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 293 & 59 & 7634 & 7693 & 0 & 1255\\
2004 & 214 & 2187 & 4507 & 6694 & 0 & 1671\\
2005 & 315 & 6736 & 1311 & 8047 & 0 & 1374\\
2006 & 332 & 5280 & 918 & 6198 & 0 & 1244\\
2007 & 367 & 4779 & 720 & 5499 & 0 & 1294\\
2008 & 362 & 4040 & 657 & 4697 & 0 & 1301\\
2009 & 340 & 3554 & 641 & 4195 & 0 & 1229\\
2010 & 360 & 3200 & 659 & 3859 & 0 & 1272\\
2011 & 347 & 3896 & 801 & 4697 & 0 & 1236\\
2012 & 349 & 3670 & 1008 & 4678 & 0 & 1243\\
2013 & 247 & 2465 & 654 & 3089 & 0 & 891\\
2014 & 346 & 3847 & 770 & 4592 & 0 & 1256\\
2015 & 332 & 3713 & 798 & 4496 & 0 & 1220\\
2016 & 355 & 3991 & 613 & 4604 & 0 & 1000\\
2017 & 363 & 4328 & 402 & 4730 & 0 & 1009\\
2018 & 368 & 4489 & 507 & 4996 & 0 & 733\\
2019 & 175 & 2263 & 138 & 2401 & 0 & 340\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Silvergray rockfish 
 The most recent assessment of Silvergray rockfish was a data-limited assessment conducted in 2010. Across available data, Silvergray rockfish have been observed and sampled generally by both commercial and recreational fisheries.  
 
 Across all years of available data, commercial fisheries have collected a total of 6,876 length observations, 0 age readings, and 2,284 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 3,256 length observations, 0 age readings, and 2,078 otoliths that are available to be aged. In California, since 2000, a total of 17 length observations, 0 age readings, and 6 otoliths have been collected In Oregon, since 2000, a total of 2,134 length observations, 0 age readings, and 2,072 otoliths have been collected In Washington, since 2000, a total of 1,105 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 465 length observations, 0 age readings, and 76 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 432 length observations, 0 age readings, and 76 otoliths that are available to be aged. In California, since 2003, a total of 1 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 328 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 103 length observations, 0 age readings, and 76 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1984 & 3 & 2 & 5 & 0 & 3\\
C & 1985 & 7 & 1 & 8 & 0 & 7\\
C & 1986 & 4 & 0 & 4 & 0 & 1\\
C & 1987 & 1 & 0 & 1 & 0 & 0\\
C & 1988 & 2 & 0 & 2 & 0 & 0\\
C & 1989 & 2 & 0 & 2 & 0 & 0\\
C & 1990 & 2 & 0 & 2 & 0 & 0\\
C & 1992 & 1 & 7 & 8 & 0 & 0\\
C & 1993 & 5 & 4 & 9 & 0 & 0\\
C & 1994 & 4 & 1 & 5 & 0 & 0\\
C & 1995 & 0 & 1 & 1 & 0 & 0\\
C & 1996 & 0 & 1 & 1 & 0 & 0\\
C & 1997 & 1 & 0 & 1 & 0 & 0\\
C & 1999 & 4 & 0 & 4 & 0 & 0\\
C & 2000 & 3 & 1 & 4 & 0 & 0\\
C & 2007 & 1 & 1 & 2 & 0 & 1\\
C & 2009 & 1 & 0 & 1 & 0 & 1\\
C & 2011 & 1 & 0 & 1 & 0 & 0\\
C & 2013 & 1 & 0 & 1 & 0 & 0\\
C & 2016 & 1 & 3 & 4 & 0 & 4\\
C & 2017 & 0 & 1 & 1 & 0 & 0\\
C & 2018 & 1 & 0 & 1 & 0 & 0\\
C & 2020 & 2 & 0 & 2 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1990 & 100 & 0 & 100 & 0 & 100\\
O & 1998 & 44 & 0 & 44 & 0 & 0\\
O & 1999 & 27 & 0 & 27 & 0 & 0\\
O & 2000 & 14 & 0 & 14 & 0 & 14\\
O & 2001 & 1 & 0 & 1 & 0 & 0\\
O & 2002 & 40 & 0 & 40 & 0 & 0\\
O & 2003 & 6 & 0 & 6 & 0 & 6\\
O & 2004 & 1 & 0 & 1 & 0 & 0\\
O & 2005 & 34 & 0 & 34 & 0 & 34\\
O & 2007 & 9 & 0 & 9 & 0 & 9\\
O & 2008 & 108 & 0 & 108 & 0 & 108\\
O & 2009 & 70 & 0 & 70 & 0 & 70\\
O & 2010 & 31 & 0 & 31 & 0 & 31\\
O & 2011 & 81 & 0 & 81 & 0 & 81\\
O & 2012 & 106 & 0 & 106 & 0 & 106\\
O & 2013 & 119 & 0 & 119 & 0 & 118\\
O & 2014 & 107 & 0 & 107 & 0 & 104\\
O & 2015 & 37 & 0 & 37 & 0 & 37\\
O & 2016 & 172 & 0 & 172 & 0 & 172\\
O & 2017 & 222 & 0 & 222 & 0 & 222\\
O & 2018 & 357 & 0 & 357 & 0 & 357\\
O & 2019 & 339 & 0 & 339 & 0 & 333\\
O & 2020 & 280 & 0 & 280 & 0 & 270\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 552 & 1 & 553 & 0 & 95\\
W & 1981 & 700 & 0 & 700 & 0 & 0\\
W & 1982 & 100 & 0 & 100 & 0 & 0\\
W & 1996 & 0 & 508 & 508 & 0 & 0\\
W & 1997 & 0 & 691 & 691 & 0 & 0\\
W & 1998 & 478 & 72 & 550 & 0 & 0\\
W & 1999 & 273 & 21 & 294 & 0 & 0\\
W & 2000 & 6 & 3 & 9 & 0 & 0\\
W & 2001 & 16 & 12 & 28 & 0 & 0\\
W & 2002 & 76 & 0 & 76 & 0 & 0\\
W & 2003 & 95 & 0 & 95 & 0 & 0\\
W & 2004 & 9 & 0 & 9 & 0 & 0\\
W & 2005 & 2 & 0 & 2 & 0 & 0\\
W & 2006 & 42 & 0 & 42 & 0 & 0\\
W & 2007 & 3 & 0 & 3 & 0 & 0\\
W & 2008 & 10 & 0 & 10 & 0 & 0\\
W & 2009 & 8 & 0 & 8 & 0 & 0\\
W & 2011 & 23 & 0 & 23 & 0 & 0\\
W & 2012 & 12 & 0 & 12 & 0 & 0\\
W & 2013 & 106 & 1 & 107 & 0 & 0\\
W & 2014 & 44 & 1 & 45 & 0 & 0\\
W & 2015 & 3 & 0 & 3 & 0 & 0\\
W & 2016 & 24 & 0 & 24 & 0 & 0\\
W & 2017 & 158 & 0 & 158 & 0 & 0\\
W & 2018 & 123 & 0 & 123 & 0 & 0\\
W & 2019 & 291 & 0 & 291 & 0 & 0\\
W & 2020 & 37 & 0 & 37 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2019 & 0 & 1 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 9 & 9 & 0 & 0\\
O & 2002 & 0 & 24 & 24 & 0 & 0\\
O & 2003 & 0 & 20 & 20 & 0 & 0\\
O & 2004 & 0 & 3 & 3 & 0 & 0\\
O & 2005 & 0 & 14 & 14 & 0 & 0\\
O & 2006 & 0 & 2 & 2 & 0 & 0\\
O & 2007 & 0 & 4 & 4 & 0 & 0\\
O & 2008 & 0 & 4 & 4 & 0 & 0\\
O & 2009 & 0 & 25 & 25 & 0 & 0\\
O & 2011 & 0 & 90 & 90 & 0 & 0\\
O & 2012 & 0 & 42 & 42 & 0 & 0\\
O & 2013 & 0 & 22 & 22 & 0 & 0\\
O & 2014 & 0 & 42 & 42 & 0 & 0\\
O & 2015 & 0 & 7 & 7 & 0 & 0\\
O & 2016 & 0 & 1 & 1 & 0 & 0\\
O & 2017 & 0 & 9 & 9 & 0 & 0\\
O & 2018 & 0 & 29 & 29 & 0 & 0\\
O & 2019 & 0 & 14 & 14 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2006 & 1 & 0 & 1 & 0 & 0\\
W & 2009 & 0 & 1 & 1 & 0 & 0\\
W & 2011 & 0 & 7 & 7 & 0 & 0\\
W & 2012 & 0 & 13 & 13 & 0 & 0\\
W & 2014 & 4 & 0 & 4 & 0 & 4\\
W & 2015 & 11 & 0 & 10 & 0 & 11\\
W & 2016 & 9 & 1 & 10 & 0 & 9\\
W & 2017 & 17 & 1 & 18 & 0 & 17\\
W & 2018 & 7 & 2 & 9 & 0 & 7\\
W & 2019 & 16 & 1 & 17 & 0 & 16\\
W & 2020 & 2 & 0 & 2 & 0 & 2\\
W & 2021 & 10 & 1 & 11 & 0 & 10\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Speckled rockfish 
 The most recent assessment of Speckled rockfish was a data-limited assessment conducted in 2010. Across available data, Speckled rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 3,003 length observations, 0 age readings, and 235 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 357 length observations, 0 age readings, and 3 otoliths that are available to be aged. In California, since 2000, a total of 357 length observations, 0 age readings, and 3 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 9,704 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 9,704 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 9,703 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 1 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 255 length observations, 0 age readings, and 127 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 3,455 length observations, 0 age readings, and 3,343 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 0 & 5 & 5 & 0 & 0\\
C & 1981 & 5 & 21 & 26 & 0 & 12\\
C & 1982 & 18 & 15 & 33 & 0 & 7\\
C & 1983 & 33 & 54 & 87 & 0 & 33\\
C & 1984 & 15 & 28 & 43 & 0 & 16\\
C & 1985 & 74 & 153 & 227 & 0 & 98\\
C & 1986 & 128 & 8 & 136 & 0 & 46\\
C & 1987 & 88 & 13 & 101 & 0 & 2\\
C & 1988 & 22 & 17 & 39 & 0 & 0\\
C & 1989 & 127 & 76 & 203 & 0 & 0\\
C & 1990 & 21 & 6 & 27 & 0 & 0\\
C & 1991 & 51 & 10 & 61 & 0 & 12\\
C & 1992 & 10 & 205 & 215 & 0 & 4\\
C & 1993 & 19 & 96 & 115 & 0 & 2\\
C & 1994 & 30 & 211 & 241 & 0 & 0\\
C & 1995 & 41 & 57 & 98 & 0 & 0\\
C & 1996 & 48 & 184 & 232 & 0 & 0\\
C & 1997 & 109 & 198 & 307 & 0 & 0\\
C & 1998 & 9 & 423 & 432 & 0 & 0\\
C & 1999 & 1 & 17 & 18 & 0 & 0\\
C & 2000 & 7 & 0 & 7 & 0 & 0\\
C & 2001 & 1 & 0 & 1 & 0 & 0\\
C & 2004 & 1 & 0 & 1 & 0 & 1\\
C & 2006 & 1 & 2 & 3 & 0 & 1\\
C & 2007 & 1 & 1 & 2 & 0 & 0\\
C & 2008 & 0 & 25 & 25 & 0 & 0\\
C & 2009 & 0 & 32 & 32 & 0 & 0\\
C & 2010 & 0 & 170 & 170 & 0 & 0\\
C & 2012 & 0 & 1 & 1 & 0 & 0\\
C & 2014 & 0 & 6 & 6 & 0 & 0\\
C & 2015 & 1 & 14 & 15 & 0 & 1\\
C & 2016 & 0 & 32 & 32 & 0 & 0\\
C & 2017 & 0 & 15 & 15 & 0 & 0\\
C & 2018 & 0 & 30 & 30 & 0 & 0\\
C & 2019 & 0 & 15 & 15 & 0 & 0\\
C & 2020 & 0 & 2 & 2 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 261 & 261 & 0 & 0\\
C & 2005 & 0 & 403 & 403 & 0 & 0\\
C & 2006 & 1 & 596 & 597 & 0 & 0\\
C & 2007 & 0 & 542 & 542 & 0 & 0\\
C & 2008 & 0 & 709 & 709 & 0 & 0\\
C & 2009 & 0 & 977 & 977 & 0 & 0\\
C & 2010 & 2 & 674 & 676 & 0 & 0\\
C & 2011 & 0 & 655 & 655 & 0 & 0\\
C & 2012 & 0 & 460 & 460 & 0 & 0\\
C & 2013 & 0 & 1022 & 1022 & 0 & 0\\
C & 2014 & 0 & 487 & 487 & 0 & 0\\
C & 2015 & 0 & 1097 & 1097 & 0 & 0\\
C & 2016 & 0 & 612 & 612 & 0 & 0\\
C & 2017 & 0 & 413 & 413 & 0 & 0\\
C & 2018 & 0 & 385 & 385 & 0 & 0\\
C & 2019 & 0 & 378 & 378 & 0 & 0\\
C & 2020 & 0 & 29 & 29 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2019 & 0 & 1 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2006 & 2 & 17 & 0 & 17 & 0 & 17\\
2007 & 3 & 106 & 0 & 106 & 0 & 25\\
2009 & 6 & 19 & 0 & 19 & 0 & 19\\
2010 & 3 & 4 & 3 & 7 & 0 & 7\\
2012 & 1 & 1 & 0 & 1 & 0 & 1\\
2013 & 5 & 70 & 0 & 70 & 0 & 23\\
2014 & 1 & 11 & 0 & 11 & 0 & 11\\
2016 & 1 & 11 & 0 & 11 & 0 & 11\\
2017 & 1 & 8 & 0 & 8 & 0 & 8\\
2018 & 1 & 1 & 0 & 1 & 0 & 1\\
2019 & 2 & 4 & 0 & 4 & 0 & 4\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 15 & 42 & 0 & 42 & 0 & 41\\
2005 & 10 & 28 & 0 & 28 & 0 & 27\\
2006 & 33 & 113 & 0 & 112 & 0 & 113\\
2007 & 20 & 42 & 0 & 42 & 0 & 39\\
2008 & 44 & 138 & 0 & 138 & 0 & 137\\
2009 & 35 & 86 & 1 & 86 & 0 & 85\\
2010 & 20 & 87 & 3 & 90 & 0 & 85\\
2011 & 30 & 70 & 0 & 70 & 0 & 68\\
2012 & 24 & 58 & 0 & 58 & 0 & 54\\
2013 & 26 & 59 & 0 & 59 & 0 & 59\\
2014 & 48 & 302 & 0 & 302 & 0 & 298\\
2015 & 87 & 541 & 4 & 541 & 0 & 534\\
2016 & 78 & 559 & 3 & 559 & 0 & 553\\
2017 & 66 & 362 & 5 & 362 & 0 & 351\\
2018 & 88 & 624 & 2 & 621 & 0 & 573\\
2019 & 73 & 345 & 4 & 345 & 0 & 326\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Splitnose rockfish 
 The most recent assessment of Splitnose rockfish was a full assessment conducted in 2009. Across available data, Splitnose rockfish have been observed and sampled generally by commercial fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 64,855 length observations, 1,647 age readings, and 24,144 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 30,920 length observations, 3 age readings, and 13,902 otoliths that are available to be aged. In California, since 2000, a total of 17,073 length observations, 0 age readings, and 3,147 otoliths have been collected In Oregon, since 2000, a total of 10,996 length observations, 0 age readings, and 10,755 otoliths have been collected In Washington, since 2000, a total of 2,851 length observations, 3 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 47,053 length observations, 2,906 age readings, and 8,501 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 338 & 6 & 344 & 114 & 184\\
C & 1981 & 457 & 2 & 459 & 92 & 364\\
C & 1982 & 530 & 6 & 534 & 0 & 368\\
C & 1983 & 2114 & 25 & 2137 & 460 & 2061\\
C & 1984 & 3631 & 42 & 3672 & 502 & 3574\\
C & 1985 & 4017 & 366 & 4383 & 475 & 3204\\
C & 1986 & 2375 & 38 & 2413 & 0 & 335\\
C & 1987 & 1605 & 95 & 1700 & 0 & 3\\
C & 1988 & 602 & 21 & 623 & 0 & 4\\
C & 1989 & 536 & 92 & 628 & 1 & 0\\
C & 1990 & 651 & 210 & 861 & 0 & 0\\
C & 1991 & 477 & 151 & 628 & 0 & 0\\
C & 1992 & 373 & 782 & 1155 & 0 & 0\\
C & 1993 & 1575 & 551 & 2126 & 0 & 0\\
C & 1994 & 1051 & 495 & 1546 & 0 & 0\\
C & 1995 & 850 & 676 & 1526 & 0 & 0\\
C & 1996 & 929 & 687 & 1616 & 0 & 0\\
C & 1997 & 1365 & 469 & 1834 & 0 & 0\\
C & 1998 & 2257 & 864 & 3121 & 0 & 145\\
C & 1999 & 1148 & 260 & 1408 & 0 & 0\\
C & 2000 & 563 & 326 & 889 & 0 & 38\\
C & 2001 & 802 & 656 & 1299 & 0 & 37\\
C & 2002 & 1156 & 561 & 1679 & 0 & 191\\
C & 2003 & 1085 & 393 & 1462 & 0 & 407\\
C & 2004 & 831 & 638 & 1142 & 0 & 331\\
C & 2005 & 470 & 420 & 885 & 0 & 339\\
C & 2006 & 590 & 409 & 999 & 0 & 404\\
C & 2007 & 702 & 447 & 1147 & 0 & 262\\
C & 2008 & 850 & 641 & 1440 & 0 & 318\\
C & 2009 & 477 & 702 & 1178 & 0 & 106\\
C & 2010 & 292 & 708 & 999 & 0 & 33\\
C & 2011 & 60 & 393 & 436 & 0 & 48\\
C & 2012 & 121 & 589 & 655 & 0 & 93\\
C & 2013 & 208 & 684 & 373 & 0 & 203\\
C & 2014 & 73 & 825 & 340 & 0 & 21\\
C & 2015 & 183 & 676 & 475 & 0 & 153\\
C & 2016 & 221 & 497 & 463 & 0 & 70\\
C & 2017 & 383 & 154 & 396 & 0 & 93\\
C & 2018 & 119 & 319 & 389 & 0 & 0\\
C & 2019 & 90 & 27 & 117 & 0 & 0\\
C & 2020 & 262 & 123 & 310 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1995 & 63 & 0 & 63 & 0 & 0\\
O & 1996 & 380 & 0 & 380 & 0 & 0\\
O & 1997 & 65 & 0 & 65 & 0 & 0\\
O & 1998 & 100 & 0 & 100 & 0 & 0\\
O & 2001 & 84 & 0 & 84 & 0 & 84\\
O & 2002 & 7 & 0 & 7 & 0 & 0\\
O & 2003 & 141 & 0 & 141 & 0 & 134\\
O & 2004 & 286 & 0 & 286 & 0 & 275\\
O & 2005 & 265 & 0 & 265 & 0 & 258\\
O & 2006 & 190 & 0 & 190 & 0 & 190\\
O & 2007 & 703 & 0 & 703 & 0 & 665\\
O & 2008 & 335 & 0 & 335 & 0 & 333\\
O & 2009 & 431 & 0 & 431 & 0 & 431\\
O & 2010 & 516 & 0 & 515 & 0 & 485\\
O & 2011 & 530 & 0 & 530 & 0 & 528\\
O & 2012 & 668 & 0 & 668 & 0 & 626\\
O & 2013 & 1045 & 0 & 1045 & 0 & 1040\\
O & 2014 & 1147 & 1 & 1148 & 0 & 1148\\
O & 2015 & 1188 & 0 & 1188 & 0 & 1128\\
O & 2016 & 662 & 0 & 662 & 0 & 662\\
O & 2017 & 863 & 0 & 863 & 0 & 863\\
O & 2018 & 759 & 1 & 760 & 0 & 760\\
O & 2019 & 785 & 1 & 786 & 0 & 756\\
O & 2020 & 389 & 0 & 389 & 0 & 389\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 80 & 80 & 0 & 0\\
W & 1997 & 0 & 355 & 355 & 0 & 0\\
W & 1998 & 150 & 4 & 154 & 0 & 0\\
W & 1999 & 19 & 5 & 24 & 0 & 0\\
W & 2000 & 104 & 13 & 117 & 0 & 0\\
W & 2001 & 64 & 5 & 69 & 0 & 0\\
W & 2002 & 298 & 17 & 315 & 0 & 0\\
W & 2003 & 147 & 10 & 157 & 0 & 0\\
W & 2004 & 44 & 0 & 44 & 0 & 0\\
W & 2005 & 4 & 0 & 4 & 0 & 0\\
W & 2006 & 54 & 0 & 54 & 0 & 0\\
W & 2007 & 83 & 2 & 85 & 0 & 0\\
W & 2008 & 62 & 0 & 62 & 0 & 0\\
W & 2009 & 163 & 0 & 163 & 0 & 0\\
W & 2010 & 9 & 0 & 9 & 3 & 0\\
W & 2011 & 94 & 2 & 96 & 0 & 0\\
W & 2012 & 290 & 23 & 313 & 0 & 0\\
W & 2013 & 364 & 13 & 377 & 0 & 0\\
W & 2014 & 285 & 21 & 306 & 0 & 0\\
W & 2015 & 223 & 6 & 229 & 0 & 0\\
W & 2016 & 82 & 10 & 92 & 0 & 0\\
W & 2017 & 15 & 0 & 15 & 0 & 0\\
W & 2018 & 104 & 0 & 104 & 0 & 0\\
W & 2019 & 142 & 0 & 142 & 0 & 0\\
W & 2020 & 97 & 1 & 98 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 134 & 6304 & 417 & 6633 & 513 & 555\\
2004 & 98 & 4821 & 314 & 5135 & 418 & 9\\
2005 & 126 & 4052 & 415 & 4467 & 498 & 45\\
2006 & 146 & 3181 & 380 & 3561 & 494 & 120\\
2007 & 158 & 2775 & 588 & 3363 & 495 & 389\\
2008 & 153 & 2539 & 379 & 2918 & 488 & 342\\
2009 & 153 & 2447 & 520 & 2967 & 0 & 895\\
2010 & 129 & 1525 & 496 & 2021 & 0 & 710\\
2011 & 132 & 1678 & 473 & 2151 & 0 & 725\\
2012 & 136 & 1658 & 257 & 1915 & 0 & 745\\
2013 & 102 & 1185 & 222 & 1407 & 0 & 555\\
2014 & 140 & 1614 & 345 & 1959 & 0 & 701\\
2015 & 132 & 1616 & 265 & 1881 & 0 & 697\\
2016 & 129 & 1758 & 165 & 1923 & 0 & 729\\
2017 & 129 & 1587 & 304 & 1891 & 0 & 520\\
2018 & 114 & 1579 & 212 & 1791 & 0 & 470\\
2019 & 74 & 969 & 101 & 1070 & 0 & 294\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Squarespot rockfish 
 The most recent assessment of Squarespot rockfish was a data-moderate assessment conducted in 2021. Across available data, Squarespot rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 155 length observations, 0 age readings, and 6 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 116 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2000, a total of 116 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 15,716 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 15,716 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 15,716 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 4,133 length observations, 408 age readings, and 809 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 1,478 length observations, 344 age readings, and 1,065 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1985 & 0 & 16 & 16 & 0 & 1\\
C & 1986 & 2 & 0 & 2 & 0 & 5\\
C & 1992 & 0 & 1 & 1 & 0 & 0\\
C & 1993 & 0 & 3 & 3 & 0 & 0\\
C & 1994 & 0 & 5 & 5 & 0 & 0\\
C & 1995 & 0 & 2 & 2 & 0 & 0\\
C & 1997 & 0 & 3 & 3 & 0 & 0\\
C & 1998 & 4 & 2 & 6 & 0 & 0\\
C & 1999 & 0 & 1 & 1 & 0 & 0\\
C & 2008 & 1 & 2 & 3 & 0 & 0\\
C & 2009 & 0 & 19 & 19 & 0 & 0\\
C & 2010 & 0 & 22 & 22 & 0 & 0\\
C & 2011 & 0 & 1 & 1 & 0 & 0\\
C & 2014 & 0 & 5 & 5 & 0 & 0\\
C & 2015 & 0 & 7 & 7 & 0 & 0\\
C & 2016 & 0 & 43 & 43 & 0 & 0\\
C & 2017 & 0 & 2 & 2 & 0 & 0\\
C & 2020 & 0 & 14 & 14 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 9 & 9 & 0 & 0\\
C & 2004 & 3 & 321 & 324 & 0 & 0\\
C & 2005 & 1 & 424 & 425 & 0 & 0\\
C & 2006 & 0 & 401 & 401 & 0 & 0\\
C & 2007 & 1 & 413 & 414 & 0 & 0\\
C & 2008 & 1 & 976 & 977 & 0 & 0\\
C & 2009 & 1 & 933 & 934 & 0 & 0\\
C & 2010 & 4 & 588 & 592 & 0 & 0\\
C & 2011 & 6 & 619 & 625 & 0 & 0\\
C & 2012 & 0 & 571 & 571 & 0 & 0\\
C & 2013 & 0 & 1738 & 1738 & 0 & 0\\
C & 2014 & 1 & 1505 & 1504 & 0 & 0\\
C & 2015 & 0 & 1721 & 1721 & 0 & 0\\
C & 2016 & 0 & 1401 & 1401 & 0 & 0\\
C & 2017 & 0 & 1256 & 1256 & 0 & 0\\
C & 2018 & 0 & 1243 & 1243 & 0 & 0\\
C & 2019 & 0 & 1540 & 1540 & 0 & 0\\
C & 2020 & 0 & 41 & 41 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 2 & 30 & 0 & 30 & 0 & 0\\
2004 & 5 & 141 & 18 & 159 & 0 & 25\\
2005 & 13 & 219 & 26 & 245 & 41 & 26\\
2006 & 6 & 151 & 1 & 152 & 10 & 39\\
2007 & 13 & 333 & 91 & 424 & 33 & 60\\
2008 & 10 & 234 & 5 & 239 & 36 & 44\\
2009 & 16 & 487 & 3 & 490 & 34 & 74\\
2010 & 13 & 181 & 4 & 185 & 17 & 46\\
2011 & 6 & 64 & 13 & 77 & 11 & 24\\
2012 & 6 & 28 & 4 & 32 & 5 & 27\\
2013 & 10 & 517 & 7 & 524 & 21 & 73\\
2014 & 9 & 17 & 215 & 232 & 6 & 40\\
2015 & 9 & 223 & 24 & 247 & 34 & 52\\
2016 & 18 & 172 & 157 & 329 & 59 & 79\\
2017 & 11 & 277 & 45 & 322 & 62 & 66\\
2018 & 11 & 193 & 47 & 240 & 17 & 91\\
2019 & 6 & 187 & 19 & 206 & 22 & 43\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 4 & 6 & 0 & 6 & 1 & 5\\
2005 & 17 & 26 & 2 & 28 & 4 & 21\\
2006 & 13 & 35 & 1 & 35 & 7 & 27\\
2007 & 8 & 10 & 0 & 10 & 7 & 3\\
2008 & 21 & 63 & 1 & 64 & 7 & 52\\
2009 & 12 & 20 & 0 & 20 & 4 & 15\\
2010 & 8 & 28 & 0 & 28 & 3 & 23\\
2011 & 13 & 24 & 0 & 24 & 2 & 19\\
2012 & 4 & 4 & 0 & 4 & 0 & 4\\
2013 & 7 & 8 & 0 & 8 & 2 & 6\\
2014 & 27 & 81 & 5 & 86 & 20 & 60\\
2015 & 36 & 145 & 1 & 145 & 16 & 128\\
2016 & 45 & 220 & 2 & 221 & 25 & 191\\
2017 & 56 & 265 & 1 & 265 & 45 & 213\\
2018 & 67 & 343 & 2 & 343 & 174 & 144\\
2019 & 60 & 192 & 0 & 191 & 27 & 154\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Starry flounder 
 The most recent assessment of Starry flounder was a data-limited assessment conducted in 2010. Across available data, Starry flounder have been observed and sampled generally by both the commercial and recreational fisheries and the NWFSC WCGBT survey.  
 
 Across all years of available data, commercial fisheries have collected a total of 8,729 length observations, 0 age readings, and 3,027 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 6,838 length observations, 0 age readings, and 2,836 otoliths that are available to be aged. In California, since 2000, a total of 3,927 length observations, 0 age readings, and 70 otoliths have been collected In Oregon, since 2000, a total of 2,911 length observations, 0 age readings, and 2,766 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 1,011 length observations, 0 age readings, and 7 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 982 length observations, 0 age readings, and 4 otoliths that are available to be aged. In California, since 2003, a total of 655 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2003, a total of 245 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 82 length observations, 0 age readings, and 4 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 459 length observations, 0 age readings, and 407 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2001 & 0 & 7 & 7 & 0 & 0\\
C & 2002 & 11 & 19 & 30 & 0 & 0\\
C & 2003 & 0 & 24 & 24 & 0 & 0\\
C & 2004 & 31 & 37 & 37 & 0 & 0\\
C & 2005 & 115 & 30 & 132 & 0 & 10\\
C & 2006 & 182 & 17 & 186 & 0 & 0\\
C & 2007 & 89 & 26 & 115 & 0 & 0\\
C & 2008 & 132 & 15 & 147 & 0 & 60\\
C & 2009 & 172 & 192 & 364 & 0 & 0\\
C & 2010 & 77 & 116 & 193 & 0 & 0\\
C & 2011 & 27 & 203 & 230 & 0 & 0\\
C & 2012 & 38 & 116 & 154 & 0 & 0\\
C & 2013 & 107 & 114 & 221 & 0 & 0\\
C & 2014 & 141 & 103 & 244 & 0 & 0\\
C & 2015 & 55 & 125 & 180 & 0 & 0\\
C & 2016 & 8 & 288 & 296 & 0 & 0\\
C & 2017 & 3 & 275 & 278 & 0 & 0\\
C & 2018 & 24 & 223 & 247 & 0 & 0\\
C & 2019 & 0 & 475 & 475 & 0 & 0\\
C & 2020 & 0 & 367 & 367 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1987 & 150 & 0 & 150 & 0 & 150\\
O & 1996 & 41 & 0 & 41 & 0 & 41\\
O & 2001 & 3 & 0 & 3 & 0 & 0\\
O & 2003 & 12 & 0 & 12 & 0 & 12\\
O & 2004 & 31 & 7 & 38 & 0 & 30\\
O & 2005 & 72 & 0 & 72 & 0 & 72\\
O & 2007 & 385 & 0 & 385 & 0 & 385\\
O & 2008 & 337 & 0 & 337 & 0 & 337\\
O & 2009 & 253 & 0 & 253 & 0 & 253\\
O & 2010 & 294 & 6 & 300 & 0 & 259\\
O & 2011 & 332 & 0 & 332 & 0 & 332\\
O & 2012 & 180 & 0 & 180 & 0 & 180\\
O & 2013 & 156 & 0 & 156 & 0 & 156\\
O & 2014 & 159 & 0 & 159 & 0 & 159\\
O & 2015 & 295 & 0 & 295 & 0 & 234\\
O & 2016 & 122 & 0 & 122 & 0 & 120\\
O & 2017 & 78 & 0 & 78 & 0 & 78\\
O & 2018 & 150 & 2 & 152 & 0 & 122\\
O & 2019 & 36 & 0 & 36 & 0 & 36\\
O & 2020 & 1 & 0 & 1 & 0 & 1\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 100 & 0 & 100 & 0 & 0\\
W & 1981 & 300 & 0 & 300 & 0 & 0\\
W & 1982 & 150 & 0 & 150 & 0 & 0\\
W & 1983 & 450 & 0 & 450 & 0 & 0\\
W & 1984 & 400 & 0 & 400 & 0 & 0\\
W & 1985 & 250 & 0 & 250 & 0 & 0\\
W & 1986 & 50 & 0 & 50 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 50 & 50 & 0 & 0\\
C & 2005 & 0 & 110 & 110 & 0 & 0\\
C & 2006 & 0 & 51 & 51 & 0 & 0\\
C & 2007 & 0 & 43 & 43 & 0 & 0\\
C & 2008 & 0 & 42 & 42 & 0 & 0\\
C & 2009 & 0 & 30 & 30 & 0 & 0\\
C & 2010 & 0 & 22 & 22 & 0 & 0\\
C & 2011 & 0 & 21 & 21 & 0 & 0\\
C & 2012 & 0 & 48 & 48 & 0 & 0\\
C & 2013 & 1 & 39 & 40 & 0 & 0\\
C & 2014 & 0 & 76 & 76 & 0 & 0\\
C & 2015 & 0 & 45 & 45 & 0 & 0\\
C & 2016 & 0 & 14 & 14 & 0 & 0\\
C & 2017 & 0 & 16 & 16 & 0 & 0\\
C & 2018 & 0 & 11 & 11 & 0 & 0\\
C & 2019 & 0 & 31 & 31 & 0 & 0\\
C & 2020 & 0 & 5 & 5 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 3 & 3 & 0 & 0\\
O & 2002 & 0 & 17 & 17 & 0 & 0\\
O & 2003 & 0 & 62 & 62 & 0 & 0\\
O & 2004 & 0 & 34 & 34 & 0 & 0\\
O & 2005 & 0 & 26 & 26 & 0 & 0\\
O & 2006 & 0 & 15 & 15 & 0 & 0\\
O & 2007 & 0 & 4 & 4 & 0 & 0\\
O & 2008 & 0 & 16 & 16 & 0 & 0\\
O & 2009 & 0 & 5 & 5 & 0 & 0\\
O & 2010 & 0 & 2 & 2 & 0 & 0\\
O & 2011 & 0 & 24 & 24 & 0 & 0\\
O & 2012 & 0 & 7 & 7 & 0 & 0\\
O & 2013 & 0 & 3 & 3 & 0 & 0\\
O & 2014 & 0 & 5 & 5 & 0 & 0\\
O & 2015 & 0 & 9 & 9 & 0 & 0\\
O & 2016 & 0 & 9 & 9 & 0 & 0\\
O & 2017 & 0 & 16 & 16 & 0 & 0\\
O & 2018 & 0 & 4 & 4 & 0 & 0\\
O & 2019 & 0 & 4 & 4 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 9 & 0 & 9 & 0 & 3\\
W & 2003 & 8 & 0 & 8 & 0 & 0\\
W & 2004 & 13 & 8 & 21 & 0 & 0\\
W & 2005 & 1 & 0 & 1 & 0 & 0\\
W & 2006 & 0 & 1 & 1 & 0 & 0\\
W & 2007 & 2 & 5 & 7 & 0 & 0\\
W & 2008 & 0 & 1 & 1 & 0 & 0\\
W & 2009 & 0 & 7 & 7 & 0 & 0\\
W & 2010 & 2 & 3 & 5 & 0 & 1\\
W & 2011 & 0 & 2 & 2 & 0 & 0\\
W & 2012 & 0 & 2 & 2 & 0 & 0\\
W & 2013 & 0 & 2 & 2 & 0 & 0\\
W & 2014 & 0 & 5 & 5 & 0 & 0\\
W & 2015 & 1 & 3 & 4 & 0 & 0\\
W & 2017 & 0 & 9 & 9 & 0 & 0\\
W & 2018 & 2 & 1 & 3 & 0 & 2\\
W & 2019 & 1 & 3 & 4 & 0 & 1\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 4 & 36 & 0 & 36 & 0 & 30\\
2005 & 7 & 41 & 1 & 42 & 0 & 31\\
2006 & 4 & 7 & 0 & 7 & 0 & 7\\
2007 & 6 & 15 & 0 & 15 & 0 & 15\\
2008 & 9 & 22 & 0 & 22 & 0 & 22\\
2009 & 11 & 21 & 0 & 21 & 0 & 21\\
2010 & 13 & 27 & 0 & 27 & 0 & 27\\
2011 & 18 & 62 & 0 & 62 & 0 & 62\\
2012 & 7 & 15 & 0 & 15 & 0 & 15\\
2013 & 10 & 26 & 0 & 26 & 0 & 0\\
2014 & 8 & 36 & 0 & 36 & 0 & 36\\
2015 & 10 & 42 & 0 & 42 & 0 & 33\\
2016 & 10 & 43 & 0 & 43 & 0 & 43\\
2017 & 16 & 29 & 0 & 29 & 0 & 29\\
2018 & 6 & 23 & 0 & 23 & 0 & 23\\
2019 & 6 & 13 & 0 & 13 & 0 & 13\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Starry rockfish 
 The most recent assessment of Starry rockfish was a data-limited assessment conducted in 2010. Across available data, Starry rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 4,939 length observations, 0 age readings, and 83 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 615 length observations, 0 age readings, and 5 otoliths that are available to be aged. In California, since 2000, a total of 615 length observations, 0 age readings, and 5 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 30,862 length observations, 0 age readings, and 36 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 30,862 length observations, 0 age readings, and 36 otoliths that are available to be aged. In California, since 2003, a total of 30,862 length observations, 0 age readings, and 36 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 79 length observations, 0 age readings, and 73 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 1,805 length observations, 0 age readings, and 1,726 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 1 & 1 & 2 & 0 & 8\\
C & 1981 & 0 & 9 & 9 & 0 & 3\\
C & 1982 & 0 & 2 & 2 & 0 & 0\\
C & 1983 & 0 & 41 & 41 & 0 & 0\\
C & 1984 & 1 & 57 & 58 & 0 & 10\\
C & 1985 & 4 & 89 & 93 & 0 & 46\\
C & 1986 & 19 & 198 & 217 & 0 & 11\\
C & 1987 & 45 & 83 & 128 & 0 & 0\\
C & 1988 & 4 & 33 & 37 & 0 & 0\\
C & 1989 & 29 & 202 & 231 & 0 & 0\\
C & 1990 & 5 & 13 & 18 & 0 & 0\\
C & 1991 & 1 & 25 & 26 & 0 & 0\\
C & 1992 & 0 & 399 & 399 & 0 & 0\\
C & 1993 & 4 & 378 & 382 & 0 & 0\\
C & 1994 & 0 & 463 & 463 & 0 & 0\\
C & 1995 & 0 & 368 & 351 & 0 & 0\\
C & 1996 & 0 & 667 & 667 & 0 & 0\\
C & 1997 & 1 & 382 & 383 & 0 & 0\\
C & 1998 & 2 & 678 & 680 & 0 & 0\\
C & 1999 & 0 & 137 & 137 & 0 & 0\\
C & 2000 & 0 & 12 & 12 & 0 & 0\\
C & 2001 & 0 & 41 & 36 & 0 & 0\\
C & 2002 & 0 & 5 & 5 & 0 & 0\\
C & 2006 & 0 & 17 & 17 & 0 & 0\\
C & 2007 & 0 & 13 & 12 & 0 & 0\\
C & 2008 & 0 & 29 & 27 & 0 & 0\\
C & 2009 & 1 & 30 & 31 & 0 & 0\\
C & 2010 & 0 & 21 & 21 & 0 & 0\\
C & 2011 & 0 & 15 & 15 & 0 & 0\\
C & 2012 & 0 & 10 & 10 & 0 & 0\\
C & 2013 & 0 & 23 & 23 & 0 & 0\\
C & 2014 & 0 & 35 & 35 & 0 & 0\\
C & 2015 & 0 & 30 & 30 & 0 & 0\\
C & 2016 & 3 & 69 & 72 & 0 & 3\\
C & 2017 & 0 & 18 & 18 & 0 & 0\\
C & 2018 & 0 & 76 & 76 & 0 & 0\\
C & 2019 & 0 & 71 & 71 & 0 & 2\\
C & 2020 & 0 & 104 & 104 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 12 & 12 & 0 & 0\\
C & 2004 & 0 & 833 & 833 & 0 & 0\\
C & 2005 & 0 & 1549 & 1549 & 0 & 0\\
C & 2006 & 3 & 1810 & 1813 & 0 & 0\\
C & 2007 & 7 & 2976 & 2983 & 0 & 0\\
C & 2008 & 1 & 2665 & 2666 & 0 & 0\\
C & 2009 & 4 & 2449 & 2453 & 0 & 0\\
C & 2010 & 15 & 1968 & 1983 & 0 & 0\\
C & 2011 & 2 & 2016 & 2018 & 0 & 0\\
C & 2012 & 0 & 1691 & 1690 & 0 & 0\\
C & 2013 & 2 & 1889 & 1891 & 0 & 0\\
C & 2014 & 0 & 942 & 941 & 0 & 0\\
C & 2015 & 1 & 1326 & 1327 & 0 & 0\\
C & 2016 & 0 & 1396 & 1396 & 0 & 0\\
C & 2017 & 6 & 2471 & 2477 & 0 & 0\\
C & 2018 & 10 & 2118 & 2128 & 0 & 7\\
C & 2019 & 11 & 2619 & 2628 & 0 & 29\\
C & 2020 & 0 & 74 & 74 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 1 & 1 & 0 & 1 & 0 & 0\\
2004 & 3 & 3 & 0 & 3 & 0 & 3\\
2005 & 2 & 3 & 0 & 3 & 0 & 3\\
2006 & 1 & 1 & 0 & 1 & 0 & 1\\
2007 & 3 & 6 & 0 & 6 & 0 & 6\\
2008 & 2 & 2 & 0 & 2 & 0 & 2\\
2009 & 6 & 17 & 1 & 18 & 0 & 18\\
2010 & 2 & 2 & 0 & 2 & 0 & 2\\
2011 & 1 & 1 & 0 & 1 & 0 & 1\\
2012 & 2 & 3 & 0 & 3 & 0 & 3\\
2013 & 1 & 2 & 0 & 2 & 0 & 0\\
2016 & 2 & 2 & 0 & 2 & 0 & 2\\
2017 & 5 & 9 & 0 & 9 & 0 & 6\\
2018 & 4 & 9 & 1 & 10 & 0 & 10\\
2019 & 4 & 16 & 0 & 16 & 0 & 16\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 13 & 24 & 0 & 24 & 0 & 24\\
2005 & 15 & 32 & 0 & 32 & 0 & 32\\
2006 & 27 & 60 & 2 & 61 & 0 & 62\\
2007 & 29 & 63 & 0 & 63 & 0 & 61\\
2008 & 50 & 117 & 0 & 117 & 0 & 115\\
2009 & 39 & 85 & 0 & 85 & 0 & 80\\
2010 & 37 & 93 & 0 & 93 & 0 & 89\\
2011 & 23 & 39 & 1 & 39 & 0 & 35\\
2012 & 30 & 69 & 0 & 69 & 0 & 63\\
2013 & 29 & 55 & 0 & 55 & 0 & 54\\
2014 & 47 & 124 & 0 & 124 & 0 & 122\\
2015 & 59 & 189 & 0 & 189 & 0 & 188\\
2016 & 51 & 120 & 0 & 120 & 0 & 112\\
2017 & 60 & 152 & 1 & 153 & 0 & 144\\
2018 & 80 & 253 & 1 & 253 & 0 & 237\\
2019 & 85 & 323 & 6 & 328 & 0 & 308\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Treefish 
 The most recent assessment of Treefish was a data-limited assessment conducted in 2010. Across available data, Treefish have been observed and sampled generally by both commercial and recreational fisheries.  
 
 Across all years of available data, commercial fisheries have collected a total of 1,075 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 855 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2000, a total of 855 length observations, 0 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 10,368 length observations, 0 age readings, and 0 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 10,368 length observations, 0 age readings, and 0 otoliths that are available to be aged. In California, since 2003, a total of 10,368 length observations, 0 age readings, and 0 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1985 & 0 & 1 & 1 & 0 & 0\\
C & 1986 & 1 & 0 & 1 & 0 & 0\\
C & 1987 & 0 & 5 & 5 & 0 & 0\\
C & 1989 & 0 & 14 & 14 & 0 & 0\\
C & 1993 & 0 & 5 & 5 & 0 & 0\\
C & 1994 & 0 & 10 & 10 & 0 & 0\\
C & 1995 & 0 & 17 & 17 & 0 & 0\\
C & 1996 & 0 & 46 & 46 & 0 & 0\\
C & 1997 & 0 & 52 & 52 & 0 & 0\\
C & 1998 & 0 & 30 & 30 & 0 & 0\\
C & 1999 & 0 & 39 & 39 & 0 & 0\\
C & 2000 & 1 & 117 & 118 & 0 & 0\\
C & 2001 & 0 & 116 & 108 & 0 & 0\\
C & 2002 & 0 & 24 & 15 & 0 & 0\\
C & 2003 & 0 & 14 & 14 & 0 & 0\\
C & 2004 & 0 & 3 & 3 & 0 & 0\\
C & 2005 & 0 & 11 & 0 & 0 & 0\\
C & 2006 & 0 & 35 & 0 & 0 & 0\\
C & 2007 & 0 & 63 & 2 & 0 & 0\\
C & 2008 & 0 & 46 & 3 & 0 & 0\\
C & 2009 & 0 & 31 & 18 & 0 & 0\\
C & 2010 & 0 & 80 & 69 & 0 & 0\\
C & 2011 & 0 & 56 & 56 & 0 & 0\\
C & 2012 & 0 & 37 & 37 & 0 & 0\\
C & 2013 & 0 & 23 & 23 & 0 & 0\\
C & 2014 & 0 & 14 & 14 & 0 & 0\\
C & 2015 & 0 & 76 & 76 & 0 & 0\\
C & 2016 & 1 & 87 & 88 & 0 & 0\\
C & 2017 & 0 & 76 & 76 & 0 & 0\\
C & 2018 & 1 & 36 & 37 & 0 & 0\\
C & 2019 & 0 & 69 & 68 & 0 & 0\\
C & 2020 & 0 & 31 & 30 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 30 & 30 & 0 & 0\\
C & 2004 & 0 & 278 & 278 & 0 & 0\\
C & 2005 & 0 & 400 & 400 & 0 & 0\\
C & 2006 & 1 & 497 & 498 & 0 & 0\\
C & 2007 & 0 & 664 & 664 & 0 & 0\\
C & 2008 & 1 & 669 & 670 & 0 & 0\\
C & 2009 & 0 & 869 & 869 & 0 & 0\\
C & 2010 & 1 & 550 & 552 & 0 & 0\\
C & 2011 & 1 & 813 & 814 & 0 & 0\\
C & 2012 & 0 & 834 & 834 & 0 & 0\\
C & 2013 & 0 & 974 & 973 & 0 & 0\\
C & 2014 & 0 & 621 & 621 & 0 & 0\\
C & 2015 & 0 & 688 & 688 & 0 & 0\\
C & 2016 & 0 & 764 & 764 & 0 & 0\\
C & 2017 & 0 & 559 & 559 & 0 & 0\\
C & 2018 & 0 & 497 & 497 & 0 & 0\\
C & 2019 & 4 & 599 & 603 & 0 & 0\\
C & 2020 & 0 & 54 & 54 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Vermilion/Sunset rockfish 
 The most recent assessment of Vermilion/Sunset rockfish was a full assessment conducted in 2021. Across available data, Vermilion/Sunset rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 22,483 length observations, 1,233 age readings, and 1,804 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 9,130 length observations, 1,122 age readings, and 1,390 otoliths that are available to be aged. In California, since 2000, a total of 6,795 length observations, 0 age readings, and 509 otoliths have been collected In Oregon, since 2000, a total of 2,335 length observations, 1,122 age readings, and 881 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 144,311 length observations, 2,208 age readings, and 1,198 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 143,629 length observations, 2,205 age readings, and 1,198 otoliths that are available to be aged. In California, since 2003, a total of 129,489 length observations, 0 age readings, and 117 otoliths have been collected In Oregon, since 2003, a total of 12,708 length observations, 1,450 age readings, and 943 otoliths have been collected In Washington, since 2003, a total of 1,432 length observations, 755 age readings, and 138 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 2,549 length observations, 776 age readings, and 980 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 22,986 length observations, 2,564 age readings, and 15,670 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 44 & 768 & 812 & 5 & 15\\
C & 1981 & 11 & 898 & 909 & 0 & 13\\
C & 1982 & 16 & 408 & 424 & 3 & 53\\
C & 1983 & 40 & 216 & 256 & 17 & 119\\
C & 1984 & 121 & 168 & 289 & 56 & 129\\
C & 1985 & 54 & 427 & 481 & 30 & 66\\
C & 1986 & 63 & 514 & 577 & 0 & 19\\
C & 1987 & 106 & 142 & 248 & 0 & 0\\
C & 1988 & 21 & 151 & 172 & 0 & 0\\
C & 1989 & 19 & 405 & 424 & 0 & 0\\
C & 1990 & 13 & 179 & 192 & 0 & 0\\
C & 1991 & 1 & 127 & 128 & 0 & 0\\
C & 1992 & 27 & 634 & 661 & 0 & 0\\
C & 1993 & 33 & 1475 & 1508 & 0 & 0\\
C & 1994 & 0 & 789 & 789 & 0 & 0\\
C & 1995 & 2 & 1079 & 1081 & 0 & 0\\
C & 1996 & 1 & 1141 & 1142 & 0 & 0\\
C & 1997 & 3 & 1223 & 1226 & 0 & 0\\
C & 1998 & 50 & 1202 & 1252 & 0 & 0\\
C & 1999 & 64 & 699 & 763 & 0 & 0\\
C & 2000 & 3 & 196 & 198 & 0 & 0\\
C & 2001 & 0 & 174 & 167 & 0 & 21\\
C & 2002 & 18 & 173 & 188 & 0 & 1\\
C & 2003 & 9 & 41 & 50 & 0 & 9\\
C & 2004 & 9 & 72 & 77 & 0 & 2\\
C & 2005 & 1 & 121 & 72 & 0 & 1\\
C & 2006 & 8 & 247 & 157 & 0 & 8\\
C & 2007 & 11 & 308 & 256 & 0 & 0\\
C & 2008 & 11 & 206 & 181 & 0 & 2\\
C & 2009 & 7 & 279 & 251 & 0 & 5\\
C & 2010 & 5 & 145 & 147 & 0 & 4\\
C & 2011 & 0 & 239 & 238 & 0 & 0\\
C & 2012 & 7 & 167 & 170 & 0 & 6\\
C & 2013 & 2 & 247 & 238 & 0 & 2\\
C & 2014 & 37 & 704 & 696 & 0 & 3\\
C & 2015 & 52 & 644 & 682 & 0 & 54\\
C & 2016 & 43 & 754 & 796 & 0 & 41\\
C & 2017 & 14 & 532 & 546 & 0 & 14\\
C & 2018 & 50 & 476 & 511 & 0 & 0\\
C & 2019 & 45 & 541 & 570 & 0 & 336\\
C & 2020 & 87 & 517 & 604 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1999 & 19 & 0 & 19 & 0 & 0\\
O & 2000 & 68 & 0 & 68 & 0 & 0\\
O & 2001 & 107 & 0 & 107 & 14 & 0\\
O & 2002 & 38 & 0 & 38 & 12 & 5\\
O & 2003 & 63 & 0 & 63 & 50 & 3\\
O & 2004 & 83 & 0 & 83 & 65 & 0\\
O & 2005 & 57 & 0 & 57 & 47 & 0\\
O & 2006 & 54 & 2 & 56 & 39 & 0\\
O & 2007 & 59 & 1 & 60 & 20 & 23\\
O & 2008 & 38 & 0 & 38 & 13 & 12\\
O & 2009 & 118 & 0 & 118 & 56 & 58\\
O & 2010 & 88 & 0 & 87 & 38 & 42\\
O & 2011 & 202 & 0 & 202 & 106 & 96\\
O & 2012 & 118 & 0 & 118 & 58 & 55\\
O & 2013 & 177 & 1 & 178 & 87 & 86\\
O & 2014 & 138 & 0 & 138 & 69 & 69\\
O & 2015 & 72 & 0 & 72 & 35 & 37\\
O & 2016 & 125 & 0 & 125 & 63 & 61\\
O & 2017 & 192 & 0 & 192 & 97 & 95\\
O & 2018 & 123 & 0 & 123 & 64 & 59\\
O & 2019 & 318 & 0 & 318 & 149 & 141\\
O & 2020 & 94 & 0 & 94 & 40 & 39\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 34 & 34 & 0 & 0\\
C & 2004 & 3 & 6471 & 6474 & 0 & 0\\
C & 2005 & 2 & 9163 & 9165 & 0 & 0\\
C & 2006 & 0 & 10023 & 10023 & 0 & 0\\
C & 2007 & 2 & 9555 & 9557 & 0 & 0\\
C & 2008 & 3 & 6230 & 6233 & 0 & 0\\
C & 2009 & 1 & 5491 & 5492 & 0 & 0\\
C & 2010 & 6 & 6680 & 6686 & 0 & 0\\
C & 2011 & 6 & 8347 & 8353 & 0 & 0\\
C & 2012 & 4 & 8641 & 8644 & 0 & 0\\
C & 2013 & 5 & 9948 & 9953 & 0 & 0\\
C & 2014 & 2 & 7239 & 7241 & 0 & 0\\
C & 2015 & 1 & 9220 & 9220 & 0 & 0\\
C & 2016 & 5 & 8466 & 8471 & 0 & 0\\
C & 2017 & 9 & 6966 & 6974 & 0 & 0\\
C & 2018 & 7 & 7204 & 7211 & 0 & 76\\
C & 2019 & 20 & 9352 & 9371 & 0 & 41\\
C & 2020 & 0 & 387 & 387 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 224 & 224 & 0 & 0\\
O & 2002 & 0 & 450 & 450 & 0 & 0\\
O & 2003 & 0 & 743 & 743 & 0 & 0\\
O & 2004 & 0 & 413 & 413 & 0 & 0\\
O & 2005 & 107 & 960 & 1067 & 115 & 16\\
O & 2006 & 186 & 609 & 795 & 284 & 0\\
O & 2007 & 206 & 823 & 1029 & 109 & 99\\
O & 2008 & 327 & 739 & 1066 & 179 & 156\\
O & 2009 & 188 & 501 & 689 & 97 & 92\\
O & 2010 & 157 & 633 & 790 & 85 & 74\\
O & 2011 & 191 & 724 & 915 & 101 & 90\\
O & 2012 & 174 & 928 & 1102 & 89 & 85\\
O & 2013 & 170 & 647 & 817 & 87 & 82\\
O & 2014 & 84 & 373 & 457 & 43 & 43\\
O & 2015 & 60 & 296 & 356 & 32 & 30\\
O & 2016 & 49 & 271 & 320 & 29 & 21\\
O & 2017 & 52 & 543 & 595 & 29 & 23\\
O & 2018 & 47 & 575 & 622 & 29 & 18\\
O & 2019 & 142 & 611 & 753 & 75 & 66\\
O & 2020 & 115 & 64 & 179 & 67 & 48\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 3 & 5 & 8 & 3 & 0\\
W & 2003 & 1 & 3 & 4 & 0 & 0\\
W & 2004 & 15 & 0 & 15 & 11 & 4\\
W & 2005 & 27 & 28 & 55 & 23 & 3\\
W & 2006 & 27 & 30 & 57 & 25 & 2\\
W & 2007 & 36 & 33 & 69 & 35 & 1\\
W & 2008 & 28 & 15 & 43 & 28 & 0\\
W & 2009 & 9 & 7 & 16 & 9 & 0\\
W & 2010 & 3 & 6 & 9 & 3 & 0\\
W & 2011 & 11 & 24 & 35 & 11 & 0\\
W & 2012 & 17 & 35 & 52 & 16 & 1\\
W & 2013 & 5 & 49 & 54 & 5 & 0\\
W & 2014 & 60 & 2 & 62 & 58 & 2\\
W & 2015 & 130 & 11 & 141 & 127 & 3\\
W & 2016 & 91 & 3 & 94 & 87 & 4\\
W & 2017 & 82 & 65 & 147 & 81 & 0\\
W & 2018 & 103 & 100 & 203 & 86 & 10\\
W & 2019 & 160 & 105 & 265 & 150 & 10\\
W & 2020 & 18 & 2 & 20 & 0 & 18\\
W & 2021 & 81 & 10 & 91 & 0 & 80\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2010 & 15 & 78 & 28 & 106 & 93 & 13\\
2011 & 4 & 28 & 0 & 28 & 4 & 24\\
2012 & 15 & 467 & 4 & 471 & 106 & 204\\
2013 & 11 & 396 & 9 & 405 & 70 & 165\\
2014 & 14 & 35 & 2 & 37 & 26 & 11\\
2015 & 12 & 226 & 0 & 226 & 9 & 138\\
2016 & 20 & 75 & 25 & 100 & 56 & 44\\
2017 & 16 & 199 & 44 & 243 & 75 & 132\\
2018 & 12 & 196 & 1 & 197 & 47 & 93\\
2019 & 7 & 172 & 0 & 172 & 44 & 89\\
2003 & 6 & 61 & 1 & 62 & 0 & 0\\
2004 & 3 & 8 & 1 & 9 & 8 & 0\\
2005 & 6 & 48 & 1 & 49 & 11 & 17\\
2006 & 5 & 27 & 0 & 27 & 18 & 9\\
2007 & 8 & 120 & 0 & 120 & 56 & 0\\
2008 & 12 & 73 & 1 & 74 & 39 & 26\\
2009 & 13 & 223 & 0 & 223 & 114 & 15\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 53 & 736 & 1 & 737 & 73 & 552\\
2005 & 72 & 838 & 27 & 864 & 142 & 673\\
2006 & 65 & 577 & 5 & 579 & 555 & 24\\
2007 & 71 & 904 & 24 & 907 & 129 & 776\\
2008 & 78 & 857 & 2 & 857 & 90 & 766\\
2009 & 74 & 1062 & 6 & 1067 & 202 & 864\\
2010 & 79 & 1151 & 3 & 1151 & 250 & 897\\
2011 & 73 & 1325 & 3 & 1328 & 152 & 1166\\
2012 & 91 & 1184 & 6 & 1184 & 169 & 1013\\
2013 & 93 & 1224 & 12 & 1230 & 267 & 961\\
2014 & 114 & 1698 & 9 & 1706 & 0 & 1696\\
2015 & 146 & 1905 & 14 & 1911 & 0 & 1904\\
2016 & 142 & 1905 & 14 & 1904 & 0 & 1863\\
2017 & 157 & 2653 & 27 & 2649 & 0 & 1027\\
2018 & 158 & 2645 & 17 & 2648 & 535 & 538\\
2019 & 148 & 2233 & 35 & 2264 & 0 & 950\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Widow rockfish 
 The most recent assessment of Widow rockfish was a update assessment conducted in 2019. Across available data, Widow rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 202,098 length observations, 89,194 age readings, and 124,286 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 52,978 length observations, 21,059 age readings, and 20,305 otoliths that are available to be aged. In California, since 2000, a total of 8,073 length observations, 1,884 age readings, and 3,278 otoliths have been collected In Oregon, since 2000, a total of 27,734 length observations, 9,848 age readings, and 15,444 otoliths have been collected In Washington, since 2000, a total of 17,171 length observations, 9,327 age readings, and 1,583 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 15,760 length observations, 2,328 age readings, and 704 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 15,263 length observations, 2,328 age readings, and 704 otoliths that are available to be aged. In California, since 2003, a total of 6,705 length observations, 0 age readings, and 27 otoliths have been collected In Oregon, since 2003, a total of 4,873 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 3,685 length observations, 2,328 age readings, and 677 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 4,233 length observations, 2,508 age readings, and 184 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 777 length observations, 0 age readings, and 764 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 680 & 61 & 740 & 404 & 751\\
C & 1981 & 1783 & 24 & 1801 & 803 & 1622\\
C & 1982 & 4119 & 67 & 4185 & 3247 & 2931\\
C & 1983 & 2784 & 113 & 2886 & 2706 & 3047\\
C & 1984 & 3090 & 47 & 3137 & 3050 & 3302\\
C & 1985 & 4449 & 92 & 4541 & 4364 & 2409\\
C & 1986 & 3147 & 160 & 3307 & 2904 & 3032\\
C & 1987 & 3012 & 17 & 3028 & 2910 & 2253\\
C & 1988 & 2318 & 18 & 2336 & 2123 & 2232\\
C & 1989 & 2758 & 47 & 2782 & 2649 & 2579\\
C & 1990 & 3334 & 113 & 3447 & 2987 & 3186\\
C & 1991 & 2521 & 179 & 2700 & 2256 & 2218\\
C & 1992 & 1947 & 665 & 2612 & 936 & 1513\\
C & 1993 & 1895 & 636 & 2531 & 564 & 1548\\
C & 1994 & 1081 & 1488 & 2569 & 629 & 1146\\
C & 1995 & 1734 & 602 & 2336 & 235 & 1534\\
C & 1996 & 1619 & 566 & 2185 & 1131 & 1203\\
C & 1997 & 2230 & 590 & 2820 & 1157 & 2242\\
C & 1998 & 1740 & 407 & 2147 & 1327 & 500\\
C & 1999 & 1581 & 184 & 1765 & 1159 & 1372\\
C & 2000 & 925 & 119 & 1044 & 562 & 571\\
C & 2001 & 485 & 43 & 528 & 211 & 182\\
C & 2002 & 369 & 80 & 449 & 333 & 333\\
C & 2003 & 233 & 21 & 254 & 87 & 80\\
C & 2004 & 115 & 0 & 115 & 96 & 19\\
C & 2005 & 30 & 1 & 31 & 0 & 30\\
C & 2006 & 102 & 13 & 115 & 79 & 145\\
C & 2007 & 103 & 3 & 106 & 63 & 83\\
C & 2008 & 179 & 17 & 196 & 169 & 356\\
C & 2009 & 255 & 89 & 344 & 194 & 409\\
C & 2010 & 204 & 108 & 311 & 90 & 190\\
C & 2011 & 5 & 44 & 49 & 0 & 5\\
C & 2012 & 103 & 42 & 145 & 0 & 96\\
C & 2013 & 119 & 136 & 255 & 0 & 93\\
C & 2014 & 80 & 297 & 377 & 0 & 78\\
C & 2015 & 130 & 178 & 252 & 0 & 132\\
C & 2016 & 57 & 125 & 182 & 0 & 35\\
C & 2017 & 451 & 104 & 555 & 0 & 326\\
C & 2018 & 1069 & 118 & 1187 & 0 & 115\\
C & 2019 & 622 & 119 & 741 & 0 & 0\\
C & 2020 & 436 & 404 & 837 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1981 & 2026 & 0 & 2026 & 51 & 1975\\
O & 1982 & 4749 & 3 & 4752 & 0 & 4752\\
O & 1983 & 799 & 0 & 799 & 0 & 799\\
O & 1984 & 3919 & 0 & 3919 & 0 & 3919\\
O & 1985 & 6204 & 0 & 6204 & 0 & 6204\\
O & 1986 & 4213 & 0 & 4213 & 0 & 4213\\
O & 1987 & 3063 & 0 & 3063 & 0 & 3063\\
O & 1988 & 2155 & 0 & 2155 & 0 & 2155\\
O & 1989 & 2942 & 0 & 2942 & 28 & 2914\\
O & 1990 & 2774 & 0 & 2774 & 2757 & 17\\
O & 1991 & 3011 & 0 & 3011 & 2979 & 32\\
O & 1992 & 3744 & 0 & 3744 & 2463 & 1141\\
O & 1993 & 3631 & 0 & 3631 & 2516 & 1092\\
O & 1994 & 2099 & 0 & 2099 & 2097 & 2\\
O & 1995 & 1927 & 5 & 1932 & 1873 & 1\\
O & 1996 & 1700 & 1 & 1701 & 1657 & 2\\
O & 1997 & 2729 & 1 & 2729 & 2516 & 154\\
O & 1998 & 2200 & 0 & 2200 & 1278 & 899\\
O & 1999 & 2911 & 1 & 2912 & 1935 & 841\\
O & 2000 & 2063 & 14 & 2077 & 642 & 1434\\
O & 2001 & 1618 & 0 & 1618 & 1469 & 48\\
O & 2002 & 558 & 0 & 558 & 531 & 27\\
O & 2004 & 198 & 0 & 198 & 0 & 198\\
O & 2005 & 48 & 0 & 48 & 0 & 48\\
O & 2006 & 590 & 13 & 603 & 48 & 555\\
O & 2007 & 681 & 23 & 704 & 221 & 432\\
O & 2008 & 1030 & 0 & 1029 & 724 & 218\\
O & 2009 & 937 & 0 & 937 & 744 & 192\\
O & 2010 & 1376 & 0 & 1376 & 1220 & 74\\
O & 2011 & 1510 & 2 & 1512 & 707 & 728\\
O & 2012 & 1515 & 1 & 1515 & 408 & 1049\\
O & 2013 & 1546 & 0 & 1546 & 520 & 1025\\
O & 2014 & 1672 & 0 & 1672 & 509 & 1163\\
O & 2015 & 1590 & 0 & 1590 & 349 & 1116\\
O & 2016 & 1385 & 1 & 1386 & 377 & 1008\\
O & 2017 & 2455 & 2 & 2457 & 581 & 1786\\
O & 2018 & 2753 & 0 & 2753 & 798 & 1143\\
O & 2019 & 2613 & 1 & 2614 & 0 & 1875\\
O & 2020 & 1540 & 1 & 1541 & 0 & 1325\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 1900 & 2 & 1902 & 0 & 1775\\
W & 1981 & 3100 & 0 & 3100 & 0 & 3050\\
W & 1982 & 4000 & 100 & 4100 & 0 & 3944\\
W & 1983 & 2500 & 0 & 2500 & 0 & 2480\\
W & 1984 & 2198 & 1 & 2199 & 0 & 2194\\
W & 1985 & 1600 & 0 & 1600 & 0 & 1591\\
W & 1986 & 2648 & 2 & 2650 & 0 & 2594\\
W & 1987 & 1941 & 1 & 1942 & 0 & 1940\\
W & 1988 & 1050 & 0 & 1050 & 0 & 993\\
W & 1989 & 1500 & 0 & 1499 & 0 & 1494\\
W & 1990 & 2050 & 0 & 2050 & 0 & 2047\\
W & 1991 & 1747 & 0 & 1747 & 0 & 1739\\
W & 1992 & 1550 & 0 & 1550 & 0 & 1547\\
W & 1993 & 1799 & 1 & 1800 & 0 & 1798\\
W & 1994 & 1491 & 1 & 1492 & 1399 & 0\\
W & 1995 & 1650 & 0 & 1650 & 1650 & 0\\
W & 1996 & 1349 & 5 & 1354 & 1348 & 0\\
W & 1997 & 1499 & 8 & 1507 & 1498 & 0\\
W & 1998 & 1259 & 6 & 1265 & 1099 & 0\\
W & 1999 & 1499 & 3 & 1502 & 1450 & 0\\
W & 2000 & 1050 & 1 & 1051 & 1048 & 0\\
W & 2001 & 551 & 1 & 552 & 485 & 0\\
W & 2002 & 635 & 1 & 636 & 587 & 0\\
W & 2003 & 256 & 4 & 260 & 208 & 0\\
W & 2004 & 521 & 1 & 522 & 520 & 0\\
W & 2005 & 448 & 1 & 449 & 449 & 0\\
W & 2006 & 418 & 100 & 518 & 156 & 261\\
W & 2007 & 669 & 143 & 812 & 221 & 395\\
W & 2008 & 826 & 386 & 1212 & 290 & 435\\
W & 2009 & 1048 & 321 & 1369 & 745 & 244\\
W & 2010 & 844 & 349 & 1193 & 531 & 248\\
W & 2011 & 443 & 250 & 693 & 384 & 0\\
W & 2012 & 821 & 340 & 1161 & 310 & 0\\
W & 2013 & 571 & 154 & 725 & 311 & 0\\
W & 2014 & 1123 & 100 & 1223 & 618 & 0\\
W & 2015 & 809 & 250 & 1058 & 652 & 0\\
W & 2016 & 621 & 50 & 671 & 547 & 0\\
W & 2017 & 880 & 0 & 880 & 854 & 0\\
W & 2018 & 562 & 0 & 562 & 411 & 0\\
W & 2019 & 1090 & 0 & 1090 & 0 & 0\\
W & 2020 & 534 & 0 & 534 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 557 & 557 & 0 & 0\\
C & 2005 & 0 & 171 & 171 & 0 & 0\\
C & 2006 & 0 & 325 & 325 & 0 & 0\\
C & 2007 & 0 & 461 & 461 & 0 & 0\\
C & 2008 & 0 & 338 & 338 & 0 & 0\\
C & 2009 & 0 & 99 & 99 & 0 & 0\\
C & 2010 & 0 & 53 & 53 & 0 & 0\\
C & 2011 & 0 & 95 & 95 & 0 & 0\\
C & 2012 & 0 & 343 & 343 & 0 & 0\\
C & 2013 & 0 & 916 & 916 & 0 & 0\\
C & 2014 & 0 & 918 & 918 & 0 & 0\\
C & 2015 & 0 & 342 & 342 & 0 & 0\\
C & 2016 & 0 & 136 & 136 & 0 & 0\\
C & 2017 & 0 & 359 & 359 & 0 & 0\\
C & 2018 & 0 & 715 & 715 & 0 & 24\\
C & 2019 & 0 & 871 & 871 & 0 & 3\\
C & 2020 & 0 & 6 & 6 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 237 & 237 & 0 & 0\\
O & 2002 & 0 & 260 & 260 & 0 & 0\\
O & 2003 & 0 & 351 & 351 & 0 & 0\\
O & 2004 & 0 & 124 & 124 & 0 & 0\\
O & 2005 & 0 & 393 & 393 & 0 & 0\\
O & 2006 & 0 & 179 & 179 & 0 & 0\\
O & 2007 & 0 & 193 & 193 & 0 & 0\\
O & 2008 & 0 & 230 & 230 & 0 & 0\\
O & 2009 & 0 & 182 & 182 & 0 & 0\\
O & 2010 & 0 & 119 & 119 & 0 & 0\\
O & 2011 & 0 & 149 & 149 & 0 & 0\\
O & 2012 & 0 & 276 & 276 & 0 & 0\\
O & 2013 & 0 & 268 & 268 & 0 & 0\\
O & 2014 & 0 & 159 & 159 & 0 & 0\\
O & 2015 & 0 & 216 & 216 & 0 & 0\\
O & 2016 & 0 & 64 & 64 & 0 & 0\\
O & 2017 & 0 & 257 & 257 & 0 & 0\\
O & 2018 & 0 & 966 & 966 & 0 & 0\\
O & 2019 & 0 & 726 & 726 & 0 & 0\\
O & 2020 & 0 & 21 & 21 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2003 & 20 & 0 & 20 & 0 & 0\\
W & 2004 & 4 & 1 & 5 & 0 & 0\\
W & 2005 & 13 & 3 & 16 & 0 & 0\\
W & 2006 & 0 & 1 & 1 & 0 & 0\\
W & 2007 & 4 & 0 & 4 & 0 & 0\\
W & 2008 & 7 & 0 & 7 & 0 & 0\\
W & 2009 & 70 & 4 & 74 & 0 & 0\\
W & 2010 & 30 & 3 & 33 & 0 & 0\\
W & 2011 & 8 & 116 & 124 & 0 & 0\\
W & 2012 & 0 & 29 & 29 & 0 & 0\\
W & 2013 & 127 & 19 & 146 & 0 & 0\\
W & 2014 & 405 & 86 & 491 & 405 & 0\\
W & 2015 & 334 & 1 & 335 & 334 & 0\\
W & 2016 & 556 & 5 & 561 & 551 & 5\\
W & 2017 & 777 & 23 & 800 & 776 & 1\\
W & 2018 & 266 & 66 & 332 & 262 & 0\\
W & 2019 & 103 & 32 & 135 & 0 & 103\\
W & 2020 & 459 & 3 & 462 & 0 & 458\\
W & 2021 & 110 & 0 & 110 & 0 & 110\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 18 & 212 & 4 & 216 & 0 & 10\\
2004 & 12 & 84 & 0 & 84 & 43 & 0\\
2005 & 20 & 78 & 0 & 78 & 65 & 2\\
2006 & 26 & 172 & 0 & 172 & 89 & 0\\
2007 & 27 & 91 & 1 & 92 & 83 & 0\\
2008 & 17 & 26 & 0 & 26 & 20 & 6\\
2009 & 32 & 142 & 0 & 142 & 124 & 0\\
2010 & 28 & 240 & 0 & 240 & 116 & 2\\
2011 & 31 & 313 & 0 & 313 & 152 & 0\\
2012 & 32 & 181 & 0 & 181 & 91 & 2\\
2013 & 18 & 361 & 3 & 364 & 246 & 0\\
2014 & 28 & 349 & 0 & 349 & 264 & 0\\
2015 & 21 & 149 & 0 & 149 & 93 & 0\\
2016 & 40 & 888 & 0 & 888 & 556 & 0\\
2017 & 30 & 310 & 0 & 310 & 213 & 1\\
2018 & 34 & 410 & 0 & 410 & 353 & 0\\
2019 & 23 & 219 & 0 & 219 & 0 & 161\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 13 & 56 & 0 & 56 & 0 & 55\\
2005 & 14 & 73 & 2 & 75 & 0 & 74\\
2006 & 15 & 68 & 1 & 68 & 0 & 68\\
2007 & 12 & 43 & 0 & 43 & 0 & 43\\
2008 & 33 & 173 & 3 & 174 & 0 & 173\\
2009 & 14 & 39 & 0 & 39 & 0 & 39\\
2010 & 9 & 15 & 0 & 15 & 0 & 15\\
2011 & 8 & 13 & 0 & 13 & 0 & 13\\
2012 & 3 & 3 & 0 & 3 & 0 & 3\\
2013 & 13 & 16 & 0 & 16 & 0 & 16\\
2014 & 26 & 70 & 0 & 69 & 0 & 70\\
2015 & 12 & 19 & 0 & 19 & 0 & 19\\
2016 & 18 & 39 & 1 & 39 & 0 & 37\\
2017 & 25 & 56 & 0 & 56 & 0 & 50\\
2018 & 25 & 69 & 0 & 69 & 0 & 66\\
2019 & 11 & 21 & 2 & 23 & 0 & 23\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Yelloweye rockfish 
 The most recent assessment of Yelloweye rockfish was a full assessment conducted in 2017. Across available data, Yelloweye rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 9,182 length observations, 2,116 age readings, and 330 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 3,568 length observations, 2,097 age readings, and 207 otoliths that are available to be aged. In California, since 2000, a total of 218 length observations, 0 age readings, and 17 otoliths have been collected In Oregon, since 2000, a total of 994 length observations, 449 age readings, and 190 otoliths have been collected In Washington, since 2000, a total of 2,356 length observations, 1,648 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 3,399 length observations, 52 age readings, and 323 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 2,583 length observations, 52 age readings, and 323 otoliths that are available to be aged. In California, since 2003, a total of 863 length observations, 0 age readings, and 211 otoliths have been collected In Oregon, since 2003, a total of 1,530 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 190 length observations, 52 age readings, and 112 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 824 length observations, 684 age readings, and 139 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 86 length observations, 0 age readings, and 80 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 17 & 18 & 35 & 0 & 17\\
C & 1981 & 5 & 58 & 62 & 0 & 11\\
C & 1982 & 11 & 11 & 22 & 0 & 10\\
C & 1983 & 22 & 21 & 43 & 0 & 12\\
C & 1984 & 18 & 12 & 30 & 0 & 20\\
C & 1985 & 11 & 16 & 27 & 0 & 34\\
C & 1986 & 16 & 7 & 23 & 0 & 4\\
C & 1987 & 26 & 0 & 26 & 0 & 0\\
C & 1988 & 15 & 6 & 21 & 0 & 0\\
C & 1989 & 9 & 42 & 51 & 0 & 0\\
C & 1990 & 9 & 19 & 28 & 0 & 0\\
C & 1991 & 12 & 212 & 224 & 0 & 0\\
C & 1992 & 9 & 484 & 493 & 0 & 0\\
C & 1993 & 6 & 704 & 710 & 0 & 0\\
C & 1994 & 8 & 728 & 736 & 0 & 0\\
C & 1995 & 10 & 368 & 378 & 0 & 0\\
C & 1996 & 50 & 476 & 526 & 0 & 0\\
C & 1997 & 3 & 287 & 290 & 0 & 0\\
C & 1998 & 5 & 57 & 62 & 0 & 0\\
C & 1999 & 23 & 485 & 508 & 0 & 1\\
C & 2000 & 3 & 23 & 26 & 0 & 0\\
C & 2001 & 7 & 125 & 132 & 0 & 2\\
C & 2002 & 3 & 1 & 4 & 0 & 2\\
C & 2003 & 1 & 0 & 1 & 0 & 1\\
C & 2004 & 7 & 0 & 7 & 0 & 7\\
C & 2012 & 1 & 0 & 1 & 0 & 0\\
C & 2013 & 3 & 0 & 3 & 0 & 3\\
C & 2014 & 0 & 1 & 1 & 0 & 0\\
C & 2015 & 1 & 1 & 1 & 0 & 1\\
C & 2017 & 1 & 2 & 3 & 0 & 1\\
C & 2019 & 11 & 0 & 11 & 0 & 0\\
C & 2020 & 24 & 4 & 28 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1992 & 13 & 0 & 13 & 0 & 13\\
O & 1993 & 20 & 0 & 20 & 19 & 1\\
O & 1995 & 73 & 25 & 98 & 0 & 0\\
O & 1996 & 161 & 0 & 161 & 0 & 0\\
O & 1997 & 256 & 0 & 256 & 0 & 0\\
O & 1998 & 118 & 0 & 118 & 0 & 0\\
O & 1999 & 166 & 0 & 166 & 0 & 0\\
O & 2000 & 141 & 0 & 141 & 0 & 0\\
O & 2001 & 233 & 0 & 233 & 23 & 0\\
O & 2002 & 4 & 0 & 4 & 4 & 0\\
O & 2003 & 29 & 0 & 29 & 29 & 0\\
O & 2004 & 8 & 0 & 8 & 4 & 3\\
O & 2005 & 4 & 0 & 4 & 4 & 0\\
O & 2006 & 19 & 1 & 20 & 19 & 0\\
O & 2007 & 1 & 0 & 1 & 0 & 1\\
O & 2008 & 16 & 0 & 16 & 16 & 0\\
O & 2009 & 22 & 0 & 22 & 22 & 0\\
O & 2010 & 2 & 0 & 2 & 2 & 0\\
O & 2011 & 12 & 0 & 12 & 10 & 0\\
O & 2012 & 14 & 0 & 14 & 14 & 0\\
O & 2013 & 13 & 0 & 13 & 13 & 0\\
O & 2014 & 9 & 0 & 9 & 9 & 0\\
O & 2015 & 17 & 0 & 17 & 16 & 1\\
O & 2016 & 16 & 0 & 16 & 16 & 0\\
O & 2017 & 84 & 0 & 84 & 69 & 15\\
O & 2018 & 47 & 0 & 47 & 46 & 1\\
O & 2019 & 215 & 1 & 216 & 133 & 83\\
O & 2020 & 86 & 0 & 86 & 0 & 86\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 0 & 4 & 4 & 0 & 0\\
W & 1982 & 14 & 0 & 14 & 0 & 0\\
W & 1996 & 0 & 266 & 266 & 0 & 0\\
W & 1997 & 0 & 118 & 118 & 0 & 0\\
W & 1998 & 25 & 15 & 40 & 0 & 0\\
W & 1999 & 17 & 28 & 45 & 0 & 0\\
W & 2000 & 18 & 343 & 361 & 0 & 0\\
W & 2001 & 325 & 490 & 813 & 493 & 0\\
W & 2002 & 266 & 4 & 270 & 270 & 0\\
W & 2003 & 29 & 0 & 29 & 20 & 0\\
W & 2004 & 78 & 0 & 78 & 76 & 0\\
W & 2006 & 152 & 3 & 152 & 155 & 0\\
W & 2007 & 32 & 0 & 32 & 32 & 0\\
W & 2008 & 2 & 0 & 2 & 2 & 0\\
W & 2009 & 23 & 0 & 23 & 22 & 0\\
W & 2010 & 54 & 0 & 54 & 54 & 0\\
W & 2011 & 16 & 1 & 17 & 14 & 0\\
W & 2012 & 30 & 36 & 66 & 66 & 0\\
W & 2013 & 42 & 0 & 42 & 42 & 0\\
W & 2014 & 18 & 0 & 18 & 18 & 0\\
W & 2015 & 61 & 0 & 61 & 61 & 0\\
W & 2016 & 11 & 0 & 11 & 11 & 0\\
W & 2017 & 29 & 1 & 30 & 28 & 0\\
W & 2018 & 123 & 1 & 124 & 122 & 0\\
W & 2019 & 170 & 1 & 171 & 162 & 0\\
W & 2020 & 2 & 0 & 2 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2004 & 0 & 17 & 17 & 0 & 0\\
C & 2005 & 0 & 60 & 60 & 0 & 0\\
C & 2006 & 0 & 95 & 95 & 0 & 0\\
C & 2007 & 0 & 57 & 57 & 0 & 0\\
C & 2008 & 0 & 31 & 31 & 0 & 0\\
C & 2009 & 1 & 57 & 58 & 0 & 0\\
C & 2010 & 1 & 21 & 22 & 0 & 0\\
C & 2011 & 0 & 23 & 23 & 0 & 0\\
C & 2012 & 0 & 26 & 26 & 0 & 0\\
C & 2013 & 0 & 17 & 17 & 0 & 0\\
C & 2014 & 0 & 24 & 24 & 0 & 0\\
C & 2015 & 0 & 42 & 42 & 0 & 0\\
C & 2016 & 0 & 36 & 36 & 0 & 1\\
C & 2017 & 0 & 112 & 112 & 0 & 67\\
C & 2018 & 0 & 115 & 115 & 0 & 70\\
C & 2019 & 0 & 127 & 127 & 0 & 73\\
C & 2020 & 0 & 1 & 1 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 2001 & 0 & 368 & 368 & 0 & 0\\
O & 2002 & 0 & 448 & 448 & 0 & 0\\
O & 2003 & 0 & 492 & 492 & 0 & 0\\
O & 2004 & 0 & 23 & 23 & 0 & 0\\
O & 2005 & 1 & 25 & 26 & 0 & 0\\
O & 2006 & 0 & 49 & 49 & 0 & 0\\
O & 2007 & 0 & 62 & 62 & 0 & 0\\
O & 2008 & 0 & 74 & 74 & 0 & 0\\
O & 2009 & 0 & 39 & 39 & 0 & 0\\
O & 2010 & 0 & 28 & 28 & 0 & 0\\
O & 2011 & 0 & 49 & 49 & 0 & 0\\
O & 2012 & 0 & 112 & 112 & 0 & 0\\
O & 2013 & 0 & 57 & 57 & 0 & 0\\
O & 2014 & 0 & 89 & 89 & 0 & 0\\
O & 2015 & 0 & 42 & 42 & 0 & 0\\
O & 2016 & 0 & 34 & 34 & 0 & 0\\
O & 2017 & 0 & 102 & 102 & 0 & 0\\
O & 2018 & 0 & 120 & 120 & 0 & 0\\
O & 2019 & 0 & 120 & 120 & 0 & 0\\
O & 2020 & 0 & 12 & 12 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2003 & 0 & 2 & 2 & 0 & 0\\
W & 2004 & 12 & 0 & 12 & 10 & 2\\
W & 2005 & 4 & 0 & 4 & 4 & 0\\
W & 2006 & 1 & 0 & 1 & 1 & 0\\
W & 2008 & 6 & 3 & 9 & 6 & 0\\
W & 2010 & 1 & 0 & 1 & 1 & 0\\
W & 2011 & 2 & 0 & 2 & 2 & 0\\
W & 2012 & 3 & 2 & 5 & 3 & 0\\
W & 2014 & 0 & 1 & 1 & 0 & 0\\
W & 2015 & 2 & 0 & 2 & 2 & 0\\
W & 2017 & 5 & 3 & 8 & 5 & 0\\
W & 2018 & 7 & 1 & 8 & 6 & 0\\
W & 2019 & 13 & 13 & 26 & 12 & 1\\
W & 2020 & 7 & 0 & 7 & 0 & 7\\
W & 2021 & 102 & 0 & 102 & 0 & 102\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 19 & 68 & 0 & 68 & 67 & 1\\
2004 & 8 & 21 & 0 & 21 & 21 & 0\\
2005 & 13 & 40 & 0 & 40 & 40 & 0\\
2006 & 13 & 42 & 1 & 43 & 43 & 0\\
2007 & 12 & 24 & 2 & 26 & 19 & 7\\
2008 & 15 & 43 & 0 & 43 & 43 & 0\\
2009 & 11 & 39 & 0 & 39 & 39 & 0\\
2010 & 15 & 52 & 0 & 52 & 52 & 0\\
2011 & 13 & 47 & 0 & 47 & 47 & 0\\
2012 & 14 & 44 & 0 & 44 & 44 & 0\\
2013 & 14 & 38 & 0 & 38 & 35 & 2\\
2014 & 19 & 92 & 0 & 92 & 92 & 0\\
2015 & 13 & 51 & 0 & 51 & 51 & 0\\
2016 & 25 & 91 & 0 & 91 & 91 & 0\\
2017 & 19 & 45 & 0 & 45 & 0 & 45\\
2018 & 24 & 50 & 0 & 50 & 0 & 50\\
2019 & 9 & 34 & 0 & 34 & 0 & 34\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 1 & 1 & 0 & 1 & 0 & 1\\
2006 & 1 & 1 & 0 & 1 & 0 & 1\\
2007 & 3 & 3 & 0 & 3 & 0 & 3\\
2009 & 2 & 5 & 0 & 5 & 0 & 5\\
2010 & 2 & 2 & 0 & 2 & 0 & 2\\
2012 & 1 & 1 & 0 & 1 & 0 & 1\\
2015 & 7 & 13 & 0 & 13 & 0 & 13\\
2016 & 7 & 15 & 0 & 15 & 0 & 10\\
2017 & 7 & 15 & 0 & 15 & 0 & 15\\
2018 & 8 & 13 & 0 & 13 & 0 & 13\\
2019 & 11 & 17 & 0 & 17 & 0 & 16\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Yellowmouth rockfish 
 The most recent assessment of Yellowmouth rockfish was a data-limited assessment conducted in 2010. Across available data, Yellowmouth rockfish have been observed and sampled generally by  
 
 Across all years of available data, commercial fisheries have collected a total of 4,091 length observations, 1 age readings, and 2,360 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 2,631 length observations, 1 age readings, and 2,169 otoliths that are available to be aged. In California, since 2000, a total of 23 length observations, 0 age readings, and 0 otoliths have been collected In Oregon, since 2000, a total of 2,183 length observations, 0 age readings, and 2,169 otoliths have been collected In Washington, since 2000, a total of 425 length observations, 1 age readings, and 0 otoliths have been collected 



## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1983 & 5 & 0 & 5 & 0 & 6\\
C & 1984 & 6 & 0 & 6 & 0 & 6\\
C & 1985 & 13 & 3 & 16 & 0 & 12\\
C & 1986 & 18 & 0 & 18 & 0 & 0\\
C & 1987 & 3 & 0 & 3 & 0 & 0\\
C & 1989 & 1 & 0 & 1 & 0 & 0\\
C & 1990 & 2 & 0 & 2 & 0 & 0\\
C & 1992 & 1 & 34 & 35 & 0 & 0\\
C & 1993 & 2 & 10 & 12 & 0 & 0\\
C & 1994 & 1 & 0 & 1 & 0 & 0\\
C & 1996 & 5 & 0 & 5 & 0 & 0\\
C & 1997 & 0 & 20 & 20 & 0 & 0\\
C & 1998 & 1 & 0 & 1 & 0 & 0\\
C & 2006 & 1 & 0 & 1 & 0 & 0\\
C & 2007 & 1 & 0 & 1 & 0 & 0\\
C & 2009 & 0 & 1 & 1 & 0 & 0\\
C & 2012 & 0 & 1 & 1 & 0 & 0\\
C & 2016 & 0 & 1 & 1 & 0 & 0\\
C & 2019 & 0 & 2 & 2 & 0 & 0\\
C & 2020 & 16 & 20 & 16 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1981 & 107 & 0 & 107 & 0 & 107\\
O & 1982 & 60 & 0 & 60 & 0 & 60\\
O & 1995 & 56 & 0 & 56 & 0 & 0\\
O & 1996 & 226 & 0 & 226 & 0 & 0\\
O & 1997 & 100 & 0 & 100 & 0 & 0\\
O & 1998 & 41 & 0 & 41 & 0 & 0\\
O & 2001 & 16 & 0 & 16 & 0 & 16\\
O & 2004 & 93 & 0 & 93 & 0 & 93\\
O & 2005 & 61 & 0 & 61 & 0 & 61\\
O & 2006 & 45 & 0 & 45 & 0 & 45\\
O & 2007 & 213 & 0 & 213 & 0 & 213\\
O & 2008 & 95 & 0 & 95 & 0 & 95\\
O & 2009 & 271 & 0 & 271 & 0 & 269\\
O & 2010 & 104 & 0 & 104 & 0 & 104\\
O & 2011 & 44 & 0 & 44 & 0 & 44\\
O & 2012 & 114 & 0 & 114 & 0 & 114\\
O & 2013 & 162 & 0 & 162 & 0 & 162\\
O & 2014 & 85 & 0 & 85 & 0 & 85\\
O & 2015 & 129 & 0 & 129 & 0 & 129\\
O & 2016 & 75 & 0 & 75 & 0 & 75\\
O & 2017 & 109 & 0 & 109 & 0 & 109\\
O & 2018 & 163 & 0 & 163 & 0 & 163\\
O & 2019 & 225 & 0 & 225 & 0 & 225\\
O & 2020 & 179 & 0 & 179 & 0 & 167\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1996 & 0 & 312 & 312 & 0 & 0\\
W & 1997 & 0 & 307 & 307 & 0 & 0\\
W & 1998 & 23 & 7 & 30 & 0 & 0\\
W & 1999 & 93 & 3 & 96 & 0 & 0\\
W & 2000 & 11 & 0 & 11 & 0 & 0\\
W & 2001 & 3 & 1 & 4 & 0 & 0\\
W & 2002 & 1 & 0 & 1 & 0 & 0\\
W & 2003 & 3 & 2 & 5 & 0 & 0\\
W & 2004 & 4 & 0 & 4 & 0 & 0\\
W & 2005 & 1 & 0 & 1 & 0 & 0\\
W & 2007 & 7 & 0 & 7 & 0 & 0\\
W & 2008 & 57 & 0 & 57 & 0 & 0\\
W & 2011 & 1 & 0 & 1 & 1 & 0\\
W & 2012 & 6 & 0 & 6 & 0 & 0\\
W & 2013 & 21 & 0 & 21 & 0 & 0\\
W & 2014 & 7 & 0 & 7 & 0 & 0\\
W & 2015 & 26 & 0 & 26 & 0 & 0\\
W & 2016 & 18 & 0 & 18 & 0 & 0\\
W & 2017 & 9 & 0 & 9 & 0 & 0\\
W & 2018 & 29 & 0 & 29 & 0 & 0\\
W & 2019 & 218 & 0 & 218 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  
# Yellowtail rockfish 
 The most recent assessment of Yellowtail rockfish was a full assessment conducted in 2017. Across available data, Yellowtail rockfish have been observed and sampled generally by both commercial and recreational fisheries and the NWFSC WCGBT and HKL surveys.  
 
 Across all years of available data, commercial fisheries have collected a total of 199,193 length observations, 139,121 age readings, and 30,622 otoliths that are available to be aged. Since 2000, commercial fisheries have collected a 80,311 length observations, 54,916 age readings, and 7,464 otoliths that are available to be aged. In California, since 2000, a total of 4,241 length observations, 802 age readings, and 1,404 otoliths have been collected In Oregon, since 2000, a total of 39,043 length observations, 26,044 age readings, and 6,060 otoliths have been collected In Washington, since 2000, a total of 37,027 length observations, 28,070 age readings, and 0 otoliths have been collected 

Across all years of available data, recreational fisheries have collected a total of 110,549 length observations, 5,775 age readings, and 1,236 otoliths that are available to be aged. Since 2003, recreational fisheries have collected a 107,676 length observations, 5,775 age readings, and 1,224 otoliths that are available to be aged. In California, since 2003, a total of 60,278 length observations, 0 age readings, and 217 otoliths have been collected In Oregon, since 2003, a total of 34,567 length observations, 0 age readings, and 0 otoliths have been collected In Washington, since 2003, a total of 12,831 length observations, 5,775 age readings, and 1,007 otoliths have been collected 

Across all years of available data, the NWFSC WCGBT survey has collected a total of 16,040 length observations, 5,193 age readings, and 2,973 otoliths that are available to be aged. Across all years of available data, the NWFSC HKL survey has collected a total of 1,531 length observations, 124 age readings, and 1,192 otoliths that are available to be aged. 

## commercial fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 1980 & 138 & 22 & 160 & 127 & 103\\
C & 1981 & 175 & 148 & 323 & 174 & 194\\
C & 1982 & 355 & 27 & 382 & 276 & 262\\
C & 1983 & 536 & 47 & 576 & 474 & 1033\\
C & 1984 & 895 & 55 & 950 & 846 & 1517\\
C & 1985 & 780 & 366 & 1146 & 642 & 883\\
C & 1986 & 708 & 81 & 788 & 664 & 624\\
C & 1987 & 250 & 79 & 329 & 162 & 781\\
C & 1988 & 316 & 6 & 321 & 177 & 302\\
C & 1989 & 700 & 201 & 901 & 687 & 698\\
C & 1990 & 425 & 167 & 592 & 400 & 346\\
C & 1991 & 555 & 415 & 970 & 528 & 515\\
C & 1992 & 679 & 2660 & 3339 & 529 & 537\\
C & 1993 & 257 & 1806 & 2063 & 141 & 233\\
C & 1994 & 364 & 2793 & 3157 & 355 & 441\\
C & 1995 & 382 & 837 & 1219 & 167 & 146\\
C & 1996 & 659 & 830 & 1489 & 576 & 575\\
C & 1997 & 385 & 758 & 1143 & 245 & 247\\
C & 1998 & 474 & 870 & 1344 & 169 & 341\\
C & 1999 & 407 & 251 & 658 & 251 & 253\\
C & 2000 & 152 & 151 & 303 & 35 & 33\\
C & 2001 & 192 & 161 & 353 & 179 & 180\\
C & 2002 & 100 & 9 & 109 & 71 & 91\\
C & 2003 & 55 & 20 & 75 & 0 & 59\\
C & 2004 & 64 & 31 & 95 & 32 & 96\\
C & 2005 & 74 & 34 & 108 & 78 & 160\\
C & 2006 & 97 & 86 & 183 & 93 & 186\\
C & 2007 & 121 & 75 & 196 & 0 & 0\\
C & 2008 & 82 & 37 & 119 & 72 & 150\\
C & 2009 & 22 & 105 & 127 & 6 & 6\\
C & 2010 & 4 & 3 & 7 & 4 & 4\\
C & 2011 & 58 & 16 & 74 & 26 & 29\\
C & 2012 & 41 & 123 & 162 & 28 & 28\\
C & 2013 & 12 & 207 & 218 & 12 & 12\\
C & 2014 & 111 & 154 & 243 & 110 & 111\\
C & 2015 & 117 & 156 & 242 & 56 & 56\\
C & 2016 & 146 & 105 & 251 & 0 & 38\\
C & 2017 & 267 & 153 & 420 & 0 & 27\\
C & 2018 & 276 & 119 & 395 & 0 & 0\\
C & 2019 & 277 & 88 & 363 & 0 & 138\\
C & 2020 & 181 & 17 & 198 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1981 & 607 & 0 & 607 & 0 & 607\\
O & 1982 & 1499 & 0 & 1499 & 0 & 1397\\
O & 1983 & 199 & 0 & 199 & 0 & 199\\
O & 1984 & 1099 & 0 & 1098 & 0 & 1099\\
O & 1985 & 2104 & 0 & 2104 & 0 & 2004\\
O & 1986 & 1156 & 0 & 1156 & 0 & 1004\\
O & 1987 & 1891 & 0 & 1891 & 0 & 1891\\
O & 1988 & 1670 & 0 & 1670 & 0 & 1670\\
O & 1989 & 2055 & 0 & 2055 & 0 & 2055\\
O & 1990 & 1802 & 0 & 1802 & 1792 & 10\\
O & 1991 & 1296 & 0 & 1296 & 1289 & 7\\
O & 1992 & 2490 & 0 & 2490 & 2424 & 66\\
O & 1993 & 2022 & 0 & 2022 & 1981 & 3\\
O & 1994 & 2641 & 0 & 2641 & 2637 & 4\\
O & 1995 & 2242 & 0 & 2242 & 2203 & 9\\
O & 1996 & 2259 & 0 & 2259 & 2161 & 47\\
O & 1997 & 4093 & 0 & 4092 & 3735 & 32\\
O & 1998 & 3250 & 0 & 3235 & 2263 & 915\\
O & 1999 & 3577 & 1 & 3578 & 3383 & 9\\
O & 2000 & 3005 & 3 & 3006 & 2863 & 20\\
O & 2001 & 2832 & 0 & 2832 & 2749 & 26\\
O & 2002 & 1536 & 2 & 1538 & 1470 & 66\\
O & 2003 & 701 & 0 & 701 & 44 & 655\\
O & 2004 & 1341 & 0 & 1341 & 0 & 1331\\
O & 2005 & 916 & 64 & 980 & 522 & 453\\
O & 2006 & 1236 & 15 & 1251 & 350 & 880\\
O & 2007 & 1189 & 7 & 1196 & 376 & 99\\
O & 2008 & 584 & 1 & 585 & 575 & 2\\
O & 2009 & 855 & 0 & 855 & 638 & 9\\
O & 2010 & 1618 & 0 & 1618 & 1080 & 4\\
O & 2011 & 1816 & 0 & 1816 & 1005 & 6\\
O & 2012 & 1914 & 1 & 1915 & 1307 & 74\\
O & 2013 & 1263 & 6 & 1269 & 1103 & 53\\
O & 2014 & 1893 & 5 & 1898 & 1828 & 10\\
O & 2015 & 2391 & 2 & 2393 & 2158 & 20\\
O & 2016 & 3141 & 4 & 3145 & 2202 & 13\\
O & 2017 & 3037 & 3 & 3040 & 2465 & 135\\
O & 2018 & 2948 & 2 & 2950 & 2081 & 146\\
O & 2019 & 2782 & 0 & 2782 & 1228 & 593\\
O & 2020 & 1926 & 6 & 1932 & 0 & 1465\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the commercial fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 1980 & 3800 & 105 & 3905 & 3727 & 0\\
W & 1981 & 3900 & 0 & 3900 & 3741 & 0\\
W & 1982 & 3496 & 0 & 3496 & 3331 & 99\\
W & 1983 & 2366 & 0 & 2366 & 2350 & 0\\
W & 1984 & 3200 & 0 & 3200 & 3192 & 0\\
W & 1985 & 3500 & 0 & 3500 & 3498 & 0\\
W & 1986 & 2992 & 0 & 2992 & 2985 & 0\\
W & 1987 & 2096 & 0 & 2046 & 2092 & 0\\
W & 1988 & 1650 & 0 & 1650 & 1645 & 0\\
W & 1989 & 1650 & 0 & 1650 & 1643 & 0\\
W & 1990 & 1874 & 51 & 1925 & 1872 & 0\\
W & 1991 & 2296 & 1 & 2297 & 2191 & 0\\
W & 1992 & 2197 & 0 & 2197 & 2193 & 0\\
W & 1993 & 2743 & 0 & 2743 & 2741 & 0\\
W & 1994 & 4406 & 0 & 4406 & 2591 & 0\\
W & 1995 & 4567 & 1 & 4567 & 2962 & 0\\
W & 1996 & 3938 & 6 & 3944 & 2436 & 0\\
W & 1997 & 3316 & 11 & 3327 & 2600 & 0\\
W & 1998 & 2571 & 1 & 2572 & 2559 & 0\\
W & 1999 & 2411 & 2 & 2413 & 2398 & 0\\
W & 2000 & 2737 & 1 & 2738 & 2704 & 0\\
W & 2001 & 2173 & 55 & 2228 & 2226 & 0\\
W & 2002 & 1660 & 3 & 1663 & 1654 & 0\\
W & 2003 & 1942 & 5 & 1947 & 1941 & 0\\
W & 2004 & 2084 & 5 & 2087 & 2059 & 0\\
W & 2005 & 1173 & 0 & 1173 & 1169 & 0\\
W & 2006 & 899 & 69 & 968 & 749 & 0\\
W & 2007 & 1610 & 905 & 2515 & 1397 & 0\\
W & 2008 & 1499 & 342 & 1841 & 1077 & 0\\
W & 2009 & 1174 & 508 & 1682 & 1172 & 0\\
W & 2010 & 1059 & 651 & 1710 & 977 & 0\\
W & 2011 & 1093 & 435 & 1528 & 924 & 0\\
W & 2012 & 1741 & 700 & 2441 & 1489 & 0\\
W & 2013 & 1023 & 300 & 1323 & 797 & 0\\
W & 2014 & 1158 & 152 & 1309 & 713 & 0\\
W & 2015 & 1058 & 703 & 1760 & 900 & 0\\
W & 2016 & 1321 & 266 & 1587 & 1184 & 0\\
W & 2017 & 2667 & 0 & 2667 & 1771 & 0\\
W & 2018 & 1898 & 0 & 1898 & 1391 & 0\\
W & 2019 & 1411 & 0 & 1411 & 1279 & 0\\
W & 2020 & 550 & 2 & 551 & 497 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
## recreational fisheries 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in California.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in California. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
C & 2003 & 0 & 41 & 41 & 0 & 0\\
C & 2004 & 1 & 919 & 920 & 0 & 0\\
C & 2005 & 0 & 881 & 881 & 0 & 0\\
C & 2006 & 0 & 2001 & 2001 & 0 & 0\\
C & 2007 & 0 & 4780 & 4780 & 0 & 0\\
C & 2008 & 0 & 2032 & 2032 & 0 & 0\\
C & 2009 & 0 & 3534 & 3534 & 0 & 0\\
C & 2010 & 0 & 2192 & 2192 & 0 & 0\\
C & 2011 & 0 & 4718 & 4718 & 0 & 0\\
C & 2012 & 2 & 5641 & 5643 & 0 & 0\\
C & 2013 & 0 & 6754 & 6753 & 0 & 0\\
C & 2014 & 0 & 6354 & 6354 & 0 & 0\\
C & 2015 & 0 & 6980 & 6980 & 0 & 0\\
C & 2016 & 0 & 2738 & 2738 & 0 & 0\\
C & 2017 & 0 & 3209 & 3209 & 0 & 0\\
C & 2018 & 0 & 3375 & 3374 & 0 & 96\\
C & 2019 & 0 & 4126 & 4125 & 0 & 121\\
C & 2020 & 0 & 3 & 3 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Oregon. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
O & 1999 & 326 & 0 & 326 & 0 & 0\\
O & 2000 & 192 & 0 & 192 & 0 & 0\\
O & 2001 & 12 & 692 & 704 & 0 & 12\\
O & 2002 & 0 & 1457 & 1457 & 0 & 0\\
O & 2003 & 0 & 1740 & 1740 & 0 & 0\\
O & 2004 & 1 & 1394 & 1395 & 0 & 0\\
O & 2005 & 0 & 1916 & 1916 & 0 & 0\\
O & 2006 & 0 & 1585 & 1585 & 0 & 0\\
O & 2007 & 0 & 1721 & 1721 & 0 & 0\\
O & 2008 & 0 & 2043 & 2043 & 0 & 0\\
O & 2009 & 0 & 2678 & 2678 & 0 & 0\\
O & 2010 & 0 & 2462 & 2462 & 0 & 0\\
O & 2011 & 0 & 2318 & 2318 & 0 & 0\\
O & 2012 & 1 & 2753 & 2754 & 0 & 0\\
O & 2013 & 0 & 2182 & 2182 & 0 & 0\\
O & 2014 & 0 & 1936 & 1936 & 0 & 0\\
O & 2015 & 0 & 2018 & 2018 & 0 & 0\\
O & 2016 & 0 & 870 & 870 & 0 & 0\\
O & 2017 & 0 & 1347 & 1347 & 0 & 0\\
O & 2018 & 0 & 2607 & 2607 & 0 & 0\\
O & 2019 & 0 & 2866 & 2866 & 0 & 0\\
O & 2020 & 0 & 129 & 129 & 0 & 0\\*
\end{longtable}
\endgroup{}
\endgroup{}
\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington.}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the recreational fisheries in Washington. \textit{(continued)}}\\
\toprule
State & Year & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
W & 2002 & 172 & 22 & 194 & 0 & 0\\
W & 2003 & 745 & 55 & 800 & 0 & 0\\
W & 2004 & 606 & 69 & 675 & 0 & 0\\
W & 2005 & 719 & 150 & 869 & 0 & 0\\
W & 2006 & 274 & 88 & 362 & 18 & 0\\
W & 2007 & 235 & 78 & 313 & 0 & 0\\
W & 2008 & 118 & 71 & 189 & 0 & 0\\
W & 2009 & 329 & 135 & 464 & 6 & 1\\
W & 2010 & 148 & 70 & 218 & 17 & 0\\
W & 2011 & 55 & 336 & 391 & 15 & 0\\
W & 2012 & 92 & 136 & 228 & 0 & 0\\
W & 2013 & 234 & 122 & 356 & 0 & 4\\
W & 2014 & 533 & 147 & 680 & 533 & 0\\
W & 2015 & 624 & 59 & 683 & 624 & 0\\
W & 2016 & 836 & 92 & 928 & 836 & 0\\
W & 2017 & 1152 & 176 & 1328 & 1150 & 2\\
W & 2018 & 652 & 256 & 908 & 641 & 4\\
W & 2019 & 1311 & 489 & 1800 & 1306 & 5\\
W & 2020 & 883 & 8 & 891 & 629 & 253\\
W & 2021 & 738 & 10 & 748 & 0 & 738\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC WCGBT 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC WCGBT survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2003 & 35 & 815 & 34 & 849 & 275 & 0\\
2004 & 26 & 626 & 0 & 626 & 187 & 1\\
2005 & 45 & 1047 & 0 & 1047 & 348 & 2\\
2006 & 35 & 386 & 0 & 386 & 169 & 0\\
2007 & 45 & 946 & 12 & 958 & 277 & 2\\
2008 & 37 & 682 & 8 & 690 & 355 & 0\\
2009 & 40 & 457 & 0 & 457 & 333 & 0\\
2010 & 46 & 1130 & 1 & 1131 & 486 & 432\\
2011 & 48 & 788 & 0 & 788 & 469 & 0\\
2012 & 46 & 906 & 1 & 907 & 354 & 0\\
2013 & 30 & 407 & 0 & 407 & 177 & 0\\
2014 & 55 & 1527 & 1 & 1528 & 651 & 1\\
2015 & 59 & 820 & 0 & 820 & 541 & 1\\
2016 & 80 & 2261 & 0 & 2261 & 571 & 837\\
2017 & 71 & 1702 & 0 & 1701 & 0 & 724\\
2018 & 61 & 916 & 0 & 916 & 0 & 628\\
2019 & 32 & 568 & 0 & 568 & 0 & 345\\*
\end{longtable}
\endgroup{}
\endgroup{}
## NWFSC HKL 
 \begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}>{\raggedright\arraybackslash}p{1.57cm}}
\caption{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey.}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endfirsthead
\caption[]{(\#tab:tab-label)Data collected annually from the NWFSC HKL survey. \textit{(continued)}}\\
\toprule
Year & Positive Sites/Tows & Sexed Fish & Unsexed Fish & Lengths & Ages & Otoliths\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
2004 & 14 & 126 & 3 & 126 & 124 & 0\\
2005 & 14 & 107 & 25 & 122 & 0 & 122\\
2006 & 6 & 88 & 3 & 88 & 0 & 88\\
2007 & 18 & 119 & 12 & 119 & 0 & 119\\
2008 & 15 & 139 & 2 & 139 & 0 & 139\\
2009 & 16 & 79 & 6 & 80 & 0 & 80\\
2010 & 12 & 60 & 3 & 60 & 0 & 0\\
2011 & 13 & 78 & 52 & 126 & 0 & 1\\
2012 & 11 & 107 & 5 & 106 & 0 & 106\\
2013 & 13 & 93 & 6 & 96 & 0 & 95\\
2014 & 17 & 105 & 9 & 110 & 0 & 110\\
2015 & 13 & 78 & 2 & 78 & 0 & 78\\
2016 & 14 & 87 & 3 & 89 & 0 & 87\\
2017 & 12 & 46 & 0 & 46 & 0 & 40\\
2018 & 12 & 76 & 4 & 77 & 0 & 66\\
2019 & 13 & 67 & 4 & 69 & 0 & 61\\*
\end{longtable}
\endgroup{}
\endgroup{}
  
  





<!--chapter:end:20data.Rmd-->

# Maturity data

Maturity samples for a wide range of West Coast groundfish specie shave been across a range of sources: NWFSC WCGBT survey, NWFSC HKL survey, Pacific hake survey, at-sea sampling of the Pacific hake fishery, and port sampling in Oregon and Washington.  Samples have been collected between 2009 - 2019. A summary of maturity samples collected and read maturity samples by species is provided below.  

The following summary does not include collection from the 2021 NWFSC WCGBT or HKL surveys.  Addtionally, the data summary has not been updated to reflect maturity reading efforts conducted in 2021 (e.g., Dover sole, copper rockfish). 

\begingroup\fontsize{10}{12}\selectfont
\begingroup\fontsize{10}{12}\selectfont

\begin{longtable}[t]{l>{\raggedright\arraybackslash}p{2cm}>{\raggedright\arraybackslash}p{2cm}}
\caption{(\#tab:maturity)Summary of collected and read maturity samples by species.}\\
\toprule
Species & Total Collected & Total Read\\
\midrule
\endfirsthead
\caption[]{(\#tab:maturity)Summary of collected and read maturity samples by species. \textit{(continued)}}\\
\toprule
Species & Total Collected & Total Read\\
\midrule
\endhead

\endfoot
\bottomrule
\endlastfoot
Arrowtooth flounder & 217 & 0\\
Aurora rockfish & 567 & 567\\
Bank rockfish & 432 & 62\\
Big skate & 180 & 180\\
Black rockfish & 599 & 599\\
Blackgill rockfish & 126 & 126\\
Blue rockfish & 155 & 0\\
Bocaccio & 837 & 737\\
Brown rockfish & 12 & 0\\
Canary rockfish & 1179 & 1169\\
Chilipepper rockfish & 157 & 157\\
Copper rockfish & 160 & 49\\
Cowcod & 217 & 102\\
Darkblotched rockfish & 898 & 898\\
Deacon rockfish & 23 & 0\\
Dover sole & 573 & 106\\
Greenspotted rockfish & 175 & 175\\
Greenstriped rockfish & 73 & 73\\
Kelp greenling & 8 & 8\\
Lingcod & 1325 & 760\\
Longnose skate & 508 & 508\\
Longspine thornyhead & 31 & 0\\
Mexico rockfish & 1 & 1\\
Olive rockfish & 1 & 1\\
Pacific cod & 51 & 0\\
Pacific hake & 3914 & 2947\\
Pacific ocean perch & 583 & 583\\
Petrale sole & 545 & 394\\
Rosy rockfish & 59 & 39\\
Rougheye rockfish & 90 & 86\\
Sablefish & 978 & 876\\
Shortspine thornyhead & 1350 & 591\\
Speckeled rockfish & 401 & 301\\
Squarespot rockfish & 118 & 118\\
Stripedtail rockfish & 67 & 67\\
Swordspine rockfish & 89 & 89\\
Vermilion and sunset rockfish & 1481 & 1139\\
Widow rockfish & 306 & 50\\
Yelloweye rockfish & 221 & 97\\
Yellowtail rockfish & 726 & 468\\*
\end{longtable}
\endgroup{}
\endgroup{}

<!--chapter:end:30maturity.Rmd-->

\newpage

# Acknowledgement

We would like to thank Theresa Tsou (WDFW), Alison Whitman (ODFW), and Traci Larinto (CDFW) for providing data that were essential in producing data summaries.  We are appreciative of Brad Stenberg (PSMFC) and Jason Edwards (PSMFC) being responsive to our questions regarding both commercial and recreational data. We appreciate Melissa Head providing maturity sample collections. Finally, we would like to thank the NWFSC Fisheries Resource Analysis and Monitoring survey team and their dedication to collecting high quality data. The data they collect is essential to the assessment of West Coast groundfish stocks.

<!--chapter:end:40acknowledgments.Rmd-->


<!--chapter:end:49bibliography.Rmd-->

