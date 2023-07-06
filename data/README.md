# Data for: Unreviewed science in the news
*Juan Pablo Alperin, Kenneth Shores, Alice Fleerackers*

Last updated: July 1, 2023

The dataset that accompanies this code should be placed inside this folder. 

The dataset contains the News and Media mentions identified by Altmetric.com for publications indexed in Web of Science and in four preprint servers (arXiv, bioRxiv, medRxiv, and SSRN). The seed data was queried from the snapshots of both the Web of Science and Altmetric found at the Observatoire des Sciences et des Technologies (OST). The data included here has been filtered to only include mentions of the research that was made in 94 outlets, as described in the related publication and code (see below). The OST data was then augmented, for the purposes of determining the most likely publication dates, with queries to the Crossref and arXiv APIs. 

## Related Publication
Fleerackers, A., Shores, K., Chtena, N. & Alperin, J.P. (2023). Unreviewed science in the news: 
The evolution of preprint media coverage from 2014-2021. *bioarxiv*. 

## Related dataset:
Alperin, Juan Pablo; Fleerackers; Shores, 2023, "Data for: Unreviewed science in the news", https://doi.org/10.7910/DVN/ZHQUFD, *Harvard Dataverse*

## Data files
`.research_mentions_all_final.csv`  Main data file including Altmetric Mews mentions for both WoS and preprints, augmented with publication dates and other calculated variables 
`./excluded_outlets.csv`      List of outlets found in Altmetric that were excluded for not being in English or for having problematic data
`./top outlets/outlets.csv`     Included outlets and number of mentions in each

