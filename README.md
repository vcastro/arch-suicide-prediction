# ARCH (SCILHS) Suicide Data/Modeling Scripts

Source code for extracting data from the ARCH ([https://github.com/ARCH-commons]) network sites and running NBC model prediction

Reference:

Barak-Corren Y, Castro, VM, Nock, MK, Mandl K, Madsen, E, Seiger, A, Adams, W, Applegate, RJ, Bernstam, E, Klann, JG, Lozinski, G, McCarthy, E, Murphy, SN, Natter, M, Ostasiewski, B, Patibandla, N, Rosenthal, G, Silva, GS, Wei, K, Weber, GM, Weiler, SR, Reis, BY, Smoller, JW. (2019) *Validation of an EHR-Based Suicide Risk Alert Algorithm across Multiple Healthcare Systems*. Under Review.
 

### Usage

These scripts will work on an ARCH i2b2 instance on Microsoft SQL Server.  The scripts should be run in the following order:

1) sql/build_rxnorm_ing_list.sql - create a table with a list of RXNORM ingredients for drug grouping
2) sql/scilhs_suicide_builddatafiles.sql - apply inclusion/exclusion criteria, assign case/control status and extract tall data files to text files  
3) R/suicide_txt_to_Rds.R - converts to txt files from SQL scripts to RDS
4) R/suicide_siteNBC.R - trains the suicide prediction model and reports results



