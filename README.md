# Inferring the distribution of Visceral Leishmaniasis incidence from data at different spatial scales

Code to reproduce the results of the above manuscript, investigating the use of a disaggregation approach to predict village-level incidence of visceral leishmaniasis from sub-district level surveillance across a region targeted for elimination. 

## Abstract

As cases of visceral leishmaniasis (VL) dwindle, there is justification to pursue a finer scale than the sub-district ("block") for monitoring the progress of elimination in India. Low incidence projections across broad regions of unevenly-distributed communities are difficult to act upon, and  focal pockets of incidence cannot be overlooked if the goal is to avoid resurgence and sustain equitable elimination. However, maintaining consistent surveillance at a fine scale is difficult to justify in a low-resource setting and is not sustainable long-term. 

This work analyses village-level incidence across Bihar state, India. Spatial auto-correlation in observed incidence and associations with local environmental conditions are explored, and a statistical disaggregation approach is evaluated to infer village-level variation from routinely-collected block-level data. We found that the approach did not estimate village-level incidence more accurately than a baseline assumption of block-homogenous incidence, and that this inaccuracy was unlikely due to non-linear or interacting covariate effects. We suggest that reactive mechanisms of the current surveillance system may limit the predictability of observed village-level incidence, and that this should be a consideration when interpreting the output of disaggregation models. 

Spatial autocorrelation is evident on a block-level but appears weak between neighbouring villages within individual blocks, suggesting that an important transmission mechanism may act at a longer range (for example due to population movement). Increasing the range of reactive interventions to neighbouring villages may therefore not improve their efficacy in suppressing transmission. However, maintaining surveillance and diagnostic capacity in areas distant from recently observed cases - in particular along routes of population movement out of endemic regions - could reduce the risk of reintroduction into previously unaffected villages.

## Ethics and permissions

Ethical approval was obtained from the London School of Hygiene and Tropical Medicine ethics committee for this specific study (ref:27487) which falls within the broader objectives of the SPEAK India research consortium https://speakindia.org.in/ (ref: 14674). Permissions were granted by the National Center for Vector Borne Disease Control in India (NCVBDC) for analysis of the KA-MIS surveillance data to address SPEAK India's research objectives.

## Data Availability

Tables of results underlying the model comparison (aggregated error metrics) and associated figures (observed versus predicted distribution of counts across all villages) are shared in this repository. The VL incidence data (block- and village-level) are the property of the National Center for Vector-Borne Diseases Control (NCVBDC, Ministry of Health and Family Welfare, Government of India) and requests to access these data should be made directly to the Center. 
