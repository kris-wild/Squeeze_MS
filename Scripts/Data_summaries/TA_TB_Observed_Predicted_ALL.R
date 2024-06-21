#######
# combinding Tb and Ta observation vs prediction data for analysis: 
pacman::p_load(dplyr, flextable,here)
# Africa
A_aculeata_SiteA <- read.csv(file = here("output/checking Tbs/Agama aculeata/diffs_Agama_aculeataSiteA.csv")) %>% 
  mutate(species = "A_aculeata",
         site = "KLA",
         region = "Africa")
A_aculeata_SiteX <- read.csv(file = here("output/checking Tbs/Agama aculeata/diffs_Agama_aculeataSiteX.csv")) %>% 
  mutate(species = "A_aculeata",
         site = "KLX",
         region = "Africa")
C_angulifer_SiteA <- read.csv(file = here("output/checking Tbs/Meroles suborbitalis/diffs_Meroles_suborbitalisSiteA.csv")) %>% 
  mutate(species = "C_angulifer",
         site = "KLA",
         region = "Africa")
C_angulifer_SiteL <- read.csv(file = here("output/checking Tbs/Meroles suborbitalis/diffs_Meroles_suborbitalisSiteL.csv")) %>% 
  mutate(species = "C_angulifer",
         site = "KLL",
         region = "Africa")
M_suborbitalisSiteA <- read.csv(file = here("output/checking Tbs/Meroles suborbitalis/diffs_Meroles_suborbitalisSiteA.csv")) %>% 
  mutate(species = "M_suborbitalis",
         site = "KLA",
         region = "Africa")
M_suborbitalisSiteL <- read.csv(file = here("output/checking Tbs/Meroles suborbitalis/diffs_Meroles_suborbitalisSiteL.csv")) %>% 
  mutate(species = "M_suborbitalis",
         site = "KLL",
         region = "Africa")
P_lineoocellataSiteA <- read.csv(file = here("output/checking Tbs/Pedioplanis lineoocellata/diffs_Pedioplanis_lineoocellataSiteA.csv")) %>% 
  mutate(species = "P_lineoocellata",
         site = "KLA",
         region = "Africa")
P_lineoocellataSiteT <- read.csv(file = here("output/checking Tbs/Pedioplanis lineoocellata/diffs_Pedioplanis_lineoocellataSiteT.csv")) %>% 
  mutate(species = "P_lineoocellata",
         site = "KLT",
         region = "Africa")
T_sparsaSiteB <- read.csv(file = here("output/checking Tbs/Trachylepis sparsa/diffs_Trachylepis_sparsaSiteB.csv")) %>% 
  mutate(species = "T_sparsa",
         site = "KLB",
         region = "Africa")
T_sparsaSiteK <- read.csv(file = here("output/checking Tbs/Trachylepis sparsa/diffs_Trachylepis_sparsaSiteK.csv")) %>% 
  mutate(species = "T_sparsa",
         site = "KLK",
         region = "Africa")
Africa_diffs <- rbind(A_aculeata_SiteA, A_aculeata_SiteX,
                      C_angulifer_SiteA, C_angulifer_SiteL,
                      M_suborbitalisSiteA, M_suborbitalisSiteL,
                      P_lineoocellataSiteA, P_lineoocellataSiteT,
                      T_sparsaSiteB, T_sparsaSiteK)

## Australia
C_isolepisSiteL <- read.csv(file = here("output/checking Tbs/Ctenophorus isolepis/diffs_Ctenophorus_isolepisSiteL.csv")) %>% 
  mutate(species = "C_isolepis",
         site = "AUSL",
         region = "Australia")
C_isolepisSiteR <- read.csv(file = here("output/checking Tbs/Ctenophorus isolepis/diffs_Ctenophorus_isolepisSiteR.csv")) %>% 
  mutate(species = "C_isolepis",
         site = "AUSR",
         region = "Australia")
C_quattSiteL <- read.csv(file = here("output/checking Tbs/Ctenotus quatt/diffs_Ctenotus_quattSiteL.csv")) %>% 
  mutate(species = "C_quatt",
         site = "AUSL",
         region = "Australia")
C_quattSiteR <- read.csv(file = here("output/checking Tbs/Ctenotus quatt/diffs_Ctenotus_quattSiteR.csv")) %>% 
  mutate(species = "C_quatt",
         site = "AUSR",
         region = "Australia")
G_variegataSiteL <- read.csv(file = here("output/checking Tbs/Gehyra variegata/diffs_Gehyra_variegataSiteL.csv")) %>% 
  mutate(species = "G_variegata",
         site = "AUSL",
         region = "Australia")
G_variegataSiteR <- read.csv(file = here("output/checking Tbs/Gehyra variegata/diffs_Gehyra_variegataSiteR.csv")) %>% 
  mutate(species = "G_variegata",
         site = "AUSR",
         region = "Australia")
M_horridusSiteD <- read.csv(file = here("output/checking Tbs/Moloch horridus/diffs_Moloch_horridusSiteD.csv")) %>% 
  mutate(species = "M_horridus",
         site = "AUSD",
         region = "Australia")
M_horridusSiteE <- read.csv(file = here("output/checking Tbs/Moloch horridus/diffs_Moloch_horridusSiteE.csv")) %>% 
  mutate(species = "M_horridus",
         site = "AUSE",
         region = "Australia")
M_horridusSiteL <- read.csv(file = here("output/checking Tbs/Moloch horridus/diffs_Moloch_horridusSiteL.csv")) %>% 
  mutate(species = "M_horridus",
         site = "AUSL",
         region = "Australia")
M_horridusSiteR <- read.csv(file = here("output/checking Tbs/Moloch horridus/diffs_Moloch_horridusSiteR.csv")) %>% 
  mutate(species = "M_horridus",
         site = "AUSR",
         region = "Australia")
P_minorSiteA <- read.csv(file = here("output/checking Tbs/Pogona minor/diffs_Pogona_minorSiteA.csv")) %>% 
  mutate(species = "P_minor",
         site = "AUSA",
         region = "Australia")
P_minorSiteI <- read.csv(file = here("output/checking Tbs/Pogona minor/diffs_Pogona_minorSiteI.csv")) %>% 
  mutate(species = "P_minor",
         site = "AUSI",
         region = "Australia")
P_minorSiteL <- read.csv(file = here("output/checking Tbs/Pogona minor/diffs_Pogona_minorSiteL.csv")) %>% 
  mutate(species = "P_minor",
         site = "AUSL",
         region = "Australia")
P_minorSiteR <- read.csv(file = here("output/checking Tbs/Pogona minor/diffs_Pogona_minorSiteR.csv")) %>% 
  mutate(species = "P_minor",
         site = "AUSR",
         region = "Australia")
Australia_diffs <- rbind(C_isolepisSiteL, C_isolepisSiteR,
                         C_quattSiteL, C_quattSiteR,
                         G_variegataSiteL, G_variegataSiteR,
                         M_horridusSiteD, M_horridusSiteE, M_horridusSiteL, M_horridusSiteR,
                         C_quattSiteL, C_quattSiteR,
                         P_minorSiteA, P_minorSiteI, P_minorSiteL, P_minorSiteR) %>% 
  dplyr::rename(Genus_species = Genus.species)


# save data in CSV to data
write.csv(x = Australia_diffs, "Raw_data/Aus_TB_TA_Observed_Predicted.csv")
write.csv(x = Africa_diffs, "Raw_data/Kal_TB_TA_Observed_Predicted.csv")
