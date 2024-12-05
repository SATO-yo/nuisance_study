data.original <-
  readxl::read_xlsx("/home/rstudio/NuisanceStudy/study1/data/data_study1_forUse.xlsx",
                    sheet = "ToUse", col_names = T
  )

data.clean <-
  data.original %>%
  dplyr::filter(used == 1) %>%
  dplyr::select(
    -StartDate, -EndDate, -Status, -Progress, -"Duration (in seconds)", 
    -Finished, -RecordedDate, -ResponseId, -RecipientLastName, -RecipientFirstName,
    -RecipientEmail, -ExternalReference,
    -DistributionChannel, -UserLanguage, -dplyr::ends_with("check"), -from_Lancers,
    -nation, -device, -approved, -used
  ) %>%
  lapply(as.numeric) %>%
  as.data.frame() %>%
  dplyr::filter_at(vars(matches("^Disp.*10$")), all_vars(. == 4 | is.na(.))) %>%
  dplyr::select(-matches("^Disp.*10$")) %>%
  dplyr::mutate(gender = factor(gender)) %>%
  dplyr::mutate(ID = factor(1:nrow(.)), .before = MCfs_std_FS_DO)


trim <- function(cond_b, cond_w, scene) {
  # list object in which each trimmed data is stored in corresponding levels
  result <- list()
  
  cnt <- 1
  
  for (i in cond_b) {
    for (j in cond_w) {
      for (k in scene) {
        data.clean %>%
          dplyr::filter(.data[[paste0("MCfs_", k, "_", j, "_", i)]] %in% c(1:7)) %>%
          dplyr::select(ID, contains(paste0(k, "_", j, "_", i)), age, gender) -> medresult1
        
        if (i == "DO") {
          # contains("std_FS_DO") includes both "std_FS_DO" and "std_FS_DONT"
          dplyr::select(medresult1, -contains("DONT")) -> medresult2
        } else {
          medresult1 -> medresult2
        }
        
        medresult2 %>%
          mutate(act = rep(factor(paste0(i)), nrow(.)), .after = ID) %>%
          mutate(Mental = rep(factor(paste0(j)), nrow(.)), .after = act) %>%
          mutate(Scene = rep(factor(paste0(k)), nrow(.)), .after = Mental) -> result[[cnt]]
        
        cnt <- cnt + 1
      }
    }
  }
  
  result %>%
    purrr::map_dfr(
      stats::setNames,
      c(
        "ID", "act", "Mental", "Scene", "MCfs", "MCdo",
        "emo1", "emo2", "emo3", "emo4", "emo5", "emo6",
        "beh1", "beh2", "beh3", "beh4",
        "prsn1", "prsn2", "prsn3", "prsn4", "prsn5",
        "prsn6", "prsn7", "prsn8", "prsn9", "prsn10",
        "prsn11", "prsn12", "prsn13",
        "disp1", "disp2", "disp3", "disp4",
        "disp5", "disp6", "disp7", "disp8", "disp9",
        "dist1", "dist2", "dist3", "age", "gender"
      )
    ) %>%
    dplyr::mutate(MCdo = as.factor(MCdo)) %>%
    # Excluding rows of participants with any missing values
    dplyr::group_by(ID) %>%
    dplyr::filter_all(all_vars(!any(is.na(.)))) %>%
    # Excluding rows of participants whom the stimulus were mistakenly presented to
    # 5 times not 3 times
    dplyr::filter(n() == 3) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      8 - across(prsn1:prsn13),
      8 - across(dist1:dist3)
    ) %>%
    dplyr::mutate(
      prsn3 = 8 - prsn3,
      prsn10 = 8 - prsn10
    ) %>%
    # prsn3 and prsn10 are reverse items
    
    assign("dat", ., envir = .GlobalEnv)
}

trim(
  cond_b = c("DO", "DONT"),
  cond_w = c("FS", "UF", "NO"),
  scene = c("std", "bge", "bag")
)


# No miss in MCdo ---------------------------------------------------------

# Excluding the participants who incorrectly answered to MCdo at least once
dat_nomiss <-
  dat %>%
  dplyr::group_by(ID) %>%
  dplyr::filter(!any(act == "DONT" & MCdo == 2 | act == "DO" & MCdo == 1)) %>%
  dplyr::ungroup()



# Combined data without wrong answer to MCdo ------------------------------

dat_combined_nomiss <-
  data.frame(
    dat_nomiss %>% 
      dplyr::select(ID, act, Mental),
    MCfs = dat_nomiss %>% 
      dplyr::select(MCfs),
    emo = dat_nomiss %>% 
      dplyr::select(dplyr::starts_with("emo")) %>% rowMeans(),
    beh = dat_nomiss %>% 
      dplyr::select(dplyr::starts_with("beh")) %>% rowMeans(),
    cold = dat_nomiss %>% 
      dplyr::select(prsn1, prsn2, prsn3, prsn4, prsn5, prsn6) %>% rowMeans(),
    incomp = dat_nomiss %>% 
      dplyr::select(prsn7, prsn8, prsn9, prsn10, prsn11, prsn13) %>% rowMeans(),
    disp = dat_nomiss %>% 
      dplyr::select(dplyr::starts_with("disp")) %>% rowMeans(),
    dist = dat_nomiss %>% 
      dplyr::select(dplyr::starts_with("dist")) %>% rowMeans()
  )

