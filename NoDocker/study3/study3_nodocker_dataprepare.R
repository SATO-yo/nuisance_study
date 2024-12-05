
# some data cleansing and private data delete ----------------------------

# data.original <-
#   readxl::read_xlsx("study3_data_raw.xlsx",
#                     sheet = "Sheet0", col_names = T
#   )
# 
# data.clean <-
#   data.original %>%
#   dplyr::filter(ToUse == 1) %>%
#   dplyr::mutate(age = as.numeric(age),
#                 age_over66 = as.numeric(age_over66)) %>% 
#   dplyr::mutate(age = if_else(age >= 65, age_over66, age)) %>% 
#   
#   # exclude younger than 18-year-old
#   dplyr::filter(age >= 18) %>% 
#   
#   dplyr::select(
#     -StartDate:-Status,
#     -Progress:-cover,
#     # 9th and 10th item of impression assessments are filler
#     -matches("^prsn.*9$"),
#     -matches("^prsn.*10$"),
#     -age_over66:-ToUse
#   ) %>% 
#   dplyr::mutate(IPAddress = as.factor(IPAddress)) %>% 
#   
#   # delete the 3rd and all the after responses from the same IP Addresses
#   dplyr::group_by(IPAddress) %>%
#   dplyr::slice(1:2) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(-IPAddress) %>%
#   
#   # filter participants who failed attention checks
#   dplyr::filter_at(.vars = vars(matches("^ACstd")),
#                    .vars_predicate = all_vars(. == 2 | is.na(.))) %>%
#   dplyr::filter_at(.vars = vars(matches("^ACbag")),
#                    .vars_predicate = all_vars(. == 1 | is.na(.))) %>%
#   dplyr::filter_at(.vars = vars(matches("^dist_bge.*4$")),
#                    .vars_predicate = all_vars(. == 7 | is.na(.))) %>%
#   dplyr::select(-starts_with("AC"),
#                 -matches("^dist_bge.*4$")) %>%
#   
#   dplyr::mutate(ID = factor(1:nrow(.)), .before = fore1_std_FS_DO)
# 
# write.csv(
#   data.clean,
#   "data/data_study3_public.csv",
#   row.names = FALSE
# )



# main process ------------------------------------------------------------

data.clean <-
  utils::read.csv("data_study3_public.csv", header = TRUE)

makelong <- function(x, cond_act, cond_mnt, scene) {
  # list object in which each trimmed data is stored in corresponding levels
  result <- list()
  
  cnt <- 1
  
  for (ACT in cond_act) {
    for (MNT in cond_mnt) {
      for (SCN in scene) {
        x %>%
          dplyr::filter(.data[[paste0("MCfs_", SCN, "_", MNT, "_", ACT)]] %in% c(1:7)) %>%
          dplyr::select(ID, contains(paste0(SCN, "_", MNT, "_", ACT)), gender, age) -> medresult1
        
        if (ACT == "DO") {
          # contains("std_FS_DO") includes both "std_FS_DO" and "std_FS_DONT"
          dplyr::select(medresult1, -contains("DONT")) -> medresult2
        } else if (ACT == "DONT") {
          medresult1 -> medresult2
        }
        
        medresult2 %>%
          # dplyr::select(-contains("MCdo")) %>% 
          # Due to the error in manipulation check of "act", 
          # the columns of "MCdo" no longer have any information
          
          dplyr::mutate(act = rep(factor(paste0(ACT)), nrow(.)), .after = ID) %>%
          dplyr::mutate(Mental = rep(factor(paste0(MNT)), nrow(.)), .after = act) %>%
          dplyr::mutate(Scene = rep(factor(paste0(SCN)), nrow(.)), .after = Mental) -> result[[cnt]]
        
        cnt <- cnt + 1
      }
    }
  }
  
  result %>%
    purrr::map_dfr(
      stats::setNames,
      c(
        "ID",
        "act",
        "Mental",
        "Scene",
        "fore1",
        "fore2",
        "fore3",
        "fore4",
        "deh1",
        "deh2",
        "deh3",
        "deh4",
        "blm1",
        "blm2",
        "blm3",
        "prsn1",
        "prsn2",
        "prsn3",
        "prsn4",
        "prsn5",
        "prsn6",
        "prsn7",
        "prsn8",
        "dist1",
        "dist2",
        "dist3",
        "MCfs",
        "MCdo",
        "gender",
        "age"
      )
    ) %>%
    # dplyr::mutate(MCdo = as.factor(MCdo)) %>% 
    
    # Excluding participants with any missing values
    group_by(ID) %>%
    dplyr::filter_at(
      vars(
        ID:age
      ), 
      all_vars(!any(is.na(.)))
    ) %>%
    
    # Excluding participants whom the stimulus were mistakenly presented to other than 3 times
    dplyr::filter(n() == 3) %>%
    
    ungroup() %>%
    mutate(across(c(fore1:MCfs, age), as.numeric),
           MCdo = as.factor(MCdo),
           gender = as.factor(gender)
           ) %>% 
    assign("dat", ., envir = .GlobalEnv)
}


makelong(
  x = data.clean,
  cond_act = c("DO", "DONT"),
  cond_mnt = c("FS", "UF"),
  scene = c("std", "bge", "bag")
)

# Exclude participants who incorrectly answered manipulation check of act 
dat_nomiss <-
  dat %>% 
  dplyr::group_by(ID) %>% 
  dplyr::filter(!any(act == "DO" & MCdo == "1" | act == "DONT" & MCdo == "2")) %>% 
  dplyr::ungroup()


# combined data set -------------------------------------------------------

dat_nomiss_combined <-
  dat_nomiss %>% 
  dplyr::select(-MCdo) %>% 
  dplyr::transmute(
    ID = ID,
    act = act,
    Mental = Mental,
    Scene = Scene,
    gender = gender,
    age = age,
    MCfs = MCfs,
    fore = rowMeans(dplyr::select(., contains("fore"))),
    # deh1:従順なUH-L/HN-L, deh2:のんきなUH-L/HN-H, deh3:よくしゃべるUH-H/HN-H, deh4:几帳面なUH-H/HN-L
    deh = rowMeans(dplyr::select(., deh1, deh2)) - rowMeans(dplyr::select(., deh3, deh4)),
    deh_mach = rowMeans(dplyr::select(., deh1, deh4) - rowMeans(dplyr::select(., deh2, deh3))),
    blm = rowMeans(select(., contains("blm"))),
    cold = 8 - rowMeans(select(., prsn1, prsn2, prsn5, prsn6)),
    incomp = 8 - rowMeans(select(., prsn3, prsn4, prsn7, prsn8)),
    dist = 8 - rowMeans(select(., contains("dist")))
  )

dat_nomiss_noscene <-
  dat_nomiss %>% 
  dplyr::select(-MCdo) %>% 
  tidyr::pivot_longer(fore1:MCfs, names_to = "values", values_to = "score") %>% 
  dplyr::group_by(ID, act, Mental, gender, age, values) %>% 
  dplyr::reframe(
    score = mean(score)
  ) %>% 
  tidyr::pivot_wider(names_from = "values", values_from = "score")

dat_nomiss_noscene_combined <-
  dat_nomiss_noscene %>%
  dplyr::transmute(
    ID = ID,
    act = act,
    Mental = Mental,
    gender = gender,
    age = age,
    MCfs = MCfs,
    fore = rowMeans(dplyr::select(., contains("fore"))),
    # deh1:従順なUH-L/HN-L, deh2:のんきなUH-L/HN-H, deh3:よくしゃべるUH-H/HN-H, deh4:几帳面なUH-H/HN-L
    deh = rowMeans(dplyr::select(., deh1, deh2)) - rowMeans(dplyr::select(., deh3, deh4)),
    deh_mach = rowMeans(dplyr::select(., deh1, deh4) - rowMeans(dplyr::select(., deh2, deh3))),
    blm = rowMeans(select(., contains("blm"))),
    cold = 8 - rowMeans(select(., prsn1, prsn2, prsn5, prsn6)),
    incomp = 8 - rowMeans(select(., prsn3, prsn4, prsn7, prsn8)),
    dist = 8 - rowMeans(select(., contains("dist")))
  )
  

# data set including incorrect answer to the manipulation check -----------

dat_combined <-
  dat %>% 
  dplyr::select(-MCdo) %>% 
  dplyr::transmute(
    ID = ID,
    act = act,
    Mental = Mental,
    Scene = Scene,
    gender = gender,
    age = age,
    MCfs = MCfs,
    fore = rowMeans(dplyr::select(., contains("fore"))),
    deh = rowMeans(dplyr::select(., deh1, deh2)) - rowMeans(dplyr::select(., deh3, deh4)),
    deh_mach = rowMeans(dplyr::select(., deh1, deh4) - rowMeans(dplyr::select(., deh2, deh3))),
    blm = rowMeans(select(., contains("blm"))),
    cold = 8 - rowMeans(select(., prsn1, prsn2, prsn5, prsn6)),
    incomp = 8 - rowMeans(select(., prsn3, prsn4, prsn7, prsn8)),
    dist = 8 - rowMeans(select(., contains("dist")))
  )

dat_noscene <-
  dat %>% 
  dplyr::select(-MCdo) %>% 
  tidyr::pivot_longer(fore1:MCfs, names_to = "values", values_to = "score") %>% 
  dplyr::group_by(ID, act, Mental, gender, age, values) %>% 
  dplyr::reframe(
    score = mean(score)
  ) %>% 
  tidyr::pivot_wider(names_from = "values", values_from = "score")

dat_noscene_combined <-
  dat_noscene %>%
  dplyr::transmute(
    ID = ID,
    act = act,
    Mental = Mental,
    gender = gender,
    age = age,
    MCfs = MCfs,
    fore = rowMeans(dplyr::select(., contains("fore"))),
    deh = rowMeans(dplyr::select(., deh1, deh2)) - rowMeans(dplyr::select(., deh3, deh4)),
    deh_mach = rowMeans(dplyr::select(., deh1, deh4) - rowMeans(dplyr::select(., deh2, deh3))),
    blm = rowMeans(select(., contains("blm"))),
    cold = 8 - rowMeans(select(., prsn1, prsn2, prsn5, prsn6)),
    incomp = 8 - rowMeans(select(., prsn3, prsn4, prsn7, prsn8)),
    dist = 8 - rowMeans(select(., contains("dist")))
  )
