data.original <-
  readxl::read_xlsx("/home/rstudio/NuisanceStudy/study2/data/data_study2_re_for_analysis.xlsm",
                    sheet = "ToUse", col_names = T
  )

data.clean <-
  data.original %>%
  dplyr::filter(used == 1) %>%
  dplyr::rename("Duration" = "Duration (in seconds)") %>%
  dplyr::select(
    -StartDate,
    -EndDate,
    -Progress,
    -Duration,
    -Finished,
    -RecordedDate,
    -nationality,
    -device,
    -approved,
    -used,
    -matches("^MCdo.*_e$")
  ) %>%
  lapply(as.numeric) %>%
  as.data.frame() %>%
  dplyr::filter_at(vars(matches("^deh_std.*17$")),
                   all_vars(. == 3 | is.na(.))) %>%
  dplyr::filter_at(vars(matches("^deh_bge.*17$")),
                   all_vars(. == 7 | is.na(.))) %>%
  dplyr::filter_at(vars(matches("^deh_bag.*17$")),
                   all_vars(. == 2 | is.na(.))) %>%
  dplyr::select(-matches("^deh.*17$"),
                -matches("^prsn.*9$"),
                -matches("^prsn.*10$")) %>%
  mutate(gender = factor(gender)) %>%
  mutate(ID = factor(1:nrow(.)), .before = fore1_std_FS_DO)


trim <- function(x, cond_b, cond_w, scene) {
  # list object in which each trimmed data is stored in corresponding levels
  result <- list()
  
  cnt <- 1
  
  for (between in cond_b) {
    for (within in cond_w) {
      for (s in scene) {
        x %>%
          dplyr::filter(.data[[paste0("MCfs_", s, "_", within, "_", between)]] %in% c(1:7)) %>%
          dplyr::select(ID, contains(paste0(s, "_", within, "_", between)), gender, age) -> medresult1
        
        if (between == "DO") {
          # contains("std_FS_DO") includes both "std_FS_DO" and "std_FS_DONT"
          dplyr::select(medresult1, -contains("DONT")) -> medresult2
        } else if (between == "DONT") {
          medresult1 -> medresult2
        }
        
        medresult2 %>%
          # dplyr::select(-contains("MCdo")) %>% 
          # Due to the error in manipulation check of "act", 
          # the columns of "MCdo" no longer have any information
          
          dplyr::mutate(act = rep(factor(paste0(between)), nrow(.)), .after = ID) %>%
          dplyr::mutate(Mental = rep(factor(paste0(within)), nrow(.)), .after = act) %>%
          dplyr::mutate(Scene = rep(factor(paste0(s)), nrow(.)), .after = Mental) -> result[[cnt]]
        
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
        "deh5",
        "deh6",
        "deh7",
        "deh8",
        "deh9",
        "deh10",
        "deh11",
        "deh12",
        "deh13",
        "deh14",
        "deh15",
        "deh16",
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
    dplyr::mutate(MCdo = as.factor(MCdo)) %>% 
    
    # Excluding rows of participants with any missing values
    # "MCdo" is not the target of missing values test
    group_by(ID) %>%
    dplyr::filter_at(
      vars(
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
        "deh5",
        "deh6",
        "deh7",
        "deh8",
        "deh9",
        "deh10",
        "deh11",
        "deh12",
        "deh13",
        "deh14",
        "deh15",
        "deh16",
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
        "gender",
        "age"
      ), 
      all_vars(!any(is.na(.)))
    ) %>%
    
    # Excluding rows of participants whom the stimulus were mistakenly presented to other than 3 times
    dplyr::filter(n() == 3) %>%
    
    ungroup() %>%
    assign("dat", ., envir = .GlobalEnv)
}


trim(
  x = data.clean,
  cond_b = c("DO", "DONT"),
  cond_w = c("FS", "UF", "NO"),
  scene = c("std", "bge", "bag")
)


# Combined all data -------------------------------------------------------

dat_combined <-
  dat %>%
  dplyr::transmute(
    ID = ID,
    act = act,
    Mental = Mental,
    MCfs = MCfs,
    fore = rowMeans(dplyr::select(., contains("fore"))),
    deh = rowMeans(
      dplyr::select(.,
                    deh3, deh4, deh7, deh8, deh11, deh12, deh15, deh16)
    ) - rowMeans(
      dplyr::select(.,
                    deh1, deh2, deh5, deh6, deh9, deh10, deh13, deh14)
    ),
    blm = rowMeans(select(., contains("blm"))),
    cold = 8 - rowMeans(select(., prsn1, prsn2, prsn5, prsn6)),
    incomp = 8 - rowMeans(select(., prsn3, prsn4, prsn7, prsn8)),
    dist = 8 - rowMeans(select(., contains("dist"))),
    gender = gender, 
    age = age
  )


