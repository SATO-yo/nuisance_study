
# 参加者内要因を含まないデザインの時、auto = TRUE かつ、besci = TRUE にすると、信頼区間をブートストラップで求める関数が正常に動作しない
# 参加者内要因を含まないデザインの時、besci = TRUE にすると、算出された効果量の信頼区間の臨界値がNaNになる

anovakun_main <- function(
    x, 
    id, 
    iv_b = NULL, 
    iv_w = NULL, 
    dv, 
    design, 
    fac_levels, 
    auto = TRUE,
    eta_g = TRUE, 
    eta_p = FALSE,
    besci = FALSE,
    nesci = TRUE,
    intr1 = iv_b,
    intr2 = iv_w
) {
  
  anovakun_result <- list()
  aov_result <- list()
  
  for (variable in dv) {

    # 各独立変数についてanovakun()実行し、anovakun_resultオブジェクトに格納
    anovakun_result[[variable]] <-
      x %>%
      dplyr::select(
        dplyr::all_of(id),
        dplyr::all_of(iv_b),
        dplyr::all_of(iv_w),
        dplyr::all_of(variable)
      ) %>%
      anovakun(
        dataset = .,
        design = design,
        fac_levels,
        long = T,
        geta = eta_g,
        peta = eta_p,
        cin = T,
        holm = T,
        auto = auto,
        tech = T,
        besci = besci,
        nesci = nesci
      )

    # aov_result[[variable]][["df"]] <-
    #   x %>%
    #   dplyr::select(
    #     dplyr::all_of(id),
    #     dplyr::all_of(iv_b),
    #     dplyr::all_of(iv_w),
    #     dplyr::all_of(variable)
    #   )

    aov_result_med <-
      anovakun_result[[variable]][["ANOVA TABLE"]][[2]]


    # AsB Design main effect--------------------------------------------------------------

    if (design == "AsB") {
      # 参加者間要因の効果を格納
      if (!is.null(iv_b)) {
        for (between_v in iv_b) {

          # 参照中のDVに対する、参照中の参加者間IVの自由度・F値・p値
          for (stats in c("df.col", "f.col", "p.col")) {

            if (stats == "df.col") {
              # between自由度分子
              aov_result[[variable]][[between_v]][[stats]][["numer"]] <-
                aov_result_med %>%
                dplyr::filter(source.col == paste0(between_v)) %>%
                .[[stats]] %>%
                sprintf(., fmt = '%.0f')

              # between自由度分母
              aov_result[[variable]][[between_v]][[stats]][["denom"]] <-
                aov_result_med %>%
                dplyr::filter(source.col == paste0("s x ", between_v)) %>%
                .[[stats]] %>%
                sprintf(., fmt = '%.0f')

            } else if (stats == "f.col") {
              # F値
              aov_result[[variable]][[between_v]][[stats]] <-
                aov_result_med %>%
                dplyr::filter(source.col == paste0(between_v)) %>%
                .[[stats]] %>%
                sprintf(., fmt = '%.2f')

            } else if (stats == "p.col") {
              # p値
              aov_result[[variable]][[between_v]][[stats]] <-
                aov_result_med %>%
                dplyr::filter(source.col == paste0(between_v)) %>%
                .[[stats]] %>%
                subnum(digits = 2, eql = TRUE)
            }
          }

          # 参照中のDVに対する、参照中の参加者間IVのgetaかpetaの値
          # geta使用の場合
          if (eta_g == TRUE) {
            aov_result[[variable]][[between_v]][["geta"]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste0(between_v)) %>%
              .[["G.eta^2"]] %>%
              subnum(digits = 2, eql = TRUE)

            aov_result[[variable]][[between_v]][["report"]] <-
              paste0(
                "*F* (",
                aov_result[[variable]][[between_v]][["df.col"]][["numer"]],
                ", ",
                aov_result[[variable]][[between_v]][["df.col"]][["denom"]] ,
                ") = ",
                aov_result[[variable]][[between_v]][["f.col"]],
                ", *p* ",
                aov_result[[variable]][[between_v]][["p.col"]],
                ", $η^2_g$ ",
                aov_result[[variable]][[between_v]][["geta"]]
              )

            if (besci == TRUE) {
              aov_result[[variable]][[between_v]][["geta"]][["Lower.Limit"]] <-
                anovakun_result[[variable]][["EFFECT SIZE INFORMATION"]][[2]] %>%
                dplyr::filter(Source == paste(between_v)) %>%
                .[["CI_L"]] %>%
                subnum(digits = 2, eql = FALSE)

              aov_result[[variable]][[between_v]][["geta"]][["Upper.Limit"]] <-
                anovakun_result[[variable]][["EFFECT SIZE INFORMATION"]][[2]] %>%
                dplyr::filter(Source == paste(between_v)) %>%
                .[["CI_U"]] %>%
                subnum(digits = 2, eql = FALSE)

              aov_result[[variable]][[between_v]][["report"]] <-
                paste0(
                  aov_result[[variable]][[between_v]][["report"]],
                  ", 95%CI [",
                  aov_result[[variable]][[between_v]][["geta"]][["Lower.Limit"]],
                  ", ",
                  aov_result[[variable]][[between_v]][["geta"]][["Upper.Limit"]],
                  "]"
                )
            } else if (nesci == TRUE) {
              aov_result[[variable]][[between_v]][["geta"]][["Lower.Limit"]] <-
                anovakun_result[[variable]][["ANOVA TABLE"]][[3]] %>%
                dplyr::filter(Source == paste(between_v)) %>%
                .[["CI_L"]] %>%
                subnum(digits = 2, eql = FALSE)

              aov_result[[variable]][[between_v]][["geta"]][["Upper.Limit"]] <-
                anovakun_result[[variable]][["ANOVA TABLE"]][[3]] %>%
                dplyr::filter(Source == paste(between_v)) %>%
                .[["CI_U"]] %>%
                subnum(digits = 2, eql = FALSE)

              aov_result[[variable]][[between_v]][["report"]] <-
                paste0(
                  aov_result[[variable]][[between_v]][["report"]],
                  ", 95%CI [",
                  aov_result[[variable]][[between_v]][["geta"]][["Lower.Limit"]],
                  ", ",
                  aov_result[[variable]][[between_v]][["geta"]][["Upper.Limit"]],
                  "]"
                )
            }



          } else if (eta_p == TRUE) {
            aov_result[[variable]][[between_v]][["peta"]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste0(between_v)) %>%
              .[["P.eta^2"]] %>%
              subnum(digits = 2, eql = TRUE)

            aov_result[[variable]][[between_v]][["report"]] <-
              paste0(
                "*F* (",
                aov_result[[variable]][[between_v]][["df.col"]][["numer"]],
                ", ",
                aov_result[[variable]][[between_v]][["df.col"]][["denom"]],
                ") = ",
                aov_result[[variable]][[between_v]][["f.col"]],
                ", *p* ",
                aov_result[[variable]][[between_v]][["p.col"]],
                ", $η^2_p$ ",
                aov_result[[variable]][[between_v]][["peta"]]
              )

          } else {
            aov_result[[variable]][[between_v]][["report"]] <-
              paste0(
                "*F* (",
                aov_result[[variable]][[between_v]][["df.col"]][["numer"]],
                ", ",
                aov_result[[variable]][[between_v]][["df.col"]][["denom"]],
                ") = ",
                aov_result[[variable]][[between_v]][["f.col"]],
                ", *p* ",
                aov_result[[variable]][[between_v]][["p.col"]]
              )
          }

          # 参加者内要因の効果を格納
          # 混合計画では、自由度の分母がbetween要因と交絡している
          if (!is.null(iv_w)) {
            for (within_v in iv_w) {

              # 参照中のDVに対する、参照中の参加者内IVの自由度・F値・p値
              for (stats in c("df.col", "f.col", "p.col")) {

                if (stats == "df.col") {
                  # between自由度分子
                  aov_result[[variable]][[within_v]][[stats]][["numer"]] <-
                    aov_result_med %>%
                    dplyr::filter(source.col == paste0(within_v)) %>%
                    .[[stats]] %>%
                    sprintf(., fmt = '%.2f')

                  # between自由度分母
                  aov_result[[variable]][[within_v]][[stats]][["denom"]] <-
                    aov_result_med %>%
                    dplyr::filter(source.col == paste("s x", between_v, "x", within_v)) %>%
                    .[[stats]] %>%
                    sprintf(., fmt = '%.2f')

                } else if (stats == "f.col") {
                  # F値
                  aov_result[[variable]][[within_v]][[stats]] <-
                    aov_result_med %>%
                    dplyr::filter(source.col == paste0(within_v)) %>%
                    .[[stats]] %>%
                    sprintf(., fmt = '%.2f')

                } else if (stats == "p.col") {
                  # p値
                  aov_result[[variable]][[within_v]][[stats]] <-
                    aov_result_med %>%
                    dplyr::filter(source.col == paste0(within_v)) %>%
                    .[[stats]] %>%
                    subnum(digits = 2, eql = TRUE)
                }
              }

              # 参照中のDVに対する、参照中の参加者内IVのgetaかpetaの値
              if (eta_g == TRUE) {
                aov_result[[variable]][[within_v]][["geta"]] <-
                  aov_result_med %>%
                  dplyr::filter(source.col == paste0(within_v)) %>%
                  .[["G.eta^2"]] %>%
                  subnum(digits = 2, eql = TRUE)

                aov_result[[variable]][[within_v]][["report"]] <-
                  paste0(
                    "*F* (",
                    aov_result[[variable]][[within_v]][["df.col"]][["numer"]],
                    ", ",
                    aov_result[[variable]][[within_v]][["df.col"]][["denom"]] ,
                    ") = ",
                    aov_result[[variable]][[within_v]][["f.col"]],
                    ", *p* ",
                    aov_result[[variable]][[within_v]][["p.col"]],
                    ", $η^2_g$ ",
                    aov_result[[variable]][[within_v]][["geta"]]
                  )

                if (besci == TRUE) {
                  aov_result[[variable]][[within_v]][["geta"]][["Lower.Limit"]] <-
                    anovakun_result[[variable]][["EFFECT SIZE INFORMATION"]][[2]] %>%
                    dplyr::filter(Source == paste(within_v)) %>%
                    .[["CI_L"]] %>%
                    subnum(digits = 2, eql = FALSE)

                  aov_result[[variable]][[within_v]][["geta"]][["Upper.Limit"]] <-
                    anovakun_result[[variable]][["EFFECT SIZE INFORMATION"]][[2]] %>%
                    dplyr::filter(Source == paste(within_v)) %>%
                    .[["CI_U"]] %>%
                    subnum(digits = 2, eql = FALSE)

                  aov_result[[variable]][[within_v]][["report"]] <-
                    paste0(
                      aov_result[[variable]][[within_v]][["report"]],
                      ", 95%CI [",
                      aov_result[[variable]][[within_v]][["geta"]][["Lower.Limit"]],
                      ", ",
                      aov_result[[variable]][[within_v]][["geta"]][["Upper.Limit"]],
                      "]"
                    )
                }


              } else if (eta_p == TRUE) {
                aov_result[[variable]][[within_v]][["peta"]] <-
                  aov_result_med %>%
                  dplyr::filter(source.col == paste0(within_v)) %>%
                  .[["P.eta^2"]] %>%
                  subnum(digits = 2, eql = TRUE)

                aov_result[[variable]][[within_v]][["report"]] <-
                  paste0(
                    "*F* (",
                    aov_result[[variable]][[within_v]][["df.col"]][["numer"]],
                    ", ",
                    aov_result[[variable]][[within_v]][["df.col"]][["denom"]],
                    ") = ",
                    aov_result[[variable]][[within_v]][["f.col"]],
                    ", *p* ",
                    aov_result[[variable]][[within_v]][["p.col"]],
                    ", $η^2_p$ ",
                    aov_result[[variable]][[within_v]][["peta"]]
                  )

              } else {
                aov_result[[variable]][[within_v]][["report"]] <-
                  paste0(
                    "*F* (",
                    aov_result[[variable]][[within_v]][["df.col"]][["numer"]],
                    ", ",
                    aov_result[[variable]][[within_v]][["df.col"]][["denom"]],
                    ") = ",
                    aov_result[[variable]][[within_v]][["f.col"]],
                    ", *p* ",
                    aov_result[[variable]][[within_v]][["p.col"]]
                  )
              }
            }
          }

        }
      }


      # AsBデザインの時の交互作用の効果を格納（現状2要因の交互作用のみに対応）
      if (!is.null(intr1) & !is.null(intr2)) {

        for (stats in c("df.col", "f.col","p.col")) {
          if (stats == "df.col") {
            aov_result[[variable]][["intr"]][[stats]][["numer"]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste(intr1, "x", intr2)) %>%
              .[[stats]] %>%
              sprintf(., fmt = '%.2f')
            aov_result[[variable]][["intr"]][[stats]][["denom"]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste("s x", intr1, "x", intr2)) %>%
              .[[stats]] %>%
              sprintf(., fmt = '%.2f')
          } else if (stats == "f.col") {
            aov_result[[variable]][["intr"]][[stats]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste(intr1, "x", intr2)) %>%
              .[[stats]] %>%
              sprintf(., fmt = '%.2f')
          } else if (stats == "p.col") {
            aov_result[[variable]][["intr"]][[stats]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste(intr1, "x", intr2)) %>%
              .[[stats]] %>%
              subnum(digits = 2, eql = TRUE)
          }

        }

        # 参照中のDVに対する、交互作用項のgetaかpetaの値
        # geta使用の場合
        if (eta_g == TRUE) {
          aov_result[[variable]][["intr"]][["geta"]] <-
            aov_result_med %>%
            dplyr::filter(source.col == paste(intr1, "x", intr2)) %>%
            .[["G.eta^2"]] %>%
            subnum(digits = 2, eql = TRUE)

          aov_result[[variable]][["intr"]][["report"]] <-
            paste0(
              "*F* (",
              aov_result[[variable]][["intr"]][["df.col"]][["numer"]],
              ", ",
              aov_result[[variable]][["intr"]][["df.col"]][["denom"]],
              ") = ",
              aov_result[[variable]][["intr"]][["f.col"]],
              ", *p* ",
              aov_result[[variable]][["intr"]][["p.col"]],
              ", $η^2_g$ ",
              aov_result[[variable]][["intr"]][["geta"]]
            )

          # aov_result_med %>%
          #   dplyr::filter(source.col == paste("s x", intr1, "x", intr2)) %>%
          #   .[["df.col"]] %>%
          #   MBESS::conf.limits.ncf(
          #     F.value = as.numeric(aov_result[[variable]][["intr"]][["f.col"]]),
          #     df.1 = as.numeric(aov_result[[variable]][["intr"]][["df.col"]][["numer"]]),
          #     df.2 = .,
          #     conf.level = 0.95
          #   ) %>%
          #   {
          #     .[["Lower.Limit"]] ->> lower_limit
          #     .[["Upper.Limit"]] ->> upper_limit
          #   }
          #
          # aov_result[[variable]][["intr"]][["geta"]][["Lower.Limit"]] <-
          #   c(
          #     lower_limit /
          #       (
          #         lower_limit +
          #           aov_result_med %>% dplyr::filter(source.col == "Total") %>% .[["df.col"]] + 1
          #       )
          #   ) %>%
          #   subnum(digits = 2, eql = FALSE)
          # lower_limit <- NULL
          #
          # aov_result[[variable]][["intr"]][["geta"]][["Upper.Limit"]] <-
          #   c(
          #     upper_limit /
          #       (
          #         upper_limit +
          #           aov_result_med %>% dplyr::filter(source.col == "Total") %>% .[["df.col"]] + 1
          #       )
          #   ) %>%
          #   subnum(digits = 2, eql = FALSE)
          # upper_limit <- NULL

          if (besci == TRUE) {
            aov_result[[variable]][["intr"]][["geta"]][["Lower.Limit"]] <-
              anovakun_result[[variable]][["EFFECT SIZE INFORMATION"]][[2]] %>%
              dplyr::filter(Source == paste(intr1, "x", intr2)) %>%
              .[["CI_L"]] %>%
              subnum(digits = 2, eql = FALSE)

            aov_result[[variable]][["intr"]][["geta"]][["Upper.Limit"]] <-
              anovakun_result[[variable]][["EFFECT SIZE INFORMATION"]][[2]] %>%
              dplyr::filter(Source == paste(intr1, "x", intr2)) %>%
              .[["CI_U"]] %>%
              subnum(digits = 2, eql = FALSE)

            aov_result[[variable]][["intr"]][["report"]] <-
              paste0(
                aov_result[[variable]][["intr"]][["report"]],
                ", 95%CI [",
                aov_result[[variable]][["intr"]][["geta"]][["Lower.Limit"]],
                ", ",
                aov_result[[variable]][["intr"]][["geta"]][["Upper.Limit"]],
                "]"
              )
          }


          # peta使用の場合
        } else if (eta_p == TRUE) {
          aov_result[[variable]][["intr"]][["peta"]] <-
            aov_result_med %>%
            dplyr::filter(source.col == paste0(intr1, "x", intr2)) %>%
            .[["P.eta^2"]] %>%
            subnum(digits = 2, eql = TRUE)

          aov_result[[variable]][["intr"]][["report"]] <-
            paste0(
              "*F* (",
              aov_result[[variable]][["intr"]][["df.col"]][["numer"]],
              ", ",
              aov_result[[variable]][["intr"]][["df.col"]][["denom"]],
              ") = ",
              aov_result[[variable]][["intr"]][["f.col"]],
              ", *p* ",
              aov_result[[variable]][["intr"]][["p.col"]],
              ", $η^2_p$ ",
              aov_result[[variable]][["intr"]][["peta"]]
            )

        } else {
          aov_result[[variable]][["intr"]][["report"]] <-
            paste0(
              "*F* (",
              aov_result[[variable]][["intr"]][["df.col"]][["numer"]],
              ", ",
              aov_result[[variable]][["intr"]][["df.col"]][["denom"]],
              ") = ",
              aov_result[[variable]][["intr"]][["f.col"]],
              ", *p* ",
              aov_result[[variable]][["intr"]][["p.col"]]
            )
        }

        # 交互作用が有意だった場合、単純主効果検定の結果を格納
        p.intr <-
          aov_result_med %>%
          dplyr::filter(source.col == paste(intr1, "x", intr2)) %>%
          .[["p.col"]]


        if (p.intr < 0.05) {


          # 単純主効果を見るため、各要因の水準を取得
          levels_vector_intr1 <-
            levels(anovakun_result[[variable]][["DESCRIPTIVE STATISTICS"]][["bstatist"]][[intr1]])
          levels_vector_intr2 <-
            levels(anovakun_result[[variable]][["DESCRIPTIVE STATISTICS"]][["bstatist"]][[intr2]])

          # 参加者間要因の単純主効果
          for (level_intr2 in levels_vector_intr2) {

            for (stats in c("df.col", "f.col","p.col")) {
              if (stats == "df.col") {
                aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][[stats]][["numer"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(intr1, "at", level_intr2)) %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.0f')
                aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][[stats]][["denom"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste("Er", "at", level_intr2)) %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.0f')
              } else if (stats == "f.col") {
                aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][[stats]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(intr1, "at", level_intr2)) %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.2f')
              } else if (stats == "p.col") {
                aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][[stats]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(intr1, "at", level_intr2)) %>%
                  .[[stats]] %>%
                  subnum(digits = 2, eql = TRUE)
              }

            }

            # 参照中のDVに対する、参照中の参加者間IVの単純主効果のgetaかpetaの値
            if (eta_g == TRUE) {
              aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["geta"]] <-
                anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                dplyr::filter(source.col == paste(intr1, "at", level_intr2)) %>%
                .[["G.eta^2"]] %>%
                subnum(digits = 2, eql = TRUE)

              aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["p.col"]],
                  ", $η^2_g$ ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["geta"]]
                )

              if (besci == TRUE) {
                aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["geta"]][["Lower.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["bescitab"]] %>%
                  dplyr::filter(Source == paste(intr1, "at", level_intr2)) %>%
                  .[["CI_L"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["geta"]][["Upper.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["bescitab"]] %>%
                  dplyr::filter(Source == paste(intr1, "at", level_intr2)) %>%
                  .[["CI_U"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["report"]] <-
                  paste0(
                    aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["report"]],
                    ", 95%CI [",
                    aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["geta"]][["Lower.Limit"]],
                    ", ",
                    aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["geta"]][["Upper.Limit"]],
                    "]"
                  )
              } else if (nesci == TRUE) {
                aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["geta"]][["Lower.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simnesci"]] %>%
                  dplyr::filter(Source == paste(intr1, "at", level_intr2)) %>%
                  .[["CI_L"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["geta"]][["Upper.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simnesci"]] %>%
                  dplyr::filter(Source == paste(intr1, "at", level_intr2)) %>%
                  .[["CI_U"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["report"]] <-
                  paste0(
                    aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["report"]],
                    ", 95%CI [",
                    aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["geta"]][["Lower.Limit"]],
                    ", ",
                    aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["geta"]][["Upper.Limit"]],
                    "]"
                  )
              }

            } else if (eta_p == TRUE) {
              aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["peta"]] <-
                anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                dplyr::filter(source.col == paste(intr1, "at", level_intr2)) %>%
                .[["P.eta^2"]] %>%
                subnum(digits = 2, eql = TRUE)

              aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["p.col"]],
                  ", $η^2_g$ ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["peta"]]
                )

            } else {
              aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[intr1]][[paste("at", level_intr2)]][["p.col"]]
                )
            }


          }

          # 参加者内要因の単純主効果
          for (level_intr1 in levels_vector_intr1) {

            for (stats in c("df.col", "f.col","p.col")) {
              if (stats == "df.col") {
                aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][[stats]][["numer"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(intr2, "at", level_intr1)) %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.2f')
                aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][[stats]][["denom"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste("s x", intr2, "at", level_intr1)) %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.2f')
              } else if (stats == "f.col") {
                aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][[stats]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(intr2, "at", level_intr1)) %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.2f')
              } else if (stats == "p.col") {
                aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][[stats]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(intr2, "at", level_intr1)) %>%
                  .[[stats]] %>%
                  subnum(digits = 2, eql = TRUE)
              }

            }

            # 参照中のDVに対する、参照中の参加者内IVの単純主効果のgetaかpetaの値
            if (eta_g == TRUE) {
              aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["geta"]] <-
                anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                dplyr::filter(source.col == paste(intr2, "at", level_intr1)) %>%
                .[["G.eta^2"]] %>%
                subnum(digits = 2, eql = TRUE)

              aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["p.col"]],
                  ", $η^2_g$ ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["geta"]]
                )

              if (besci == TRUE) {
                aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["geta"]][["Lower.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["bescitab"]] %>%
                  dplyr::filter(Source == paste(intr2, "at", level_intr1)) %>%
                  .[["CI_L"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["geta"]][["Upper.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["bescitab"]] %>%
                  dplyr::filter(Source == paste(intr2, "at", level_intr1)) %>%
                  .[["CI_U"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["report"]] <-
                  paste0(
                    aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["report"]],
                    ", 95%CI [",
                    aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["geta"]][["Lower.Limit"]],
                    ", ",
                    aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["geta"]][["Upper.Limit"]],
                    "]"
                  )
              }

            } else if (eta_p == TRUE) {
              aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["peta"]] <-
                anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                dplyr::filter(source.col == paste(intr2, "at", level_intr1)) %>%
                .[["P.eta^2"]] %>%
                subnum(digits = 2, eql = TRUE)

              aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["p.col"]],
                  ", $η^2_g$ ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["peta"]]
                )

            } else {
              aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[intr2]][[paste("at", level_intr1)]][["p.col"]]
                )
            }

          }

        }


      }
    }


    # ABs Design main effect--------------------------------------------------------------

    if (design == "ABs") {
      # 参加者間要因の効果を格納
      if (!is.null(iv_b)) {
        for (between_v in iv_b) {

          # 参照中のDVに対する、参照中の参加者間IVの自由度・F値・p値
          for (stats in c("df.col", "f.col", "p.col")) {

            if (stats == "df.col") {
              # between自由度分子
              aov_result[[variable]][[between_v]][[stats]][["numer"]] <-
                aov_result_med %>%
                dplyr::filter(source.col == paste0(between_v)) %>%
                .[[stats]] %>%
                sprintf(., fmt = '%.0f')

              # between自由度分母
              aov_result[[variable]][[between_v]][[stats]][["denom"]] <-
                aov_result_med %>%
                dplyr::filter(source.col == "Error") %>%
                .[[stats]] %>%
                sprintf(., fmt = '%.0f')

            } else if (stats == "f.col") {
              # F値
              aov_result[[variable]][[between_v]][[stats]] <-
                aov_result_med %>%
                dplyr::filter(source.col == paste0(between_v)) %>%
                .[[stats]] %>%
                sprintf(., fmt = '%.2f')

            } else if (stats == "p.col") {
              # p値
              aov_result[[variable]][[between_v]][[stats]] <-
                aov_result_med %>%
                dplyr::filter(source.col == paste0(between_v)) %>%
                .[[stats]] %>%
                subnum(digits = 2, eql = TRUE)
            }
          }

          # 参照中のDVに対する、参照中の参加者間IVのgetaかpetaの値
          if (eta_g == TRUE) {
            aov_result[[variable]][[between_v]][["geta"]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste0(between_v)) %>%
              .[["G.eta^2"]] %>%
              subnum(digits = 2, eql = TRUE)

            aov_result[[variable]][[between_v]][["report"]] <-
              paste0(
                "*F* (",
                aov_result[[variable]][[between_v]][["df.col"]][["numer"]],
                ", ",
                aov_result[[variable]][[between_v]][["df.col"]][["denom"]] ,
                ") = ",
                aov_result[[variable]][[between_v]][["f.col"]],
                ", *p* ",
                aov_result[[variable]][[between_v]][["p.col"]],
                ", $η^2_g$ ",
                aov_result[[variable]][[between_v]][["geta"]]
              )

            if (besci == TRUE) {
              aov_result[[variable]][[between_v]][["geta"]][["Lower.Limit"]] <-
                anovakun_result[[variable]][["EFFECT SIZE INFORMATION"]][[2]] %>%
                dplyr::filter(Source == paste(between_v)) %>%
                .[["CI_L"]] %>%
                subnum(digits = 2, eql = FALSE)

              aov_result[[variable]][[between_v]][["geta"]][["Upper.Limit"]] <-
                anovakun_result[[variable]][["EFFECT SIZE INFORMATION"]][[2]] %>%
                dplyr::filter(Source == paste(between_v)) %>%
                .[["CI_U"]] %>%
                subnum(digits = 2, eql = FALSE)

              aov_result[[variable]][[between_v]][["report"]] <-
                paste0(
                  aov_result[[variable]][[between_v]][["report"]],
                  ", 95%CI [",
                  aov_result[[variable]][[between_v]][["geta"]][["Lower.Limit"]],
                  ", ",
                  aov_result[[variable]][[between_v]][["geta"]][["Upper.Limit"]],
                  "]"
                )
            } else if (nesci == TRUE) {
              aov_result[[variable]][[between_v]][["geta"]][["Lower.Limit"]] <-
                anovakun_result[[variable]][["ANOVA TABLE"]][[3]] %>%
                dplyr::filter(Source == paste(between_v)) %>%
                .[["CI_L"]] %>%
                subnum(digits = 2, eql = FALSE)

              aov_result[[variable]][[between_v]][["geta"]][["Upper.Limit"]] <-
                anovakun_result[[variable]][["ANOVA TABLE"]][[3]] %>%
                dplyr::filter(Source == paste(between_v)) %>%
                .[["CI_U"]] %>%
                subnum(digits = 2, eql = FALSE)

              aov_result[[variable]][[between_v]][["report"]] <-
                paste0(
                  aov_result[[variable]][[between_v]][["report"]],
                  ", 95%CI [",
                  aov_result[[variable]][[between_v]][["geta"]][["Lower.Limit"]],
                  ", ",
                  aov_result[[variable]][[between_v]][["geta"]][["Upper.Limit"]],
                  "]"
                )
            }

          } else if (eta_p == TRUE) {
            aov_result[[variable]][[between_v]][["peta"]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste0(between_v)) %>%
              .[["P.eta^2"]] %>%
              subnum(digits = 2, eql = TRUE)

            aov_result[[variable]][[between_v]][["report"]] <-
              paste0(
                "*F* (",
                aov_result[[variable]][[between_v]][["df.col"]][["numer"]],
                ", ",
                aov_result[[variable]][[between_v]][["df.col"]][["denom"]],
                ") = ",
                aov_result[[variable]][[between_v]][["f.col"]],
                ", *p* ",
                aov_result[[variable]][[between_v]][["p.col"]],
                ", $η^2_p$ ",
                aov_result[[variable]][[between_v]][["peta"]]
              )

          } else {
            aov_result[[variable]][[between_v]][["report"]] <-
              paste0(
                "*F* (",
                aov_result[[variable]][[between_v]][["df.col"]][["numer"]],
                ", ",
                aov_result[[variable]][[between_v]][["df.col"]][["denom"]],
                ") = ",
                aov_result[[variable]][[between_v]][["f.col"]],
                ", *p* ",
                aov_result[[variable]][[between_v]][["p.col"]]
              )
          }

        }
      }

      # ABsデザインの時の交互作用の効果を格納（現状2要因の交互作用のみに対応）
      # if (!is.null(intr1) & !is.null(intr2)) {

        for (stats in c("df.col", "f.col","p.col")) {
          if (stats == "df.col") {
            aov_result[[variable]][["intr"]][[stats]][["numer"]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste(iv_b[[1]], "x", iv_b[[2]])) %>%
              .[[stats]] %>%
              sprintf(., fmt = '%.0f')
            aov_result[[variable]][["intr"]][[stats]][["denom"]] <-
              aov_result_med %>%
              dplyr::filter(source.col == "Error") %>%
              .[[stats]] %>%
              sprintf(., fmt = '%.0f')
          } else if (stats == "f.col") {
            aov_result[[variable]][["intr"]][[stats]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste(iv_b[[1]], "x", iv_b[[2]])) %>%
              .[[stats]] %>%
              sprintf(., fmt = '%.2f')
          } else if (stats == "p.col") {
            aov_result[[variable]][["intr"]][[stats]] <-
              aov_result_med %>%
              dplyr::filter(source.col == paste(iv_b[[1]], "x", iv_b[[2]])) %>%
              .[[stats]] %>%
              subnum(digits = 2, eql = TRUE)
          }

        }

        # 参照中のDVに対する、参照中の参加者間IVのgetaかpetaの値
        if (eta_g == TRUE) {
          aov_result[[variable]][["intr"]][["geta"]] <-
            aov_result_med %>%
            dplyr::filter(source.col == paste(iv_b[[1]], "x", iv_b[[2]])) %>%
            .[["G.eta^2"]] %>%
            subnum(digits = 2, eql = TRUE)

          aov_result[[variable]][["intr"]][["report"]] <-
            paste0(
              "*F* (",
              aov_result[[variable]][["intr"]][["df.col"]][["numer"]],
              ", ",
              aov_result[[variable]][["intr"]][["df.col"]][["denom"]],
              ") = ",
              aov_result[[variable]][["intr"]][["f.col"]],
              ", *p* ",
              aov_result[[variable]][["intr"]][["p.col"]],
              ", $η^2_g$ ",
              aov_result[[variable]][["intr"]][["geta"]]
            )

          if (besci == TRUE) {
            aov_result[[variable]][["intr"]][["geta"]][["Lower.Limit"]] <-
              anovakun_result[[variable]][["EFFECT SIZE INFORMATION"]][[2]] %>%
              dplyr::filter(Source == paste(iv_b[[1]], "x", iv_b[[2]])) %>%
              .[["CI_L"]] %>%
              subnum(digits = 2, eql = FALSE)

            aov_result[[variable]][["intr"]][["geta"]][["Upper.Limit"]] <-
              anovakun_result[[variable]][["EFFECT SIZE INFORMATION"]][[2]] %>%
              dplyr::filter(Source == paste(iv_b[[1]], "x", iv_b[[2]])) %>%
              .[["CI_U"]] %>%
              subnum(digits = 2, eql = FALSE)

            aov_result[[variable]][["intr"]][["report"]] <-
              paste0(
                aov_result[[variable]][["intr"]][["report"]],
                ", 95%CI [",
                aov_result[[variable]][["intr"]][["geta"]][["Lower.Limit"]],
                ", ",
                aov_result[[variable]][["intr"]][["geta"]][["Upper.Limit"]],
                "]"
              )
          }

          else if (nesci == TRUE) {
            aov_result[[variable]][["intr"]][["geta"]][["Lower.Limit"]] <-
              anovakun_result[[variable]][["ANOVA TABLE"]][[3]] %>%
              dplyr::filter(Source == paste(iv_b[[1]], "x", iv_b[[2]])) %>%
              .[["CI_L"]] %>%
              subnum(digits = 2, eql = FALSE)

            aov_result[[variable]][["intr"]][["geta"]][["Upper.Limit"]] <-
              anovakun_result[[variable]][["ANOVA TABLE"]][[3]] %>%
              dplyr::filter(Source == paste(iv_b[[1]], "x", iv_b[[2]])) %>%
              .[["CI_U"]] %>%
              subnum(digits = 2, eql = FALSE)

            aov_result[[variable]][["intr"]][["report"]] <-
              paste0(
                aov_result[[variable]][["intr"]][["report"]],
                ", 95%CI [",
                aov_result[[variable]][["intr"]][["geta"]][["Lower.Limit"]],
                ", ",
                aov_result[[variable]][["intr"]][["geta"]][["Upper.Limit"]],
                "]"
              )
          }

        } else if (eta_p == TRUE) {
          aov_result[[variable]][["intr"]][["peta"]] <-
            aov_result_med %>%
            dplyr::filter(source.col == paste0(iv_b[[1]], "x", iv_b[[2]])) %>%
            .[["P.eta^2"]] %>%
            subnum(digits = 2, eql = TRUE)

          aov_result[[variable]][["intr"]][["report"]] <-
            paste0(
              "*F* (",
              aov_result[[variable]][["intr"]][["df.col"]][["numer"]],
              ", ",
              aov_result[[variable]][["intr"]][["df.col"]][["denom"]],
              ") = ",
              aov_result[[variable]][["intr"]][["f.col"]],
              ", *p* ",
              aov_result[[variable]][["intr"]][["p.col"]],
              ", $η^2_p$ ",
              aov_result[[variable]][["intr"]][["peta"]]
            )

        } else {
          aov_result[[variable]][["intr"]][["report"]] <-
            paste0(
              "*F* (",
              aov_result[[variable]][["intr"]][["df.col"]][["numer"]],
              ", ",
              aov_result[[variable]][["intr"]][["df.col"]][["denom"]],
              ") = ",
              aov_result[[variable]][["intr"]][["f.col"]],
              ", *p* ",
              aov_result[[variable]][["intr"]][["p.col"]]
            )
        }


        # 交互作用が有意だった場合、単純主効果検定の結果を格納
        p.intr <-
          aov_result_med %>%
          dplyr::filter(source.col == paste(iv_b[[1]], "x", iv_b[[2]])) %>%
          .[["p.col"]]


        if (p.intr < 0.05) {


          # 単純主効果を見るため、各要因の水準を取得
          levels_vector_intr1 <-
            levels(anovakun_result[[variable]][["DESCRIPTIVE STATISTICS"]][["bstatist"]][[iv_b[[1]]]])
          levels_vector_intr2 <-
            levels(anovakun_result[[variable]][["DESCRIPTIVE STATISTICS"]][["bstatist"]][[iv_b[[2]]]])

          for (level_intr2 in levels_vector_intr2) {

            for (stats in c("df.col", "f.col","p.col")) {
              if (stats == "df.col") {
                aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][[stats]][["numer"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(iv_b[[1]], "at", level_intr2)) %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.0f')
                aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][[stats]][["denom"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == "Error") %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.0f')
              } else if (stats == "f.col") {
                aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][[stats]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(iv_b[[1]], "at", level_intr2)) %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.2f')
              } else if (stats == "p.col") {
                aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][[stats]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(iv_b[[1]], "at", level_intr2)) %>%
                  .[[stats]] %>%
                  subnum(digits = 2, eql = TRUE)
              }

            }

            # 参照中のDVに対する、参照中の参加者間IVのgetaかpetaの値
            if (eta_g == TRUE) {
              aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["geta"]] <-
                anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                dplyr::filter(source.col == paste(iv_b[[1]], "at", level_intr2)) %>%
                .[["G.eta^2"]] %>%
                subnum(digits = 2, eql = TRUE)

              aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["p.col"]],
                  ", $η^2_g$ ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["geta"]]
                )

              if (besci == TRUE) {
                aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["geta"]][["Lower.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["bescitab"]] %>%
                  dplyr::filter(Source == paste(iv_b[[1]], "at", level_intr2)) %>%
                  .[["CI_L"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["geta"]][["Upper.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["bescitab"]] %>%
                  dplyr::filter(Source == paste(iv_b[[1]], "at", level_intr2)) %>%
                  .[["CI_U"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["report"]] <-
                  paste0(
                    aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["report"]],
                    ", 95%CI [",
                    aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["geta"]][["Lower.Limit"]],
                    ", ",
                    aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["geta"]][["Upper.Limit"]],
                    "]"
                  )
              } else if (nesci == TRUE) {
                aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["geta"]][["Lower.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simnesci"]] %>%
                  dplyr::filter(Source == paste(iv_b[[1]], "at", level_intr2)) %>%
                  .[["CI_L"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["geta"]][["Upper.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simnesci"]] %>%
                  dplyr::filter(Source == paste(iv_b[[1]], "at", level_intr2)) %>%
                  .[["CI_U"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["report"]] <-
                  paste0(
                    aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["report"]],
                    ", 95%CI [",
                    aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["geta"]][["Lower.Limit"]],
                    ", ",
                    aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["geta"]][["Upper.Limit"]],
                    "]"
                  )
              }

            } else if (eta_p == TRUE) {
              aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["peta"]] <-
                anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                dplyr::filter(source.col == paste(iv_b[[1]], "at", level_intr2)) %>%
                .[["P.eta^2"]] %>%
                subnum(digits = 2, eql = TRUE)

              aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["p.col"]],
                  ", $η^2_g$ ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["peta"]]
                )

            } else {
              aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[1]]]][[paste("at", level_intr2)]][["p.col"]]
                )
            }


          }

          for (level_intr1 in levels_vector_intr1) {

            for (stats in c("df.col", "f.col","p.col")) {
              if (stats == "df.col") {
                aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][[stats]][["numer"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(iv_b[[2]], "at", level_intr1)) %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.0f')
                aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][[stats]][["denom"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == "Error") %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.0f')
              } else if (stats == "f.col") {
                aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][[stats]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(iv_b[[2]], "at", level_intr1)) %>%
                  .[[stats]] %>%
                  sprintf(., fmt = '%.2f')
              } else if (stats == "p.col") {
                aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][[stats]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                  dplyr::filter(source.col == paste(iv_b[[2]], "at", level_intr1)) %>%
                  .[[stats]] %>%
                  subnum(digits = 2, eql = TRUE)
              }

            }

            # 参照中のDVに対する、参照中の参加者間IVのgetaかpetaの値
            if (eta_g == TRUE) {
              aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["geta"]] <-
                anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                dplyr::filter(source.col == paste(iv_b[[2]], "at", level_intr1)) %>%
                .[["G.eta^2"]] %>%
                subnum(digits = 2, eql = TRUE)

              aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["p.col"]],
                  ", $η^2_g$ ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["geta"]]
                )

              if (besci == TRUE) {
                aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["geta"]][["Lower.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["bescitab"]] %>%
                  dplyr::filter(Source == paste(iv_b[[2]], "at", level_intr1)) %>%
                  .[["CI_L"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["geta"]][["Upper.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["bescitab"]] %>%
                  dplyr::filter(Source == paste(iv_b[[2]], "at", level_intr1)) %>%
                  .[["CI_U"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["report"]] <-
                  paste0(
                    aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["report"]],
                    ", 95%CI [",
                    aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["geta"]][["Lower.Limit"]],
                    ", ",
                    aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["geta"]][["Upper.Limit"]],
                    "]"
                  )
              } else if (nesci == TRUE) {
                aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["geta"]][["Lower.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simnesci"]] %>%
                  dplyr::filter(Source == paste(iv_b[[2]], "at", level_intr1)) %>%
                  .[["CI_L"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["geta"]][["Upper.Limit"]] <-
                  anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simnesci"]] %>%
                  dplyr::filter(Source == paste(iv_b[[2]], "at", level_intr1)) %>%
                  .[["CI_U"]] %>%
                  subnum(digits = 2, eql = FALSE)

                aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["report"]] <-
                  paste0(
                    aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["report"]],
                    ", 95%CI [",
                    aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["geta"]][["Lower.Limit"]],
                    ", ",
                    aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["geta"]][["Upper.Limit"]],
                    "]"
                  )
              }

            } else if (eta_p == TRUE) {
              aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["peta"]] <-
                anovakun_result[[variable]][["POST ANALYSES"]][["A:B"]][["simtab"]] %>%
                dplyr::filter(source.col == paste(iv_b[[2]], "at", level_intr1)) %>%
                .[["P.eta^2"]] %>%
                subnum(digits = 2, eql = TRUE)

              aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["p.col"]],
                  ", $η^2_g$ ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["peta"]]
                )

            } else {
              aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["report"]] <-
                paste0(
                  "*F* (",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["df.col"]][["numer"]],
                  ", ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["df.col"]][["denom"]],
                  ") = ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["f.col"]],
                  ", *p* ",
                  aov_result[[variable]][["smplmain"]][[iv_b[[2]]]][[paste("at", level_intr1)]][["p.col"]]
                )
            }

          }

        }

      # }

    }
  }

  aov_result[["anovakun_all"]] <- anovakun_result
  return(aov_result)
}





# dat_combined %>% 
#   anovakun_main(
#     x = ., 
#     id = "ID",
#     iv_b = "act",
#     iv_w = "Mental",
#     dv = c("MCfs", "cold", "incomp", "deh", "fore", "blm", "dist"),
#     design = "AsB", 
#     fac_levels = c(2, 3),
#     intr1 = "act",
#     intr2 = "Mental",
#     eta_g = T,
#     eta_p = F
# ) -> anova_results


# calculation of confidence intervals of an eta is based on Haebara (2014).
# Haebara, T. (2014). Further foundations of statistics for psychological research. Yuhikaku.
