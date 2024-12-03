tategaki <- function(x){
  x <- stringr::str_replace_all(x, "ー", "丨") # 長音符の処理
  stringr::str_wrap(x, width = 1)
}

tategaki_alt <- function(x) {
  stringr::str_replace_all(
    x,
    c(
      `ー` = "丨",         # 長音符を縦書きにする
      `（` = "︵",         # 丸括弧開きを縦書きにする
      `）` = "︶",         # 丸括弧閉じを縦書きにする
      `(?<=.)(?=.)` = "\n" # 文字どうしの間に改行コードを挿入する
    )
  )
} 