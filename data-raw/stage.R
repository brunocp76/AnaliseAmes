# Conforme exemplo no livro "Zen do R"...

ames <- "E:/Bruno Pasquini/Projetos R/CursoR4DS2/data/ames.rds" %>%
   readRDS()

usethis::use_data(ames,
                  overwrite = TRUE,
                  compress = "xz",
                  version = 3)
