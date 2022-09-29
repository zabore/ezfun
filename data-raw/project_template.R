## Create an analysis project template
ez_analysis_template <-
  list(
    readme = rlang::expr(list(
      template_filename = fs::path_package("project_templates/readme.md",
                                           package = "ezfun"),
      filename = "README.md",
      copy = FALSE
    )),
    gitignore = rlang::expr(list(
      template_filename = fs::path_package("project_templates/gitignore.txt",
                                           package = "ezfun"),
      filename = ".gitignore",
      copy = TRUE
    )),
    rproj = rlang::expr(list(
      template_filename = fs::path_package("project_templates/default_rproj.Rproj",
                                           package = "ezfun"),
      filename = glue::glue("{folder_name}.Rproj"),
      copy = TRUE
    )),
    # only add Rprofile if renv was used
    rprofile =
      rlang::expr(
        switch(renv,
               list(
                 template_filename =
                   fs::path_package("project_templates/default_rprofile.R",
                                    package = "starter"),
                 filename = stringr::str_glue(".Rprofile"),
                 glue = TRUE
                 )
      )),
    munge = rlang::expr(list(
      template_filename = fs::path_package("project_templates/01-munge.R",
                                           package = "ezfun"),
      filename = glue::glue("code/{folder_name}_01-munge.R"),
      copy = FALSE
    )),
    report = rlang::expr(list(
      template_filename = fs::path_package("project_templates/02-report.Rmd",
                                           package = "ezfun"),
      filename = glue::glue("code/{folder_name}_02-report.Rmd"),
      copy = FALSE
    )),
    doc_template = rlang::expr(list(
      template_filename = fs::path_package("project_templates/doc_template.docx",
                                           package = "ezfun"),
      filename = "code/templates/doc_template.docx",
      copy = TRUE
    ))
    )
