## Create an analysis project template
ez_analysis_template <-
  list(
    readme = rlang::expr(list(
      template_filename = fs::path_package("project_templates/readme.md",
                                           package = "ezfun"),
      filename = "README.md",
      copy = FALSE
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
      filename = glue::glue("code/{folder_name}-munge.R"),
      copy = FALSE
    )),
    report = rlang::expr(list(
      template_filename = fs::path_package("project_templates/02-report.qmd",
                                           package = "ezfun"),
      filename = glue::glue("code/{folder_name}-report.qmd"),
      copy = FALSE
    )),
    doc_template = rlang::expr(list(
      template_filename = fs::path_package("project_templates/doc_template.docx",
                                           package = "ezfun"),
      filename = "code/templates/doc_template.docx",
      copy = TRUE
    ))
    )


# Create template object -----
attr(ez_analysis_template, "label") <- "EZ Analysis Template"

usethis::use_data(ez_analysis_template, overwrite = TRUE)
