library(babelquarto)

fixed_add_links <- function(path, main_language, language_code, site_url, type,
                             config, output_folder, path_language, project_dir) {
  html <- xml2::read_html(path)
  document_path <- path

  lang_profile <- fs::path(project_dir, paste0("_quarto-", language_code), ext = "yml")
  if (fs::file_exists(lang_profile)) {
    lang_config <- yaml::read_yaml(lang_profile)
    config <- utils::modifyList(config, lang_config)
  }

  codes <- config[["babelquarto"]][["languagecodes"]]
  current_lang <- purrr::keep(codes, ~ .x[["name"]] == language_code)

  placement <- config[["babelquarto"]][["languagelinks"]] %||%
    switch(type, website = "navbar", book = "sidebar")

  sidebar_wanted <- (type == "website" && placement == "sidebar")
  no_sidebar_config <- (is.null(config[["website"]][["sidebar"]]))
  if (sidebar_wanted && no_sidebar_config) {
    cli::cli_abort(c(
      "Can't find {.field website.sidebar} in {.field _quarto.yml}.",
      i = "You set {.field babelquarto.languagelinks} to {.field sidebar} but don't have a sidebar."
    ))
  }
  if (placement == "navbar" && is.null(config[[type]][["navbar"]])) {
    cli::cli_abort(c(
      "Can't find {.field {type}.navbar} in {.field _quarto.yml}.",
      i = "You set {.field babelquarto/languagelinks} to {.field navbar} but don't have a navbar."
    ))
  }

  version_text <- if (length(current_lang) > 0L) {
    current_lang[[1L]][["text"]] %||% sprintf("Version in %s", toupper(language_code))
  } else {
    sprintf("Version in %s", toupper(language_code))
  }

  # --- FIX: build the suffix-stripping pattern from the actual code, not a
  # hardcoded 2-character wildcard ---
  strip_pattern <- function(code) paste0("\\.", code, "\\.html$")

  if (language_code == main_language) {
    new_path <- if (type == "book") {
      sub(
        strip_pattern(path_language),
        ".html",
        babelquarto:::path_rel(path, output_folder, path_language, main_language)
      )
    } else {
      babelquarto:::path_rel(path, output_folder, path_language, main_language)
    }
    href <- sprintf("%s/%s", site_url, new_path)
    no_translated_version <- !fs::file_exists(file.path(output_folder, new_path))
    if (no_translated_version) return()
  } else {
    base_path <- sub(
      strip_pattern(path_language),
      ".html",
      babelquarto:::path_rel(path, output_folder, path_language, main_language)
    )
    new_path <- if (type == "book") {
      fs::path_ext_set(base_path, sprintf(".%s.html", language_code))
    } else {
      base_path
    }
    href <- sprintf("%s/%s/%s", site_url, language_code, new_path)
    no_translated_version <- !fs::file_exists(file.path(output_folder, language_code, new_path))
    if (no_translated_version) return()
  }

  languages_links <- xml2::xml_find_first(html, "//ul[@id='languages-links']")
  languages_links_div_exists <- (length(languages_links) > 0L)

  if (!languages_links_div_exists) {
    if (placement == "navbar") {
      navbar <- xml2::xml_find_first(html, "//ul[contains(@class, 'navbar-nav')]")
      navbar_li <- xml2::xml_add_child(navbar, "li", class = "nav-item", .where = 0L)
      xml2::xml_add_child(navbar_li, "div", class = "dropdown",
                           id = "languages-links-parent", .where = "before")
    } else {
      sidebar_menu <- xml2::xml_find_first(html, "//div[contains(@class,'sidebar-menu-container')]")
      if (inherits(sidebar_menu, "xml_missing")) return()
      xml2::xml_add_sibling(sidebar_menu, "div", class = "dropdown",
                             id = "languages-links-parent", .where = "before")
    }

    parent <- xml2::xml_find_first(html, "//div[@id='languages-links-parent']")
    xml2::xml_add_child(parent, "button", "",
      class = "btn btn-primary dropdown-toggle babelquarto-languages-button",
      type = "button", `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false", id = "languages-button")

    button <- xml2::xml_find_first(html, "//button[@id='languages-button']")
    xml2::xml_add_child(button, "i", class = config[["babelquarto"]][["icon"]] %||% "bi bi-globe2")
    xml2::xml_text(button) <- sprintf(" %s", babelquarto:::find_language_name(path_language, config))

    xml2::xml_add_child(parent, "ul", class = "dropdown-menu", id = "languages-links")
    languages_links <- xml2::xml_find_first(html, "//ul[@id='languages-links']")
  }

  xml2::xml_add_child(languages_links, "a", version_text,
    class = "dropdown-item", href = href,
    id = sprintf("language-link-%s", language_code), .where = 0L)
  xml2::xml_add_parent(
    xml2::xml_find_first(html, sprintf("//a[@id='language-link-%s']", language_code)),
    "li"
  )

  xml2::write_html(html, document_path)
}

environment(fixed_add_links) <- asNamespace("babelquarto")
assignInNamespace("add_links", fixed_add_links, ns = "babelquarto")