html_file_to_pdf <- function(
    html_table_path,
    pdf_path,
    chrome_path = NULL
) {
  
  if (!file.exists(html_table_path)) {
    stop("HTML file does not exist: ", html_table_path)
  }
  
  table_html <- paste(readLines(html_table_path), collapse = "\n")
  
  full_html <- paste0(
    "<html><head><style>
      @page { size: A4; margin: 0.5cm; }

      body {
        margin: 0;
        padding: 0;
        font-family: Arial, sans-serif;
      }

      .fit-page {
        transform: scale(1);
        transform-origin: top left;
        width: 100%;
        page-break-inside: avoid;
      }

      table {
        border-collapse: collapse;
        width: 100%;
        page-break-inside: avoid;
      }

      td, th {
        padding: 2px 4px;
        font-size: 8pt;
        white-space: nowrap;
      }
    </style></head><body>",
    "<div class='fit-page'>",
    table_html,
    "</div></body></html>"
  )
  
  tmp_html <- tempfile(fileext = ".html")
  writeLines(full_html, tmp_html)
  
  args <- list(
    input  = normalizePath(tmp_html),
    output = normalizePath(pdf_path),
    extra_args = c(
      "--no-sandbox",
      "--disable-gpu",
      "--headless",
      "--disable-dev-shm-usage",
      "--disable-software-rasterizer"
    ),
    format = "pdf"
  )
  
  if (!is.null(chrome_path)) {
    args$browser <- chrome_path
  }
  
  do.call(pagedown::chrome_print, args)
  
  invisible(pdf_path)
}
