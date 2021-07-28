#' Radio buttons that display HTML
#'
#' @param choices a character vector, one for each choice
#'
#' @export
bigRadioButtons <- function(id, label, choices, origNum) {
  head <- glue::glue('
  <div id="{id}" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="choices-label">
  <label class="control-label" id="{id}-label" for="{id}">{label}</label>
  <div class="shiny-options-group">
')

  buttons <- character(0)
  for (k in 1:length(choices)) {
    buttons[k] <- bigRadioButtonItem(id, origNum[k], choices[k])
  }


  tail <- '  </div>
  </div>
  '

  paste(c(head, buttons, tail), collapse="\n")
}

#' @param id the same id given to `bigRadioButtons`
#' @param value identifier for the individual radio button, typically, 1, 2, 3, ...
#' for successive buttons.
#' @param string the HTML string in the `<div>` for the button.
#' @export
bigRadioButtonItem <- function(id, value, string) {
  item_template <- '
    <div class="radio">
      <label>
        <input type="radio" name="{id}" value="{value}"/>
        <div id="{id}{value}" class="shiny-html-output MCchoice">{string}</div>
      </label>
    </div>
  '

    glue::glue(item_template)

}
