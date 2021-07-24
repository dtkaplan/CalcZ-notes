buttons <-
'<div id="choices" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="choices-label">
  <label class="control-label" id="choices-label" for="choices">Choices</label>
  <div class="shiny-options-group">
    <div class="radio">
      <label>
        <input type="radio" name="choices" value="A" checked="checked"/>
        <span>A</span>
      </label>
    </div>
    <div class="radio">
      <label>
        <input type="radio" name="choices" value="B"/>
        <span>B</span>
      </label>
    </div>
    <div class="radio">
      <label>
        <input type="radio" name="choices" value="C"/>
        <span>C</span>
      </label>
    </div>
    <div class="radio">
      <label>
        <input type="radio" name="choices" value="D"/>
        <span>D</span>
      </label>
    </div>
  </div>
</div>'


b2 <- '
<div id="choices" class="form-group shiny-input-radiogroup shiny-input-container" role="radiogroup" aria-labelledby="choices-label">
  <label class="control-label" id="choices-label" for="choices">Choices</label>
  <div class="shiny-options-group">
    <div class="radio">
      <label>
        <input type="radio" name="choices" value="1" checked="checked"/>
        <div>First one</div>
        <div id="choice1" class="shiny-html-output"></div>
        <div id="picture1" class="shiny-image-output" style="width:30%;"></div>
      </label>
    </div>
    <div class="radio">
      <label>
        <input type="radio" name="choices" value="2"/>
        <div>Second one</div>
        <div id="choice2" class="shiny-html-output"></div>
        <div id="picture2" class="shiny-image-output" style="width:30%;"></div>
      </label>
    </div>
    <div class="radio">
      <label>
        <input type="radio" name="choices" value="3"/>
        <div>Third one \\(x^2 + \\sin(\\sqrt{x}\\)</div>
        <div id="choice3" class="shiny-html-output"></div>
      </label>
    </div>
  </div>
</div>'
