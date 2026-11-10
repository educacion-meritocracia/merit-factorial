-- {{< words-blinded >}}: total word count of the blinded manuscript, as saved
-- in wordcount-blinded.txt by wordcount-blinded.lua when the anonymous
-- version is rendered. Render the anonymous version first.

return {
  ['words-blinded'] = function(args, kwargs, meta)
    local dir = pandoc.path.directory(quarto.doc.input_file)
    local f = io.open(pandoc.path.join({ dir, "wordcount-blinded.txt" }), "r")
    if not f then
      quarto.log.warning("wordcount-blinded.txt not found: render the anonymous version first")
      return pandoc.Str("[render anonymous version first]")
    end
    local n = f:read("*l")
    f:close()
    return pandoc.Str(n)
  end
}
