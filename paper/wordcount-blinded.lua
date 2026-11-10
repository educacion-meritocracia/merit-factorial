-- Saves the total word count of the blinded (anonymous) manuscript to
-- wordcount-blinded.txt, so the author-details version can report it
-- with the {{< words-blinded >}} shortcode (see wordcount-blinded-shortcode.lua).
-- Must run after _extensions/andrewheiss/wordcount/wordcount.lua.

function Meta(m)
  if m.blinded and pandoc.utils.stringify(m.blinded) == "true"
     and m.wordcount_total_words ~= nil then
    local dir = pandoc.path.directory(quarto.doc.input_file)
    local f = io.open(pandoc.path.join({ dir, "wordcount-blinded.txt" }), "w")
    if f then
      f:write(pandoc.utils.stringify(m.wordcount_total_words), "\n")
      f:close()
    end
  end
end
