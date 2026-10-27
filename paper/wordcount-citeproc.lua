-- Replacement for _extensions/andrewheiss/wordcount/citeproc.lua.
-- Runs citeproc (so references are counted) but hides Quarto cross-references
-- (@fig-, @tbl-, ...) from it, avoiding "citation fig-... not found" warnings.
-- Cross-references are restored afterwards for Quarto to resolve.

local crossref_prefixes = {
  fig = true, tbl = true, sec = true, eq = true, lst = true, apx = true,
  thm = true, lem = true, cor = true, prp = true, cnj = true, def = true,
  exm = true, exr = true
}

local function is_crossref(cite)
  for _, c in ipairs(cite.citations) do
    local prefix = c.id:match("^(%a+)-")
    if not (prefix and crossref_prefixes[prefix]) then
      return false
    end
  end
  return true
end

function Pandoc(doc)
  local hidden = {}
  doc = doc:walk({
    Cite = function(el)
      if is_crossref(el) then
        table.insert(hidden, el)
        return pandoc.Span({}, { ["wc-xref"] = tostring(#hidden) })
      end
    end
  })
  doc = pandoc.utils.citeproc(doc)
  return doc:walk({
    Span = function(el)
      local i = el.attributes["wc-xref"]
      if i then return hidden[tonumber(i)] end
    end
  })
end
