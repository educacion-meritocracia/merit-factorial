-- CriticMarkup rendering for HTML output.
--
--   {++added++}  {--deleted--}  {~~old~>new~~}  {==highlight==}{>>comment<<}
--   an optional initials tag {>>JC<<} may follow an addition/deletion/substitution.
--
-- Mode (YAML `criticmarkup:`): show (default) | accept | reject
--   show   - additions green, deletions red strikeout, highlights/comments marked
--   accept - apply all changes, drop comments
--   reject - revert all changes, drop comments
--
-- Only active for HTML output; other formats are left untouched.

if not quarto.doc.is_format("html") then
  return {}
end

local MARKERS = {
  { "{++", "open", "A" }, { "++}", "close", "A" },
  { "{--", "open", "D" }, { "{\u{2013}", "open", "D" },   -- smart typography turns -- into an en dash
  { "--}", "close", "D" }, { "\u{2013}}", "close", "D" },
  { "{~~", "open", "S" }, { "~~}", "close", "S" },
  { "{==", "open", "H" }, { "==}", "close", "H" },
  { "{>>", "open", "C" }, { "<<}", "close", "C" },
  { "~>", "sep", "S" },
}

local CSS = [[
<style>
.cm-add { color: #2e8b2e; background: rgba(46,158,46,.13); border-radius: 2px; }
.cm-del { color: #d03a3a; background: rgba(217,64,64,.10); text-decoration: line-through; border-radius: 2px; }
.cm-hl  { background: rgba(255,214,0,.35); border-radius: 2px; }
.cm-note { font-size: .82em; color: #4a7fd4; background: rgba(128,128,128,.18); border-radius: 3px; padding: 0 .3em; }
</style>
]]

local mode = "show"

local function is_table(x) return type(x) == "table" and x.m ~= nil end

-- Split a Str into plain Str pieces and marker tokens.
local function tokenize_str(text, out)
  local pos = 1
  while pos <= #text do
    local best_s, best_e, best
    for _, mk in ipairs(MARKERS) do
      local s, e = string.find(text, mk[1], pos, true)
      if s and (not best_s or s < best_s) then
        best_s, best_e, best = s, e, mk
      end
    end
    if not best_s then
      out[#out + 1] = pandoc.Str(text:sub(pos))
      return
    end
    if best_s > pos then
      out[#out + 1] = pandoc.Str(text:sub(pos, best_s - 1))
    end
    out[#out + 1] = { m = best[2], kind = best[3], lit = best[1] }
    pos = best_e + 1
  end
end

local flatten
flatten = function(inlines, out)
  local i = 1
  while i <= #inlines do
    local el = inlines[i]
    if el.t == "Str" then
      tokenize_str(el.text, out)
    elseif el.t == "Strikeout" then
      -- {~~old~>new~~} is parsed by pandoc as "{" Strikeout "}"
      local prev = out[#out]
      local nxt = inlines[i + 1]
      if prev and not is_table(prev) and prev.t == "Str" and prev.text:sub(-1) == "{"
         and nxt and nxt.t == "Str" and nxt.text:sub(1, 1) == "}" then
        local t = prev.text:sub(1, -2)
        if t == "" then out[#out] = nil else out[#out] = pandoc.Str(t) end
        out[#out + 1] = { m = "open", kind = "S", lit = "{~~" }
        flatten(el.content, out)
        out[#out + 1] = { m = "close", kind = "S", lit = "~~}" }
        local rest = nxt.text:sub(2)
        if rest ~= "" then tokenize_str(rest, out) end
        i = i + 1 -- consumed the "}" Str
      else
        out[#out + 1] = el
      end
    else
      out[#out + 1] = el
    end
    i = i + 1
  end
end

-- Build a tree of { cm = kind, ... } nodes and plain inlines.
local function parse(tokens)
  local root = { buf = {} }
  local stack = { root }
  local changed = false
  local function top() return stack[#stack] end
  for _, tok in ipairs(tokens) do
    if is_table(tok) then
      if tok.m == "open" then
        stack[#stack + 1] = { kind = tok.kind, lit = tok.lit, buf = {} }
      elseif tok.m == "sep" then
        local f = top()
        if f.kind == "S" and not f.old then
          f.old = f.buf
          f.buf = {}
        else
          local b = top().buf
          b[#b + 1] = pandoc.Str(tok.lit)
        end
      else -- close
        local f = top()
        if f.kind == tok.kind and #stack > 1 then
          stack[#stack] = nil
          local node = { cm = f.kind }
          if f.kind == "S" then
            node.old, node.new = f.old or f.buf, f.old and f.buf or {}
          else
            node.content = f.buf
          end
          local b = top().buf
          b[#b + 1] = node
          changed = true
        else
          local b = top().buf
          b[#b + 1] = pandoc.Str(tok.lit)
        end
      end
    else
      local b = top().buf
      b[#b + 1] = tok
    end
  end
  -- unclosed openers: emit literally
  while #stack > 1 do
    local f = stack[#stack]
    stack[#stack] = nil
    local b = top().buf
    b[#b + 1] = pandoc.Str(f.lit)
    for _, x in ipairs(f.old or {}) do b[#b + 1] = x end
    if f.old then b[#b + 1] = pandoc.Str("~>") end
    for _, x in ipairs(f.buf) do b[#b + 1] = x end
  end
  return root.buf, changed
end

local function stringify(items)
  local parts = {}
  for _, x in ipairs(items) do
    if not (type(x) == "table" and x.cm) then
      parts[#parts + 1] = pandoc.utils.stringify(x)
    end
  end
  return table.concat(parts)
end

-- Attach initials tags / comments to the preceding change.
local function attach(items)
  local out = {}
  local i = 1
  while i <= #items do
    local x = items[i]
    local nxt = items[i + 1]
    if type(x) == "table" and x.cm and (x.cm == "A" or x.cm == "D" or x.cm == "S")
       and type(nxt) == "table" and nxt.cm == "C" then
      local s = stringify(nxt.content)
      if s:match("^[^%s:{}<]+$") and #s <= 8 then
        x.author = s
        i = i + 1
      end
    elseif type(x) == "table" and x.cm == "H" and type(nxt) == "table" and nxt.cm == "C" then
      x.note = nxt.content
      i = i + 1
    end
    if type(x) == "table" and x.cm then
      for _, key in ipairs({ "content", "old", "new" }) do
        if x[key] then x[key] = attach(x[key]) end
      end
      if x.note then x.note = attach(x.note) end
    end
    out[#out + 1] = x
    i = i + 1
  end
  return out
end

local render

local function span(class, items, author)
  local attrs = {}
  if author then attrs[#attrs + 1] = { "title", author } end
  return pandoc.Span(render(items), pandoc.Attr("", { class }, attrs))
end

-- Split "JC: text" into author and text inlines.
local function split_note(items)
  local first = items[1]
  if first and not is_table(first) and first.t == "Str" then
    local who = first.text:match("^([^%s:{}<]+):$")
    if who then
      local rest = {}
      local start = 2
      if items[2] and items[2].t == "Space" then start = 3 end
      for j = start, #items do rest[#rest + 1] = items[j] end
      return who, rest
    end
  end
  return nil, items
end

local function note_span(items)
  local who, text = split_note(items)
  local inl = { pandoc.Str("[") }
  if who then
    inl[#inl + 1] = pandoc.Strong({ pandoc.Str(who) })
    inl[#inl + 1] = pandoc.Str(": ")
  end
  for _, x in ipairs(render(text)) do inl[#inl + 1] = x end
  inl[#inl + 1] = pandoc.Str("]")
  return pandoc.Span(inl, pandoc.Attr("", { "cm-note" }))
end

render = function(items)
  local out = {}
  local function add(list)
    for _, x in ipairs(list) do out[#out + 1] = x end
  end
  for _, x in ipairs(items) do
    if type(x) == "table" and x.cm then
      local k = x.cm
      if mode == "accept" then
        if k == "A" or k == "H" then add(render(x.content))
        elseif k == "S" then add(render(x.new)) end
      elseif mode == "reject" then
        if k == "D" or k == "H" then add(render(x.content))
        elseif k == "S" then add(render(x.old)) end
      else
        if k == "A" then out[#out + 1] = span("cm-add", x.content, x.author)
        elseif k == "D" then out[#out + 1] = span("cm-del", x.content, x.author)
        elseif k == "S" then
          out[#out + 1] = span("cm-del", x.old, x.author)
          out[#out + 1] = span("cm-add", x.new, x.author)
        elseif k == "H" then
          out[#out + 1] = span("cm-hl", x.content)
          if x.note then
            out[#out + 1] = pandoc.Str(" ")
            out[#out + 1] = note_span(x.note)
          end
        elseif k == "C" then out[#out + 1] = note_span(x.content) end
      end
    else
      out[#out + 1] = x
    end
  end
  return out
end

local function process(inlines)
  local tokens = {}
  flatten(inlines, tokens)
  local tree, changed = parse(tokens)
  if not changed then return nil end
  return pandoc.Inlines(render(attach(tree)))
end

function Pandoc(doc)
  local m = doc.meta.criticmarkup
  if m then mode = pandoc.utils.stringify(m) end
  if mode ~= "accept" and mode ~= "reject" then
    mode = "show"
    quarto.doc.include_text("in-header", CSS)
  end
  -- walk() on the document covers both the body and metadata (e.g. a YAML abstract)
  return doc:walk({ Inlines = process })
end
