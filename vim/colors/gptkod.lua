local lush = require('lush')
local hsl = lush.hsl

local theme = lush(function()
  return {
    Normal       { fg = "#eee", bg = "#222" },
    Cursor       { fg = "#ff3319" },
    Visual       { bg = "#2F3F52" },
    LineNr       { fg = "#666", bg = "#333" },
    CursorLineNr { fg = "#eee", bg = "#333", gui = "bold" },
    Comment      { fg = "#666", gui = "italic" },
    Constant     { fg = "#C0D164" },
    String       { fg = "#9aca7e", bg = "#212A24" },
    Number       { fg = "#C969B6" },
    Keyword      { fg = "#66c8ef" },
    Function     { fg = "#85FFDF", bg = "#1F2B31" },
    Type         { fg = "#ffa07a" },
    PreProc      { fg = "#9C8B7C" },
    Special      { fg = "#A19DBF" },
    Todo         { fg = "#946B57", gui = "bold" },
    MatchParen   { fg = "#66c8ef", gui = "bold" },
    Error        { fg = "#ff3319", gui = "bold" },
    StatusLine   { fg = "#eee", bg = "#444" },
    StatusLineNC { fg = "#666", bg = "#333" },
    VertSplit    { fg = "#444", bg = "#444" },
    Pmenu        { fg = "#eee", bg = "#333" },
    PmenuSel     { bg = "#2F3F52" },
    DiffAdd      { bg = "#212A24" },
    DiffDelete   { bg = "#342C22" },
    DiffChange   { bg = "#2B2F26" },
    DiffText     { bg = "#1F2B31" },
  }
end)

return theme
