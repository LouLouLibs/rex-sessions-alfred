-- Snapshot every session and tab on one Rex server, in one connection.
-- Run: rex [-S HOST] do snapshot.lua tail=70
-- Prints a JSON string: {sessions = [{session_id, label, active_window_id,
--   windows = [{window_id, label, panes, labels, title, pwd, html,
--     programs = [{focused, argv0, name, invoked_path}]}]}]}
local tail = tonumber(rex.args.tail) or 70

-- Optional pane metadata may disappear while taking a snapshot.
local function try(method, args)
  local ok, res = pcall(rex.call, method, args)
  if ok then return res end
  return nil
end

-- A failed inventory is not an empty inventory. Let the caller retain its cache.
local function required(method, args)
  local res, err = rex.call(method, args)
  if err ~= nil then error(method .. ": " .. tostring(err)) end
  if res == nil then error(method .. ": no result") end
  return res
end

-- Keep only the last `tail` lines; rex.json can't ship megabytes of scrollback.
local function last_lines(s, n)
  local count, i = 0, #s
  while i > 0 do
    if s:byte(i) == 10 then
      count = count + 1
      if count > n then return s:sub(i + 1), true end
    end
    i = i - 1
  end
  return s, false
end

-- The program in front in a terminal pane: what Rex picks the pane's icon by.
local function foreground_program(session_id, block)
  if block.creator_name ~= "com.superlogical.terminal" then
    return {argv0 = "", name = block.flavor or "", invoked_path = ""}
  end
  local p = try("block.method", {session_id = session_id, block_id = block.block_id,
                                 method = "process"})
  local front = p and (p.foreground or p.child) or {}
  return {argv0 = front.argv0 or "", name = front.name or "", invoked_path = front.invoked_path or ""}
end

local out = {}
local list = required("session.list", {})
for _, s in ipairs(list.sessions or {}) do
  -- A script may only read sessions it is attached to.
  required("session.attach", {session_id = s.session_id})
  local view = required("session.view", {session_id = s.session_id})
  local sess = {
    session_id = s.session_id,
    label = s.label or "",
    active_window_id = view and view.active_window_id or "",
    windows = {},
  }
  for wi, w in ipairs(view and view.windows or {}) do
    local labels, panes, programs = {}, 0, {}
    for _, layer in ipairs(w.layers or {}) do
      for _, b in ipairs(layer.blocks or {}) do
        panes = panes + 1
        if b.label and b.label ~= "" then labels[#labels + 1] = b.label end
        local program = foreground_program(s.session_id, b)
        program.focused = b.block_id == w.focused_block_id
        programs[#programs + 1] = program
      end
    end
    local win = {
      window_id = w.window_id,
      label = w.label or "",
      index = wi,
      panes = panes,
      labels = labels,
      programs = programs,
      title = "", pwd = "", html = "", truncated = false,
    }
    local bid = w.focused_block_id
    if bid then
      local t = try("block.method", {session_id = s.session_id, block_id = bid, method = "title"})
      if t and t.title then win.title = t.title end
      local p = try("block.method", {session_id = s.session_id, block_id = bid, method = "pwd"})
      if p and p.pwd then win.pwd = p.pwd end
      local f = try("block.method", {session_id = s.session_id, block_id = bid, method = "format",
                                     args = {format = "html"}})
      if f and f.content then win.html, win.truncated = last_lines(f.content, tail) end
    end
    sess.windows[#sess.windows + 1] = win
  end
  try("session.detach", {session_id = s.session_id})
  out[#out + 1] = sess
end

return rex.json.encode({sessions = out})
