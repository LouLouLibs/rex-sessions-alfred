-- Test the actual snapshot script without a running Rex.
local script = assert(arg[1])
local function snapshot(failed_method, throws, empty)
  local optional_calls = 0
  rex = {
    args = {},
    json = {encode = function(value) return value end},
    call = function(method)
      if method == failed_method then
        if throws then error("connection lost") end
        return nil, "connection lost"
      end
      if method == "session.list" then
        return {sessions = empty and {} or {{session_id = "session:1", label = "notes"}}}
      elseif method == "session.attach" then
        return {client_id = "client:1", view = {}}
      elseif method == "session.view" then
        return {windows = {{window_id = "window:1", focused_block_id = "block:1",
          layers = {{blocks = {{block_id = "block:1", creator_name = "com.superlogical.terminal"}}}}}}}
      elseif method == "block.method" then
        optional_calls = optional_calls + 1
        if throws then error("optional read failed") end
        return nil, "optional read failed"
      end
      return {}
    end,
  }
  local ok, result = pcall(dofile, script)
  return ok, result, optional_calls
end

for _, method in ipairs({"session.list", "session.attach", "session.view"}) do
  for _, throws in ipairs({false, true}) do
    local ok, err = snapshot(method, throws)
    assert(not ok and tostring(err):find("connection lost", 1, true), method .. " must fail")
  end
end
for _, throws in ipairs({false, true}) do
  local ok, result, calls = snapshot(nil, throws)
  assert(ok and #result.sessions == 1)
  local windows = result.sessions[1].windows
  assert(#windows == 1 and windows[1].window_id == "window:1")
  assert(windows[1].html == "" and windows[1].title == "" and windows[1].pwd == "")
  assert(calls == 4, "process, title, pwd and format should all be attempted")
end
local ok, result = snapshot(nil, false, true)
assert(ok and #result.sessions == 0, "a genuinely empty inventory must succeed")
print("snapshot checks passed")
