-- Swayimg configuration
-- vim: filetype=lua

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

swayimg.set_window_size(1280, 720)
swayimg.decoration = false
swayimg.antialiasing = true

--------------------------------------------------------------------------------
-- Image list
--------------------------------------------------------------------------------

swayimg.imagelist.order = "alpha"

--------------------------------------------------------------------------------
-- Font / text overlay
--------------------------------------------------------------------------------

swayimg.text.font = "monospace"
swayimg.text.size = 14
swayimg.text.color = 0xffcccccc
swayimg.text.shadow = 0xd0000000
swayimg.text.background = 0x00000000
swayimg.text.padding = 10
swayimg.text.timeout = 5
swayimg.text.status_timeout = 3

swayimg.on_initialized(function() swayimg.text.visible = false end)

--------------------------------------------------------------------------------
-- Viewer mode
--------------------------------------------------------------------------------

swayimg.viewer.set_window_background(0xff000000)
swayimg.viewer.default_scale = "optimal"
swayimg.viewer.default_position = "center"
swayimg.viewer.loop = true
swayimg.viewer.preload = 1
swayimg.viewer.drag_button = "MouseLeft"

swayimg.viewer.set_text("topleft", {
  "{name}", "{format}", "{sizehr}", "{frame.width}x{frame.height}",
  "{meta.Exif.Photo.DateTimeOriginal}", "{meta.Exif.Image.Model}",
})
swayimg.viewer.set_text("topright", { "{list.index}/{list.total}" })
swayimg.viewer.set_text("bottomleft", { "{scale}" })
swayimg.viewer.set_text("bottomright", {})

swayimg.viewer.on_signal("USR2", function()
  swayimg.viewer.open("next")
end)

-- viewer key bindings
swayimg.viewer.bind_reset()

swayimg.viewer.on_key("Home",           function() swayimg.viewer.open("first") end)
swayimg.viewer.on_key("g",              function() swayimg.viewer.open("first") end)
swayimg.viewer.on_key("End",            function() swayimg.viewer.open("last") end)
swayimg.viewer.on_key("Shift-g",        function() swayimg.viewer.open("last") end)
swayimg.viewer.on_key("Prior",          function() swayimg.viewer.open("prev") end)
swayimg.viewer.on_key("BackSpace",      function() swayimg.viewer.open("prev") end)
swayimg.viewer.on_key("Next",           function() swayimg.viewer.open("next") end)
swayimg.viewer.on_key("Space",          function() swayimg.viewer.open("next") end)
swayimg.viewer.on_key("Shift-r",        function() swayimg.viewer.open("random") end)
swayimg.viewer.on_key("Shift-d",        function() swayimg.viewer.open("prev_dir") end)
swayimg.viewer.on_key("d",              function() swayimg.viewer.open("next_dir") end)
swayimg.viewer.on_key("Shift-o",        function() swayimg.viewer.frame = swayimg.viewer.frame - 1 end)
swayimg.viewer.on_key("o",              function() swayimg.viewer.frame = swayimg.viewer.frame + 1 end)
swayimg.viewer.on_key("c",              function()
  swayimg.imagelist.remove(swayimg.viewer.get_image().path)
end)
swayimg.viewer.on_key("s",              function() swayimg.mode = "slideshow" end)
swayimg.viewer.on_key("n",              function()
  swayimg.viewer.animation = not swayimg.viewer.animation
end)
swayimg.viewer.on_key("f",              function() swayimg.fullscreen = not swayimg.fullscreen end)
swayimg.viewer.on_key("Return",         function() swayimg.mode = "gallery" end)
swayimg.viewer.on_key("Left",           function()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(pos.x - 10, pos.y)
end)
swayimg.viewer.on_key("Right",          function()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(pos.x + 10, pos.y)
end)
swayimg.viewer.on_key("Up",             function()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(pos.x, pos.y - 10)
end)
swayimg.viewer.on_key("Down",           function()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(pos.x, pos.y + 10)
end)
swayimg.viewer.on_key("Equal",          function()
  swayimg.viewer.set_abs_scale(swayimg.viewer.scale * 1.1)
end)
swayimg.viewer.on_key("Plus",           function()
  swayimg.viewer.set_abs_scale(swayimg.viewer.scale * 1.1)
end)
swayimg.viewer.on_key("Minus",          function()
  swayimg.viewer.set_abs_scale(swayimg.viewer.scale * 0.9)
end)
swayimg.viewer.on_key("w",              function() swayimg.viewer.set_fix_scale("width") end)
swayimg.viewer.on_key("Shift-w",        function() swayimg.viewer.set_fix_scale("height") end)
swayimg.viewer.on_key("z",              function() swayimg.viewer.set_fix_scale("fit") end)
swayimg.viewer.on_key("Shift-z",        function() swayimg.viewer.set_fix_scale("fill") end)
swayimg.viewer.on_key("0",              function() swayimg.viewer.set_fix_scale("real") end)
swayimg.viewer.on_key("Shift-BackSpace",function() swayimg.viewer.set_fix_scale("optimal") end)
swayimg.viewer.on_key("k",              function() swayimg.viewer.set_fix_scale("keep") end)
swayimg.viewer.on_key("Alt-s",          function() swayimg.viewer.reset() end)
swayimg.viewer.on_key("Alt-p",          function() swayimg.viewer.set_fix_position("center") end)
swayimg.viewer.on_key("bracketleft",    function() swayimg.viewer.rotate(270) end)
swayimg.viewer.on_key("bracketright",   function() swayimg.viewer.rotate(90) end)
swayimg.viewer.on_key("m",              function() swayimg.viewer.flip_vertical() end)
swayimg.viewer.on_key("Shift-m",        function() swayimg.viewer.flip_horizontal() end)
swayimg.viewer.on_key("a",              function()
  swayimg.antialiasing = not swayimg.antialiasing
end)
swayimg.viewer.on_key("i",              function()
  swayimg.text.visible = not swayimg.text.visible
end)
swayimg.viewer.on_key("Shift-Delete",   function()
  local image = swayimg.viewer.get_image()
  os.execute("rm -f '" .. image.path .. "'")
  swayimg.text.status = "File removed: " .. image.path
  swayimg.imagelist.remove(image.path)
end)
swayimg.viewer.on_key("Escape",         function() swayimg.exit() end)
swayimg.viewer.on_key("q",              function() swayimg.exit() end)
swayimg.viewer.on_key("Ctrl-w",         function()
  os.execute("/home/nasreddin/.local/bin/setbg '" .. swayimg.viewer.get_image().path .. "'")
end)
swayimg.viewer.on_key("Ctrl-Shift-w",   function()
  os.execute("/home/nasreddin/.local/bin/setbg --tile '" .. swayimg.viewer.get_image().path .. "'")
end)

-- viewer mouse bindings
swayimg.viewer.on_mouse("ScrollLeft",       function()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(pos.x + 5, pos.y)
end)
swayimg.viewer.on_mouse("ScrollRight",      function()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(pos.x - 5, pos.y)
end)
swayimg.viewer.on_mouse("ScrollUp",         function()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(pos.x, pos.y - 5)
end)
swayimg.viewer.on_mouse("ScrollDown",       function()
  local pos = swayimg.viewer.get_position()
  swayimg.viewer.set_abs_position(pos.x, pos.y + 5)
end)
swayimg.viewer.on_mouse("Ctrl-ScrollUp",    function()
  local pos = swayimg.get_mouse_pos()
  swayimg.viewer.set_abs_scale(swayimg.viewer.scale * 1.1, pos.x, pos.y)
end)
swayimg.viewer.on_mouse("Ctrl-ScrollDown",  function()
  local pos = swayimg.get_mouse_pos()
  swayimg.viewer.set_abs_scale(swayimg.viewer.scale * 0.9, pos.x, pos.y)
end)
swayimg.viewer.on_mouse("Shift-ScrollUp",   function() swayimg.viewer.open("prev") end)
swayimg.viewer.on_mouse("Shift-ScrollDown", function() swayimg.viewer.open("next") end)
swayimg.viewer.on_mouse("Alt-ScrollUp",     function() swayimg.viewer.frame = swayimg.viewer.frame - 1 end)
swayimg.viewer.on_mouse("Alt-ScrollDown",   function() swayimg.viewer.frame = swayimg.viewer.frame + 1 end)
swayimg.viewer.on_mouse("MouseSide",        function() swayimg.viewer.open("prev") end)
swayimg.viewer.on_mouse("MouseExtra",       function() swayimg.viewer.open("next") end)

--------------------------------------------------------------------------------
-- Slideshow mode
--------------------------------------------------------------------------------

swayimg.slideshow.timeout = 3
swayimg.slideshow.set_window_background("auto")
swayimg.slideshow.set_image_background(0xff000000)
swayimg.slideshow.default_scale = "fit"
swayimg.slideshow.default_position = "center"

swayimg.slideshow.set_text("topleft",    {})
swayimg.slideshow.set_text("topright",   {})
swayimg.slideshow.set_text("bottomleft", {})
swayimg.slideshow.set_text("bottomright", { "{dir}" })

swayimg.slideshow.on_signal("USR2", function()
  swayimg.slideshow.open("next")
end)

local slideshow_paused = false
local slideshow_timeout = 3

swayimg.slideshow.bind_reset()

swayimg.slideshow.on_key("Home",    function() swayimg.slideshow.open("first") end)
swayimg.slideshow.on_key("g",       function() swayimg.slideshow.open("first") end)
swayimg.slideshow.on_key("End",     function() swayimg.slideshow.open("last") end)
swayimg.slideshow.on_key("Shift-g", function() swayimg.slideshow.open("last") end)
swayimg.slideshow.on_key("Prior",   function() swayimg.slideshow.open("prev") end)
swayimg.slideshow.on_key("Next",    function() swayimg.slideshow.open("next") end)
swayimg.slideshow.on_key("Shift-r", function() swayimg.slideshow.open("random") end)
swayimg.slideshow.on_key("Shift-d", function() swayimg.slideshow.open("prev_dir") end)
swayimg.slideshow.on_key("d",       function() swayimg.slideshow.open("next_dir") end)
swayimg.slideshow.on_key("Space",   function()
  if slideshow_paused then
    swayimg.slideshow.timeout = slideshow_timeout
    slideshow_paused = false
  else
    swayimg.slideshow.timeout = 86400
    slideshow_paused = true
  end
end)
swayimg.slideshow.on_key("i",       function()
  swayimg.text.visible = not swayimg.text.visible
end)
swayimg.slideshow.on_key("f",       function() swayimg.fullscreen = not swayimg.fullscreen end)
swayimg.slideshow.on_key("Return",  function() swayimg.mode = "viewer" end)
swayimg.slideshow.on_key("Escape",  function() swayimg.exit() end)
swayimg.slideshow.on_key("q",       function() swayimg.exit() end)

--------------------------------------------------------------------------------
-- Gallery mode
--------------------------------------------------------------------------------

swayimg.gallery.thumb_size = 200
swayimg.gallery.cache = 100
swayimg.gallery.preload = false
swayimg.gallery.pstore = false
swayimg.gallery.aspect = "fill"
swayimg.gallery.window_color = 0xff000000
swayimg.gallery.unselected_color = 0xff202020
swayimg.gallery.selected_color = 0xff404040
swayimg.gallery.border_color = 0xff000000

swayimg.gallery.set_text("topleft",    {})
swayimg.gallery.set_text("topright",   { "{list.index}/{list.total}" })
swayimg.gallery.set_text("bottomleft", {})
swayimg.gallery.set_text("bottomright", { "{name}" })

swayimg.gallery.bind_reset()

swayimg.gallery.on_key("Home",        function() swayimg.gallery.select("first") end)
swayimg.gallery.on_key("g",           function() swayimg.gallery.select("first") end)
swayimg.gallery.on_key("End",         function() swayimg.gallery.select("last") end)
swayimg.gallery.on_key("Shift-g",     function() swayimg.gallery.select("last") end)
swayimg.gallery.on_key("Left",        function() swayimg.gallery.select("left") end)
swayimg.gallery.on_key("h",           function() swayimg.gallery.select("left") end)
swayimg.gallery.on_key("Right",       function() swayimg.gallery.select("right") end)
swayimg.gallery.on_key("l",           function() swayimg.gallery.select("right") end)
swayimg.gallery.on_key("Up",          function() swayimg.gallery.select("up") end)
swayimg.gallery.on_key("k",           function() swayimg.gallery.select("up") end)
swayimg.gallery.on_key("Down",        function() swayimg.gallery.select("down") end)
swayimg.gallery.on_key("j",           function() swayimg.gallery.select("down") end)
swayimg.gallery.on_key("Prior",       function() swayimg.gallery.select("pgup") end)
swayimg.gallery.on_key("Next",        function() swayimg.gallery.select("pgdown") end)
swayimg.gallery.on_key("c",           function()
  swayimg.imagelist.remove(swayimg.gallery.get_image().path)
end)
swayimg.gallery.on_key("s",           function() swayimg.mode = "slideshow" end)
swayimg.gallery.on_key("f",           function() swayimg.fullscreen = not swayimg.fullscreen end)
swayimg.gallery.on_key("Return",      function() swayimg.mode = "viewer" end)
swayimg.gallery.on_key("a",           function()
  swayimg.antialiasing = not swayimg.antialiasing
end)
swayimg.gallery.on_key("i",           function()
  swayimg.text.visible = not swayimg.text.visible
end)
swayimg.gallery.on_key("Equal",       function()
  swayimg.gallery.thumb_size = swayimg.gallery.thumb_size + 20
end)
swayimg.gallery.on_key("Plus",        function()
  swayimg.gallery.thumb_size = swayimg.gallery.thumb_size + 20
end)
swayimg.gallery.on_key("Minus",       function()
  swayimg.gallery.thumb_size = swayimg.gallery.thumb_size - 20
end)
swayimg.gallery.on_key("Shift-Delete", function()
  local image = swayimg.gallery.get_image()
  os.execute("rm -f '" .. image.path .. "'")
  swayimg.text.status = "File removed: " .. image.path
  swayimg.imagelist.remove(image.path)
end)
swayimg.gallery.on_key("Escape",      function() swayimg.exit() end)
swayimg.gallery.on_key("q",           function() swayimg.exit() end)
swayimg.gallery.on_key("Ctrl-w",      function()
  os.execute("/home/nasreddin/.local/bin/setbg '" .. swayimg.gallery.get_image().path .. "'")
end)
swayimg.gallery.on_key("Ctrl-Shift-w", function()
  os.execute("/home/nasreddin/.local/bin/setbg --tile '" .. swayimg.gallery.get_image().path .. "'")
end)

-- gallery mouse bindings
swayimg.gallery.on_mouse("ScrollLeft",      function() swayimg.gallery.select("right") end)
swayimg.gallery.on_mouse("ScrollRight",     function() swayimg.gallery.select("left") end)
swayimg.gallery.on_mouse("ScrollUp",        function() swayimg.gallery.select("up") end)
swayimg.gallery.on_mouse("ScrollDown",      function() swayimg.gallery.select("down") end)
swayimg.gallery.on_mouse("Ctrl-ScrollUp",   function()
  swayimg.gallery.thumb_size = swayimg.gallery.thumb_size + 20
end)
swayimg.gallery.on_mouse("Ctrl-ScrollDown", function()
  swayimg.gallery.thumb_size = swayimg.gallery.thumb_size - 20
end)
swayimg.gallery.on_mouse("MouseLeft",       function() swayimg.mode = "viewer" end)
