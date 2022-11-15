module TerminalScreens

using LinuxTouchEvents
using LinuxTouchEvents: BTN_TOUCH, EV_KEY
using TerminalButtons
using TerminalButtons: Rect
using Crayons

export TerminalScreen, Rect
export process_screen,
       draw_screen, update_screen, draw_field,
       control_by_name, control_text,
       control_to_left, control_to_right,
       control_value, set_control_value

export ANSI_SET_SCROLL_ROWS,
       ANSI_RESET_SCROLL_ROWS,
       ANSI_CURSOR_UP,
       ANSI_SET_CURSOR,
       ANSI_CLEAR_END,
       ANSI_CLEAR_TO_TOP,
       ANSI_CLEAR_SCREEN,
       ANSI_RESET_COLOR,
       ANSI_HIDE_CURSOR,
       ANSI_SHOW_CURSOR,
       ANSI_SAVE_CURSOR,
       ANSI_RESTORE_CURSOR


global terminal_out
global touch_in

set_terminal_out(io) = global terminal_out = io
set_touch_in(io) = global touch_in = io


mutable struct TerminalScreen
    width::Int
    height::Int
    text::String
    rows::Vector{String}
    chars::Vector{Vector{Char}}
    controls::Dict{String, Vector{Rect}}
    actions::Vector{Pair}
    update::Function
    idle::Function
    is_open::Bool
    function TerminalScreen(text; actions, update=(x)->nothing,
                                           idle=(x)->nothing)
        text = chomp(remove_border(text))
        rows = split(chomp(text), "\n")
        chars = [Vector{Char}(row) for row in rows]
        height = length(rows)
        width = maximum(length.(chars))
        return new(width, height, text, rows, chars,
                   Dict(), actions, update, idle, false)
    end
end

Base.close(ts::TerminalScreen) = ts.is_open = false



# Screen Drawing

update_screen(ts) = ts.update(ts)


function draw_screen(ts)
    print(terminal_out,
          ANSI_HIDE_CURSOR,
          ANSI_SET_CURSOR(1,1),
          ANSI_RESET_COLOR,
          ts.text)
    update_screen(ts)
end


function remove_border(text)
    join((row[nextind(row, 1):prevind(row, lastindex(row))]
          for row in split(chomp(text), "\n")[2:end-1]), "\n")

end



# Extracting Controls from Screen

"""
    find_word(screen, word) -> [(col, row), ...]

Find locations where `word` appears on screen.
"""
function find_word(ts::TerminalScreen, word)
    result = []
    for (row_i, row) in enumerate(ts.rows)
        for col_i in findall(" " * word * " ", row)
            col_i = textwidth(row[1:first(col_i)])
            push!(result, (first(col_i)+1, row_i))
        end
    end
    return result
end


"""
Is point `a` next to point `b`?
"""
function adjacent(a, b)
    col_a, row_a = a
    col_b, row_b = b
    return (row_b == row_a+1) && (abs(col_a-col_b) < 4)
end


"""
    control_by_name(screen, pattern) -> [Rect(), ...]

Find controls matching `pattern`.
"""
function control_by_name(ts, name)
    if !haskey(ts.controls, name)
        ts.controls[name] = _find_control(ts, name)
    end
    return ts.controls[name]
end


function _find_control(ts::TerminalScreen, pattern)

    # First look for `pattern` verbatim.
    locations = find_word(ts, pattern)

    # Next, split `pattern` into words.
    pattern = split(pattern)

    if length(pattern) > 1

        # Find locations for each work in pattern.
        word_locations = [find_word(ts, word) for word in pattern]

        # Return locations where all words are nearby.
        for a in word_locations[1]
            if all(any(adjacent.([a], x))
                   for x in word_locations[2:end])
                push!(locations, a)
            end
        end
    end
    return [extent(ts, p...) for p in locations]
end


"""
Find text content of control `c`.
"""
function control_text(ts, c)
    x = (c.x + 1):(c.x + c.width-1)
    y = (c.y + 1):(c.y + c.height-1)
    join(String(r[x]) for r in ts.chars[y]) |> strip
end


"""
Is the character at (`row`, `col) a control boundary?
"""
function is_box(ts, row, col) 
    row < 1 ||
    col < 1 ||
    row > ts.height ||
    col > ts.width ||
    ts.chars[row][col] ∈ "─│"
end


"""
Find the extent of the control at point (`col`, `row`).

The control is bounded by box drawing characters (see `is_box()`).
"""
function extent(ts::TerminalScreen, col, row)
    col_min = col
    col_max = col
    row_min = row
    row_max = row

    while !is_box(ts, row, col_min) col_min -= 1 end
    while !is_box(ts, row, col_max) col_max += 1 end
    while !is_box(ts, row_min, col) row_min -= 1 end
    while !is_box(ts, row_max, col) row_max += 1 end

    TerminalButtons.Rect(col_min, row_min,
                         col_max - col_min,
                         row_max - row_min)
end


"""
Control to the right of `c`.
"""
control_to_right(ts, c) = extent(ts, c.x + c.width + 1, c.y+1)


"""
Control to the left of `c`.
"""
control_to_left(ts, c) = extent(ts, c.x - 1, c.y+1)


"""
Find control that was touched at `point`.
"""
function control_for_touch(ts, point, control_name)
    for rect in control_by_name(ts, control_name)
        if point in rect
            return rect
        end
    end
    return nothing
end



# Setting Control Values


"""
Set content of named field to `v`.
"""
function draw_field(ts, name, v)
    x, y = first(find_word(ts, name))
    print(terminal_out, ANSI_SET_CURSOR(y, x), v)
end


"""
Set value of named control to `v`.
"""
set_control_value(ts, name, v; kw...) =
    set_control_value(ts, name, first(control_by_name(ts, name)), v; kw...)

function set_control_value(ts, name, c::Rect, v; pad=10)
    v = lpad(v, pad)
    row = c.y + c.height÷2
    col = c.x + c.width - (textwidth(v) + 1)
    style = crayon"fg:blue"
    print(terminal_out,
          ANSI_SET_CURSOR(row, col),
          style,
          v,
          inv(style))
end



# Touch Input

function wait_for_event(ts, touch, event)
    while true
        e, x, y = take!(touch)
        if e == event
            return round(Int, x * ts.width), round(Int, y * ts.height)
        end
    end
end

wait_for_touch(ts, touch) = wait_for_event(ts, touch, 0)
wait_for_release(ts, touch) = wait_for_event(ts, touch, 3)

function wait_for_touch(ts, touch::TouchEventChannel)
    x, y = take!(touch)
    return round(Int, x * ts.width), round(Int, y * ts.height)
end

function wait_for_release(ts, touch::TouchEventChannel)
    LinuxTouchEvents.wait_for_event(touch.io, EV_KEY, BTN_TOUCH, 0)
end



# Event Loop


function process_screen(ts::TerminalScreen; timeout=Inf)

    # Wait for touch screen input.
    if !isready(touch_in; timeout)
        ts.idle(ts)
        return
    end
    touch_point = wait_for_touch(ts, touch_in)

    # Find the control that was touched.
    for (name, action) in ts.actions
        control = control_for_touch(ts, touch_point, name) 
        if control != nothing
            # Execute the touch action.
            process_touch(ts, control, action)
            update_screen(ts)
        end
    end
end


"""
Process `action` for touch event on `control`.

`action` is either a function or a pair of functions (for press and release).
"""
function process_touch(ts, control, action)

    # Highlight the control rectangle.
    style = crayon"bg:light_magenta"
    set_style(ts, style, control)

    try
        if action isa Tuple
            # Process seperate press and release actions.
            press_action, release_action = action

            press_action(control)

            # Repeat press action while touch remains pressed.
            pressed = Ref(true)
            press_done = Condition()
            @async begin
                sleep(0.1)
                while pressed[]
                    press_action(control)
                    sleep(0.1)
                end
                notify(press_done)
            end
            wait_for_release(ts, touch_in)
            pressed[] = false
            wait(press_done)

            release_action(control)
        else
            wait_for_release(ts, touch_in)
            action(control)
        end

    finally
        # Remove the control highlight.
        if ts.is_open
            set_style(ts, inv(style), control)
        end
    end
end


# ANSI Terminal Control.

CSI(f, args...) = string("\e[", join(args, ";"), f)
macro CSI(e)
    if Meta.isexpr(e, :call)
        f = string(e.args[1])
        args = e.args[2:end]
    else
        f = string(e)
        args = ()
    end
    esc(:(CSI($f, $(args...))))
end

      ANSI_SET_SCROLL_ROWS(top, bottom) = @CSI r(top, bottom)
const ANSI_RESET_SCROLL_ROWS            = @CSI r
      ANSI_CURSOR_UP(n)                 = @CSI A(n)
      ANSI_SET_CURSOR(row, col=1)       = @CSI H(row, col)
const ANSI_CLEAR_END                    = @CSI K(0)
const ANSI_CLEAR_TO_TOP                 = @CSI J(1)
const ANSI_CLEAR_SCREEN                 = @CSI J(2)
const ANSI_RESET_COLOR                  = @CSI m(0)
const ANSI_FG_DEFAULT                   = @CSI m(39)
const ANSI_HIDE_CURSOR                  = @CSI l("?25")
const ANSI_SHOW_CURSOR                  = @CSI h("?25")
const ANSI_SAVE_CURSOR                  = "\e7"
const ANSI_RESTORE_CURSOR               = "\e8"


xywh(r::Rect) = r.x, r.y, r.width, r.height

set_style(ts, style, r::Rect) = set_style(ts, style, xywh(r)...)

function set_style(ts, style, x, y, w, h; offset=1)
    print(terminal_out, style)
    for row in y+offset:y+h-offset
        print(terminal_out, ANSI_SET_CURSOR(row, x+offset))
        print(terminal_out, String(ts.chars[row][x+offset:x+w+1-2*offset]))
    end
    print(terminal_out, inv(style))
end



end # module TerminalScreens
