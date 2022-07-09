a = "one string"
b = string.gsub(a, "one", "another")  -- change string parts
print(a)
print(b)

a = "a line"
b = 'another line' -- Both single and double quotes constitute a string

--[[
-- Escape sequences:
-- \a   bell
-- \b   back space
-- \f   form feed
-- \n   newline
-- \r   carriage return
-- \t   horizontal tab
-- \v   vertical tab
-- \\   backslash
-- \"   double quote
-- \'   single quote
-- \[   left square bracket
-- \]   right square bracket
--]]

-- Illustration of escape sequences:
print("one line\nnext line\n\"in quotes\", 'in quotes'")
print('a backslash inside quotes: \'\\\'')
print("a simpler way: '\\'")

-- Bracketed String literals (for lua5.2+ include n '=' in long format)
page = [==[
<HTML>
<HEAD>
<TITLE>An HTML Page</TITLE>
</HEAD>
<BODY>
  <A HREF="http://www.lua.org">Lua</A>
  [[a text between double brackets]]
</BODY>
</HTML>
]==]

print(page)

-- Automatice conversions at runtime
print("10" + 1)       --> 11
print("10 + 1")       --> 10 + 1
print("-5.3e-10"*"2") --> -1.06e-09
-- print("hello" + 1) --> ERROR (cannot convert "hello")
print(10 .. 20)       --> 1020


-- Explicite type conversions
print("Enter a number: ")
line = io.read()      -- read a line
n = tonumber(line)    -- try to convert it to a number
if n == nil then
  error(line .. " is not a valid number")
else
  print(n*2)
end

print(tostring(10) == "10")   --> true
print(10 .. "" == "10")       --> true
