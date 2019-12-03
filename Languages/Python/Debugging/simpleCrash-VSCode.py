# Refer to Notes-Python-Debugging.txt for a more detailed description

# Step 1: Set breakpoint on the first line "print("hello")". Debug and see output in Terminal window.
# Step 2: Uncomment "x = x / 0" and repeat. You get an exception on this line.

print("hello")
x = 5
print("x =", x)
print("1")
x = (x * 2)
print("x =", x)
#x = x / 0          # Crash!
print("2")
print("world!")
