# Refer to Notes-Python-Debugging.txt for a more detailed description

# Step 1: Run from Terminal with "python FILE"
# Step 2: Uncomment "import pdb; pdb.set_trace()" and run again
# Step 3: When the breakpoint is hit, execution will stop
#   Step lines with "n"
#   Type "x" to see the current value of this variable
# Step 4: Uncomment "x = x / 0" and repeat. You get an exception on this line.

print("hello")
x = 5
print("x =", x)
#import pdb; pdb.set_trace()
print("1")
x = (x * 2)
print("x =", x)
#x = x / 0          # Crash!
print("2")
print("world!")
