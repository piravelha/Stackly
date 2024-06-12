arr = []

for i in range(1000000):
    if i % 15 == 0:
        arr.insert(0, "fizzbuzz")
    elif i % 5 == 0:
        arr.insert(0, "buzz")
    elif i % 3 == 0:
        arr.insert(0, "fizz")

print(arr)