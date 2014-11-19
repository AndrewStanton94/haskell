def getInput():
    return eval(input("Limit to check: "))
    
def test(num):
    if num % 3 == 0:
        return True
    elif num % 5 == 0:
        return True
    else:
        return False

def main():
    total = 0
    values = []
    for i in range(0, getInput()):
        if test(i):
            total += i
            values.append(i)
    print("Total: " + str(total))
    print("Values " + str(values))
    
if __name__ == "__main__":
    main()