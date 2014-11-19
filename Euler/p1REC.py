def main(i, lim, total):
    if i == lim:
        print("Total: "+ str(total))
    else:
        if i % 3 == 0 or i % 5 == 0:
            total += i
            print(i)
        main(i + 1, lim, total)