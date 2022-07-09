
def decimal_to_hex(deci_num):
    hex_num = 0
    i = 0
    while 10**i < deci_num:
        i = i + 1

        for potens in range(i, 0, -1):
            hex_num = hex_num + (deci_num % 16**i)
            return hex_num


