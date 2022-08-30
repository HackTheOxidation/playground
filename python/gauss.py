'''
x = gauss_elimin(a, b)
Solves [a][x] = [b] by Gauss elimination.
'''
from numpy import dot, array

def gauss_elimin(a, b):
    '''Performs gauss elimination.'''
    (rows, _) = a.shape
    # elimination phase
    for row in range(0, rows-1): # pivot equation/row
        for i in range(row+1, rows):
            if a[i, row] != 0.0:
                factor = a[i, row]/a[row, row]
                a[i, row+1:rows] = a[i, row+1:rows] - factor*a[row, row+1:rows]
                b[i] = b[i] - factor*b[row]


    print("a: ", a)
    print("b: ", b)

    # back substitution
    for k in range(rows-1,-1,-1):
        b[k] = (b[k] - dot(a[k, k+1:rows],b[k+1:rows]))/a[k, k]
    return b

#a = array([[3, 2.0], [-6, 6]])
#b = array([7, 6])
#print(gauss_elimin(a, b))

a = array([[2, 1.0, -1], [-3.0, -1, 2], [-2, 1, 2]])
b = array([8, -11, -3])
print("result: ", gauss_elimin(a, b))
