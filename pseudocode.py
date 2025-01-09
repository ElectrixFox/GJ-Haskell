m = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

def Ars(m, r, s, lam):
    tm = [lam * x for x in m[r - 1]]
    resm = []
    for i in range(0, len(m)):
        if i == s - 1:
            resm += [[m[s - 1][p] + tm[p] for p in range(len(tm))]]
        else:
            resm += [m[i]]
    return resm

print(m)
print(Ars(m, 1, 2, 3))

def Mr(m, r, lam):
    return [m[i] if i != r else
            [lam * x for x in m[i]]
            for i in range(len(m))]

print(m)
print(Mr(m, 1, 2))