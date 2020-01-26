#from functools import lru_cache

#@lru_cache(maxsize=None)
def simetricen(w):
    return w == w[::-1]

def stevilo_delov(w, simetricen):
    if w == "":
        return 0
    if simetricen(w):
        return 1
    
    options = [
        stevilo_delov(w[:i], simetricen) + stevilo_delov(w[i:],simetricen) for i in range(1, len(w))
    ]
    return min(options)

def razdeli(w, f):
    if len(w) == 0:
        return (0, [w])
    if f(w):
        return (1, [w])

    options = None
    for i in range(1, len(w)):
        nl, wl = razdeli(w[:i], f)
        nr, wr = razdeli(w[i:], f)
        k, ws = nl + nr, wl + wr

        if options is None:
            options = (k, ws)

        if k < options[0]:
            options = (k, ws)

    return options