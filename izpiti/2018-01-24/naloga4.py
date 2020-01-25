#from functools import lru_cache

test = [[2,4,1,1], [3,2,0,5], [8,0,7,2]]

def max_jabolk(matrix, koraki):

    #@lru_cache(maxsize=None)
    def skoci(vr, st, k):
        J = matrix[vr][st]
        if k == 0:
            return 0
        elif vr == (len(matrix) - 1): # Ne moremo dol
            if st == len(matrix[0]) - 1:
                return J
            else:
                return J + skoci(vr, st+1, k-1)
        else: # Lahko gremo dol
            if st == len(matrix[0]) - 1: # Ne moremo desno
                return J  + skoci(vr+1, 0, k-1)
            else: # Lahko gremo desno
                return J + max(skoci(vr, st+1, k-1), skoci(vr+1, 0, k-1))
    return skoci(0, 0, koraki)






