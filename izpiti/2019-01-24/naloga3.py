#from functools import lru_cache

def pobeg_zabe(mocvara):
    #@lru_cache(maxsize=None)
    def pobeg(k, e):
        if k >= len(mocvara):
            return 0
        else:
            e += mocvara[k]
            return 1 + min([pobeg(k + d, e - d) for d in range(1, e + 1)])
    return pobeg(0, 0)

test1 = [2, 4, 1, 2, 1, 3, 1, 1, 5]  # Should be 3
test2 = [4, 1, 8, 2, 11, 1, 1, 1, 1, 1]  # Should be 2
