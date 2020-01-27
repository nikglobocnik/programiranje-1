#from functools import lru_cache

def count_scenarios(containers, capacity):
    #@lru_cache(maxsize=None)
    def count_scenarios_aux(k, cap):
        if cap == 0:
            return 1
        dim = len(containers)
        if k >= dim:
            return 0
        n = 0
        weight = containers[k]
        if weight <= cap:
            n += count_scenarios_aux(k, (cap - weight))
        n += count_scenarios_aux(k+1, cap)
        return n
    return (count_scenarios_aux(0, capacity))