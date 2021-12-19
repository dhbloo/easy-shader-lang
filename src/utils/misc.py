def merge_matched(matched1, matched2):
    matched = {**matched1}
    for k, t in matched2.items():
        if k in matched:
            if not (matched[k] == t):
                return None
        else:
            matched[k] = t
    return matched