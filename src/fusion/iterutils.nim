import sugar

iterator findAll*[T](s : openArray[T], val : T) : int =
    for i, x in s:
        if x == val:
            yield i

iterator findAll*[T](s : openArray[T], pred : (T) -> bool) : int =
    for i, x in s:
        if pred x:
            yield i