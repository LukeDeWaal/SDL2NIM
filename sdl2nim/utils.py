import inspect


__all__ = ['not_implemented_error']

def not_implemented_error():
    raise NotImplementedError(f"{inspect.stack()[1][3]} is not implemented yet")