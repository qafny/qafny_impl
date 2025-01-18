def make_repr(class_name: str, props: {str: any}) -> str:
    """Returns a string representation of an object closely matching python construction syntax.
    None types are hidden by default"""
    property_list = ", ".join(f'{k}={v}' for k, v in props.items() if v is not None)
    return f"{class_name}({property_list})"