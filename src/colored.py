def fore(color: str) -> str:
    """Returns ANSI escape code for foreground color."""
    colors = {
        'black': '\033[30m',
        'red': '\033[31m',
        'green': '\033[32m',
        'yellow': '\033[33m',
        'blue': '\033[34m',
        'magenta': '\033[35m',
        'cyan': '\033[36m',
        'white': '\033[37m',
        'dark_gray': '\033[90m'
    }
    return colors.get(color.lower(), '')

def back(color: str) -> str:
    """Returns ANSI escape code for background color."""
    colors = {
        'black': '\033[40m',
        'red': '\033[41m',
        'green': '\033[42m',
        'yellow': '\033[43m',
        'blue': '\033[44m',
        'magenta': '\033[45m',
        'cyan': '\033[46m',
        'white': '\033[47m'
    }
    return colors.get(color.lower(), '')

def stylize(text: str, style: str) -> str:
    """Applies the style to the text and resets the style afterward."""
    return f"{style}{text}\033[0m"