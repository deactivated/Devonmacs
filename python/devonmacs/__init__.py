from .devonthink import *


def reload_package():
    "Reload the entire DVM package."
    import devonmacs
    from . import devonthink
    reload(devonthink)
    reload(devonmacs)
