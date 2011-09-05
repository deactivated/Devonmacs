import tempfile, subprocess
from Pymacs import lisp
from appscript import *


__all__ = ["quicksearch", "url", "create_record"]


def quicksearch(text):
    dtpo = app('DEVONthink Pro.app')
    return [(r.uuid.get(), r.name.get())
            for r in dtpo.search(text)]


def url(text):
    opts = quicksearch(text)
    if opts:
        opts_map = {n: u for u, n in opts}
        sel = lisp.ido_completing_read("Choose: ", [n for u, n in opts])
    if opts and sel:
        url = "x-devonthink-item://%s" % opts_map[sel]

    mode = lisp.major_mode.value()
    if mode == lisp["org-mode"]:
        link = "[[%s][%s]]" % (url, sel)
    elif mode == lisp["markdown-mode"]:
        link = "[%s](%s)" % (url, sel)
    else:
        link = url
    lisp.insert(link)

url.interaction = 'MSearch: '


def create_record(name, type, content=None, fn=None):
    sources = {
        "html": k.source,
        "rich": k.rich_text,
        "plain": k.plain_text
    }
    types = {
        "html": k.html,
        "rich": k.text,
        "plain": k.text
    }

    if fn:
        content = open(fn).read()

    dtpo = app('DEVONthink Pro.app')
    record = dtpo.create_record_with({
        k.name: name,
        k.type: types[type],
        sources[type]: content})
    return record.uuid.get(), record.path.get()
