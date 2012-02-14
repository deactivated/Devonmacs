from Pymacs import lisp
from appscript import *


__all__ = ["quicksearch", "url", "create_record", "update_record",
           "get_record", "append_tags"]


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


def create_record(name, type, content=None, fn=None, tags=()):
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
        content = open(fn, "rb").read()

    dtpo = app('DEVONthink Pro.app')
    record = dtpo.create_record_with({
        k.name: name.decode('utf8'),
        k.type: types[type],
        k.tags: tags,
        sources[type]: content.decode('utf8')})
    return record.uuid.get(), record.path.get()


def update_record(uuid, **kwargs):
    dtpo = app('DEVONthink Pro.app')
    record = dtpo.get_record_with_uuid(uuid)
    for k, v in kwargs.iteritems():
        if hasattr(record, k):
            getattr(record, k).set(v)


def get_record(uuid):
    keys = ["uuid", "name", "tags", "path"]

    dtpo = app('DEVONthink Pro.app')
    record = dtpo.get_record_with_uuid(uuid)

    return [(k, getattr(record, k).get()) for k in keys]


def append_tags(uuid, tags):
    dtpo = app('DEVONthink Pro.app')
    record = dtpo.get_record_with_uuid(uuid)

    current_tags = record.tags.get()
    record.tags.set(current_tags + tags)
