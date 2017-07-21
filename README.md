# outline-toc.el

An Emacs extension that will render a "table of contents" for any buffer in outline-mode.

This TOC will show the sections in the document, highlighting the one that
you're editing in the master document. The whole idea is to give you context as
you're editing a large outline-mode document (e.g. a markdown file or something).

## Current status

Right now this doesn't do a whole lot. You can open up the TOC and it'll stay
roughly synchronized with your master doc. It doesn't do highlighting or
anything like that yet.

## Quickstart

Right now this probably only works with `markdown-mode` files. Open up a
markdown file and run `outline-toc-mode`. This should open up a new window with
a table of contents for your file. As you move around in the source buffer, the
TOC should highlight the section you're in.
