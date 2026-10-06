#!/usr/bin/env python3
"""Import Hypothesis annotations as CriticMarkup comments into the paper's .qmd files.

  export HYPOTHESIS_TOKEN=...        # https://hypothes.is/account/developer
  python3 hypothesis_to_criticmarkup.py            # dry run (default)
  python3 hypothesis_to_criticmarkup.py --apply    # write changes

Each annotation becomes  {==quoted text==}{>>comment (user)<<}
Annotations whose quote cannot be matched uniquely are listed for manual placement.
"""
import argparse, json, os, re, sys, urllib.parse, urllib.request

URI = "https://educacion-meritocracia.github.io/merit-factorial/paper/paper.html"
GROUP_NAME = "Revisión merit-factorial"
FILES = ["01-introduction.qmd", "02-antecedentes.qmd", "03-methods.qmd",
         "04-results.qmd", "05-discussion.qmd", "06-conclusion.qmd", "paper.qmd"]
API = "https://api.hypothes.is/api"


def get(path, token, **params):
    url = f"{API}/{path}" + ("?" + urllib.parse.urlencode(params) if params else "")
    req = urllib.request.Request(url, headers={"Authorization": f"Bearer {token}"})
    with urllib.request.urlopen(req) as r:
        return json.load(r)


def fetch(token):
    groups = get("profile/groups", token)
    gid = next((g["id"] for g in groups if g["name"] == GROUP_NAME), None)
    if not gid:
        sys.exit(f"Group '{GROUP_NAME}' not found. Your groups: {[g['name'] for g in groups]}")
    rows, offset = [], 0
    while True:
        res = get("search", token, uri=URI, group=gid, limit=200, offset=offset, sort="created", order="asc")
        rows += res["rows"]
        offset += len(res["rows"])
        if not res["rows"] or offset >= res["total"]:
            return rows


def quote_of(a):
    for t in a.get("target", []):
        for s in t.get("selector", []):
            if s.get("type") == "TextQuoteSelector":
                return s["exact"]
    return None


def loose_regex(quote):
    """Match the rendered quote against markdown source: tolerate whitespace, *, _, and [@cites]."""
    words = quote.split()
    sep = r"(?:\s|\*|_|\\)+"
    return re.compile(sep.join(re.escape(w) for w in words))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--apply", action="store_true")
    args = ap.parse_args()
    token = os.environ.get("HYPOTHESIS_TOKEN") or sys.exit("Set HYPOTHESIS_TOKEN first.")

    anns = [a for a in fetch(token) if not a.get("hidden")]
    # replies have references; attach them to the parent comment
    replies = {}
    for a in anns:
        if a.get("references"):
            replies.setdefault(a["references"][0], []).append(a)
    tops = [a for a in anns if not a.get("references")]
    print(f"{len(tops)} annotations, {sum(map(len, replies.values()))} replies")

    texts = {f: open(f, encoding="utf-8").read() for f in FILES if os.path.exists(f)}
    unplaced, edits = [], {f: [] for f in texts}

    for a in tops:
        user = a["user"].split(":")[1].split("@")[0]
        body = a.get("text", "").strip()
        for r in replies.get(a["id"], []):
            body += f" | re {r['user'].split(':')[1].split('@')[0]}: {r.get('text','').strip()}"
        body = " ".join(body.split()).replace("<<}", "< <}")
        quote = quote_of(a)
        if not quote:
            unplaced.append((None, user, body)); continue
        rx = loose_regex(quote)
        hits = [(f, m) for f, t in texts.items() for m in rx.finditer(t)
                if "{==" not in t[max(0, m.start() - 3):m.start()]]
        if len(hits) != 1:
            unplaced.append((quote, user, body, len(hits))); continue
        f, m = hits[0]
        edits[f].append((m.start(), m.end(), f"{{=={m.group(0)}==}}{{>>{body} ({user})<<}}"))

    placed = 0
    for f, es in edits.items():
        if not es: continue
        t = texts[f]
        for s, e, rep in sorted(es, reverse=True):
            t = t[:s] + rep + t[e:]
            placed += 1
        if args.apply:
            open(f, "w", encoding="utf-8").write(t)
        print(f"{f}: {len(es)} comments")

    print(f"\nPlaced {placed}; {len(unplaced)} need manual placement:")
    for u in unplaced:
        print("-", u)
    if not args.apply:
        print("\nDry run only. Re-run with --apply to write.")


if __name__ == "__main__":
    main()
