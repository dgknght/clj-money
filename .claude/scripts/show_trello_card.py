import json
import sys


def load_cards(raw):
    data = json.loads(raw)
    # MCP tool results are sometimes wrapped as [{"text": "<json array>"}]
    if isinstance(data, list) and data and isinstance(data[0], dict) and "text" in data[0]:
        return json.loads(data[0]["text"])
    return data


def main():
    if len(sys.argv) < 2:
        print("usage: show_trello_card.py <result-file> <name-substring-or-pos>", file=sys.stderr)
        sys.exit(1)

    raw = open(sys.argv[1]).read()
    cards = load_cards(raw)
    needle = sys.argv[2] if len(sys.argv) > 2 else None

    for c in cards:
        match = needle is None or needle.lower() in c["name"].lower() or str(c.get("pos")) == needle
        if match:
            print(json.dumps(c, indent=2))


if __name__ == "__main__":
    main()
