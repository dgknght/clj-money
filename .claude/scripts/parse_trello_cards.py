import json, sys


def load_cards(raw):
    data = json.loads(raw)
    # MCP tool results are sometimes wrapped as [{"text": "<json array>"}]
    if isinstance(data, list) and data and isinstance(data[0], dict) and "text" in data[0]:
        return json.loads(data[0]["text"])
    return data


def main():
    raw = open(sys.argv[1]).read() if len(sys.argv) > 1 else sys.stdin.read()
    cards = load_cards(raw)
    sorted_cards = sorted(cards, key=lambda c: c["pos"])
    for c in sorted_cards[:5]:
        labels = [l["name"] for l in c.get("labels", [])]
        print(f"pos={c['pos']:.0f} | {c['name']} | labels={labels}")
        print(f"  desc: {c['desc'][:200]}")
        print()


if __name__ == "__main__":
    main()
