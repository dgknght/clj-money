import json, sys

data = json.load(sys.stdin)
cards = json.loads(data[0]['text'])
sorted_cards = sorted(cards, key=lambda c: c['pos'])
top5 = sorted_cards[:5]
for c in top5:
    labels = [l['name'] for l in c.get('labels', [])]
    print(f"pos={c['pos']:.0f} | {c['name']} | labels={labels}")
    print(f"  desc: {c['desc'][:200]}")
    print()
