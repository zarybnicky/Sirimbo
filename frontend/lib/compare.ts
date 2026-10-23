export const compareText = new Intl.Collator('cs-CZ').compare;

export const compareNaturalText = new Intl.Collator('cs-CZ', {
  numeric: true,
}).compare;

export function compareByOrder(order: readonly string[]) {
  const ranks = new Map(order.map((value, index) => [value, index]));
  return (a: string, b: string) =>
    (ranks.get(a) ?? order.length) - (ranks.get(b) ?? order.length);
}
