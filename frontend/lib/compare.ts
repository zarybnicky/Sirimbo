export const compareText = new Intl.Collator('cs-CZ').compare;

export const compareNaturalText = new Intl.Collator('cs-CZ', {
  numeric: true,
}).compare;
