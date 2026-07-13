
export function stripHtml(value: string | null | undefined) {
  return (value ?? '')
    .replaceAll(/<[^>]*>/g, ' ')
    .replaceAll('&nbsp;', ' ')
    .replaceAll(/\s+/g, ' ')
    .trim();
}

