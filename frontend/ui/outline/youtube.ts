// Every shape YouTube hands out: the watch page, a share link, an embed URL, a
// short, a live stream, and the id on its own. Anything else is not ours.
const PATTERNS = [
  /^[\w-]{11}$/,
  /[?&]v=([\w-]{11})(?:&|$)/,
  /youtu\.be\/([\w-]{11})(?:[?#/]|$)/,
  /youtube\.com\/(?:embed|shorts|live|v)\/([\w-]{11})(?:[?#/]|$)/,
];

export function youtubeVideoId(input: string): string | null {
  const trimmed = input.trim();
  for (const pattern of PATTERNS) {
    const found = pattern.exec(trimmed);
    if (found) {
      return found[1] ?? trimmed;
    }
  }
  return null;
}
