const EMOJI_FALLBACK = new RegExp(
  [
    String.raw`[#*0-9]\uFE0F?\u20E3`,
    String.raw`\p{Regional_Indicator}{2}`,
    String.raw`\u{1F3F4}[\u{E0060}-\u{E007F}]+`,
    String.raw`\p{Extended_Pictographic}(?:\uFE0F|\p{Emoji_Modifier})?` +
      String.raw`(?:\u200D\p{Extended_Pictographic}(?:\uFE0F|\p{Emoji_Modifier})?)*`,
  ].join('|'),
  'gu'
);

let RGI_EMOJI;
try {
  RGI_EMOJI = new RegExp(String.raw`\p{RGI_Emoji}`, 'gv');
} catch {
  RGI_EMOJI = EMOJI_FALLBACK;
}
const FORBIDDEN_CHARS = /[\p{Cc}\p{Cs}\p{Co}]/gu;

export const sanitizeUnicode = (s: string | undefined | null) =>
  s
    ?.replaceAll(RGI_EMOJI, "")
    .replaceAll(FORBIDDEN_CHARS, "")
    .normalize("NFC")
    .replaceAll(/\s+/g, " ")
    .trim() ?? "";

export function sanitizeReturnURL(
  value: string | undefined | null,
  origin: string,
) {
  if (!value || !URL.canParse(value, origin)) return null;

  const url = new URL(value, origin);
  return url.origin === origin ? `${url.pathname}${url.search}${url.hash}` : null;
}
