import type { SeriesInfoFragment } from '@/graphql/Event';
import Link from 'next/link';

export function SeriesInfoLink({ info }: { info: SeriesInfoFragment | null | undefined }) {
  if (
    !info?.id ||
    info.position == null ||
    info.length == null ||
    info.length < 2
  ) {
    return null;
  }

  const name = info.name?.trim();

  return (
    <Link
      href={`/terminy/${info.id}`}
      className="text-sm text-neutral-11 underline decoration-neutral-7 underline-offset-2 hover:text-accent-11"
    >
      {info.position}. z {info.length} v sérii{name ? ` ${name}` : ''}
    </Link>
  );
}
