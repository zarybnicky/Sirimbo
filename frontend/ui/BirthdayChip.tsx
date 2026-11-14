import { Cake } from 'lucide-react';
import Link from 'next/link';

export function BirthdayChip({ person }: { person: { id: string; name: string } }) {
  return (
    <Link
      href={`/clenove/${person.id}`}
      title={`Narozeniny: ${person.name}`}
      className="inline-flex max-w-full items-center gap-1 rounded-sm border border-neutral-6 bg-neutral-2 px-2 py-1 text-xs text-neutral-12 hover:bg-neutral-3 hover:underline"
    >
      <Cake className="size-3.5 shrink-0 text-accent-11" />
      <span className="truncate">{person.name}</span>
    </Link>
  );
}
