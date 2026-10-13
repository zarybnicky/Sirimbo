import Link from 'next/link';
import { cn } from '@/lib/cn';

export function RoundNav({
  href,
  rounds,
  current,
}: {
  href: string;
  rounds: { roundKey: string; roundLabel?: string | null }[];
  current?: string;
}) {
  if (rounds.length === 0) return null;
  const tabs = [
    { href, label: 'Přehled', active: current == null },
    ...rounds.map((round) => {
      const label = round.roundLabel || round.roundKey;
      return {
        href: `${href}/rounds/${encodeURIComponent(round.roundKey)}`,
        label:
          label === 'F'
            ? 'Finále'
            : label === 'SF'
              ? 'Semifinále'
              : /^\d+$/.test(label)
                ? `${label}. kolo`
                : label,
        active: current === round.roundKey,
      };
    }),
  ];
  return (
    <nav
      aria-label="Kola soutěže"
      className="flex gap-4 overflow-x-auto border-b border-neutral-7 mb-2"
    >
      {tabs.map((tab) => (
        <Link
          key={tab.href}
          href={tab.href}
          prefetch={false}
          aria-current={tab.active ? 'page' : undefined}
          className={cn(
            'whitespace-nowrap py-2 px-1 border-b-2 font-medium text-sm',
            tab.active
              ? 'border-accent-9 text-accent-11'
              : 'border-transparent text-neutral-11 hover:text-neutral-12 hover:border-neutral-8',
          )}
        >
          {tab.label}
        </Link>
      ))}
    </nav>
  );
}
